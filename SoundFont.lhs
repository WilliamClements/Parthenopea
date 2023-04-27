> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>

SoundFont support =========================================================================

> module SoundFont where
>
> import Cecil
> import qualified Codec.SoundFont      as F
> import Control.Arrow
> import Control.Arrow.ArrowP ( ArrowP(ArrowP), strip )
> import qualified Control.SF.SF        as C
> import Covers
> import Data.Array.Unboxed ( Array, (!), IArray(bounds) )
> import qualified Data.Audio           as A
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List (find, foldr, minimumBy)
> import qualified Data.Map as Map
> import Data.Maybe (isJust, fromJust, fromMaybe)
> import Debug.Trace ( traceIO, traceM )
> import Euterpea.IO.Audio.BasicSigFuns ( envASR, envLineSeg )
> import Euterpea.IO.Audio.Basics ( apToHz, outA )
> import Euterpea.IO.Audio.IO ( outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, AudSF, Mono )
> import Euterpea.Music
> import Fanfare
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA )
> import Parthenopea ( traceIf, traceAlways, addDur, envDAHdSR )
> import SunPyg
> import System.Environment(getArgs)  
  
importing sampled sound (from SoundFont (*.sf2) file) =====================================

> useEnvelopes = True
> usePitchCorrection = True
>
> newtype InstrumentZones =
>   InstrumentZones {
>               zZones  :: [InstrumentZone]} deriving Show
>
> data InstrumentZone =
>   InstrumentZone {
>     zStartOffs         :: Maybe Int
>   , zEndOffs           :: Maybe Int
>   , zLoopStartOffs     :: Maybe Int
>   , zLoopEndOffs       :: Maybe Int
>
>   , zStartCoarseOffs   :: Maybe Int
>   , zEndCoarseOffs     :: Maybe Int
>   , zLoopStartCoarseOffs
>                        :: Maybe Int
>   , zLoopEndCoarseOffs :: Maybe Int
>
>   , zInstIndex         :: Maybe Word
>   , zKeyRange          :: Maybe (AbsPitch, AbsPitch)
>   , zVelRange          :: Maybe (Volume, Volume)
>   , zCoarseTune        :: Maybe Int
>   , zFineTune          :: Maybe Int
>   , zSampleIndex       :: Maybe Word
>   , zSampleMode        :: Maybe A.SampleMode
>
>   , zDelayVolEnv       :: Maybe Int
>   , zAttackVolEnv      :: Maybe Int
>   , zHoldVolEnv        :: Maybe Int
>   , zDecayVolEnv       :: Maybe Int
>   , zSustainVolEnv     :: Maybe Int 
>   , zReleaseVolEnv     :: Maybe Int
>
>   , zChorus            :: Maybe Int
>   , zReverb            :: Maybe Int
>   , zPan               :: Maybe Int
>
>   , zRootKey           :: Maybe Word} deriving Show
>
> defInstrumentZone      :: InstrumentZone
> defInstrumentZone = InstrumentZone Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing
>   
> data Reconciled =
>   Reconciled {
>     rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: Word
>   , rPitchCorrection   :: Double
>   , rEnvelope          :: Envelope} deriving Show
>           
> data SoundFontArrays = 
>   SoundFontArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssShdrs            :: Array Word F.Shdr
>   , ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
>
> data Envelope =
>   Envelope {
>     eDelay             :: Double
>   , eAttack            :: Double
>   , eHold              :: Double
>   , eDecay             :: Double
>   , eSustain           :: Double
>   , eRelease           :: Double} deriving Show
>
  
slurp in instruments from one SoundFont (*.sf2) file ======================================

> doSoundFont            :: FilePath → IO ()
> doSoundFont inFile =
>   do
>     putStrLn "entering doSoundFont"
>     putStrLn ("inFile=" ++ inFile)
>     maybeAudio ← F.importFile inFile
>     case maybeAudio of
>       Left s               → putStrLn $ "SoundFont decoding error: " ++ s
>       Right soundFont      → do
>         let pdata = F.pdta soundFont
>         let sdata = F.sdta soundFont
>         let arrays = SoundFontArrays
>                        (F.insts pdata)
>                        (F.ibags pdata)
>                        (F.igens pdata)
>                        (F.shdrs pdata)
>                        (F.smpl sdata)
>                        (F.sm24 sdata)
>         case ssM24 arrays of
>           Nothing      → print "16-bit datapoints"
>           Just s24data → print "24-bit datapoints"
>         let ipal = dSoundFontV4Inst
>         let ppal = dSoundFontV4Perc
>         instruments ← doInstruments arrays ipal ppal
>         traceIO ("doInstruments returned " ++ show instruments ++ "\n")
>         return ()
>     putStrLn "leaving doSoundFont"
>
> isNotNull              :: [a] → Bool
> isNotNull [] = False
> isNotNull (x:xs) = True
>
> doInstruments          :: SoundFontArrays
>                           → [(String, (Desirability, InstrumentName))]
>                           → [(String, (Desirability, PercussionSound))]
>                           → IO [InstrumentZones]
> doInstruments arrays ipal ppal = do
>   let is = ssInsts arrays
>   let (ist, ien) = bounds is 
>   putStrLn ("instrument start/stop=" ++ show (ist, ien) ++ "first instrument name=" ++ show (F.instName (is!ist)))
>
>   let selectedI = map (shouldDoInstrument is ipal) [ist..ien-1]
>   let selectedP = map (shouldDoPercussion is ppal) [ist..ien-1]
>
>   putStrLn ("# selected=" ++ show (length selectedI))
>
>   let filteredI = filter isJust selectedI
>   let filteredP = filter isNotNull selectedP
>
>   putStrLn ("# filteredI=" ++ show (length filteredI) ++ " out of " ++ show (length ipal))
>   putStrLn ("# filteredP=" ++ show (length filteredP) ++ " out of " ++ show (length ppal))
>
>   let readyI = map (doInstrument arrays is) filteredI
>   let readyP = map (doPercussion arrays is) filteredP
>
>   let readyI' = readyI {- WOX ++ assignPercussion arrays readyP -}
>
>   putStrLn ("# ready=" ++ show (length readyI'))
>   doPlayInstruments readyI'
>   return []
>
> shouldDoInstrument     :: Array Word F.Inst
>                           → [(String, (Desirability, InstrumentName))]
>                           → Word
>                           → Maybe (InstrumentName, Word)
> shouldDoInstrument is ipal n =
>   let i = is ! n
>       ilist = filter (match (F.instName i)) ipal
>   in case ilist of
>     [] → Nothing
>     _  → Just (snd (snd (head ilist)), n)
>   where
>     match :: String → (String, (Desirability, InstrumentName)) → Bool
>     match iname cand = iname == fst cand
>
> shouldDoPercussion     :: Array Word F.Inst
>                           → [(String, (Desirability, PercussionSound))]
>                           → Word
>                           → [(PercussionSound, (Word, Word))]
> shouldDoPercussion is ppal n =
>   let
>     iinst = is ! n
>     jinst = is ! (n + 1)
>     ibagi = F.instBagNdx iinst
>     jbagi = F.instBagNdx jinst
>     jjill = [] -- filter (matchFirst (F.instName iinst)) ppal
>
>   in case jjill of
>           []      → []
>           (x:xs)  → [] -- WOX unrollZones ibagi jbagi
>   where
>     matchFirst :: String → (String, (Desirability, PercussionSound)) → Bool
>     matchFirst iname cand = iname == fst cand
>
> {-
>     unrollZones        :: Word → Word → [(PercussionSound, (Word, Word))]
>     unrollZones ibagi ibagj =
> -}
>
>         
> doInstrument           :: SoundFontArrays
>                           → Array Word F.Inst
>                           → Maybe (InstrumentName, Word)
>                           → (InstrumentName, Instr (Mono AudRate))
> doInstrument arrays is mis
>   | traceIf msg False = undefined
>   | otherwise = (iname, assignInstrument arrays (InstrumentZones iList))
>   where 
>     (iname, n) = fromJust mis
>     iinst = is ! n
>     jinst = is ! (n + 1)
>     ibagi = F.instBagNdx iinst
>     jbagi = F.instBagNdx jinst
>     gIx   = if jbagi - ibagi < 2
>             then error "must have one global zone and at least one other zone"
>             else [ibagi]
>     oIx   = [ibagi+1..jbagi-1]
>     gList = map (doZone arrays iinst defInstrumentZone) gIx
>     oList = map (doZone arrays iinst (head gList))      oIx
>     iList = gList ++ oList
>     msg = unwords ["doInstrument=", show mis
>                  , " n= ", show (snd (fromJust mis))
>                  , " ", show iinst
>                  , " ", show jinst
>                  , " global generators=", show (head gList)]
>
> doPercussion           :: SoundFontArrays
>                           → Array Word F.Inst
>                           → [(PercussionSound, (Word, Word))]
>                           → [(PercussionSound, Instr (Mono AudRate))]
> doPercussion arrays is mis = []
>
> assignPercussion       :: SoundFontArrays → Int
> assignPercussion arrays = 259
>     
> fromGens               :: InstrumentZone → [F.Generator] → InstrumentZone
> fromGens iz [] = iz
> fromGens iz (g:gs) = fromGens iz' gs
>   where iz' = addGen iz g
>   
> addGen                 :: InstrumentZone → F.Generator → InstrumentZone
> addGen iz gen =
>   case gen of
>   F.StartAddressOffset i         → iz {zStartOffs =                Just i}
>   F.EndAddressOffset i           → iz {zEndOffs =                  Just i}
>   F.LoopStartAddressOffset i     → iz {zStartOffs =                Just i}
>   F.LoopEndAddressOffset i       → iz {zEndOffs =                  Just i}
>
>   F.StartAddressCoarseOffset i   → iz {zStartCoarseOffs =          Just i}
>   F.EndAddressCoarseOffset i     → iz {zEndCoarseOffs =            Just i}
>   F.LoopStartAddressCoarseOffset i
>                                  → iz {zLoopStartCoarseOffs =      Just i}
>   F.LoopEndAddressCoarseOffset i
>                                  → iz {zLoopEndCoarseOffs =        Just i}
>
>   F.InstIndex w                  → iz {zInstIndex =                Just w}
>   F.KeyRange a b                 → iz {zKeyRange =                 Just (fromIntegral a, fromIntegral b)}
>   F.VelRange a b                 → iz {zVelRange =                 Just (fromIntegral a, fromIntegral b)}
>   F.CoarseTune i                 → iz {zCoarseTune =               Just i}
>   F.FineTune i                   → iz {zFineTune =                 Just i}
>   F.SampleIndex w                → iz {zSampleIndex =              Just w}
>   F.SampleMode a                 → iz {zSampleMode =               Just a}
>
>   F.DelayVolEnv i                → iz {zDelayVolEnv =              Just i}
>   F.AttackVolEnv i               → iz {zAttackVolEnv =             Just i}
>   F.HoldVolEnv i                 → iz {zHoldVolEnv =               Just i}
>   F.DecayVolEnv i                → iz {zDecayVolEnv =              Just i}
>   F.SustainVolEnv i              → iz {zSustainVolEnv =            Just i}
>   F.ReleaseVolEnv i              → iz {zReleaseVolEnv =            Just i}
>
>   F.Chorus i                     → iz {zChorus =                   Just i}
>   F.Reverb i                     → iz {zReverb =                   Just i}
>   F.Pan i                        → iz {zPan =                      Just i}
>
>   F.RootKey w                    → iz {zRootKey =                  Just (fromIntegral w)}
>   _                              → iz
>
> doZone                 :: SoundFontArrays → F.Inst → InstrumentZone → Word → InstrumentZone
> doZone arrays iinst fromZone bagIndex
>   | traceIf msg False = undefined
>   | otherwise = fromGens fromZone gens
>   where
>     ibags = ssIBags arrays
>     xgeni = F.genNdx $ ibags!bagIndex
>     ygeni = F.genNdx $ ibags!(bagIndex + 1)
>     gens = if ygeni - xgeni < 0
>            then error "degenerate generator list"
>            else getGens arrays iinst [xgeni..ygeni-1]
>
>     msg = unwords ["doZone xgeni=", show xgeni, " ygeni=", show ygeni, "\n"]
>
> getGens                :: SoundFontArrays → F.Inst → [Word] → [F.Generator]
> getGens arrays iinst words
>   | traceIf msg False = undefined
>   | otherwise = gens
>   where
>     selected = map (doGenerator arrays iinst) words
>     filtered = filter isJust selected
>     gens = map fromJust filtered
>
>     msg = unwords ["getGens=", show words]
> 
> doGenerator :: SoundFontArrays → F.Inst → Word → Maybe F.Generator
> doGenerator arrays iinst zw = Just $ ssIGens arrays ! zw
>
  
define signal functions for playing instruments ===========================================

> eutPhaser              :: Double
>                           → Double
>                           → Double
>                           → Double
>                           → AudSF () Double 
> eutPhaser secs sr iphs freqFactor =
>   let frac             :: RealFrac r ⇒ r → r
>       frac                           = snd . properFraction
>       delta            :: Double
>       delta                          = 1 / (secs * freqFactor * sr)
>   in proc () → do
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> eutRelay               :: A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → (Word, Word)
>                           → Envelope
>                           → Volume
>                           → Dur
>                           → AudSF Double Double
> eutRelay s16 ms8 (st, en) renv vol dur =
>   let
>     -- (st, en)           :: (Word, Word) = (rStart reconciled, rEnd reconciled)
>     nc = 1 -- numChans (undefined :: u)
>     numS               :: Double
>     numS = case ms8 of
>           Nothing → fromIntegral $ (en - st + 1) `div` nc
>           Just _  → error "24-bit not yet supported"
>     amp                :: Double
>     amp = fromIntegral vol / 100
>     secs               :: Double
>     secs = 2 * fromRational dur
>   in proc pos → do
>     let sampleAddress  :: Int = fromIntegral st + truncate (numS * pos)
> 
>     aenv ← if useEnvelopes
>            then doEnvelope renv secs ⤙ ()
>            else constA 1             ⤙ ()
> 
>     outA ⤙ aenv * amp * fromIntegral (s16 ! sampleAddress)
>   where
>     doEnvelope         :: Envelope → Double → AudSF () Double
>     doEnvelope renv secs
>       | traceIf msg False = undefined
>       | otherwise = envDAHdSR secs
>                               (eDelay renv)
>                               (eAttack renv)
>                               (eHold renv)
>                               (eDecay renv)
>                               (eSustain renv)
>                               (eRelease renv)
>       where
>         msg = unwords ["Envelope=", show renv]
>
> assignInstrument       :: SoundFontArrays → InstrumentZones → Instr (Mono AudRate)
> assignInstrument arrays zones dur pch vol params =
>   let
>     (zone, shdr)       :: (InstrumentZone
>                          , F.Shdr)         = selectZone arrays zones pch vol
>     rData              :: Reconciled       = reconcile zone shdr
>     sr                 :: Double           = fromIntegral $ F.sampleRate shdr
>     ns                 :: Double           = fromIntegral $ rEnd rData - rStart rData + 1
>     secs               :: Double           = ns / sr
>     ap                 :: AbsPitch
>
>     -- if duration is too long, use the looped range
>     (st, en) = if secs < 2 * fromRational dur
>                then (rLoopStart rData, rLoopEnd rData)
>                else (rStart rData,     rEnd rData)
>
>     ok = checkReconcile shdr zone rData secs (st, en)
>     ap = if not ok
>          then error "InstrumentZone and F.Shdr could not be reconciled"
>          else fromIntegral (rRootKey rData)
>
>     ns'                :: Double              = fromIntegral (en - st + 1)
>     secs'              :: Double              = ns' / sr
>     freqFactor         :: Double              = if usePitchCorrection
>                                                 then freqRatio * rateRatio / rPitchCorrection rData
>                                                 else freqRatio * rateRatio
>     freqRatio          :: Double              = apToHz ap / apToHz pch
>     rateRatio          :: Double              = 44100 / sr
>     sig                :: AudSF () Double     = eutPhaser secs' sr 0 freqFactor
>                                             >>> eutRelay (ssData arrays) (ssM24 arrays) (st, en) (rEnvelope rData) vol dur
>   in proc _ → do
>     z ← sig ⤙ ()
>     outA ⤙ z
>

zone selection ============================================================================

> selectZone             :: SoundFontArrays
>                           → InstrumentZones
>                           → AbsPitch
>                           → Volume
>                           → (InstrumentZone, F.Shdr)
> selectZone arrays zones pch vol =
>   let 
>     -- skip the Global zone as we usually do
>     scores = map (scoreOneZone pch vol) (tail (zZones zones))
>     zone = snd $ minimumBy compareScores scores
>   in
>     (zone, ssShdrs arrays ! fromJust (zSampleIndex zone))
>
> compareScores          :: (Num a, Ord a) ⇒ (a, b) → (a, b) → Ordering
> compareScores (a1, b1) (a2, b2) = compare a1 a2 
>                         
> scoreOneZone           :: AbsPitch → Volume → InstrumentZone → (Int, InstrumentZone)
> scoreOneZone pch vol zone = (score, zone)
>   where
>     score = score1 + score2
>     score1 = computeDistance pch (zKeyRange zone)
>     score2 = computeDistance vol (zVelRange zone)
>
> computeDistance        :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> computeDistance cand mrange =
>   case mrange of
>     Nothing                   → 128
>     Just (rangeMin, rangeMax) → if cand >= rangeMin && cand <= rangeMax
>                                 then 0
>                                 else min (abs $ rangeMin - cand) (abs $ cand - rangeMax)

reconcile zone and sample header ==========================================================

>
> sumOfMaybeInts         :: [Maybe Int] → Int
> sumOfMaybeInts = foldr ((+) . fromMaybe 0) 0
>       
> addIntToWord           :: Word → Int → Word
> addIntToWord w i =
>   let iw               :: Int = fromIntegral w
>       sum              :: Int = iw + i
>   in fromIntegral sum
>
> reconcile              :: InstrumentZone → F.Shdr → Reconciled
> reconcile zone shdr =
>   Reconciled {
>     rStart           = addIntToWord          (F.start shdr)           (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd             = addIntToWord          (F.end shdr)             (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart       = addIntToWord          (F.startLoop shdr)       (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd         = addIntToWord          (F.endLoop shdr)         (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey         = fromMaybe             (F.originalPitch shdr)   (zRootKey zone)
>   , rPitchCorrection = resolvePitchCorrection(F.pitchCorrection shdr) (zCoarseTune zone) (zFineTune zone)
>   , rEnvelope        = deriveEnvelope        (zDelayVolEnv zone)
>                                              (zAttackVolEnv zone)
>                                              (zHoldVolEnv zone)
>                                              (zDecayVolEnv zone)
>                                              (zSustainVolEnv zone)
>                                              (zReleaseVolEnv zone)}
>
> resolvePitchCorrection :: Int → Maybe Int → Maybe Int → Double
> resolvePitchCorrection alt mps mpc = 2 ** (fromIntegral cents/12/100)
>   where
>     cents              :: Int = if isJust mps || isJust mpc
>                                 then coarse * 100 + fine
>                                 else alt
>     coarse             :: Int = fromMaybe 0 mps
>     fine               :: Int = fromMaybe 0 mpc
>
> deriveEnvelope         :: Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Envelope
> deriveEnvelope mDelay mAttack mHold mDecay mSustain mRelease
>   | traceIf msg False = undefined
>   | otherwise =
>     let
>       iDelay = fromMaybe (-12000) mDelay
>       iAttack = fromMaybe (-12000) mAttack
>       iHold   = fromMaybe (-12000) mHold
>       iDecay  = fromMaybe (-12000) mDecay
>       iSustain = fromMaybe 0 mSustain
>       iRelease = fromMaybe (-12000) mRelease
>     in 
>       Envelope (conv iDelay) (conv iAttack)          (conv iHold)
>                (conv iDecay) (fromIntegral iSustain) (conv iRelease)
>     where
>       conv               :: Int → Double
>       conv k = 2**(fromIntegral k/1200)
>       msg = unwords ["mDelay=",   show mDelay,   "mAttack=",    show mAttack
>                     ,"mHold=",    show mHold,    "mDecay=",     show mDecay
>                     ,"mSustain=", show mSustain, "mRelease=",   show mRelease]
>
> checkReconcile         :: F.Shdr
>                           → InstrumentZone
>                           → Reconciled
>                           → Double
>                           → (Word, Word)
>                           → Bool
> checkReconcile shdr zone recon secs (st, en)
>   | traceIf msg False = undefined
>   | otherwise = True
>   where
>     msg = unwords ["checkReconcile=", show shdr
>                                     , show zone
>                                     , show recon
>                                     , show secs
>                                     , show (st, en)]
>

organize instruments from multiple SoundFont files ========================================

> data Desirability =
>   DLow | DMed | DHigh
>
> soundFontDatabase      :: [   (String
>                               , (Desirability
>                                  , ([   (String
>                                    , (Desirability
>                                    , InstrumentName))]
>                                  , [   (String
>                                    , (Desirability
>                                    , PercussionSound))])))]
> soundFontDatabase =
>   [ ("editDSoundFontV4.sf2",      (DMed, (dSoundFontV4Inst, dSoundFontV4Perc)))
>    ,("editEssentials.sf2",        (DLow, (essentialsInst, essentialsPerc))) ]
>
> dSoundFontV4Inst       :: [(String, (Desirability, InstrumentName))]
> dSoundFontV4Inst =
>   [
>       ("+++Gtr Harmonics",        (DMed,  GuitarHarmonics))
>     , ("60's Organ 1",            (DMed,  RockOrgan))
>     , ("Accordion1",              (DMed,  Accordion))
>     , ("Agogo",                   (DMed,  Agogo))
>     , ("Applause0",               (DMed,  Applause))
>     , ("Banjo",                   (DMed,  Banjo))
>     , ("Baritone BV",             (DMed,  BaritoneSax))
>     , ("Bird",                    (DMed,  BirdTweet))
>     , ("Bottle Blow",             (DMed,  BlownBottle))
>     , ("Brass",                   (DMed,  BrassSection))
>     , ("Breath Noise0",           (DMed,  BreathNoise))
>     , ("Campana",                 (DMed,  TubularBells))
>     , ("Celesta",                 (DMed,  Celesta))
>     , ("Church Organ",            (DMed,  ChurchOrgan))
>     , ("Clarinet",                (DMed,  Clarinet))
>     , ("Classical Guitar 1",      (DMed,  AcousticGuitarSteel))
>     , ("Clavinet1",               (DMed,  Clavinet))
>     , ("Dulcimer-Hammered",       (DMed,  Dulcimer))
>     , ("Eddie's English Horn",    (DMed,  EnglishHorn))
>     , ("Finger Bass",             (DMed,  ElectricBassFingered))
>     , ("French Horns",            (DMed,  FrenchHorn))
>     , ("German 8 Harpsichord",    (DMed,  Harpsichord))
>     , ("Glockenspiel 1",          (DMed,  Glockenspiel))
>     , ("Grand Piano",             (DMed,  AcousticGrandPiano))
>     , ("Guitar Fret Noise",       (DMed,  GuitarFretNoise))
>     , ("Gunshot",                 (DMed,  Gunshot))
>     , ("Helicopter",              (DMed,  Helicopter))
>     , ("hrp:Harp",                (DMed,  OrchestralHarp))
>     , ("Iowa Alto Sax",           (DMed,  AltoSax))
>     , ("iowa bassoon",            (DMed,  Bassoon))
>     , ("Iowa Marimba",            (DMed,  Marimba))
>     , ("Iowa Oboe",               (DMed,  Oboe))
>     , ("IowaTrumpet",             (DMed,  Trumpet))
>     , ("Iowa Viola-mf",           (DMed,  Viola))
>     , ("Iowa Woodblock",          (DMed,  Woodblock))
>     , ("Iowa Xylophone",          (DMed,  Xylophone))
>     , ("Kalimba",                 (DMed,  Kalimba))
>     , ("Koto",                    (DMed,  Koto))
>     , ("Music Box",               (DMed,  MusicBox))
>     , ("Ocarina",                 (DMed,  Ocarina))
>     , ("Ottos Fretless",          (DMed,  FretlessBass))
>     , ("Orchestral Kit",          (DMed,  Percussion))
>     , ("Pan Flute",               (DMed,  PanFlute))
>     , ("Piccolo",                 (DMed,  Piccolo))
>     , ("Picked Bass",             (DMed,  ElectricBassPicked))
>     , ("Pizzicato",               (DMed,  PizzicatoStrings))
>     , ("Recorder",                (DMed,  Recorder))
>     , ("Reed Organ",              (DMed,  ReedOrgan))
>     , ("Rhodes",                  (DMed,  RhodesPiano))
>     , ("Seashore",                (DMed,  Seashore))
>     , ("Shakuhachi",              (DMed,  Shakuhachi))
>     , ("Shamisen",                (DMed,  Shamisen))
>     , ("Shenai",                  (DMed,  Shanai))
>     , ("Sitar",                   (DMed,  Sitar))
>     , ("soprano sax",             (DMed,  SopranoSax))
>     , ("Spatial Grand Piano",     (DMed,  ElectricGrandPiano))
>     , ("Steel Drum",              (DMed,  SteelDrums))
>     , ("Strat Marshall",          (DMed,  DistortionGuitar))
>     , ("Stream",                  (DMed,  Pad2Warm))
>     , ("Synth Bass 3",            (DMed,  SynthBass2))
>     , ("Synth Drum",              (DMed,  SynthDrum))
>     , ("Synth Brass 1",           (DMed,  SynthBrass1))
>     , ("Synth Brass 2",           (DMed,  SynthBrass2))
>     , ("Synth Strings 1",         (DMed,  SynthStrings1))
>     , ("Synth Strings 2",         (DMed,  SynthStrings2))
>     , ("Taiko Drum",              (DMed,  TaikoDrum))
>     , ("TELEPHONE",               (DMed,  TelephoneRing))
>     , ("Tenor Both Xfade",        (DMed,  TenorSax))
>     , ("Timpani All",             (DMed,  Timpani))
>     , ("Trombone1",               (DMed,  Trombone))
>     , ("Tuba",                    (DMed,  Tuba))
>     , ("Ukelele",                 (DMed,  Pad3Polysynth))
>     , ("vibraphone",              (DMed,  Vibraphone))
>     , ("Whistle",                 (DMed,  Whistle))
>   ]
> dSoundFontV4Perc       :: [(String, (Desirability, PercussionSound))]
> dSoundFontV4Perc =
>   []
>
> essentialsInst         :: [(String, (Desirability, InstrumentName))]
> essentialsInst =
>   [
>       ("Alto Sax XSwitch",        (DMed,  AltoSax))
>     , ("B3-1 Slow Rotor",         (DMed,  ReedOrgan))
>     , ("Bassoon",                 (DMed,  Bassoon))
>     , ("Cello",                   (DMed,  Cello))
>     , ("ChGrand01v1",             (DMed,  AcousticGrandPiano))
>     , ("Clarinet",                (DMed,  Clarinet))
>     , ("Clean Guitar",            (DMed,  ElectricGuitarClean))
>     , ("DX7 Rhodes",              (DMed,  RhodesPiano))
>     , ("F",                       (DMed,  Violin))
>     , ("Flute",                   (DMed,  Flute))
>     , ("Group 162",               (DMed,  Viola))
>     , ("Jazz Guitar",             (DMed,  ElectricGuitarJazz))
>     , ("MagiCs 5Strg Banjo",      (DMed,  Banjo))
>     , ("Nylon Guitar 1",          (DMed,  AcousticGuitarNylon))
>     , ("Oboe",                    (DMed,  Oboe))
>     , ("Palm Muted Guitar",       (DMed,  ElectricGuitarMuted))
>     , ("Piccolo",                 (DMed,  Piccolo))
>     , ("Pipe Organ",              (DMed,  ChurchOrgan))
>     , ("Spanish",                 (DMed,  AcousticGuitarSteel))
>     , ("String Ensembles",        (DMed,  StringEnsemble1))
>     , ("Strings Pan",             (DMed,  StringEnsemble2))
>     , ("Tenor Both Xfade",        (DMed,  TenorSax))
>     , ("Timpani 1 JN",            (DMed,  Timpani))
>     , ("Trombone",                (DMed,  Trombone))
>     , ("Trumpet",                 (DMed,  Trumpet))
>     , ("Tuba",                    (DMed,  Tuba))
>     , ("Upright-Piano-1",         (DMed,  BrightAcousticPiano))
>     -- , ("Violin3",            Violin)
>   ]
> essentialsPerc         :: [(String, (Desirability, PercussionSound))]
> essentialsPerc =
>   []
>
> evaluateFiles          :: [InstrumentName]
> evaluateFiles =
>    let coveredFile = concatMap doFile soundFontDatabase
>    in coveredFile
>    where
>      subtract _ = []
>      doFile            :: ( String, (Desirability, ([(String, (Desirability, InstrumentName))], [(String, (Desirability, PercussionSound))]))) → [InstrumentName]
>      doFile file =
>        let
>          list1          :: (Desirability, ([(String, (Desirability, InstrumentName))], [(String, (Desirability, PercussionSound))]))
>          list1 = snd file
>          list2          :: ([(String, (Desirability, InstrumentName))], [(String, (Desirability, PercussionSound))])
>          list2 = snd list1
>          list3          :: [(String, (Desirability, InstrumentName))]
>          list3 = fst list2
>
>          coveredInst   :: [InstrumentName]
>          coveredInst = map (snd.snd) list3         
>        in coveredInst
>
> type WHCMap1 = Map.Map InstrumentName Int32
>
> roundtuit :: IO ()
> roundtuit =
>   let
>     bar = evaluateFiles
>
>     dumb :: [InstrumentName] -> WHCMap1 -> WHCMap1
>     dumb [] whcdi = Map.empty
>     dumb (x:xs) whcdi = Map.union (Map.insert x 1 whcdi) (dumb xs whcdi)
>     
>     st :: Int = fromEnum AcousticGrandPiano
>     en :: Int = fromEnum Gunshot
>
>     alli = [st .. en]
>     allj = map (\x -> (toEnum x,1)) alli
>     all = Map.fromList allj
>     -- all = dumb [(fromEnum AcousticGrandPiano) .. ()]
>     some = dumb bar Map.empty
>
>     leftover = Map.difference all some
>
>   in do
>     print "all"
>     print all
>
>     print "some"
>     print some
>
>     print "leftover"
>     print $ show leftover
> {-
> roundtuit :: IO ()
> roundtuit = do
>     let all = [AcousticBassDrum..OpenTriangle]
>     show all
>
> korgInstruments        :: [(String, InstrumentName)]
> korgInstruments =
>   [
>       ("Orchestra",               Percussion)
>   ]
>
> tyrosAcoosticBass      :: [(String, InstrumentName)]
> tyrosAcoosticBass = 
>   [
>       ("TyrosAcoosticBa0",        AcousticBass)
>     , ("TyrosAcoosticBa1",        ElectricBassPicked)
>   ]
>
> pianoKorgTriton        :: [(String, InstrumentName)]
> pianoKorgTriton = 
>   [
>       ("Piano Korg Triton",       AcousticGrandPiano)
>   ]
>
> glockenspiel        :: [(String, InstrumentName)]
> glockenspiel = 
>   [
>       ("Glockenspiel       ",     Glockenspiel)
>   ]
> -}
>
> doPlayInstruments      :: InstrMap (Mono AudRate) → IO ()
> doPlayInstruments imap
>   | traceAlways msg False = undefined
>   | otherwise = do
>       let (d,s) = renderSF (slot 2) imap
>       putStrLn ("duration=" ++ show d ++ " seconds")
>       outFileNorm "blaat.wav" d s
>       return ()
>   where
>     msg = unwords ["doPlayInstruments ", show $ length imap
>                             , " insts=", concatMap (show . fst) imap]
>
> gUnit :: Music Pitch
> gUnit = addDur qn [f 4, a 4, b 4, a 4, f 4, a 4, b 4, a 4
>                  , e 4, a 4, b 4, a 4, e 4, a 4, b 4, a 4]
>
> gUnitAtVolume          :: Volume → Music (Pitch, Volume)
> gUnitAtVolume vol = addVolume vol gUnit
>
> nylon :: Music (Pitch, Volume)
> nylon =
>   removeZeros
>   $ tempo 1
>   $ transpose 0
>   $ keysig A Major
>   $ instrument Glockenspiel
>     (line [gUnitAtVolume  40, rest hn, gUnitAtVolume  60, rest hn, gUnitAtVolume 80, rest hn
>          , gUnitAtVolume 100, rest hn, gUnitAtVolume 120])
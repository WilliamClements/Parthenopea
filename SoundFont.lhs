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
> import Data.Array.Unboxed
> import qualified Data.Audio           as A
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List (find, foldr, minimumBy)
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

> useEnvelopes = False
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
>         instruments ← doInstruments arrays
>         traceIO ("doInstruments returned " ++ show instruments ++ "\n")
>         return ()
>     putStrLn "leaving doSoundFont"
>
> doInstruments          :: SoundFontArrays → IO [InstrumentZones]
> doInstruments arrays = do
>   let is = ssInsts arrays
>   let (ist, ien) = bounds is 
>   putStrLn ("instrument start/stop=" ++ show (ist, ien) ++ "first instrument name=" ++ show (F.instName (is!ist)))
>
>   let selected = map (shouldDoInstrument is) [ist..ien-1]
>
>   putStrLn ("# selected=" ++ show (length selected))
>
>   let filtered = filter isJust selected
>
>   putStrLn ("# filtered=" ++ show (length filtered))
>
>   let ready = map (doInstrument arrays is) filtered
>
>   putStrLn ("# ready=" ++ show (length ready))
>   doPlayInstruments ready
>   return []
>
> shouldDoInstrument     :: Array Word F.Inst → Word → Maybe (InstrumentName, Word)
> shouldDoInstrument is n =
>   let i = is ! n
>       ilist = filter (match (F.instName i)) essentials -- WOX korgInstruments
>   in case ilist of
>     [] → Nothing
>     _  → Just (snd (head ilist), n)
>   where
>     match :: String → (String, InstrumentName) → Bool
>     match iname cand = iname == fst cand
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
>       | traceAlways msg False = undefined
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

> essentials             :: [(String, InstrumentName)]
> essentials =
>   [
>       ("Alto Sax XSwitch",        AltoSax)
>     , ("B3-1 Slow Rotor",         ReedOrgan)
>     , ("Bassoon",                 Bassoon)
>     , ("Cello",                   Cello)
>     , ("ChGrand01v1",             AcousticGrandPiano)
>     , ("Clarinet",                Clarinet)
>     , ("Clean Guitar",            ElectricGuitarClean)
>     , ("DX7 Rhodes",              RhodesPiano)
>     , ("F",                       Violin)
>     , ("Flute",                   Flute)
>     , ("Jazz Guitar",             ElectricGuitarJazz)
>     , ("MagiCs 5Strg Banjo",      Banjo)
>     , ("Nylon Guitar 1",          AcousticGuitarNylon)
>     , ("Oboe",                    Oboe)
>     , ("Palm Muted Guitar",       ElectricGuitarMuted)
>     , ("Piccolo",                 Piccolo)
>     , ("Pipe Organ",              ChurchOrgan)
>     , ("Spanish",                 AcousticGuitarSteel)
>     , ("String Ensembles",        StringEnsemble1)
>     , ("Strings Pan",             StringEnsemble2)
>     , ("Synth 1",                 SynthBass1)           -- discord
>     , ("Synth 2",                 SynthBass2)           -- discord
>     , ("Tenor Both Xfade",        TenorSax)
>     , ("Timpani 1 JN",            Timpani)
>     , ("Trombone",                Trombone)
>     , ("Trumpet",                 Trumpet)
>     , ("Tuba",                    Tuba)
>     , ("Upright-Piano-1",         BrightAcousticPiano)
>     -- , ("Violin3",            Violin)
>   ]
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
>
> doPlayInstruments      :: InstrMap (Mono AudRate) → IO ()
> doPlayInstruments imap
>   | traceAlways msg False = undefined
>   | otherwise = do
>       let (d,s) = renderSF basicLick imap
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
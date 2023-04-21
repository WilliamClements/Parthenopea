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
> import qualified Codec.SoundFont      as F
> import Control.Arrow
> import Control.Arrow.ArrowP ( ArrowP(ArrowP), strip )
> import qualified Control.SF.SF        as C
> import Covers
> import Data.Array.Unboxed
> import qualified Data.Audio           as A
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List (find, foldr)
> import Data.Maybe (isJust, fromJust, fromMaybe)
> import Debug.Trace ( traceIO, traceM )
> import Euterpea.IO.Audio.Basics
> import Euterpea.IO.Audio.IO
> import Euterpea.IO.Audio.Render
> import Euterpea.IO.Audio.Types
> import Euterpea.Music
> import Fanfare
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay) )
> import Parthenopea
> import SunPyg
> import System.Environment(getArgs)  
  
importing sampled sound (from SoundFont (*.sf2) file) =====================================

> ignoreZonePitchOverride = False
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
>   , zChorus            :: Maybe Int
>   , zReverb            :: Maybe Int
>   , zPan               :: Maybe Int
>   , zRootKey           :: Maybe Word} deriving Show
>
> defInstrumentZone      :: InstrumentZone
> defInstrumentZone = InstrumentZone Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing
>   
> data Reconciled =
>   Reconciled {
>      rStart             :: Word
>   ,  rEnd               :: Word
>   ,  rLoopStart         :: Word
>   ,  rLoopEnd           :: Word
>   ,  rRootKey           :: Word} deriving Show
>           
> data SoundFontArrays = 
>   SoundFontArrays {
>               ssInsts  :: Array Word F.Inst
>             , ssIBags  :: Array Word F.Bag
>             , ssIGens  :: Array Word F.Generator
>             , ssShdrs  :: Array Word F.Shdr
>             , ssData   :: A.SampleData Int16
>             , ssM24    :: Maybe (A.SampleData Int8)}
>
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
>   let selected = map (shouldDoInstrument is) [ist..ien-1]
>   let filtered = filter isJust selected
>   let ready = map (doInstrument arrays is) filtered
>   doPlayInstruments ready
>   return []
>
> shouldDoInstrument     :: Array Word F.Inst → Word → Maybe (InstrumentName, Word)
> shouldDoInstrument is n =
>   let i = is ! n
>       ilist = filter (match (F.instName i)) selectedInstruments
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
>   | otherwise = (iname, assignInstrument arrays (InstrumentZones ilist))
>   where 
>     (iname, n) = fromJust mis
>     iinst = is ! n
>     jinst = is ! (n + 1)
>     ibagi = F.instBagNdx iinst
>     jbagi = F.instBagNdx jinst
>     zones = [ibagi+1..jbagi-1]
>     ilist = map (doZone arrays iinst) zones
>     msg = unwords ["doInstrument=", show mis, " n= ", show (snd (fromJust mis)), " ", show iinst, " ", show jinst]
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
>   F.Chorus i                     → iz {zChorus =                   Just i}
>   F.Reverb i                     → iz {zReverb =                   Just i}
>   F.Pan i                        → iz {zPan =                      Just i}
>
>   F.RootKey w                    → iz {zRootKey =                  Just (fromIntegral w)}
>   _                              → iz
>
> doZone                 :: SoundFontArrays → F.Inst → Word → InstrumentZone
> doZone arrays iinst bagIndex
>   | traceIf msg False = undefined
>   | otherwise = fromGens defInstrumentZone gens
>   where
>     ibags = ssIBags arrays
>     xbag = ibags ! bagIndex
>     ybag = ibags ! (bagIndex + 1)
>     xgeni = F.genNdx xbag
>     ygeni = F.genNdx ybag
>     gens = getGens arrays iinst [xgeni..ygeni-1]
>
>     msg = unwords ["doZone xgeni=", show xgeni, " ygeni=", show ygeni, "\n"]
>
> getGens                :: SoundFontArrays -> F.Inst -> [Word] -> [F.Generator]
> getGens arrays iinst words
>   | traceIf msg False = undefined
>   | otherwise = gens
>   where
>     selected = map (doGenerator arrays iinst) words
>     filtered = filter isJust selected
>     gens = map fromJust filtered
>
>     msg = unwords ["getGenerators=", show words]
> 
> doGenerator :: SoundFontArrays → F.Inst → Word → Maybe F.Generator
> doGenerator arrays iinst zw = Just $ ssIGens arrays ! zw
>
> eutPhaser              :: InstrumentZone
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → AudSF () Double 
> eutPhaser zone secs sr iphs freqFactor =
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
> eutSampler             :: SoundFontArrays → Reconciled → Volume → AudSF Double Double
> eutSampler arrays reconciled vol =
>   let
>     (st, en)           :: (Word, Word) = (rStart reconciled, rEnd reconciled)
>     nc = 1 -- numChans (undefined :: u)
>     numS :: Double
>     numS = case ssM24 arrays of
>           Nothing → fromIntegral $ (en - st + 1) `div` nc
>           Just _  → error "24-bit not yet supported"
>   in proc pos → do
>     let amp            :: Double = fromIntegral vol / 100
>     let sampleAddress  :: Int = fromIntegral st + truncate (numS * pos)
>     outA ⤙ amp * fromIntegral (ssData arrays ! sampleAddress)
>
> checkReconcile     :: F.Shdr
>                       → InstrumentZone
>                       → Reconciled
>                       → Double
>                       → (Word, Word)
>                       → Bool
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
> assignInstrument       :: SoundFontArrays → InstrumentZones → Instr (Mono AudRate)
> assignInstrument arrays zones dur pch vol params =
>   let
>     (zone, shdr)       :: (InstrumentZone
>                          , F.Shdr)         = selectZone arrays zones pch vol
>     rData              :: Reconciled       = reconcile zone shdr
>     sr                 :: Double           = fromIntegral $ F.sampleRate shdr
>     ns                 :: Double           = fromIntegral $ rEnd rData - rStart rData + 1
>     secs               :: Double           = ns / sr
> 
>     (st, en) = if secs <= 2 * fromRational dur
>                then (rLoopStart rData, rLoopEnd rData)
>                else (rStart rData,     rEnd rData)
>
>     rData' = rData {rStart = st, rEnd = en}
>
>     -- ap = fromIntegral (rRootKey rData)
>     ok = checkReconcile shdr zone rData' secs (st, en)
>     ap = if not ok
>          then error "InstrumentZone and F.Shdr could not be reconciled"
>          else fromIntegral (rRootKey rData)
>
>     ns'                :: Double              = fromIntegral (en - st + 1)
>     secs'              :: Double              = ns' / sr
>     freqFactor         :: Double              = apToHz ap / apToHz pch
>     sig                :: AudSF () Double     = eutPhaser zone secs' sr 0 freqFactor
>                                             >>> eutSampler arrays rData' vol
>   in proc _ → do
>     z ← sig ⤙ ()
>     outA ⤙ z
>
> selectZone             :: SoundFontArrays
>                           → InstrumentZones
>                           → AbsPitch
>                           → Volume
>                           → (InstrumentZone, F.Shdr)
> selectZone arrays zones pch vol =
>   let
>     -- ToDo : do pick closest zone if pitch is out of range
>     mzone = find (contains pch vol) (zZones zones)
>     zone = case mzone of
>              Nothing     → head (zZones zones) -- error "all splits failed"
>                                                --       "to contain the pitch"
>              Just zone'  → zone'
>   in
>     (zone, ssShdrs arrays ! fromJust (zSampleIndex zone))
>   where
>     contains           :: AbsPitch → Volume → InstrumentZone → Bool
>     contains ap vol zone
>       | isJust (zKeyRange zone)
>         = if isJust (zVelRange zone)
>           then ap >= kst && ap <= ken && vol >= vst && vol <= ven
>           else ap >= kst && ap <= ken
>       | isJust (zVelRange zone)
>         = vol >= vst && vol <= ven
>       | otherwise
>         = True
>       where
>         (kst, ken) = fromJust (zKeyRange zone)
>         (vst, ven) = fromJust (zVelRange zone)
>
> sumOfMaybeInts         :: [Maybe Int] -> Int
> sumOfMaybeInts = foldr ((+) . fromMaybe 0) 0
>       
> addIntToWord           :: Word -> Int -> Word
> addIntToWord w i =
>   let iw               :: Int = fromIntegral w
>       sum              :: Int = iw + i
>   in fromIntegral sum
>
> reconcile              :: InstrumentZone -> F.Shdr -> Reconciled
> reconcile zone shdr =
>   Reconciled {
>     rStart      = addIntToWord (F.start shdr)           (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd        = addIntToWord (F.end shdr)             (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart  = addIntToWord (F.startLoop shdr)       (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd    = addIntToWord (F.endLoop shdr)         (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey    = fromMaybe    (F.originalPitch shdr)   (if ignoreZonePitchOverride
>                                                          then Nothing
>                                                          else zRootKey zone)}
>
> selectedInstruments    :: [(String, InstrumentName)]
> selectedInstruments =
>   [
>       ("Alto Sax XSwitch",   AltoSax)
>     , ("Bassoon",            Bassoon)
>     , ("Cello",              Cello)
>     , ("ChGrand01v1",        AcousticGrandPiano)
>     , ("Clarinet",           Clarinet)
>     , ("Clean Guitar",       ElectricGuitarClean)
>     , ("DX7 Rhodes",         RhodesPiano)
>     , ("Flute",              Flute)
>     , ("Jazz Guitar",        ElectricGuitarJazz)
>     , ("MagiCs 5Strg Banjo", Banjo)
>     , ("Nylon Guitar 1",     AcousticGuitarNylon)
>     , ("Oboe",               Oboe)
>     , ("Palm Muted Guitar",  ElectricGuitarMuted)
>     , ("Piccolo",            Piccolo)
>     , ("Pipe Organ",         ChurchOrgan)
>     , ("String Ensembles",   StringEnsemble1)
>     , ("Tenor Both Xfade",   TenorSax)
>     , ("Timpani 1 JN",       Timpani)
>     , ("Trombone",           Trombone)
>     , ("Trumpet",            Trumpet)
>     , ("Tuba",               Tuba)
>     , ("Upright-Piano-1",    BrightAcousticPiano)
>     , ("Violin3",            Violin)
>   ]
>
> doPlayInstruments      :: InstrMap (Mono AudRate) → IO ()
> doPlayInstruments imap
>   | traceIf msg False = undefined
>   | otherwise = do
>       let (d,s) = renderSF roger imap
>       outFileNorm "blaat.wav" d s
>       return ()
>   where
>     msg = unwords ["doPlayInstruments ", show $ length imap
>                             , " insts=", concatMap (show . fst) imap]
>
> gUnit :: Music Pitch
> gUnit = addDur qn [f 4, a 4, b 4, a 4, f 4, a 4, b 4, a 4
>                  , e 4, a 4, b 4, a 4, e 4, a 4, b 4, a 4]
> nylon :: Music (Pitch, Volume)
> nylon =
>   removeZeros
>   $ tempo 1
>   $ transpose 0
>   $ keysig A Major
>   $ chord [ addVolume  50 $ instrument AcousticGuitarNylon gUnit]
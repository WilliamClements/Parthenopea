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
> import Data.List (find)
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

> newtype InstrumentZones =
>   InstrumentZones {
>               ssZones  :: [InstrumentZone]} deriving Show
>
> data InstrumentZone =
>   InstrumentZone {
>     ssStartOffs        :: Maybe Int
>   , ssEndOffs          :: Maybe Int
>   , ssLoopStartOffs    :: Maybe Int
>   , ssLoopEndOffs      :: Maybe Int
>
>   , ssInstIndex        :: Maybe Word
>   , ssKeyRange         :: Maybe (AbsPitch, AbsPitch)
>   , ssSampleIndex      :: Maybe Word
>   , ssSampleMode       :: Maybe A.SampleMode
>
>   , ssRootKey          :: Maybe AbsPitch} deriving Show
>
> defInstrumentZone      :: InstrumentZone
> defInstrumentZone = InstrumentZone Nothing Nothing Nothing Nothing
>                                    Nothing Nothing Nothing Nothing
>                                    Nothing
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
> data SampleSpec = 
>   SampleSpec {
>               ssName   :: String
>             , ssRange  :: (Word, Word)
>             , ssLoop   :: (Word, Word)
>             , ssCount  :: Double
>             , ssRate   :: Double
>             , ssChans  :: Double
>             , ssFreq   :: Double} deriving Show
>
> initSampleSpec         :: String
>                            → (Word, Word)
>                            → (Word, Word)
>                            → Double
>                            → Double
>                            → Double
>                            → Double
>                            → SampleSpec
> initSampleSpec nm (st, en) (lst, len) cnt sr ch freq
>   | traceIf msg False = undefined
>   | otherwise = SampleSpec nm (st, en) (lst, len) cnt sr ch freq
>   where
>     msg = unwords ["initSampleSpec=", show $ SampleSpec nm (st, en) (lst, len) cnt sr ch freq]
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
>           Nothing      → print "16-bit"
>           Just s24data → print "24-bit"
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
>   print ("doInstruments filtered = " ++ show filtered)
>   print ("doInstruments ready length = " ++ show (length ready))
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
> checkInstrument        :: InstrumentZones → Word → Word → InstrumentZones
> checkInstrument ilist ibagi jbagi
>   | traceIf msg False = undefined
>   | otherwise = ilist
>   where
>     msg = unwords ["\ncheckInstrument =" , show ibagi, " ", show jbagi, "\n"]
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
>   F.StartAddressOffset i         → iz {ssStartOffs =          Just i}
>   F.EndAddressOffset i           → iz {ssEndOffs =            Just i}
>   F.LoopStartAddressOffset i     → iz {ssStartOffs =          Just i}
>   F.LoopEndAddressOffset i       → iz {ssEndOffs =            Just i}
>
>   F.InstIndex w                  → iz {ssInstIndex =          Just w}
>   F.KeyRange a b                 → iz {ssKeyRange =           Just (fromIntegral a, fromIntegral b)}
>   F.SampleIndex w                → iz {ssSampleIndex =        Just w}
>   F.SampleMode a                 → iz {ssSampleMode =         Just a}
>
>   F.RootKey w                    → iz {ssRootKey =            Just (fromIntegral w)}
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
> eutPhaser              :: SampleSpec
>                           → InstrumentZone
>                           → Double
>                           → Double
>                           → AudSF () Double 
> eutPhaser spec zone iphs freqFactor =
>   let frac             :: RealFrac r ⇒ r → r
>       frac                           = snd . properFraction
>       secs             :: Double     = ssCount spec / ssRate spec
>       delta            :: Double
>       delta                          = ssChans spec / (secs * freqFactor * ssRate spec)
>   in proc () → do
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> eutSampler             :: SoundFontArrays → SampleSpec → AudSF Double Double
> eutSampler arrays spec =
>   let
>     nc = 1 -- numChans (undefined :: u)
>     (st, en) = ssRange spec
>     numS :: Double
>     numS = case ssM24 arrays of
>           Nothing → fromIntegral $ (en - st + 1) `div` nc
>           Just _  → error "24-bit not yet supported"
>   in proc pos → do
>     outA ⤙ fromIntegral $ ssData arrays ! (fromIntegral st + truncate (numS * pos))
>
> assignInstrument       :: SoundFontArrays → InstrumentZones → Instr (Mono AudRate)
> assignInstrument arrays zones dur pch vol params =
>   let
>     (shdr, zone)       :: (F.Shdr
>                            , InstrumentZone)  = findClosestSplit arrays zones pch
>     (st, en)           :: (Word, Word)        = (F.start     shdr, F.end       shdr)
>     (lst, len)         :: (Word, Word)        = (F.startLoop shdr, F.endLoop   shdr)
>     sr                 :: Double              = fromIntegral $ F.sampleRate    shdr
>     sap                :: AbsPitch            = fromIntegral $ F.originalPitch shdr              
>     ap                 :: AbsPitch            = fromMaybe sap (ssRootKey zone)
>     ns                 :: Double              = fromIntegral (en - st + 1)
>     spec               :: SampleSpec          = initSampleSpec
>                                                   (F.sampleName shdr)
>                                                   (st, en)
>                                                   (lst, len)
>                                                   ns
>                                                   sr
>                                                   1
>                                                   $ apToHz ap 
>     freqFactor         :: Double              = ssFreq spec / apToHz  pch
>     sig                :: AudSF () Double     = eutPhaser spec zone 0 freqFactor >>> eutSampler arrays spec
>   in proc _ → do
>     z ← sig ⤙ ()
>     outA ⤙ z
>
> findClosestSplit       :: SoundFontArrays → InstrumentZones → AbsPitch → (F.Shdr, InstrumentZone)
> findClosestSplit arrays zones pch =
>   let
>     mzone = find (contained pch) (ssZones zones)
>     zone = case mzone of
>              Nothing     → head (ssZones zones) -- error "all splits failed to contain the pitch"
>              Just zone'  → zone'
>   in
>     (ssShdrs arrays ! fromJust (ssSampleIndex zone), zone)
>   where
>     contained          :: AbsPitch → InstrumentZone → Bool
>     contained ap zone =
>       let (pst, pen) = fromJust $ ssKeyRange zone
>       in ap >= pst && ap <= pen
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
>       let (d,s) = renderSF basicLick imap
>       outFileNorm "blaat.wav" d s
>       return ()
>   where
>     msg = unwords ["doPlayInstruments ", show $ length imap, " insts=", concatMap (show . fst) imap]
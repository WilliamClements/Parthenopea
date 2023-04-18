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
> import Control.Lens
> import qualified Control.SF.SF        as C
> import Data.Array.Unboxed
> import qualified Data.Audio           as A
> import Data.Colour
> import Data.Colour.Names
> import Data.Default.Class
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List (find)
> import Data.Maybe (isJust, fromJust)
> import Debug.Trace ( traceIO, traceM )
> import Euterpea
> import Fanfare
> import FRP.UISF.AuxFunctions
> import HSoM
> import Parthenopea
> import System.Environment(getArgs)  
  
importing sampled sound (from SoundFont (*.sf2) file) =====================================

> newtype InstrumentZones =
>   InstrumentZones {
>               ssZones  :: [InstrumentZone]} deriving Show
>
> data InstrumentZone =
>   InstrumentZone {
>           ssSampleIx   :: Word
>         , ssPchRange   :: (AbsPitch, AbsPitch)  
>         , ssGenerators :: [F.Generator]} deriving Show
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
>             , ssCount  :: Double
>             , ssRate   :: Double
>             , ssChans  :: Double
>             , ssFreq   :: Double} deriving Show
>   
> initSampleSpec         :: String
>                            → (Word, Word)
>                            → Double
>                            → Double
>                            → Double
>                            → Double
>                            → SampleSpec
> initSampleSpec nm (st, en) cnt sr ch freq
>   | traceAlways msg False = undefined
>   | otherwise = SampleSpec nm (st, en) cnt sr ch freq
>   where
>     msg = unwords ["initSampleSpec=", show $ SampleSpec nm (st, en) cnt sr ch freq]
>
> eutPhaser              :: SampleSpec
>                           → Double
>                           → Double
>                           → AudSF () Double 
> eutPhaser spec iphs freqFactor =
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
>         traceIO ("doInstruments returned " ++ show instruments)
>         return ()
>         -- let shdrs = F.shdrs (F.pdta soundFont)
>         -- let (sst, sen) = bounds shdrs
>         -- let selected = map (shouldDoSample shdrs) [sst..sen]
>         -- let filtered = filter isJust selected
>         -- print filtered
>         -- let imap = map (extractSample arrays shdrs) filtered
>         -- doPlayInstruments imap
>     putStrLn "leaving doSoundFont"
>
> doInstruments          :: SoundFontArrays → IO [InstrumentZones]
> doInstruments arrays = do
>   let is = ssInsts arrays
>   let (ist, ien) = bounds is 
>   let selected = map (shouldDoInstrument is) [ist..ien-1]
>   let filtered = filter isJust selected
>   print ("doInstruments filtered = " ++ show filtered)
>   return $ map (doInstrument arrays is) filtered
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
> checkInstrument        :: InstrumentZones → InstrumentZones
> checkInstrument zones
>   | traceAlways msg False = undefined
>   | otherwise = zones
>   where
>     msg = unwords ["\ncheckInstrument =" , show zones, "\n"]
>
> doInstrument           :: SoundFontArrays
>                           → Array Word F.Inst
>                           → Maybe (InstrumentName, Word)
>                           → InstrumentZones
> doInstrument arrays is mis =
>   let (iname, n) = fromJust mis
>       iinst = is ! n
>       jinst = is ! (n + 1)
>       ibagi = F.instBagNdx iinst
>       jbagi = F.instBagNdx jinst
>       zones = [ibagi..jbagi-1]
>       ilist = map (doZone arrays iinst) zones
>   in checkInstrument $ InstrumentZones ilist
>     
>     -- instr = toInstr arrays spec
>
> doZone                 :: SoundFontArrays → F.Inst → Word → InstrumentZone
> doZone arrays iinst w =
>   let
>     ibags = ssIBags arrays
>     n = F.instBagNdx iinst
>     zbag = ibags ! n
>     ybag = ibags ! (n + 1)
>     zgeni = F.genNdx zbag
>     ygeni = F.genNdx ybag
>     selected = map (doGenerator arrays iinst) [zgeni..ygeni-1]
>     filtered = filter isJust selected
>   in InstrumentZone 0 {- WOX -} (5, 100) $ map fromJust filtered
>
> doGenerator            :: SoundFontArrays → F.Inst → Word → Maybe F.Generator
> doGenerator arrays iinst zw =
>   let generator = ssIGens arrays ! zw
>   in case generator of
>     F.KeyRange a b       →  Just generator 
>     F.SampleIndex w      →  Just generator
>     F.SampleMode m       →  Just generator
>     F.InstIndex i        →  Just generator
>     F.RootKey k          →  Just generator
>     _                    →  Nothing
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
>     shdr               :: F.Shdr              = findClosestSplit arrays zones pch
>     (st, en)           :: (Word, Word)        = (F.start shdr, F.end shdr)
>     sr                 :: Double              = fromIntegral $ F.sampleRate shdr
>     ap                 :: AbsPitch            = fromIntegral $ F.originalPitch shdr              
>     ns                 :: Double              = fromIntegral (en - st + 1)
>     spec               :: SampleSpec          = initSampleSpec (F.sampleName shdr) (st, en) ns sr 1 $ apToHz ap 
>     freqFactor         :: Double              = ssFreq spec / apToHz pch
>     sig                :: AudSF () Double     = eutPhaser spec 0 freqFactor >>> eutSampler arrays spec
>   in proc _ → do
>     z ← sig ⤙ ()
>     outA ⤙ z
>
> findClosestSplit       :: SoundFontArrays → InstrumentZones → AbsPitch → F.Shdr
> findClosestSplit arrays zones pch = {- WOX ssShdrs arrays ! 0 -}
>   let
>     mzone = find (contained pch) (ssZones zones)
>     zone = case mzone of
>              Nothing     → error "replace this error by falling back on Global"
>              Just zone'  → zone'
>   in
>     ssShdrs arrays ! ssSampleIx zone
>   where
>     contained          :: AbsPitch → InstrumentZone → Bool
>     contained ap zone =
>       let (pst, pen) = ssPchRange zone
>       in ap >= pst && ap <= pen
>   
> toInstr                :: SoundFontArrays → SampleSpec → Instr (Mono AudRate)
> toInstr arrays spec dur pch vol params =
>   let
>     freqFactor :: Double
>     freqFactor = ssFreq spec / apToHz pch
>     sig :: AudSF () Double
>     sig = eutPhaser spec 0 freqFactor >>> eutSampler arrays spec
>   in proc _ → do
>      z ← sig ⤙ ()
>      outA ⤙ z
>
> shouldDoSample          :: Array Word F.Shdr → Word → Maybe (InstrumentName, Word)
> shouldDoSample shdrs n =
>   let shdr = shdrs ! n
>       ilist = filter (match (F.sampleName shdr)) selectedSamples
>   in case ilist of
>     [] → Nothing
>     _  → Just (snd (head ilist), n)
>   where
>     match :: String → (String, InstrumentName) → Bool
>     match sname cand = sname == fst cand
>
> extractSample          :: SoundFontArrays
>                           → Array Word F.Shdr
>                           → Maybe (InstrumentName, Word)
>                           → (InstrumentName, Instr (Mono AudRate))
> extractSample arrays shdrs mis
>   | traceAlways msg False = undefined
>   | otherwise = (iname, instr)
>   where
>     (iname, n) = fromJust mis
>     shdr = shdrs ! n
>     (st, en) :: (Word, Word)                  = (F.start shdr, F.end shdr)
>     sr       :: Double                        = fromIntegral $ F.sampleRate shdr
>     ap       :: AbsPitch                      = fromIntegral $ F.originalPitch shdr              
>     ns       :: Double                        = fromIntegral (en - st + 1)
>     spec     :: SampleSpec                    = initSampleSpec (F.sampleName shdr) (st, en) ns sr 1 $ apToHz ap 
>     instr = toInstr arrays spec
>     msg = unwords ["extractSample ", show shdr]
>
> selectedInstruments    :: [(String, InstrumentName)]
> selectedInstruments =
>   [
>       ("Alto Sax XSwitch",   AltoSax)
>     , ("Cello",              Cello)
>     , ("Flute",              Flute)
>     , ("Oboe",               Oboe)
>     , ("Trumpet",            Trumpet)
>     , ("Violin3",            Violin)
>   ]
>
> doPlayInstruments      :: InstrMap (Mono AudRate) → IO ()
> doPlayInstruments imap
>   | traceAlways msg False = undefined
>   | otherwise = do
>     let m = pendingtonArnt 1
>     let (d,s) = renderSF m imap
>     outFileNorm "blaat.wav" d s
>     return ()
>   where
>     msg = unwords ["InstrMap ", show $ length imap, " insts=", concatMap (show . fst) imap]
>
> selectedSamples        :: [(String, InstrumentName)]
> selectedSamples =
>   [
>       ("Oboe-A4",            Oboe)
>     , ("Trumpet-D5",         Trumpet)
>     , ("Violin f 56(L)",     Violin)
>     , ("Banjo-A4-M-44",      Banjo)
>     , ("Tenor Hard A5 1",    TenorSax)
>     , ("Clean Guitar-D3",    AcousticGuitarSteel)
>     , ("Cello C3",           Contrabass)
>   ]
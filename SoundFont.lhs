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
> import Data.Maybe (isJust, fromJust)
> import Debug.Trace ( traceIO, traceM )
> import Euterpea
> import Fanfare
> import FRP.UISF.AuxFunctions
> import HSoM
> import Parthenopea
> import System.Environment(getArgs)  

importing sampled sound (from SoundFont (*.sf2) file) =====================================

> data SampleArrays = 
>   SampleArrays {
>               ssInsts  :: Array Word F.Inst
>             , ssIBags  :: Array Word F.Bag
>             , ssIGens  :: Array Word F.Generator
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
>   | traceIf msg False = undefined
>   | otherwise = SampleSpec nm (st, en) cnt sr ch freq
>   where
>     msg = unwords ["initSampleSpec=", show $ SampleSpec nm (st, en) cnt sr ch freq]

> phaser                 :: SampleSpec
>                           → Double
>                           → Double
>                           → AudSF () Double 
> phaser spec iphs freqFactor =
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
>         let arrays = SampleArrays (F.insts pdata)
>                                   (F.ibags pdata)
>                                   (F.igens pdata)
>                                   (F.smpl sdata)
>                                   (F.sm24 sdata)
>         case ssM24 arrays of
>           Nothing      → print "16-bit"
>           Just s24data → print "24-bit"
>         doInstruments arrays
>         -- let shdrs = F.shdrs (F.pdta soundFont)
>         -- let (sst, sen) = bounds shdrs
>         -- let selected = map (shouldDoSample shdrs) [sst..sen]
>         -- let filtered = filter isJust selected
>         -- print filtered
>         -- let imap = map (extractSample arrays shdrs) filtered
>         -- doPlayInstruments imap
>     putStrLn "leaving doSoundFont"
>
> doInstruments          :: SampleArrays → IO ()
> doInstruments arrays = do
>   let is = ssInsts arrays
>   let (ist, ien) = bounds is 
>   let selected = map (shouldDoInstrument is) [ist..ien-1]
>   let filtered = filter isJust selected
>   print ("doInstruments filtered = " ++ show filtered)
>   let imap = map (doInstrument arrays is) filtered
>   print ("doInstruments = " ++ show imap ++ "\n")
>   -- print imap
>   return ()
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
> doInstrument           :: SampleArrays
>                           → Array Word F.Inst
>                           → Maybe (InstrumentName, Word)
>                           → [[F.Generator]]
> doInstrument arrays is mis
>   | traceAlways msg False = undefined
>   | otherwise = ilist
>   where
>     (iname, n) = fromJust mis
>     iinst = is ! n
>     jinst = is ! (n + 1)
>     ibagi = F.instBagNdx iinst
>     jbagi = F.instBagNdx jinst
>     zones = [ibagi..jbagi-1]
>     ilist = map (doZone arrays iinst) zones
>     m = F.genNdx
>     -- instr = toInstr arrays spec
>     msg = unwords ["doInstument ", show (iname, n), " ", show zones, " ", show (length ilist)]
>
> doZone                 :: SampleArrays → F.Inst → Word → [F.Generator]
> doZone arrays iinst w
>   | traceAlways msg False = undefined
>   | otherwise = jlist
>   where
>     ibags = ssIBags arrays
>     n = F.instBagNdx iinst
>     zbag = ibags ! n
>     ybag = ibags ! (n + 1)
>     zgeni = F.genNdx zbag
>     ygeni = F.genNdx ybag
>     jlist = map (doGenerator arrays iinst) [zgeni..ygeni-1]
>     msg = unwords ["doZone ", show w]
>
> {-
> doInstrument          :: SampleArrays → Word → IO ()
> doInstrument arrays n = do
>   let is = ssInsts arrays
>   let iinst = is ! n
>   let jinst = is ! (n + 1)
>   let ibag = (ssIBags arrays) ! (F.instBagNdx iinst)
>   let jbag = (ssIBags arrays) ! (F.instBagNdx jinst)
>   let (gst, gen) = (F.genNdx ibag, F.genNdx jbag)
>   print iinst
>   print (gst, gen)
>   mapM_ (doGenerator arrays iinst) [gst..gen-1]
>
> doZones                :: SampleArrays → F.Inst → (Word, Word) → IO ()
> doZones arrays iinst (zst, zen) = do
>   mapM_ (doZone arrays iinst) [zst..zen-1]
> -}
>
> doGenerator            :: SampleArrays → F.Inst → Word → F.Generator
> doGenerator arrays iinst zw = ssIGens arrays ! zw
> {-
>   let generator = 
>   case generator of
>     F.KeyRange a b       →  do
>                               traceIO ("KeyRange=" ++ show a ++ "," ++ show b)
>     F.SampleIndex w      →  do 
>                               traceIO ("SampleIndex=" ++ show w)
>     F.SampleMode m       →  do 
>                               traceIO ("SampleMode=" ++ show m)
>     F.InstIndex i        →  do 
>                               traceIO ("InstIndex=" ++ show i)
>     F.RootKey k          →  do 
>                               traceIO ("RootKey=" ++ show k)
>     _                    →  return ()
>
>   -- print generator
>   return ()
>
> -}
> sampler                :: SampleArrays → SampleSpec → AudSF Double Double
> sampler arrays spec =
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
> toInstr                :: SampleArrays → SampleSpec → Instr (Mono AudRate)
> toInstr arrays spec dur pch vol params =
>   let
>     freqFactor :: Double
>     freqFactor = ssFreq spec / apToHz pch
>     sig :: AudSF () Double
>     sig = phaser spec 0 freqFactor >>> sampler arrays spec
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
> extractSample          :: SampleArrays
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
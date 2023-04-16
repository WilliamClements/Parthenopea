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
> import Euterpea
> import FRP.UISF.AuxFunctions
> import HSoM
> import Parthenopea
> import System.Environment(getArgs)
>

importing sampled sound (from SoundFont (*.sf2) file) =====================================

> data SampleSpec = 
>   SampleSpec { ssData     :: A.SampleData Int16
>                , ssM24    :: Maybe (A.SampleData Int8)
>                , ssRange  :: (Int, Int)
>                , ssCount  :: Double
>                , ssRate   :: Double
>                , ssChans  :: Double
>                , ssFreq   :: Double} deriving Show
>   
> getSFForPhase          ::      SampleSpec
>                              → Double
>                              → Double
>                              → ArrowP C.SF AudRate () Double 
> getSFForPhase spec iphs freqFactor =
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
>         let sdata = F.smpl (F.sdta soundFont)
>         let m24 = F.sm24 (F.sdta soundFont)
>         case m24 of
>           Nothing → print "16-bit"
>           Just s24data → do
>             print "24-bit"
>         let shdrs = F.shdrs (F.pdta soundFont)
>         let (sst, sen) = bounds shdrs
>         let upper = min 2000 sen
>         mapM_ (doSampleHeader sdata m24 shdrs) [0..upper]
>     putStrLn "leaving doSoundFont"
>
> doSampleHeader         ::      A.SampleData Int16
>                              → Maybe (A.SampleData Int8)
>                              → Array Word F.Shdr
>                              → Word → IO ()
> doSampleHeader sdata m24 shdrs n = do
>   let shdr                                      = shdrs ! n
>   let (st, en) :: (Int, Int)                    = (fromIntegral (F.start shdr), fromIntegral (F.end shdr))
>   let sr       :: Double                        = fromIntegral $ F.sampleRate shdr
>   let ap       :: AbsPitch                      = fromIntegral $ F.originalPitch shdr              
>   let ns       :: Double                        = fromIntegral (en - st + 1)
>   let secs     :: DeltaT                        = ns / sr
>   let spec     :: SampleSpec                    = SampleSpec sdata m24 (st, en) ns sr 1 $ apToHz ap 
>   let sig      :: ArrowP C.SF AudRate () Double = getSFForPhase spec 0 1 >>> sampler sdata m24 (st, en)
>   let match    :: Bool                          = "Oboe-A4" == F.sampleName shdr
>   if match then do
>              -- print spec
>              print ap
>              putStrLn "match"
>              doPlayInstrument spec
>              outFileNorm "bloody.wav" secs sig
>            else do
>              return ()
>   return ()
>
> sampler                ::     A.SampleData Int16
>                             → Maybe (A.SampleData Int8)
>                             → (Int, Int)
>                             → ArrowP C.SF AudRate Double Double
> sampler sdt m24 (st, en) =
>   let
>     nc = 1 -- numChans (undefined :: u)
>     numS :: Double
>     numS = case m24 of
>           Nothing → fromIntegral $ (en - st + 1) `div` nc
>           Just _  → error "24-bit not yet supported"
>   in proc pos → do
>     outA ⤙ fromIntegral $ sdt ! (st + truncate (numS * pos))
>
> toInstr                :: SampleSpec → Instr (Mono AudRate)
> toInstr spec dur pch vol params           =
>   let
>     freqFactor :: Double
>     -- freqFactor = apToHz pch / ssFreq spec
>     freqFactor = ssFreq spec / apToHz pch
>     sig :: ArrowP C.SF AudRate () Double
>     sig = getSFForPhase spec 0 freqFactor
>            >>> sampler (ssData spec) (ssM24 spec) (ssRange spec)
>   in proc _ → do
>     z ← sig ⤙ ()
>     outA ⤙ z
>     
> doPlayInstrument       :: SampleSpec → IO ()
> doPlayInstrument spec = do
>   let imap :: InstrMap (Mono AudRate)
>       imap = [(Violin, toInstr spec)]
>       m = instrument Violin pSnippet02
>       (d,s) = renderSF m imap
>   outFileNorm "blaat.wav" d s
>   return ()

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
> import Euterpea
> import Fanfare
> import FRP.UISF.AuxFunctions
> import HSoM
> import Parthenopea
> import System.Environment(getArgs)
>

importing sampled sound (from SoundFont (*.sf2) file) =====================================

> data SampleSpec = 
>   SampleSpec { ssData     :: A.SampleData Int16
>                , ssM24    :: Maybe (A.SampleData Int8)
>                , ssRange  :: (Word, Word)
>                , ssCount  :: Double
>                , ssRate   :: Double
>                , ssChans  :: Double
>                , ssFreq   :: Double} deriving Show
>   
> getSFForPhase          :: SampleSpec
>                           → Double
>                           → Double
>                           → ArrowP C.SF AudRate () Double 
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
>         let imap = map (extractFromHeader sdata m24 shdrs)
>                    $ filter isJust
>                    $ map (shouldExtract shdrs) [sst..sen-1]
>         doPlayInstruments imap
>     putStrLn "leaving doSoundFont"
>
> sampler                :: SampleSpec → ArrowP C.SF AudRate Double Double
> sampler spec =
>   let
>     nc = 1 -- numChans (undefined :: u)
>     (st, en) = ssRange spec
>     numS :: Double
>     numS = case ssM24 spec of
>           Nothing → fromIntegral $ (en - st + 1) `div` nc
>           Just _  → error "24-bit not yet supported"
>   in proc pos → do
>     outA ⤙ fromIntegral $ ssData spec ! (fromIntegral st + truncate (numS * pos))
>
> toInstr                :: SampleSpec → Instr (Mono AudRate)
> toInstr spec dur pch vol params           =
>   let
>     freqFactor :: Double
>     freqFactor = ssFreq spec / apToHz pch
>     sig :: ArrowP C.SF AudRate () Double
>     sig = getSFForPhase spec 0 freqFactor >>> sampler spec
>   in proc _ → do
>     z ← sig ⤙ ()
>     outA ⤙ z
>
> shouldExtract :: Array Word F.Shdr -> Word -> Maybe (InstrumentName, Word)
> shouldExtract shdrs n =
>   let shdr = shdrs ! n
>       ilist = filter (match (F.sampleName shdr)) theExtractMap
>   in case ilist of
>     [] -> Nothing
>     _  -> Just (snd (head ilist), n)
>   where
>     match :: String -> (String, InstrumentName) -> Bool
>     match sname cand = sname == fst cand
>
> extractFromHeader :: A.SampleData Int16 → Maybe (A.SampleData Int8) -> Array Word F.Shdr -> Maybe (InstrumentName, Word) -> (InstrumentName, Instr (Mono AudRate))
> extractFromHeader sdata m24 shdrs mis = (iname, instr)
>   where
>     (iname, n) = fromJust mis
>     shdr = shdrs ! n
>     (st, en) :: (Word, Word)                  = (F.start shdr, F.end shdr)
>     sr       :: Double                        = fromIntegral $ F.sampleRate shdr
>     ap       :: AbsPitch                      = fromIntegral $ F.originalPitch shdr              
>     ns       :: Double                        = fromIntegral (en - st + 1)
>     spec     :: SampleSpec                    = SampleSpec sdata m24 (st, en) ns sr 1 $ apToHz ap 
>     instr = toInstr spec
>
> theExtractMap :: [(String, InstrumentName)]
> theExtractMap =
>   [
> -- remotely appropriate
>       ("Oboe-A4",        Oboe)
>     , ("Trumpet-D5",     Trumpet)
>     , ("Violin f 56(L)", Violin)
>     , ("Banjo-A4-M-44",  Banjo)
> -- completely bogus
>     , ("Trumpet-D5",     TenorSax)
>     , ("Banjo-A4-M-44",  AcousticGuitarSteel)
>     , ("Cello C3",       Contrabass)
>   ]
>
> doPlayInstruments      :: InstrMap (Mono AudRate) → IO ()
> doPlayInstruments imap = do
>   let m = pendingtonArnt 4
>   let (d,s) = renderSF m imap
>   outFileNorm "blaat.wav" d s
>   return ()
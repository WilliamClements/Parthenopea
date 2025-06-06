> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE UnicodeSyntax #-}

SynthesizerTest
William Clements
March 2, 2024

> module Parthenopea.Repro.SynthesizerTest ( synthesizerTests ) where
>
> import qualified Data.Audio              as A
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Types ( Clock(..), AudRate, Signal )
> import Parthenopea.Debug ( aEqual )
> import Parthenopea.Music.Siren ( SlwRate, toSampleDubs )
> import Parthenopea.Repro.Modulation ( NoteOn(NoteOn), defModulation )
> import Parthenopea.Repro.Synthesizer ( eutDriver, Effects(Effects), Recon(Recon), TimeFrame(..) )
  
Synthesizer-related tests =============================================================================================

> defRecon               :: Recon
> defRecon                                 =
>   Recon
>     A.ContLoop 44100 0 0 0 0
>     60 Nothing Nothing 100
>     (NoteOn 100 60)
>     0 Nothing Nothing
>     defModulation defEffects
>
> defEffects             :: Effects
> defEffects                               = Effects 0 0 0
>
> driveDriver            :: ∀ p. Clock p ⇒ Signal p () Double
> driveDriver                              = proc _ → do
>   x                                      ← eutDriver (TimeFrame 2 1 1 True) defRecon 0.1 ⤙ ()
>   outA                                                                                 ⤙ x
> 
> checkSignal            :: ∀ p. Clock p ⇒ Double → Signal p () Double → IO Bool
> checkSignal nSecs sf                     = do
>   let ss                                 = toSampleDubs nSecs sf
>   let sr                                 = rate (undefined :: p)
>   print $ length ss
>   print $ maximum $ map abs ss
>   return $ aEqual (truncate (nSecs * sr)) (length ss)
> 
> testSlw, testAud       :: Double → IO Bool
>
> synthesizerTests       :: [IO Bool]
> synthesizerTests                         = [testSlw 1, testSlw 10, testAud 1, testAud 10]
>
> testSlw nSecs                            = do
>   let sf :: Signal SlwRate () Double     = driveDriver
>   checkSignal nSecs sf
>   
> testAud nSecs                            = do
>   let sf :: Signal AudRate () Double     = driveDriver
>   checkSignal nSecs sf

The End
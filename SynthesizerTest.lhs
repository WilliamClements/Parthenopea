> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module SynthesizerTest where
>
> import qualified Data.Audio              as A
> import Debug.Trace ( traceIO )
> import Euterpea.IO.Audio.Basics
> import Euterpea.IO.Audio.Types
> import Modulation
> import Parthenopea
> import Synthesizer
  
Testing ===============================================================================================================

> defRecon                                 =
>   Recon
>     A.ContLoop 44100 0 0 0 0
>     60 Nothing Nothing
>     (NoteOn 100 60)
>     0 Nothing Nothing
>     defModulation defEffects
>
> defEffects                               = Effects 0 0 0
>
> driveDriver            :: ∀ p. Clock p ⇒ Signal p () Double
> driveDriver                              = proc _ → do
>   (x, (_, _))                            ← eutDriver 1 (defRecon, defRecon) 1 0.1 True  ⤙ ()
>   outA                                                                                  ⤙ x
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
> synthesizerTests       :: [IO Bool]      = [testSlw 1, testSlw 10, testAud 1, testAud 10]
>
> testSlw nSecs                            = do
>   let sf :: Signal SlwRate () Double     = driveDriver
>   checkSignal nSecs sf
>   
> testAud nSecs                            = do
>   let sf :: Signal AudRate () Double     = driveDriver
>   checkSignal nSecs sf
>   
>
> runSynthesizerTests                       = runTests synthesizerTests

The End
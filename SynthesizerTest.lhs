> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE UnicodeSyntax #-}

SynthesizerTest
William Clements
March 2, 2024

> module SynthesizerTest ( runSynthesizerTests, synthesizerTests ) where
>
> import qualified Data.Audio              as A
> import Euterpea.IO.Audio.Basics
> import Euterpea.IO.Audio.Types
> import Modulation
> import Parthenopea.Siren
> import Synthesizer ( eutDriver, Effects(Effects), Recon(Recon) )
  
Testing ===============================================================================================================

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
>   x                                      ← eutDriver 1 defRecon 1 0.1 True  ⤙ ()
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
>   
>
> runSynthesizerTests    :: IO ()
> runSynthesizerTests                       = runTests synthesizerTests

The End
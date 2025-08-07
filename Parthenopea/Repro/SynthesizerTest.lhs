> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

SynthesizerTest
William Clements
March 2, 2024

> module Parthenopea.Repro.SynthesizerTest ( synthesizerTests, passageTests ) where
>
> import qualified Data.Audio              as A
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Types ( Clock(..), AudRate, Signal )
> import Euterpea.Music
> import Parthenopea.Debug ( aEqual )
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Modulation ( NoteOn(NoteOn), defModulation )
> import Parthenopea.Repro.Synthesizer ( eutDriver, Effects(Effects), Recon(Recon), TimeFrame(..) )
  
Synthesizer-related tests =============================================================================================

> defRecon               :: Recon
> defRecon                                 =
>   Recon
>     A.ContLoop 44100 0 0 0 0
>     60 Nothing Nothing 100
>     Nothing
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
>
> passageTests           :: [IO Bool]
> passageTests                             = [singleIsSingle
>                                           , closedMarkingsLeaveNoNothings]
>
> testMeks               :: [MekNote]
> testMeks                                 = [ makeMekNote (Note wn (Af, 3)) 0
>                                            , makeMekNote (Rest hn) 1
>                                            , makeMekNote (Note wn (Bf, 3))  2]
>
> testMarkingses         :: [[Marking]]
> testMarkingses                           =
>   [ [Mark2 PP FF]
>   , [Mark PP, Mark FF]
>   , [Mark PP, SpanN 1, Mark FF]
>   -- , [Mark PP, SpanN 1]
>   , [Mark PP, Mark P, Mark FF]
>   ]
>
> vnodesHaveNoNothings   :: [VelocityNode] → Bool
> vnodesHaveNoNothings vns                 = not $ Nothing `elem` map vBefore vns || Nothing `elem` map vAfter vns
>
> singleIsSingle         :: IO Bool
> singleIsSingle                           = do
>   return $ aEqual
>              [VelocityNode (Just (PP, 0)) (Just (FF, 0))]
>              (compileMarkings testMeks [Mark2 PP FF])
>
> closedMarkingsLeaveNoNothings
>                        :: IO Bool
> closedMarkingsLeaveNoNothings            = do
>   mapM_ output testMarkingses
>   return $ all allGood testMarkingses
>   where
>     allGood markings                     = vnodesHaveNoNothings $ compileMarkings testMeks markings
>     output vn                            = print $ compileMarkings testMeks vn

The End 
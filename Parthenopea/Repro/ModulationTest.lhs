> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE UnicodeSyntax #-}

ModulationTest
William Clements
November 24, 2023

> module Parthenopea.Repro.ModulationTest ( modulationTests, runModulationTests ) where
>
> import Control.Exception
> import Data.Either
> import Data.List ( nub, sort, sortOn )
> import qualified Data.Map                as Map
> import Data.Ord
> import Parthenopea.Debug
> import Parthenopea.Repro.Modulation
> import Parthenopea.SoundFont.SFSpec

Testing ===============================================================================================================

> countSurvivingMods     :: [Modulator] → IO Int
> countSurvivingMods m8rs                  = do
>   let !count                             =
>         sum $ map length $ Map.elems $ mModsMap $ resolveMods defModulation m8rs []
>   return count
>
> vanillaModulatorWillNotBeEliminated
>   , unlinkedModulatorWillBeEliminated
>   , linkedModulatorWillNotBeEliminated
>   , danglingModulatorWillBeEliminated
>   , mutuallyCyclicModulatorsWillBeRejected
>   , repeatedNoteOnsGiveSameResult
>   , positiveEvaluationsIncrease
>   , negativeEvaluationsDecrease
>   , supercededModulatorWillBeEliminated
>   , unsupercededModulatorWillNotBeEliminated
>   , canonicalOrderingWorks
>                        :: IO Bool
>
> modulationTests        :: [IO Bool]
> modulationTests                          = [vanillaModulatorWillNotBeEliminated
>                                         ,   unlinkedModulatorWillBeEliminated
>                                         ,   linkedModulatorWillNotBeEliminated
>                                         ,   danglingModulatorWillBeEliminated
>                                         ,   mutuallyCyclicModulatorsWillBeRejected
>                                         ,   repeatedNoteOnsGiveSameResult
>                                         ,   positiveEvaluationsIncrease
>                                         ,   negativeEvaluationsDecrease
>                                         ,   supercededModulatorWillBeEliminated
>                                         ,   unsupercededModulatorWillNotBeEliminated
>                                         ,   canonicalOrderingWorks]
>
> runModulationTests     :: IO ()
> runModulationTests                       = runTests modulationTests
>
> vanillaModulatorWillNotBeEliminated      = do
>   let m8r                                = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModDest = ToFilterFc}
>   count                                  ← countSurvivingMods [m8r]
>   return $ aEqual count 1
>
> unlinkedModulatorWillBeEliminated        = do
>   let m8r                                = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToFilterFc}
>   count                                  ← countSurvivingMods [m8r]
>   return $ aEqual count 0
>
> linkedModulatorWillNotBeEliminated       = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToFilterFc}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModDest = ToLink 0}
>   count                                  ← countSurvivingMods [m8r0, m8r1]
>   return $ aEqual count 2
>
> danglingModulatorWillBeEliminated        = do
>   let m8r                                = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModDest = ToLink 77}
>   count                                  ← countSurvivingMods [m8r]
>   return $ aEqual count 0
>
> mutuallyCyclicModulatorsWillBeRejected   = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToLink 1}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToLink 0}
>   result::(Either ErrorCall Int)         ← try $ countSurvivingMods [m8r0, m8r1]
>   return $ isLeft result
>
> repeatedNoteOnsGiveSameResult            = do
>   let m8r                                = defModulator{
>                                              mrModId = 0
>                                            , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                            , mrModAmount = 0.5
>                                            , mrModDest = ToFilterFc}
>   let graph                              = compileMods [m8r]
>
>   let eval                               = evaluateMods ToFilterFc graph 
>   let results                            = [eval (NoteOn x 64) | x ← [0..127]]
>   return $ aEqual True $ all (==0.25) results
>
> positiveEvaluationsIncrease              = do
>   let m8r                                = defModulator{mrModId = 0
>                                                       , mrModSrc = defModSrc{
>                                                           msSource = FromNoteOnKey
>                                                         , msMapping = defMapping{msContinuity = Concave}}
>                                                       , mrModAmount = 0.5
>                                                       , mrModDest = ToFilterFc}
>   let graph                              = compileMods [m8r]
>
>   let eval                               = evaluateMods ToFilterFc graph 
>   let results                            = [eval (NoteOn 64 x) | x ← [0..127]]
>   return $ aEqual results $ (nub . sort) results
>
> negativeEvaluationsDecrease              = do
>   let m8r                                = defModulator{mrModId = 0
>                                                       , mrModSrc = defModSrc{
>                                                           msSource = FromNoteOnKey
>                                                         , msMapping = defMapping{msContinuity = Concave
>                                                                                , msMax2Min = True}}
>                                                       , mrModAmount = 0.5
>                                                       , mrModDest = ToFilterFc}
>   let graph                              = compileMods [m8r]
>
>   let eval                               = evaluateMods ToFilterFc graph 
>   let results                            = [eval (NoteOn 64 x) | x ← [0..127]]
>   return $ aEqual results $ (nub . sortOn Down) results
>
> supercededModulatorWillBeEliminated      = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.75                                           
>                                           , mrModDest = ToFilterFc}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.25
>                                           , mrModDest = ToFilterFc}
>   count                                  ← countSurvivingMods [m8r0, m8r1]
>   return $ aEqual count 1
>
> unsupercededModulatorWillNotBeEliminated = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.75                                           
>                                           , mrModDest = ToFilterFc}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModAmount = 0.75
>                                           , mrModDest = ToFilterFc}
>   count                                  ← countSurvivingMods [m8r0, m8r1]
>   return $ aEqual count 2
>
> canonicalOrderingWorks                   = do
>   let m8r00                              = defModulator{
>                                             mrModId = 101
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.75                                           
>                                           , mrModDest = ToFilterFc}
>   let m8r01                              = defModulator{
>                                             mrModId = 102
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModAmount = 0.75
>                                           , mrModDest = ToFilterFc}
>   let m8r10                              = defModulator{
>                                             mrModId = 103
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModAmount = 0.75                                           
>                                           , mrModDest = ToFilterFc}
>   let m8r11                              = defModulator{
>                                             mrModId = 104
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.75
>                                           , mrModDest = ToFilterFc}
>   let m8n0                               = resolveMods defModulation [m8r00, m8r01] []
>   let m8n1                               = resolveMods defModulation [m8r10, m8r11] []
>   return $ aEqual m8n0 m8n1

The End
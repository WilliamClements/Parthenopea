> {-# LANGUAGE UnicodeSyntax #-}

ModulationTest
William Clements
November 24, 2023

> module Parthenopea.Repro.ModulationTest ( modulationTests ) where
>
> import Control.Exception ( try, ErrorCall )
> import Data.Either ( isLeft )
> import qualified Data.Map                as Map
> import Parthenopea.Debug ( aEqual )
> import Parthenopea.Repro.Modulation

Modulation-related tests ==============================================================================================

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
>                                         ,   supercededModulatorWillBeEliminated
>                                         ,   unsupercededModulatorWillNotBeEliminated
>                                         ,   canonicalOrderingWorks]
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
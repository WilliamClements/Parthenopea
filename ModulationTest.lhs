> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE BangPatterns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module ModulationTest where
>
> import Control.Exception
> import Control.Monad
> import Data.Either
> import Data.List ( foldl', sort, nub, sortOn )
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord
> import Debug.Trace ( traceIO )
> import Euterpea ( (<<<), (>>>) )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns ( osc, filterLowPassBW, filterBandPass )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types ( AudSF, Signal, Clock, AudRate )
> import Euterpea.Music ( Volume, AbsPitch, Dur, absPitch, PitchClass(..) )
> import FRP.UISF.AuxFunctions ( constA )
> import HSoM.Examples.Additive ( sineTable, sfTest1 )
> import Modulation
> import Parthenopea
  
Testing ===============================================================================================================

A modulator is defined by its sfModSrcOper, its sfModDestOper, and its sfModSrcAmtOper

struct sfInstModList
{
  SFModulator sfModSrcOper;
  SFGenerator sfModDestOper;
  SHORT modAmount;
  SFModulator sfModAmtSrcOper;
  SFTransform sfModTransOper;
};

> countSurvivingMods     :: [Modulator] → IO Int
> countSurvivingMods m8rs                  = do
>   let !count                             =
>         length $ concat $ Map.elems $ modGraph $ resolveMods defModulation m8rs []
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
>                        :: IO Bool
>
> modulationTests        :: [IO Bool]      = [vanillaModulatorWillNotBeEliminated
>                                         ,   unlinkedModulatorWillBeEliminated
>                                         ,   linkedModulatorWillNotBeEliminated
>                                         ,   danglingModulatorWillBeEliminated
>                                         ,   mutuallyCyclicModulatorsWillBeRejected
>                                         ,   repeatedNoteOnsGiveSameResult
>                                         ,   positiveEvaluationsIncrease
>                                         ,   negativeEvaluationsDecrease
>                                         ,   supercededModulatorWillBeEliminated
>                                         ,   unsupercededModulatorWillNotBeEliminated]
>
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
> table2vals             :: Double → [Double] → [(Double, Double, Double, Double)]
> table2vals scalix                        = zipWith convFun [0..]
>   where
>     convFun            :: Int → Double → (Double, Double, Double, Double)
>     convFun i y                          = (fromIntegral i / scalix, y, 0, 0)
>
> vals' = table2vals 20 prevals
>         
> -- range = 0.75 (max resonance) ..    1.25 (no resonance)
> --         960 cB    -170-    ..      0 cB
>
> data IterData =
>   IterData {
>     theFc              :: Double
>   , theFq              :: Double
>   , theFt              :: Double
>   , theVel             :: Double} deriving Show
>
> -- vary: 1 or 2 for first arg to filterBandPass
> -- vary: 0 to 10 for initQ
>
> allFilterTypes         :: [ResonanceType]
> allFilterTypes                           = map toEnum [fromEnum ResonanceLowpass .. fromEnum ResonanceSVF]
>
> benchFilters            :: IO ()
> benchFilters                              = do
>   mapM_ benchFilter allFilterTypes
>   where
>     qRange                               = (0, 960)
>     qChunk                               = 240
>     allQms             :: [Double]       = map (\i → fromIntegral i * qChunk) [0..]
>     allQ                                 = takeWhile (\q → q <= snd qRange) allQms
>     initFc                               = 20
>
>     benchFilter rType                    = do
>       mapM_ doBench allQ
>       where
>         doBench initQ                    = do
>           let path                       = "test_" ++ show rType ++ show initQ ++ ".wav"
>           let m8n                        = defModulation{mLowPass = LowPass rType initFc initQ}
>           outFile path 10 $ sfTest1 (procFilter m8n) 10 (absPitch (C,6)) 64 []

nice simple range for initQ    0..960

setting bw to testQ / 10  ... at 5000 (i.e., 500) things looked nominal so let us "equate" 

> prevals = []

The End
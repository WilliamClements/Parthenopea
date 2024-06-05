> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE BangPatterns #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module ModulationTest where
>
> import Chart
> import Control.Exception
> import Control.Monad
> import Data.Colour
> import Data.Colour.Names
> import Data.Either
> import Data.List ( foldl', sort, nub, sortOn )
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord
> import Debug.Trace ( traceIO )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns ( osc, filterLowPassBW, filterBandPass )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types ( AudSF, Signal, Clock, AudRate )
> import Euterpea.Music ( Volume, AbsPitch, Dur, absPitch, PitchClass(..) )
> import FRP.UISF.AuxFunctions ( constA )
> import HSoM.Examples.Additive ( sineTable )
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
>                                         ,   unsupercededModulatorWillNotBeEliminated
>                                         ,   canonicalOrderingWorks]
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
>
> table2vals             :: Double → [Double] → [(Double, Double, Double, Double)]
> table2vals scalix                        = zipWith convFun [0..]
>   where
>     convFun            :: Int → Double → (Double, Double, Double, Double)
>     convFun i y                          = (fromIntegral i / scalix, y, 0, 0)

Feed chart ============================================================================================================

> -- range = 0.75 (max resonance) ..    1.25 (no resonance)
> --         960 cB    -170-    ..      0 cB
> -- vary: 1 or 2 for first arg to filterBandPass
> -- vary: 0 to 10 for initQ
>
> allFilterTypes         :: [ResonanceType]
> allFilterTypes                           = [minBound..maxBound]
>
> nKews                  :: Int            = 2
> kews                   :: [Int]          = breakUp (0, 960) 0 nKews
> nCutoffs               :: Int            = 11
> cutoffs                :: [Int]          = breakUp (20, 10000) 0 {- 2.7182818284590452353602874713527 -} nCutoffs
> nFreaks                :: Int            = 39
> freaks                 :: [Int]          = breakUp (20, 9000) 0 {- 2.7182818284590452353602874713527 -} nFreaks
>
> filterTestDur          :: Double         = 0.25
>
> colors                 :: [AlphaColour Double]
>                                          =
>   cycle
>     [  opaque blue
>     ,  opaque orange
>     ,  opaque green
>     ,  opaque red
>     ,  opaque purple]
>
> bench                  :: IO ()
> bench                                    =
>   benchFilters measureResponse [ResonanceSVF1] cutoffs kews freaks
>
> porch                  :: IO ()
> porch                                    =
>   benchFilters measureResponse [ResonanceTwoPoles] cutoffs kews freaks
>
> testFrFr               :: Int → IO ()
> testFrFr npoints                         = do
>   let frFr                               = createFrFun 500 15 259
>   let points                             = [20000 * fromIntegral x / fromIntegral npoints | x ← [0..(npoints-1)]]
>   let points'                            = map frFr points
>   let grouts                             = zip points points'
>   chartPoints "goose" [Section (opaque blue) grouts]
>   print grouts
>   return ()
>
> measureResponse        :: BenchSpec → [(Double, Double)]
> measureResponse BenchSpec{ .. }          = map doFk bench_fks
>   where
>     m8n                                  = defModulation{mLowpass = Lowpass bench_rt bench_fc bench_q}
>
>     doFk               :: Double → (Double, Double)
>     doFk fk                              = (fk, maxSample filterTestDur sf)
>       where
>         sf             :: AudSF () Double
>         sf                               = createFilterTest sineTable bench_fc fk (procFilter m8n)
> 
> benchFilters           :: (BenchSpec → [(Double, Double)]) → [ResonanceType] → [Int] → [Int] → [Int] → IO ()
> benchFilters fun rts fcs qs fks          = doFilters fun bRanges
>   where
>     bRanges                            = 
>       BenchRanges
>         rts
>         (map fromIntegral fcs) 
>         (map fromIntegral qs)
>         (map fromIntegral fks)
>
> doFilters              :: (BenchSpec → [(Double, Double)]) → BenchRanges → IO ()
> doFilters fun BenchRanges{ .. }          = mapM_ doRt ranges_rts
>   where
>     doRt               :: ResonanceType → IO ()
>     doRt currentRt                       =
>       if varyFc
>         then mapM_ doQ ranges_qs
>         else mapM_ doFc ranges_fcs
>       where
>         doFc           :: Double → IO ()
>         doFc currentFc                   = do
>           putStrLn $ unwords ["doFc", show currentFc]
>           doQs ranges_qs
>           where
>             doQs       :: [Double] → IO ()
>             doQs qs                      = do
>               putStrLn $ unwords ["doQs", show qs]
>               let sects                  = zipWith Section colors (map calc qs)
>               chartPoints (concat [show currentRt, "_fc", show currentFc]) sects
>               where
>                 calc   :: Double → [(Double, Double)]
>                 calc currentQ            = fun bs
>                   where
>                     bs                   = BenchSpec currentRt filterTestDur currentFc currentQ ranges_fks
>
>         doQ           :: Double → IO ()
>         doQ currentQ                     = do
>           putStrLn $ unwords ["doQ", show currentQ]
>           doFcs ranges_fcs
>           where
>             doFcs     :: [Double] → IO ()
>             doFcs fcs                    = do
>               putStrLn $ unwords ["doFcs", show fcs]
>               let sects                  = zipWith Section colors (map calc fcs)
>               chartPoints (concat [show currentRt, "_q", show currentQ]) sects
>               where
>                 calc   :: Double → [(Double, Double)]
>                 calc currentFc           = fun bs
>                   where
>                     bs                   = BenchSpec currentRt filterTestDur currentFc currentQ ranges_fks
>
> data BenchRanges                         =
>   BenchRanges {
>       ranges_rts       :: [ResonanceType]
>     , ranges_fcs       :: [Double]
>     , ranges_qs        :: [Double]
>     , ranges_fks       :: [Double]} deriving Show
>
> data BenchSpec                           =
>   BenchSpec {
>       bench_rt         :: ResonanceType
>     , bench_dur        :: Double
>     , bench_fc         :: Double
>     , bench_q          :: Double
>     , bench_fks        :: [Double]} deriving Show
>
> varyFc                                   = False

nice simple range for initQ    0..960

setting bw to testQ / 10  ... at 5000 (i.e., 500) things looked nominal so let us "equate" 

> prevals = []

The End
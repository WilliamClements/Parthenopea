> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE BangPatterns #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module ModulationTest where
>
> import Chart
> import Control.Exception
> import Control.Monad
> import Data.Array.Unboxed
> import Data.Colour
> import Data.Colour.Names
> import Data.Either
> import Data.List ( foldl', sort, nub, sortOn )
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord
> import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
> import Debug.Trace ( traceIO )
> import Discrete
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns ( osc, Table, filterLowPassBW, filterBandPass )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types ( AudSF, Signal, Clock (rate), AudRate )
> import Euterpea.Music ( Volume, AbsPitch, Dur, absPitch, PitchClass(..), a )
> import FRP.UISF.AuxFunctions ( constA )
> import HSoM.Examples.Additive ( sineTable )
> import Modulation
> import Parthenopea
  
Testing ===============================================================================================================

"A modulator is defined by its sfModSrcOper, its sfModDestOper, and its sfModSrcAmtOper"
--SoundFont spec

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
>         sum $ map length $ Map.elems $ modGraph $ resolveMods defModulation m8rs []
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

> allFilterTypes         :: [ResonanceType]
> allFilterTypes                           = [minBound..maxBound]
>
> nKews                  :: Int            = 3
> kews                   :: [Int]          = breakUp (0, 240) 0 nKews
> nCutoffs               :: Int            = 7
> cutoffs                :: [Int]          = breakUp (1000, 10_000) 0 {- 2.7182818284590452353602874713527 -} nCutoffs
> nFreaks                :: Int            = 5
> freaks                 :: [Int]          = breakUp (80, 8_000) 0 {- 2.7182818284590452353602874713527 -} nFreaks
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
>   benchFilters measureResponse [ResonanceConvo] cutoffs kews freaks
>
> porch                  :: IO ()
> porch                                    =
>   benchFilters measureResponse [ResonanceTwoPoles] cutoffs kews freaks
>
> testFrFr               :: Int → IO ()
> testFrFr npoints                         = do
>   print "max"
>   let frFr                               = createFrFun 880 0.25 10
>   let points                             = [4_000 * fromIntegral x / fromIntegral npoints | x ← [0..(npoints-1)]]
>   -- let points                             = [fromIntegral npoints | x ← [0..(npoints-1)]]
>   let points'                            = map frFr points
>   print $ maximum points
>   print $ maximum points'
>   let grouts                             = zip points points'
>   sumUp                                  ← checkGrouts grouts
>   putStrLn sumUp
>
>   chartPoints "goose" [Section (opaque blue) grouts]
>   return ()
>
> checkGrouts            ::  [(Double, Double)] → IO String
> checkGrouts grouts                       = do
>   let bummer                             = foldl' check ("checkGrouts", (0, 0)) grouts
>   let outStr                             = unwords ["checkGrouts", fst bummer]
>   return outStr
>   where
>     check              :: (String, (Double, Double)) → (Double, Double) → (String, (Double, Double))
>     check (str, (x, y)) (x', y')         = (newStr, (x', y'))
>       where
>         newStr = str ++ if 0 == x || 0 == y || 0 == x' || 0 == y' || x == x' || y == y' 
>                           then ""
>                           else ident (x, y) (x', y')
>     ident (a, b) (c, d)                  = unwords ["ident (", show octaves, show amps, show rate, ")\n"]
>       where
>         octaves                          = c / a / 2
>         amps                             = toCentibels (d / b)
>         rate                             = amps / octaves
>
> measureResponse        :: BenchSpec → Modulation → [(Double, Double)]
> measureResponse BenchSpec{ .. } m8n@Modulation{ .. }
>                                          = map doFk bench_fks
>   where
>     Lowpass{ .. }                        = mLowpass
>
>     doFk               :: Double → (Double, Double)
>     doFk fk                              = (fk, maxSample filterTestDur sf)
>       where
>         sf             :: AudSF () Double
>         sf                               =
>           if ResonanceConvo == lowpassType
>             then createConvoTest sineTable m8n fk
>             else createFilterTest sineTable m8n fk
> 
> benchFilters           :: (BenchSpec → Modulation → [(Double, Double)]) → [ResonanceType] → [Int] → [Int] → [Int] → IO ()
> benchFilters fun rts fcs qs fks          = doFilters fun bRanges
>   where
>     bRanges                              = 
>       BenchRanges
>         rts
>         (map fromIntegral fcs) 
>         (map fromIntegral qs)
>         (map fromIntegral fks)
>
> doFilters              :: (BenchSpec → Modulation → [(Double, Double)]) → BenchRanges → IO ()
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
>                 calc currentQ            = fun bs m8n
>                   where
>                     bs@BenchSpec{ .. }   = BenchSpec currentRt filterTestDur currentFc currentQ ranges_fks
>                     ks                   = KernelSpec
>                                              (toAbsoluteCents bench_fc)
>                                              (round (960 * currentQ))
>                                              44_100
>
>                     m8n                  = defModulation{mLowpass = Lowpass bench_rt ks bench_fc bench_q}
>
>         doQ           :: Double → IO ()
>         doQ currentQ                     = do
>           putStrLn $ unwords ["doQ", show currentQ]
>           ts1                            ← getCurrentTime
>           doFcs ranges_fcs
>           ts2                            ← getCurrentTime 
>           putStrLn $ unwords ["doQ elapsed=: ", show (diffUTCTime ts2 ts1)]
>           where
>             doFcs     :: [Double] → IO ()
>             doFcs fcs                    = do
>               putStrLn $ unwords ["doFcs", show fcs]
>               ts1                        ← getCurrentTime
>               let sects                  = zipWith Section colors (map calc fcs)
>               chartPoints (concat [show currentRt, "_q", show currentQ]) sects
>               ts2                        ← getCurrentTime 
>               putStrLn $ unwords ["doFcs elapsed=: ", show (diffUTCTime ts2 ts1)]
>               where
>                 calc   :: Double → [(Double, Double)]
>                 calc currentFc           = fun bs m8n
>                   where
>                     bs@BenchSpec{ .. }   = BenchSpec currentRt filterTestDur currentFc currentQ ranges_fks
>                     ks                   = KernelSpec
>                                              (toAbsoluteCents bench_fc)
>                                              (round (960 * currentQ))
>                                              44_100
>                     m8n                  = defModulation{mLowpass = Lowpass bench_rt ks bench_fc bench_q}
>
> chartIr                :: IO ()
> chartIr                                  = chartPoints "impulseResponse" [Section (opaque blue) grouts]
>   where
>     vec                                  = fromJust $ memoizedComputeIR $ KernelSpec 500 0 44_100
>     len                                  = snd $ bounds vec
>     grouts             :: [(Double, Double)]
>                                          = [(fromIntegral i, vec ! i) | i ← [0..len]]
>     
> createConvoTest        :: ∀ p . Clock p ⇒ Table → Modulation → Double → Signal p () Double
> createConvoTest waveTable Modulation{ .. } freq
>   | traceNow trace_CCT False             = undefined
>   | otherwise                            = applyConvolutionMono mLowpass filterTestDur waveSF
>   where
>     trace_CCT                            = unwords ["createConvoTest", show lowpassKs]
>
>     Lowpass{ .. }                        = mLowpass
>     KernelSpec{ .. }                     = lowpassKs
>
>     waveSF                               =
>       proc () → do
>         a1 ← osc waveTable 0 ⤙ freq
>         outA ⤙ a1
>
> createFilterTest       :: ∀ p . Clock p ⇒ Table → Modulation → Double → Signal p () Double
> createFilterTest waveTable m8n@Modulation{ .. } freq
>   | traceNot trace_CFT False             = undefined
>   | otherwise                            =
>   proc () → do
>     a1 ← osc waveTable 0 ⤙ freq
>     a2 ← filtersf ⤙ (a1, fc)
>     outA ⤙ a2 * 100 / 128
>   where
>     trace_CFT                            = unwords ["createFilterTest", show fc, show freq]
>
>     Lowpass{ .. }                        = mLowpass
>     KernelSpec{ .. }                     = lowpassKs
>     fc                                   = fromAbsoluteCents ksFc                     
>
>     filtersf                             = procFilter m8n
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
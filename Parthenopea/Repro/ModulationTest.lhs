> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

ModulationTest
William Clements
November 24, 2023

> module Parthenopea.Repro.ModulationTest ( modulationTests, filterTestDur ) where
>
> import Control.Exception ( try, ErrorCall )
> import qualified Control.Monad           as CM
> import Data.Either ( isLeft )
> import Data.List
> import qualified Data.Map.Strict         as Map
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import Diagrams.Prelude
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Types ( AudSF, Clock, Signal )
> import Euterpea.IO.Audio.BasicSigFuns ( osc, Table )
> import Parthenopea.Debug ( aEqual )
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Chart
> import Parthenopea.Repro.Modulation
> import Parthenopea.SoundFont.Utility

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
>   , allMappingsOk
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
>                                         ,   canonicalOrderingWorks
>                                         ,   allMappingsOk]
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

In SoundFont spec, https://www.synthfont.com/SFSPEC21.PDF, see 9.5.2 Pictorial Examples of Source Types

> allMappingsOk                            = do
>   drawings                               ← CM.zipWithM drawOne [0..] allMappings
>   mapM_ chartOne drawings
>   return True
>
>   where
>
>   drawOne              :: Int → Mapping → IO (Section, String, FilePath)
>   drawOne iter ping                      =
>     let section                          = Section (opaque blue) (zip xs ys)
>     in return (section, show ping, show iter)
>     where
>       xs, ys           :: [Double]
>       xs                                 = [fromIntegral x / 128 | x ← [0..127]]
>       ys                                 = map (controlDenormal ping (0, 1)) xs
>
>   chartOne             :: (Section, String, FilePath) → IO Bool
>   chartOne (section, title, path)        = chartPoints title ("ping" ++ path) (singleton section)
>
> bench, porch           :: IO ()
> bench                                    = benchFilters measureResponse [ResonanceSVF] cutoffs kews freaks
> porch                                    = benchFilters measureResponse [ResonanceSVF] cutoffs kews freaks
>
> cutoffs                :: [Int]
> cutoffs                                  = [2_000, 4_000, 8_000, 15_353]
>                                            -- 9_370, 9_373, 9_3274]
>                                           -- [9_300, 9_371, 9_380, 9_200, 9_300, 9_365]
>                                           -- [9_350, 9_360, 9_380, 9_400]
> -- cutoffs                                  = [9_350, 9_400, 9_425, 9_500]
> nKews                  :: Int
> nKews                                    = 4
> kews                   :: [Int]
> kews                                     = [0, 120, 240, 480] -- breakUp (0, 480) 0 nKews
> nFreaks                :: Int
> nFreaks                                  = 23
> freaks                 :: [Int]
> freaks                                   = breakUp (25, 22_000) 0 nFreaks
> varyFc                 :: Bool
> varyFc                                   = False
>
> filterTestDur          :: Double
> filterTestDur          :: Double         = 1
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
> createFilterTest       :: ∀ p . Clock p ⇒ Table → Lowpass → Double → Signal p () Double
> createFilterTest waveTable lp freq       =
>   proc () → do
>     a1 ← osc waveTable 0                 ⤙ freq
>     a2 ← procFilter lp                   ⤙ (a1, fc)
>     outA                                 ⤙ a2 * 100 / qMidiDouble128
>   where
>     fc                                   = fromAbsoluteCents lp.lowpassKs.ksFc                     
>
> benchFilters           :: (BenchSpec → Modulation → [(Double, Double)])
>                           → [ResonanceType] → [Int] → [Int] → [Int] → IO ()
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
>               let sects                  = zipWith Section chartColors (map calc qs)
>               chartPoints "doFilters" (concat [show currentRt, "_fc", show currentFc]) sects
>               return ()
>               where
>                 calc   :: Double → [(Double, Double)]
>                 calc currentQ            = fun bs m8n
>                   where
>                     bs@BenchSpec{ .. }   
>                                          = BenchSpec currentRt filterTestDur currentFc currentQ ranges_fks
>                     ks                   = KernelSpec
>                                              (toAbsoluteCents bench_fc)
>                                              (round currentQ)
>                                              44_100
>                                              True
>                                              256
>
>                     m8n                  = defModulation{mLowpass = Lowpass bench_rt ks}
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
>               let sects                  = zipWith Section chartColors (map calc fcs)
>               print sects
>               chartPoints "doQ" (concat [show currentRt, "_q", show currentQ]) sects
>               ts2                        ← getCurrentTime 
>               putStrLn $ unwords ["doFcs elapsed=: ", show (diffUTCTime ts2 ts1)]
>               where
>                 calc   :: Double → [(Double, Double)]
>                 calc currentFc           = fun bs m8n
>                   where
>                     bs@BenchSpec{ .. }   = BenchSpec currentRt filterTestDur currentFc currentQ ranges_fks
>                     ks                   = KernelSpec
>                                              (toAbsoluteCents bench_fc)
>                                              (round currentQ)
>                                              44_100
>                                              True
>                                              256
>                     m8n                  = defModulation{mLowpass = Lowpass bench_rt ks}
>
> measureResponse        :: BenchSpec → Modulation → [(Double, Double)]
> measureResponse BenchSpec{ .. } Modulation{ .. }
>                                          = map doFk bench_fks
>   where
>     doFk               :: Double → (Double, Double)
>     doFk fk                              = (fk, maxSample filterTestDur sf)
>       where
>         sf             :: AudSF () Double
>         sf                               = createFilterTest sineTable mLowpass fk
> 

The End
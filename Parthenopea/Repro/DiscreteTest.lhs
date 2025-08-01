> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

DiscreteTest
William Clements
June 22, 2024

> module Parthenopea.Repro.DiscreteTest where
>
> import Control.Arrow
> import Data.Complex ( imagPart, realPart, Complex )
> import Data.List ( foldl' )
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import qualified Data.Vector.Unboxed     as VU
> import Diagrams.Prelude hiding ( fc )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns ( osc, Table, envExpon )
> import Euterpea.IO.Audio.IO ( outFile )
> import Euterpea.IO.Audio.Render ( Instr )
> import Euterpea.IO.Audio.Types ( AudSF, Signal, Clock, Mono, AudRate )
> import Parthenopea.Music.Siren ( maxSample, qMidiSize128 )
> import Parthenopea.Repro.Chart ( Section(Section), chartPoints )
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Modulation

Feed chart ============================================================================================================

> allFilterTypes         :: [ResonanceType]
> allFilterTypes                           = [minBound..maxBound]
>
> nKews                  :: Int
> nKews                                    = 4
> kews                   :: [Int]
> kews                                     = [0, 120, 240, 480] -- breakUp (0, 480) 0 nKews
> nCutoffs               :: Int
> nCutoffs                                 = 4
> cutoffs                :: [Int]
> cutoffs                                  = [2_000, 4_000, 8_000, 15_353]
>                                            -- 9_370, 9_373, 9_3274]
    
>                                           -- [9_300, 9_371, 9_380, 9_200, 9_300, 9_365]
>                                           -- [9_350, 9_360, 9_380, 9_400]
> -- cutoffs                                  = [9_350, 9_400, 9_425, 9_500]
> nFreaks                :: Int
> nFreaks                                  = 23
> freaks                 :: [Int]
> freaks                                   = breakUp (25, 22_000) 0 nFreaks
>
> filterTestDur          :: Double
> filterTestDur          :: Double         = 1
>
> colors                 :: [AlphaColour Double]
> colors                 :: [AlphaColour Double]
>                                          =
>   cycle
>     [  blue `withOpacity` 2
>     ,  orange `withOpacity` 2
>     ,  green `withOpacity` 2
>     ,  red  `withOpacity` 2
>     ,  purple `withOpacity` 2]
>
> bench, porch           :: IO ()
> bench                                    =
>   benchFilters measureResponse [ResonanceSVF] cutoffs kews freaks
> porch                                    =
>   benchFilters measureResponse [ResonanceSVF] cutoffs kews freaks
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
> measureResponse BenchSpec{ .. } Modulation{ .. }
>                                          = map doFk bench_fks
>   where
>     doFk               :: Double → (Double, Double)
>     doFk fk                              = (fk, maxSample filterTestDur sf)
>       where
>         sf             :: AudSF () Double
>         sf                               = createFilterTest sineTable mLowpass fk
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
>                                              (round currentQ)
>                                              44_100
>                                              useFastFourier
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
>               let sects                  = zipWith Section colors (map calc fcs)
>               print sects
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
>                                              (round currentQ)
>                                              44_100
>                                              useFastFourier
>                                              256
>                     m8n                  = defModulation{mLowpass = Lowpass bench_rt ks}
>
> chartIr                :: IO ()
> chartIr                                  = do
>   chartPoints
>     (if useFastFourier then "freakResponse" else "impulseResponse")
>     [Section (opaque blue) groutsR, Section (opaque orange) groutsI]
>   where
>     targetFc, dataLen, sampleRate
>                        :: Int
>     freakSpan          :: [Int]
>
>     targetFc                             = 999
>     targetQ                              = 240
>
>     dataLen                              = 65_536
>     sampleRate                           = 44_100
>
>     freakSpan                            = [10..19_990] -- [middleFreakShow - 900 .. middleFreakShow  + 900]
>
>     kspec              :: KernelSpec
>     kspec                                =
>       KernelSpec
>         ((toAbsoluteCents . fromIntegral) targetFc)
>         targetQ sampleRate useFastFourier dataLen
>
>     cdsigFr            :: DiscreteSig (Complex Double)
>     cdsigFr                              = memoizedComputeFR kspec     
>         
>     vecR, vecI         :: VU.Vector Double
>     vecR                                 = VU.map realPart (dsigVec cdsigFr)
>     vecI                                 = VU.map imagPart (dsigVec cdsigFr)
>     groutsR, groutsI   :: [(Double, Double)]
>     groutsR                              = [(fromIntegral i, - (vecR VU.! i)) | i ← freakSpan]
>     groutsI                              = [(fromIntegral i, - (vecI VU.! i)) | i ← freakSpan]
>     
> createConvoTest        :: ∀ p . Clock p ⇒ Table → Lowpass → Double → Signal p () Double
> createConvoTest waveTable lp freq        = applyConvolutionMono lp filterTestDur waveSF
>   where
>     waveSF                               =
>       proc () → do
>         a1 ← osc waveTable 0 ⤙ freq
>         outA ⤙ a1
>
> createFilterTest       :: ∀ p . Clock p ⇒ Table → Lowpass → Double → Signal p () Double
> createFilterTest waveTable lp freq       =
>   proc () → do
>     a1 ← osc waveTable 0                 ⤙ freq
>     a2 ← filtersf                        ⤙ (a1, fc)
>     outA                                 ⤙ a2 * 100 / fromIntegral qMidiSize128
>   where
>     fc                                   = fromAbsoluteCents lp.lowpassKs.ksFc                     
>
>     filtersf                             = procFilter lp
>
> env1                   :: AudSF () Double
> env1                                     = envExpon 20 10 10_000
>
> sfTest1                :: AudSF (Double,Double) Double → Instr (Mono AudRate)
>                        -- AudSF (Double,Double) Double → 
>                        -- Dur → AbsPitch → Volume → [Double] → AudSF () Double
> sfTest1 sf _ ap vol _                    =
>   let f = apToHz ap
>       v = fromIntegral vol / 100
>   in proc () → do
>        a1 ← osc sineTable 0 <<< env1 -< () 
>        a2 ← sf ⤙ (a1,f)
>        outA ⤙ a2*v
>
> tsvf                   :: IO ()
> tsvf                                     = outFile "low.wav" 10 $ sfTest1 (procSVF lp) 10 72 80 []
>   where
>     lp                                   = Lowpass ResonanceSVF ks
>     ks                                   = (defKernelSpec True) {ksFc = 1000}
>            
> testKS                 :: KernelSpec
> testKS                                   =
>   KernelSpec
>     (toAbsoluteCents 999)
>     120 -- Q in centibels
>     44_100
>     True
>     4_096
>
> testFreaks             :: Int → IO Double
> testFreaks qIter                         = do
>   let ks@KernelSpec{ .. }                = testKS{ksQ = qIter}
>   let kd                                 = calcKernelData ks
>   let shapes                             = makeShapes ResponseNormal
>   let fun                                = getFreaky kd shapes
>
>   let fc                                 = fromAbsoluteCents ksFc
>   let fci              :: Int            = round fc
>   let spread           :: Int            = min fci (50 * round (log fc))
>   let fcStart                            = fci - spread `div` 2
>   let fcEnd                              = fci + spread `div` 2
>
>   let xs               :: [Double]       = [fromIntegral x | x ← [fcStart..fcEnd]]
>   let ys                                 = map (realPart . fun) xs
>   let zs                                 = map ((*) (-1) . imagPart . fun) xs
>   let rpairs                             = zip xs ys
>   let cpairs                             = zip xs zs
>   let sects                              = [Section (opaque blue) rpairs, Section (opaque orange) cpairs]
>   
>   -- print pairs
>   chartPoints ("ResonanceConvo_fc" ++ show qIter) sects
>   
>   return 0
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
> varyFc                 :: Bool
> varyFc                                   = False
>
> testDecline            :: Double → Double → IO ()
> testDecline freakStart _                 = do
>   mapM_ mapfun [freakStart..freakStart + 50]
>   where
>     mapfun             :: Double → IO ()
>     mapfun xIn                           = print (xIn, friCompute xIn)
>
>     FrItem{ .. }                         =
>       FrItem
>         (freakStart + 50)
>         xformDecline
>       where
>         xformDecline   :: Double → Double
>         xformDecline                     =
>             fromCentibels
>           . ddLinear2 (-dropoffRate) 0 -- hcent
>           . flip (-) (logBase 2 freakStart)
>           . logBase 2

The End
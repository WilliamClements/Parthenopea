> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

DiscreteTest
William Clements
June 22, 2024

> module Parthenopea.Repro.DiscreteTest where
>
> import Data.Colour
> import Data.Colour.Names
> import Data.Complex
> import Data.List ( foldl' )
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.BasicSigFuns ( osc, Table )
> import Euterpea.IO.Audio.Types ( AudSF, Signal, Clock )
> import Parthenopea.Debug(notracer, traceNot)
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Chart
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Modulation

Feed chart ============================================================================================================

> allFilterTypes         :: [ResonanceType]
> allFilterTypes                           = [minBound..maxBound]
>
> nKews                  :: Int
> nKews                                    = 1
> kews                   :: [Int]
> kews                                     = [0] -- breakUp (0, 480) 0 nKews
> nCutoffs               :: Int
> nCutoffs                                 = 3
> cutoffs                :: [Int]
> cutoffs                                  = [200, 1_500, 15_353]
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
>   benchFilters measureResponse [ResonanceSVF1] cutoffs kews freaks
> porch                                    =
>   benchFilters measureResponse [ResonanceBandpass] cutoffs kews freaks
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
>     Lowpass{ .. }                        = mLowpass
>
>     doFk               :: Double → (Double, Double)
>     doFk fk                              = (fk, maxSample filterTestDur sf)
>       where
>         sf             :: AudSF () Double
>         sf                               =
>           if ResonanceConvo == lowpassType
>             then createConvoTest sineTable mLowpass fk
>             else createFilterTest sineTable mLowpass fk
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
>     groutsR                              = [(fromIntegral i, - vecR VU.! i) | i ← freakSpan]
>     groutsI                              = [(fromIntegral i, - vecI VU.! i) | i ← freakSpan]
>     
> createConvoTest        :: ∀ p . Clock p ⇒ Table → Lowpass → Double → Signal p () Double
> createConvoTest waveTable lp@Lowpass{ .. } freq
>   | traceNot trace_CCT False             = undefined
>   | otherwise                            = applyConvolutionMono lp filterTestDur waveSF
>   where
>     trace_CCT                            = unwords ["createConvoTest", show lowpassKs]
>
>     waveSF                               =
>       proc () → do
>         a1 ← osc waveTable 0 ⤙ freq
>         outA ⤙ a1
>
> createFilterTest       :: ∀ p . Clock p ⇒ Table → Lowpass → Double → Signal p () Double
> createFilterTest waveTable lp@Lowpass{ .. } freq
>   | traceNot trace_CFT False             = undefined
>   | otherwise                            =
>   proc () → do
>     a1 ← osc waveTable 0 ⤙ freq
>     a2 ← filtersf ⤙ (a1, fc)
>     outA ⤙ a2 * 100 / fromIntegral qMidiSize128
>   where
>     trace_CFT                            = unwords ["createFilterTest", show fc, show freq]
>
>     KernelSpec{ .. }                     = lowpassKs
>     fc                                   = fromAbsoluteCents ksFc                     
>
>     filtersf                             = procFilter lp
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
> varyFc                                   = True
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
>             notracer "fromCentibels"     . fromCentibels
>           . notracer "ddLinear2"         . ddLinear2 (-dropoffRate) 0 -- hcent
>           . notracer "delta"             . flip (-) (logBase 2 freakStart)
>           . notracer "logBase 2"         . logBase 2
>           . notracer "xIn"

The End
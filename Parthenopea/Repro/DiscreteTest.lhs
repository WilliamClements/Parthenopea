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
> import qualified Data.Vector.Unboxed     as VU
> import Diagrams.Prelude hiding ( fc )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns ( osc, Table, envExpon )
> import Euterpea.IO.Audio.IO ( outFile )
> import Euterpea.IO.Audio.Render ( Instr )
> import Euterpea.IO.Audio.Types ( AudSF, Signal, Clock, Mono, AudRate )
> import Parthenopea.Repro.Chart
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.ModulationTest
> import Parthenopea.SoundFont.Utility

Feed chart ============================================================================================================

> allFilterTypes         :: [ResonanceType]
> allFilterTypes                           = [minBound..maxBound]
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
> chartIr                :: Bool → IO ()
> chartIr useFastFourier                   = do
>   chartPoints
>     "chartIr"
>     (if useFastFourier then "freakResponse" else "impulseResponse")
>     [Section (opaque blue) groutsR, Section (opaque orange) groutsI]
>   return ()
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
> sfTest1                :: AudSF (Double,Double) Double → Instr (Mono AudRate)
>                        -- AudSF (Double,Double) Double → 
>                        -- Dur → AbsPitch → Volume → [Double] → AudSF () Double
> sfTest1 sf _ ap vol _                    =
>   let f = apToHz ap
>       v = fromIntegral vol / 100
>       env1 = envExpon 20 10 10_000
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
>   let ks                                 = testKS{ksQ = qIter}
>   let kd                                 = calcKernelData ks
>   let shapes                             = makeShapes ResponseNormal
>   let fun                                = getFreaky kd shapes
>
>   let fc                                 = fromAbsoluteCents ks.ksFc
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
>   chartPoints "DiscreteTest" ("ResonanceConvo_fc" ++ show qIter) sects
>   
>   return 0
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
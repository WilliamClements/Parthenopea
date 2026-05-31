> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

ShowResults
William Clements
May 28, 2026

> module Eng.ShowResults (
>         showResults ) where
>
> import Control.Lens hiding ( element, ix )
> import qualified Data.IntSet             as IntSet
> import Data.List
> import qualified Data.Map.Strict         as Map
> import Data.Ord
> import qualified Data.Vector.Strict      as VB
> import Debug.Trace
> import Eng.SFSpec
> import Eng.ShredFile
>
> skipFileOutput         :: Bool
> skipFileOutput                           = False
>
> showResults       :: VB.Vector GenSum → IO (VB.Vector (VB.Vector String))
> showResults vGenSum                      = do
>   let gensOutput                         = if skipFileOutput || VB.length vGenSum <= 1
>                                              then VB.empty
>                                              else VB.concatMap showFile vGenSum
>   let vRollup                            =
>         (gsTag .~ "<summary>") (VB.foldl' addGenSums (VB.head vGenSum) (VB.tail vGenSum))
>   let rollupOutput                       = showFile vRollup
>   return $ gensOutput VB.++ rollupOutput
>   where
>     showFile           :: GenSum → VB.Vector (VB.Vector String)
>     showFile gensum                      =
>       let
>         headerOutput                     = VB.fromList ["", show (gensum ^. gsTag)]
>         slateOutput                      = VB.map show (gensum ^. gsGenSlate)
>         envOutput                        =
>           VB.fromList ["\nModulation Envelope configurations:\n"]
>           VB.++ showEnvMap (gensum ^. gsModEnvMap)
>           VB.++ VB.fromList ["\nVolume Envelope configurations:\n"]
>           VB.++ showEnvMap (gensum ^. gsVolEnvMap)
>           VB.++ VB.fromList ["\n\n"]
>         statsOutput                       =
>           VB.fromList ["\nGenerator statistics:\n"]
>           VB.++ showStats gensum
>           VB.++ VB.fromList ["\n\n"]
>       in
>         VB.fromList [headerOutput, slateOutput, envOutput, statsOutput]
>
>     showEnvMap envMap
>       | Map.null envMap                  = VB.empty
>       | otherwise                        = VB.fromList (map (uncurry showEC) arrivals)
>         where
>           arrivals     :: [(Int, EConfig)]    
>           arrivals                       = sortBy (comparing Down) (Map.foldrWithKey shuffle [] envMap)
>                                               where shuffle ec count acc = (count, ec) : acc
>           showEC       :: Int → EConfig → String
>           showEC count ec                = unwords [show count, show ec]
>
>     showStats gensum                     = VB.map showOneGen valueBearing
>       where
>         showOneGen ge                    = showTwoStats ((gensum ^. gsGenSlate) VB.! fromEnum ge)
>         showTwoStats genData             =
>           if IntSet.null (genData ^. gWildValues)
>              then s1
>              else unwords [s1, "   (", s2, ")"]
>           where
>             s1                           = unwords [show (genData ^. gId), showStat (genData ^. gGoodValues)]
>             s2                           = showStat ((genData ^. gGoodValues) `IntSet.union` (genData ^. gWildValues))
>         showStat is                      = if IntSet.null is then [] else unwords [show m, "+-", show s]
>           where
>             (m, s)                       = dispersion (IntSet.toList is)
>
>     dispersion         :: [Int] → (Double, Double)
>     dispersion vals                      = if null vals
>                                              then error "dispersion: empty list would cause divide-by-zero"
>                                              else (mean, stdDev)
>       where
>         dubs           :: [Double]       = map fromIntegral (traceShowId vals)
>
>         denom, mean, stdDev
>                        :: Double
>         denom                            = fromIntegral (length vals)
>         mean                             = sum dubs / denom
>         stdDev                           = sqrt (sum (map dev dubs) / denom)
>                                              where dev x = (x - mean) ** 2

The End
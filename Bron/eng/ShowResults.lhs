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
> import Eng.SFSpec
> import Eng.ShredFile
>
> showResults       :: VB.Vector GenSum → IO (VB.Vector String)
> showResults vGenSum                      = do
>   let gensOutput                         = VB.concatMap showFile vGenSum
>   let vRollup                            =
>         (gsFilename .~ "<rollup>") (VB.foldl' addGenSums (VB.head vGenSum) (VB.tail vGenSum))
>   let rollupOutput                       = showFile vRollup VB.++ showStats vRollup
>   return $ gensOutput VB.++ rollupOutput
>   where
>     showFile gensum                      = VB.concat [
>       VB.fromList ["", gensum ^. gsFilename]
>       , VB.map show (gensum ^. gsGenData)
>       , showInstData (gensum ^. gsModEnvMap)
>       , showInstData (gensum ^. gsVolEnvMap)]
>
>     showInstData envMap
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
>         showOneGen ge                    = showTwoStats ((gensum ^. gsGenData) VB.! fromEnum ge)
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
>                                              then error "dispersion: empty list (avoid divide by zero)"
>                                              else (mean, stdDev)
>       where
>         dubs           :: [Double]       = map fromIntegral vals
>
>         denom, mean, stdDev
>                        :: Double
>         denom                            = realToFrac (length vals)
>         mean                             = sum dubs / denom
>         stdDev                           = sqrt (sum (map dev dubs) / denom)
>                                              where dev x = (x - mean) ** 2

The End
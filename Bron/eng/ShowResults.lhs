> {-# LANGUAGE OverloadedRecordDot #-}
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
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Ord
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec
> import Eng.ShredFile
>
> showLevel              :: GenSumLevel
> showLevel                                = GSTRollup
>
> skipRaw                :: Bool
> skipRaw                                  = True
>
> showResults       :: GenSum → IO (VB.Vector (VB.Vector String))
> showResults rootGenSum                   = do
>   let (i, j, k)                          = overallCounts rootGenSum
>   putStrLn $ unwords ["num Files", show i]
>   putStrLn $ unwords ["num Insts", show j]
>   putStrLn $ unwords ["num Zones", show k]
>   return $ showGenSum rootGenSum
>   where
>     showGenSum         :: GenSum → VB.Vector (VB.Vector String)
>     showGenSum gensum                    =
>       let
>         recurseOutput                    = if fromEnum showLevel >= fromEnum (gensum ^. gsLevel)
>                                              then VB.empty
>                                              else VB.concatMap showGenSum (gensum ^. gsSubSums)
>         headerOutput                     = VB.fromList ["", unwords [show (gensum ^. gsLevel), gensum ^. gsTag]]
>         slateOutput                      = if skipRaw
>                                              then VB.empty
>                                              else VB.map show (gensum ^. gsGenSlate)
>         envOutput                        =
>           VB.singleton (unwords ["\nModulation Envelope configs (out of total", show (total (gensum ^. gsModEnvMap)), "):\n"])
>           VB.++ showEnvMap (gensum ^. gsModEnvMap)
>           VB.++ VB.singleton (unwords ["\nVolume Envelope configs (out of total", show (total (gensum ^. gsVolEnvMap)), "):\n"])
>           VB.++ showEnvMap (gensum ^. gsVolEnvMap)
>           VB.++ VB.fromList ["\n\n"]
>         statsOutput                       =
>           VB.fromList ["\nGenerator statistics:\n"]
>           VB.++ showStats gensum
>           VB.++ VB.fromList ["\n\n"]
>
>         total          :: Map EConfig Int → Int
>         total                            = sum . Map.elems
>       in
>         recurseOutput VB.++ VB.fromList [headerOutput, slateOutput, envOutput, statsOutput]
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
>     showStats          :: GenSum → VB.Vector String
>     showStats gensum                     = VB.concatMap showOneGen valueBearing
>       where
>
>         showOneGen     :: GenEnum → VB.Vector String
>         showOneGen ge                    = showTwoStats ((gensum ^. gsGenSlate) VB.! fromEnum ge)
>         showTwoStats   :: GenData → VB.Vector String
>         showTwoStats genData             = VB.fromList $ s0 : (if (genData ^. gOccur) == 0 then [] else [s1, s2])
>           where
>             ix                           = fromEnum (genData ^. gId)
>             s0                           = unwords [show (genData ^. gId), "...", show ix, "...", show (staticDataVector VB.! ix)]
>             s1                           = showStat (genData ^. gGoodValues)
>             s2                           = showStat ((genData ^. gGoodValues) `IntSet.union` (genData ^. gWildValues))
>         showStat is                      = if IntSet.null is then [] else unwords ["  dispersion", show m, "+-", show s]
>           where
>             (m, s)                       = dispersion (IntSet.toList is)
>
>     dispersion         :: [Int] → (Double, Double)
>     dispersion vals                      = if null vals
>                                              then error "dispersion: empty list would cause divide-by-zero"
>                                              else (mean, stdDev)
>       where
>         dubs           :: [Double]       = map fromIntegral vals
>
>         denom, mean, stdDev
>                        :: Double
>         denom                            = fromIntegral (length vals)
>         mean                             = sum dubs / denom
>         stdDev                           = sqrt (sum (map dev dubs) / denom)
>                                              where dev x = (x - mean) ** 2
>
>     overallCounts      :: GenSum → (Int, Int, Int)
>     overallCounts gensum                 = foldl' shredCounts (0, 0, 0) (flatten gensum)
>       where
>         flatten        :: GenSum → [GenSum]
>         flatten leaf                     = leaf : concatMap flatten (VB.toList (leaf ^. gsSubSums))
>
>         shredCounts    :: (Int, Int, Int) → GenSum → (Int, Int, Int)
>         shredCounts (x, y, z) eachGenSum =
>           case eachGenSum ^. gsLevel of
>             GSTFile    → (x + 1, y, z)
>             GSTInst    → (x, y + 1, z)
>             GSTZone    → (x, y, z + 1)
>             _          → (x, y, z)

The End
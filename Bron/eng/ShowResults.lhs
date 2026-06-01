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
> skipRaw                                  = False
>
> showResults       :: GenSum → IO (VB.Vector (VB.Vector String))
> showResults rootGenSum                   = do
>   putStrLn $ unwords ["num Files", show iFile]
>   putStrLn $ unwords ["num Insts", show jInst]
>   putStrLn $ unwords ["num Zones", show kZone]
>   return $ showGenSum rootGenSum
>   where
>     iFile, jInst, kZone
>                        :: Int
>     (iFile, jInst, kZone)                = overallCounts rootGenSum
>
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
>           VB.singleton (unwords ["\nModulation Envelope configs (out of total=", show (total (gensum ^. gsModEnvMap)), "):\n"])
>           VB.++ showEnvMap (gensum ^. gsModEnvMap)
>           VB.++ VB.singleton (unwords ["\nVolume Envelope configs (out of total=", show (total (gensum ^. gsVolEnvMap)), "):\n"])
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
>         showOneGen ge                    = showTwoStats ((gensum ^. gsGenSlate) VB.! fromEnum ge)
>         showTwoStats genData             = VB.fromList $ s0 : (if (genData ^. gOccur) == 0 then [] else [s1, s2])
>           where
>             gen        :: GenEnum        = genData ^. gId
>             ix                           = fromEnum gen
>             spec                         = specVector VB.! ix
>             goods      :: [Int]          = IntSet.toList (genData ^. gGoodValues)
>
>             sindent                      = replicate 4 ' '
>             sflat                        = unwords [sindent, "flat"]
>             semph                        = unwords [sindent, "emph"]
>
>             dflat                        = dispersion goods (kZone - (genData ^. gOccur)) (spec ^. gDefault)
>             demph                        = dispersion goods 0                             (spec ^. gDefault)
>
>             s0                           = unwords [show ix, show gen, show spec]
>             s1                           = if VB.notElem gen noNumericDefault
>                                              then unwords [sflat, showStat dflat]
>                                              else unwords [sflat, "n/a"]
>             s2                           = unwords [semph, showStat demph]
>
>             showStat   :: (Double, Double) → String
>             showStat (m, s)              = unwords [show m, "+-", show s]
>
> dispersion             :: [Int] → Int → Int → (Double, Double)
> dispersion vals nDef valDef              = if null vals && nDef == 0
>                                              then error "dispersion: absence of vals would cause divide-by-zero"
>                                              else (mean, stdDev)
>   where
>     denom, mean, stdDev, dubNDef, dubValDef
>                        :: Double
>     denom                                = fromIntegral (length vals) + dubNDef
>
>     dubs               :: [Double]       = map fromIntegral vals
>     (dubNDef, dubValDef)                 = (fromIntegral nDef, fromIntegral valDef)
>
>     mean                                 = (sum dubs + dubNDef * dubValDef) / denom
>     c1                                   = sum $ map dev dubs
>                                              where dev x = (x - mean) ** 2
>     c2                                   = dubNDef * (dubValDef - mean) ** 2 
>     stdDev                               = sqrt ((c1 + c2) / denom)
>
> overallCounts          :: GenSum → (Int, Int, Int)
> overallCounts gensum                     = foldl' shredCounts (0, 0, 0) (flatten gensum)
>   where
>     flatten            :: GenSum → [GenSum]
>     flatten leaf                         = leaf : concatMap flatten (VB.toList (leaf ^. gsSubSums))
>
>     shredCounts        :: (Int, Int, Int) → GenSum → (Int, Int, Int)
>     shredCounts (x, y, z) eachGenSum     =
>       case eachGenSum ^. gsLevel of
>         GSTFile        → (x + 1, y, z)
>         GSTInst        → (x, y + 1, z)
>         GSTZone        → (x, y, z + 1)
>         _              → (x, y, z)

The End
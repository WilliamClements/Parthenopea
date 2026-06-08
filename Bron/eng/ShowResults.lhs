> {-# LANGUAGE UnicodeSyntax #-}

ShowResults
William Clements
May 28, 2026

> module Eng.ShowResults (
>         showResults ) where
>
> import Control.Lens hiding ( element, ix )
> import Data.List
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Ord
> import qualified Data.Set                as Set
> import qualified Data.Text               as Text
> import Data.Text (Text)
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec

Modifiers to filter ouput =============================================================================================

> showLevel              :: GenSumLevel
> showLevel                                = GSRollLevel
>
> skipRaw                :: Bool
> skipRaw                                  = False

Start with the overall rollup and recurse down ========================================================================

> showResults       :: GenSum → IO (VB.Vector (VB.Vector Text))
> showResults rootGenSum                   = do
>   putStrLn $ unwords ["num Files", show nFiles]
>   putStrLn $ unwords ["num Insts", show nInsts]
>   putStrLn $ unwords ["num Zones", show nZones]
>   return $ showGenSum rootGenSum
>   where
>     nFiles, nInsts, nZones
>                        :: Int
>     (nFiles, nInsts, nZones)             = overallCounts rootGenSum
>
>     showGenSum         :: GenSum → VB.Vector (VB.Vector Text)
>     showGenSum gensum                    =
>       recurseOutput VB.++ VB.fromList [headerOutput, slateOutput, envOutput, statsOutput]
>       where
>         headerOutput                     = VB.fromList [Text.empty, Text.pack $ unwords [show (gensum ^. gsLevel), gensum ^. gsTag]]
>         slateOutput                      = if skipRaw
>                                              then VB.empty
>                                              else VB.map (Text.pack . show) (VB.filter scoop (gensum ^. gsGenSlate))
>                                                where scoop genData = not (isEmptyGenData genData)
>         envOutput                        =
>           VB.singleton (Text.pack $ unwords ["\nModulation Envelope configs (total=", show (total (gensum ^. gsModEnvMap)), "):\n"])
>           VB.++ showEnvMap (gensum ^. gsModEnvMap) (gensum ^. gsZoneCount)
>           VB.++ VB.singleton (Text.pack $ unwords ["\nVolume Envelope configs (total=", show (total (gensum ^. gsVolEnvMap)), "):\n"])
>           VB.++ showEnvMap (gensum ^. gsVolEnvMap) (gensum ^. gsZoneCount)
>           VB.++ VB.fromList [Text.pack "\n\n"]
>           where
>             total      :: Map EConfig Int → Int
>             total                        = sum . Map.elems
>         statsOutput                       =
>           VB.fromList [Text.pack "\nGenerator statistics:\n"]
>           VB.++ showStats gensum
>           VB.++ VB.singleton (Text.pack "\n\n")

Conditionally propagate showGenSum to subservients ====================================================================

>         recurseOutput                    = if fromEnum showLevel >= fromEnum (gensum ^. gsLevel)
>                                              then VB.empty
>                                              else VB.concatMap showGenSum (gensum ^. gsSubSums)

Collate (modulation or volume) envelope configurations by occurrence ==================================================

>     showEnvMap envMap zcount
>       | Map.null envMap                  = VB.empty
>       | otherwise                        = VB.fromList (map (uncurry showEConfig) arrivals)
>         where
>           arrivals     :: [(Int, EConfig)]    
>           arrivals                       = sortBy (comparing Down) (Map.foldrWithKey shuffle [] envMap)
>                                               where shuffle ec count acc = (count, ec) : acc
>           showEConfig  :: Int → EConfig → Text
>           showEConfig count ec           = Text.pack $ unwords [percent count zcount, show count, show ec]
>             where
>               percent  :: Int → Int → String
>               percent n denom           =
>                 let
>                   frac :: Double        = fromIntegral n / fromIntegral denom
>                 in
>                   show (round (frac * 100)::Int) ++ "%"

Compute and show numerical statistics for each value-bearing generator type ===========================================

>     showStats          :: GenSum → VB.Vector Text
>     showStats gensum                     = VB.concatMap showOneGen valueBearing
>       where
>         showOneGen genOne                = showOneGenData ((gensum ^. gsGenSlate) VB.! fromEnum genOne)
>         showOneGenData genData           =
>           let
>             skip       :: Bool           = isEmptyGenData genData
>             stats      :: [Text]         = [s0, s1, s2]
>               where
>                 s0                       = Text.pack $ unwords [show ix, show gen, show spec]
>                 s1                       = if Set.notMember gen noNumericDefault
>                                              then Text.unwords [spop, showStat (pMean, pStdDev)]
>                                              else Text.unwords [spop, Text.pack "n/a"]
>                 s2                       = Text.unwords [ssample, showStat (sMean, sStdDev)]
>
>             gen        :: GenEnum        = genData ^. gId
>             ix                           = fromEnum gen
>             spec                         = specVector VB.! ix
>
>             sindent, spop, ssample, popMean, sampleMean
>                        :: Text
>             sindent                      = Text.replicate 4 (Text.singleton ' ')
>
>             spop                         =
>               Text.unwords [sindent
>                           , Text.justifyRight 15 ' ' (Text.show (gensum ^. gsZoneCount))
>                           , sindent
>                           , Text.justifyRight 15 ' ' popMean]
>             ssample                      =
>               Text.unwords [sindent
>                           , Text.justifyRight 15 ' ' (Text.show (genData ^. gOccur))
>                           , sindent
>                           , Text.justifyRight 15 ' ' sampleMean]
>
>             popMean                      = Text.pack "pop.mean"
>             sampleMean                   = Text.pack "sample.mean"
>
>             (pMean, pStdDev)             = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gAccumSquares)
>                                              ((gensum ^. gsZoneCount) - (genData ^. gOccur))
>                                              (spec ^. gDefault)
>             (sMean, sStdDev)             = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gAccumSquares)
>                                              0
>                                              (spec ^. gDefault)
>
>             showStat   :: (Double, Double) → Text
>             showStat (m, s)              = Text.unwords [Text.show m, Text.pack "+-", Text.show s]
>
>             means      :: VB.Vector Text
>             means                        = if spec ^. gUnit /= NoUnit
>                                              then VB.singleton (Text.unwords [sindent, Text.show gresult])
>                                              else VB.empty
>               where
>                 gresult                  = GenResult
>                                              strUnit
>                                              ((convert . fromIntegral) (spec ^. gDefault))
>                                              (convert pMean)
>                                              (genData ^. gOccur)
>                                              (if genData ^. gOccur == 0
>                                                then Nothing
>                                                else Just (convert sMean))
>
>                 (strUnit, convert)       = unitAction (spec ^. gUnit)
>           in
>             if skip
>               then VB.empty
>               else VB.fromList stats VB.++ means
>
> dispersion             :: Int → Double → Double → Int → Int → (Double, Double)
> dispersion nVals accum_ accumSquares_ nDef valDef
>   | (nVals + nDef) == 0                   = error "dispersion: absence of vals would cause divide-by-zero"
>   | (nVals + nDef) == 1                   = (accum, 0)
>   | otherwise                             = (mean, stdDev)
>   where
>     denom, mean, stdDev, dubNDef, dubValDef, accum, accumSquares
>                        :: Double
>     denom                                = fromIntegral (nVals + nDef)
>     (dubNDef, dubValDef)                 = (fromIntegral nDef, fromIntegral valDef)
>     accum                                = accum_ + (dubNDef * dubValDef)
>     accumSquares                         = accumSquares_ + (dubNDef * (dubValDef * dubValDef))
>
>     mean                                 = accum / denom
>     stdDev                               = sqrt (ss / (denom - 1))
>       where
>         ss                               = accumSquares - ((accum * accum) / denom)
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
>         GSFileLevel    → (x + 1,     y,     z)
>         GSInstLevel    → (x,     y + 1,     z)
>         GSZoneLevel    → (x,         y,     z + 1)
>         _              → (x,         y,     z)

The End
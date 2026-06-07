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
> import qualified Data.Text               as Text
> import Data.Text (Text)
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec

Modifiers to filter ouput =============================================================================================

> showLevel              :: GenSumLevel
> showLevel                                = GSInstLevel
>
> skipRaw                :: Bool
> skipRaw                                  = False

Start with the overall rollup and recurse down ========================================================================

> showResults       :: GenSum → IO (VB.Vector (VB.Vector Text))
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
>     showGenSum         :: GenSum → VB.Vector (VB.Vector Text)
>     showGenSum gensum                    =
>       let
>         headerOutput                     = VB.fromList [Text.empty, Text.pack $ unwords [show (gensum ^. gsLevel), gensum ^. gsTag]]
>         slateOutput                      = if skipRaw
>                                              then VB.empty
>                                              else VB.map (Text.pack . show) (VB.filter scoop (gensum ^. gsGenSlate))
>                                                where scoop genData = not (isEmptyGenData genData)
>         envOutput                        =
>           VB.singleton (Text.pack $ unwords ["\nModulation Envelope configs (out of total=", show (total (gensum ^. gsModEnvMap)), "):\n"])
>           VB.++ showEnvMap (gensum ^. gsModEnvMap)
>           VB.++ VB.singleton (Text.pack $ unwords ["\nVolume Envelope configs (out of total=", show (total (gensum ^. gsVolEnvMap)), "):\n"])
>           VB.++ showEnvMap (gensum ^. gsVolEnvMap)
>           VB.++ VB.fromList [Text.pack "\n\n"]
>         statsOutput                       =
>           VB.fromList [Text.pack "\nGenerator statistics:\n"]
>           VB.++ showStats gensum
>           VB.++ VB.singleton (Text.pack "\n\n")

Recurse down to the next level ========================================================================================

>         recurseOutput                    = if fromEnum showLevel >= fromEnum (gensum ^. gsLevel)
>                                              then VB.empty
>                                              else VB.concatMap showGenSum (gensum ^. gsSubSums)
>
>         total          :: Map EConfig Int → Int
>         total                            = sum . Map.elems
>       in
>         recurseOutput VB.++ VB.fromList [headerOutput, slateOutput, envOutput, statsOutput]

Collate (modulation or volume) envelope configurations by occurrence ==================================================

>     showEnvMap envMap
>       | Map.null envMap                  = VB.empty
>       | otherwise                        = VB.fromList (map (uncurry showEConfig) arrivals)
>         where
>           arrivals     :: [(Int, EConfig)]    
>           arrivals                       = sortBy (comparing Down) (Map.foldrWithKey shuffle [] envMap)
>                                               where shuffle ec count acc = (count, ec) : acc
>           showEConfig  :: Int → EConfig → Text
>           showEConfig count ec           = Text.pack $ unwords [percent count kZone, show count, show ec]
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
>                 s1                       = if VB.notElem gen noNumericDefault
>                                              then Text.pack $ unwords [spop, showStat (pMean, pStdDev)]
>                                              else Text.pack $ unwords [spop, "n/a"]
>                 s2                       = Text.pack $ unwords [ssample, showStat (sMean, sStdDev)]
>
>             gen        :: GenEnum        = genData ^. gId
>             ix                           = fromEnum gen
>             spec                         = specVector VB.! ix
>
>             sindent, spop, ssample
>                        :: String
>             sindent                      = replicate 4 ' '
>             spop                         = unwords [sindent, "   pop"]
>             ssample                      = unwords [sindent, "sample"]
>
>             (pMean, pStdDev)             = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gAccumSquares)
>                                              (kZone - (genData ^. gOccur))
>                                              (spec ^. gDefault)
>             (sMean, sStdDev)             = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gAccumSquares)
>                                              0
>                                              (spec ^. gDefault)
>
>             showStat   :: (Double, Double) → String
>             showStat (m, s)              = unwords [show m, "+-", show s]
>
>             means      :: VB.Vector Text
>             means                        = if spec ^. gUnit /= NoUnit
>                                              then VB.singleton (Text.pack $ unwords [sindent, show gresult])
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
> dispersion             :: Int → Int → Double → Int → Int → (Double, Double)
> dispersion nVals accum__ accumSquares__ nDef valDef
>   | (nVals + nDef) == 0                   = error "dispersion: absence of vals would cause divide-by-zero"
>   | (nVals + nDef) == 1                   = (accum, 0)
>   | otherwise                             = (mean, stdDev)
>   where
>     denom, mean, stdDev, dubNDef, dubValDef, accum, accumSquares
>                        :: Double
>     denom                                = fromIntegral (nVals + nDef)
>     (dubNDef, dubValDef)                 = (fromIntegral nDef, fromIntegral valDef)
>     (accum_, accumSquares_)              = (fromIntegral accum__, accumSquares__)
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
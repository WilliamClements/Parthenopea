> {-# LANGUAGE UnicodeSyntax #-}

ShowResults
William Clements
May 28, 2026

> module Eng.ShowResults (
>         showResults ) where
>
> import Control.Lens hiding ( element, ix )
> import Control.Monad                     as CM
> import qualified Data.Bifunctor          as BF
> import Data.List
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
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

> showResults       :: GenSum → IO ()
> showResults rootGenSum                   = do
>   putStrLn $ unwords ["num Files", show nFiles]
>   putStrLn $ unwords ["num Insts", show nInsts]
>   putStrLn $ unwords ["num Zones", show nZones]
>   showGenSum rootGenSum
>   where
>     nFiles, nInsts, nZones
>                        :: Int
>     (nFiles, nInsts, nZones)             = overallCounts rootGenSum
>
> showGenSum             :: GenSum → IO ()
> showGenSum gensum                        = do
>   outputHeader
>   outputStats
>   outputEnv
>   outputSlate
>   recurseOutput
>   where
>     outputHeader, outputStats, outputEnv, outputSlate, recurseOutput
>                        :: IO ()
>     outputHeader                         = putStrLn $ unwords [show (gensum ^. gsLevel), gensum ^. gsTag]
>     outputStats                          = do
>       putStrLn $ unwords ["\nGenerator statistics:", show (gensum ^. gsZoneCount), "zones \n"]
>       CM.mapM_ statsOneGen allGens
>       putStrLn $ unwords ["\n\n"]
>     outputEnv                            = do
>       putStrLn $ unwords ["\nModulation Envelope configs (total=", show (total (gensum ^. gsModEnvMap)), "):\n"]
>       showEnvMap (gensum ^. gsModEnvMap) (gensum ^. gsZoneCount)
>       putStrLn $ unwords ["\nVolume Envelope configs (total=", show (total (gensum ^. gsVolEnvMap)), "):\n"]
>       showEnvMap (gensum ^. gsVolEnvMap) (gensum ^. gsZoneCount)
>       putStrLn "\n\n"
>       where
>         total          :: Map EConfig Int → Int
>         total                            = sum . Map.elems
>     outputSlate                          = do
>       CM.unless skipRaw
>         (do
>            let genDatas                  = VB.filter (not . isEmptyGenData) (gensum ^. gsGenSlate)
>            CM.mapM_ print genDatas)

Conditionally propagate showGenSum to subservients ====================================================================

>     recurseOutput                        = do
>       CM.unless
>         (fromEnum showLevel >= fromEnum (gensum ^. gsLevel))
>         (CM.mapM_ showGenSum (gensum ^. gsSubSums))

Collate (modulation or volume) envelope configurations by occurrence ==================================================

>     showEnvMap         :: Map EConfig Int → Int → IO ()
>     showEnvMap envMap zcount             = do
>       CM.unless (Map.null envMap) (CM.mapM_ (uncurry showEConfig) arrivals)
>       where
>         arrivals       :: [(Int, EConfig)]    
>         arrivals                         = sortBy (comparing Down) (Map.foldrWithKey shuffle [] envMap)
>                                               where shuffle ec count acc = (count, ec) : acc
>         showEConfig    :: Int → EConfig → IO ()
>         showEConfig count ec             = putStrLn $ unwords [percent count zcount, show ec]

Compute and show numerical statistics for each value-bearing generator type ===========================================

>     statsOneGen genOne                   = statsOneGenData ((gensum ^. gsGenSlate) VB.! fromEnum genOne)
>     statsOneGenData genData              = do
>       CM.unless noData (putStrLn $ Text.unpack stats)
>       where
>         gen            :: GenEnum        = genData ^. gGen
>         ix                               = fromEnum gen
>         spec                             = specVector VB.! ix
>
>         noData, noDefault
>                        :: Bool
>         noData                           = isEmptyGenData genData
>         noDefault                        = Set.member gen noNumericDefault
>         noStats                          = isNothing (spec ^. gClip) || (spec ^. gUnit) == NoUnit
>         readyClip                        = fromJust $ spec ^. gClip
>
>         (strUnit, convert)               = unitAction (spec ^. gUnit)
>
>         stats          :: Text           =
>           let
>             showStat mean stdDev         = Text.unwords [showMean, Text.pack "+-", showStdDev]
>               where
>                 showMean                 = Text.justifyRight 32 ' ' (Text.show mean)
>                 showStdDev               = Text.justifyLeft 32 ' ' (Text.show stdDev)
>             indent     :: Text           = Text.pack "      "
>
>             sHead      :: Text           = Text.unwords [  Text.justifyLeft 3 ' ' (Text.show ix)
>                                                          , Text.justifyLeft 32 ' ' (Text.show gen)
>                                                          , Text.show spec]
>
>             sPercent                     = percent (genData ^. gOccur) (gensum ^. gsZoneCount)
>             spMean     :: Text           = if noDefault
>                                              then Text.unwords [spop, Text.pack "n/a"]
>                                              else Text.unwords [spop, showStat pMean pStdDev]
>             ssMean     :: Text           = if noData
>                                              then Text.unwords [ssample, Text.pack "n/a"]
>                                              else Text.unwords [ssample, showStat sMean sStdDev]
>             genResult                    = GenResult
>                                              strUnit
>                                              (BF.bimap (convert . fromIntegral) (convert . fromIntegral) readyClip)
>                                              ((convert . fromIntegral) (spec ^. gDefault))
>             sResult                      = indent `Text.append` Text.show genResult
>
>             sOccur     :: Text           = indent `Text.append` Text.pack (unwords ["Occurence:", sPercent])
>           in
>             if noStats
>               then Text.unlines [sHead, sOccur]
>               else Text.unlines [sHead, sOccur, spMean, ssMean, sResult]
>
>         sindent, spop, ssample, popMean, sampleMean
>                        :: Text
>         sindent                          = Text.replicate 4 (Text.singleton ' ')
>
>         spop                             =
>           Text.unwords [sindent, sindent
>                       , Text.justifyRight 15 ' ' popMean]
>         ssample                          =
>           Text.unwords [sindent, sindent
>                       , Text.justifyRight 15 ' ' sampleMean]
>
>         popMean                          = Text.pack "pop.mean"
>         sampleMean                       = Text.pack "sample.mean"
>
>         (pMean, pStdDev)                 = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gAccumSquares)
>                                              ((gensum ^. gsZoneCount) - (genData ^. gOccur))
>                                              (fromIntegral (spec ^. gDefault))
>         (sMean, sStdDev)                 = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gAccumSquares)
>                                              0
>                                              (fromIntegral (spec ^. gDefault))
>
> dispersion             :: Int → Double → Double → Int → Double → (Double, Double)
> dispersion nVals accum_ accumSquares_ nDef valDef
>   | (nVals + nDef) == 0                   = error "dispersion: absence of vals would cause divide-by-zero"
>   | (nVals + nDef) == 1                   = (accum, 0)
>   | otherwise                             = (mean, stdDev)
>   where
>     denom, mean, stdDev, dubNDef, dubValDef, accum, accumSquares
>                        :: Double
>     denom                                = fromIntegral (nVals + nDef)
>     (dubNDef, dubValDef)                 = (fromIntegral nDef, valDef)
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
>
> percent                :: Int → Int → String
> percent n denom                          =
>   let
>     frac               :: Double         = fromIntegral n / fromIntegral denom
>     nPercent           :: Int            = round (frac * 100)
>     tPercent           :: Text           = Text.justifyRight 8 ' ' (Text.pack $ show nPercent ++ "%")
>     tOccur             :: Text           = Text.justifyRight 12 ' ' (Text.pack $ "(" ++ show n ++ ")")
>   in
>     Text.unpack $ tPercent `Text.append` tOccur

The End
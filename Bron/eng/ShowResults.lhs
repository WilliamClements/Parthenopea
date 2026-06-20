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
> import Data.Map.Strict ( Map )
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import Data.Ord
> import qualified Data.Text               as Text
> import Data.Text ( Text )
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
>   outputLeaders
>   outputStats
>   outputEnv
>   outputSlate
>   recurseOutput
>   where
>     outputHeader, outputStats, outputEnv, outputSlate, recurseOutput, outputLeaders
>                        :: IO ()
>     outputHeader                         = do
>       putStrLn ""
>       putStrLn $ unwords [show (gensum ^. gsLevel), gensum ^. gsTag]
>     outputLeaders                        = do
>       putStrLn $ unwords ["\nGenerators by popularity:", show (gensum ^. gsZoneCount), "zones"]
>       let genDatas                       = VB.toList $ VB.filter (not . isEmptyGenData) (gensum ^. gsGenSlate)
>       let leaders                        = sortOn (Down . evalEach) genDatas
>                                              where evalEach gd = gd ^. gOccur
>       CM.mapM_ printLeader leaders
>       where
>         printLeader gd                   =
>           let
>             gen        :: GenEnum        = gd ^. gGen
>             ix                           = fromEnum gen
>             sout       :: Text           = Text.unwords [  Text.justifyLeft 3 ' ' (Text.show ix)
>                                                          , Text.justifyLeft 32 ' ' (Text.show gen)
>                                                          , sPercent]
>             sPercent   :: Text           = Text.pack $ percent (gd ^. gOccur) (gensum ^. gsZoneCount)
>           in
>             putStrLn $ Text.unpack sout
>     outputStats                          = do
>       putStrLn $ unwords ["\nGenerator statistics:"]
>       CM.mapM_ statsOneGen allGens
>       putStrLn $ unwords ["\n\n"]
>     outputEnv                            = do
>       putStrLn $ unwords ["\nModulation Envelope configs (total=", show (total (gensum ^. gsModEnvMap)), "):\n"]
>       showEnvMap (gensum ^. gsModEnvMap) (gensum ^. gsZoneCount)
>       putStrLn $ unwords ["\nVolume Envelope configs (total=", show (total (gensum ^. gsVolEnvMap)), "):\n"]
>       showEnvMap (gensum ^. gsVolEnvMap) (gensum ^. gsZoneCount)
>       putStrLn "\n"
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
>       CM.unless noData (putStrLn $ Text.unpack $ Text.unlines stats)
>       where
>         gen            :: GenEnum        = genData ^. gGen
>         ix                               = fromEnum gen
>         spec                             = specVector VB.! ix
>
>         noData, noDefault, noStats
>                        :: Bool
>         noData                           = isEmptyGenData genData
>         noDefault                        = isNothing (spec ^. gClip)
>         noStats                          = (spec ^. gUnit) == NoUnit
>         readyClip                        = fromJust $ spec ^. gClip
>
>         (strUnit, convert)               = unitAction (spec ^. gUnit)
>
>         (pMean, pStdDev)                 = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gSquares)
>                                              ((gensum ^. gsZoneCount) - (genData ^. gOccur))
>                                              (fromIntegral (spec ^. gDefault))
>         (sMean, sStdDev)                 = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gAccum)
>                                              (genData ^. gSquares)
>                                              0
>                                              (fromIntegral (spec ^. gDefault))
>         (uMean, uStdDev)                 = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gUnitAccum)
>                                              (genData ^. gUnitSquares)
>                                              ((gensum ^. gsZoneCount) - (genData ^. gOccur))
>                                              (fromIntegral (spec ^. gDefault))
>         (vMean, vStdDev)                 = dispersion
>                                              (genData ^. gOccur)
>                                              (genData ^. gUnitAccum)
>                                              (genData ^. gUnitSquares)
>                                              0
>                                              (fromIntegral (spec ^. gDefault))
>
>         sindent        :: Text           = Text.replicate 4 (Text.singleton ' ')
>
>         makeLabel      :: Bool → Bool → Text
>         makeLabel pop u                  = (if pop then Text.pack "pop." else Text.pack "sample.")
>                                            `Text.append`
>                                            (if u then Text.pack "unit" else Text.pack "raw")
>
>         oneDis         :: Bool → Text → Double → Double → Text
>         oneDis isUnit label mean stdDev  = Text.unwords [oneLabel, showMean, Text.pack "+-", showStdDev]
>           where
>             cv         :: Double → Double
>             cv                           = if isUnit then convert else id
>
>             oneLabel                     = Text.justifyLeft 16 ' ' label 
>             showMean                     = Text.justifyRight 32 ' ' (Text.show $ cv mean)
>             showStdDev                   = Text.justifyLeft 32 ' ' (Text.show $ cv stdDev)
>
>         noDis          :: Text → Text
>         noDis label                      = Text.unwords [oneLabel, showNA]
>           where
>             oneLabel                     = Text.justifyLeft 16 ' ' label 
>             showNA                       = Text.justifyRight 32 ' ' (Text.pack "n/a")
>
>         package        :: Bool → [Text]
>         package isUnit                   =
>           let
>             pDispersion, sDispersion
>                        :: Text
>             pDispersion                  = if noDefault
>                                              then Text.unwords [  sindent, sindent
>                                                                 , noDis (makeLabel True isUnit)]
>                                              else Text.unwords [  sindent, sindent
>                                                                 , oneDis isUnit (makeLabel True isUnit) pMean pStdDev]
>             sDispersion                  = if noData
>                                              then Text.unwords [  sindent, sindent
>                                                                 , noDis (makeLabel False isUnit)]
>                                              else Text.unwords [  sindent, sindent
>                                                                 , oneDis isUnit (makeLabel False isUnit) sMean sStdDev]
>             uDispersion, vDispersion
>                        :: Text
>             uDispersion                  = if noDefault
>                                              then Text.unwords [  sindent, sindent
>                                                                 , noDis (makeLabel True isUnit)]
>                                              else Text.unwords [  sindent, sindent
>                                                                 , oneDis isUnit (makeLabel True isUnit) uMean uStdDev]
>             vDispersion                  = if noData
>                                              then Text.unwords [  sindent, sindent
>                                                                 , noDis (makeLabel False isUnit)]
>                                              else Text.unwords [  sindent, sindent
>                                                                 , oneDis isUnit (makeLabel False isUnit) vMean vStdDev]
>           in
>             if isUnit
>               then [uDispersion, vDispersion]
>               else [pDispersion, sDispersion]
>              
>         stats          :: [Text]         =
>           let
>             indent     :: Text           = Text.pack "      "
>
>             sHead      :: Text           = Text.unwords [  Text.justifyLeft 3 ' ' (Text.show ix)
>                                                          , Text.justifyLeft 32 ' ' (Text.show gen)
>                                                          , Text.show spec]
>
>             sPercent                     = percent (genData ^. gOccur) (gensum ^. gsZoneCount)
>             genResult                    = GenResult
>                                              strUnit
>                                              (BF.bimap (convert . fromIntegral) (convert . fromIntegral) readyClip)
>                                              ((convert . fromIntegral) (spec ^. gDefault))
>             sResult                      = if noDefault
>                                              then Text.empty
>                                              else indent `Text.append` Text.show genResult
>
>             sOccur     :: Text           = Text.justifyRight 68 ' ' (Text.pack (unwords ["Occurence:", sPercent]))
>           in
>             if noStats
>               then [sHead, sOccur]
>               else [sHead, sOccur] ++ package False ++ [sResult] ++ package True
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
>     stdDev                               = sqrt (max 0 ss / (denom - 1))
>       where
>         ss                               = accumSquares - ((accum * accum) / denom)
>
> testDis                :: [Int] → (Double, Double)
> testDis is                               =
>   let
>     square             :: Int → Double
>     square x                             = fromIntegral x * fromIntegral x
>   in
>     dispersion
>       (length is)
>       ((fromIntegral . sum) is)
>       (sum $ map square is)
>       0
>       0
>
> testDat                :: [Double] → (Double, Double)
> testDat xs                               =
>   let
>     square             :: Double → Double
>     square x                             = x * x
>   in
>     dispersion
>       (length xs)
>       (sum xs)
>       (sum $ map square xs)
>       0
>       0
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
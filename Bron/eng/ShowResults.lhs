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
> import Data.IntMap.Strict ( IntMap )
> import qualified Data.IntMap.Strict      as IntMap
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

> showResults            :: GenSum → IO ()
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
>         noDefault                        = isNothing (spec ^. gDefault)
>         noClip                           = isNothing (spec ^. gClip)
>         noStats                          = (spec ^. gUnit) == NoUnit
>         safeDefault    :: Int            = fromMaybe 0 (spec ^. gDefault)
>         safeClip       :: (Int, Int)     = fromMaybe (0, 0) (spec ^. gClip)
>
>         mcv            :: Maybe (Double → Double)
>         (strUnit, mcv)                   = unitAction (spec ^. gUnit)
>
>         m              :: IntMap Int     = genData ^. gHisto
>         n              :: Int            = (gensum ^. gsZoneCount) - (genData ^. gOccur) 
>         defaults       :: IntMap Int     = IntMap.fromList [(safeDefault, n)]
>
>         (pMean, pStdDev)                 = dispersion (IntMap.unionWith (+) m defaults) Nothing
>         (sMean, sStdDev)                 = dispersion m                                 Nothing
>         (uMean, uStdDev)                 = dispersion (IntMap.unionWith (+) m defaults) mcv
>         (vMean, vStdDev)                 = dispersion m                                 mcv
>
>         stats          :: [Text]
>         stats                            =
>           let
>             tHead      :: Text           = Text.unwords [  Text.justifyLeft 3 ' ' (Text.show ix)
>                                                          , Text.justifyLeft 32 ' ' (Text.show gen)
>                                                          , Text.show spec]
>             tOccur     :: Text           = Text.justifyRight 68 ' ' (Text.pack (unwords ["Occurence:", sPercent]))
>             sPercent                     = percent (genData ^. gOccur) (gensum ^. gsZoneCount)
>             genResult                    = InUnits
>                                              strUnit
>                                              (BF.bimap (converti mcv) (converti mcv) safeClip)
>                                              (converti mcv safeDefault)
>             tResult    :: Text           = if noDefault || noClip
>                                              then Text.empty
>                                              else largeIndent `Text.append` Text.show genResult
>             t1, t2, u1, u2, tt, uu
>                        :: [Text]
>             t1                           = [showDispersion noDefault    False True  (pMean, pStdDev)]
>             t2                           = [showDispersion noData       False False (sMean, sStdDev)]
>             u1                           = [showDispersion noDefault    True True   (uMean, uStdDev)]
>             u2                           = [showDispersion noData       True False  (vMean, vStdDev)]
>
>             tt                           = t1 ++ t2
>             uu                           = if isNothing mcv then [] else u1 ++ u2
>
>           in
>             if noStats
>               then    [tHead, tOccur]
>               else    [tHead, tOccur] ++ tt ++ [tResult] ++ uu
>
>         largeIndent                      = Text.pack (replicate 37 ' ')
>
> showDispersion         :: Bool → Bool → Bool → (Double, Double) → Text
> showDispersion noDispersion isUnit isPop (mean, stdDev)
>                                          =
>   if noDispersion
>     then showNa
>     else showDis
>   where
>     tLabel                               = Text.justifyLeft 16 ' ' makeLabel
>     showNa                               = Text.unwords [tLabel, Text.justifyRight 32 ' ' (Text.pack "n/a")]
>     showDis                              = Text.unwords [tLabel, showMean, Text.pack "+-", showStdDev]
>       where
>         showMean                         = Text.justifyRight 24 ' ' (Text.show mean)
>         showStdDev                       = Text.justifyLeft  24 ' ' (Text.show stdDev)
>
>     makeLabel                            = smallIndent
>                                              `Text.append`
>                                              (if isPop then Text.pack "pop." else Text.pack "sample.")
>                                              `Text.append`
>                                              (if isUnit then Text.pack "unit" else Text.pack "raw")
>     smallIndent                          = Text.pack (replicate 5 ' ')
>
> dispersion             :: IntMap Int → Maybe (Double → Double) → (Double, Double)
> dispersion m mcv
>   | null m                               = error "dispersion: absence of vals would cause divide-by-zero"
>   | round denom == (1::Int)              = (accum, 0)
>   | otherwise                            = (mean, stdDev)
>   where
>     mean                                 = accum / denom
>     stdDev                               = sqrt (accumSquares / (denom - 1))
>
>     denom, mean, stdDev, accum, accumSquares
>                        :: Double
>     denom                                = fromIntegral (IntMap.foldlWithKey grow 0 m)
>                                              where grow soFar _ v = soFar + v
>     accum                                = IntMap.foldlWithKey cumul8 0 m
>     accumSquares                         = IntMap.foldlWithKey devi8 0 m
>
>     cumul8, devi8      :: Double → Int → Int → Double
>     cumul8 soFar k v                     = soFar + fromIntegral v * converti mcv k
>     devi8 soFar k v                      = soFar + fromIntegral v * devi * devi
>                                              where devi = converti mcv k - mean
>
> testDis                :: [Int] → (Double, Double)
> testDis is                               = dispersion histo Nothing
>   where
>     histo                                = foldl' groove IntMap.empty is
>                                              where groove m i = IntMap.insertWith (+) i 1 m
>
> overallCounts          :: GenSum → (Int, Int, Int)
> overallCounts gensum                     = foldl' shredCounts (0, 0, 0) (flatten gensum)
>   where
>     flatten leaf                         = leaf : concatMap flatten (VB.toList (leaf ^. gsSubSums))
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
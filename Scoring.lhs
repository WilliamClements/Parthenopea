> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE UnicodeSyntax #-}

Scoring
William Clements
September 12, 2024

> module Scoring where
>
> import Data.Array.Unboxed
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Parthenopea.Siren
> import Parthenopea.Debug
> import SoundFont
  
notes on three kinds of scoring =======================================================================================

In order of when they occur in the overall process:

1. FuzzyFind        - For each *.sf2, we record items into overall roster when their names score high when
                      fuzzy-matched versus identifying words (e.g. piano) The instrument selections are _profoundly_
                      affected by fuzziness. But PercussionSound winners go mostly by matching "pitch" with zonal key
                      range.
  
2. artifact grading - Before rendering, we bind the tournament winner (highest grade) to each GM InstrumentName or
                      PercussionSound. Empirically measured attributes (stereo, number of splits, fuzziness, etc.),
                      are multiplied against "hard-coded" weights. The products are then summed to make final grade.

3. zone scoring     - While rendering, presented with a note, and a SoundFont Instrument already selected, we choose
                      zone (by _lowest_ score) that best fits required pitch, velocity, etc. Note that it is zone
                      scoring that strictly drives _render time_ choice of percussion as well.

Scoring stuff =========================================================================================================
 
> data SSHint =
>   DLow
>   | DMed
>   | DHigh
>   | DScore Double deriving Show
>
> scoreHint              :: SSHint → Rational
> scoreHint h                              = case h of
>                                              DLow            → -1
>                                              DMed            → 0
>                                              DHigh           → 1
>                                              DScore _        → 0 
>             
> foldHints              :: [SSHint] → Double
> foldHints                                = foldr ((+) . fromRational . scoreHint) 0
>
> ssWeights              :: [Double]
> ssWeights                                = [ fromRational weighHints
>                                            , fromRational weighStereo
>                                            , fromRational weigh24Bit
>                                            , fromRational weighResolution
>                                            , fromRational weighConformance
>                                            , fromRational weighFuzziness ]
>
> -- hints
> data HintId =
>   HintId {
>     pNameF             :: String
>   , pNameI             :: String
>   , mpNameZ            :: Maybe String} deriving (Eq, Ord, Show)
>
> type HintBody          = String
>
> myHints                :: [(HintId, HintBody)]
> myHints                                  =
>   [
>       (HintId "editHiDef.sf2"             "Rock Tom"            (Just "TOM_S446.446T.L08")      , "analyze")
>     , (HintId "editHiDef.sf2"             "Tuba"                (Just "Tuba.A-A*B")             , "analyze")
>   ]
>                           
> qqHints                :: Map HintId HintBody
> qqHints                                  = Map.fromList myHints

Utilities =============================================================================================================

> scorePitchDistance     :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> scorePitchDistance cand mrange           =
>   case mrange of
>     Nothing                   → 1000
>     Just (rangeMin, rangeMax) → let
>                                   dist1  = abs $ cand - rangeMin
>                                   dist2  = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 100 * max dist1 dist2
>                                   else 1000 * min dist1 dist2
>
> scoreVelocityDistance  :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> scoreVelocityDistance cand mrange        =
>   case mrange of
>     Nothing                   → 100
>     Just (rangeMin, rangeMax) → let
>                                   dist1  = abs $ cand - rangeMin
>                                   dist2  = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 10 * max dist1 dist2
>                                   else 100 * min dist1 dist2
>
> showEmpiricals         :: [Double] → (String, Int)
> showEmpiricals ds                        = (concatMap fun ds, 7 * length ds)
>   where
>     fun                :: Double → String
>     fun x                                = fillFieldL 6 (show $ roundBy 10 x)
>
> scoreOnsets  :: Int → [Double] → Array Int Int
> scoreOnsets nBins ts
>   | traceAlways trace_SO False           = undefined
>   | otherwise                            = hist (0, nBins - 1) is
>   where
>      safemax, safemin :: [Double] → Double
>      safemax ts0
>        | null ts0                        = 0
>        | otherwise                       = maximum ts0
>      safemin ts0
>        | null ts0                        = 0
>        | otherwise                       = minimum ts0
>   
>      hi, lo, fact      :: Double
>      hi                                  = safemax ts + 0.000_000_1
>      lo                                  = safemin ts - 0.000_000_1
>      fact                                = fromIntegral nBins / (hi - lo)
>
>      is                :: [Int]
>      is                                  = map (\d → floor $ (d - lo) * fact) ts
>
>      trace_SO =
>        unwords [ "scoreOnsets lo, hi, fact="
>                , show (safemin ts)
>                , ":", show (safemax ts)
>                , ":", show fact
>                , "ts="
>                , show (length ts)
>                , "is="
>                , show (length is)]
>
> data Grader =
>   Grader {
>     gorWeights         :: [Double]
>   , gorScalar          :: Double} 
> gradeEmpiricals        :: Grader → [Double] → ArtifactGrade
> gradeEmpiricals grader emps
>   | traceNot trace_GE False              = undefined
>   | otherwise                            = ArtifactGrade (consume (grader.gorScalar * lincombo)) emps
>   where
>     trace_GE                             =
>       unwords["gradeEmpiricals", "emps", show emps, "lincombo", show lincombo]
>     wSize                                = length grader.gorWeights
>     eSize                                = length emps
>     lincombo           :: Double         =
>       profess
>         (wSize == eSize)
>         (unwords ["gradeEmpiricals:", "wSize =", show wSize, "eSize =", show eSize])
>         (sum $ zipWith (*) emps grader.gorWeights)
>     consume            :: Double → Int
>     consume x                            =
>       profess
>         (x == clip (-1_000_000, 1_000_000) x)
>         (unwords ["gradeEmpiricals", "fatal scoring out of bounds numbers like", show x])
>         (round x)

Flags for customization ===============================================================================================

> qqDesireReStereo       :: Desires
> qqDesireRe24Bit        :: Desires
> qqDesireReSplits       :: Desires
> qqDesireReConformance  :: Desires
> qqDesireReFuzzy        :: Desires
>
> qqDesireReStereo                         = DPreferOn
> qqDesireRe24Bit                          = DPreferOn
> qqDesireReSplits                         = DPreferOn
> qqDesireReConformance                    = DPreferOn
> qqDesireReFuzzy                          = DPreferOn
>
> data Desires =
>   DAllOff | DPreferOff | DNeutral | DPreferOn | DAllOn deriving (Eq, Show)
>
> scoreDesire            :: Desires → Rational
> scoreDesire d = case d of
>   DAllOff          → (-1)
>   DPreferOff       → (-1)
>   DNeutral         → 0
>   DPreferOn        → 1
>   DAllOn           → 1
>
> scoreBool              :: Bool → Rational
> scoreBool b = if b then 1 else (-1)
>
> qqDesires              :: [Desires]
> qqDesires                                = [  qqDesireReStereo
>                                             , qqDesireRe24Bit
>                                             , qqDesireReSplits
>                                             , qqDesireReConformance
>                                             , qqDesireReFuzzy]
> qqDesires'             :: [Double]
> qqDesires'                               = map (fromRational . scoreDesire) qqDesires

Configurable parameters ===============================================================================================

> weighHints             :: Rational
> weighStereo            :: Rational
> weigh24Bit             :: Rational
> weighResolution        :: Rational
> weighConformance       :: Rational
> weighFuzziness         :: Rational

Edit the following ====================================================================================================

> weighHints                               = 10
> weighStereo                              = 2
> weigh24Bit                               = 0
> weighResolution                          = 3/2
> weighConformance                         = 3
> weighFuzziness                           = 3

The End
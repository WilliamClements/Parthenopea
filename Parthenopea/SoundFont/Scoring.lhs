> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE UnicodeSyntax #-}

Scoring
William Clements
September 12, 2024

> module Parthenopea.SoundFont.Scoring where
>
> import qualified Codec.SoundFont         as F
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.List
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord ( Down(Down) )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Modulation
> import Parthenopea.SoundFont.SFSpec
> import qualified Text.FuzzyFind          as FF
  
notes on three kinds of scoring =======================================================================================

In order of when they occur in the overall process:

1. FuzzyFind        - For each *.sf2, we record items when their names score high fuzzy-matched versus identifying
                      words (e.g. piano) and low fuzzy-matched versus contra-keywords. This becomes principal criterion
                      for instrument selection = pairing the GM ids like Flute with SoundFont instruments. Percussion
                      winners go mostly by matching "pitch" with zonal key range.
  
2. artifact grading - Before rendering, we bind the tournament winner (highest grade) to each GM InstrumentName or
                      PercussionSound. Empirically measured attributes (stereo, number of splits, fuzziness, etc.)
                      are weighted and summed.

3. zone scoring     - While rendering, presented with a note, and a SoundFont Instrument already selected, we choose
                      zone (by _lowest_ score) that best fits required pitch, velocity, etc. Note that it is zone
                      scoring that strictly drives _render time_ choice of percussion as well.

> type Fuzz = Double
>
> data FFMatches =
>   FFMatches {
>     ffInput            :: String
>   , ffInst             :: Map InstrumentName Fuzz
>   , ffPerc             :: Map PercussionSound Fuzz} deriving Show
> adhocFuzz              :: String → [String] → [Maybe FF.Alignment]
> adhocFuzz inp                            = map (`FF.bestMatch` inp)

handle "matching as" cache misses =====================================================================================

> createFuzzMap          :: ∀ a. (GMPlayable a, Eq a, Ord a) ⇒ String → (a → Maybe (a, [String])) → Map a Fuzz
> createFuzzMap inp getFFKeys              = Map.fromList $ mapMaybe (evalAgainstKindKeys inp) asLooks
>   where
>     -- weed out candidates with no fuzzy keys
>     asLooks            :: [(a, [String])]
>     asLooks                              = mapMaybe getFFKeys (select allKinds)
>
> evalAgainstKeys        :: String → [String] → Fuzz
> evalAgainstKeys inp keys                 = sum $ zipWith evalAgainstOne keys weights
>   where
>     lFactor            :: Double         = sqrt $ fromIntegral $ length keys
>     weights            :: [Double]       = [1.9 / lFactor
>                                           , 1.6 / lFactor
>                                           , 1.25 / lFactor
>                                           , 1.17 / lFactor
>                                           , 1.14 / lFactor]
>
>     evalAgainstOne     :: String → Double → Double
>     evalAgainstOne key weight            = maybe 0 ((* weight) . fromIntegral . FF.score) (FF.bestMatch key inp)
>
> evalAgainstKindKeys    :: String → (a, [String]) → Maybe (a, Fuzz)
> evalAgainstKindKeys inp (kind, keys)     = if tot <= 0 then Nothing else Just (kind, tot)
>   where
>     tot                :: Double         = evalAgainstKeys inp keys
>
> evalGenericPerc        :: String → Fuzz
> evalGenericPerc inp                      =
>   maximum (map (evalAgainstKeys inp . singleton) genericPercFFKeys)

apply fuzzyfind to mining instruments + percussion ====================================================================

> class GMPlayable a where
>   toGMKind             :: a → GMKind
>   select               :: ([InstrumentName], [PercussionSound]) → [a]
>   getFuzzMap           :: FFMatches → Map a Fuzz
>
> instance GMPlayable InstrumentName where
>   toGMKind                               = Left
>   select rost                            =
>     if narrowInstrumentScope
>       then fst rost
>       else fst allKinds
>   getFuzzMap                             = ffInst
>
> instance GMPlayable PercussionSound where
>   toGMKind                               = Right
>   select rost                            =
>     if narrowInstrumentScope
>       then snd rost
>       else snd allKinds
>   getFuzzMap                             = ffPerc
>
> class GMPlayable a ⇒ SFScorable a where
>   splitScore           :: a → [PreZone] → Double
>   fuzzFactor           :: a → Double
>
> instance SFScorable InstrumentName where
>   splitScore _ pzs                       = fromIntegral (length pzs)
>   fuzzFactor _                           = 7/8
>
> instance SFScorable PercussionSound where
>   splitScore _ _                         = 1
>   fuzzFactor _                           = 3/4

use "matching as" cache ===============================================================================================

> combineFF              :: ∀ a. (GMPlayable a, Eq a, Ord a) ⇒ Map a Fuzz → Map a Fuzz → Map a Fuzz
> combineFF ffpros ffcons                  =
>   Map.filter (>= 0) (Map.unionWith (+) ffpros (Map.map (* (- fromRational conRatio)) ffcons))
>
> computeFFMatches       :: String → FFMatches
> computeFFMatches inp                     = FFMatches inp
>                                              (combineFF ias ibs)
>                                              (combineFF pas pbs)
>   where
>     ias = createFuzzMap inp instrumentProFFKeys
>     ibs = createFuzzMap inp instrumentConFFKeys
>
>     pas = createFuzzMap inp percussionProFFKeys
>     pbs = createFuzzMap inp percussionConFFKeys
>
> zoneConforms           :: (PreZone, SFZone) → Bool
> zoneConforms (pz, zone)                  = not $ or unsupported
>   where
>     shdr                                 = effPZShdr pz
>
>     unsupported        :: [Bool]
>     unsupported                          =
>       [
>           case zone.zSampleMode of
>             Nothing                      → False
>             Just n                       → n == A.PressLoop
>         , case zone.zInitQ of
>             Nothing                      → False
>             Just n                       → n /= 0
>         , case zone.zScaleTuning of
>             Nothing                      → False
>             Just n                       → n /= 0 -- && n /= 100
>         , case zone.zExclusiveClass of
>             Nothing                      → False
>             Just n                       → n /= 0
>         , shdr.end < shdr.start
>         , shdr.endLoop < shdr.startLoop
>       ]

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
>
> data SFRuntime                           =
>   SFRuntime {
>     zFiles             :: Array Word SFFile
>   , zBoot              :: SFBoot
>   , zWinningRecord     :: WinningRecord}
> instance Show SFRuntime where
>   show runt                 =
>     unwords ["SFRuntime", show runt.zBoot, show (length runt.zWinningRecord.pWinningI, length runt.zWinningRecord.pWinningP)]
>
> type AgainstKindResult                   = Double
> 
> data ArtifactGrade =
>   ArtifactGrade {
>     pScore             :: Int
>   , pEmpiricals        :: [Double]} deriving (Show)
>
> data PerGMScored                         =
>   PerGMScored {
>     pArtifactGrade     :: ArtifactGrade
>   , pKind              :: GMKind
>   , pAgainstKindResult :: AgainstKindResult
>   , pPerGMKey          :: PerGMKey
>   , szI                :: String
>   , mszP               :: Maybe String} deriving (Show)
>
> data WinningRecord                       =
>   WinningRecord {
>     pWinningI          :: Map InstrumentName PerGMScored
>   , pWinningP          :: Map PercussionSound PerGMScored}
> seedWinningRecord      :: WinningRecord
> seedWinningRecord                        = WinningRecord Map.empty Map.empty
>
> data Matches                             =
>   Matches {
>     mSMatches          :: Map PreSampleKey FFMatches
>   , mIMatches          :: Map PerGMKey FFMatches}
> defMatches             :: Matches
> defMatches                               = Matches Map.empty Map.empty
> combineMatches         :: Matches → Matches → Matches
> combineMatches m1 m2                     =
>   m1{  mSMatches                         = Map.union m1.mSMatches    m2.mSMatches
>      , mIMatches                         = Map.union m1.mIMatches    m2.mIMatches}

tournament starts here ================================================================================================

> decideWinners          :: SFRuntime
>                           → ([InstrumentName], [PercussionSound]) 
>                           → Matches
>                           → IO (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
> decideWinners runt rost matches          = do
>   return wiExec
>
>   where
>     fName_                               = "decideWinners"
>
>     wiExec             :: (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>     wiExec                               = (wI', wP')
>       where
>         (wI, wP)                         = Map.foldlWithKey wiFolder (Map.empty, Map.empty) runt.zBoot.zPerInstCache         
>         wI'                              = Map.map (sortOn (Down . pScore . pArtifactGrade)) wI
>         wP'                              = Map.map (sortOn (Down . pScore . pArtifactGrade)) wP
>
>     wiFolder           :: (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>                           → PerGMKey → PerInstrument
>                           → (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>     wiFolder (wI, wP) pergmI_ perI
>       | traceIf trace_WIF False          = undefined
>       | otherwise                        = result
>       where
>         fName                            = unwords [fName_, "wiFolder"]
>         trace_WIF                        = unwords [fName, show pergmI_]
>
>         result         :: (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>
>         result                           =
>           case perI.pInstCat of
>             InstCatPerc _                → decidePerc
>             InstCatInst                  → decideInst
>             _                            → error $ unwords [fName, "only Inst and Perc are valid here"]
>
>         decideInst     :: (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>         decideInst                       = 
>           let
>             iMatches                     = deJust "mIMatches" (Map.lookup pergmI_ matches.mIMatches)
>             fuzzMap                      = getFuzzMap iMatches
>
>             i2Fuzz     :: Map InstrumentName Fuzz
>             i2Fuzz                       = Map.filterWithKey (\k _ → k `elem` select rost) fuzzMap
>
>             i2Fuzz'    :: [InstrumentName]
>             i2Fuzz'                      =
>               profess
>                 (not $ Map.null i2Fuzz)
>                 (unwords [fName, "unexpected empty matches"]) 
>                 (if multipleCompetes
>                    then Map.keys i2Fuzz
>                    else (singleton . fst) (Map.findMax i2Fuzz))
>           in
>             (foldl' (xaEnterTournament fuzzMap pergmI_ []) wI i2Fuzz', wP)
>     
>         decidePerc     :: (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>         decidePerc                       = 
>           let
>             pzs                          = map fst perI.pZones
>
>             bixen                        = case perI.pInstCat of
>               InstCatPerc x              → x
>               InstCatInst                → []
>               _                          → error $ unwords [fName, "only Inst and Perc are valid here"]
>
>             pergmsP                      = instrumentPercList pergmI_ bixen
>
>             pFolder    :: Map PercussionSound [PerGMScored]
>                           → PerGMKey
>                           → Map PercussionSound [PerGMScored]
>             pFolder wpFold pergmP        = xaEnterTournament fuzzMap pergmP [] wpFold kind
>               where
>                 mz     :: Maybe PreZone
>                 mz                       = pergmP.pgkwBag >>= findByBagIndex pzs
>                 mkind  :: Maybe PercussionSound
>                 mkind                    = mz >>= getAP >>= pitchToPerc
>                 kind                     = deJust (unwords["wpFolder", "mkind"]) mkind
>
>                 mffm   :: Maybe FFMatches
>                 mffm                     =
>                   mz >>= (zdSampleIndex . pzDigest)
>                      >>= Just . PreSampleKey pergmI_.pgkwFile
>                      >>= (`Map.lookup` matches.mSMatches)
>                 fuzzMap
>                        :: Map PercussionSound Fuzz
>                 fuzzMap                  = getFuzzMap $ deJust (unwords ["mffm"]) mffm
>
>                 getAP  :: PreZone → Maybe AbsPitch
>                 getAP pz                 = pz.pzDigest.zdKeyRange >>= (Just . fromIntegral . fst)
>           in
>             (wI, foldl' pFolder wP pergmsP)
>
>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           Map a Fuzz
>                           → PerGMKey
>                           → [SSHint]
>                           → Map a [PerGMScored]
>                           → a
>                           → Map a [PerGMScored]
>     xaEnterTournament fuzzMap pergm hints wins kind
>       | traceIf trace_XAET False         = undefined
>       | otherwise                        = Map.insertWith (++) kind [scored] wins
>       where
>         fName                            = unwords [fName_, "xaEnterTournament"]
>         trace_XAET                       =
>           unwords [fName, iName, fromMaybe "" mnameZ, show kind]
>
>         pergm_                           = pergm{pgkwBag = Nothing}
>         preI                             = runt.zBoot.zPreInstCache Map.! pergm_
>         iName                            = preI.piChanges.cnName
>         perI                             = runt.zBoot.zPerInstCache Map.! pergm_
>
>         scope_, scope  :: [(PreZone, SFZone)]
>         scope_                           =
>           case pergm.pgkwBag of
>             Nothing                      → perI.pZones
>             Just bagI                    →
>                    maybe
>                      (error $ unwords [fName, "findByBagIndex' returned a Nothing for"
>                                       , show pergm.pgkwFile, iName, show bagI])
>                      singleton
>                      (findByBagIndex' perI.pZones bagI)
>         scope                            =
>           profess
>             (not (null scope_))
>             (unwords[fName, "null scope", iName])
>             scope_
>             
>         mnameZ         :: Maybe String   = pergm.pgkwBag
>                                            >>= findByBagIndex' perI.pZones
>                                            >>= \(q, _) → Just (F.sampleName (effPZShdr q))
>
>         computeGrade   :: [(PreZone, SFZone)] → ArtifactGrade
>         computeGrade zs                  = gradeEmpiricals (Grader ssWeights 500) empiricals
>           where
>             empiricals :: [Double]       = [   foldHints hints
>                                              , fromRational $ scoreBool $ isStereoInst zs
>                                              , fromRational $ scoreBool $ is24BitInst zs
>                                              , computeResolution zs
>                                              , fromRational $ scoreBool $ all zoneConforms zs
>                                              , fuzz]
>             howgood                      = akResult - stands
>             fuzz       :: Double
>               | howgood > 0.000_001      = max 0 (logBase 2 howgood) * fuzzFactor kind
>               | otherwise                = 0
>   
>         scored         :: PerGMScored    =
>           PerGMScored (computeGrade scope) (toGMKind kind) akResult pergm iName mnameZ
>
>         computeResolution
>                        :: [(PreZone, SFZone)] → Double
>         computeResolution zs
>           | null zs                      = error $ unwords ["null zs"]
>           | otherwise                    = fromRational m1 * evalSplits kind + fromRational m2 * evalSampleSize
>           where
>             theSplit                     = splitScore kind (map fst zs)
>             evalSplits _
>               | theSplit <= 1            = 1
>               | otherwise                = log (m3 * theSplit)
>             evalSampleSize               = sum (map (durScoring . fst) zs) / fromIntegral (length zs)
>
>             m1                           = 2/3
>             m2                           = 1/3
>             m3                           = 3 * if isStereoInst zs then 1/2 else 1
>
>         durScoring     :: PreZone → Double
>         durScoring pz                    = if score < 0.01 then -10 else 1
>           where
>             shdr                         = effPZShdr pz
>             zd                           = pz.pzDigest
>             score                        = sampleSize / fromIntegral shdr.sampleRate
>
>             sampleSize :: Double
>             sampleSize                   = fromIntegral $ xEnd - xStart
>               where
>                 xStart :: Word           = shdr.start + fromIntegral zd.zdStart
>                 xEnd   :: Word           = shdr.end + fromIntegral zd.zdEnd
>
>         akResult                         = fromMaybe 0 (Map.lookup kind fuzzMap)
>
>     instrumentPercList :: PerGMKey → [Word] → [PerGMKey]
>     instrumentPercList pergmI            = map (\w → pergmI {pgkwBag = Just w})

emit standard output text detailing what choices we made for rendering GM items =======================================

> printChoices           :: SFRuntime
>                           → [InstrumentName]
>                           → [PercussionSound]
>                           → ([(Bool, [Emission])], [(Bool, [Emission])])
> printChoices runt is ps
>                                          = (map (showI runt.zWinningRecord) is, map (showP runt.zWinningRecord) ps)
>   where
>     showI              :: WinningRecord → InstrumentName → (Bool, [Emission])
>     showI winners kind
>       | isJust mpergm                    = (True, true kind mpergm)
>       | kind == Percussion               = (True, [Blanks 3, gmId kind, Unblocked "(pseudo-instrument)", EndOfLine])
>       | otherwise                        = (False, false kind)
>       where
>         mpergm                           = Map.lookup kind winners.pWinningI
>
>     showP               :: WinningRecord → PercussionSound → (Bool, [Emission])
>     showP winners kind
>       | isJust mpergm                    = (True, true kind mpergm)
>       | otherwise                        = (False, false kind)
>       where
>         mpergm                           = Map.lookup kind winners.pWinningP
>
>     true kind mpergm                     =
>       [Blanks 3, gmId kind, Unblocked " -> "] ++ showPerGM (fromJust mpergm) ++ [EndOfLine]
>     false kind                           =
>       [Blanks 3, gmId kind, Unblocked " not found", EndOfLine]
>
> showPerGM              :: PerGMScored → [Emission]
> showPerGM scored                         =
>   [emitShowL scored.pPerGMKey.pgkwFile 4] ++ [ToFieldL scored.szI 22] ++ showmZ
>   where
>     showmZ                               = maybe [] showZ scored.mszP
>     showZ name                           = [Unblocked name]
>
> dumpContestants        :: ∀ a. (Ord a, Show a, SFScorable a) ⇒ (a, [PerGMScored]) → [Emission]
> dumpContestants (kind, contestants)      = prolog ++ ex ++ epilog
>   where
>     prolog, ex, epilog :: [Emission]
>
>     prolog                               = emitLine [emitShowL kind 50]
>     ex                                   = concatMap dumpContestant contestants
>     epilog                               = emitLine []
>
> dumpContestant         :: PerGMScored → [Emission]
> dumpContestant scored                    = ex
>   where
>     showAkr            :: Double         = roundBy 10 scored.pAgainstKindResult
>     (showEmp, n)                         = showEmpiricals scored.pArtifactGrade.pEmpiricals
> 
>     ex = emitLine [ Blanks 4, emitShowL      scored.pPerGMKey.pgkwFile       8
>                             , ToFieldR       scored.szI                      22
>                   , Blanks 4, ToFieldR      (fromMaybe "" scored.mszP)       22
>                   , Blanks 4, emitShowL      scored.pArtifactGrade.pScore                   15
>                             , ToFieldL       showEmp                         n
>                             , emitShowR      showAkr                         8]

Utilities =============================================================================================================

> isPossible, stands, isConfirmed
>                        :: Double
> isPossible                               = 50
> stands                                   = 150
> isConfirmed                              = 250
>
> isPossible', stands', isConfirmed'
>                        :: Double → Bool
> isPossible' fuzz                         = fuzz > isPossible
> stands' fuzz                             = fuzz > stands
> isConfirmed' fuzz                        = fuzz > isConfirmed
>
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
> showEmpiricals dubs                      = (concatMap fun dubs, 7 * length dubs)
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
>      is                                  = map (\x → floor $ (x - lo) * fact) ts
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
> hist                   :: (Ix a, Integral b) ⇒ (a,a) → [a] → Array a b
> hist bnds is                             = accumArray (+) 0 bnds [(i, 1) | i ← is, inRange bnds i]
>
> data Grader =
>   Grader {
>     gorWeights         :: [Double]
>   , gorScalar          :: Double} 
> gradeEmpiricals        :: Grader → [Double] → ArtifactGrade
> gradeEmpiricals grader emps              = ArtifactGrade (consume (grader.gorScalar * lincombo)) emps
>   where
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
>
> genericInstFFKeys      :: [String]
> genericInstFFKeys                        = singleton "horn" 
>
> genericPercFFKeys      :: [String]
> genericPercFFKeys                        = ["kit", "kick", "drum", "perc", "hat"]
>
> instrumentConFFKeys    :: InstrumentName → Maybe (InstrumentName, [String])
> instrumentConFFKeys inst                 = embed inst keys
>   where
>    keys = case inst of
>       AcousticBass              → Just            ["drum", "brass", "bassoon", "tremolo", "elec"]
>       AcousticGrandPiano        → Just            ["drum", "harp", "upright", "bright", "mellow", "elec"]
>       ElectricGuitarJazz        → Just            ["drum", "bass"]
>       AcousticGuitarNylon       → Just            ["drum", "bass"]
>       AcousticGuitarSteel       → Just            ["drum", "bass"]
>       Agogo                     → Just            ["hi", "low"]
>       BrightAcousticPiano       → Just            ["elec", "grand"]
>       Cello                     → Just            ["tremolo", "strike", "pluck", "stacc"]
>       Contrabass                → Just $ singleton "tremolo"
>       ElectricBassFingered      → Just            ["acous", "brass", "bassoon"]
>       ElectricBassPicked        → Just            ["acous", "brass", "bassoon"]
>       EnglishHorn               → Just $ singleton "french"
>       Flute                     → Just $ singleton "pan"
>       FrenchHorn                → Just $ singleton "english"
>       FretlessBass              → Just            ["brass", "bassoon"]
>       GuitarFretNoise           → Just            ["clean", "nylon"]
>       HonkyTonkPiano            → Just            ["grand", "rhodes"]
>       OrchestraHit              → Just $ singleton "kit"
>       RhodesPiano               → Just            ["upright", "grand"]
>       SlapBass1                 → Just            ["brass", "bassoon"]
>       SlapBass2                 → Just            ["brass", "bassoon"]
>       SynthBass1                → Just            ["brass", "bassoon"]
>       SynthBass2                → Just            ["brass", "bassoon"]
>       SynthDrum                 → Just            ["bass"]
>       TelephoneRing             → Just $ singleton "string"
>       Trombone                  → Just            ["elec", "tom"]
>       Trumpet                   → Just $ singleton "mute"
>       Violin                    → Just            ["tremolo", "strike", "pluck", "stacc"]
>       _                         → Nothing
>
> instrumentProFFKeys    :: InstrumentName → Maybe (InstrumentName, [String])
> instrumentProFFKeys inst                 = embed inst keys
>   where
>     keys = case inst of
>       AcousticGrandPiano        → Just            ["piano", "grand", "concert"]
>       BrightAcousticPiano       → Just            ["piano", "bright", "brite"]
>       ElectricGrandPiano        → Just            ["piano", "elec"]
>       HonkyTonkPiano            → Just            ["honky", "tonk", "piano"]
>       RhodesPiano               → Just            ["rhodes", "piano"]
>       ChorusedPiano             → Just            ["chorused", "piano"]
>       Harpsichord               → Just            ["harpsi", "harpsichord"]
>       Clavinet                  → Just $ singleton "clav"
>       Celesta                   → Just $ singleton "celesta"
>       Glockenspiel              → Just $ singleton "glockenspiel"
>       MusicBox                  → Just $ singleton "musicbox"
>       Vibraphone                → Just            ["vibra", "phone"]
>       Marimba                   → Just $ singleton "marimba"
>       Xylophone                 → Just $ singleton "xylo"
>       TubularBells              → Just            ["tubular", "bells"]
>       Dulcimer                  → Just $ singleton "dulcimer"
>       HammondOrgan              → Just            ["organ", "hamm"]
>       PercussiveOrgan           → Just            ["organ", "percuss"]
>       RockOrgan                 → Just            ["organ", "rock"] 
>       ChurchOrgan               → Just            ["organ", "church"]
>       ReedOrgan                 → Just            ["organ", "reed", "accord"]
>       Accordion                 → Just $ singleton "accord"
>       Harmonica                 → Just $ singleton "harmonica"
>       TangoAccordion            → Just            ["accordion", "tango"]
>       AcousticGuitarNylon       → Just            ["nylon", "guit", "acous"]
>       AcousticGuitarSteel       → Just            ["steel", "guit", "acous"]
>       ElectricGuitarJazz        → Just            ["jazz", "guit", "elec"]
>       ElectricGuitarClean       → Just            ["clean", "guit", "elec"]
>       ElectricGuitarMuted       → Just            ["mute", "guit", "elec"]
>       OverdrivenGuitar          → Just            ["over", "driv", "guit"]
>       DistortionGuitar          → Just            ["dist", "guit", "fuzz"]
>       GuitarHarmonics           → Just            ["harmonics", "guit"]
>       AcousticBass              → Just            ["bass", "acous"]
>       ElectricBassFingered      → Just            ["bass", "finger", "elec"]
>       ElectricBassPicked        → Just            ["bass", "pick", "elec"]
>       FretlessBass              → Just            ["fret", "less", "bass"] 
>       SlapBass1                 → Just            ["bass", "slap", "1"]
>       SlapBass2                 → Just            ["bass", "slap", "2"]
>       SynthBass1                → Just            ["bass", "synth", "1"]
>       SynthBass2                → Just            ["bass", "synth", "2"]
>       Violin                    → Just $ singleton "violin"
>       Viola                     → Just $ singleton "viola"
>       Cello                     → Just $ singleton "cello"
>       Contrabass                → Just $ singleton "contrabass"
>       TremoloStrings            → Just            ["tremolo", "string"]
>       PizzicatoStrings          → Just            ["string", "pizzicato"]
>       OrchestralHarp            → Just            ["harp", "harp", "orchest", "concert"]
>       Timpani                   → Just            ["timpani", "timp"]
>       StringEnsemble1           → Just            ["ensemble", "string", "1"]
>       StringEnsemble2           → Just            ["ensemble", "string", "2"]
>       SynthStrings1             → Just            ["synth", "string", "1"]
>       SynthStrings2             → Just            ["synth", "string", "2"]
>       ChoirAahs                 → Just            ["choir", "aahs", "chorus"]
>       VoiceOohs                 → Just            ["voice", "oohs", "chorus"]
>       SynthVoice                → Just            ["voice", "synth"]
>       OrchestraHit              → Just            ["orchest", "hit"]
>       Trumpet                   → Just            ["trumpet", "trump"]
>       Trombone                  → Just $ singleton "trom"
>       Tuba                      → Just $ singleton "tuba"
>       MutedTrumpet              → Just            ["trumpet", "mute"]
>       FrenchHorn                → Just            ["horn", "french"]
>       BrassSection              → Just            ["brass", "section"]
>       SynthBrass1               → Just            ["brass", "synth", "1"]
>       SynthBrass2               → Just            ["brass", "synth", "2"]
>       SopranoSax                → Just            ["sax" , "sopr"]
>       AltoSax                   → Just            ["sax" , "alto"]
>       TenorSax                  → Just            ["sax" , "tenor"]
>       BaritoneSax               → Just            ["sax" , "bari"]
>       Oboe                      → Just $ singleton "oboe"
>       Bassoon                   → Just $ singleton "bassoon"
>       EnglishHorn               → Just            ["horn", "english"]
>       Clarinet                  → Just $ singleton "clarinet"
>       Piccolo                   → Just $ singleton "piccolo"
>       Flute                     → Just $ singleton "flute"
>       Recorder                  → Just $ singleton "recorder"
>       PanFlute                  → Just $ singleton "panflute"
>       BlownBottle               → Just            ["bottle", "blown"]
>       Shakuhachi                → Just $ singleton "shakuhachi"
>       Whistle                   → Just $ singleton "whistle"
>       Ocarina                   → Just $ singleton "ocarina"
>       Lead1Square               → Just $ singleton "lead1square"
>       Lead2Sawtooth             → Just $ singleton "lead2sawtooth"
>       Lead3Calliope             → Just $ singleton "lead3calliope"
>       Lead4Chiff                → Just $ singleton "lead4chiff"
>       Lead5Charang              → Just $ singleton "lead5charang"
>       Lead6Voice                → Just $ singleton "lead6voice"
>       Lead7Fifths               → Just $ singleton "lead7fifths"
>       Lead8BassLead             → Just $ singleton "lead8basslead"
>       Pad1NewAge                → Just             ["new", "age"]
>       Pad2Warm                  → Just $ singleton "pad2warm"
>       Pad3Polysynth             → Just $ singleton "pad3polysynth"
>       Pad4Choir                 → Just $ singleton "pad4choir"
>       Pad5Bowed                 → Just $ singleton "pad5bowed"
>       Pad6Metallic              → Just $ singleton "pad6metallic"
>       Pad7Halo                  → Just             ["halo", "pad"]
>       Pad8Sweep                 → Just             ["sweep", "pad"]
>       FX1Train                  → Just $ singleton "train"
>       FX2Soundtrack             → Just $ singleton "soundtrack"
>       FX3Crystal                → Just $ singleton "crystal"
>       FX4Atmosphere             → Just $ singleton "atmosphere"
>       FX5Brightness             → Just $ singleton "brightness"
>       FX6Goblins                → Just $ singleton "goblins"
>       FX7Echoes                 → Just $ singleton "echoes"
>       FX8SciFi                  → Just $ singleton "scifi"
>       Sitar                     → Just $ singleton "sitar"
>       Banjo                     → Just $ singleton "banjo"
>       Shamisen                  → Just $ singleton "shamisen"
>       Koto                      → Just $ singleton "koto"
>       Kalimba                   → Just $ singleton "kalimba"
>       Bagpipe                   → Just $ singleton "bagpipe"
>       Fiddle                    → Just $ singleton "fiddle"
>       Shanai                    → Just $ singleton "shanai"
>       TinkleBell                → Just            ["bell", "tinkle"]
>       Agogo                     → Just $ singleton "agogo"
>       SteelDrums                → Just            ["drums", "steel"]
>       Woodblock                 → Just $ singleton "woodblock"
>       TaikoDrum                 → Just            ["drum", "taiko"]
>       MelodicDrum               → Just            ["drum", "melodic"]
>       SynthDrum                 → Just            ["drum", "synth"]
>       ReverseCymbal             → Just            ["cymbal", "reverse"]
>       GuitarFretNoise           → Just            ["fret", "noise", "guit"]
>       BreathNoise               → Just            ["breath", "noise"]
>       Seashore                  → Just $ singleton "seashore"
>       BirdTweet                 → Just            ["bird", "tweet"]
>       TelephoneRing             → Just            ["tele", "ring"]
>       Helicopter                → Just $ singleton "helicopter"
>       Applause                  → Just $ singleton "applause"
>       Gunshot                   → Just $ singleton "gunshot"
>       _                         → Nothing
>
> percussionConFFKeys    :: PercussionSound → Maybe (PercussionSound, [String])
> percussionConFFKeys p = embed p keys
>   where
>     keys = case p of
>       AcousticSnare             → Just            ["elec"]
>       AcousticBassDrum          → Just            ["elec"]
>       _                         → Nothing
>
> percussionProFFKeys    :: PercussionSound → Maybe (PercussionSound, [String])
> percussionProFFKeys p = embed p keys
>   where
>     keys = case p of
>       AcousticBassDrum          → Just            ["drum", "acous", "bass", "concert"]
>       BassDrum1                 → Just            ["kick", "drum", "bass"]
>       SideStick                 → Just            ["side", "stick"]
>       AcousticSnare             → Just            ["snare", "drum", "acous"]
>       HandClap                  → Just            ["clap", "hand"]
>       ElectricSnare             → Just            ["snare", "elec", "drum"]
>       LowFloorTom               → Just            ["tom", "floor", "low"]
>       ClosedHiHat               → Just            ["hihat", "close"]
>       HighFloorTom              → Just            ["tom", "high", "floor"]
>       PedalHiHat                → Just            ["hihat", "pedal"]
>       LowTom                    → Just            ["tom", "low"]
>       OpenHiHat                 → Just            ["hihat", "open"]
>       LowMidTom                 → Just            ["tom", "mid", "low"]
>       HiMidTom                  → Just            ["tom", "high", "mid"]
>       CrashCymbal1              → Just            ["crash", "cymbal", "1"]
>       HighTom                   → Just            ["tom", "high"]
>       RideCymbal1               → Just            ["cymbal", "ride", "1"]
>       ChineseCymbal             → Just            ["cymbal", "chinese"]
>       RideBell                  → Just            ["bell", "ride"]
>       Tambourine                → Just            ["tambo"]
>       SplashCymbal              → Just            ["cymbal", "splash"]
>       Cowbell                   → Just            ["cowbell"]
>       CrashCymbal2              → Just            ["crash", "cymbal", "2"]
>       Vibraslap                 → Just            ["vibraslap"]
>       RideCymbal2               → Just            ["cymbal", "ride", "2"]
>       HiBongo                   → Just            ["bongo", "hi"]
>       LowBongo                  → Just            ["bongo", "low"]
>       MuteHiConga               → Just            ["conga", "mute", "hi"]
>       OpenHiConga               → Just            ["conga", "open", "hi"]
>       LowConga                  → Just            ["conga", "low"]
>       HighTimbale               → Just            ["timbale", "hi"]
>       LowTimbale                → Just            ["timbale", "low"]
>       HighAgogo                 → Just            ["agogo", "hi"]
>       LowAgogo                  → Just            ["agogo", "low"]
>       Cabasa                    → Just            ["cabasa"]
>       Maracas                   → Just            ["maracas"]
>       ShortWhistle              → Just            ["whistle", "short"]
>       LongWhistle               → Just            ["whistle", "long"]
>       ShortGuiro                → Just            ["guiro", "short"]
>       LongGuiro                 → Just            ["guiro", "long"]
>       Claves                    → Just            ["claves"]
>       HiWoodBlock               → Just            ["woodblock", "hi"]
>       LowWoodBlock              → Just            ["woodblock", "low"]
>       MuteCuica                 → Just            ["cuica", "mute"]
>       OpenCuica                 → Just            ["cuica", "open"]
>       MuteTriangle              → Just            ["triangle", "mute"]
>       OpenTriangle              → Just            ["triangle", "open"]
>
> embed                  :: a → Maybe b → Maybe (a, b)
> embed kind                               = fmap (kind,)

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
> scoreDesire x = case x of
>   DAllOff          → (-1)
>   DPreferOff       → (-1)
>   DNeutral         → 0
>   DPreferOn        → 1
>   DAllOn           → 1
>
> scoreBool              :: Bool → Rational
> scoreBool x = if x then 1 else (-1)
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
>
> multipleCompetes       :: Bool
> multipleCompetes                         = True
>
> conRatio, absorbRatio  :: Rational
> conRatio                                 = 3/4
> absorbRatio                              = 7/10
>
> narrowInstrumentScope  :: Bool
> narrowInstrumentScope                    = True
> allowSpecifiedCrossovers, allowInferredCrossovers
>                        :: Bool
> allowSpecifiedCrossovers                 = False
> allowInferredCrossovers                  = False

The End
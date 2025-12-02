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

> module Parthenopea.SoundFont.Scoring
>        ( adhocFuzz
>        , combineMatches
>        , computeFFMatches
>        , decideWinners
>        , defMatches
>        , establishWinners
>        , GMChoices(..)
>        , gradeEmpiricals
>        , isConfirmed
>        , isConfirmed'
>        , isPossible
>        , isPossible'
>        , Matches(..)
>        , myHints
>        , PerGMScored(..)
>        , qqHints
>        , scoreOnsets
>        , showPerGM
>        , stands
>        , stands'
>        ) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.IntMap.Strict (IntMap)
> import qualified Data.IntMap as IntMap
> import Data.IntSet (IntSet)
> import qualified Data.IntSet as IntSet
> import Data.List
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord ( Down(Down) )
> import qualified Data.Vector.Strict      as VB
> import Debug.Trace ( traceIO )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
> import qualified Text.FuzzyFind          as FF
  
notes on three kinds of scoring =======================================================================================

In order of when they occur in the overall process:

1. FuzzyFind        - For each *.sf2, we rank Sample and Instrument items whose names (strings) score highly when
                      fuzzy-matched versus keywords like "piano". Also that score poorly when fuzzy-matched versus
                      contra-keywords! The goal here is to pair a MIDI/GM instrument number with a good SoundFont
                      Instrument for rendering music.
                      
                      Percussion winners go mostly by matching incoming pitch with Zonal key range.
  
2. artifact grading - Before rendering, we bind the tournament winner (highest grade) to each GM InstrumentName or
                      PercussionSound. Empirically measured attributes (stereo, number of splits, fuzziness, etc.)
                      are weighted and summed.

3. zone scoring     - Nowadays  we just crunch all the pitch and velocity ranges so that incoming notes are
                      immediately mapped to suitable Zones.

> adhocFuzz              :: String → [String] → [Maybe FF.Alignment]
> adhocFuzz inp                            = map (`FF.bestMatch` inp)

use "matching as" cache ===============================================================================================

> data PerGMScored                         =
>   PerGMScored {
>     pArtifactGrade     :: ArtifactGrade
>   , pKind              :: GMKind
>   , pAgainstKindResult :: AgainstKindResult
>   , pPerGMKey          :: PerGMKey
>   , szI                :: String
>   , mszP               :: Maybe String} deriving (Show)
> goodScore              :: PerGMScored → Bool
> goodScore score                          =
>   score.pAgainstKindResult > 0 && fromIntegral score.pArtifactGrade.pScore > stands
> showPerGM              :: PerGMScored → [Emission]
> showPerGM scored                         =
>   [emitShowL scored.pPerGMKey.pgkwFile 5] ++ [ToFieldL scored.szI 22] ++ showmZ
>   where
>     showmZ                               = maybe [] showZ scored.mszP
>     showZ name                           = [Unblocked name]
>
> data GMChoices                           =
>   GMChoices {
>     gmFound          :: Bool
>   , gmPerGMKey       :: Maybe PerGMKey
>   , gmEmission       :: [Emission]}
>   deriving (Eq, Ord)
>
> computeFFMatches       :: Rational → String → Bool → FFMatches
> computeFFMatches conRatio inp narrow     = FFMatches 
>                                              inp
>                                              (combineFF ias ibs)
>                                              (combineFF pas pbs)
>   where
>     combineFF ffpros ffcons              =
>       Map.filter (>= 0) (Map.unionWith (+) ffpros (Map.map (* (- fromRational conRatio)) ffcons))
>
>     createFuzzMap getFFKeys              =
>       Map.fromList $ mapMaybe evalAgainstKindKeys (mapMaybe getFFKeys (select allKinds narrow))
>
>     evalAgainstKindKeys (kind, keys)     = if tot <= 0 then Nothing else Just (kind, tot)
>                                              where tot = evalAgainstKeys keys
>
>     evalAgainstKeys keys                 = sum $ zipWith evalAgainstOne keys weights
>       where
>         lFactor            :: Double         = sqrt $ fromIntegral $ length keys
>         weights            :: [Double]       = [1.9 / lFactor
>                                               , 1.6 / lFactor
>                                               , 1.25 / lFactor
>                                               , 1.17 / lFactor
>                                               , 1.14 / lFactor]
>
>     evalAgainstOne     :: String → Double → Double
>     evalAgainstOne key weight            = maybe 0 ((* weight) . fromIntegral . FF.score) (FF.bestMatch key inp)
>
>     ias = createFuzzMap instrumentProFFKeys
>     ibs = createFuzzMap instrumentConFFKeys
>
>     pas = createFuzzMap percussionProFFKeys
>     pbs = createFuzzMap percussionConFFKeys
>
> zoneConforms           :: PreZone → Bool
> zoneConforms pz                          = not $ or unsupported
>   where
>     zone                                 = pz.pzSFZone
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
>         , isJust zone.zKeyToModEnvHold
>         , isJust zone.zKeyToModEnvDecay
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

tournament starts here ================================================================================================

> establishWinners       :: ([InstrumentName], [PercussionSound])
>                           → (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>                           → IO (Map InstrumentName GMChoices, Map PercussionSound GMChoices)
> establishWinners rost (wI_, wP_)
>                                          = do
>   let (wI, wP)                           = (Map.map head wI_, Map.map head wP_)
>   return (BF.bimap (foldl' (buildUp wI) Map.empty) (foldl' (buildUp wP) Map.empty) rost)
>   where
>     buildUp ref m kind                   = Map.insert kind (kindChoices ref kind) m
>     kindChoices m k
>       | isJust pergm                     =
>           GMChoices
>             True
>             pergm
>             (trueChoice k (deJust (unwords ["establishWinners kindChoices"]) mscored))
>       | specialCase k                    =
>           GMChoices
>             True
>             pergm
>             [Blanks 3, gmId k, Unblocked "(pseudo-instrument)", EndOfLine]
>       | otherwise                        =
>           GMChoices
>             False
>             Nothing
>             (falseChoice k)
>       where
>         mscored                          = Map.lookup k m
>         pergm                            = mscored >>= (Just . pPerGMKey)
>
>         trueChoice kind scored           =
>           [Blanks 3, gmId kind, Unblocked " ... "] ++ showPerGM scored ++ [EndOfLine]
>         falseChoice kind                 =
>           [Blanks 3, gmId kind, Unblocked " not found", EndOfLine]
>
> decideWinners          :: Directives
>                           → ([InstrumentName], [PercussionSound]) 
>                           → VB.Vector SFFileBoot
>                           → Map PerGMKey PerInstrument
>                           → Matches
>                           → IO (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
> decideWinners dives rost vFiles cache matches   = do
>   CM.when diagnosticsEnabled             (traceIO $ unwords [fName__, show $ length cache])
>   return wiExec
>
>   where
>     fName__                              = "decideWinners"
>
>     narrow                               = dives.narrowRosterForBoot
>
>     wiExec             :: (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>     wiExec                               = (wI', wP')
>       where
>         (wI, wP)                         = Map.foldlWithKey wiFolder (Map.empty, Map.empty) cache    
>         wI'                              = Map.map (sortOn (Down . pScore . pArtifactGrade)) wI
>         wP'                              = Map.map (sortOn (Down . pScore . pArtifactGrade)) wP
>
>     wiFolder           :: (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>                           → PerGMKey → PerInstrument
>                           → (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>     wiFolder (wI, wP) pergmI_ perI       = (decideInst, decidePerc)
>       where
>         fName_                           = unwords [fName__, "wiFolder"]
>
>         sffile                           = vFiles VB.! pergmI_.pgkwFile
>
>         decideInst     :: Map InstrumentName [PerGMScored]
>         decideInst                       = proposeXAs iMatches wI pergmI_
>           where
>             iMatches = deJust "iMatches" (Map.lookup pergmI_ matches.mIMatches)
>     
>         decidePerc     :: Map PercussionSound [PerGMScored]
>         decidePerc                       = foldl' pFolder wP pergmsP
>           where
>             pergmsP                      = instrumentPercList pergmI_ perI.pBixen
>
>             pFolder wpFold pergmP
>               | traceNot trace_PF False  = undefined
>               | null mkind               = wpFold
>               | null mffm                = wpFold
>               | otherwise                = proposeXAs ffm wpFold pergmP
>               where
>                 fName                    = unwords [fName_, "pFolder"]
>                 trace_PF                 = unwords [fName, show pergmP, show mz
>                                                   , show (mz >>= getAP)
>                                                   , show (mz >>= getAP >>= pitchToPerc)]
>
>                 mz     :: Maybe PreZone
>                 mz                       = pergmP.pgkwBag >>= (Just . accessPreZone "pergmP" sffile.zPreZones . fromIntegral)
>                 mkind  :: Maybe PercussionSound
>                 mkind                    = mz >>= getAP >>= pitchToPerc
>                 mffm   :: Maybe FFMatches
>                 mffm                     =
>                   mz >>= (zdSampleIndex . pzDigest)
>                      >>= Just . PreSampleKey pergmI_.pgkwFile
>                      >>= (`Map.lookup` matches.mSMatches)
>                 ffm                      = deJust (unwords [fName, "mffm"]) mffm
>
>                 getAP  :: PreZone → Maybe AbsPitch
>                 getAP pz                 =
>                   pz.pzDigest.zdKeyRange
>                   >>= Just . singleton
>                   >>= Just . (++) (singleton percPitchRange)
>                   >>= intersectRanges
>                   >>= Just . fromIntegral . fst
>
>     proposeXAs         :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           FFMatches
>                           → Map a [PerGMScored]
>                           → PerGMKey
>                           → Map a [PerGMScored]
>     proposeXAs iMatches wX pergmX    = foldl' (xaEnterTournament iMatches pergmX []) wX i2Fuzz'
>       where
>         i2Fuzz                       = Map.filterWithKey isInRoster (getFuzzMap iMatches)
>                                          where isInRoster k _ = k `elem` select rost narrow
>         i2Fuzz'                      =
>           if dives.multipleCompetes
>             then Map.keys i2Fuzz
>             else (singleton . fst) (Map.findMax i2Fuzz)
>
>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           FFMatches
>                           → PerGMKey
>                           → [SSHint]
>                           → Map a [PerGMScored]
>                           → a
>                           → Map a [PerGMScored]
>     xaEnterTournament ffm pergm hints wins kind
>       | traceIf trace_XAET False         = undefined
>       | goodScore scored                 = Map.insertWith (++) kind [scored] wins
>       | otherwise                        = wins
>       where
>         fName                            = unwords [fName__, "xaEnterTournament"]
>         trace_XAET                       =
>           unwords [fName, iName, show pergm, show kind, show perI.pBixen]
>
>         sffile                           = vFiles VB.! pergm.pgkwFile
>         pergm_                           = pergm{pgkwBag = Nothing}
>         perI                             = cache Map.! pergm_
>         iName                            = perI.piChanges.cnName
>
>         scope                            =
>           let
>             oneZone bix                  = IntMap.singleton bix (accessPreZone "oneZone" sffile.zPreZones bix)
>           in
>             maybe (accessPreZones "scope" sffile.zPreZones perI.pBixen) (oneZone . fromIntegral) pergm.pgkwBag
>
>         mnameZ         :: Maybe String   = pergm.pgkwBag
>                                            >>= (Just . accessPreZone "mnameZ" sffile.zPreZones . fromIntegral)
>                                            >>= \q → Just (F.sampleName (effPZShdr q))
>
>         computeGrade   :: IntMap PreZone → ArtifactGrade
>         computeGrade pzs                 = gradeEmpiricals (Grader ssWeights 500) empiricals
>           where
>             zs                           = IntMap.elems pzs
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
>                        :: [PreZone] → Double
>         computeResolution pzs
>           | null pzs                     = error $ unwords [fName, "null pzs"]
>           | otherwise                    = m1 * evalSplits kind + m2 * evalSampleSize
>           where
>             theSplit                     = splitScore kind pzs
>             evalSplits _
>               | theSplit <= 1            = 1
>               | otherwise                = log (m3 * theSplit)
>             evalSampleSize               = sum (map durScoring pzs) / fromIntegral (length pzs)
>
>             m1, m2, m3 :: Double
>             m1                           = 2/3
>             m2                           = 1/3
>             m3                           = 3 * if isStereoInst pzs then 1/2 else 1
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
>         akResult                         = fromMaybe 0 (Map.lookup kind (getFuzzMap ffm))
>
>     instrumentPercList :: PerGMKey → IntSet → [PerGMKey]
>     instrumentPercList pergmI bixen      = map cv (IntSet.toList bixen)
>                                              where cv bix = pergmI {pgkwBag = Just $ fromIntegral bix}

Utilities =============================================================================================================

> isPossible, stands, isConfirmed
>                        :: Double
> isPossible                               = 50
> stands                                   = 100
> isConfirmed                              = 200
>
> isPossible', stands', isConfirmed'
>                        :: Double → Bool
> isPossible' fuzz                         = fuzz > isPossible
> stands' fuzz                             = fuzz > stands
> isConfirmed' fuzz                        = fuzz > isConfirmed
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
>      is                                  = map enfloor ts
>                                              where enfloor t = floor ((t - lo) * fact)
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
>       OrchestralHarp            → Just            ["harp", "harp", "orch", "concert"]
>       Timpani                   → Just            ["timpani", "timp"]
>       StringEnsemble1           → Just            ["ensemble", "string", "1"]
>       StringEnsemble2           → Just            ["ensemble", "string", "2"]
>       SynthStrings1             → Just            ["synth", "string", "1"]
>       SynthStrings2             → Just            ["synth", "string", "2"]
>       ChoirAahs                 → Just            ["choir", "aahs", "chorus"]
>       VoiceOohs                 → Just            ["voice", "oohs", "chorus"]
>       SynthVoice                → Just            ["voice", "synth"]
>       OrchestraHit              → Just            ["orch", "hit"]
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
>       AcousticBassDrum          → Just            ["elec"]
>       AcousticSnare             → Just            ["elec"]
>       BassDrum1                 → Just            ["orch", "concert"]
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
>
> scoreBool              :: Bool → Rational
> scoreBool x = if x then 1 else (-1)

The End
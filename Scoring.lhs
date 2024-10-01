> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NamedFieldPuns #-}
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
> import Data.List
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe ( mapMaybe )
> import Euterpea.Music
> import Parthenopea
> import qualified Text.FuzzyFind          as FF
  
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
>                                              DScore x        → 0 
>             
> foldHints              :: [SSHint] → Double
> foldHints                                = foldr ((+) . fromRational . scoreHint) 0
>
> ssWeights              :: [Rational]     = [ weighHints
>                                            , weighStereo
>                                            , weigh24Bit
>                                            , weighResolution
>                                            , weighConformance
>                                            , weighFuzziness ]
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
>                                          =
>   [
>       (HintId "editHiDef.sf2"             "Rock Tom"            (Just "TOM_S446.446T.L08")      , "analyze")
>     , (HintId "editHiDef.sf2"             "Tuba"                (Just "Tuba.A-A*B")             , "analyze")
>   ]
>                           
> qqHints                :: Map HintId HintBody
> qqHints                                  = Map.fromList myHints

apply fuzzyfind to mining instruments + percussion ====================================================================

> class GMPlayable a where
>   toKind               :: a → Kind
>   select               :: ([InstrumentName], [PercussionSound]) → [a]
>   getFuzzMap           :: FFMatches → Map a Fuzz
>
> instance GMPlayable InstrumentName where
>   toKind                                 = Left
>   select rost                            =
>     if narrowInstrumentScope
>       then fst rost
>       else fst allKinds
>   getFuzzMap                             = ffInst
>
> instance GMPlayable PercussionSound where
>   toKind                                 = Right
>   select rost                            =
>     if narrowInstrumentScope
>       then snd rost
>       else snd allKinds
>   getFuzzMap                             = ffPerc
>
> type Fuzz = Double
>
> embed                  :: a → Maybe b → Maybe (a, b)
> embed kind                               = fmap (kind,)
>
> genericInstFFKeys      :: [String]
> genericInstFFKeys                        = singleton "horn" 
>
> genericPercFFKeys      :: [String]
> genericPercFFKeys                        = ["percuss", "kit", "kick"]
>
> instrumentConFFKeys    :: InstrumentName → Maybe (InstrumentName, [String])
> instrumentConFFKeys inst                 = embed inst keys
>   where
>    keys = case inst of
>       AcousticBass              → Just            ["drum", "brass", "bassoon", "tremolo", "elec"]
>       AcousticGrandPiano        → Just            ["drum", "upright", "bright", "mellow", "elec"]
>       ElectricGuitarJazz        → Just            ["drum", "bass"]
>       AcousticGuitarNylon       → Just            ["drum", "bass"]
>       AcousticGuitarSteel       → Just            ["drum", "bass"]
>       Agogo                     → Just            ["hi", "low"]
>       BrightAcousticPiano       → Just            ["brightness", "grand"]
>       Cello                     → Just            ["tremolo", "strike", "pluck", "staccato"]
>       Contrabass                → Just $ singleton "tremolo"
>       ElectricBassFingered      → Just            ["acous", "brass", "bassoon"]
>       ElectricBassPicked        → Just            ["acous", "brass", "bassoon"]
>       Flute                     → Just $ singleton "pan"
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
>       Trumpet                   → Just $ singleton "mute"
>       Violin                    → Just            ["tremolo", "strike", "pluck", "staccato"]
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
>       HammondOrgan              → Just            ["organ", "hammond"]
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
>       OverdrivenGuitar          → Just            ["overdrive", "guit"]
>       DistortionGuitar          → Just            ["distort", "guit", "fuzz"]
>       GuitarHarmonics           → Just            ["harmonics", "guit"]
>       AcousticBass              → Just            ["bass", "acous"]
>       ElectricBassFingered      → Just            ["bass", "finger", "elec"]
>       ElectricBassPicked        → Just            ["bass", "pick", "elec"]
>       FretlessBass              → Just            ["fretless", "bass"] 
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
>       Trombone                  → Just $ singleton "trombone"
>       Tuba                      → Just $ singleton "tuba"
>       MutedTrumpet              → Just            ["trumpet", "mute"]
>       FrenchHorn                → Just            ["horn", "french"]
>       BrassSection              → Just            ["brass", "section"]
>       SynthBrass1               → Just            ["brass", "synth", "1"]
>       SynthBrass2               → Just            ["brass", "synth", "2"]
>       SopranoSax                → Just            ["sax" , "soprano"]
>       AltoSax                   → Just            ["sax" , "alto"]
>       TenorSax                  → Just            ["sax" , "tenor"]
>       BaritoneSax               → Just            ["sax" , "baritone"]
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
>       Pad1NewAge                → Just $ singleton "pad1newage"
>       Pad2Warm                  → Just $ singleton "pad2warm"
>       Pad3Polysynth             → Just $ singleton "pad3polysynth"
>       Pad4Choir                 → Just $ singleton "pad4choir"
>       Pad5Bowed                 → Just $ singleton "pad5bowed"
>       Pad6Metallic              → Just $ singleton "pad6metallic"
>       Pad7Halo                  → Just $ singleton "pad7halo"
>       Pad8Sweep                 → Just $ singleton "pad8sweep"
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
> percussionConFFKeys perc = embed perc keys
>   where
>     keys = case perc of
>       AcousticSnare             → Just            ["elec"]
>       AcousticBassDrum          → Just            ["elec"]
>       _                         → Nothing
>
> percussionProFFKeys    :: PercussionSound → Maybe (PercussionSound, [String])
> percussionProFFKeys perc = embed perc keys
>   where
>     keys = case perc of
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
>     lFactor        :: Double             = sqrt $ fromIntegral $ length keys
>     weights        :: [Double]           = [1.9 / lFactor
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
>     tot            :: Double             = evalAgainstKeys inp keys
>
> evalAgainstGeneric     :: String → Fuzz
> evalAgainstGeneric inp                   =
>   evalAgainstKeys inp genericInstFFKeys - evalAgainstKeys inp genericPercFFKeys

use "matching as" cache ===============================================================================================

> data FFMatches =
>   FFMatches {
>     ffInput            :: String
>   , ffInst             :: Map InstrumentName Fuzz
>   , ffPerc             :: Map PercussionSound Fuzz} deriving Show
>
> combineFF              :: ∀ a. (GMPlayable a, Eq a, Ord a) ⇒ Map a Fuzz → Map a Fuzz → Map a Fuzz
> combineFF ffpros ffcons                  =
>   Map.filter (>= 0) (Map.unionWith (\pro con → pro + ((- conRatio) * con)) ffpros ffcons)
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

Utilities =============================================================================================================

> pinnedKR               :: [PercussionSound] → (AbsPitch, AbsPitch) → Bool
> pinnedKR pss (p1, p2)                    = (p2 < p1 + 4) && all available [p1 .. p2]
>   where
>     available          :: AbsPitch → Bool
>     available ap                         = maybe False (`elem` pss) (pitchToPerc ap)
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
>      safemax ts
>        | null ts                         = 0
>        | otherwise                       = maximum ts
>      safemin ts
>        | null ts                         = 0
>        | otherwise                       = minimum ts
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
> type AgainstKindResult                   = Double
> 
> data ArtifactGrade =
>   ArtifactGrade {
>     pScore             :: Int
>   , pEmpiricals        :: [Double]} deriving (Show)
>
> data Grader =
>   Grader {
>     gorWeights         :: [Double]
>   , gorScalar          :: Double} 
> grader                 :: [Rational] → Double → Grader
> grader rs                                = Grader (map fromRational rs)
> gradeEmpiricals        :: Grader → [Double] → ArtifactGrade
> gradeEmpiricals Grader{ .. } emps        = ArtifactGrade score emps
>   where
>     wSize                                = length gorWeights
>     eSize                                = length emps
>     lincombo           :: Double         =
>       profess
>         (wSize == eSize)
>         (unwords ["gradeEmpiricals:", "wSize =", show wSize, "eSize =", show eSize])
>         (sum $ zipWith (*) emps gorWeights)
>     score              :: Int            = round $ gorScalar * lincombo

Flags for customization ===============================================================================================

> data ScoringSettings =
>   ScoringSettings {
>     qqDesireReStereo                     :: Desires
>   , qqDesireRe24Bit                      :: Desires
>   , qqDesireReSplits                     :: Desires
>   , qqDesireReConformance                :: Desires
>   , qqDesireReFuzzy                      :: Desires
>
>   , qqWeighHints                         :: Rational
>   , qqWeighStereo                        :: Rational
>   , qqWeigh24Bit                         :: Rational
>   , qqWeighResolution                    :: Rational
>   , qqWeighConformance                   :: Rational
>   , qqWeighFuzziness                     :: Rational
>
>   , qqNarrowInstrumentScope              :: Bool
>   , qqConRatio                           :: Double
>
>   , qqFFThresholdPossible                :: Double
>   , qqFFThresholdStands                  :: Double
>   , qqFFThresholdConfirmed               :: Double
>
>   , qqEnableSampleAnalysis :: Bool} deriving (Eq, Show)
>
> weighHints                               = qqWeighHints                 defT
> weighStereo                              = qqWeighStereo                defT
> weigh24Bit                               = qqWeigh24Bit                 defT
> weighResolution                          = qqWeighResolution            defT
> weighConformance                         = qqWeighConformance           defT
> weighFuzziness                           = qqWeighFuzziness             defT
>
> isPossible                               = qqFFThresholdPossible        defT
> stands                                   = qqFFThresholdStands          defT
> isConfirmed                              = qqFFThresholdConfirmed       defT
>
> narrowInstrumentScope                    = qqNarrowInstrumentScope      defT
> conRatio                                 = qqConRatio                   defT
>
> isPossible' fuzz                         = fuzz > qqFFThresholdPossible defT
> stands' fuzz                             = fuzz > qqFFThresholdStands defT
> isConfirmed' fuzz                        = fuzz > qqFFThresholdConfirmed defT
>
> sampleAnalyisEnabled                     = qqEnableSampleAnalysis       defT
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
> qqDesires              :: [Desires]      = [qqDesireReStereo      defT
>                                           , qqDesireRe24Bit       defT
>                                           , qqDesireReSplits      defT
>                                           , qqDesireReConformance defT
>                                           , qqDesireReFuzzy       defT]
> qqDesires'             :: [Double]       = map (fromRational . scoreDesire) qqDesires
>

Edit the following ====================================================================================================

> defT                   :: ScoringSettings
> defT =
>   ScoringSettings {
>     qqDesireReStereo                     = DPreferOn
>   , qqDesireRe24Bit                      = DPreferOn
>   , qqDesireReSplits                     = DPreferOn
>   , qqDesireReConformance                = DPreferOn
>   , qqDesireReFuzzy                      = DPreferOn
>
>   , qqWeighHints                         = 10
>   , qqWeighStereo                        = 2
>   , qqWeigh24Bit                         = 0
>   , qqWeighResolution                    = 3/2
>   , qqWeighConformance                   = 3
>   , qqWeighFuzziness                     = 3
>
>   , qqNarrowInstrumentScope              = True
>   , qqConRatio                           = 3/4
>
>   , qqFFThresholdPossible                = 50
>   , qqFFThresholdStands                  = 150
>   , qqFFThresholdConfirmed               = 250
>
>   , qqEnableSampleAnalysis               = False} 

The End
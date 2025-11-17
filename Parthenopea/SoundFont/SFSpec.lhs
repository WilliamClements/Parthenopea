> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

SFSpec
William Clements
April 16, 2023

> module Parthenopea.SoundFont.SFSpec where
>
> import qualified Codec.SoundFont         as F
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Char
> import Data.Foldable
> import Data.Int ( Int8, Int16 )
> import Data.IntMap.Strict (IntMap)
> import qualified Data.IntMap as IntMap
> import Data.IntSet (IntSet)
> import Data.List
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ratio ( (%) )
> import Data.Time
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.MIDI.GeneralMidi ( )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.SoundFont.Utility
  
implementing SoundFont spec ===========================================================================================

> type SampleIndex                         = Word
> type BagIndex                            = Word
>
> data PreSampleKey                        =
>   PreSampleKey {
>     pskwFile           :: Int
>   , pskwSampleIndex    :: Word} deriving (Eq, Ord, Show)
> type PreSample                           = ChangeName F.Shdr
>
> data PreZone                             =
>   PreZone {
>     pzWordF            :: Int
>   , pzWordS            :: Word
>   , pzWordI            :: Word
>   , pzWordB            :: Word
>   , pzDigest           :: ZoneDigest
>   , pzSFZone           :: SFZone
>   , pzChanges          :: ChangeEar F.Shdr
>   , pzRecon            :: Maybe Recon}
>   deriving Eq
> instance Show PreZone where
>   show pz                                =
>     unwords ["PreZone", show (pz.pzWordF, pz.pzWordS, pz.pzWordI, pz.pzWordB), show pz.pzDigest]
> showBad                :: PreZone → String
> showBad pz                               = show (pz.pzWordB, (pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange))
>
> makePreZone            :: Int → Word → Word → Word → [F.Generator] → F.Shdr → PreZone
> makePreZone wF wS wI wB gens shdr        =
>   PreZone
>     wF wS wI wB 
>      (formDigest gens) defZone (ChangeEar shdr [])
>      Nothing
>
> extractSampleKey       :: PreZone → PreSampleKey
> extractSampleKey pz                      = PreSampleKey pz.pzWordF pz.pzWordS
> extractInstKey         :: PreZone → PerGMKey
> extractInstKey pz                        = PerGMKey pz.pzWordF pz.pzWordI Nothing
> extractZoneKey         :: PreZone → PreZoneKey
> extractZoneKey pz                        =
>   PreZoneKey pz.pzWordF pz.pzWordI pz.pzWordB pz.pzWordS
> extractSpace           :: PreZone → (Word, [Maybe (Word, Word)])
> extractSpace pz@PreZone{pzWordB, pzDigest}  
>                                          = (pzWordB, [pzDigest.zdKeyRange, pzDigest.zdVelRange, Just chans])
>   where
>     chans
>       | isLeftPZ pz                      = (0, 0)
>       | isRightPZ pz                     = (1, 1)
>       | otherwise                        = (0, 1)
>
> effPSShdr              :: PreSample → F.Shdr
> effPSShdr ps                             = ps.cnSource{F.sampleName = ps.cnName}
> isLeftPS               :: PreSample → Bool
> isLeftPS ps                              = Just SampleTypeLeft == toMaybeSampleType (effPSShdr ps).sampleType
> isRightPS              :: PreSample → Bool
> isRightPS ps                             = Just SampleTypeRight == toMaybeSampleType (effPSShdr ps).sampleType
>
> wordS, wordI, wordB    :: PreZone → Int
> wordS pz                                 = fromIntegral pz.pzWordS
> wordI pz                                 = fromIntegral pz.pzWordI
> wordB pz                                 = fromIntegral pz.pzWordB
> effPZShdr              :: PreZone → F.Shdr
> effPZShdr PreZone{pzChanges}             =
>   if MakeMono `elem` pzChanges.ceChanges
>     then pzChanges.ceSource{F.sampleType = fromSampleType SampleTypeMono, F.sampleLink = 0}
>     else pzChanges.ceSource
> makeMono               :: PreZone → PreZone
> makeMono pz@PreZone{pzChanges}           = pz{pzChanges = pzChanges{ceChanges = MakeMono : pzChanges.ceChanges}}
> wasSwitchedToMono      :: PreZone → Bool
> wasSwitchedToMono PreZone{pzChanges}     = MakeMono `elem` pzChanges.ceChanges
> showPreZones           :: [PreZone] → String
> showPreZones pzs                         = show $ map pzWordB pzs
>
> data PreZoneKey                          =
>   PreZoneKey {
>     pzkwFile           :: Int
>   , pzkwInst           :: Word
>   , pzkwBag            :: Word
>   , pzkwSampleIndex    :: Word}
>   deriving (Eq, Ord, Show)
>
> data AppliedLimits                       =
>   AppliedLimits {
>     rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word}
>   deriving (Eq, Show)
> defApplied             :: AppliedLimits
> defApplied                               = AppliedLimits 0 0 0 0
>
> data Recon                               =
>   Recon {
>     rSampleMode        :: A.SampleMode
>   , rSampleRate        :: Double
>   , rApplied           :: AppliedLimits
>   , rRootKey           :: AbsPitch
>   , rTuning            :: Int
>   , rDynamics          :: VB.Vector Double
>   , rAttenuation       :: Double
>   , rVolEnv            :: Maybe FEnvelope
>   , rPitchCorrection   :: Maybe Double
>   , rM8n               :: Modulation
>   , rEffects           :: Effects}
>   deriving (Eq, Show)
>
> data Effects                             =
>   Effects {
>     efChorus           :: Double
>   , efReverb           :: Double
>   , efPan              :: Double}
>   deriving (Eq, Show)
>
> data ChangeNameItem                      = FixBadName deriving Eq
>
> data ChangeName a                        =
>   ChangeName {
>     cnSource           :: a
>   , cnChanges          :: [ChangeNameItem]
>   , cnName             :: String}
>
> data ChangeEarItem                       = MakeMono deriving Eq
>
> data ChangeEar a                         =
>   ChangeEar {
>     ceSource           :: a
>   , ceChanges          :: [ChangeEarItem]} deriving Eq
>
> data PerInstrument                       =
>   PerInstrument {
>     piChanges          :: ChangeName F.Inst
>   , pZoneBags          :: IntSet
>   , pSmashing          :: Smashing Word}
> instance Show PerInstrument where
>   show perI                              = unwords ["PerInstrument", show perI.pZoneBags]
>
> data SFZone =
>   SFZone {
>     zInstIndex         :: Maybe Word
>   , zKey               :: Maybe Word
>   , zVel               :: Maybe Word
>   , zInitAtten         :: Maybe Int
>   , zCoarseTune        :: Maybe Int
>   , zFineTune          :: Maybe Int
>   , zSampleIndex       :: Maybe Word
>   , zSampleMode        :: Maybe A.SampleMode
>   , zScaleTuning       :: Maybe Int
>   , zExclusiveClass    :: Maybe Word
>
>   , zDelayVolEnv       :: Maybe Int
>   , zAttackVolEnv      :: Maybe Int
>   , zHoldVolEnv        :: Maybe Int
>   , zDecayVolEnv       :: Maybe Int
>   , zSustainVolEnv     :: Maybe Int 
>   , zReleaseVolEnv     :: Maybe Int
>
>   , zChorus            :: Maybe Int
>   , zReverb            :: Maybe Int
>   , zPan               :: Maybe Int
>
>   , zRootKey           :: Maybe Word
>
>   , zModLfoToPitch     :: Maybe Int
>   , zVibLfoToPitch     :: Maybe Int
>   , zModEnvToPitch     :: Maybe Int
>   , zInitFc            :: Maybe Int
>   , zInitQ             :: Maybe Int
>   , zModLfoToFc        :: Maybe Int
>   , zModEnvToFc        :: Maybe Int
>   , zModLfoToVol       :: Maybe Int
>   , zDelayModLfo       :: Maybe Int
>   , zFreqModLfo        :: Maybe Int
>   , zDelayVibLfo       :: Maybe Int
>   , zFreqVibLfo        :: Maybe Int
>   , zDelayModEnv       :: Maybe Int
>   , zAttackModEnv      :: Maybe Int
>   , zHoldModEnv        :: Maybe Int
>   , zDecayModEnv       :: Maybe Int
>   , zSustainModEnv     :: Maybe Int
>   , zReleaseModEnv     :: Maybe Int
>   , zKeyToModEnvHold   :: Maybe Int
>   , zKeyToModEnvDecay  :: Maybe Int
>   , zKeyToVolEnvHold   :: Maybe Int
>   , zKeyToVolEnvDecay  :: Maybe Int
>
>   , zModulators        :: [Modulator]} deriving (Eq, Show)
>
> defZone                :: SFZone
> defZone                                  = SFZone 
>                                            Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing
>
>                                            Nothing Nothing Nothing
>     
>                                            Nothing
> 
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing
>
>                                            []
>
> data PerGMKey                            =
>   PerGMKey {
>     pgkwFile           :: Int
>   , pgkwInst           :: Word
>   , pgkwBag            :: Maybe Word}
>   deriving (Eq, Ord, Show)
>
> data SFFileBoot                          =
>   SFFileBoot {
>     zWordFBoot         :: Int
>   , zFilename          :: FilePath
>   , zFileArrays        :: FileArrays
>   , zPreZones          :: IntMap PreZone
>   , zSquirrelSample    :: SampleArrays}
>
> accessPreZone          :: String → IntMap PreZone → Int → PreZone
> accessPreZone tag pzs bag             =
>   case bag `IntMap.lookup` pzs of
>     Nothing            → error $ unwords ["accessPreZone", tag, "bad bag", show bag]
>     Just pz            → pz
> accessPreZones         :: String → IntMap PreZone → IntSet → IntMap PreZone
> accessPreZones tag pzs                   = IntMap.fromSet (accessPreZone tag pzs)
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
>
> data Survey                              =
>   Survey {
>     sPreZones          :: IntMap PreZone
>   , sPerInstruments    :: Map PerGMKey PerInstrument
>   , sMatches           :: Matches
>   , sDispositions      :: ResultDispositions}
> defSurvey              :: Survey
> defSurvey                                =
>   Survey
>     IntMap.empty
>     Map.empty
>     (Matches Map.empty Map.empty)
>     virginrd
>     
> data FileArrays                          = 
>   FileArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssIMods            :: Array Word F.Mod
>   , ssShdrs            :: Array Word F.Shdr}
>
> data SFFileRuntime                       =
>   SFFileRuntime {
>     zWordFRuntime      :: Int
>   , zPerInstrument     :: IntMap PerInstrument
>   , zPreZone           :: IntMap PreZone
>   , zSample            :: SampleArrays}
> instance Show SFFileRuntime where
>   show sffile                            =
>     unwords [  "SFFileRuntime"
>              , show sffile.zPerInstrument]
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
> goodScore              :: PerGMScored → Bool
> goodScore score                          = score.pAgainstKindResult > 0 && score.pArtifactGrade.pScore > 0
>
> class GMPlayable a where
>   toGMKind             :: a → GMKind
>   select               :: ([InstrumentName], [PercussionSound]) → Bool → [a]
>   specialCase          :: a → Bool
>   getFuzzMap           :: FFMatches → Map a Fuzz
>
> instance GMPlayable InstrumentName where
>   toGMKind                               = Left
>   select rost narrowInstrumentScope      =
>     if narrowInstrumentScope
>       then fst rost
>       else fst allKinds
>   specialCase kind                       = Percussion == kind
>   getFuzzMap                             = ffInst
>
> instance GMPlayable PercussionSound where
>   toGMKind                               = Right
>   select rost narrowInstrumentScope      =
>     if narrowInstrumentScope
>       then snd rost
>       else snd allKinds
>   specialCase _                          = False
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
>
> weighHints             :: Rational
> weighStereo            :: Rational
> weigh24Bit             :: Rational
> weighResolution        :: Rational
> weighConformance       :: Rational
> weighFuzziness         :: Rational
>
> ssWeights              :: [Double]
> ssWeights                                = [ fromRational weighHints
>                                            , fromRational weighStereo
>                                            , fromRational weigh24Bit
>                                            , fromRational weighResolution
>                                            , fromRational weighConformance
>                                            , fromRational weighFuzziness ]
> showWeights            :: Int → [Emission]
> showWeights spacing                      = concatMap (\weight → [emitShowL weight spacing]) ssWeights
>
> weighHints                               = 10
> weighStereo                              = 2
> weigh24Bit                               = 0
> weighResolution                          = 3/2
> weighConformance                         = 3
> weighFuzziness                           = 3
>
> type Fuzz = Double
>
> data FFMatches =
>   FFMatches {
>     ffInput            :: String
>   , ffInst             :: Map InstrumentName Fuzz
>   , ffPerc             :: Map PercussionSound Fuzz} deriving Show
>
> data SampleArrays                        = 
>   SampleArrays {
>     ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
>
> data ZoneDigest                          =
>   ZoneDigest {
>     zdKeyRange         :: Maybe (Word, Word)
>   , zdVelRange         :: Maybe (Word, Word)
>   , zdPan              :: Maybe Int
>   , zdSampleIndex      :: Maybe Word
>   , zdSampleMode       :: Maybe A.SampleMode
>   , zdStart            :: Int
>   , zdEnd              :: Int
>   , zdStartLoop        :: Int
>   , zdEndLoop          :: Int} deriving (Eq, Show)
> defDigest              :: ZoneDigest
> defDigest                                = ZoneDigest Nothing Nothing Nothing Nothing Nothing 0 0 0 0
> formDigest             :: [F.Generator] → ZoneDigest
> formDigest                               = foldr inspectGen defDigest
>   where
>     inspectGen         :: F.Generator → ZoneDigest → ZoneDigest 
>     inspectGen (F.KeyRange i j)                          zd
>                                          = zd {zdKeyRange = Just(i, j)}
>     inspectGen (F.VelRange i j)                          zd
>                                          = zd {zdVelRange = Just(i, j)}
>     inspectGen (F.Pan i)                                 zd
>                                          = zd {zdPan = Just i}
>     inspectGen (F.SampleIndex w)                         zd
>                                          = zd {zdSampleIndex = Just w}
>     inspectGen (F.SampleMode m)                          zd
>                                          = zd {zdSampleMode = Just m}
>
>     inspectGen (F.StartAddressCoarseOffset i)            zd
>                                          = zd {zdStart = zd.zdStart + 32_768 * i}
>     inspectGen (F.StartAddressOffset i)                  zd
>                                          = zd {zdStart = zd.zdStart + i}
>     inspectGen (F.EndAddressCoarseOffset i)              zd
>                                          = zd {zdEnd = zd.zdEnd + 32_768 * i}
>     inspectGen (F.EndAddressOffset i)                    zd
>                                          = zd {zdEnd = zd.zdEnd + i}
>
>     inspectGen (F.LoopStartAddressCoarseOffset i)        zd
>                                          = zd {zdStartLoop = zd.zdStartLoop + 32_768 * i}
>     inspectGen (F.LoopStartAddressOffset i)              zd
>                                          = zd {zdStartLoop = zd.zdStartLoop + i}
>     inspectGen (F.LoopEndAddressCoarseOffset i)          zd
>                                          = zd {zdEndLoop = zd.zdEndLoop + 32_768 * i}
>     inspectGen (F.LoopEndAddressOffset i)                zd
>                                          = zd {zdEndLoop = zd.zdEndLoop + i}
>
>     inspectGen _ zd                      = zd
>
> okGMRanges             :: ZoneDigest → Bool
> okGMRanges zd                            = rOk && iOk
>   where
>     infinite                             = (0, qMidiSize128 - 1)
>
>     kLim                                 = fromMaybe infinite zd.zdKeyRange
>     vLim                                 = fromMaybe infinite zd.zdVelRange
>
>     rOk                                  = okRange kLim && okRange vLim
>     okRange (j, k)                       = (0 <= j) && j <= k && k < qMidiSize128
>
>     iOk = kLim /= infinite || vLim /= infinite

bootstrapping =========================================================================================================

> data Disposition                         =
>   Accepted | Modified | Violated | Rescued | Dropped | NoChange
>   deriving (Eq, Ord, Show)
>
> data Impact                              =
>   Ok | NoZones | BadName
>      | BadSampleRate | BadSampleType | BadSampleLimits
>      | BadAppliedLimits | BadStereoPartner | RomBased | BadGMRange 
>      | Orphaned | ToCache
>      | Absorbing | Absorbed | NoAbsorption    
>      | Unrecognized | Narrow | NoPercZones
>      | Captured | Adopted | AdoptedAsMono | GlobalZone
>   deriving (Eq, Ord, Show)
>
> data Scan                                =
>   Scan {
>     sDisposition       :: Disposition
>   , sImpact            :: Impact
>   , sFunction          :: String
>   , sClue              :: String} deriving (Eq, Show)
> getTriple              :: Scan → (Disposition, Impact, String)
> getTriple s                              = (s.sDisposition, s.sImpact, s.sFunction)
>
> noClue                 :: String
> noClue                                   = ""
>
> calcElideSet           :: Rational → [Disposition]
> deadset, rescueset     :: [Disposition]    -- a given list is filtered down to the three Dispositions
>                                            -- then we count "membership" per distinct Impact
>                                            -- ...dead if any counts are odd
> deadset                                  = [Violated, Dropped, Rescued]
>                                            -- the following only optionally appear in scan report
> calcElideSet dive                        = if dive < (1/2)
>                                              then [Accepted, Modified, NoChange]
>                                              else []
> rescueset                                = [Rescued]
>
> surveyDispositions     :: [Disposition] → [Scan] → Map Impact Int
> surveyDispositions dns                   = foldr survFold Map.empty
>   where
>     survFold           ::  Scan → Map Impact Int → Map Impact Int
>     survFold s m
>       | s.sDisposition `elem` dns        = Map.insertWith (+) s.sImpact 1 m
>       | otherwise                        = m
>
> dead                   :: [Scan] → Bool
> dead ss                                  =
>   surveyDispositions [Violated, Dropped] ss /= surveyDispositions [Rescued] ss
>
> hasImpact              :: Impact → [Scan] → Bool
> hasImpact impact                         = any (\s → s.sImpact == impact)
>
> wasRescued             :: Impact → [Scan] → Bool
> wasRescued impact                        = any (\s → s.sDisposition `elem` rescueset && s.sImpact == impact)
>
> badButMaybeFix         :: ∀ a. (Show a) ⇒ Bool → Impact → String → a → a → [Scan]
> badButMaybeFix doFix imp fName bad good  = if doFix
>                                              then [viol, resc]
>                                              else [viol]
>   where
>     viol                                 = Scan Violated imp fName (show bad)
>     resc                                 = Scan Rescued imp fName (show good)
>
> data ResultDispositions                  =
>   ResultDispositions {
>     preSampleDispos    :: Map PreSampleKey     [Scan]
>   , preInstDispos      :: Map PerGMKey         [Scan]
>   , preZoneDispos      :: Map PreZoneKey       [Scan]}
> instance Show ResultDispositions where
>   show rd                                =
>     unwords [  "ResultDispositions"
>              , show (length rd.preSampleDispos, length rd.preInstDispos, length rd.preZoneDispos, rdCountScans rd)]
>
> deadrd                 :: ∀ k . SFResource k ⇒ k → ResultDispositions → Bool
> deadrd k rd                              = dead (inspect k rd)
> virginrd               :: ResultDispositions
> virginrd                                 = ResultDispositions Map.empty Map.empty Map.empty
> emptyrd                :: ResultDispositions → Bool
> emptyrd rd                               = null rd.preSampleDispos && null rd.preInstDispos && null rd.preZoneDispos
> rdLengths              :: ResultDispositions → (Int, Int)
> rdLengths rd                             = (length rd.preSampleDispos, length rd.preInstDispos)
> countScans             :: ∀ k . SFResource k ⇒ Map k [Scan] → Int
> countScans                               = Map.foldl' (\n ss → n + length ss) 0
> rdCountScans           :: ResultDispositions → (Int, Int)
> rdCountScans rd                          = (countScans rd.preSampleDispos, countScans rd.preInstDispos)
> combinerd              :: ResultDispositions → ResultDispositions → ResultDispositions
> combinerd rd1 rd2                        =
>   rd1{  preSampleDispos                  = Map.unionWith (++) rd1.preSampleDispos rd2.preSampleDispos
>       , preInstDispos                    = Map.unionWith (++) rd1.preInstDispos   rd2.preInstDispos
>       , preZoneDispos                    = Map.unionWith (++) rd1.preZoneDispos   rd2.preZoneDispos}
>
> class SFResource a where
>   sfkey                :: Int → Word → a
>   wfile                :: a → Int
>   wblob                :: a → Word
>   kname                :: a → SFFileBoot → [Emission]
>   inspect              :: a → ResultDispositions → [Scan]
>   dispose              :: a → [Scan] → ResultDispositions → ResultDispositions
>
> instance SFResource PreSampleKey where
>   sfkey                                  = PreSampleKey
>   wfile k                                = k.pskwFile
>   wblob k                                = k.pskwSampleIndex
>   kname k sffile                         = [Unblocked (show (ssShdrs sffile.zFileArrays ! wblob k).sampleName)]
>   inspect presk rd                       = fromMaybe [] (Map.lookup presk rd.preSampleDispos)
>   dispose presk ss rd                    =
>     rd{preSampleDispos = Map.insertWith (flip (++)) presk ss rd.preSampleDispos}
>
> instance SFResource PerGMKey where
>   sfkey wF wI                            = PerGMKey wF wI Nothing
>   wfile k                                = k.pgkwFile
>   wblob k                                = k.pgkwInst
>   kname k sffile                         = [Unblocked (show (ssInsts sffile.zFileArrays ! wblob k).instName)]
>   inspect pergm rd                       = fromMaybe [] (Map.lookup pergm rd.preInstDispos)
>   dispose pergm ss rd                    =
>     rd{preInstDispos = Map.insertWith (flip (++)) pergm ss rd.preInstDispos}
>
> instance SFResource PreZoneKey where
>   sfkey _ _                              = error "sfkey not supported for PreZoneKey"
>   wfile k                                = k.pzkwFile
>   wblob k                                = k.pzkwInst
>   kname k sffile                         =    kname (PerGMKey k.pzkwFile k.pzkwInst Nothing) sffile
>                                            ++ [comma]
>                                            ++ kname (PreSampleKey k.pzkwFile k.pzkwSampleIndex) sffile
>   inspect prezk rd                       = fromMaybe [] (Map.lookup prezk rd.preZoneDispos)
>   dispose prezk ss rd                    =
>     rd{preZoneDispos = Map.insertWith (flip (++)) prezk ss rd.preZoneDispos}

Note that harsher consequences of unacceptable sample header are enforced earlier. Logically, that would be
sufficient to protect below code from bad data and document the situation. But ... mechanism such as putting
out diagnostics might cause us to execute this code first. So, being crash-free/minimal in isStereoZone et al.

> isStereoInst, is24BitInst
>                        :: [PreZone] → Bool
>
> isStereoInst zs                          = isJust $ find isStereoPZ zs
>       
>
> isStereoPZ, isLeftPZ, isRightPZ
>                        :: PreZone → Bool
> isStereoPZ pz                            = isLeftPZ pz || isRightPZ pz
> isLeftPZ pz                              = SampleTypeLeft == toSampleType (effPZShdr pz).sampleType
> isRightPZ pz                             = SampleTypeRight == toSampleType (effPZShdr pz).sampleType
>
> is24BitInst _                            = True -- was isJust $ ssM24 arrays       
>
> writeReportBySections  :: Directives → FilePath → [[Emission]] → IO ()
> writeReportBySections dives fp eSections   = do
>   tsStarted                              ← getZonedTime
>   let prolog                             = [Unblocked (show tsStarted), EndOfLine, EndOfLine]
>   let epilog                             = [EndOfLine, Unblocked $ show dives]
>   writeFileBySections fp ([prolog] ++ eSections ++ [epilog, theEnd])
>
>   putStr $ reapEmissions [Unblocked $ unwords ["wrote", fp], EndOfLine]
>
> emitMsgs               :: InstrumentName → [(InstrumentName, [String])] → [Emission]
> emitMsgs kind msgs                       = concatMap (\s → [Unblocked s, EndOfLine]) imsgs
>   where
>     imsgs              :: [String]       = fromMaybe [] (lookup kind msgs)
>
> howClose               :: ∀ j . (Eq j) ⇒ [j] → [j] → Rational
> howClose js0 js1
>   | null js0 || null js1                 = 0
>   | otherwise                            = genericLength commonPrefix % genericLength js1
>   where
>     commonPrefix                         = takeWhile (uncurry (==)) (zip js0 js1)
>
> goodChar               :: Char → Bool
> goodChar cN                              = isAscii cN && not (isControl cN)
>
> goodName               :: String → Bool
> goodName name                            = not (null name) && all goodChar name
>
> fixName                :: String → String
> fixName name
>   | null name                            = "<noname>"
>   | otherwise                            = map (\cN → if goodChar cN then cN else '_') name
>
> data SampleType =
>   SampleTypeMono
>   | SampleTypeRight
>   | SampleTypeLeft
>   | SampleTypeLinked
>   | SampleTypeOggVorbis
>   | SampleTypeRomMono
>   | SampleTypeRomRight
>   | SampleTypeRomLeft
>   | SampleTypeRomLinked deriving (Eq, Show)
>
> toSampleType           :: Word → SampleType
> toSampleType hex                         = deJust "toMaybeSampleType" (toMaybeSampleType hex)
>
> toMaybeSampleType      :: Word → Maybe SampleType
> toMaybeSampleType n                      =
>   case n of
>     0x0                    → Just SampleTypeMono
>     0x1                    → Just SampleTypeMono
>     0x2                    → Just SampleTypeRight
>     0x4                    → Just SampleTypeLeft
>     0x8                    → Just SampleTypeLinked
>     0x10                   → Just SampleTypeOggVorbis
>     0x8001                 → Just SampleTypeRomMono
>     0x8002                 → Just SampleTypeRomRight
>     0x8004                 → Just SampleTypeRomLeft
>     0x8008                 → Just SampleTypeRomLinked
>     _                      → Nothing
>
> fromSampleType             :: SampleType → Word
> fromSampleType stype =
>   case stype of
>     SampleTypeMono         → 0x1
>     SampleTypeRight        → 0x2
>     SampleTypeLeft         → 0x4
>     SampleTypeLinked       → 0x8
>     SampleTypeOggVorbis    → 0x10
>     SampleTypeRomMono      → 0x8001
>     SampleTypeRomRight     → 0x8002
>     SampleTypeRomLeft      → 0x8004
>     SampleTypeRomLinked    → 0x8008
>
> allKinds               :: ([InstrumentName], [PercussionSound])
> allKinds                                 =
>   (  map toEnum [fromEnum AcousticGrandPiano .. fromEnum Gunshot]
>    , map toEnum [fromEnum AcousticBassDrum .. fromEnum OpenTriangle])

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportRangesName       :: FilePath
> reportRangesName                         = "Ranges.report"
> reportScanName         :: FilePath
> reportScanName                           = "Scan.report"
> reportTournamentName   :: FilePath
> reportTournamentName                     = "Tournament.report"
>
> data ReportVerbosity                     =
>   ReportVerbosity {
>     dForRanges         :: Rational
>   , dForScan           :: Rational
>   , dForTournament     :: Rational} deriving (Eq, Show)
> okReportVerbosity      :: ReportVerbosity → Bool
> okReportVerbosity rv                     = 
>      inCanonicalRange rv.dForRanges
>   && inCanonicalRange rv.dForScan
>   && inCanonicalRange rv.dForTournament
>   where
>     inCanonicalRange diveItem            = diveItem == clip (0::Rational, 1::Rational) diveItem
> allOn, allOff          :: ReportVerbosity
> allOn                                    =
>   ReportVerbosity 1 1 1
> allOff                                   =
>   ReportVerbosity 0 0 0
>
> data SynthSwitches                       =
>   SynthSwitches {
>     usePitchCorrection :: Bool
>   , useAttenuation     :: Bool
>   , useLoopSwitching   :: Bool
>   , useReverb          :: Bool
>   , useChorus          :: Bool
>   , usePan             :: Bool
>   , useDCBlock         :: Bool
>   , noStereoNoPan      :: Bool
>   , normalizingOutput  :: Bool} deriving (Eq, Show)
> defSynthSwitches       :: SynthSwitches
> defSynthSwitches                         =
>   SynthSwitches
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>
> data Directives                          =
>   Directives {
>     doAbsorption       :: Bool
>   , fixBadNames        :: Bool
>   , narrowRosterForBoot
>                        :: Bool
>   , narrowRosterForRuntime
>                        :: Bool
>   , crossInstrumentPairing
>                        :: Bool
>   , parallelPairing    :: Bool
>   , switchBadStereoZonesToMono
>                        :: Bool
>   , multipleCompetes   :: Bool
>   , proConRatio        :: Rational
>   , absorbThreshold    :: Rational
>   , synthSwitches      :: SynthSwitches
>   , dReportVerbosity   :: ReportVerbosity} deriving (Eq, Show)
>
> defDirectives          :: Directives
> defDirectives                            = baseDives
> -- make experimental changes here
> -- For example:
>       {- {narrowRosterForBoot                 = False} -}
>       {- , dReportVerbosity                   = allOff -}
>   where
>     baseDives                            =
>       Directives
> -- not here
>         True
>         True
>         True
>         True
>         True
>         False
>         False
>         True
>         (3/4)
>         (4/5)
>         defSynthSwitches
>         allOn
>
> okDirectives           :: Directives → Bool
> okDirectives dives                       = okReportVerbosity dives.dReportVerbosity

The End
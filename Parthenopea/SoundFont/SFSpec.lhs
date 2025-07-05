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
> import Data.List
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ratio ( (%) )
> import Euterpea.Music ( InstrumentName )
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Smashing
  
implementing SoundFont spec ===========================================================================================

> data ChangeNameItem                      = FixCorruptName deriving Eq
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
> data PreSampleKey                        =
>   PreSampleKey {
>     pskwFile           :: Word
>   , pskwSampleIndex    :: Word} deriving (Eq, Ord, Show)
>
> type PreSample                           = ChangeName F.Shdr
>
> effPSShdr              :: PreSample → F.Shdr
> effPSShdr ps                             = ps.cnSource{F.sampleName = ps.cnName}
>
> data PreZoneKey                          =
>   PreZoneKey {
>     pzkwFile           :: Word
>   , pzkwInst           :: Word
>   , pzkwBag            :: Word
>   , pzkwSampleIndex    :: Word} deriving (Eq, Ord, Show)
>
> data PreZone                             =
>   PreZone {
>     pzWordF            :: Word
>   , pzWordS            :: Word
>   , pzWordI            :: Word
>   , pzWordB            :: Word
>   , pzDigest           :: ZoneDigest
>   , pzChanges          :: ChangeEar F.Shdr} deriving Eq
> instance Show PreZone where
>   show pz                                =
>     unwords ["PreZone", show (pz.pzWordF, pz.pzWordS, pz.pzWordI, pz.pzWordB), show pz.pzDigest]
> showBad                :: PreZone → String
> showBad pz                               = show (pz.pzWordB, (pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange))
>
> makePreZone            :: Word → Word → Word → Word → [F.Generator] → F.Shdr → PreZone
> makePreZone wF wS wI wB gens shdr        =
>   PreZone wF wS wI wB (formDigest gens) (ChangeEar shdr [])
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
>       | isLeftPreZone pz                 = (0, 0)
>       | isRightPreZone pz                = (1, 1)
>       | otherwise                        = (0, 1)
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
> data PreInstrument                       =
>   PreInstrument {
>     piChanges          :: ChangeName F.Inst
>   , iGlobalKey         :: Maybe PreZoneKey}
> instance Show PreInstrument where
>   show preI                              =
>     unwords ["PreInstrument", show preI.piChanges.cnName]
>
> data PerInstrument                       =
>   PerInstrument {
>     pZones             :: [(PreZone, SFZone)]
>   , pInstCat           :: InstCat
>   , pSmashing          :: Smashing Word}
> showBags               :: PerInstrument → String
> showBags perI                            = show (map (pzWordB . fst) perI.pZones)
>
> data SFZone =
>   SFZone {
>   zInstIndex         :: Maybe Word
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
>     pgkwFile           :: Word
>   , pgkwInst           :: Word
>   , pgkwBag            :: Maybe Word} deriving (Eq, Ord, Show)
>
> data InstCat                             =
>        InstCatInst
>      | InstCatPerc [Word]
>      | InstCatDisq Impact String
> instance Show InstCat where
>   show icat                              =
>     unwords ["InstCat", showMaybeInstCat $ Just icat]
> getMaybePercList       :: Maybe InstCat → Maybe [Word]
> getMaybePercList                         =
>   \case
>     Nothing                              → Nothing
>     Just (InstCatPerc bixen)             → Just bixen
>     _                                    → Just []
> showMaybeInstCat       :: Maybe InstCat → String
> showMaybeInstCat                         =
>   \case
>     Nothing                              → "icNothing"
>     Just InstCatInst                     → "icInst"
>     Just (InstCatPerc _)                 → "icPerc"
>     Just (InstCatDisq imp why)           → unwords ["icDisq", show imp, why]
> data SFBoot                              =
>   SFBoot {
>     zPreSampleCache    :: Map PreSampleKey PreSample
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>   , zPerInstCache      :: Map PerGMKey PerInstrument}
> dasBoot                :: SFBoot
> dasBoot                                  = SFBoot Map.empty Map.empty Map.empty
> instance Show SFBoot where
>   show boot                              =
>     unwords [  "SFBoot"
>              , show (length boot.zPreSampleCache)
>              , show (length boot.zPreInstCache)
>              , show (length boot.zPerInstCache)]
> combineBoot            :: SFBoot → SFBoot → SFBoot
> combineBoot boot1 boot2                  =
>   boot1{  zPreSampleCache                = Map.union boot1.zPreSampleCache   boot2.zPreSampleCache
>         , zPreInstCache                  = Map.union boot1.zPreInstCache     boot2.zPreInstCache
>         , zPerInstCache                  = Map.union boot1.zPerInstCache     boot2.zPerInstCache}
>
> data SFFile                              =
>   SFFile {
>     zWordF             :: Word
>   , zFilename          :: FilePath
>   , zFileArrays        :: FileArrays
>   , zSample            :: SampleArrays}
>
> data FileArrays                          = 
>   FileArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssIMods            :: Array Word F.Mod
>   , ssShdrs            :: Array Word F.Shdr}
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
> okGMRanges             :: ZoneDigest → Bool
> okGMRanges ZoneDigest{zdKeyRange, zdVelRange}
>                                          = okGMRange zdKeyRange && okGMRange zdVelRange
>   where
>     okGMRange          :: (Num a, Ord a) ⇒ Maybe (a, a) → Bool
>     okGMRange mrng                       =
>       case mrng of
>         Just (j, k)                      → (0 <= j) && j <= k && k < 128
>         Nothing                          → True
>
> findBySampleIndex      :: [SFZone] → Word → Maybe SFZone
> findBySampleIndex zs w                   = find (\z → z.zSampleIndex == Just w) zs
>
> findBySampleIndex'     :: [(a, SFZone)] → Word → Maybe (a, SFZone)
> findBySampleIndex' zs w                  = find (\(_, z) → z.zSampleIndex == Just w) zs
>
> findByBagIndex         :: [PreZone] → Word → Maybe PreZone
> findByBagIndex pzs w                     = find (\pz → w == pz.pzWordB) pzs
>
> findByBagIndex'        :: [(PreZone, a)] → Word → Maybe (PreZone, a)
> findByBagIndex' zs w                     = find (\(pz, _) → w == pz.pzWordB) zs

bootstrapping =========================================================================================================

> data Disposition                         =
>   Accepted | Modified | Violated | Rescued | Dropped | NoChange
>   deriving (Eq, Ord, Show)
>
> data Impact                              =
>   Ok | NoZones | CorruptName
>      | BadSampleRate | BadSampleType | BadSampleLimits
>      | DevolveToMono | BadStereoPartner
>      | Paired | Unpaired
>      | OrphanedBySample | OrphanedByInst | ToZoneCache
>      | Absorbing | Absorbed | NoAbsorption
>      | CorruptGMRange | Narrow | BadSampleLooping
>      | RomBased | UndercoveredRanges | OverCoveredRanges
>      | Unrecognized | NoPercZones
>      | CatIsPerc | CatIsInst
>      | Adopted | AdoptedAsMono | GlobalZone
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
> deadset, elideset, rescueset
>                        :: [Disposition]    -- a given list is filtered down to the three Dispositions
>                                            -- then we count "membership" per distinct Impact
>                                            -- ...dead if any counts are odd
> deadset                                  = [Violated, Dropped, Rescued]
>                                            -- the following only optionally appear in scan report
> elideset                                 = if howVerboseScanReport < (1/2)
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
>   sfkey                :: Word → Word → a
>   wfile                :: a → Word
>   wblob                :: a → Word
>   kname                :: a → SFFile → [Emission]
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
>                        :: [(PreZone, a)] → Bool
>
> isStereoInst zs                          = isJust $ find isStereoZone (map fst zs)
>       
> isStereoZone pz                          = isLeftPreZone pz || isRightPreZone pz
>
> isStereoZone, isLeftPreZone, isRightPreZone
>                        :: PreZone → Bool
> isLeftPreZone pz                         = SampleTypeLeft == toSampleType (effPZShdr pz).sampleType
> isRightPreZone pz                        = SampleTypeRight == toSampleType (effPZShdr pz).sampleType
>
> is24BitInst _                            = True -- was isJust $ ssM24 arrays       
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

"A modulator is defined by its sfModSrcOper, its sfModDestOper, and its sfModSrcAmtOper"
--SoundFont spec

struct sfInstModList
{
  SFModulator sfModSrcOper;
  SFGenerator sfModDestOper;
  SHORT modAmount;
  SFModulator sfModAmtSrcOper;
  SFTransform sfModTransOper;
};

> data Modulator                           =
>   Modulator {
>     mrModId            :: Word
>   , mrModSrc           :: ModSrc
>   , mrModDest          :: ModDestType
>   , mrModAmount        :: Double
>   , mrAmountSrc        :: ModSrc} deriving (Eq, Show)
>    
> defModulator           :: Modulator
> defModulator                             = Modulator 0 defModSrc NoDestination 0 defModSrc
>
> data ModKey                              =
>   ModKey {
>     krSrc              :: ModSrc
>   , krDest             :: ModDestType
>   , krAmtSrc           :: ModSrc} deriving (Eq, Ord, Show)
>
> data ModDestType                         =
>     NoDestination
>   | ToPitch
>   | ToFilterFc
>   | ToVolume
>   | ToInitAtten
>   | ToChorus
>   | ToReverb
>   | ToLink Word deriving (Eq, Ord, Show)
>
> data ModSrcSource                        =
>     FromNoController
>   | FromNoteOnVel
>   | FromNoteOnKey
>   | FromLinked deriving (Eq, Ord, Show)
>
> data ModSrc                              =
>   ModSrc {
>     msMapping          :: Mapping
>   , msSource           :: ModSrcSource} deriving (Eq, Ord, Show)
>
> defModSrc              :: ModSrc
> defModSrc                                = ModSrc defMapping FromNoController
>
> data Mapping =
>   Mapping {
>     msContinuity     :: Continuity
>   , msBiPolar        :: Bool  
>   , msMax2Min        :: Bool
>   , msCCBit          :: Bool} deriving (Eq, Ord, Show)
>
> data Continuity =
>     Linear
>   | Concave
>   | Convex
>   | Switch deriving (Eq, Ord, Show, Enum)
>
> defMapping             :: Mapping
> defMapping                               = Mapping Linear False False False
> allMappings            :: [Mapping]
> allMappings                              = [Mapping cont bipolar max2min False
>                                                   | cont                  ← [Linear, Concave, Convex, Switch]
>                                                        , bipolar          ← [False, True]
>                                                              , max2min    ← [False, True]]                                          

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportScanName         :: FilePath
> reportScanName                           = "Scan.report"
> reportTournamentName   :: FilePath
> reportTournamentName                     = "Tournament.report"
>
> howVerboseScanReport   :: Rational
> howVerboseScanReport                     = 2/3
>
> howVerboseTournamentReport
>                        :: Rational
> howVerboseTournamentReport               = 3/4

The End
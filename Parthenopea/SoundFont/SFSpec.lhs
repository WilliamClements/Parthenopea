> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-} 
> {-# LANGUAGE UnicodeSyntax #-}

SFSpec
William Clements
April 16, 2023

> module Parthenopea.SoundFont.SFSpec
>        (  accepted
>         , changePreZone
>         , autopsy
>         , badButMaybeFix
>         , ChangeEar(..)
>         , ChangeEarItem(..)
>         , ChangeName(..)
>         , ChangeNameItem(..)
>         , changePreSample
>         , combineBoot
>         , combinerd
>         , dasBoot
>         , dead
>         , deadrd
>         , defZone
>         , dropped
>         , Disposition(..)
>         , effPSShdr
>         , effPZShdr
>         , elideset
>         , emitMsgs
>         , emptyrd
>         , extractInstKey
>         , extractSampleKey
>         , extractZoneKey
>         , FileArrays(..)
>         , findByBagIndex
>         , findByBagIndex'
>         , findBySampleIndex
>         , findBySampleIndex'
>         , finishScans
>         , fixName
>         , formPreZoneMap
>         , fromSampleType
>         , getMaybePercList
>         , howVerboseScanReport
>         , howVerboseTournamentReport
>         , Impact(..)
>         , InstCat(..)
>         , InstCatData(..)
>         , is24BitInst
>         , isStereoInst
>         , isStereoZone
>         , KeyNumber
>         , lookupCellIndex
>         , makePreZone
>         , modified
>         , noChange
>         , noClue
>         , notDead
>         , PerGMKey(..)
>         , PerInstrument(..)
>         , PreInstrument(..)
>         , PreSample
>         , PreSampleKey(..)
>         , PreZone(..)
>         , PreZoneKey(..)
>         , rdLengths
>         , reportScanName
>         , reportTournamentName
>         , rescued
>         , ResultDispositions(..)
>         , SampleArrays(..)
>         , SampleType(..)
>         , Scan(..)
>         , SFBoot(..)
>         , SFFile(..)
>         , SFResource(..)
>         , SFZone(..)
>         , showBags
>         , showMaybeInstCat
>         , showPreZones
>         , toMaybeSampleType
>         , toSampleType
>         , Velocity
>         , violated
>         , virginrd
>         , wasRescued
>         , wasSwitchedToMono
>         , ZoneDigest(..)
>         )
>         where
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
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Smashing
> import Parthenopea.Repro.Modulation
  
implementing SoundFont spec ===========================================================================================

> data ChangeNameItem = FixCorruptName deriving Eq
>
> data ChangeName a =
>   ChangeName {
>     cnSource           :: a
>   , cnChanges          :: [ChangeNameItem]
>   , cnName             :: String}
>
> data ChangeEarItem = MakeMono deriving Eq
>
> data ChangeEar a =
>   ChangeEar {
>     ceSource           :: a
>   , ceChanges          :: [ChangeEarItem]} deriving Eq
>
> data PreSampleKey =
>   PreSampleKey {
>     pskwFile           :: Word
>   , pskwSampleIndex    :: Word} deriving (Eq, Ord, Show)
>
> type PreSample = ChangeName F.Shdr
> effPSShdr              :: PreSample → F.Shdr
> effPSShdr ps                             =
>   let
>     raw                                  = ps.cnSource
>     name                                 = ps.cnName
>   in
>     raw{F.sampleName = name}
>
> data PreZoneKey =
>   PreZoneKey {
>     pzkwFile           :: Word
>   , pzkwBag            :: Word} deriving (Eq, Ord, Show)
>
> data PreZone =
>   PreZone {
>     pzWordF            :: Word
>   , pzWordS            :: Word
>   , pzWordI            :: Word
>   , pzWordB            :: Word
>   , pzDigest           :: ZoneDigest
>   , pzmkPartners       :: [PreZoneKey]
>   , pzChanges          :: ChangeEar F.Shdr} deriving Eq
> instance Show PreZone where
>   show (PreZone{ .. })                   =
>     unwords ["PreZone", show (pzWordF, pzWordS, pzWordI, pzWordB), show pzDigest, show pzmkPartners]
>
> makePreZone            :: Word → Word → Word → Word → [F.Generator] → F.Shdr → PreZone
> makePreZone wF wS wI wB gens shdr        = PreZone wF wS wI wB (formDigest gens) [] (ChangeEar shdr [])
>
> extractSampleKey       :: PreZone → PreSampleKey
> extractSampleKey pz                      = PreSampleKey pz.pzWordF pz.pzWordS
> extractInstKey         :: PreZone → PerGMKey
> extractInstKey pz                        = PerGMKey pz.pzWordF pz.pzWordI Nothing
> extractZoneKey         :: PreZone → PreZoneKey
> extractZoneKey pz                        = PreZoneKey pz.pzWordF pz.pzWordB
> changePreSample        :: PreSample → PreSample
> changePreSample ps@ChangeName{ .. }      = ps{cnChanges = [FixCorruptName], cnName = fixName cnName}
> effPZShdr              :: PreZone → F.Shdr
> effPZShdr PreZone{ .. }                  =
>   if MakeMono `elem` pzChanges.ceChanges
>     then pzChanges.ceSource{F.sampleType = fromSampleType SampleTypeMono, F.sampleLink = 0}
>     else pzChanges.ceSource
> changePreZone          :: PreZone → PreZone
> changePreZone pz@PreZone{ .. }           = pz{pzChanges = pzChanges{ceChanges = MakeMono : pzChanges.ceChanges}}
> wasSwitchedToMono      :: PreZone → Bool
> wasSwitchedToMono PreZone{ .. }          = MakeMono `elem` pzChanges.ceChanges
> showPreZones           :: [PreZone] → String
> showPreZones pzs                         = show $ map pzWordB pzs
> formPreZoneMap         :: [PreZone] → Map PreZoneKey PreZone
> formPreZoneMap                           = foldl' (\xs y → Map.insert (extractZoneKey y) y xs) Map.empty
>
> data PreInstrument                       =
>   PreInstrument {
>     piChanges          :: ChangeName F.Inst
>   , iGlobalKey         :: Maybe PreZoneKey}
> instance Show PreInstrument where
>   show (PreInstrument{ .. })             =
>     unwords ["PreInstrument", show piChanges.cnName]
>
> data PerInstrument                       =
>   PerInstrument {
>     pZones             :: [(PreZone, SFZone)]
>   , pSmashing          :: Smashing Word}
> showBags               :: PerInstrument → String
> showBags perI                            = show (map (pzWordB . fst) perI.pZones)
>
> data SFZone =
>   SFZone {
>     zStartOffs         :: Maybe Int
>   , zEndOffs           :: Maybe Int
>   , zLoopStartOffs     :: Maybe Int
>   , zLoopEndOffs       :: Maybe Int
>
>   , zStartCoarseOffs   :: Maybe Int
>   , zEndCoarseOffs     :: Maybe Int
>   , zLoopStartCoarseOffs
>                        :: Maybe Int
>   , zLoopEndCoarseOffs :: Maybe Int
>
>   , zInstIndex         :: Maybe Word
>   , zKeyRange          :: Maybe (AbsPitch, AbsPitch)
>   , zVelRange          :: Maybe (Volume, Volume)
>   , zKey               :: Maybe Word
>   , zVel               :: Maybe Word
>   , zInitAtten         :: Maybe Int
>   , zCoarseTune        :: Maybe Int
>   , zFineTune          :: Maybe Int
>   , zSampleIndex       :: Maybe Word
>   , zSampleMode        :: Maybe A.SampleMode
>   , zScaleTuning       :: Maybe Int
>   , zExclusiveClass    :: Maybe Int
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
> defZone                                  = SFZone Nothing Nothing Nothing Nothing
>
>                                            Nothing Nothing Nothing Nothing
>
>                                            Nothing Nothing Nothing Nothing
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
>        InstCatInst InstCatData
>      | InstCatPerc InstCatData
>      | InstCatDisq String [Scan]
> instance Show InstCat where
>   show icat                              =
>     unwords ["InstCat", showMaybeInstCat $ Just icat]
> getMaybePercList       :: Maybe InstCat → Maybe [Word]
> getMaybePercList                         =
>   \case
>     Nothing                              → Nothing
>     Just (InstCatPerc icd)               → Just icd.inPercBixen
>     _                                    → Just []
> showMaybeInstCat       :: Maybe InstCat → String
> showMaybeInstCat                         =
>   \case
>     Nothing                              → "icNothing"
>     Just (InstCatInst _)                 → "icInst"
>     Just (InstCatPerc _)                 → "icPerc"
>     Just (InstCatDisq why _)             → unwords ["icDisq", why]
> data InstCatData                         =
>   InstCatData {
>     inPreZones         :: [PreZone]
>   , inSmashup          :: Smashing Word
>   , inPercBixen        :: [Word]} deriving Show
>
> data SFBoot                              =
>   SFBoot {
>     zPreSampleCache    :: Map PreSampleKey PreSample
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>
>   , zTempPartnerMap    :: Map PreSampleKey PreSampleKey
>
>   , zOwners            :: Map PerGMKey [PreZone]
>   , zJobs              :: Map PerGMKey InstCat
>   , zPerInstCache      :: Map PerGMKey PerInstrument}
> instance Show SFBoot where
>   show (SFBoot{ .. })                  =
>     unwords [  "SFBoot"
>              , show (length zPreSampleCache), "=samples"
>              , show (length zPreInstCache), "=insts"
>
>              , show (length zOwners), "=owners"
>              , show (length zJobs), "=jobs"
>              , show (length zPerInstCache), "=perI"]
> combineBoot            :: SFBoot → SFBoot → SFBoot
> combineBoot boot1 boot2                  =
>   boot1{  zPreSampleCache                = Map.union boot1.zPreSampleCache   boot2.zPreSampleCache
>         , zPreInstCache                  = Map.union boot1.zPreInstCache     boot2.zPreInstCache
>
>         , zTempPartnerMap                = Map.union boot1.zTempPartnerMap   boot2.zTempPartnerMap
>
>         , zOwners                        = Map.union boot1.zOwners           boot2.zOwners
>         , zJobs                          = Map.union boot1.zJobs             boot2.zJobs
>         , zPerInstCache                  = Map.union boot1.zPerInstCache     boot2.zPerInstCache}
>
> dasBoot                :: SFBoot
> dasBoot                                  =
>   SFBoot
>     Map.empty Map.empty
>     Map.empty
>     Map.empty Map.empty Map.empty
>
> data SFFile =
>   SFFile {
>     zWordF             :: Word
>   , zFilename          :: FilePath
>   , zFileArrays        :: FileArrays
>   , zSample            :: SampleArrays}
>
> data FileArrays = 
>   FileArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssIMods            :: Array Word F.Mod
>   , ssShdrs            :: Array Word F.Shdr}
>
> data SampleArrays = 
>   SampleArrays {
>     ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
>
> data ZoneDigest =
>   ZoneDigest {
>     zdKeyRange         :: Maybe (Word, Word)
>   , zdVelRange         :: Maybe (Word, Word)
>   , zdSampleIndex      :: Maybe Word
>   , zdStart            :: Int
>   , zdEnd              :: Int
>   , zdStartLoop        :: Int
>   , zdEndLoop          :: Int} deriving (Eq, Show)
> defDigest              :: ZoneDigest
> defDigest                                = ZoneDigest Nothing Nothing Nothing 0 0 0 0
> formDigest             :: [F.Generator] → ZoneDigest
> formDigest                               = foldr inspectGen defDigest
>   where
>     inspectGen         :: F.Generator → ZoneDigest → ZoneDigest 
>     inspectGen (F.KeyRange i j)                          zd
>                                          = zd {zdKeyRange = normalizeRange i j}
>     inspectGen (F.VelRange i j)                          zd
>                                          = zd {zdVelRange = normalizeRange i j}
>     inspectGen (F.SampleIndex w)                         zd
>                                          = zd {zdSampleIndex = Just w}
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
>     normalizeRange x y                   = if x > y
>                                              then Just (y, x) -- WOX
>                                              else Just (x, y)
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
>   Ok | CorruptName
>      | BadSampleRate | BadSampleType | BadSampleLimits | BadSampleLoopLimits
>      | MissingStereoPartner | BadStereoPartner
>      | Groomed
>      | OrphanedBySample | OrphanedByInst
>      | Absorbing | Absorbed | NoZones
>      | CorruptGMRange | Narrow | BadLinkage | IllegalCrossover
>      | RomBased | UndercoveredRanges | OverCoveredRanges
>      | Unrecognized | NoPercZones | Harvested | CatIsPerc | CatIsInst | Disqualified
>      | Adopted | AdoptedAsMono | GlobalZone
>   deriving (Eq, Ord, Show)
>
> data Scan                                =
>   Scan {
>     sDisposition       :: Disposition
>   , sImpact            :: Impact
>   , sFunction          :: String
>   , sClue              :: String} deriving (Eq, Show)
> noClue                 :: String
> noClue                                   = ""
>
> autopsy                :: [Scan] → (Disposition, Impact, String)
> autopsy ss_                              = maybe notDead getTriple mkiller    
>   where
>     mii                :: Map Impact Int
>     mii                                  = foldl' (\n v → Map.insertWith (+) v 1 n) Map.empty (map sImpact ss)
>
>     ss                                   = filter (\s → s.sDisposition `elem` deadset) ss_
>     mkiller                              = find (odd . snd) (Map.assocs mii)
>
>     getTriple          :: (Impact, Int) → (Disposition, Impact, String)
>     getTriple (impact, _)                = striple $ fromJust $ find (\s → s.sImpact == impact) ss
>
> deadset, elideset, rescueset
>                        :: [Disposition]
>                                            -- a given list is filtered down to the three Dispositions
>                                            -- then we count "membership" per distinct Impact
>                                            -- ...dead if any counts are odd
> deadset                                  = [Violated, Dropped, Rescued]
>                                            -- the following only optionally appear in scan report
> elideset                                 = if howVerboseScanReport < (1/2)
>                                              then [Accepted, NoChange]
>                                              else []
> rescueset                                = [Rescued]
>
> notDead                :: (Disposition, Impact, String)
> notDead                                  = (Accepted, Ok, "")
>
> striple                :: Scan → (Disposition, Impact, String)
> striple s                                = (s.sDisposition, s.sImpact, s.sFunction)
>
> dead                   :: [Scan] → Bool
> dead ss                                  = notDead /= autopsy ss
>
> wasRescued             :: Impact → [Scan] → Bool
> wasRescued impact                        = any (\s → s.sDisposition `elem` rescueset && s.sImpact == impact)
>
> accepted, violated, dropped, rescued, noChange, modified
>                        :: ∀ r . (SFResource r) ⇒ r → Impact → [Scan]
> accepted key impact                      = singleton $ Scan Accepted impact "FromName" (zshow key)
> violated key impact                      = singleton $ Scan Violated impact "FromName" (zshow key)
> dropped key impact                       = singleton $ Scan Dropped impact "FromName" (zshow key)
> rescued key impact                       = singleton $ Scan Rescued impact "FromName" (zshow key)
> noChange key impact                      = singleton $ Scan NoChange impact "FromName" (zshow key)
> modified key impact                      = singleton $ Scan Modified impact "FromName" (zshow key)
>
> badButMaybeFix         :: ∀ a. (Show a) ⇒ Bool → Impact → String → a → a → [Scan]
> badButMaybeFix doFix imp fName bad good  = if doFix
>                                              then [viol, resc]
>                                              else [viol]
>   where
>     viol                                 = Scan Violated imp fName (show bad)
>     resc                                 = Scan Rescued imp fName (show good)
>
> finishScans            :: String → String → [Scan] → [Scan]
> finishScans function clue                = map (\x → x{  sFunction = function, sClue = clue})
>
> data ResultDispositions               =
>   ResultDispositions {
>     preSampleDispos    :: Map PreSampleKey     [Scan]
>   , preInstDispos      :: Map PerGMKey         [Scan]}
> instance Show ResultDispositions where
>   show rd@ResultDispositions{ .. }       =
>     unwords [  "ResultDispositions"
>              , show (length preSampleDispos, length preInstDispos, rdScans rd)]
>
> deadrd                 :: ∀ k . SFResource k ⇒ k → ResultDispositions → Bool
> deadrd k rd                              = dead (inspect k rd)
>
> virginrd               :: ResultDispositions
> virginrd                                 = ResultDispositions Map.empty Map.empty
> emptyrd                :: ResultDispositions → Bool
> emptyrd ResultDispositions{ .. }         = null preSampleDispos && null preInstDispos
> rdLengths              :: ResultDispositions → (Int, Int)
> rdLengths ResultDispositions{ .. }       = (length preSampleDispos, length preInstDispos)
> countScans             :: ∀ k . SFResource k ⇒ Map k [Scan] → Int
> countScans                               = foldl' (\n ss → n + length ss) 0
> rdScans                :: ResultDispositions → (Int, Int)
> rdScans  ResultDispositions{ .. }        = (countScans preSampleDispos, countScans preInstDispos)
> combinerd              :: ResultDispositions → ResultDispositions → ResultDispositions
> combinerd rd1 rd2                        =
>   rd1{  preSampleDispos                  = Map.unionWith (++) rd1.preSampleDispos rd2.preSampleDispos
>       , preInstDispos                    = Map.unionWith (++) rd1.preInstDispos   rd2.preInstDispos}
>
> class SFResource a where
>   sfkey                :: Word → Word → a
>   wfile                :: a → Word
>   wblob                :: a → Word
>   kname                :: a → SFFile → String
>   inspect              :: a → ResultDispositions → [Scan]
>   dispose              :: a → [Scan] → ResultDispositions → ResultDispositions
>
> instance SFResource PreSampleKey where
>   sfkey                                  = PreSampleKey
>   wfile k                                = k.pskwFile
>   wblob k                                = k.pskwSampleIndex
>   kname k sffile                         = (ssShdrs sffile.zFileArrays ! wblob k).sampleName
>   inspect presk rd                       = fromMaybe [] (Map.lookup presk rd.preSampleDispos)
>   dispose presk ss rd                    =
>     rd{preSampleDispos = Map.insertWith (flip (++)) presk ss rd.preSampleDispos}
>
> instance SFResource PerGMKey where
>   sfkey wF wI                            = PerGMKey wF wI Nothing
>   wfile k                                = k.pgkwFile
>   wblob k                                = k.pgkwInst
>   kname k sffile                         = (ssInsts sffile.zFileArrays ! wblob k).instName
>   inspect pergm rd                       = fromMaybe [] (Map.lookup pergm rd.preInstDispos)
>   dispose pergm ss rd                    =
>     rd{preInstDispos = Map.insertWith (++) pergm ss rd.preInstDispos}

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
> goodChar               :: Char → Bool
> goodChar cN                              = isAscii cN && not (isControl cN)
>
> fixName                :: String → String
> fixName                                  = map (\cN → if goodChar cN then cN else '_')
>
> zshow                  :: ∀ a . a → String
> zshow _                                  = "list"

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportScanName         :: FilePath
> reportScanName                           = "ScanReport'.log"
> reportTournamentName   :: FilePath
> reportTournamentName                     = "TournamentReport'.log"
>
> howVerboseScanReport   :: Rational
> howVerboseScanReport                     = 3/4
>
> howVerboseTournamentReport
>                        :: Rational
> howVerboseTournamentReport               = 3/4

The End
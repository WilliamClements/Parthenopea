> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-} 
> {-# LANGUAGE UnicodeSyntax #-}

SoundFont
William Clements
April 16, 2023

> module Parthenopea.SoundFont.SFSpec
>        (  accepted
>         , adhocFuzz
>         , adjustedSampleSizeOk
>         , allKinds
>         , allowStereoCrossovers
>         , appendChange
>         , ArtifactGrade(..)
>         , bracks
>         , cancels
>         , checkSmashing
>         , combineBoot
>         , combinerd
>         , comma
>         , commaOrNot
>         , computeFFMatches
>         , computeInstSmashup
>         , computePreSample
>         , dasBoot
>         , dead
>         , deadrd
>         , defZone
>         , dropped
>         , Disposition(..)
>         , effShdr
>         , emitMsgs
>         , emptyrd
>         , evalAgainstGeneric
>         , extractInstKey
>         , extractZoneKey
>         , FFMatches(..)
>         , FileArrays(..)
>         , finishScans
>         , fixBadNames
>         , formPreZoneMap
>         , fromSampleType
>         , Fuzz
>         , getMaybePercList
>         , gmId
>         , GMKind
>         , GMPlayable(..)
>         , Impact(..)
>         , InstCat(..)
>         , InstCatData(..)
>         , is24BitInst
>         , isConfirmed
>         , isConfirmed'
>         , isPossible
>         , isPossible'
>         , isStereoInst
>         , isStereoZone
>         , KeyNumber
>         , lookupCellIndex
>         , makePreZone
>         , modified
>         , multipleCompetes
>         , noChange
>         , noClue
>         , openSoundFontFile
>         , parens
>         , PerGMKey(..)
>         , PerGMScored(..)
>         , PerInstrument(..)
>         , pinnedKR
>         , pitchToPerc
>         , PreInstrument(..)
>         , PreSample(..)
>         , PreSampleKey(..)
>         , PreZone(..)
>         , PreZoneKey(..)
>         , rdLengths
>         , reportCategorizationName
>         , reportScan
>         , reportTournamentName
>         , rescued
>         , ResultDispositions(..)
>         , SampleArrays(..)
>         , sampleLoopSizeOk
>         , sampleSizeMin
>         , sampleSizeOk
>         , SampleType(..)
>         , Scan(..)
>         , seedWinningRecord
>         , SFBoot(..)
>         , SFFile(..)
>         , SFResource(..)
>         , SFRuntime(..)
>         , SFZone(..)
>         , SFScorable(..)
>         , ShdrXForm(..)
>         , showBags
>         , showMaybeInstCat
>         , showPreZones
>         , smush
>         , stands
>         , stands'
>         , toMaybeSampleType
>         , toSampleType
>         , Velocity
>         , violated
>         , virginrd
>         , WinningRecord(..)
>         , writeFileBySections
>         , writeScanReport
>         , ZoneDigest(..)
>         )
>         where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Char
> import Data.Foldable
> import Data.Int ( Int8, Int16 )
> import Data.List hiding (insert)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import Debug.Trace
> import Euterpea.IO.MIDI.GeneralMidi()
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Smashing
> import Parthenopea.Repro.Modulation
> import qualified Text.FuzzyFind          as FF
  
importing sampled sound (from SoundFont (*.sf2) files) ================================================================

> data PreSampleKey =
>   PreSampleKey {
>     pskwFile           :: Word
>   , pskwSampleIndex    :: Word} deriving (Eq, Ord, Show)
>
> data PreSample =
>   PreSample {
>     sName              :: String
>   , sMatches           :: FFMatches
>   , psShdr             :: F.Shdr
>   , psChanges          :: [ShdrXForm]}
> computePreSample       :: F.Shdr → PreSample
> computePreSample shdr@F.Shdr{ .. }       = PreSample sampleName (computeFFMatches sampleName) shdr []
>
> data PreZoneKey =
>   PreZoneKey {
>     pzkwFile           :: Word
>   , pzkwBag            :: Word} deriving (Eq, Ord, Show)
>
> data ShdrXForm =
>   MakeMono
>   | MakeLeft PreZoneKey
>   | MakeRight PreZoneKey
>   | FixCorruptName deriving Eq
>
> data PreZone =
>   PreZone {
>     pzWordF            :: Word
>   , pzWordS            :: Word
>   , pzWordI            :: Word
>   , pzWordB            :: Word
>   , pzDigest           :: ZoneDigest
>   , pzmkPartners       :: [PreZoneKey]
>   , pzChanges          :: [ShdrXForm]} deriving Eq
> instance Show PreZone where
>   show (PreZone{ .. })                   =
>     unwords ["PreZone", show (pzWordF, pzWordS, pzWordI, pzWordB), show pzDigest, show pzmkPartners]
>
> makePreZone            :: Word → Word → Word → Word → [F.Generator] → PreZone
> makePreZone wF wS wI wB gens             = PreZone wF wS wI wB (formDigest gens) [] []
>
> extractSampleKey       :: PreZone → PreSampleKey
> extractSampleKey pz                      = PreSampleKey pz.pzWordF pz.pzWordS
> extractInstKey         :: PreZone → PerGMKey
> extractInstKey pz                        = PerGMKey pz.pzWordF pz.pzWordI Nothing
> extractZoneKey         :: PreZone → PreZoneKey
> extractZoneKey pz                        = PreZoneKey pz.pzWordF pz.pzWordB
> extractSpace           :: PreZone → (Word, [Maybe (Word, Word)])
> extractSpace pz                          = (pz.pzWordB, [pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange])
> effShdr                :: Map PreSampleKey PreSample → PreZone → F.Shdr
> effShdr psCache pz                       =
>   foldl' (\s x → (\case
>                   MakeMono               → s{F.sampleType = fromSampleType SampleTypeMono, F.sampleLink = 0}
>                   MakeLeft _             → s{F.sampleType = fromSampleType SampleTypeLeft, F.sampleLink = 0}
>                   MakeRight _            → s{F.sampleType = fromSampleType SampleTypeRight, F.sampleLink = 0}
>                   FixCorruptName         → s{F.sampleName = fixName (F.sampleName s)}) x)
>          (psShdr (deJust "rawShdr" (Map.lookup (extractSampleKey pz) psCache)))
>          pz.pzChanges
> appendChange           :: PreZone → ShdrXForm → PreZone
> appendChange pz@PreZone{ .. } change     = pz{pzChanges = pzChanges ++ singleton change}
> showPreZones           :: [PreZone] → String
> showPreZones pzs                         = show $ map pzWordB pzs
> formPreZoneMap         :: [PreZone] → Map PreZoneKey PreZone
> formPreZoneMap                           = foldl' (\xs y → Map.insert (extractZoneKey y) y xs) Map.empty
>
> data PreInstrument                       =
>   PreInstrument {
>     pInst              :: F.Inst
>   , iName              :: String
>   , iMatches           :: FFMatches
>   , iGlobalKey         :: Maybe PreZoneKey}
> instance Show PreInstrument where
>   show (PreInstrument{ .. })                   =
>     unwords ["PreInstrument", show iName]
>
> data PerInstrument                       =
>   PerInstrument {
>     pZones             :: [(PreZone, SFZone)]
>   , pSmashing          :: Smashing Word}
> showBags               :: PerInstrument → String
> showBags perI                            = show (map (pzWordB . fst) perI.pZones)
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
>   , pWinningP          :: Map PercussionSound PerGMScored} deriving Show
> seedWinningRecord      :: WinningRecord
> seedWinningRecord                        = WinningRecord Map.empty Map.empty
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

Instrument categories: instrument, percussion, disqualified

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
> allKinds               :: ([InstrumentName], [PercussionSound])
> allKinds                                 =
>   (  map toEnum [fromEnum AcousticGrandPiano .. fromEnum Gunshot]
>    , map toEnum [fromEnum AcousticBassDrum .. fromEnum OpenTriangle])
>
> type GMKind                              = Either InstrumentName PercussionSound
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
> data SFBoot                              =
>   SFBoot {
>     zPreSampleCache    :: Map PreSampleKey PreSample
>   , zPartnerMap        :: Map PreSampleKey PreSampleKey
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>
>   , zTempWordMap       :: Map Word Word
>   , zTempBackMap       :: Map PreSampleKey [PreZoneKey]
>   , zTempHoldMap       :: Map Word [PreZone]
>
>   , zOwners            :: Map PerGMKey [PreZone]
>   , zJobs              :: Map PerGMKey InstCat
>   , zPerInstCache      :: Map PerGMKey PerInstrument}
> instance Show SFBoot where
>   show (SFBoot{ .. })                  =
>     unwords [  "SFBoot"
>              , show (length zPreSampleCache, length zPartnerMap, length zPreInstCache)
>              , show (length zOwners), "=owners"
>              , show (length zJobs), "=jobs"
>              , show (length zPerInstCache), "=perI"]
> combineBoot            :: SFBoot → SFBoot → SFBoot
> combineBoot boot1 boot2                  =
>   boot1{  zPreSampleCache                = Map.union boot1.zPreSampleCache boot2.zPreSampleCache
>         , zPartnerMap                    = Map.union boot1.zPartnerMap     boot2.zPartnerMap
>         , zPreInstCache                  = Map.union boot1.zPreInstCache   boot2.zPreInstCache
>
>         , zTempWordMap                   = Map.union boot1.zTempWordMap    boot2.zTempWordMap
>         , zTempBackMap                   = Map.union boot1.zTempBackMap    boot2.zTempBackMap
>         , zTempHoldMap                   = Map.union boot1.zTempHoldMap    boot2.zTempHoldMap
>
>         , zOwners                        = Map.union boot1.zOwners         boot2.zOwners
>         , zJobs                          = Map.union boot1.zJobs           boot2.zJobs
>         , zPerInstCache                  = Map.union boot1.zPerInstCache   boot2.zPerInstCache}
>
> dasBoot                :: SFBoot
> dasBoot                                  =
>   SFBoot
>     Map.empty Map.empty Map.empty
>     Map.empty Map.empty Map.empty
>     Map.empty Map.empty Map.empty
> data SFRuntime                           =
>   SFRuntime {
>     zFiles             :: Array Word SFFile
>   , zBoot              :: SFBoot
>   , zWinningRecord     :: WinningRecord}
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
>     inspectGen (F.KeyRange i j) zd       = zd {zdKeyRange = normalizeRange i j}
>     inspectGen (F.VelRange i j) zd       = zd {zdVelRange = normalizeRange i j}
>     inspectGen (F.SampleIndex w) zd      = zd {zdSampleIndex = Just w}
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
>                                              then Just (y, x)
>                                              else Just (x, y)
>

bootstrapping =========================================================================================================

> openSoundFontFile      :: Word → FilePath → IO SFFile
> openSoundFontFile wFile filename = do
>   putStr (unwords [show wFile, filename])
>   ts1                                    ← getCurrentTime
>   result                                 ← F.importFile filename
>   case result of
>     Left s                               →
>       error $ unwords ["openSoundFontFile", "decoding error", s, show filename]
>     Right soundFont                      → do
>       let pdata                          = F.pdta soundFont
>       let sdata                          = F.sdta soundFont
>       let boota                          =
>             FileArrays
>               (F.insts pdata) (F.ibags pdata)
>               (F.igens pdata) (F.imods pdata)
>               (F.shdrs pdata)
>       let samplea                          =
>             SampleArrays
>               (F.smpl  sdata) (F.sm24  sdata)
>       let sffile                         = SFFile wFile filename boota samplea
>       let nBits::Word                      =
>             case samplea.ssM24 of
>               Nothing                    → 16
>               Just _                     → 24
>       ts2                                ← getCurrentTime
>       CM.when diagnosticsEnabled (
>         putStrLn $ unwords [
>                           "openSoundFontFile"
>                       ,   "insts,bags,gens,mods,shdrs"
>                       ,   show $ length boota.ssInsts
>                       ,   show $ length boota.ssIBags
>                       ,   show $ length boota.ssIGens
>                       ,   show $ length boota.ssIMods
>                       ,   show $ length boota.ssShdrs ])
>       putStrLn (unwords ["(", show nBits, ") loaded in", show (diffUTCTime ts2 ts1)])
>       return sffile
>
> data Disposition                         =
>   Accepted | Modified | Violated | Rescued | Dropped | NoChange
>   deriving (Eq, Show)
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
> cancelset, deadset, elideset
>                        :: [Disposition]
>                                            -- cancelled if (not dead and) any of the following appear
> cancelset                                = [Violated, Dropped, Rescued, NoChange, Modified]
>                                            -- a given list is filtered down to the three Dispositions
>                                            -- then we count "membership" per distinct Impact
>                                            -- ...dead if any counts are odd
> deadset                                  = [Violated, Dropped, Rescued]
>                                            -- the following only optionally appear in scan report
> elideset                                 = if howVerboseScan < (1/2)
>                                              then [Accepted, NoChange]
>                                              else []
>
> dead, cancels          :: [Scan] → Bool
> dead ss_                                 = any odd (Map.elems m)
>   where
>     ss                                   = filter (\s → s.sDisposition `elem` deadset) ss_
>
>     m                  :: Map Impact Int
>     m                                    = foldl' (\n v → Map.insertWith (+) v 1 n) Map.empty (map sImpact ss)
>
> cancels                                  = any (\s → s.sDisposition `elem` cancelset)
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
>   inspect              :: a → ResultDispositions → [Scan]
>   dispose              :: a → [Scan] → ResultDispositions → ResultDispositions
>   emit                 :: SFRuntime → a → [Emission]
>
> instance SFResource PreSampleKey where
>   sfkey                                  = PreSampleKey
>   inspect presk rd                       = fromMaybe [] (Map.lookup presk rd.preSampleDispos)
>   dispose presk ss rd                    =
>     rd{preSampleDispos = Map.insertWith (flip (++)) presk ss rd.preSampleDispos}
>   emit runt presk                 =
>     [  Unblocked (show presk)
>      , Blanks 5
>      , Unblocked sffile.zFilename
>      , Blanks 5
>      , Unblocked (show shdr.sampleName)]
>     where
>       sffile                             = runt.zFiles ! presk.pskwFile
>       sfboota                            = sffile.zFileArrays
>       shdr                               = sfboota.ssShdrs ! presk.pskwSampleIndex
>
> instance SFResource PerGMKey where
>   sfkey wF wI                            = PerGMKey wF wI Nothing
>   inspect pergm rd                       = fromMaybe [] (Map.lookup pergm rd.preInstDispos)
>   dispose pergm ss rd                    =
>     rd{preInstDispos = Map.insertWith (++) pergm ss rd.preInstDispos}
>   emit runt pergm                        =
>     [  Unblocked (show pergm)
>      , Blanks 5
>      , Unblocked sffile.zFilename
>      , Blanks 5
>      , Unblocked (show iinst.instName)]
>     where
>       sffile                             = runt.zFiles ! pergm.pgkwFile
>       sfboota                            = sffile.zFileArrays
>       iinst                              = sfboota.ssInsts ! pergm.pgkwInst
>
> sampleSizeOk           :: (Word, Word) → Bool
> sampleSizeOk (stS, enS)                  = stS >= 0 && enS - stS >= 0 && enS - stS < 2 ^ (22::Word)
> sampleLoopSizeOk       :: (Word, Word, A.SampleMode) → Bool
> sampleLoopSizeOk (stL, enL, smode)       = 
>   A.NoLoop == smode || (stL >= 0 && enL - stL >= sampleSizeMin && enL - stL < 2 ^ (22::Word))
>
> adjustedSampleSizeOk   :: ZoneDigest → F.Shdr → Bool
> adjustedSampleSizeOk zd shdr             = 0 <= stA && stA <= enA && 0 <= stL && stL <= enL
>   where
>     stA                                  = shdr.start     + fromIntegral zd.zdStart
>     enA                                  = shdr.end       + fromIntegral zd.zdEnd
>     stL                                  = shdr.startLoop + fromIntegral zd.zdStartLoop
>     enL                                  = shdr.endLoop   + fromIntegral zd.zdEndLoop

Note that harsher consequences of unacceptable sample header are enforced earlier. Logically, that would be
sufficient to protect below code from bad data and document the situation. But ... mechanism such as putting
out diagnostics might cause us to execute this code first. So, being crash-free/minimal in isStereoZone et al.

> isStereoInst, is24BitInst
>                        :: Map PreSampleKey PreSample → [(PreZone, a)] → Bool
>
> isStereoInst preSampleCache zs           = isJust $ find (isStereoZone preSampleCache) (map fst zs)
>       
> isStereoZone preSampleCache pz           = isLeftPreZone preSampleCache pz || isRightPreZone preSampleCache pz
>
> isStereoZone, isLeftPreZone, isRightPreZone
>                        :: Map PreSampleKey PreSample → PreZone → Bool
> isLeftPreZone preSampleCache pz          = SampleTypeLeft == toSampleType (effShdr preSampleCache pz).sampleType
> isRightPreZone preSampleCache pz         = SampleTypeRight == toSampleType (effShdr preSampleCache pz).sampleType
>
> is24BitInst _ _                          = True -- was isJust $ ssM24 arrays       
>
> checkSmashing          :: PerGMKey → Smashing Word → Maybe [Scan]
> checkSmashing pergm smashup
>   | not ok1                              = Just $ violated pergm UndercoveredRanges
>   | not ok2                              = Just $ violated pergm OverCoveredRanges 
>   | otherwise                            = Nothing
>   where
>     ok1                                  = allowOutOfRange || smashup.smashStats.countNothings == 0
>     ok2                                  = allowOverlappingRanges || smashup.smashStats.countMultiples == 0
>
> computeInstSmashup     :: [PreZone] → Smashing Word
> computeInstSmashup pzs
>   | traceNot trace_CIS False             = undefined
>   | otherwise                            = computeSmashup (unwords["computeInstSmashup"]) subs
>   where
>     -- create smashup consisting of 16_384 (128 x 128) Word pairs - adds up to 131_072 bytes
>     subs               :: [(Word, [Maybe (Word, Word)])]
>     subs                                 = map extractSpace pzs
>
>     trace_CIS                            = unwords ["computeInstSmashup", showPreZones pzs, show subs]
>
> smush                  :: [([PreZone], Smashing Word)] → Smashing Word
> smush pears                              = smashSubspaces allTags dims allSpaces
>   where
>     allTags            :: String
>     allSpaces          :: [(Word, [Maybe (Word, Word)])]
>     (allTags, allSpaces)                 =
>       foldl' (\(at, ax) (pzs, smashup) → (at ++ smashup.smashTag, ax ++ map extractSpace pzs)) ([], []) pears
>     dims                                 = [fromIntegral qMidiSize128, fromIntegral qMidiSize128]
>
> emitMsgs               :: InstrumentName → [(InstrumentName, [String])] → [Emission]
> emitMsgs kind msgs                       = concatMap (\s → [Unblocked s, EndOfLine]) imsgs
>   where
>     imsgs              :: [String]       = fromMaybe [] (lookup kind msgs)
>
> isPossible, stands, isConfirmed
>                        :: Double
> isPossible                               = 50
> stands                                   = 150
> isConfirmed                              = 250
>
> writeScanReport        :: SFRuntime → ResultDispositions → IO ()
> writeScanReport runt rd@ResultDispositions{ .. }
>                        = do
>   CM.when diagnosticsEnabled (putStrLn $ unwords [fName, show rd])
>   tsStarted            ← getCurrentTime
>
>   -- output all selections to the report file
>   let esTimeStamp      = [Unblocked (show tsStarted), EndOfLine, EndOfLine]
>   let esSampleScan     = procMap preSampleDispos ++ [EndOfLine]
>   let esInstScan       = procMap preInstDispos ++ [EndOfLine]
>   let esTail           = [EndOfLine, EndOfLine]
>
>   writeFileBySections reportScanName [esTimeStamp, esSampleScan, esInstScan, esTail]
>   tsFinished           ← getCurrentTime
>   putStrLn (unwords ["___report scan results:", show (diffUTCTime tsFinished tsStarted)])
>   traceIO (unwords ["wrote", reportScanName])
>
>   where
>     fName              = "writeScanReport"
>
>     procMap            :: ∀ r . (SFResource r, Show r) ⇒ Map r [Scan] → [Emission]
>     procMap sm         = concat $ Map.mapWithKey procr sm
>
>     procr              :: ∀ r . (SFResource r, Show r) ⇒ r → [Scan] → [Emission]
>     procr k ss_        =
>       let
>         ss             = filter (\s → s.sDisposition `notElem` elideset) ss_
>       in
>         if null ss
>           then []
>           else emit runt k ++ [EndOfLine] ++ concatMap procs ss ++ [EndOfLine]
>
>     procs          :: Scan → [Emission]
>     procs scan
>                        =
>       [  emitShowL scan.sDisposition 24
>        , emitShowL scan.sImpact      32
>        , ToFieldL scan.sFunction     52
>        , Unblocked scan.sClue
>        , EndOfLine]
>

use "matching as" cache ===============================================================================================

> data FFMatches =
>   FFMatches {
>     ffInput            :: String
>   , ffInst             :: Map InstrumentName Fuzz
>   , ffPerc             :: Map PercussionSound Fuzz} deriving Show
>
> combineFF              :: ∀ a. (GMPlayable a, Eq a, Ord a) ⇒ Map a Fuzz → Map a Fuzz → Map a Fuzz
> combineFF ffpros ffcons                  =
>   Map.filter (>= 0) (Map.unionWith (+) ffpros (Map.map (* (- conRatio)) ffcons))
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

> embed                  :: a → Maybe b → Maybe (a, b)
> embed kind                               = fmap (kind,)
>
> genericInstFFKeys      :: [String]
> genericInstFFKeys                        = singleton "horn" 
>
> genericPercFFKeys      :: [String]
> genericPercFFKeys                        = ["perc", "hat", "kit", "kick"]
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
> type Fuzz = Double
>
> writeFileBySections    :: FilePath → [[Emission]] → IO ()
> writeFileBySections fp eSections         = do
>   mapM_ (appendFile fp . reapEmissions) eSections
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
> pinnedKR               :: [PercussionSound] → (AbsPitch, AbsPitch) → Maybe (AbsPitch, AbsPitch)
> pinnedKR pss (p1, p2)                    = if qualifies then Just (p1, p2) else Nothing                   
>   where
>     qualifies                            = (p2 < p1 + 2) && all available [p1 .. p2]
>     available          :: AbsPitch → Bool
>     available ap                         = maybe False (`elem` pss) (pitchToPerc ap)
>
> pitchToPerc            :: AbsPitch → Maybe PercussionSound
> pitchToPerc ap                           =
>   let
>     ad                                   = ap - 35
>   in
>     if ad >= fromEnum AcousticBassDrum && ad <= fromEnum OpenTriangle
>       then Just (toEnum ad)
>       else Nothing
>
> zshow                  :: ∀ a . a → String
> zshow _                                  = "list"

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportCategorizationName   :: FilePath
> reportCategorizationName                 = "CategorizationReport'.log"
> reportScanName         :: FilePath
> reportScanName                           = "ScanReport'.log"
> reportTournamentName   :: FilePath
> reportTournamentName                     = "TournamentReport'.log"
>
> fixBadNames, multipleCompetes
>                        :: Bool
> fixBadNames                              = True
> multipleCompetes                         = True
>
> allowOutOfRange        :: Bool
> allowOutOfRange                          = True
> allowOverlappingRanges :: Bool
> allowOverlappingRanges                   = True
> allowStereoCrossovers  :: Bool
> allowStereoCrossovers                    = False
> howVerboseScan         :: Double
> howVerboseScan                           = 3/4
> narrowInstrumentScope  :: Bool
> narrowInstrumentScope                    = True
> reportScan             :: Bool
> reportScan                               = True
> sampleSizeMin          :: Word
> sampleSizeMin                            = 0
>
> isPossible', stands', isConfirmed'
>                        :: Double → Bool
> isPossible' fuzz                         = fuzz > isPossible
> stands' fuzz                             = fuzz > stands
> isConfirmed' fuzz                        = fuzz > isConfirmed
>
> conRatio               :: Double
> conRatio                                 = 3/4

The End
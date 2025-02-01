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

Boot
William Clements
January 21, 2025

> module Boot
>        (  accepted
>         , accommodate
>         , adhocFuzz
>         , allCellsEqualTo
>         , allKinds
>         , allowStereoCrossovers
>         , BootstrapArrays(..)
>         , bracks
>         , cancels
>         , clip
>         , comma
>         , commaOrNot
>         , deJust
>         , deriveRange
>         , Disposition(..)
>         , effShdr
>         , Emission(..)
>         , emitDefault
>         , emitLine
>         , emitMsgs
>         , emitComment
>         , emitNextComment
>         , emitShowL
>         , emitShowR
>         , emptyrd
>         , epsilon
>         , equipInstruments
>         , extractInstKey
>         , extractZoneKey
>         , FFMatches(..)
>         , fillFieldL
>         , fillFieldR
>         , fractionCovered
>         , fractionEmpty
>         , fromSampleType
>         , Fuzz
>         , gmId
>         , GMKind
>         , GMPlayable(..)
>         , Impact(..)
>         , InstCat(..)
>         , InstCatData(..)
>         , is24BitInst
>         , isConfirmed'
>         , isStereoInst
>         , KeyNumber
>         , listInstruments
>         , lookupCellIndex
>         , parens
>         , PerGMKey(..)
>         , pitchToPerc
>         , PreInstrument(..)
>         , PreSample(..)
>         , PreSampleKey(..)
>         , PreZone(..)
>         , PreZoneKey(..)
>         , profess
>         , professInRange
>         , qMidiSize128
>         , qMidiSizeSpace
>         , reapEmissions
>         , reportScan
>         , rescued
>         , ResultDispositions(..)
>         , SampleArrays(..)
>         , sampleLoopSizeOk
>         , sampleSizeMin
>         , SampleType(..)
>         , Scan(..)
>         , ScanAlts(..)
>         , SFBoot(..)
>         , SFFile(..)
>         , SFResource(..)
>         , SFScorable(..)
>         , shorten
>         , showPreZones
>         , sLength
>         , Smashing(..)
>         , SmashStats(..)
>         , smashSubspaces
>         , smush
>         , stands
>         , stands'
>         , theE
>         , toSampleType
>         , traceAlways
>         , traceIf
>         , traceNever
>         , traceNot
>         , traceNow
>         , tracer
>         , upsilon
>         , Velocity
>         , violated
>         , virginrd
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
> import qualified Data.Bifunctor          as BF
> import Data.Char
> import Data.Either
> import Data.Foldable
> import Data.Int ( Int8, Int16 )
> import Data.List hiding (insert)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ratio ( (%) )
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import qualified Data.Vector.Unboxed     as VU
> import Debug.Trace
> import Euterpea.IO.MIDI.GeneralMidi()
> import Euterpea.Music
> import qualified System.FilePattern.Directory
>                                          as FP
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
> makePreZone wF wS wI wB gens         = PreZone wF wS wI wB (formDigest gens) [] []
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
>      | InstCatDisq [Scan]
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
>     Just (InstCatDisq _)                 → "icDisq"  
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
>     zFiles             :: Array Word SFFile
>   , zPreSampleCache    :: Map PreSampleKey PreSample
>   , zPartnerMap        :: Map PreSampleKey PreSampleKey
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>   , zOwners            :: Map PerGMKey [PreZone]
>   , zJobs              :: Map PerGMKey InstCat}
> instance Show SFBoot where
>   show (SFBoot{ .. })                    = unwords ["SFBoot", show (length zPreInstCache, length zOwners, length zJobs)]
> combineBoot            :: SFBoot → SFBoot → SFBoot
> combineBoot boot1 boot2                  =
>   boot1{  zPreSampleCache                = Map.union boot1.zPreSampleCache boot2.zPreSampleCache
>         , zPartnerMap                    = Map.union boot1.zPartnerMap     boot2.zPartnerMap
>         , zPreInstCache                  = Map.union boot1.zPreInstCache   boot2.zPreInstCache
>         , zOwners                        = Map.union boot1.zOwners         boot2.zOwners
>         , zJobs                          = Map.union boot1.zJobs           boot2.zJobs}
>
> seedBoot               :: Array Word SFFile → SFBoot
> seedBoot vFile                           =
>   SFBoot vFile Map.empty Map.empty Map.empty Map.empty Map.empty
>
> data SFFile =
>   SFFile {
>     zWordF             :: Word
>   , zFilename          :: FilePath
>   , zBoot              :: BootstrapArrays
>   , zSample            :: SampleArrays}
>
> data BootstrapArrays = 
>   BootstrapArrays {
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
>     inspectGen (F.KeyRange i j) zd       = zd {zdKeyRange = Just (i, j)}
>     inspectGen (F.VelRange i j) zd       = zd {zdVelRange = Just (i, j)}
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
> data FileWork =
>   FileWork {
>     fwPreSampleCache   :: Map PreSampleKey PreSample
>   , fwPartnerMap       :: Map PreSampleKey PreSampleKey
>   , fwPreInstCache     :: Map PerGMKey PreInstrument
>   , fwOwners           :: Map PerGMKey [PreZone]
>   , fwJobs             :: Map PerGMKey InstCat
>   , fwZRecs            :: [InstZoneRecord]
>   , fwDispositions     :: ResultDispositions}
> instance Show FileWork where
>   show (FileWork{ .. })                  =
>     unwords [  "FileWork"
>              , show (length fwPreSampleCache, length fwPartnerMap, length fwPreInstCache)
>              , show (length fwOwners), "=owners"
>              , show (length fwJobs), "=jobs"
>              , show (length fwZRecs), "=zrecs"
>              , show fwDispositions]
> defFileWork            :: FileWork
> defFileWork                              =
>   FileWork Map.empty Map.empty Map.empty Map.empty Map.empty [] virginrd
>
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
>
> preSampleTaskIf, partneringTaskIf, preInstTaskIf, surveyTaskIf, captureTaskIf
>                , groomTaskIf, vetTaskIf, harvestTaskIf, catTaskIf {-, zoneTaskIf -}
>                        :: SFFile → ([InstrumentName], [PercussionSound]) → FileWork → FileWork
>
> makeFileIterate        :: SFFile → ([InstrumentName], [PercussionSound]) → FileIterate
> makeFileIterate sffile rost              =
>   FileIterate
>     defFileWork 
>     [  ("preSample",  preSampleTaskIf    sffile rost)
>      , ("partnering", partneringTaskIf   sffile rost)
>      , ("preInst",    preInstTaskIf      sffile rost)
>      , ("survey",     surveyTaskIf       sffile rost)
>      , ("capture",    captureTaskIf      sffile rost)
>      , ("groom",      groomTaskIf        sffile rost)
>      , ("vet",        vetTaskIf          sffile rost)
>      , ("harvest",    harvestTaskIf      sffile rost)
>      , ("cat",        catTaskIf          sffile rost) {-, zoneTaskIf -} ]
>
> reduceFileIterate      :: SFBoot → FileIterate → IO (SFBoot, ResultDispositions)
> reduceFileIterate preBoot fiIn           = do
>   let fwIn                               = fiIn.fiFw
>   let rdIn                               = fwIn.fwDispositions
>   let boot                               =
>         preBoot{
>           zPreSampleCache = fwIn.fwPreSampleCache
>           , zPartnerMap   = fwIn.fwPartnerMap
>           , zPreInstCache = fwIn.fwPreInstCache
>           , zOwners       = fwIn.fwOwners
>           , zJobs         = fwIn.fwJobs}
>   return (boot, rdIn)
>

executive =============================================================================================================

> listInstruments        :: IO ()
> listInstruments                          = do
>   (mboot, pergmsI, pergmsP, rdGen03)     ← equipInstruments allKinds
>   if isJust mboot
>     then do
>       let boot                           = deJust "mboot" mboot
>       writeCategorizationReport boot pergmsI pergmsP
>       CM.when reportScan (writeScanReport boot rdGen03)
>     else do
>       return ()

To support extracting from flawed SoundFont files, we - up front - withdraw unrecoverable items from
their respective collections. The withdrawn items are ignored by all later phases. When constructing those
later items, some critical data may thereby be missing. So that entails deletion-recovery also.

> equipInstruments       :: ([InstrumentName], [PercussionSound])
>                           → IO (Maybe SFBoot, [PerGMKey], [PerGMKey], ResultDispositions)
> equipInstruments rost                    = do
>   tsStarted                              ← getCurrentTime
>
>   putStrLn $ unwords ["rost", show rost]
>
>   -- represent all input SoundFont files in ordered list, thence a vector
>   fps                                    ← FP.getDirectoryFiles "." (singleton "*.sf2")
>   if null fps
>     then do
>       putStrLn "no *.sf2 files found"
>       return (Nothing, [], [], virginrd)
>     else do
>       let nfiles                         = length fps
>       let boundsF::(Word, Word)          = (0, fromIntegral (nfiles - 1))
>       sffilesp                           ← CM.zipWithM openSoundFontFile [0..] fps
>       let preBoot                        = seedBoot (listArray boundsF sffilesp)
>
>       tsLoaded                           ← getCurrentTime
>       putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>       -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>       (boot, rdGen03)                    ← CM.foldM bootFolder (preBoot, virginrd) preBoot.zFiles
>       tsBooted                           ← getCurrentTime
>       putStrLn ("___booted: " ++ show (diffUTCTime tsBooted tsLoaded))
>
>       (pergmsI, pergmsP, ss)             ← sortByCategory boot.zJobs
>       putStrLn $ unwords ["length pergmsI, pergmsP", show $ length pergmsI, show $ length pergmsP, show ss]
>
>       tsFinished                         ← getCurrentTime
>       putStrLn ("___sorted: " ++ show (diffUTCTime tsFinished tsBooted))
>       return (Just boot, pergmsI, pergmsP, rdGen03 )
>   where
>     bootFolder         :: (SFBoot, ResultDispositions) → SFFile → IO (SFBoot, ResultDispositions)
>     bootFolder (preBoot, rdIn) sffile       = do
>       (boot, rdOut)                         ← reduceFileIterate preBoot (ingestFile sffile)
>       CM.when diagnosticsEnabled (putStrLn $ unwords ["bootFolder", show boot])
>       return (combineBoot preBoot boot, combinerd rdIn rdOut)
>
>     ingestFile         :: SFFile → FileIterate
>     ingestFile sffile                    =
>       head $ dropWhile unfinished (iterate' nextGen (makeFileIterate sffile rost))
>       where
>         unfinished fiIn                  = not (null fiIn.fiTaskIfs)
>         nextGen fiIn                     = fiIn{ fiFw = modify fiIn
>                                                , fiTaskIfs = tail fiIn.fiTaskIfs}
>         modify fiIn                      = 
>           let
>             fwIn                         = fiIn.fiFw
>             namedFun                     = head fiIn.fiTaskIfs
>             name                         = fst namedFun
>             taskIf     :: FileWork → FileWork
>             taskIf                       = snd namedFun
>           in
>             taskIf fwIn
>
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
>             BootstrapArrays
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
> sortByCategory         :: Map PerGMKey InstCat → IO ([PerGMKey], [PerGMKey], [[Scan]])
> sortByCategory jobs                      = CM.foldM catFolder ([], [], []) (Map.assocs jobs) -- return $ Map.foldlWithKey catFolder ([], []) jobs
>   where
>     catFolder            :: ([PerGMKey], [PerGMKey], [[Scan]]) → (PerGMKey, InstCat) → IO ([PerGMKey], [PerGMKey], [[Scan]])
>     catFolder (pergmsI, pergmsP, ss) (pergmI_, icat)
>                                          =
>       let
>         pergmI                           = pergmI_{pgkwBag = Nothing}
>       in
>         return $
>         case icat of
>           InstCatPerc icd                → (pergmsI, pergmsP ++ instrumentPercList pergmI icd.inPercBixen, ss)
>           InstCatInst _                  → (pergmI : pergmsI, pergmsP, ss)
>           InstCatDisq scans              → (pergmsI, pergmsP, ss ++ [scans])
>
>     instrumentPercList :: PerGMKey → [Word] → [PerGMKey]
>     instrumentPercList pergmI            = map (\w → pergmI {pgkwBag = Just w})

bootstrapping methods =================================================================================================

> data Disposition                         =
>   Accepted | Violated | Rescued | Dropped | NoChange
>   deriving (Eq, Show)
>
> data Impact                              =
>   Ok | CorruptName
>      | BadSampleRate | BadSampleType | BadSampleLimits | BadSampleLoopLimits
>      | MissingStereoPartner | BadStereoPartner
>      | OrphanedBySample | OrphanedByInst
>      | Absorbed | NoZones
>      | CorruptGMRange | Narrow | BadLinkage | IllegalCrossover
>      | RomBased | UndercoveredRanges | OverCoveredRanges
>      | Unrecognized | NoPercZones
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
> curate                 :: ∀ a . Show a ⇒ a → (a → Bool) → (a → [Scan]) → Maybe [Scan]
> curate thing ok scan                     = if ok thing then Nothing else Just (scan thing)
> curate'                :: ∀ a . a → (a → Bool) → (a → [Scan]) → Maybe [Scan]
> curate' thing ok scan                    = if ok thing then Nothing else Just (scan thing)
>
> data ScanAlts r                          =
>   ScanAlts {
>     saKey              :: r
>   , saFromName         :: String
>   , saCallbacks        :: [Scan]}
>
> data ScanScan                            =
>   ScanScan {
>     s2Scans            :: [Scan]
>   , s2Alts             :: [Maybe [Scan]]}
>
> scanAlts               :: ∀ r . SFResource r ⇒
>                           ScanAlts r
>                           → [Maybe [Scan]]
>                           → ResultDispositions
>                           → ([Scan], ResultDispositions)
> scanAlts sa alts rd                      = (ss, dispose sa.saKey ss rd)
>   where
>     ss_                                  = (scanScans alts).s2Scans
>     ss                                   = if null ss_
>                                              then accepted sa Ok noClue
>                                              else ss_
>
> scanScans              :: [Maybe [Scan]] → ScanScan
> scanScans alts                           = s2Final
>   where
>     s2Final            :: ScanScan
>     s2Final                              =
>       head $ dropWhile unfinished (iterate' nextGen (ScanScan [] alts))
>
>     unfinished ScanScan{ .. }            = not (cancels [Accepted] s2Scans) && not (null s2Alts)
>
>     nextGen s2gen@ScanScan{ .. }         =
>       s2gen{s2Scans = s2Scans ++ fromMaybe [] (head s2Alts), s2Alts = tail s2Alts}
>
> cancels                :: [Disposition] → [Scan] → Bool
> cancels eds ss_                          = any odd (Map.elems m)
>   where
>     ss                                   = filter (\s → s.sDisposition `notElem` eds) ss_
>
>     m                  :: Map Impact Int
>     m                                    = foldl' (\n v → Map.insertWith (+) v 1 n) Map.empty (map sImpact ss)
>
> accepted, violated, dropped, rescued, noChange
>                        :: ∀ r a . (SFResource r) ⇒ ScanAlts r → Impact → a → [Scan]
> accepted sa impact thing                 = singleton $ Scan Accepted impact sa.saFromName (zshow thing)
> violated sa impact thing                 = singleton $ Scan Violated impact sa.saFromName (zshow thing)
> dropped sa impact thing                  = singleton $ Scan Dropped impact sa.saFromName (zshow thing)
> rescued sa impact  thing                 = singleton $ Scan Rescued impact sa.saFromName (zshow thing)
> noChange sa impact thing                 = singleton $ Scan NoChange impact sa.saFromName (zshow thing)
>
> data ResultDispositions                  =
>   ResultDispositions {
>     preSampleDispos    :: Map PreSampleKey     [Scan]
>   , preInstDispos      :: Map PerGMKey         [Scan]}
> instance Show ResultDispositions where
>   show (rd@ResultDispositions{ .. })                  =
>     unwords [  "ResultDispositions"
>              , show (length preSampleDispos, length preInstDispos, rdScans rd)]
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
>   dispose              :: a → [Scan] → ResultDispositions → ResultDispositions
>   fatalrd              :: [Disposition] → ResultDispositions → a → Bool
>   emit                 :: SFBoot → a → [Emission]
>
> instance SFResource PreSampleKey where
>   sfkey                                  = PreSampleKey
>   dispose presk ss rd                    =
>     rd{preSampleDispos = Map.insertWith (flip (++)) presk ss rd.preSampleDispos}
>   fatalrd eds rd presk                   = cancels eds $ fromMaybe [] (Map.lookup presk rd.preSampleDispos)
>   emit boot presk                        =
>     [  Unblocked (show presk)
>      , Blanks 5
>      , Unblocked sffile.zFilename
>      , Blanks 5
>      , Unblocked (show shdr.sampleName)]
>     where
>       sffile                             = boot.zFiles ! presk.pskwFile
>       sfboota                            = sffile.zBoot
>       shdr                               = sfboota.ssShdrs ! presk.pskwSampleIndex
>
> instance SFResource PerGMKey where
>   sfkey wF wI                            = PerGMKey wF wI Nothing
>   dispose pergm ss rd                    =
>     rd{preInstDispos = Map.insertWith (++) pergm ss rd.preInstDispos}
>   fatalrd eds rd pergm                   = cancels eds $ fromMaybe [] (Map.lookup pergm rd.preInstDispos)
>   emit boot pergm                        =
>     [  Unblocked (show pergm)
>      , Blanks 5
>      , Unblocked (boot.zFiles ! pergm.pgkwFile).zFilename
>      , Blanks 5
>      , Unblocked (show iinst.instName)]
>     where
>       sffile                             = boot.zFiles ! pergm.pgkwFile
>       sfboota                            = sffile.zBoot
>       iinst                              = sfboota.ssInsts ! pergm.pgkwInst
>

task interfaces =======================================================================================================

> formComprehension      :: ∀ r a . SFResource r ⇒ SFFile → (BootstrapArrays → Array Word a) → [r]
> formComprehension sffile blobfun
>   | traceNow trace_FC False              = undefined
>   | otherwise                            = map (sfkey sffile.zWordF) bRange
>   where
>     fName                                = "formComprehension"
>     trace_FC                             = unwords [fName, show bRange]
>
>     (stF, enF)         :: (Word, Word)   = bounds $ blobfun sffile.zBoot
>     bRange                               =
>       profess
>         ((stF == 0) && (stF <= enF) && (enF < 2_147_483_648))
>         (error $ unwords [fName, "corrupt blob"])
>         (deriveRange stF enF)
>
> preSampleTaskIf sffile _ fwIn            = foldl' formFolder fwIn (formComprehension sffile ssShdrs)
>   where
>     fName_                               = "preSampleTaskIf"
>
>     formFolder         :: FileWork → PreSampleKey → FileWork
>     formFolder fwForm presk
>       | traceNot trace_FF False          = undefined
>       | otherwise                        = 
>       if cancels [Accepted] scanned
>         then fwForm{  fwDispositions = rd'}
>         else fwForm{  fwPreSampleCache = Map.insert presk (computePreSample shdr) fwForm.fwPreSampleCache
>                     , fwDispositions = rd'}
>       where
>         fName                            = unwords [fName_, "formFolder"]
>         trace_FF                         = unwords [fName, show fwForm]
>
>         shdr@F.Shdr{ .. }                = sffile.zBoot.ssShdrs ! presk.pskwSampleIndex
>         sa                               = ScanAlts presk fName []
>         (scanned, rd')                   = scanAlts sa alts fwForm.fwDispositions
>           where
>             alts                         =
>               [  curate sampleName      goodName                          (violated sa CorruptName)
>                , curate sampleRate      (\x → x == clip (64::Word, (2::Word) ^ (20::Word)) x)
>                                                                           (violated sa BadSampleRate)
>                , curate sampleType      (isJust . toMaybeSampleType)      (violated sa BadSampleType)
>                , curate (start, end)    sampleSizeOk                      (violated sa BadSampleLimits)]
>
> partneringTaskIf sffile _ fwIn           = fwIn{  fwPreSampleCache = preSampleCache'
>                                                 , fwPartnerMap     = partnerMap
>                                                 , fwDispositions   = rd'}
>   where
>     fName_                               = "partneringTaskIf"
>
>     preSampleCache                       = fwIn.fwPreSampleCache
>     rdIn                                 = fwIn.fwDispositions
>
>     (preSampleCache', partnerMap, rd')   =
>       foldl' partneringFolder (Map.empty, Map.empty, rdIn) (Map.assocs preSampleCache)
>     partneringFolder   :: (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
>                           → (PreSampleKey, PreSample)
>                           → (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
>     partneringFolder (target, sPartnerMap, rdPartnering) (k, v)
>       | cancels [Accepted, NoChange] scanned
>                                          = (target,                sPartnerMap,                  rdPartnering')
>       | cancels [Accepted] scanned       = (Map.insert k v target, sPartnerMap,                  rdPartnering') 
>       | otherwise                        = (Map.insert k v target, makePartner (Just otherKey),  rdPartnering')
>       where
>         fName                            = unwords [fName_, "partneringFolder"]
>
>         makePartner                      =
>           \case
>             Nothing                      → sPartnerMap
>             Just preskPartner            → Map.insert k preskPartner sPartnerMap
>
>         otherKey                         = PreSampleKey k.pskwFile shdr.sampleLink
>         other                            = Map.lookup otherKey preSampleCache
>         oBackLink                        = if F.sampleLink oshdr == k.pskwSampleIndex
>                                              then Just otherKey
>                                              else Nothing
>         backLink                         = other >> oBackLink
>
>         shdr                             = sffile.zBoot.ssShdrs ! k.pskwSampleIndex
>         oshdr                            = sffile.zBoot.ssShdrs ! otherKey.pskwSampleIndex
>         stype                            = toSampleType shdr.sampleType
>         stereo                           = SampleTypeLeft == stype || SampleTypeRight == stype
>         sa                               = ScanAlts k fName []
>         (scanned, rdPartnering')         = scanAlts sa alts rdPartnering
>           where
>             alts                         =
>               [  curate stereo           id                  (noChange sa Ok)
>                , curate' other           isJust              (violated sa MissingStereoPartner)
>                , curate backLink         isJust              (violated sa BadStereoPartner)]

> {-
> markGlobalZone         :: Map PerGMKey PreInstrument → InstZoneRecord → Map PerGMKey PreInstrument
> markGlobalZone preic zrec
>   | traceNot trace_MGZ False             = undefined
>   | otherwise                            =
>   if isNothing zrec.zswGBix || isNothing moldpreI
>     then preic
>     else Map.insert pergm oldpreI{iGlobalKey = Just $ PreZoneKey zrec.zswFile (fromJust zrec.zswGBix)} preic
>   where
>     pergm                                = instKey zrec
>     moldpreI                             = Map.lookup pergm preic
>     oldpreI                              = deJust (unwords["mold", show pergm]) moldpreI
>     trace_MGZ                            = unwords ["markGlobalZone", show zrec.zswGBix, show pergm]
> -}
>
> preInstTaskIf sffile _ fwIn              =
>   fwIn{  fwPreInstCache = preInstCache
>        , fwDispositions = rd'}
>   where
>     fName                                = "preInstTaskIf"
>
>     pergms                               = formComprehension sffile ssInsts
>     (preInstCache, rd')                  = foldl' preIFolder (Map.empty, fwIn.fwDispositions) pergms
>
>     preIFolder         :: (Map PerGMKey PreInstrument, ResultDispositions)
>                           → PerGMKey
>                           → (Map PerGMKey PreInstrument, ResultDispositions)
>     preIFolder (m, rdIn) pergm@PerGMKey{ .. }
>                                          =
>       if iinst.instBagNdx < jinst.instBagNdx
>         then (m', rd'')
>         else error $ unwords [fName, "corrupt instBagNdx"]
>       where
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pgkwInst + 1}
>
>         nm                               = iinst.instName
>
>         m'                               =
>           if cancels [Accepted] ss
>              then m 
>              else Map.insert pergm (PreInstrument iinst nm (computeFFMatches nm) Nothing) m
>
>         sa                               = ScanAlts pergm fName []
>         (ss, rd'')                       = scanAlts sa alts rdIn
>           where
>             alts                         =
>               [curate iinst.instName goodName (violated sa CorruptName)]
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm                       = boota.ssInsts ! pergm.pgkwInst
>       where
>         boota                            = sffile.zBoot

PreZone administration ================================================================================================

> goodZRecs              :: [InstZoneRecord] → ResultDispositions → [InstZoneRecord]
> goodZRecs zrecs rdNow                    = filter (not . fatalrd [Accepted, NoChange] rdNow . instKey) zrecs
>
> data InstZoneRecord                      =
>   InstZoneRecord {
>     zswFile            :: Word
>   , zswInst            :: Word
>   , zswGBix            :: Maybe Word
>   , zsPreZones         :: [PreZone]}
> instance Show InstZoneRecord where
>   show (InstZoneRecord{ .. })            = unwords ["InstZoneRecord", show zswFile, show zswInst, show zswGBix]
> makeZRec               :: PerGMKey → InstZoneRecord
> makeZRec pergm                           = InstZoneRecord pergm.pgkwFile pergm.pgkwInst Nothing []
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             = PerGMKey zrec.zswFile zrec.zswInst Nothing       

survey task ===========================================================================================================
          instantiate zrecs

> surveyTaskIf _ _ fwIn                    =
>   fwIn{fwZRecs = map makeZRec (Map.keys fwIn.fwPreInstCache)}

iterating on InstZoneRecord list ======================================================================================

> zrecTask               :: (InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions))
>                           → SFFile → FileWork → FileWork
> zrecTask zrecFun _ fwIn                  = fwIn{  fwZRecs = zrecs
>                                                 , fwDispositions = rd' }
>   where
>     rdIn                                 = (tracer "zrecTask" fwIn).fwDispositions
>     (zrecs, rd')                         = foldl' taskRunner ([], rdIn) fwIn.fwZRecs
>
>     taskRunner (zrecsFold, rdFold) zrec        = 
>       let
>         (zrec', rd'')                     = zrecFun zrec rdFold
>       in
>         if fatalrd [Accepted, NoChange] rdFold (instKey zrec)
>           then (zrec : zrecsFold, rdFold)
>           else (zrec': zrecsFold, rd'')

capture task ==========================================================================================================
          for the first time, populate zrec with PreZones

> captureTaskIf sffile _ fwIn              = zrecTask capturer sffile fwIn 
>   where
>     capturer           :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     capturer zrec rdIn                   =
>       let
>         (newPzs, rd')                    = captureZones pergm rdIn
>         pergm                            = instKey zrec
>       in
>         (zrec{zsPreZones = newPzs}, rd')
>
>     captureZones       :: PerGMKey → ResultDispositions → ([PreZone], ResultDispositions)
>     captureZones pergm rdIn              = (pzsRemaining, rd')
>       where
>         fName_                           = unwords["captureInstZones"]
>
>         sa                               = ScanAlts pergm fName_ []
>         (_, rd')                         = scanAlts sa alts rdIn
>
>         alts                             =
>           [curate pzsRemaining (not . null) (violated sa NoZones)]
>
>         results                          = map captureZone (deriveRange ibagi jbagi)
>
>         ibagi                            = F.instBagNdx (sffile.zBoot.ssInsts ! wIn)
>         jbagi                            = F.instBagNdx (sffile.zBoot.ssInsts ! (wIn+1))
>
>         wIn                              = pgkwInst pergm
>
> {-
>             mGBKey                       = if head results == Right "global zone"
>                                              then Just ibagi
>                                              else Nothing
> -}
>         pzsRemaining                     = lefts results
>             
>         captureZone    :: Word → Either PreZone String
>         captureZone bix                  = zTry
>           where
>             fName                    = unwords [fName_, "captureZone"]
>                 
>             zTry
> {-
>                   | isNothing starget        =
>                     (  zscan, Map.singleton (instKey zscan) (violate OrphanedBySample  fName ""))
> -}
>               | isNothing pz.pzDigest.zdSampleIndex
>                                          = Right "global zone"
>               | isNothing starget        = Right (unwords [fName, "orphaned by sample"])
>               | not limitsCheckedOk      = Right (unwords [fName, "problem", "corrupt adjusted limits"]) 
>               | otherwise                = Left pz{pzChanges = pres.psChanges}
>
>             xgeni                        = F.genNdx $ sffile.zBoot.ssIBags ! bix
>             ygeni                        = F.genNdx $ sffile.zBoot.ssIBags ! (bix + 1)
>
>             gens   :: [F.Generator]
>             gens                         = profess
>                                              (xgeni <= ygeni)
>                                              (unwords [fName, "SoundFont file corrupt (gens)"])
>                                              (map (sffile.zBoot.ssIGens !) (deriveRange xgeni ygeni))
>             pz                           = makePreZone sffile.zWordF si wIn bix gens
>             si                           = deJust "produce si" pz.pzDigest.zdSampleIndex
>             shdr                         = sffile.zBoot.ssShdrs ! si
>
>             limitsCheckedOk              = adjustedSampleSizeOk pz.pzDigest shdr
>             presk                        = PreSampleKey sffile.zWordF si
>             starget                      = Map.lookup presk fwIn.fwPreSampleCache
>             pres                         = deJust "pres" starget

groom task ============================================================================================================

> makeBack               :: FileWork → [InstZoneRecord] → Map PreSampleKey [PreZoneKey]
> makeBack fw zrecs                        = foldl' Map.union Map.empty (map zrec2back zrecs)
>   where
>     zrec2back          :: InstZoneRecord → Map PreSampleKey [PreZoneKey]
>     zrec2back zrec                       =
>       foldl' backFolder Map.empty (filter (isStereoZone fw.fwPreSampleCache) zrec.zsPreZones)
>     backFolder         :: Map PreSampleKey [PreZoneKey] → PreZone → Map PreSampleKey [PreZoneKey]
>     backFolder target pz                 =
>       Map.insertWith (++) (PreSampleKey pz.pzWordF pz.pzWordS) [extractZoneKey pz] target
>
> groomTaskIf sffile _ fwIn                = zrecTask groomer sffile fwIn 
>   where
>     fName_                               = "groomTaskIf"
>
>     back                                 = makeBack fwIn (goodZRecs fwIn.fwZRecs fwIn.fwDispositions)
>
>     groomer            :: InstZoneRecord
>                           → ResultDispositions
>                           → (InstZoneRecord, ResultDispositions)
>     groomer zrec rdGroom_
>       | traceNot trace_G False           = undefined
>       | otherwise                        = (zrec{zsPreZones = newPzs}, rd')
>       where
>         fName                            = unwords [fName_, "groomer"]
>         trace_G                          = unwords [fName, show rdGroom_]
>
>         newPzs                           = groomPreZones zrec.zsPreZones
>
>         pergm                            = instKey zrec
>         sa                               = ScanAlts pergm fName []
>         (_, rd')                         = scanAlts sa alts rdGroom_
>           where           
>             alts                         = [curate newPzs (not . null) (violated sa NoZones)]
>
>     groomPreZones preZones               = pzsStereo ++ pzsMono
>       where
>         (pzsStereo_, pzsMono)            = partition (isStereoZone fwIn.fwPreSampleCache) preZones
>         pzsStereo                        = map partnerUp pzsStereo_
>
>     partnerUp pz                         =
>       let
>         mpartners                        =
>           Map.lookup (PreSampleKey pz.pzWordF (F.sampleLink (effShdr fwIn.fwPreSampleCache pz))) back 
>       in
>         pz{pzmkPartners = fromMaybe [] mpartners}

vet task ============================================================================================================
          remove bad stereo partners from PreZones per instrument, delete instrument if down to zero PreZones

> vetTaskIf sffile _ fwIn                  = zrecTask vetter sffile fwIn 
>   where
>     fName_                               = "vetTaskIf"
>     rdIn                                 = fwIn.fwDispositions
>     filePzs                              =
>       foldl' (\x y → x ++ filter (isStereoZone fwIn.fwPreSampleCache) y.zsPreZones) [] (goodZRecs fwIn.fwZRecs rdIn)
>
>     vetter             :: InstZoneRecord
>                           → ResultDispositions
>                           → (InstZoneRecord, ResultDispositions)
>     vetter zrec rdVet_
>       | traceNot trace_V False           = undefined
>       | otherwise                        = (zrec{zsPreZones = newPzs}, rd')
>       where
>         fName                            = unwords [fName_, "vetter"]
>         trace_V                          = unwords [fName, show rdVet_]
>
>         mapStereo                        = formPreZoneMap filePzs
>         newPzs                           =
>           let
>             (pzsStereo, pzsMono)         = partition (isStereoZone fwIn.fwPreSampleCache) zrec.zsPreZones
>
>             vetPreZone :: PreZone → Maybe PreZone
>             vetPreZone pz                =
>               if null newPartners
>                 then if canDevolveToMono
>                        then Just $ appendChange pz MakeMono
>                        else Nothing
>                 else Just pz{pzmkPartners = newPartners}
>               where
>                 newPartners              = filter (okPartner pz) pz.pzmkPartners
>           in
>             mapMaybe vetPreZone pzsStereo ++ pzsMono
>
>         pergm                            = instKey zrec
>         sa                               = ScanAlts pergm fName []
>         (_, rd')                         = scanAlts sa alts rdVet_
>           where           
>             alts                         = [curate newPzs (not . null) (violated sa NoZones)]
>
>         okPartner      :: PreZone → PreZoneKey → Bool
>         okPartner pz pzk                 =
>           case Map.lookup pzk mapStereo of
>             Nothing                      → False
>             Just pzPartner               → goodPartners pz pzPartner
>
>         goodPartners   :: PreZone → PreZone → Bool
>         goodPartners pzMe pzYou          =
>           let
>             mySPartner                   =
>               PreSampleKey pzMe.pzWordF   (F.sampleLink (effShdr fwIn.fwPreSampleCache pzMe))
>             yrSPartner                   =
>               PreSampleKey pzYou.pzWordF  (F.sampleLink (effShdr fwIn.fwPreSampleCache pzYou))
>           in
>             (Just yrSPartner == Map.lookup mySPartner fwIn.fwPartnerMap)
>             && (Just mySPartner == Map.lookup yrSPartner fwIn.fwPartnerMap)
>

harvest task ==========================================================================================================
          reap owners from zrecs

> harvestTaskIf _ _ fwIn                   = fwIn{fwOwners = foldl' harvestFolder Map.empty zrecs}
>   where
>     zrecs                                = fwIn.fwZRecs
>     rdIn                                 = fwIn.fwDispositions
>
>     harvestFolder      :: Map PerGMKey [PreZone] → InstZoneRecord → Map PerGMKey [PreZone]
>     harvestFolder m zrec                 =
>       let
>         pergm                            = instKey zrec
>         pzs                              = zrec.zsPreZones
>       in
>         if fatalrd [Accepted, NoChange] rdIn pergm
>           then m
>           else Map.insert pergm pzs m
>
> shorten        :: (Char → Bool) → [Char] → [Char] 
> shorten qual chars                       = reverse (dropWhile qual (reverse chars))
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

categorization task ==========================================================================================================
          reap owners from zrecs

> catTaskIf _ rost fwIn                    = 
>   fwIn{fwJobs = categorize fwIn.fwPreSampleCache fwIn.fwPreInstCache fwIn.fwOwners rost}
>
> categorize             :: Map PreSampleKey PreSample
>                           → Map PerGMKey PreInstrument
>                           → Map PerGMKey [PreZone] 
>                           → ([InstrumentName], [PercussionSound])
>                           → Map PerGMKey InstCat
> categorize preSampleCache preInstCache owners rost
>                                          = Map.mapWithKey categorizeInst preInstCache
>   where
>     categorizeInst     :: PerGMKey → a → InstCat
>     categorizeInst pergm _
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = deJust (unwords[fName__, "icat"]) icat
>       where
>         fName__                          = "categorizeInst"
>         trace_CI                         =
>           unwords [fName__, preI.iName, show (pergm.pgkwFile, pergm.pgkwInst)]
>
>         preI                             =
>           deJust (unwords [fName__, "PreInstrument"]) (Map.lookup pergm preInstCache)
>         mpzs                             = Map.lookup pergm owners
>         pzs                              = deJust (unwords[fName__, "owners"]) mpzs
>
>         -- Put the Instrument, of either category, through a gauntlet of checks.
>         -- This diverges, and then we have qualInstZone and qualPercZone 
>
>         pzsLocal                         = filter isLocal pzs
>         isLocal pz                       = not (isStereoZone preSampleCache pz) && not (hasCross pz)
>                                            
>         -- Determine which category will belong to the Instrument, based on its performance for
>         -- 1. all kinds
>         -- 2. "rost" subset, could be same as 1.
>
>         icatAllKinds, icatRost, icatNarrow, icat
>                        :: Maybe InstCat
>         icatAllKinds                     = foldl' CM.mplus Nothing (provideAlts Nothing allKinds)
>         icatRost                         = foldl' CM.mplus Nothing (provideAlts icatAllKinds rost)
>         icatNarrow                       = Just (InstCatDisq (dropped sa Narrow noClue))
>         sa                               = ScanAlts pergm fName__ []
>         icat                             =
>           case (icatAllKinds, icatRost) of
>             (Just (InstCatInst _), Just (InstCatInst _))
>                                          → icatRost
>             (Just (InstCatPerc _), Just (InstCatPerc _))
>                                          → icatRost
>             (Just (InstCatDisq _), _)    → icatAllKinds
>             (_, Just (InstCatDisq _))    → icatRost
>             _                            → icatNarrow
>
>         corrupt                          = foldl' byZone Nothing pzs
>           where
>             byZone     :: Maybe InstCat → PreZone → Maybe InstCat
>             byZone target prez           =
>               foldl' CM.mplus target [checkGMRange prez.pzDigest.zdKeyRange, checkGMRange prez.pzDigest.zdVelRange]
>
>         checkGMRange   :: (Num a, Ord a, Show a) ⇒ Maybe (a, a) → Maybe InstCat
>         checkGMRange mrng                =
>           mrng >>= \(j, k) → if (0 <= j) && j <= k && k < fromIntegral qMidiSize128
>                                then Nothing
>                                else Just $ InstCatDisq (violated sa CorruptGMRange (show mrng))
>
>         hasRom pz                        = F.sampleType (effShdr preSampleCache pz) >= 0x8000
>                                          
>         checkLinkage   :: Maybe InstCat
>         checkLinkage                     =
>           if isSafe sList || requiredZoneLinkage < 1 then Nothing else Just $ InstCatDisq (violated sa BadLinkage noClue)
>           where
>             sList      :: [(Int, Int)]
>             sList                        = map (\z → (extractIndex z, extractLink z)) pzsLocal
>
>         isSafe         :: ∀ a . (Eq a, Ord a, Show a) ⇒ [(a,a)] → Bool
>         isSafe pairs                     = closed && allPaired
>           where
>             uniquer    :: Map a a
>             uniquer                      =
>               foldl' (\target (k, t) → Map.insert k t target) Map.empty pairs
>
>             closed                       = all (\x → isJust (Map.lookup x uniquer)) uniquer
>             allPaired                    = all (paired uniquer) uniquer
>
>         paired         :: ∀ a . (Eq a, Ord a, Show a) ⇒ Map a a → a → Bool
>         paired target x                  = x == z
>           where
>             y                            = target Map.! x
>             z                            = target Map.! y
>
>         extractIndex, extractLink
>                        :: PreZone → Int
>         extractIndex pz                  = fromIntegral $ deJust "extractIndex" pz.pzDigest.zdSampleIndex
>         extractLink pz                   = fromIntegral $ F.sampleLink (effShdr preSampleCache pz)
>
>         rejectCrosses  :: Maybe InstCat
>         rejectCrosses                 =
>           if any hasCross pzs then Just $ InstCatDisq (violated sa IllegalCrossover noClue) else Nothing
>
>         hasCross       :: PreZone → Bool
>         hasCross pz                      =
>           isStereoZone preSampleCache pz && notElem (extractLink pz) (map extractLink pzs)
>
>         howLaden       :: [Word] → Double
>         howLaden ws
>           | null pzs                     = 0
>           | otherwise                    = (fromIntegral . length) ws / (fromIntegral . length) pzs
>
>         maybeSettle    :: (Foldable t, Show (t Fuzz)) ⇒ Fuzz → InstCat → t Fuzz → Maybe InstCat
>         maybeSettle thresh ic keys       = find (> thresh) keys >> Just ic
>
>         genericScore                     = evalAgainstGeneric preI.iName

   "now", sequence through the alternatives, categorizing encountered instruments as follows:
   a. Just InstCatInst           an inst bearing one inst, or
   b. Just InstCatPerc           an inst bearing one or more percs, or
   c. Just InstCatDisq           an inst disqualified from tournaments, or
   d. Nothing                    undecided

>         provideAlts    :: Maybe InstCat → ([InstrumentName], [PercussionSound]) → [Maybe InstCat]
>         provideAlts seed srost
>           | traceNot trace_PA False      = undefined
>           | otherwise                    =
>           if isNothing seed
>             then structuralAlts ++ functionalAlts allKinds
>             else functionalAlts srost
>           where
>             fName_                       = unwords [fName__, "provideAlts"]
>             trace_PA                     =
>               unwords [fName_, showMaybeInstCat seed, show (length pzs), show (BF.bimap length length srost)]
>
>             structuralAlts               =
>               [if isNothing mpzs || null (fromJust mpzs)
>                  then Just $ InstCatDisq (violated sa NoZones noClue)
>                  else Nothing
>               , corrupt
>               , if any hasRom pzs then Just $ InstCatDisq (violated sa RomBased noClue) else Nothing
>               , if allowStereoCrossovers
>                   then Nothing
>                   else rejectCrosses
>               , checkLinkage]
>             functionalAlts frost
>               | traceNot trace_FA False  = undefined
>               | otherwise                =
>               let
>                 ffInst'                  =
>                   Map.filterWithKey (\k v → k `elem` select frost && isPossible' v) preI.iMatches.ffInst
>                 ffPerc'                  =
>                   Map.filterWithKey (\k v → k `elem` select frost && isPossible' v) preI.iMatches.ffPerc
>               in
>                 [ 
>                     maybeSettle isConfirmed catInst                  ffInst'
>                   , maybeSettle isConfirmed (catPerc wZones)         ffPerc'
>                   , maybeNailAsPerc 0.6 
>                   , maybeSettle stands      catInst                  ffInst'
>
>                   , maybeSettle stands      (catPerc wZones)         ffPerc'
>                   , maybeSettle stands      (catDisq (dropped sa Narrow noClue))     preI.iMatches.ffInst
>
>                   , maybeNailAsPerc 0.3
>                   , if genericScore > 0 then Just catInst            else Nothing
>                   , if genericScore < 0 then Just (catPerc wZones)   else Nothing
>                   , Just $ catDisq (dropped sa Unrecognized noClue)
>                 ]
>               where
>                 uZones :: [Word]         = fromMaybe wZones (getMaybePercList seed)
>                 wZones :: [Word]         = mapMaybe (qualPercZone frost) pzs
>
>                 maybeNailAsPerc
>                        :: Double → Maybe InstCat
>                 maybeNailAsPerc frac  =
>                   if frac < howLaden uZones
>                     then
>                       (if 0.05 < howLaden wZones
>                          then Just (catPerc wZones)
>                          else Just (catDisq (violated sa NoPercZones noClue)))
>                     else Nothing
>
>                 trace_FA = unwords [fName_, preI.iName, show frost, show (length uZones, length wZones)]
>
>             catInst      :: InstCat      =
>               if null pzs
>                 then InstCatDisq (violated sa NoZones noClue)
>                 else (case checkSmashing pergm smashup of
>                        Nothing           → InstCatInst icd
>                        Just reason       → InstCatDisq reason)
>               where
>                 smashup                  = computeInstSmashup pzs
>                 icd                      = InstCatData pzs smashup []
>
>             catPerc      :: [Word] → InstCat
>             catPerc ws
>               | traceNot trace_CP False  = undefined
>               | otherwise                =
>               if null pzs || null ws || (null . init) ws
>                 then InstCatDisq (dropped sa Narrow (show ws))
>                 else (case check of
>                        Nothing           → InstCatPerc icd
>                        Just reason       → InstCatDisq reason)
>               where
>                 fName                    = unwords[fName_, "catPerc"]
>                 trace_CP                 = unwords [fName, show (length ws, length pzs)]
>
>                 pzs'                     = filter (\x → x.pzWordB `elem` ws) pzs
>                 smashup                  = computeInstSmashup pzs'
>                 icd                      = InstCatData pzs' smashup ws
>                 check                    = checkSmashing pergm smashup
>
>             catDisq    :: [Scan] → InstCat
>             catDisq                      = InstCatDisq
>
>         qualPercZone   :: ([InstrumentName], [PercussionSound]) → PreZone → Maybe Word
>         qualPercZone qrost prez
>           | traceNot trace_QPZ False     = undefined
>           | otherwise                    = result
>           where
>             mrange                       =
>               notracer
>                 "mrange" (prez.pzDigest.zdKeyRange >>= (Just . BF.bimap fromIntegral fromIntegral))
>             result                       =
>               mrange
>               >>= pinnedKR (select qrost)
>               >> Just prez.pzWordB
>
>             trace_QPZ                    = unwords ["qualPercZone", show prez.pzWordB, show result]
>
> checkSmashing          :: PerGMKey → Smashing Word → Maybe [Scan]
> checkSmashing pergm smashup
>   | not ok1                              = Just $ violated sa UndercoveredRanges noClue
>   | not ok2                              = Just $ violated sa OverCoveredRanges noClue
>   | otherwise                            = Nothing
>   where
>     fName                                = "checkSmashing"
>     sa                                   = ScanAlts pergm fName []
>     ok1                                  = allowOutOfRange || smashup.smashStats.countNothings == 0
>     ok2                                  = allowOverlappingRanges || smashup.smashStats.countMultiples == 0
>
> computeSmashup         :: String → [(Word, [Maybe (Word, Word)])] → Smashing Word
> computeSmashup tag                       = smashSubspaces tag dims
>   where
>     dims                                 = [fromIntegral qMidiSize128, fromIntegral qMidiSize128]
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
> writeScanReport        :: SFBoot → ResultDispositions → IO ()
> writeScanReport boot rd@ResultDispositions{ .. }
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
>         ss             = if howVerboseScan < (1/2)
>                            then filter (\s → Accepted /= s.sDisposition) ss_
>                            else ss_
>       in
>         if null ss
>           then []
>           else emit boot k ++ [EndOfLine] ++ concatMap procs ss ++ [EndOfLine]
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
> writeCategorizationReport
>                        :: SFBoot → [PerGMKey] → [PerGMKey] → IO ()
> writeCategorizationReport boot pergmsI pergmsP
>                        = do
>   tsStarted            ← getCurrentTime
>
>   -- output all selections to the report file
>   let esFiles          = emitFileListC ++ [EndOfLine]
>   let esI              = concatMap (dumpInstrument boot) pergmsI
>   let esP              = concatMap (dumpPercussion boot) pergmsP
>   let esTail           = singleton $ Unblocked "\n\nThe End\n\n"
>   let eol              = singleton EndOfLine
>
>   writeFileBySections reportCategorizationName [esFiles, esI, eol, esFiles, esP, esTail]
>   tsFinished           ← getCurrentTime
>   putStrLn (unwords ["___report categorization results:", show (diffUTCTime tsFinished tsStarted)])
>   traceIO (unwords ["wrote", reportCategorizationName])
>
>   where
>     emitFileListC      = concatMap (uncurry doF) (zip ([0..]::[Word]) (toList boot.zFiles))
>     doF nth sffile     = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>     dumpInstrument, dumpPercussion
>                        :: SFBoot → PerGMKey → [Emission]
>     dumpInstrument _ pergm
>                        = [Unblocked (show pergm), EndOfLine] 
>     dumpPercussion _ pergm
>                        = [Unblocked (show pergm), EndOfLine] 

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

Range theory ==========================================================================================================

Model rectilinear sub-space coverage; e.g. find unwanted (sub-)space overlaps. Each space (of nspaces) contains
exactly ndims (2 in the MIDI case) ranges. If dim is the value of a dimension then its overall range is implicitly
0..dim-1 -- the associated _specified_ space range carves out a subset thereof.

Say you have ndims=2 dimensions each of 64 extent. (Partially) covering overall 64x64 space are nspaces=3 "zones". 

Zone 1: 32..57 "pitch", 11..47 "velocity"
Zone 2: 21..40        , 20..21
Zone 3: 0..1          , 0..1

You see there is some overlap between Zone 1 and Zone 2.

> smashSubspaces         :: ∀ i . (Integral i, Ix i, Num i, Show i, VU.Unbox i) ⇒
>                           String → [i] → [(i, [Maybe (i, i)])] → Smashing i
> smashSubspaces tag dims spaces_
>   | traceIf trace_SS False               = undefined
>   | otherwise                            = Smashing tag dims spaces (developSmashStats svector) svector
>   where
>     spaces             :: [(i, [(i, i)])]
>     spaces                               = map (BF.second (zipWith (\dim → fromMaybe (0, dim-1)) dims)) spaces_
>
>     mag                :: Int            = fromIntegral $ product dims
>
>     svector            :: VU.Vector (i, i)
>     svector                              = foldl' sfolder (VU.replicate mag (0, 0)) spaces
>
>     sfolder            :: VU.Vector (i, i) → (i, [(i, i)]) → VU.Vector (i, i)
>     sfolder smashup (spaceId, rngs)      = VU.accum assignCell smashup (enumAssocs dims spaceId rngs)
>
>     assignCell         :: (i, i) → (i, i) → (i, i)
>     assignCell mfrom mto                 = (fst mto, snd mfrom + 1)
>
>     enumAssocs         ::  [i] → i → [(i, i)] → [(Int, (i, i))]
>     enumAssocs dimsA spaceId rngs        =
>       profess
>         (0 <= mag && mag <= 65_536 && all (uncurry validRange) (zip dimsA rngs))
>         (unwords ["enumAssocs: range violation", tag, show mag, show dimsA, show spaces])
>         (map (, (spaceId, 1)) is)
>       where
>         is             :: [Int]
>         is                               =
>           map (fromIntegral . computeCellIndex dimsA) (traverse walkRange rngs)
>
>     trace_SS                             = unwords ["smashSubspaces", show (length spaces_), show spaces_]
>
> validRange             :: ∀ i . (Integral i, Ix i) ⇒ i → (i, i) → Bool
> validRange dim (r, s)                    = 0 <= dim && r <= s && inZRange r dim && inZRange s dim
>
> validCoords            :: ∀ i . (Integral i, Ix i, VU.Unbox i) ⇒ [i] → Smashing i → Bool
> validCoords coords smashup               = and $ zipWith inZRange coords smashup.smashDims
>
> lookupCellIndex        :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒ [i] → Smashing i → (i, i)
> lookupCellIndex coords smashup           = try
>   where
>     try_                                 =
>       profess
>         (validCoords coords smashup)
>         (unwords ["lookupCellIndex", "invalid coords"])
>         (smashup.smashVec VU.! computeCellIndex smashup.smashDims coords)
>     try                                  =
>       if snd try_ > 0
>         then try_
>         else (snd $ minimum (map (measure coords) smashup.smashSpaces), 1)
>
>     measure            :: [i] → (i, [(i, i)]) → (Double, i)
>     measure coordsM space                =
>       minimum (map (distance (fst space) coordsM) (listOutPoints (snd space)))
>
>     distance           :: i → [i] → [i] → (Double, i)
>     distance bix [] []                   = (0, bix)
>     distance bix (x:xs) (y:ys)           = (var + fst (distance bix xs ys), bix)
>       where
>         delta, var     :: Double
>         delta                            = fromIntegral (x - y)
>         var                              = delta * delta
>     distance _ _ _                       =
>       error $ unwords ["distance:", "input coords args have unequal lengths"]
>
> listOutPoints          :: ∀ i . (Integral i) ⇒ [(i, i)] → [[i]]
> listOutPoints []                         = [[]]
> listOutPoints ((r, s) : ranges)          = points1 ++ points2
>   where
>     points1                              = map ([r] ++) (listOutPoints ranges)
>     points2                              = map ([s] ++) (listOutPoints ranges)
>
> computeCellIndex       :: ∀ i . (Integral i) ⇒ [i] → [i] → Int
> computeCellIndex [] []                   = 0
> computeCellIndex (_:dims) (rng:rngs)       = fromIntegral (rng * product dims) + computeCellIndex dims rngs
> computeCellIndex _ _                     =
>   error $ unwords ["computeCellIndex:", "input args dims and coords have unequal lengths"]
>
> allCellsEqualTo        :: ∀ i . (Integral i, Show i, VU.Unbox i) ⇒ Smashing i → Maybe (i, i)
> allCellsEqualTo smashup                  =
>   let
>     cand                                 = smashup.smashVec VU.! 0
>   in
>     if all (\j → cand == (smashup.smashVec VU.! j)) [0..(VU.length smashup.smashVec - 1)]
>       then Just cand
>       else Nothing
>
> data Smashing i                          =
>   Smashing {
>     smashTag            :: String
>     , smashDims         :: [i]
>     , smashSpaces       :: [(i, [(i, i)])]
>     , smashStats        :: SmashStats
>     , smashVec          :: VU.Vector (i, i)}
> instance ∀ i. (Integral i, Num i, Show i) ⇒ Show (Smashing i) where
>   show Smashing{ .. }                    =
>     unwords ["Smashing", show (smashTag, smashStats)]
> sLength                :: ∀ i. (Integral i) ⇒ Smashing i → i
> sLength smashup                        = product smashup.smashDims
> data SmashStats                        =
>   SmashStats {
>     countNothings      :: Int
>   , countSingles       :: Int
>   , countMultiples     :: Int} deriving Show
> seedSmashStats         :: SmashStats
> seedSmashStats                           = SmashStats 0 0 0
>
> developSmashStats      :: ∀ i. (Integral i, Show i, VU.Unbox i) ⇒ VU.Vector (i,i) → SmashStats
> developSmashStats                        = VU.foldl' sfolder seedSmashStats
>   where
>     sfolder            :: SmashStats → (i, i) → SmashStats
>     sfolder stats@SmashStats{ .. } (_, count)
>       | count == 0                       = stats{countNothings = countNothings + 1}
>       | count == 1                       = stats{countSingles = countSingles + 1}
>       | otherwise                        = stats{countMultiples = countMultiples + 1}
> fractionEmpty, fractionCovered
>                        :: ∀ i. (Integral i, Show i) ⇒ Smashing i → Rational
> fractionEmpty smashup                    = fromIntegral (countNothings smashup.smashStats) % fromIntegral (sLength smashup)
> fractionCovered smashup                  =
>   fromIntegral (countSingles smashup.smashStats + countMultiples smashup.smashStats) % fromIntegral (sLength smashup)
>
> inZRange               :: (Ix a, Num a) ⇒ a → a → Bool
> inZRange x y                             = inRange (0, y - 1) x 

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

Emission capability ===================================================================================================

> data Emission                            = 
>   ToFieldL String Int
>   | ToFieldR String Int
>   | Unblocked String
>   | Blanks Int
>   | Empty 
>   | EndOfLine deriving Show
>
> makeString             :: Emission → String
> makeString em                            =
>   case em of
>     ToFieldL str sz    → if len > sz then error $ unwords ["overflowL", show sz, show len, show str]
>                                      else fillFieldL sz str
>       where
>         len                              = length str
>     ToFieldR str sz    → if len > sz then error $ unwords ["overflowR", show sz, show len, show str]
>                                      else fillFieldR sz str
>       where
>         len                              = length str
>     Unblocked str      → str
>     Blanks sz          → replicate sz ' '
>     Empty              → ""
>     EndOfLine          → "\n"
>
> emitLine               :: [Emission] → [Emission]
> emitLine ex                              = singleton literate ++ ex ++ singleton EndOfLine
>
> commaOrNot             :: Int → Emission
> commaOrNot nth                           =
>   if nth == 0
>     then ToFieldL ""  2
>     else ToFieldL "," 2
>
> parens                 :: [Emission] → [Emission]
> parens ex                                = [Unblocked "("] ++ ex ++ [Unblocked ")"]
>
> bracks                 :: [Emission] → [Emission]
> bracks ex                                = [Unblocked "["] ++ ex ++ [Unblocked "]"]
>
> comma, literate        :: Emission
> comma                                    = Unblocked ", "
> literate                                 = ToFieldL ">" 2
>
> emitComment            :: [Emission] → [Emission]
> emitComment ex                           = [EndOfLine] ++ ex ++ [EndOfLine, EndOfLine]
>
> emitNextComment        :: [Emission] → [Emission]
> emitNextComment ex                       = ex ++ [EndOfLine, EndOfLine]
>
> emitShowL              :: (Show a) ⇒ a → Int → Emission
> emitShowL item                           = ToFieldL (show item)
>
> emitShowR              :: (Show a) ⇒ a → Int → Emission
> emitShowR item                           = ToFieldR (show item)
>
> emitDefault            :: (Show a) ⇒ a → Emission
> emitDefault item                         = Unblocked (show item)
>
> gmId                   :: (Show a) ⇒ a → Emission
> gmId i                                   = emitShowL i 22
>
> reapEmissions          :: [Emission] → String
> reapEmissions                            = concatMap makeString
>
> fillFieldL             :: Int → String → String
> fillFieldL fieldSz str                   = str ++ safeReplicate (length str) fieldSz ' '
>
> fillFieldR             :: Int → String → String
> fillFieldR fieldSz str                   = safeReplicate (length str) fieldSz ' ' ++ str
>
> safeReplicate          :: Int → Int → Char → String
> safeReplicate sz maxSz                   = replicate (maxSz - sz)
>
> writeFileBySections    :: FilePath → [[Emission]] → IO ()
> writeFileBySections fp eSections         = do
>   mapM_ (appendFile fp . reapEmissions) eSections
>
> type Velocity                            = Volume
> type KeyNumber                           = AbsPitch
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
> goodName               :: String → Bool
> goodName                                 = all goodChar
>
> fixName                :: String → String
> fixName                                  = map (\cN → if goodChar cN then cN else '_')
>
> profess                :: Bool → String → a → a
> profess assertion msg something          = if not assertion
>                                              then error (unwords ["Failed assertion --", msg])
>                                              else something
>
> professInRange         :: (Eq a, Ord a, Show a) ⇒ (a, a) → a → String → a → a
> professInRange rng val role            = profess
>                                              (val == clip rng val)
>                                              (unwords ["out of", role, "range", show rng, show val])
>
> deJust                 :: ∀ a. String → Maybe a → a
> deJust tag item                          = profess (isJust item) (unwords["expected Just for", tag]) (fromJust item)
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
>
> accommodate            :: Ord n ⇒ (n, n) → n → (n, n)
> accommodate (xmin, xmax) newx            = (min xmin newx, max xmax newx)
>
> clip                   :: Ord n ⇒ (n, n) → n → n
> clip (lower, upper) val                  = min upper (max lower val)
>
> deriveRange            :: Integral n ⇒ n → n → [n]
> walkRange              :: Integral n ⇒ (n, n) → [n]
> deriveRange x y                          = if x >= y || y <= 0 then [] else [x..(y-1)]
> walkRange (x, y)                         = if x > y || y < 0 then [] else [x..y]
>
> theE, epsilon, upsilon :: Double
> theE                                     = 2.718_281_828_459_045_235_360_287_471_352_7
> epsilon                                  = 1e-8               -- a generous little epsilon
> upsilon                                  = 1e10               -- a scrawny  big    upsilon
>
> qMidiSize128           :: Int
> qMidiSize128                             = 128
> qMidiSizeSpace         :: Int
> qMidiSizeSpace                           = qMidiSize128 * qMidiSize128

Tracing ===============================================================================================================

> traceIf, traceNow, traceAlways, traceNever, traceNot
>                        :: String → a → a
> traceIf str expr                         = if diagnosticsEnabled then trace str expr else expr
> traceNow                                 = trace
> traceAlways                              = trace
> traceNever _ expr                        = expr
> traceNot _ expr                          = expr
>
> tracer                 :: Show a ⇒ String → a → a
> tracer str x                             =
>   if True
>     then traceNow (unwords [str, "=", show x]) x
>     else x
>
> notracer               :: Show a ⇒ String → a → a
> notracer _ x                             = x

> reportScanName         :: FilePath
> reportScanName                           = "ScanReport'.log"
> reportCategorizationName   :: FilePath
> reportCategorizationName                 = "CategorizationReport'.log"
>
> allowOutOfRange        :: Bool
> allowOutOfRange                          = True
> allowOverlappingRanges :: Bool
> allowOverlappingRanges                   = True
> allowStereoCrossovers  :: Bool
> allowStereoCrossovers                    = False
> canDevolveToMono       :: Bool
> canDevolveToMono                         = True
> diagnosticsEnabled     :: Bool
> diagnosticsEnabled                       = True
> howVerboseScan         :: Double
> howVerboseScan                           = 3/4
> narrowInstrumentScope  :: Bool
> narrowInstrumentScope                    = True
> reportScan             :: Bool
> reportScan                               = True
> requiredZoneLinkage    :: Double
> requiredZoneLinkage                      = 0
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
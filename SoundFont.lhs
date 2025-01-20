> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeFamilies #-} 
> {-# LANGUAGE UnicodeSyntax #-}

SoundFont
William Clements
April 16, 2023

> module SoundFont where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Control.Monad.IO.Class
> import Control.Monad.Trans.Reader
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Char
> import Data.Either
> import Data.Foldable ( toList, for_ )
> import Data.Int ( Int8, Int16 )
> import Data.IntSet (IntSet)
> import qualified Data.IntSet             as IntSet
> import Data.List hiding (insert)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( fromJust, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, catMaybes )
> import Data.MemoTrie
> import Data.Ord ( Down(Down), comparing )
> import Data.Ratio ( approxRational, (%) )
> import Database.MongoDB.Connection
> import Database.Persist.MongoDB
> import Database.Persist.TH
> import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
> import qualified Data.Vector.Unboxed     as VU
> import qualified Data.Vector             as VB
> import Debug.Trace ( traceIO )
> import Discrete
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, Mono, Stereo, Clock, Signal )
> import Euterpea.Music
> import GHC.Generics (Generic) 
> import Modulation
> import Parthenopea
> import Scoring
> import SettingsDefs
> import Synthesizer
> import qualified System.FilePattern.Directory
>                                          as FP
  
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

importing sampled sound (from SoundFont (*.sf2) files) ================================================================

> data PreSampleKey =
>   PreSampleKey {
>     pskwFile           :: Word
>   , pskwSampleIndex    :: Word} deriving (Eq, Generic, Ord, Show)
> instance HasTrie PreSampleKey where
>   newtype (PreSampleKey :->: b)          = PreSampleKeyTrie { unPreSampleKeyTrie :: Reg PreSampleKey :->: b } 
>   trie                                   = trieGeneric PreSampleKeyTrie 
>   untrie                                 = untrieGeneric unPreSampleKeyTrie
>   enumerate                              = enumerateGeneric unPreSampleKeyTrie
> nilPreSampleKey                          = PreSampleKey 0 0
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
>   | MakeRight PreZoneKey deriving Eq
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
>   show (PreZone{ .. })                   = unwords ["PreZone", show (pzWordF, pzWordS, pzWordI, pzWordB), show pzDigest]
> extractSampleKey pz                      = PreSampleKey pz.pzWordF pz.pzWordS
> extractInstKey pz                        = PerGMKey pz.pzWordF pz.pzWordI Nothing
> extractZoneKey pz                        = PreZoneKey pz.pzWordF pz.pzWordB
> extractSpace pz                          = (pz.pzWordB, [pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange])
> effShdr                :: Map PreSampleKey PreSample → PreZone → F.Shdr
> effShdr psCache pz                       =
>   foldl' (\s x → (\case
>                   MakeMono               → s{F.sampleType = fromSampleType SampleTypeMono, F.sampleLink = 0}
>                   MakeLeft pz            → s{F.sampleType = fromSampleType SampleTypeLeft, F.sampleLink = 0}
>                   MakeRight pz           → s{F.sampleType = fromSampleType SampleTypeRight, F.sampleLink = 0}) x)
>          (psShdr (deJust "rawShdr" (Map.lookup (extractSampleKey pz) psCache)))
>          pz.pzChanges
> appendChange pz@PreZone{ .. } change     = pz{pzChanges = pzChanges ++ singleton change}
> showPreZones pzs                         = show $ map pzWordB pzs
>
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
>   , pgkwBag            :: Maybe Word} deriving (Eq, Generic, Ord, Show)
> instance HasTrie PerGMKey where
>   newtype (PerGMKey :->: b)              = PerGMKeyTrie { unPerGMKeyTrie :: Reg PerGMKey :->: b } 
>   trie                                   = trieGeneric PerGMKeyTrie 
>   untrie                                 = untrieGeneric unPerGMKeyTrie
>   enumerate                              = enumerateGeneric unPerGMKeyTrie
> nilPerGMKey            :: PerGMKey       = PerGMKey 0 0 Nothing
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
> findBySampleIndex      :: [SFZone] → Word → Maybe SFZone
> findBySampleIndex zones w                =
>   find (\SFZone{zSampleIndex} → zSampleIndex == Just w) zones
>
> findBySampleIndex'     :: [(a, SFZone)] → Word → Maybe (a, SFZone)
> findBySampleIndex' zs w                  =
>   find (\(_, SFZone{zSampleIndex}) → zSampleIndex == Just w) zs
>
> findByBagIndex         :: [PreZone] → Word → Maybe PreZone
> findByBagIndex pzs w                     = find (\PreZone{pzWordB} → w == pzWordB) pzs
>
> findByBagIndex'        :: [(PreZone, a)] → Word → Maybe (PreZone, a)
> findByBagIndex' zs w                     = find (\(pz, _) → w == pz.pzWordB) zs
>
> data PerInstrument                       =
>   PerInstrument {
>     pZones             :: [(PreZone, SFZone)]
>   , pSmashing          :: Smashing Word}
> showBags               :: PerInstrument → String
> showBags perI                            = show (map (pzWordB . fst) perI.pZones)

Instrument categories: instrument, percussion, disqualified

> data InstCat                             =
>        InstCatInst InstCatData
>      | InstCatPerc InstCatData
>      | InstCatDisq [Scan] deriving Show
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
>
>     
> data InstCatData                         =
>   InstCatData {
>     inPreZones         :: [PreZone]
>   , inSmashup          :: Smashing Word
>   , inPercBixen        :: [Word]} deriving Show
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
> data SFRoster                            =
>   SFRoster {
>     zFiles             :: Array Word SFFile
>   , zPreSampleCache    :: Map PreSampleKey PreSample
>   , zPartnerCache      :: Map PreZoneKey SFZone
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>   , zOwners            :: Map PerGMKey [PreZone]
>   , zRost              :: ([InstrumentName], [PercussionSound])
>   , zPerInstCache      :: Map PerGMKey PerInstrument
>   , zWinningRecord     :: WinningRecord}
>
> seedRoster :: Array Word SFFile → ([InstrumentName], [PercussionSound]) → SFRoster
> seedRoster vFile rost                    =
>   SFRoster vFile Map.empty Map.empty Map.empty Map.empty rost Map.empty seedWinningRecord
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
> theGrader                                = Grader ssWeights 500

profiler ==============================================================================================================

> -- (removed...revisit when things settle down)
> profileSF2s            :: IO ()
> profileSF2s                              = print "removed"

executive =============================================================================================================

> doEverything           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> doEverything songs     = do
>   putStrLn "everything..."
>   putStrLn $ reapEmissions emitSettingses
>   putStrLn ""
>
>   tsStarted            ← getCurrentTime
>
>   rost                 ← qualifyKinds songs
>   putStrLn $ unwords ["rost", show rost]
>   putStrLn ""
>
>   -- represent all input SoundFont files in ordered list, thence a vector
>   fps                  ← FP.getDirectoryFiles "." (singleton "*.sf2")
>   if null fps
>     then do
>       putStrLn "no *.sf2 files found"
>     else do
>       let nfiles       = length fps
>       let boundsF::(Word, Word)
>                        = (0, fromIntegral (nfiles - 1))
>       sffilesp         ← CM.zipWithM openSoundFontFile [0..] fps
>       putStrLn ""
>       let preRoster    = seedRoster (listArray boundsF sffilesp) rost
>
>       tsLoaded         ← getCurrentTime
>       putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>       -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>       sfrost           ← finishRoster preRoster
>
>       CM.when doRender (doRendering sfrost)
>
>       tsRendered       ← getCurrentTime
>       putStrLn ("___overall: " ++ show (diffUTCTime tsRendered tsStarted))
>   where
>     fName              = "doEverything"
>
>     -- track the complete _qualified_ populations of: samples, instruments, percussion
>     finishRoster       :: SFRoster → IO SFRoster
>     finishRoster preR  = do
>       let fName        = "finishRoster"
>       tsStarted        ← getCurrentTime
>
>       (preSampleCache, sPartnerMap, rdGen01)
>                        ← initSamples preR.zFiles virginrd
>       (preInstCache_, rdGen02)
>                        ← initInsts preR.zFiles rdGen01
>       (preInstCache, pOwners, rdGen03)
>                        ← initZones preR.zFiles preSampleCache sPartnerMap preInstCache_ rdGen02
>
>       jobs             ← categorize preR.zFiles preSampleCache preInstCache pOwners preR.zRost
>       tsCatted         ← getCurrentTime
>       putStrLn ("___categorize: " ++ show (diffUTCTime tsCatted tsStarted))
>
>       (zc, rdGen04)    ← formZoneCache preR.zFiles preInstCache preR.zRost jobs rdGen03
>       owners'          ← reassociateZones zc
>
>       (pergmsI, pergmsP)
>                        ← sortByCategory preInstCache jobs
>       CM.when diagnosticsEnabled (putStrLn $ unwords [fName, "ilen, plen", show (length pergmsI, length pergmsP)])
>
>       tsZoned          ← getCurrentTime
>       putStrLn ("___cache zones: " ++ show (diffUTCTime tsZoned tsCatted))
>
>       CM.when reportScan (writeScanReport
>                             preR{zPreSampleCache = preSampleCache, zPreInstCache = preInstCache}
>                             rdGen04)
>       tsScanned        ← getCurrentTime
>      
>       -- actually conduct the tournament
>       ((wI, sI), (wP, sP))
>                        ← decideWinners preR.zFiles preSampleCache preInstCache owners'
>                                        zc preR.zRost pergmsI pergmsP
>       tsDecided        ← getCurrentTime
>       putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsScanned))
>
>       CM.when reportTourney (writeTournamentReport preR.zFiles wI wP)
>       tsReported       ← getCurrentTime
>
>       -- print song/orchestration info to user (can be captured by redirecting standard out)
>       mapM_ putStrLn (sI ++ sP)
>
>       let wins         = WinningRecord (Map.map head wI) (Map.map head wP)
>
>       let sfrost       = preR{ zPreSampleCache   = preSampleCache
>                             -- WOX  , zPartnerCache     = formZPartnerCache preSampleCache zc preZoneCache
>                              , zPreInstCache     = preInstCache
>                              , zOwners           = pOwners
>                              , zPerInstCache     = zc
>                              , zWinningRecord    = wins}
>       
>       tsRecond     ← getCurrentTime
>       putStrLn ("___create winning record: " ++ show (diffUTCTime tsRecond tsReported))
>         
>       return sfrost
>
>     -- get it on
>     doRendering      :: SFRoster → IO ()
>     doRendering sfrost                   = do
>       tsStarted        ← getCurrentTime
>
>       -- readying instrument maps to be accessed from song renderer
>       traceIO          "prepareInstruments"
>       imap             ← prepareInstruments sfrost
>       tsPrepared       ← getCurrentTime
>       putStrLn ("___prepare instruments: " ++ show (diffUTCTime tsPrepared tsStarted))
>
>       -- here's the heart of the coconut
>       mapM_ (uncurry (renderSong sfrost imap)) songs
>
>       tsRendered       ← getCurrentTime
>       putStrLn ("___render songs: "        ++ show (diffUTCTime tsRendered tsPrepared))

To support extracting from flawed SoundFont files, we - up front - withdraw unrecoverable items from
their respective collections. The withdrawn items are ignored by all later phases. When constructing those
later items, some critical data may thereby be missing. So that entails deletion-recovery also.

>     initSamples        :: Array Word SFFile
>                           → ResultDispositions
>                           → IO (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
>     initSamples sffiles rd
>                        = do
>       (preSampleCache, rd')
>                        ← formPreSampleCache sffiles rd
>       CM.when
>         diagnosticsEnabled
>         (putStrLn (unwords [fName, "presample length", show (length preSampleCache, rdLengths rd')]))
>
>       doPartnering sffiles preSampleCache rd'
>         
>     initInsts          :: Array Word SFFile
>                           → ResultDispositions
>                           → IO (Map PerGMKey PreInstrument, ResultDispositions)
>     initInsts sffiles rd
>                        = do
>       (preInstCache, rd')
>                        ← formPreInstCache sffiles rd
>       return (preInstCache, rd')
>
>     initZones          :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PreSampleKey PreSampleKey → Map PerGMKey PreInstrument
>                           → ResultDispositions
>                           → IO (Map PerGMKey PreInstrument, Map PerGMKey [PreZone], ResultDispositions)
>     initZones sffiles preSampleCache sPartnerMap preInstCache_ rd_
>                        = do
>       (preInstCache, pOwners, rd)
>                        ← formPreZones sffiles preSampleCache sPartnerMap preInstCache_ rd_
>       return (preInstCache, pOwners, rd)
>
> reassociateZones       :: Map PerGMKey PerInstrument → IO (Map PerGMKey [PreZone])
> reassociateZones zc    = return $ Map.map (\q → map fst q.pZones) zc
>
> writeScanReport        :: SFRoster → ResultDispositions → IO ()
> writeScanReport sfrost rd@ResultDispositions{ .. }
>                        = do
>   CM.when diagnosticsEnabled (putStrLn $ unwords [fName, "rdLengths", show (rdLengths rd)])
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
>   putStrLn ("___report scan results: " ++ show (diffUTCTime tsFinished tsStarted))
>   traceIO ("wrote " ++ reportScanName)
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
>         ss             = if howVerboseScan < 0.5
>                            then filter (\s → Accepted /= s.sDisposition) ss_
>                            else ss_
>       in
>         if null ss
>           then []
>           else emit sfrost k ++ [EndOfLine] ++ concatMap procs ss ++ [EndOfLine]
>
>     procs          :: Scan → [Emission]
>     procs scan
>                        =
>       [  emitShowL scan.sDisposition 24 -- WOX need unit test that checks the lengths of the enum names
>        , emitShowL scan.sImpact      32
>        , ToFieldL scan.sFunction     52
>        , Unblocked scan.sClue
>        , EndOfLine]
>
> writeTournamentReport  :: Array Word SFFile
>                           → Map InstrumentName [PerGMScored]
>                           → Map PercussionSound [PerGMScored]
>                           → IO ()
> writeTournamentReport sffiles pContI pContP
>                        = do
>   tsStarted            ← getCurrentTime
>
>   -- output all selections to the report file
>   let legend           =
>           emitComment     [   Unblocked "legend = [hints, stereo, 24-bit, resolution, conformant, fuzzy]"]
>        ++ emitNextComment [   Unblocked "weights = "
>                             , Unblocked (show ssWeights)] 
>   let esFiles          = emitFileListC ++ [EndOfLine]
>   let esI              = concatMap dumpContestants (Map.toList pContI)
>   let esP              = concatMap dumpContestants (Map.toList pContP)
>   let esQ              = emitSettingses
>   let esTail           = singleton $ Unblocked "\n\nThe End\n\n"
>   let eol              = singleton EndOfLine
>
>   writeFileBySections reportTournamentName [esFiles, legend, esI, eol, esFiles, legend, esP, esQ, esTail]
>   tsFinished           ← getCurrentTime
>   putStrLn ("___report tournament results: " ++ show (diffUTCTime tsFinished tsStarted))
>   traceIO ("wrote " ++ reportTournamentName)
>
>   where
>     emitFileListC      = concatMap (uncurry doF) (zip [0..] (toList sffiles))
>     doF nth sffile     = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>

tournament starts here ================================================================================================

> decideWinners          :: Array Word SFFile
>                           → Map PreSampleKey PreSample
>                           → Map PerGMKey PreInstrument
>                           → Map PerGMKey [PreZone]
>                           → Map PerGMKey PerInstrument
>                           → ([InstrumentName], [PercussionSound]) 
>                           → [PerGMKey]
>                           → [PerGMKey]
>                           → IO ((Map InstrumentName [PerGMScored], [String])
>                               , (Map PercussionSound [PerGMScored], [String]))
> decideWinners sffiles preSampleCache preInstCache owners zc rost pergmsI pergmsP
>                                          = do
>   traceIO "decideWinners"
>   return wiExec
>
>   where
>     wiExec             :: (  (Map InstrumentName [PerGMScored], [String])
>                            , (Map PercussionSound [PerGMScored], [String]))
>     wiExec                               = ((wI', sI), (wP', sP))
>       where
>         (wI, sI)                         = foldl' wiFolder (Map.empty, []) pergmsI         
>         wI'                              = Map.map (sortOn (Down . pScore . pArtifactGrade)) wI
>
>         (wP, sP)                         = foldl' wpFolder (Map.empty, []) pergmsP
>         wP'                              = Map.map (sortOn (Down . pScore . pArtifactGrade)) wP
>
>     wiFolder           :: (Map InstrumentName [PerGMScored], [String])
>                           → PerGMKey
>                           → (Map InstrumentName [PerGMScored], [String])
>     wiFolder target pergmI@PerGMKey{pgkwFile}
>                                          = foldl' (xaEnterTournament fuzzMap pergmI []) target as'
>       where
>         -- access potentially massive amount of processed information regarding instrument
>         preI                             = deJust (unwords["wiFolder", "preI"]) (Map.lookup pergmI preInstCache)
>         pzs                              = deJust "pzs" (Map.lookup pergmI owners)
>         fuzzMap                          = getFuzzMap preI.iMatches
>
>         as             :: Map InstrumentName Fuzz
>         as                               =
>           Map.filterWithKey (\k v → k `elem` select rost) fuzzMap
>
>         as'            :: [InstrumentName]
>         as'                              =
>           profess
>             (not $ null as)
>             (unwords ["unexpected empty matches for", show pgkwFile, preI.iName]) 
>             (if multipleCompetes
>                then Map.keys as
>                else (singleton . fst) (Map.findMax as))
>     
>     wpFolder           :: (Map PercussionSound [PerGMScored], [String])
>                           → PerGMKey
>                           → (Map PercussionSound [PerGMScored], [String])
>     wpFolder wIn pergmP@PerGMKey{pgkwFile, pgkwBag}
>       | traceIf trace_WP False           = undefined
>       | otherwise                        = xaEnterTournament fuzzMap pergmP [] wIn kind
>       where
>         trace_WP                         =
>           unwords ["wpFolder", show preI.iName, show pergmP, "of", show (length perI.pZones)]
>
>         preI                             =
>           deJust (unwords["wpFolder", "PreInst"]) (Map.lookup (pergmP{pgkwBag = Nothing}) preInstCache)
>         pzs                              = deJust "pzs" (Map.lookup pergmP{pgkwBag = Nothing} owners)
>         perI                             =
>           deJust (unwords["wpFolder", "PerInst"]) (Map.lookup (pergmP{pgkwBag = Nothing}) zc)
>
>         mz             :: Maybe PreZone
>         mz                               = pgkwBag >>= findByBagIndex pzs
>         mkind          :: Maybe PercussionSound
>         mkind                            = mz >>= getAP >>= pitchToPerc
>         kind                             = deJust (unwords["wpFolder", "mkind"]) mkind
>
>         mffm           :: Maybe FFMatches
>         mffm                             =
>           mz >>= (zdSampleIndex . pzDigest)
>              >>= Just . PreSampleKey pgkwFile
>              >>= (`Map.lookup` preSampleCache)
>              >>= Just . sMatches
>         fuzzMap        :: Map PercussionSound Fuzz
>         fuzzMap                          = getFuzzMap $ deJust (unwords ["mffm"]) mffm
>
>         getAP          :: PreZone → Maybe AbsPitch
>         getAP pz                         = pz.pzDigest.zdKeyRange >>= (Just . fromIntegral . fst)
>
>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           Map a Fuzz
>                           → PerGMKey
>                           → [SSHint]
>                           → (Map a [PerGMScored], [String])
>                           → a
>                           → (Map a [PerGMScored], [String])
>     xaEnterTournament fuzzMap pergm hints (wins, ss) kind
>       | traceIf trace_XAET False         = undefined
>       | otherwise                        = (Map.insertWith (++) kind [scored] wins, ss)
>       where
>         fName                            = "xaEnterTournament"
>
>         pergm_                           = pergm{pgkwBag = Nothing}
>         preI                             =
>           deJust (unwords[fName, "PreInstrument", show pergm_]) (Map.lookup pergm_ preInstCache)
>         perI                             =
>           deJust (unwords[fName, "PerInstrument", show pergm_]) (Map.lookup pergm_ zc)
>
>         scope_, scope  :: [(PreZone, SFZone)]
>         scope_                           =
>           case pergm.pgkwBag of
>             Nothing                      → perI.pZones
>             Just bagI                    →
>                    maybe
>                      (error $ unwords [fName, "findByBagIndex' returned a Nothing for"
>                                       , show pergm.pgkwFile, preI.iName, show bagI])
>                      singleton
>                      (findByBagIndex' perI.pZones bagI)
>         scope                            =
>           profess
>             (not (null scope_))
>             (unwords[fName, "null scope", preI.iName])
>             scope_
>             
>         mnameZ         :: Maybe String   = pergm.pgkwBag
>                                            >>= findByBagIndex' perI.pZones
>                                            >>= \(q, _) → Just (F.sampleName (effShdr preSampleCache q))
>
>         trace_XAET                       =
>           unwords [fName, preI.iName, fromMaybe "" mnameZ, show kind]
>
>         computeGrade   :: [(PreZone, SFZone)] → AgainstKindResult → ArtifactGrade
>         computeGrade zs akResult         = gradeEmpiricals theGrader empiricals
>           where
>             empiricals :: [Double]       = [   foldHints hints
>                                              , fromRational $ scoreBool $ isStereoInst preSampleCache zs
>                                              , fromRational $ scoreBool $ is24BitInst preSampleCache zs
>                                              , computeResolution kind rost preI zs
>                                              , fromRational $ scoreBool $ all (zoneConforms preSampleCache) zs
>                                              , fuzz]
>             howgood                      = akResult - stands
>             fuzz       :: Double
>               | howgood > 0.000_001      = max 0 (logBase 2 howgood) * fuzzFactor kind
>               | otherwise                = 0
>   
>         scored         :: PerGMScored    =
>           PerGMScored (computeGrade scope akResult) (toGMKind kind) akResult pergm preI.iName mnameZ
>
>         computeResolution
>                        :: ∀ a. (Show a, SFScorable a) ⇒
>                           a
>                           → ([InstrumentName], [PercussionSound])
>                           → PreInstrument
>                           → [(PreZone, SFZone)]
>                           → Double
>         computeResolution kind rost preI zs
>           | null zs                      = error $ unwords ["null zs"]
>           | otherwise                    = fromRational m1 * evalSplits kind + fromRational m2 * evalSampleSize
>           where
>             theSplit                     = splitScore kind (map fst zs)
>             evalSplits kind
>               | theSplit <= 1            = 1
>               | otherwise                = log (m3 * theSplit)
>             evalSampleSize               = sum (map durScoring zs) / fromIntegral (length zs)
>
>             m1                           = 1/2
>             m2                           = 1/2
>             m3                           = 3 * if isStereoInst preSampleCache zs then 1/2 else 1
>
>         durScoring     :: (PreZone, SFZone) → Double
>         durScoring (pz, zone) = if score < 0.01 then -10 else score
>           where
>             shdr                         = effShdr preSampleCache pz
>             score                        = sampleSize / fromIntegral shdr.sampleRate
>
>             sampleSize :: Double
>             sampleSize                   = fromIntegral $ xEnd - xStart
>               where
>                 xStart                   =
>                   addIntToWord    shdr.start
>                                   (sumOfWeightedInts [zone.zStartOffs, zone.zStartCoarseOffs] qOffsetWeights)
>                 xEnd                     =
>                   addIntToWord    shdr.end
>                                   (sumOfWeightedInts [zone.zEndOffs,   zone.zEndCoarseOffs]   qOffsetWeights)
>
>         akResult                         = fromMaybe 0 (Map.lookup kind fuzzMap)

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
>                                              then accepted sa Ok ""
>                                              else ss_
>
> scanScans              :: [Maybe [Scan]] → ScanScan
> scanScans alts                           = s2Final
>   where
>    fName                                 = "scanScans"
>
>    s2Final             :: ScanScan       =
>      head $ dropWhile unfinished (iterate' nextGen (ScanScan [] alts))
>
>    unfinished s2gen@ScanScan{ .. }       = not (cancels [Accepted] s2Scans) && not (null s2Alts)
>
>    nextGen s2gen@ScanScan{ .. }          =
>      s2gen{s2Scans = s2Scans ++ fromMaybe [] (head s2Alts), s2Alts = tail s2Alts}
>
> cancels                :: [Disposition] → [Scan] → Bool
> cancels eds ss_                          = any odd (Map.elems m)
>   where
>     ss                                   = filter (\s → s.sDisposition `notElem` eds) ss_
>
>     m                  :: Map Impact Int
>     m                                    = foldl' (\n v → Map.insertWith (+) v 1 n) Map.empty (map sImpact ss)
>
> accepted, violated, dropped, rescued, violatedOrRescued
>                        :: ∀ r a . (SFResource r) ⇒ ScanAlts r → Impact → a → [Scan]
> accepted sa impact thing                 = singleton $ Scan Accepted impact sa.saFromName (zshow thing)
> violated sa impact thing                 = singleton $ Scan Violated impact sa.saFromName (zshow thing)
> dropped sa impact thing                  = singleton $ Scan Dropped impact sa.saFromName (zshow thing)
> rescued sa impact  thing                 = singleton $ Scan Rescued impact sa.saFromName (zshow thing)
> noChange sa impact thing                 = singleton $ Scan NoChange impact sa.saFromName (zshow thing)
> violatedOrRescued sa impact thing        = violated sa impact thing ++ rescued sa impact thing
>
> data ResultDispositions                  =
>   ResultDispositions {
>     preSampleDispos    :: Map PreSampleKey     [Scan]
>   , preInstDispos      :: Map PerGMKey         [Scan]} deriving Show
> virginrd                                 = ResultDispositions Map.empty Map.empty
> emptyrd ResultDispositions{ .. }         = null preSampleDispos && null preInstDispos
> rdLengths ResultDispositions{ .. }       = (length preSampleDispos, length preInstDispos)
> countScans             :: ∀ k . SFResource k ⇒ Map k [Scan] → Int
> countScans                               = foldl' (\n ss → n + length ss) 0
> rdScans  ResultDispositions{ .. }        = (countScans preSampleDispos, countScans preInstDispos)
> combinerd rd1 rd2                        =
>   rd1{  preSampleDispos                  = Map.unionWith (++) rd1.preSampleDispos rd2.preSampleDispos
>       , preInstDispos                    = Map.unionWith (++) rd1.preInstDispos   rd2.preInstDispos}
>
> class SFResource a where
>   sfkey                :: Word → Word → a
>   dispose              :: a → [Scan] → ResultDispositions → ResultDispositions
>   fatalrd              :: [Disposition] → ResultDispositions → a → Bool
>   emit                 :: SFRoster → a → [Emission]
>
> instance SFResource PreSampleKey where
>   sfkey                                  = PreSampleKey
>   dispose presk ss rd                    =
>     rd{preSampleDispos = Map.insertWith (flip (++)) presk ss rd.preSampleDispos}
>   fatalrd eds rd presk                   = cancels eds $ fromMaybe [] (Map.lookup presk rd.preSampleDispos)
>   emit sfrost presk                      =
>     [  Unblocked (show presk)
>      , Blanks 5
>      , Unblocked sffile.zFilename
>      , Blanks 5
>      , Unblocked (show shdr.sampleName)]
>     where
>       sffile                             = sfrost.zFiles ! presk.pskwFile
>       sfboota                            = sffile.zBoot
>       shdr                               = sfboota.ssShdrs ! presk.pskwSampleIndex
>
> instance SFResource PerGMKey where
>   sfkey wF wI                            = PerGMKey wF wI Nothing
>   dispose pergm ss rd                    =
>     rd{preInstDispos = Map.insertWith (flip (++)) pergm ss rd.preInstDispos}
>   fatalrd eds rd pergm                   = cancels eds $ fromMaybe [] (Map.lookup pergm rd.preInstDispos)
>   emit sfrost pergm                      =
>     [  Unblocked (show pergm)
>      , Blanks 5
>      , Unblocked (sfrost.zFiles ! pergm.pgkwFile).zFilename
>      , Blanks 5
>      , Unblocked (show iinst.instName)]
>     where
>       sffile                             = sfrost.zFiles ! pergm.pgkwFile
>       sfboota                            = sffile.zBoot
>       iinst                              = sfboota.ssInsts ! pergm.pgkwInst
>
> formComprehension      :: ∀ r a . SFResource r ⇒ Array Word SFFile → (BootstrapArrays → Array Word a) → [r]
> formComprehension sffiles blobfun        = concatMap formFolder sffiles
>   where
>     fName                                = "formComprehension"
>
>     formFolder         :: SFFile → [r]
>     formFolder sffile                    =
>       let
>         (st, en)       :: (Word, Word)   = bounds $ blobfun sffile.zBoot
>         range                            =
>           profess
>             ((st == 0) && (st <= en) && (en < 2_147_483_648))
>             (error $ unwords [fName, "corrupt blob"])
>             (deriveRange st en)
>       in
>         map (sfkey sffile.zWordF) range  
>
> formPreSampleCache     :: Array Word SFFile
>                           → ResultDispositions
>                           → IO (Map PreSampleKey PreSample, ResultDispositions)
> formPreSampleCache sffiles rd            =
>   return $ foldl' formFolder (Map.empty, rd) (formComprehension sffiles ssShdrs)
>   where
>     fName_                               = "formPreSampleCache"
>
>     formFolder         :: (Map PreSampleKey PreSample, ResultDispositions)
>                           → PreSampleKey
>                           → (Map PreSampleKey PreSample, ResultDispositions)
>     formFolder (target, rd) presk        =
>       if cancels [Accepted] scanned
>         then (target,                                          rd')
>         else (Map.insert presk (computePreSample shdr) target, rd')
>       where
>         fName                            = unwords [fName_, "formFolder"]
>
>         shdr@F.Shdr{ .. }                = (sffiles ! presk.pskwFile).zBoot.ssShdrs ! presk.pskwSampleIndex
>         sa                               = ScanAlts presk fName []
>         (scanned, rd')                   = scanAlts sa alts rd
>           where
>             alts                         =
>               [  curate sampleName      goodName                          (violated sa CorruptName)
>                , curate sampleRate      (\x → x == clip (64, 2^20) x)     (violated sa BadSampleRate)
>                , curate sampleType      (isJust . toMaybeSampleType)      (violated sa BadSampleType)
>                , curate (start, end)    sampleSizeOk                      (violated sa BadSampleLimits)]
>
>            -- WOX , curateThing (startLoop, endLoop, sampleMode)
>            -- WOX                                      sampleLoopSizeOk                  (violateThing BadSampleLoopLimits fName)]
>
> doPartnering           :: Array Word SFFile
>                           → Map PreSampleKey PreSample
>                           → ResultDispositions
>                           → IO (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
> doPartnering sffiles preSampleCache rd   =
>   return $ foldl' partneringFolder (Map.empty, Map.empty, rd) (Map.assocs preSampleCache)
>   where
>     fName_                               = "doPartnering"
>
>     partneringFolder   :: (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
>                           → (PreSampleKey, PreSample)
>                           → (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
>     partneringFolder (target, sPartnerMap, rd) (k, v)
>       | cancels [Accepted, NoChange] scanned
>                                          = (target,                sPartnerMap,                    rd')
>       | cancels [Accepted] scanned       = (Map.insert k v target, sPartnerMap,                    rd') 
>       | otherwise                        = (Map.insert k v target, makePartner (Just otherKey),    rd')
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
>         sffile                           = sffiles ! k.pskwFile
>         shdr                             = sffile.zBoot.ssShdrs ! k.pskwSampleIndex
>         oshdr                            = sffile.zBoot.ssShdrs ! otherKey.pskwSampleIndex
>         stype                            = toSampleType shdr.sampleType
>         stereo                           = SampleTypeLeft == stype || SampleTypeRight == stype
>
>         possiblySave errStr              = if canDevolveToMono
>                                              then Left (k, v{psChanges = singleton MakeMono}, Nothing)
>                                              else Right errStr
>
>         sa                               = ScanAlts k fName []
>         (scanned, rd')                   = scanAlts sa alts rd
>           where
>             alts                         =
>               [  curate stereo           id                  (noChange sa Ok)
>                , curate' other           isJust              (violated sa MissingStereoPartner)
>                , curate backLink         isJust              (violated sa BadStereoPartner)]

PreZone administration ================================================================================================

> data FileInstScan                        =
>   FileInstScan {
>     isResults          :: ([InstZoneScan], ResultDispositions)
>   , isTasks            :: [([InstZoneScan], ResultDispositions) → ([InstZoneScan], ResultDispositions)]}
> isUnfinished iscan                       = not (null iscan.isTasks)
> getZones               :: ([InstZoneScan], ResultDispositions) → Map PerGMKey [PreZone]
> getZones (zscans, rdNow)
>   | traceIf trace_GZ False              = undefined
>   | otherwise                           = foldl' getFolder Map.empty zscans
>   where
>     fName                                = "getZones"
>     (good, bad)                          = partition (not . fatalrd [Accepted, NoChange] rdNow . instKey) zscans
>     trace_GZ                             = unwords[fName, show (length good, length bad)]
>
>     getFolder          :: Map PerGMKey [PreZone] → InstZoneScan → Map PerGMKey [PreZone]
>     getFolder m i                        =
>       let
>         pergm                            = instKey i
>       in
>         if fatalrd [Accepted, NoChange] rdNow pergm then m else Map.insert pergm i.zsPreZones m
>
> goodZScans, badZScans  :: ([InstZoneScan], ResultDispositions) → [InstZoneScan]
> goodZScans (zscans, rdNow)               = filter (not . fatalrd [Accepted, NoChange] rdNow . instKey) zscans
> badZScans (zscans, rdNow)                = filter (fatalrd [Accepted, NoChange] rdNow . instKey) zscans
>
> data InstZoneScan                        =
>   InstZoneScan {
>     zswFile            :: Word
>   , zswInst            :: Word
>   , zswGBix            :: Maybe Word
>   , zsPreZones         :: [PreZone]}
> instance Show InstZoneScan where
>   show (InstZoneScan{ .. })              = unwords ["InstZoneScan", show zswFile, show zswInst, show zswGBix]
> makeZScan              :: Word → Word → [PreZone] → InstZoneScan
> makeZScan wF wI                          = InstZoneScan wF wI Nothing
> instKey                :: InstZoneScan → PerGMKey
> instKey zscan                            = PerGMKey zscan.zswFile zscan.zswInst Nothing
>         
> formPreZones           :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PreSampleKey PreSampleKey → Map PerGMKey PreInstrument
>                           → ResultDispositions
>                           → IO (Map PerGMKey PreInstrument, Map PerGMKey [PreZone], ResultDispositions)
> formPreZones sffiles preSampleCache sPartnerMap preInstCache rd_
>                                          = CM.foldM formFolder (preInstCache, Map.empty, rd_) sffiles
>   where
>     fName_                               = "formPreZones"
>
>     formFolder         :: (Map PerGMKey PreInstrument, Map PerGMKey [PreZone], ResultDispositions)
>                           → SFFile
>                           → IO (Map PerGMKey PreInstrument, Map PerGMKey [PreZone], ResultDispositions)
>     formFolder (preIs, preZs, rd) sffile = do
>       let fName                          = unwords[fName_, "formFolder"]
>       CM.when diagnosticsEnabled (putStrLn (unwords [fName, "lengths", show (length preSampleCache, length preZs, length preIs)]))
>       (preIs', preZs', rd')              ← captureFileZones sffile rd
>       return (preIs', preZs `Map.union` preZs', rd')
>          
>     makePreZone wF wS wI wB gens shdr    = PreZone wF wS wI wB (formDigest gens) [] []
>
>     captureFileZones   :: SFFile → ResultDispositions
>                           → IO (Map PerGMKey PreInstrument, Map PerGMKey [PreZone], ResultDispositions)
>     captureFileZones sffile rdFile_
>                                          = do
>       let preIs                          = 
>             foldl' markGlobalZone shavePreInstCache (goodZScans isFinal.isResults)
>       let preZs                          = getZones isFinal.isResults
>       CM.when diagnosticsEnabled (putStrLn (unwords [fName, "z&i lengths", show (length preZs, length preIs)]))
>       return (  preIs
>               , preZs
>               , rdFile_ `combinerd` snd isFinal.isResults)
>       where
>         fName                            = unwords [fName_, "captureFileZones"]
>
>         (stI, enI)     :: (Word, Word)   = bounds sffile.zBoot.ssInsts
>         uncut, cut     :: [Word]
>         uncut                            = deriveRange stI enI
>         cut                              =
>           filter (\wI → isJust $ Map.lookup (PerGMKey sffile.zWordF wI Nothing) preInstCache) uncut
>
>         tasks          :: [([InstZoneScan], ResultDispositions) → ([InstZoneScan], ResultDispositions)]
>         tasks                            = if combinePartials
>                                              then error "not supported" -- WOX [groomTask, vetTask, reorgTask]
>                                              else [groomTask, vetTask]
>
>         isInitial, isFinal
>                        :: FileInstScan
>         isInitial                        = FileInstScan (foldl' captureInstZones ([], virginrd) cut) tasks
>         isFinal                          = head $ dropWhile isUnfinished (iterate' nextGen isInitial)
>
>         nextGen        :: FileInstScan → FileInstScan
>         nextGen iscan@FileInstScan{ .. } = iscan{isResults = head isTasks isResults, isTasks = tail isTasks}
>
>         shavePreInstCache                =
>           foldl' (\x y → Map.delete (instKey y) x) preInstCache (badZScans isFinal.isResults)

capture (initial) task ================================================================================================
          assign each instrument's info to an InstZoneScan, capture PreZones; skip global zones, (but later "mark")

>         captureInstZones
>                         :: ([InstZoneScan], ResultDispositions) → Word → ([InstZoneScan], ResultDispositions)
>         captureInstZones (zscans, rdInst_) wI
>           | traceNot trace_CIZ False     = undefined
>           | otherwise                    = (zscans ++ [zscan], rd')
>           where
>             fName                        = unwords[fName_, "captureInstZones"]
>             trace_CIZ                    =
>               unwords[fName, show (length pzsRemaining), show pergm, show (rdScans rdInst_)]
>
>             pergm                        = PerGMKey sffile.zWordF wI Nothing
>             preI                           =
>               deJust "preI" (Map.lookup (PerGMKey sffile.zWordF wI Nothing) preInstCache)
>             sa                           = ScanAlts pergm fName []
>             (scanned, rd')               = scanAlts sa alts rdInst_
>               where
>                 alts                     =
>                   [  curate preOk        id                  (violated sa OrphanedByInst)
>                    , curate pzsRemaining (not . null)        (violated sa NoZones)]
>
>             preOk                        = isJust $ Map.lookup pergm preInstCache
>
>             results                      = map captureZone (deriveRange ibagi jbagi)
>
>             ibagi                        = F.instBagNdx (sffile.zBoot.ssInsts ! wI)
>             jbagi                        = F.instBagNdx (sffile.zBoot.ssInsts ! (wI+1))
>
>             mGBKey                       = if head results == Right "global zone"
>                                              then Just ibagi
>                                              else Nothing
>             zscan                        = makeZScan sffile.zWordF wI pzsRemaining
>
>             bads                         = rights results
>             pzsRemaining                 = lefts results
>             
>             captureZone
>                        :: Word → Either PreZone String
>             captureZone bix              = zTry
>               where
>                 fName                    = unwords [fName_, "captureZone"]
>                 
>                 zTry
> {-
>                   | isNothing starget        =
>                     (  zscan, Map.singleton (instKey zscan) (violate OrphanedBySample  fName ""))
> -}
>                   | isNothing pz.pzDigest.zdSampleIndex
>                                          = Right "global zone"
>                   | isNothing starget    = Right (unwords [fName, "orphaned by sample"])
>                   | not limitsCheckedOk  = Right (unwords [fName, "problem", "corrupt adjusted limits"]) 
>                   | otherwise            = Left pz{pzChanges = pres.psChanges}
>
>                 xgeni                    = F.genNdx $ sffile.zBoot.ssIBags ! bix
>                 ygeni                    = F.genNdx $ sffile.zBoot.ssIBags ! (bix + 1)
>
>                 gens   :: [F.Generator]  = profess
>                                              (xgeni <= ygeni)
>                                              (unwords [fName, "SoundFont file corrupt (gens)"])
>                                              (map (sffile.zBoot.ssIGens !) (deriveRange xgeni ygeni))
>                 pz                       = makePreZone sffile.zWordF si wI bix gens shdr
>                 si                       = deJust "produce si" pz.pzDigest.zdSampleIndex
>                 shdr                     = sffile.zBoot.ssShdrs ! si
>
>                 limitsCheckedOk          = adjustedSampleSizeOk pz.pzDigest shdr
>                 presk                    = PreSampleKey sffile.zWordF si
>                 starget                  = Map.lookup presk preSampleCache
>                 pres                     = deJust "pres" starget

iterating on InstZoneScan list ========================================================================================

>         taskTask       :: (InstZoneScan → ResultDispositions → (InstZoneScan, ResultDispositions))
>                           → ([InstZoneScan], ResultDispositions)
>                           → ([InstZoneScan], ResultDispositions)
>         taskTask fun (zscans, rd)        = foldl' taskFolder ([], rd) zscans
>           where
>             taskFolder :: ([InstZoneScan], ResultDispositions) → InstZoneScan → ([InstZoneScan], ResultDispositions)
>             taskFolder (zscans, rd) zscan
>                                          =
>               let
>                 (zscan', rd')            = fun zscan rd
>               in
>                 if fatalrd [Accepted, NoChange] rd (instKey zscan)
>                   then (zscans ++ [zscan], rd)
>                   else (zscans ++ [zscan'], rd')

groom task ============================================================================================================

>         groomTask      :: ([InstZoneScan], ResultDispositions) → ([InstZoneScan], ResultDispositions)
>         groomTask (zscans, rd)           =
>           let
>             back                         = makeBack preSampleCache (goodZScans (zscans, rd))
>           in
>             taskTask (groomer back) (zscans, rd)
>
>         groomer        :: Map PreSampleKey [PreZoneKey]
>                           → InstZoneScan
>                           → ResultDispositions
>                           → (InstZoneScan, ResultDispositions)
>         groomer back zscan rdGroom_
>           | traceNot trace_G False       = undefined
>           | otherwise                    = (zscan{zsPreZones = newPzs}, rd')
>           where
>             fName                        = unwords [fName_, "groomer"]
>             trace_G                      = unwords [fName, show (rdScans rdGroom_)]
>
>             newPzs                       = groomPreZones zscan.zsPreZones
>
>             pergm                        = instKey zscan
>             sa                           = ScanAlts pergm fName []
>             (scanned, rd')               = scanAlts sa alts rdGroom_
>               where           
>                 alts                     = [curate newPzs (not . null) (violated sa NoZones)]
>
>             groomPreZones preZones       = pzsStereo ++ pzsMono
>               where
>                 (pzsStereo_, pzsMono)    = partition (isStereoZone preSampleCache) preZones
>                 pzsStereo                = map partnerUp pzsStereo_
>
>             partnerUp pz                 =
>               let
>                 mpartners                =
>                   Map.lookup (PreSampleKey pz.pzWordF (F.sampleLink (effShdr preSampleCache pz))) back 
>               in
>                 pz{pzmkPartners = fromMaybe [] mpartners}

vet task ============================================================================================================
          remove bad stereo partners from PreZones per instrument, delete instrument if down to zero PreZones

>         vetTask (zscans, rd)             =
>           let
>             filePzs                      =
>               foldl' (\x y → x ++ filter (isStereoZone preSampleCache) y.zsPreZones) [] (goodZScans (zscans, rd))
>             mapStereo                    = formPreZoneMap filePzs
>           in
>             taskTask (vetter mapStereo) (zscans, rd)
>
>         vetter         :: Map PreZoneKey PreZone
>                           → InstZoneScan
>                           → ResultDispositions
>                           → (InstZoneScan, ResultDispositions)
>         vetter mapStereo zscan rdVet_    = (zscan{zsPreZones = newPzs}, rd')
>           where
>             fName                        = unwords[fName_, "vetter"]
>
>             newPzs                       =
>               let
>                 (pzsStereo, pzsMono)     = partition (isStereoZone preSampleCache) zscan.zsPreZones
>                 vetPreZone pz            =
>                   if null newPartners
>                     then if canDevolveToMono
>                            then Just $ appendChange pz MakeMono
>                            else Nothing
>                     else Just pz{pzmkPartners = newPartners}
>                   where
>                     newPartners          = filter (okPartner mapStereo pz) pz.pzmkPartners
>               in
>                 mapMaybe vetPreZone pzsStereo ++ pzsMono
>
>             pergm                        = instKey zscan
>             sa                           = ScanAlts pergm fName []
>             (scanned, rd')               = scanAlts sa alts rdVet_
>               where           
>                 alts                     = [curate newPzs (not . null) (violated sa NoZones)]
>
>         okPartner pzMap pz pzk           =
>           case Map.lookup pzk pzMap of
>             Nothing                      → False
>             Just pzPartner               → goodPartners pz pzPartner
>
>         goodPartners pzMe pzYou          =
>           let
>             mySPartner                   = PreSampleKey pzMe.pzWordF   (F.sampleLink (effShdr preSampleCache pzMe))
>             yrSPartner                   = PreSampleKey pzYou.pzWordF  (F.sampleLink (effShdr preSampleCache pzYou))
>           in
>             (Just yrSPartner == Map.lookup mySPartner sPartnerMap)
>             && (Just mySPartner == Map.lookup yrSPartner sPartnerMap)

reorg task ============================================================================================================
          group lists of instruments by matching names - if group qualifies, collapse its member insts together as one

> {-
>         reorgTask      :: ([InstZoneScan], ResultDispositions) → ([InstZoneScan], ResultDispositions)
>         reorgTask (zscans, rd)           = foldl' reFolder ([], rd) zscans
>           where
>             fName                        = unwords[fName_, "reorgTask"]
>
>             oMap       :: Map PerGMKey [PreZone]
>             oMap                         =
>               associateZones $ formPreZoneMap $ concatMap (\i → i.zsPreZones) zscans
>             aMap       :: Map Word Word
>             aMap                         =
>               foldl' (\m0 (wL, wsM) → foldl' (\m1 w → Map.insert w wL m1) m0 wsM) Map.empty (dissect (zscans, rdNow))
>             hMap       :: Map Word [PreZone]
>             hMap                         = foldl' makeHolds Map.empty (zscans, rd)
>             
>             reFolder   :: ([InstZoneScan], ResultDispositions)
>                           → InstZoneScan
>                           → ([InstZoneScan], ResultDispositions)
>             reFolder (zscans, rd) zscan
>               | fatalrd rd pergm         = (zscans ++ [zscan],                      rd)
>               | isJust aprobe            = (zscans ++ [zscan],                      rdNew)
>               | otherwise                = (zscans ++ [zscan'],                     rd)
>               where
>                 fName                    = unwords[fName_, "reFolder"]
>
>                 pergm                    = instKey zscan
>                 aprobe                   = Map.lookup zscan.zswInst aMap
>                 hprobe                   = Map.lookup zscan.zswInst hMap
>                 zscan'                   = zscan{zsPreZones = deJust "hprobe" hprobe}
>
>                 rdNew                    = xxdispose rd (instKey zscan) [Scan Violated Absorbed fName ""]
>
>             dissect    :: ([InstZoneScan], ResultDispositions) → [(Word, [Word])]
>             dissect res                  = filter qualifySet groupedC
>               where
>                 frags                    = concatMap enFrag res
>                 fragsets                 = sort $ mapMaybe deFrag frags
>                 groupedA                 = filter (\f → 1 < length f) (groupBy (\x y → fst x == fst y) fragsets)
>                 groupedB                 = map (map snd) groupedA
>                 groupedC                 = map (\g → (head g, g)) groupedB
>             
>             makeHolds  :: Map Word [PreZone] → (InstZoneScan, ResultDispositions) → Map Word [PreZone]
>             makeHolds iHold (zscan, rd)
>               | traceNot trace_MH False  = undefined
>               | otherwise                =
>               if emptyrd rd then exert zscan else iHold
>               where
>                 trace_MH                 = unwords [fName, "iHold", show (length iHold), show (Map.keys iHold)]
>
>                 exert zscan              =
>                   (\case
>                     Just target          → upd target
>                     Nothing              → iHold) macts
>                   where
>                     macts                = Map.lookup zscan.zswInst aMap
>                     rebased              = map rebase zscan.zsPreZones
>                     rebase pz            =
>                       (\case
>                         Just owner       → pz{pzWordI = owner}
>                         Nothing          → pz) macts
>
>                     upd target
>                       | traceNot trace_U False
>                                          = undefined
>                       | otherwise        = Map.insertWith (++) target rebased iHold
>                       where
>                         trace_U          = unwords [fName, "upd from/to", show (zscan.zswInst, target), show (length rebased)]
>
>
>             enFrag     :: (InstZoneScan, ResultDispositions) → [(String, Word)]
>             enFrag (zscan, _)            =
>               let
>                 preI                     =
>                   deJust (unwords[ fName, "preI"]) (Map.lookup (instKey zscan) preInstCache)
>               in
>                 singleton (preI.iName, zscan.zswInst)
>
>             deFrag     :: (String, Word) → Maybe (String, Word)
>             deFrag (str, w)              =
>               let
>                 str', okstr, str''
>                        :: String
>                 str'                     = shorten isDigit str
>                 okstr                    = "_-"
>                 str''                    = shorten (\c → isSpace c || c `elem` okstr) str'
>               in
>                 if str /= str'
>                   then Just (str'', w)
>                   else Nothing
>
>             qualifySet :: (Word, [Word]) → Bool
>             qualifySet (leadI, subIs)
>               | traceNot trace_QS False  = undefined
>               | otherwise                = answer
>               where
>                 fName                    = "qualifySet"
>
>                 nInsts :: Double         = fromIntegral $ genericLength subIs
>                 pergms                   = map (\wI → PerGMKey sffile.zWordF wI Nothing) subIs
>                 smashups                 = map smash pergms
>                 pzLoads                  = map (\p → deJust "pzs" (Map.lookup p oMap)) pergms
>                 pears                    = zip pzLoads smashups
>                 pzsAll                   = concat pzLoads
>                 fracOne                  = 100 * fromRational ((maximum . map calcFrac) pears)
>                 calcFrac (pzs, smashup)  = fractionCovered smashup
>                 smashAll                 = smush (zip pzLoads smashups)
>                 fracAll                  = 100 * fromRational (fractionCovered smashAll)
>                 answer                   = fracAll / fracOne > 1.25
>
>                 smash pergm              = computeInstSmashup $ deJust "pzs" (Map.lookup pergm oMap)
>
>                 trace_QS                 =
>                   unwords [fName, show leadI, show (fracOne, fracAll)
>                          , show "...."
>                          , show answer, show (fracAll / fracOne)]
> -}
>
> smush                  :: [([PreZone], Smashing Word)] → Smashing Word
> smush pears                              = smashSubspaces allTags dims allSpaces
>   where
>     allTags            :: String
>     allSpaces          :: [(Word, [Maybe (Word, Word)])]
>     (allTags, allSpaces)                 =
>       foldl' (\(at, as) (pzs, smashup) → (at ++ smashup.smashTag, as ++ map extractSpace pzs)) ([], []) pears
>     dims                                 = [fromIntegral qMidiSize128, fromIntegral qMidiSize128]
>
> shorten        :: (Char → Bool) → [Char] → [Char] 
> shorten qual chars                       = reverse (dropWhile qual (reverse chars))
>
> makeBack               :: Map PreSampleKey PreSample → [InstZoneScan] → Map PreSampleKey [PreZoneKey]
> makeBack preSampleCache zscans           = foldl' Map.union Map.empty (map zscan2back zscans)
>   where
>     zscan2back zscan                     = foldl' backFolder Map.empty (filter (isStereoZone preSampleCache) zscan.zsPreZones)
>     backFolder target pz                 =
>       Map.insertWith (++) (PreSampleKey pz.pzWordF pz.pzWordS) [extractZoneKey pz] target
>
> markGlobalZone         :: Map PerGMKey PreInstrument → InstZoneScan → Map PerGMKey PreInstrument
> markGlobalZone preic zscan
>   | traceNot trace_MGZ False             = undefined
>   | otherwise                            =
>   if isNothing zscan.zswGBix || isNothing moldpreI
>     then preic
>     else Map.insert pergm oldpreI{iGlobalKey = Just $ PreZoneKey zscan.zswFile (fromJust zscan.zswGBix)} preic
>   where
>     pergm                                = instKey zscan
>     moldpreI                             = Map.lookup pergm preic
>     oldpreI                              = deJust (unwords["mold", show pergm]) moldpreI
>     trace_MGZ                            = unwords ["markGlobalZone", show zscan.zswGBix, show pergm]
>
> formZPartnerCache      :: Map PreSampleKey PreSample → Map PerGMKey PerInstrument → Map PreZoneKey PreZone → Map PreZoneKey SFZone
> formZPartnerCache preSampleCache perIs preZoneCache_
>                                          = Map.mapMaybe chaseIt preZoneCache
>   where
>     preZoneCache                         = Map.filter (isStereoZone preSampleCache) preZoneCache_
>
>     chaseIt            :: PreZone → Maybe SFZone
>     chaseIt pz                           =
>       computeCross perIs preZoneCache pz.pzWordS (extractZoneKey pz)
> 
> computeCross           :: Map PerGMKey PerInstrument → Map PreZoneKey PreZone → Word → PreZoneKey → Maybe SFZone
> computeCross perIs preZs si pzk
>   | traceAlways trace_CC False           = undefined
>   | otherwise                            =
>   Just pzk
>   >>= (`Map.lookup` preZs)
>   >>= Just . extractInstKey
>   >>= (`Map.lookup` perIs)
>   >>= Just . pZones
>   >>= Just . map snd
>   >>= (`findBySampleIndex` si)
>   where
>     trace_CC                             = unwords ["computeCross", show pzk]
>
> formPreInstCache       :: Array Word SFFile
>                           → ResultDispositions
>                           → IO (Map PerGMKey PreInstrument, ResultDispositions)
> formPreInstCache sffiles rd              = CM.foldM preIFolder (Map.empty, rd) pergms
>   where
>     fName                                = "formPreInstCache"
>     pergms                               = formComprehension sffiles ssInsts
>
>     preIFolder         :: (Map PerGMKey PreInstrument, ResultDispositions)
>                           → PerGMKey
>                           → IO (Map PerGMKey PreInstrument, ResultDispositions)
>     preIFolder (m, rd) pergm@PerGMKey{ .. }
>                                          =
>       return $
>         if iinst.instBagNdx < jinst.instBagNdx
>           then (m', rd')
>           else error $ unwords [fName, "corrupt instBagNdx"]
> {-
>       | goodName nm                      =
>         (  Map.insert pergm (PreInstrument iinst nm (computeFFMatches nm) Nothing) m
>           , xxdispose rd pergm (accept Ok fName ""))
>       | fixBadNames                      =
>         (  Map.insert pergm (PreInstrument iinst fixednm (computeFFMatches fixednm) Nothing) m
>          , xxdispose rd pergm [viol, resc])
>       | otherwise                        =
>         (  m
>          , xxdispose rd pergm [viol])
> -}
>       where
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pgkwInst + 1}
>
>         nm                               = iinst.instName
>         fixednm                          = fixName iinst.instName -- WOX do fix names
>
>         m'                               =
>           if cancels [Accepted] ss
>              then m 
>              else Map.insert pergm (PreInstrument iinst nm (computeFFMatches nm) Nothing) m
>
>         sa                               = ScanAlts pergm fName []
>         (ss, rd')                        = scanAlts sa alts rd
>           where
>             alts                         =
>               [curate iinst.instName goodName (violated sa CorruptName)]
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm                       = boota.ssInsts ! pergm.pgkwInst
>       where
>         boota                            = (sffiles ! pergm.pgkwFile).zBoot
>
> sortByCategory         :: Map PerGMKey PreInstrument
>                           → Map PerGMKey InstCat
>                           → IO ([PerGMKey], [PerGMKey])
> sortByCategory preInstCache jobs         = return $ Map.foldlWithKey catFolder ([], []) jobs
>   where
>     catFolder            :: ([PerGMKey], [PerGMKey]) → PerGMKey → InstCat → ([PerGMKey], [PerGMKey])
>     catFolder (pergmsI, pergmsP) pergmI_ icat
>                                          =
>       let
>         pergmI                           = pergmI_{pgkwBag = Nothing}
>       in
>         case icat of
>           InstCatPerc icd                → (pergmsI, pergmsP ++ instrumentPercList pergmI icd.inPercBixen)
>           InstCatInst _                  → (pergmI : pergmsI, pergmsP)
>           _                              → (pergmsI, pergmsP)
>
>     instrumentPercList :: PerGMKey → [Word] → [PerGMKey]
>     instrumentPercList pergmI            = map (\w → pergmI {pgkwBag = Just w})
>
> openSoundFontFile      :: Word → FilePath → IO SFFile
> openSoundFontFile wFile filename = do
>   putStr (show wFile ++ " " ++ filename)
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
>       let nBits                          =
>             case samplea.ssM24 of
>               Nothing                    → 16
>               Just s24data               → 24
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
>       putStrLn (" (" ++ show nBits ++ ") loaded in " ++ show (diffUTCTime ts2 ts1))
>       return sffile
>     
> renderSong             :: ∀ p . Clock p ⇒
>                           SFRoster
>                           → InstrMap (Stereo p)
>                           → FilePath
>                           → (DynMap → Music (Pitch, [NoteAttribute]))
>                           → IO ()
> renderSong sfrost imap name song =
>   do
>     traceIO ("renderSong " ++ name)
>     ts1                                  ← getCurrentTime
>     ding                                 ← shredMusic (song Map.empty)
>     let dynMap                           = makeDynMap ding
>     CM.unless (null (Map.assocs dynMap)) (traceIO $ unwords ["dynMap", show dynMap])
>     let ks                               = Map.keys ding.shRanges
>     let (is, ps)                         = (map (\i → fromMaybe i (Map.lookup i dynMap)) (lefts ks), rights ks)
>     let (esI, esP)                       = printChoices sfrost is ding.shMsgs ps
>     let es                               = [Unblocked name, EndOfLine] ++ concatMap snd esI ++ concatMap snd esP
>     putStr (reapEmissions es)
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let path                         = name ++ ".wav"
>         putStr path
>         let (d,s)                        = renderSF (song dynMap) imap
>         traceIO (unwords ["-> outFile*", path, show d])
>         if normalizingOutput
>           then outFileNorm path d s
>           else outFile     path d s
>         traceIO (unwords ["<- outFile*", path, show d])
>         ts2                              ← getCurrentTime
>         putStrLn (" (dur=" ++ show d ++ ") written in " ++ show (diffUTCTime ts2 ts1))
>       else
>         putStrLn "skipping..."
>     return ()
>
> sampleSizeOk           :: (Word, Word) → Bool
> sampleSizeOk (st, en)                    = st >= 0 && en - st >= 0 && en - st < 2^22
> sampleLoopSizeOk       :: (Word, Word, A.SampleMode) → Bool
> sampleLoopSizeOk (stl, enl, smode)       = 
>   A.NoLoop == smode || (stl >= 0 && enl - stl >= sampleSizeMin && enl - stl < 2 ^ 22)
>
> adjustedSampleSizeOk   :: ZoneDigest → F.Shdr → Bool
> adjustedSampleSizeOk zd shdr             = 0 <= st && st <= en && 0 <= stl && stl <= enl
>   where
>     st                                   = shdr.start     + fromIntegral zd.zdStart
>     en                                   = shdr.end       + fromIntegral zd.zdEnd
>     stl                                  = shdr.startLoop + fromIntegral zd.zdStartLoop
>     enl                                  = shdr.endLoop   + fromIntegral zd.zdEndLoop

Note that harsher consequences of unacceptable sample header are enforced earlier. Logically, that would be
sufficient to protect below code from bad data and document the situation. But ... mechanism such as putting
out diagnostics might cause us to execute this code first. So, being crash-free/minimal in isStereoZone et al.

> isStereoInst, is24BitInst
>                        :: Map PreSampleKey PreSample → [(PreZone, SFZone)] → Bool
>
> isStereoInst preSampleCache zs           = isJust $ find (isStereoZone preSampleCache) (map fst zs)
>       
> isStereoZone preSampleCache pz           = isLeftPreZone preSampleCache pz || isRightPreZone preSampleCache pz
>
> -- see also zfindByBagIndex
>
> isLeftPreZone preSampleCache pz          = SampleTypeLeft == toSampleType (effShdr preSampleCache pz).sampleType
> isRightPreZone preSampleCache pz         = SampleTypeRight == toSampleType (effShdr preSampleCache pz).sampleType
>
> zoneConforms preSampleCache (pz, zone)   = not $ or unsupported
>   where
>     F.Shdr{end, start, endLoop, startLoop}
>                                          = effShdr preSampleCache pz
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
>         , end < start
>         , endLoop < startLoop
>       ]
>
> is24BitInst _ _                   = True -- was isJust $ ssM24 arrays       
 
extract data from SoundFont per instrument ============================================================================

> addGen                 :: SFZone → F.Generator → SFZone
> addGen iz gen =
>   case gen of
>   F.StartAddressOffset i         → iz {zStartOffs =                Just i}
>   F.EndAddressOffset i           → iz {zEndOffs =                  Just i}
>   F.LoopStartAddressOffset i     → iz {zLoopStartOffs =            Just i}
>   F.LoopEndAddressOffset i       → iz {zLoopEndOffs =              Just i}
>
>   F.StartAddressCoarseOffset i   → iz {zStartCoarseOffs =          Just i}
>   F.EndAddressCoarseOffset i     → iz {zEndCoarseOffs =            Just i}
>   F.LoopStartAddressCoarseOffset i
>                                  → iz {zLoopStartCoarseOffs =      Just i}
>   F.LoopEndAddressCoarseOffset i
>                                  → iz {zLoopEndCoarseOffs =        Just i}
>
>   F.InstIndex w                  → iz {zInstIndex =                Just w}
>   F.KeyRange a b                 → iz {zKeyRange =                 Just (fromIntegral a, fromIntegral b)}
>   F.VelRange a b                 → iz {zVelRange =                 Just (fromIntegral a, fromIntegral b)}
>   F.Key i                        → iz {zKey =                      Just i}
>   F.Vel i                        → iz {zVel =                      Just i}
>   F.InitAtten i                  → iz {zInitAtten =                Just i}
>   F.CoarseTune i                 → iz {zCoarseTune =               Just i}
>   F.FineTune i                   → iz {zFineTune =                 Just i}
>   F.SampleIndex w                → iz {zSampleIndex =              Just w}
>   F.SampleMode a                 → iz {zSampleMode =               Just a}
>   F.ScaleTuning i                → iz {zScaleTuning =              Just i}
>   F.ExclusiveClass i             → iz {zExclusiveClass =           Just i}
>
>   F.DelayVolEnv i                → iz {zDelayVolEnv =              Just i}
>   F.AttackVolEnv i               → iz {zAttackVolEnv =             Just i}
>   F.HoldVolEnv i                 → iz {zHoldVolEnv =               Just i}
>   F.DecayVolEnv i                → iz {zDecayVolEnv =              Just i}
>   F.SustainVolEnv i              → iz {zSustainVolEnv =            Just i}
>   F.ReleaseVolEnv i              → iz {zReleaseVolEnv =            Just i}
>
>   F.Chorus i                     → iz {zChorus =                   Just i}
>   F.Reverb i                     → iz {zReverb =                   Just i}
>   F.Pan i                        → iz {zPan =                      Just i}
>
>   F.RootKey w                    → iz {zRootKey =                  Just (fromIntegral w)}
>
>   F.ModLfoToPitch i              → iz {zModLfoToPitch =            Just i}
>   F.VibLfoToPitch i              → iz {zVibLfoToPitch =            Just i}
>   F.ModEnvToPitch i              → iz {zModEnvToPitch =            Just i}
>   F.InitFc i                     → iz {zInitFc =                   Just i}
>   F.InitQ i                      → iz {zInitQ =                    Just i}
>   F.ModLfoToFc i                 → iz {zModLfoToFc =               Just i}
>   F.ModEnvToFc i                 → iz {zModEnvToFc =               Just i}
>   F.ModLfoToVol i                → iz {zModLfoToVol =              Just i}
>   F.DelayModLfo i                → iz {zDelayModLfo =              Just i}
>   F.FreqModLfo i                 → iz {zFreqModLfo =               Just i}
>   F.DelayVibLfo i                → iz {zDelayVibLfo =              Just i}
>   F.FreqVibLfo i                 → iz {zFreqVibLfo =               Just i}
>   F.DelayModEnv i                → iz {zDelayModEnv =              Just i}
>   F.AttackModEnv i               → iz {zAttackModEnv =             Just i}
>   F.HoldModEnv i                 → iz {zHoldModEnv =               Just i}
>   F.DecayModEnv i                → iz {zDecayModEnv =              Just i}
>   F.SustainModEnv i              → iz {zSustainModEnv =            Just i}
>   F.ReleaseModEnv i              → iz {zReleaseModEnv =            Just i}
>   F.KeyToModEnvHold i            → iz {zKeyToModEnvHold =          Just i}
>   F.KeyToModEnvDecay i           → iz {zKeyToModEnvDecay =         Just i}
>   F.KeyToVolEnvHold i            → iz {zKeyToVolEnvHold =          Just i}
>   F.KeyToVolEnvDecay i           → iz {zKeyToVolEnvDecay =         Just i}
>   _                              → iz
>
> addMod                 :: (Word, F.Mod) → SFZone → SFZone
> addMod (mId, F.Mod{srcOper, destOper, amtSrcOper, amount}) iz@SFZone{zModulators} 
>                                          = maybe iz addModulator makeModulator
>   where
>     addModulator       :: Modulator → SFZone
>     addModulator mod                     = iz{zModulators = mod : zModulators}
>
>     makeModulator      :: Maybe Modulator
>     makeModulator                        = mm'
>       where
>         mm, mm'        :: Maybe Modulator
>         mm                               = unpackModSrc srcOper
>                                            >>= flip addSrc defModulator{mrModId = mId}
>                                            >>= addDest destOper
>                                            >>= addAmount (fromIntegral amount)
>         mm'                              = unpackModSrc amtSrcOper
>                                            >>= addAmtSrc mm
                                            
prepare the specified instruments and percussion ======================================================================

> categorize             :: Array Word SFFile
>                           → Map PreSampleKey PreSample
>                           → Map PerGMKey PreInstrument
>                           → Map PerGMKey [PreZone] 
>                           → ([InstrumentName], [PercussionSound])
>                           → IO (Map PerGMKey InstCat)
> categorize sffiles preSampleCache preInstCache owners rost
>                                          = return $ Map.mapWithKey categorizeInst preInstCache
>   where
>     categorizeInst     :: PerGMKey → a → InstCat
>     categorizeInst pergm _
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = deJust (unwords[fName, "icat"]) icat
>       where
>         fName                            = "categorizeInst"
>         trace_CI                         =
>           unwords [fName, preI.iName, show (pergm.pgkwFile, pergm.pgkwInst)]
>
>         preI                             =
>           deJust (unwords [fName, "PreInstrument"]) (Map.lookup pergm preInstCache)
>         mpzs                             = Map.lookup pergm owners
>         pzs                              = deJust (unwords[fName, "owners"]) mpzs
>
>         -- Put the Instrument, of either category, through a gauntlet of checks.
>         -- This diverges, and then we have qualInstZone and qualPercZone 
>
>         (pzsStereo, pzsMono)             = partition (isStereoZone preSampleCache) pzs
>         (pzsOutbound, pzsLocal)          = partition hasCross pzsStereo
>         pzsLessLocalRights               = pzsMono ++ pzsOutbound ++ filter (isLeftPreZone preSampleCache) pzsLocal
>
>         -- Determine which category will belong to the Instrument, based on its performance for
>         -- 1. all kinds
>         -- 2. "rost" subset, could be same as 1.
>
>         icatAllKinds, icatRost, icatNarrow, icat
>                        :: Maybe InstCat
>         icatAllKinds                     = foldl' CM.mplus Nothing (provideAlts Nothing allKinds)
>         icatRost                         = foldl' CM.mplus Nothing (provideAlts icatAllKinds rost)
>         icatNarrow                       = Just (InstCatDisq (dropped sa Narrow ""))
>         sa                               = ScanAlts pergm fName []
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
>           if isSafe sList || requiredZoneLinkage < 1 then Nothing else Just $ InstCatDisq (violated sa BadLinkage "")
>           where
>             sList      :: [(Int, Int)]
>             sList                        = map (\z → (extractIndex z, extractLink z)) pzsLocal
>
>         isSafe         :: ∀ a . (Eq a, Ord a, Show a) ⇒ [(a,a)] → Bool
>         isSafe pairs                     = closed && allPaired
>           where
>             uniquer    :: Map a a
>             uniquer                      =
>               foldl' (\target (f, t) → Map.insert f t target) Map.empty pairs
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
>           if any hasCross pzs then Just $ InstCatDisq (violated sa IllegalCrossover "") else Nothing
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
>         maybeSettle thresh icat keys     = find (> thresh) keys >> Just icat
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
>
>           where
>             fName                        = "provideAlts"
>             trace_PA                     =
>               unwords [fName, showMaybeInstCat seed, show (length pzs), show (BF.bimap length length srost)]
>
>             structuralAlts               =
>               [if isNothing mpzs || null (fromJust mpzs)
>                  then Just $ InstCatDisq (violated sa NoZones "")
>                  else Nothing
>               , corrupt
>               , if any hasRom pzs then Just $ InstCatDisq (violated sa RomBased "") else Nothing
>               , if allowStereoCrossovers
>                   then Nothing
>                   else rejectCrosses
>               , checkLinkage
>               ]
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
>                   , maybeSettle stands      (catDisq (dropped sa Narrow ""))     preI.iMatches.ffInst
>
>                   , maybeNailAsPerc 0.3
>                   , if genericScore > 0 then Just catInst            else Nothing
>                   , if genericScore < 0 then Just (catPerc wZones)   else Nothing
>                   , Just $ catDisq (dropped sa Unrecognized "")
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
>                          else Just (catDisq (violated sa NoPercZones "")))
>                     else Nothing
>
>                 fName                    = "functionalAlts"
>                 trace_FA = unwords [fName, preI.iName, show frost, show (length uZones, length wZones)]
>
>             catInst      :: InstCat      =
>               if null pzs
>                 then InstCatDisq (violated sa NoZones "")
>                 else (case checkSmashing pergm smashup of
>                        Left _            → InstCatInst icd
>                        Right reason      → InstCatDisq reason)
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
>                 else (case eith of
>                        Left _            → InstCatPerc icd
>                        Right reason      → InstCatDisq reason)
>               where
>                 fName                    = "catPerc"
>                 trace_CP                 = unwords [fName, show (length ws, length pzs)]
>
>                 pzs'                     = filter (\x → x.pzWordB `elem` ws) pzs
>                 smashup                  = computeInstSmashup pzs'
>                 icd                      = InstCatData pzs' smashup ws
>                 eith                     = checkSmashing pergm smashup
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
> formZoneCache          :: Array Word SFFile
>                           → Map PerGMKey PreInstrument
>                           → ([InstrumentName], [PercussionSound])
>                           → Map PerGMKey InstCat
>                           → ResultDispositions
>                           → IO (Map PerGMKey PerInstrument, ResultDispositions)
> formZoneCache sffiles preInstCache rost jobs rd_
>                                          =
>   return $ Map.foldlWithKey formFolder (Map.empty, rd_) jobs
>   where
>     fName                                = "formZoneCache"
>
>     formFolder         :: (Map PerGMKey PerInstrument, ResultDispositions)
>                           → PerGMKey → InstCat
>                           → (Map PerGMKey PerInstrument, ResultDispositions)
>     formFolder (zc, rd) pergm icat       = (Map.insert pergm (computePerInst pergm icat) zc, rd)
>
>     computePerInst     :: PerGMKey → InstCat → PerInstrument
>     computePerInst pergm icat
>       | traceIf trace_CPI False          = undefined
>       | otherwise                        = PerInstrument (zip pzs oList) icd.inSmashup
>       where
>         sffile                           = sffiles ! pergm.pgkwFile
>         preI                             = deJust "computePerInst PreInstrument" (Map.lookup pergm preInstCache)
>
>         icd            :: InstCatData
>         bixen          :: [Word]
>
>         (icd, bixen)                     =
>           case icat of
>             InstCatPerc x                → (x, x.inPercBixen)
>             InstCatInst x                → (x, map pzWordB x.inPreZones)
>             _                            → error $ unwords ["formZoneCache", "only Inst and Perc are valid here"]
>
>         gZone                            =
>           case preI.iGlobalKey of
>             Nothing                      → defZone
>             Just pzk                     → buildZone sffile defZone pzk.pzkwBag
>         oList                            = map (buildZone sffile gZone) bixen
>
>         pzs                              = filter (\pz → pz.pzWordB `elem` bixen) icd.inPreZones
>
>         trace_CPI                        =
>           unwords ["computePerInst", show pergm.pgkwFile, preI.iName, show (length oList)]
>
> checkSmashing          :: PerGMKey → Smashing Word → Either Bool [Scan]
> checkSmashing pergm smashup
>   | not ok1                              = Right $ violated sa UndercoveredRanges ""
>   | not ok2                              = Right $ violated sa OverCoveredRanges ""
>   | otherwise                            = Left True
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
> buildZone              :: SFFile → SFZone → Word → SFZone
> buildZone sffile fromZone bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = zone
>   where
>     zone                                 = foldr addMod (foldl' addGen fromZone gens) mods
>     boota                                = sffile.zBoot
>
>     xgeni                                = F.genNdx $ boota.ssIBags!bagIndex
>     ygeni                                = F.genNdx $ boota.ssIBags!(bagIndex + 1)
>     xmodi                                = F.modNdx $ boota.ssIBags!bagIndex
>     ymodi                                = F.modNdx $ boota.ssIBags!(bagIndex + 1)
>
>     gens               :: [F.Generator]  =
>       profess
>         (xgeni <= ygeni)
>         (unwords["SoundFont file", show sffile.zWordF, sffile.zFilename, "corrupt (buildZone gens)"])
>         (map (boota.ssIGens !) (deriveRange xgeni ygeni))
>     mods               :: [(Word, F.Mod)]
>                                          =
>       profess
>         (xmodi <= ymodi)
>         (unwords["SoundFont file", show sffile.zWordF, sffile.zFilename, "corrupt (buildZone mods)"])
>         (zip [10_000..] (map (boota.ssIMods !) (deriveRange xmodi ymodi)))
>
>     trace_BZ                             =
>       unwords ["buildZone", show sffile.zWordF, show bagIndex, show zone.zSampleIndex
>              , show (fromMaybe "" name), show (fromZone == defZone)]
>
>     name               :: Maybe String   =
>       zone.zSampleIndex >>= \x → Just (boota.ssShdrs ! x) >>= Just . F.sampleName

define signal functions and instrument maps to support rendering ======================================================

> prepareInstruments     :: SFRoster → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments sfrost@SFRoster{zWinningRecord}
>                                          = 
>     return $ (Percussion, assignPercussion pmap)                                                 : imap
>   where
>     WinningRecord{pWinningI, pWinningP}  = zWinningRecord
>     imap                                 = Map.foldrWithKey imapFolder [] pWinningI
>     pmap                                 = Map.foldrWithKey pmapFolder [] pWinningP
>
>     imapFolder kind PerGMScored{pPerGMKey} target
>                                          = (kind, assignInstrument pPerGMKey)                    : target
>
>     pmapFolder kind PerGMScored{pPerGMKey} target
>                                          = (kind, (pgkwFile pPerGMKey, pgkwInst pPerGMKey))      : target
>
>     assignInstrument   :: ∀ p . Clock p ⇒ PerGMKey → Instr (Stereo p)
>     assignInstrument pergm dur pch vol params
>                                          =
>       proc _ → do
>         (zL, zR)                         ← instrumentSF sfrost pergm dur pch vol params ⤙ ()
>         outA                             ⤙ (zL, zR)
>
>     assignPercussion   :: ∀ p . Clock p ⇒ [(PercussionSound, (Word, Word))] → Instr (Stereo p)
>     assignPercussion pmap dur pch vol params
>                                          = assignInstrument pergm dur pch vol params
>       where
>         pergm                            = PerGMKey wF wI Nothing
>         (wF, wI)                         =
>           case lookup kind pmap of
>             Nothing    → error ("Percussion does not have " ++ show kind ++ " in the supplied pmap.")
>             Just x     → x
>         kind           :: PercussionSound
>                                          = toEnum (pch - 35)
>
> instrumentSF           :: ∀ p . Clock p ⇒
>                           SFRoster
>                           → PerGMKey
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → Signal p () (Double, Double)
> instrumentSF sfrost pergm dur pchIn volIn nps
>   | traceNot trace_ISF False             = undefined
>   | otherwise                            = eutSynthesize (reconX, mreconX) reconX.rSampleRate
>                                              dur pchOut volOut nps
>                                              samplea.ssData samplea.ssM24
>   where
>     fName                                = "instrumentSF"
>     noon                                 = NoteOn
>                                              (clip (0, 127) volIn)
>                                              (clip (0, 127) pchIn)
>     pchOut              :: AbsPitch      = maybe noon.noteOnKey (clip (0, 127)) reconX.rForceKey
>     volOut              :: Volume        = maybe noon.noteOnVel (clip (0, 127)) reconX.rForceVel
>
>     sffile                               = sfrost.zFiles ! pergm.pgkwFile
>     samplea                              = sffile.zSample
>
>     preI                                 = deJust (unwords [fName, "preI"]) (Map.lookup pergm sfrost.zPreInstCache)
>     perI                                 = deJust (unwords [fName, "perI"]) (Map.lookup pergm sfrost.zPerInstCache)
>
>     trace_ISF                            =
>       unwords [fName, show pergm.pgkwFile, show preI.pInst, show (pchIn, volIn), show dur]
>
>     (reconX, mreconX)                    =
>       case setZone of
>         Left zplus                       → (recon zplus noon nps (fromRational dur), Nothing)
>         Right zsPlus                     → reconLR zsPlus noon nps (fromRational dur)

zone selection for rendering ==========================================================================================

>     setZone            :: Either (SFZone, F.Shdr) ((SFZone, F.Shdr), (SFZone, F.Shdr))
>     setZone                              =
>       case selectZoneConfig (selectBestZone noon) of 
>         Left (pzL, zoneL)                → Left (zoneL, shdr pzL)
>         Right ((pzL, zoneL), (pzR, zoneR))
>                                          → Right ((zoneL, shdr pzL), (zoneR, shdr pzR))
>       where
>         shdr                             = effShdr sfrost.zPreSampleCache
>
>     selectBestZone     :: NoteOn → (PreZone, SFZone)
>     selectBestZone noon
>       | traceNot trace_SBZ False         = undefined
>       | otherwise                        =
>       if count == 0
>         then error "out of range"
>         else if isNothing foundInInst
>                then error $ unwords[fName, show bagId, "not found in inst", preI.iName, showBags perI]
>                else deJust "foundInInst" foundInInst
>       where
>         (bagId, count)                   = lookupCellIndex (noonAsCoords noon) perI.pSmashing
>         foundInInst                      = findByBagIndex' perI.pZones bagId
>
>         trace_SBZ                        = unwords ["selectBestZone", show (bagId, count)]
>
>     selectZoneConfig   :: (PreZone, SFZone) → Either (PreZone, SFZone) ((PreZone, SFZone), (PreZone, SFZone))
>     selectZoneConfig z
>        | stype == SampleTypeLeft         = qualify (Right (z, oz))
>        | stype == SampleTypeRight        = qualify (Right (oz, z))
>        | otherwise                       = Left z
>        where
>          qualify       :: Either (PreZone, SFZone) ((PreZone, SFZone), (PreZone, SFZone))
>                           → Either (PreZone, SFZone) ((PreZone, SFZone), (PreZone, SFZone))
>          qualify eith                    = if isNothing mpartner
>                                              then Left z
>                                              else eith
>
>          mpartner                        = getStereoPartner z
>          shdr                            = (effShdr sfrost.zPreSampleCache . fst) z
>          stype                           = toSampleType shdr.sampleType
>          oz                              = deJust "oz" mpartner
>
>     getStereoPartner   :: (PreZone, SFZone) → Maybe (PreZone, SFZone)
>     getStereoPartner z
>       | traceNot trace_GSP False         = undefined
>       | otherwise                        =
>       case toSampleType (F.sampleType shdr) of
>         SampleTypeLeft                   → partner
>         SampleTypeRight                  → partner
>         _                                → error $ unwords [fName, "attempted on non-stereo zone"]
>       where
>         fName                            = "getStereoPartner"
>
>         shdr                             = (effShdr sfrost.zPreSampleCache . fst) z
>         partnerKeys                      = (pzmkPartners . fst) z
>
>         trace_GSP                        = unwords [fName, showable, showPreZones (singleton $ fst z)]
>         showable                         =
>           case partner of
>             Just (pz, _)                 → show pz.pzWordB
>             Nothing                      → "Nothing" 
>
>         -- maybe hide this hack of getting zone "directly" under a conditional
>         partner                          =
>           findBySampleIndex' perI.pZones (F.sampleLink shdr) `CM.mplus` getCrossover
>
>         getCrossover   :: Maybe (PreZone, SFZone)
>         getCrossover                     = if allowStereoCrossovers && not (null cands)
>                                              then Just (head cands)
>                                              else Nothing
>           where
>             cands                        = mapMaybe evalCand partnerKeys
>
>             evalCand   :: PreZoneKey → Maybe (PreZone, SFZone)
>             evalCand pzk                 = Nothing -- WOX
> {-
>               let
>                 pz                       = pzk `Map.lookup` sfrost.zPreZoneCache
>                 zone                     = pzk `Map.lookup` sfrost.zPartnerCache
>               in
>                 if isJust pz && isJust zone
>                   then Just (fromJust pz, fromJust zone)
>                   else Nothing
> -}

reconcile zone and sample header ======================================================================================

> reconLR                :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                           → NoteOn
>                           → [Double]
>                           → Dur
>                           → (Recon, Maybe Recon)
> reconLR ((zoneL, shdrL), (zoneR, shdrR)) noon nps dur
>   | traceNever trace_RLR False           = undefined
>   | otherwise                            = (recL, Just recR')
>   where
>     secsScored         :: Double         = fromRational dur
>     recL@Recon{rRootKey = rkL, rPitchCorrection = pcL}
>                                          = recon (zoneL, shdrL) noon nps secsScored
>     recR                                 = recon (zoneR, shdrR) noon nps secsScored
>     recR'                                = recR{
>                                                rRootKey                   = rkL
>                                              , rPitchCorrection           = pcL}
>
>     trace_RLR                            = unwords ["reconLR:\n", show zoneL, "\n", show shdrL]
>
> recon                  :: (SFZone, F.Shdr) → NoteOn → [Double] → Double → Recon
> recon (zone_, sHdr@F.Shdr{ .. }) noon nps secsScored
>                                          = reconL
>   where
>     zone@SFZone{ .. }                    =
>       (\case
>         Just np                          → applyNoteParameter noon zone_ np secsScored
>         Nothing                          → zone_) (listToMaybe nps)
>     m8n                                  = reconModulation zone sHdr noon nps secsScored
>
>     reconL = Recon {
>     rSampleMode    = fromMaybe           A.NoLoop           zSampleMode
>   , rSampleRate    = fromIntegral        sampleRate
>   , rStart         = addIntToWord        start              (sumOfWeightedInts
>                                                               [zStartOffs,     zStartCoarseOffs]     qOffsetWeights)
>   , rEnd           = addIntToWord        end                (sumOfWeightedInts
>                                                               [zEndOffs,       zEndCoarseOffs]       qOffsetWeights)
>   , rLoopStart     = addIntToWord        startLoop          (sumOfWeightedInts
>                                                               [zLoopStartOffs, zLoopStartCoarseOffs] qOffsetWeights)
>   , rLoopEnd       = addIntToWord        endLoop            (sumOfWeightedInts
>                                                               [zLoopEndOffs,   zLoopEndCoarseOffs]   qOffsetWeights)
>   , rRootKey       = fromIntegral $ fromMaybe
>                                          originalPitch      zRootKey
>   , rForceKey      = fmap                fromIntegral       zKey
>   , rForceVel      = fmap                fromIntegral       zVel
>   , rTuning        = fromMaybe           100                zScaleTuning
>   , rNoteOn        = noon
>   , rAttenuation   = reconAttenuation                       zInitAtten
>   , rVolEnv        = deriveEnvelope                         zDelayVolEnv
>                                                             zAttackVolEnv
>                                                             noon
>                                                             nps
>                                                             (zHoldVolEnv,  zKeyToVolEnvHold)
>                                                             (zDecayVolEnv, zKeyToVolEnvDecay)
>                                                             zSustainVolEnv
>                                                             zReleaseVolEnv
>                                                             Nothing
>   , rPitchCorrection
>                    = if usePitchCorrection
>                        then Just $ reconPitchCorrection     pitchCorrection
>                                                             zCoarseTune
>                                                             zFineTune
>                        else Nothing
>
>   , rM8n           =                                        m8n
>   , rEffects       = deriveEffects                          m8n
>                                                             noon
>                                                             zChorus
>                                                             zReverb
>                                                             zPan}
>
>     reconPitchCorrection
>                        :: Int → Maybe Int → Maybe Int → Double
>     reconPitchCorrection sub mps mpc     = fromMaybe ((fromCents . fromIntegral) sub) (fromCents' mps mpc)
>
>     reconAttenuation   :: Maybe Int → Double
>     reconAttenuation matten              = if useAttenuation
>                                              then maybe 0 fromIntegral zInitAtten
>                                              else 0.0
>
> applyNoteParameter     :: NoteOn → SFZone → Double → Double → SFZone
> applyNoteParameter noon zone bend secs   = zone'
>   where
>     zone'                                =
>       zone{  zModEnvToPitch = (Just . round) (bend * 100)
>            , zDelayModEnv   = Nothing
>            , zAttackModEnv  = Just $ toTimecents secs - 1
>            , zHoldModEnv    = Nothing
>            , zDecayModEnv   = Nothing
>            , zSustainModEnv = Just 0
>            , zKey           = (Just . fromIntegral) noon.noteOnKey
>            , zReleaseModEnv = Nothing}
>
> reconModulation        :: SFZone → F.Shdr → NoteOn → [Double] → Double → Modulation
> reconModulation SFZone{ .. } shdr noon nps secsScored
>   | traceIf trace_RM False               = undefined
>   | otherwise                            = resolveMods m8n zModulators defaultMods
>   where
>     trace_RM                             = unwords ["reconModulation", shdr.sampleName, show nModEnv]
>     m8n                :: Modulation     =
>       defModulation{
>         mLowpass                         = Lowpass resonanceType curKernelSpec
>       , mModEnv                          = nModEnv
>       , mModLfo                          = nModLfo
>       , mVibLfo                          = nVibLfo
>       , toPitchCo                        = summarize ToPitch
>       , toFilterFcCo                     = summarize ToFilterFc
>       , toVolumeCo                       = summarize ToVolume}
>
>     curKernelSpec                        =
>       KernelSpec
>         (maybe 13_500 (clip (1_500, 13_500)) zInitFc)
>         (maybe 0      (clip (0,     960))    zInitQ)
>         1 
>         useFastFourier
>         (-1) -- must always be replaced
>
>     resonanceType      :: ResonanceType  = if lowpassFc (mLowpass m8n) < 10_000
>                                              then ResonanceBandpass
>                                              else ResonanceBandpass
>
>     nModEnv            :: Maybe Envelope = deriveEnvelope
>                                              zDelayModEnv
>                                              zAttackModEnv
>                                              noon
>                                              nps
>                                              (zHoldModEnv, zKeyToModEnvHold) 
>                                              (zDecayModEnv, zKeyToModEnvDecay)
>                                              zSustainModEnv
>                                              zReleaseModEnv
>                                              (Just (zModEnvToPitch, zModEnvToFc))
>     nModLfo, nVibLfo   :: Maybe LFO
>     nModLfo                              =
>       deriveLFO zDelayModLfo zFreqModLfo zModLfoToPitch zModLfoToFc zModLfoToVol
>     nVibLfo            :: Maybe LFO      =
>       deriveLFO zDelayVibLfo zFreqVibLfo zVibLfoToPitch Nothing     Nothing
>
>     summarize          :: ModDestType → ModCoefficients
>     summarize toWhich                    =
>       ModCoefficients
>         (coAccess toWhich $ maybe defModTriple   eModTriple nModEnv)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nModLfo)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nVibLfo)

emit standard output text detailing what choices we made for rendering GM items =======================================

> printChoices           :: SFRoster
>                           → [InstrumentName]
>                           → [(InstrumentName, [String])]
>                           → [PercussionSound]
>                           → ([(Bool, [Emission])], [(Bool, [Emission])])
> printChoices SFRoster{zWinningRecord} is msgs ps
>                                          = (map (showI zWinningRecord) is, map (showP zWinningRecord) ps)
>   where
>     showI              :: WinningRecord → InstrumentName → (Bool, [Emission])
>     showI WinningRecord{pWinningI} kind
>       | isJust mpergm                    = (True, true kind mpergm ++ emitMsgs kind msgs)
>       | kind == Percussion               = (True, [Blanks 3, gmId kind, Unblocked "(pseudo-instrument)", EndOfLine])
>       | otherwise                        = (False, false kind)
>       where
>         mpergm                           = Map.lookup kind pWinningI
>
>     showP               :: WinningRecord → PercussionSound → (Bool, [Emission])
>     showP WinningRecord{pWinningP} kind
>       | isJust mpergm                    = (True, true kind mpergm)
>       | otherwise                        = (False, false kind)
>       where
>         mpergm                           = Map.lookup kind pWinningP
>
>     true kind mpergm                     =
>       [Blanks 3, gmId kind, Unblocked " -> "] ++ showPerGM (fromJust mpergm) ++ [EndOfLine]
>     false kind                           =
>       [Blanks 3, gmId kind, Unblocked " not found", EndOfLine]
>
> showPerGM              :: PerGMScored → [Emission]
> showPerGM PerGMScored{szI, mszP, pPerGMKey}
>                                          = [emitShowL pgkwFile 4] ++ [ToFieldL szI 22] ++ showmZ
>   where
>     PerGMKey{pgkwFile}                   = pPerGMKey
>     showmZ                               = maybe [] showZ mszP
>     showZ name                           = [Unblocked name]
>
> runDBActions  :: ReaderT MongoContext IO () → IO ()
> runDBActions as = 
>   withMongoDBConn "parth" "localhost" (PortNumber 27_017) Nothing 2000 $ \pool →
>     runMongoDBPool master as pool
>
> actions :: ReaderT MongoContext IO ()
> actions = do
>   ckey ← insert defC
>   skey ← insert defS
>   fkey ← insert defF 
>   mkey ← insert defM
>   tkey ← insert defT
>   dkey ← insert defD
>   let config = Config "default" ckey skey fkey mkey tkey dkey
>   gkey ← insert config
>   liftIO $ print gkey
>   return ()
>
> mmain :: IO ()
> mmain = do
>   runDBActions actions
>
> emitSettingses         :: [Emission]
> emitSettingses                           =
>   concat 
>     [ emitSettings defC
>     , emitSettings defS
>     , emitSettings defM
>     , emitSettings defF 
>     , emitSettings defT
>     , emitSettings defD]
>
> emitSettings           :: Show a ⇒ a → [Emission]
> emitSettings def_                        =
>   [ Unblocked "\n\n"
>   , Unblocked $ show def_]
>
> emitMsgs               :: InstrumentName → [(InstrumentName, [String])] → [Emission]
> emitMsgs kind msgs                       = concatMap (\s → [Unblocked s, EndOfLine]) imsgs
>   where
>     imsgs              :: [String]       = fromMaybe [] (lookup kind msgs)
>
> dumpContestants        :: ∀ a. (Ord a, Show a, SFScorable a) ⇒ (a, [PerGMScored]) → [Emission]
> dumpContestants (kind, contestants)      = prolog ++ es ++ epilog
>   where
>     prolog, es, epilog :: [Emission]
>
>     prolog                               = emitLine [emitShowL kind 50]
>     es                                   = concatMap dumpContestant contestants
>     epilog                               = emitLine []
>
> dumpContestant         :: PerGMScored → [Emission]
> dumpContestant PerGMScored{ .. }
>                                          = es
>   where
>     ArtifactGrade{pEmpiricals, pScore}   = pArtifactGrade
>     PerGMKey{pgkwFile}                   = pPerGMKey
>     showAkr            :: Double         = roundBy 10 pAgainstKindResult
>     (showEmp, n)                         = showEmpiricals pEmpiricals
> 
>     es = emitLine [ Blanks 4, emitShowL      pgkwFile                  8
>                             , ToFieldR       szI                      22
>                   , Blanks 4, ToFieldR      (fromMaybe "" mszP)       22
>                   , Blanks 4, emitShowL      pScore                   15
>                             , ToFieldL       showEmp                   n
>                             , emitShowR      showAkr                   8]
>
> fixBadNames                              = soundFontSettingsQqFixBadNames                defF
> allowStereoCrossovers                    = soundFontSettingsQqAllowStereoCrossovers      defF
> -- stereo pair can come from 2 different instruments in the same file
> allowOverlappingRanges                   = soundFontSettingsQqAllowOverlappingRanges     defF
> allowOutOfRange                          = soundFontSettingsQqAllowOutOfRange            defF
> -- more than one zone can reference a given range cell
> multipleCompetes                         = soundFontSettingsQqMultipleCompetes           defF
> combinePartials                          = soundFontSettingsQqCombinePartials            defF
> canDevolveToMono                         = soundFontSettingsQqCanDevolveToMono           defF
>
> defF                   :: SoundFontSettings
> defF                                     = SoundFontSettings True False True True True False True

The End
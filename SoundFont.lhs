> {-# LANGUAGE InstanceSigs #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeFamilies #-} 
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UnicodeSyntax #-}

SoundFont
William Clements
April 16, 2023

> module SoundFont (doEverything, profileSF2s) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Either
> import Data.Foldable ( toList, for_ )
> import Data.Int ( Int8, Int16 )
> import Data.IntSet (IntSet)
> import qualified Data.IntSet             as IntSet
> import Data.List ( find, foldl', foldr, last, minimumBy, partition, singleton, sortOn, elemIndex, iterate' )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( fromJust, fromMaybe, isJust, isNothing, mapMaybe, catMaybes )
> import Data.MemoTrie
> import Data.Ord ( Down(Down), comparing )
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
>   , pskwSample         :: Word} deriving (Eq, Generic, Ord, Show)
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
>   , sOther             :: Maybe PreSampleKey} deriving Show
>
> data PreZoneKey =
>   PreZoneKey {
>     pzkwFile           :: Word
>   , pzkwBag            :: Word} deriving (Eq, Ord, Show)
> nilPreZoneKey                            = PreZoneKey 0 0
>
> data ZoneRole                            = ZRGlobal | ZRNonGlobal deriving Show
>
> data PreZone =
>   PreZone {
>     pzRole             :: ZoneRole
>   , pzPresk            :: PreSampleKey
>   , pzPergm            :: PerGMKey
>   , pzBagIndex         :: Word
>   , pzDigest           :: Maybe ZoneDigest
>   , pzmkPartner        :: Maybe PreZoneKey} deriving Show
>
> data PreInstrument =
>   PreInstrument {
>     pInst              :: F.Inst
>   , iName              :: String
>   , iMatches           :: FFMatches
>   , iPreZones          :: [PreZone]}
>
> data PerGMKey =
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
> data PerGMScored =
>   PerGMScored {
>     pArtifactGrade     :: ArtifactGrade
>   , pKind              :: Kind
>   , pAgainstKindResult :: AgainstKindResult
>   , pPerGMKey          :: PerGMKey
>   , szI                :: String
>   , mszP               :: Maybe String} deriving (Show)
>
> data WinningRecord =
>   WinningRecord {
>     pWinningI          :: Map InstrumentName PerGMScored
>   , pWinningP          :: Map PercussionSound PerGMScored
>   , pWarnings          :: [String]} deriving Show
>
> seedWinningRecord      :: WinningRecord
> seedWinningRecord                        = WinningRecord Map.empty Map.empty []
>
> data ZoneHeader =
>   ZoneHeader {
>     zhwBag             :: Word
>   , zhShdr             :: F.Shdr} deriving Show
> zfindByBagIndex        :: [(ZoneHeader, a)] → Word → Maybe (ZoneHeader, a)
> zfindByBagIndex zs w                     =
>   find (\(ZoneHeader{zhwBag}, _) → w == zhwBag) zs
> zfindBySampleIndex     :: [(a, SFZone)] → Word → Maybe (a, SFZone)
> zfindBySampleIndex zs w                 =
>   find (\(_, SFZone{zSampleIndex}) → zSampleIndex == Just w) zs
>
> data PerInstrument =
>   PerInstrument {
>     pZones             :: [(ZoneHeader, SFZone)]
>   , pSmashing          :: Smashing Word}

Instrument categories: instrument, percussion, disqualified

> data InstCat =
>        InstCatInst [PreZone]
>      | InstCatPerc [PreZone] [Word]
>      | InstCatDisq DisqReason deriving Show
>     
> class GMPlayable a ⇒ SFScorable a where
>   splitScore           :: a → [(ZoneHeader, SFZone)] → Double
>   fuzzFactor           :: a → Double
>
> instance SFScorable InstrumentName where
>   splitScore kind zs                     = fromIntegral (length zs)
>   fuzzFactor _                           = 7/8
>
> instance SFScorable PercussionSound where
>   splitScore kind _                      = 1
>   fuzzFactor _                           = 3/4
>
> data SFRoster =
>   SFRoster {
>     zFiles             :: Array Word SFFile
>   , zPreSampleCache    :: Map PreSampleKey PreSample
>   , zPreZoneCache      :: Map PreZoneKey PreZone
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>   , zRost              :: ([InstrumentName], [PercussionSound])
>   , zZoneCache         :: Map PerGMKey PerInstrument
>   , zWinningRecord     :: WinningRecord}
>
> seedRoster :: Array Word SFFile → ([InstrumentName], [PercussionSound]) → SFRoster
> seedRoster vFile rost                    =
>   SFRoster vFile Map.empty Map.empty Map.empty rost Map.empty seedWinningRecord
>
> data SFFile =
>   SFFile {
>     zFilename          :: FilePath
>   , zArrays            :: SoundFontArrays
>   , zWordF             :: Word}
>
> data SoundFontArrays = 
>   SoundFontArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssIMods            :: Array Word F.Mod
>   , ssShdrs            :: Array Word F.Shdr
>   , ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
>
> data ZoneDigest =
>   ZoneDigest {
>     zdKeyRange         :: Maybe (Word, Word)
>   , zdVelRange         :: Maybe (Word, Word)
>   , zdSampleIndex      :: Maybe Word} deriving Show
> defDigest              :: ZoneDigest
> defDigest                                = ZoneDigest Nothing Nothing Nothing
> formDigest             :: [F.Generator] → ZoneDigest
> formDigest                               = foldr inspectGen defDigest
>   where
>     inspectGen         :: F.Generator → ZoneDigest → ZoneDigest 
>     inspectGen (F.KeyRange i j) zd       = zd {zdKeyRange = Just (i, j)}
>     inspectGen (F.VelRange i j) zd       = zd {zdVelRange = Just (i, j)}
>     inspectGen (F.SampleIndex w) zd      = zd {zdSampleIndex = Just w}
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
> theGrader              :: Grader         = grader ssWeights 500

profiler =============================================================================================================

> profileSF2s            :: IO ()
> profileSF2s                              = do
>   putStrLn "profileSF2s..."
>   fps                  ← FP.getDirectoryFiles "." (singleton "*.sf2")
>   putStrLn $ unwords ["scanning", show $ length fps, "files..."]
>   mapM_ profileSF2 fps
>
> profileSF2             :: FilePath → IO ()
> profileSF2 fp                            = do
>   sffile@SFFile{zArrays}                 ← openSoundFontFile 0 fp
>   let SoundFontArrays{ssInsts}           = zArrays
>   putStrLn ""
>   mapM_ (profileInst sffile) (deriveRange 0 (length ssInsts))
>
> profileInst          :: SFFile → Int → IO ()
> profileInst sffile@SFFile{zArrays} ii  = do
>   CM.when diagnosticsEnabled (print ii)
>   mapM_ doZone oList
>     where
>       SoundFontArrays{ssInsts}           = zArrays
>       iinst                              = ssInsts ! fromIntegral ii
>       jinst                              = ssInsts ! fromIntegral (ii+1)
>
>       ibagi                              = F.instBagNdx iinst
>       jbagi                              = F.instBagNdx jinst
>
>       (gIx, oIx)                         =
>          profess
>            (ibagi <= jbagi)
>            (unwords["SoundFont file corrupt (profileInst)"])
>            (singleton ibagi, deriveRange (ibagi+1) jbagi)
>       gList                              = map (buildZone sffile defZone)        gIx
>       gZone                              = (snd . head)                          gList
>       oList                              = map (buildZone sffile gZone)          oIx
>
>       doZone           :: (ZoneHeader, SFZone) → IO ()
>       doZone zone                        = do
>         let mout                         = profileZone zone
>         CM.forM_ mout putStrLn
> 
>       profileZone      :: (ZoneHeader, SFZone) → Maybe String
>       profileZone (zh@ZoneHeader{zhShdr}, zone@SFZone{ .. })
>                                          =
>         let
>           F.Inst{instName}               = iinst
>           F.Shdr{sampleName}             = zhShdr
>
>           seed                           = if zoneConforms (zh, zone)
>                                              then Nothing
>                                              else Just True
>
>           sout                           =
>             unwords [show instName                      , ","
>                    , show sampleName                    , "==="
>                                                         , "zInitFc,zInitQ="
>                    , show (zInitFc, zInitQ)             , "zSampleMode, zScaleTuning, zExclusiveClass="
>                    , show (zSampleMode, zScaleTuning, zExclusiveClass)
>                    , show zKeyRange                     , "= zKeyRange"]
>         in
>           seed >> if goodName instName && goodName sampleName then Just sout else Nothing

executive =============================================================================================================

> doEverything           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> doEverything songs = do
>
>   putStrLn "everything..."
>   putStrLn $ reapEmissions emitSettingses
>
>   tsStarted           ← getCurrentTime
>
>   rost                ← qualifyKinds songs
>   putStrLn $ unwords ["rost", show rost]
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
>     -- track the complete populations of: samples, instruments, percussion
>     finishRoster       :: SFRoster → IO SFRoster
>     finishRoster preRoster@SFRoster{zFiles, zRost}
>                        = do
>       tsStarted        ← getCurrentTime
>
>       putStrLn "initSamples"
>       preSampleCache   ← initSamples zFiles
>       putStrLn "initInsts"
>       preInstCache_    ← initInsts zFiles
>       putStrLn "initZones"
>       preZoneCache     ← initZones zFiles preSampleCache preInstCache_
>       preInstCache     ← associateZones preInstCache_ preZoneCache
>
>       jobs             ← categorize zFiles preInstCache zRost preZoneCache
>       tsCatted         ← getCurrentTime
>       putStrLn ("___categorize: " ++ show (diffUTCTime tsCatted tsStarted))
>
>       zc               ← formZoneCache zFiles preInstCache preZoneCache zRost jobs
>       (pergmsI, pergmsP)
>                        ← sortByCategory zc jobs
>       CM.when diagnosticsEnabled
>         (do
>           print "pergmsI"
>           print pergmsI
>           print "pergmsP"
>           print pergmsP)
>
>       tsZoned          ← getCurrentTime
>       putStrLn ("___cache zones: " ++ show (diffUTCTime tsZoned tsCatted))
>
>       -- actually conduct the tournament
>       ((wI, sI), (wP, sP))
>                        ← decideWinners zFiles preInstCache preSampleCache zc zRost pergmsI pergmsP
>       tsDecided        ← getCurrentTime
>       putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsZoned))
>
>       CM.when reportTourney (writeTournamentReport zFiles wI wP)
>       tsReported       ← getCurrentTime
>
>       let wins@WinningRecord{pWarnings}
>                        = WinningRecord (Map.map head wI) (Map.map head wP) (sI ++ sP)
>
>       -- print song/orchestration info to user (can be captured by redirecting standard out)
>       mapM_ putStrLn pWarnings
>
>       let sfrost       = preRoster{zPreSampleCache   = preSampleCache
>                                  , zPreZoneCache     = preZoneCache
>                                  , zPreInstCache     = preInstCache
>                                  , zZoneCache        = zc
>                                  , zWinningRecord    = wins}
>
>       tsRecond     ← getCurrentTime
>       putStrLn ("___create winning record: " ++ show (diffUTCTime tsRecond tsReported))
>         
>       return sfrost
>
>     -- get it on
>     doRendering      :: SFRoster → IO ()
>     doRendering sfrost                 = do
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

>     initSamples        :: Array Word SFFile → IO (Map PreSampleKey PreSample)
>     initSamples sffiles
>                        = do
>       presks           ← formMasterSampleList sffiles
>       (preSampleCache_, errs)
>                        ← formPreSampleCache sffiles presks
>       (preSampleCache, errs')
>                        ← finishPreSampleCache sffiles preSampleCache_
>
>       mapM_ putStrLn (reverse errs ++ reverse errs')
>       print $ length preSampleCache
>       return preSampleCache
>         
>     initInsts          :: Array Word SFFile → IO (Map PerGMKey PreInstrument)
>     initInsts sffiles
>                        = do
>       pergmsI_         ← formMasterInstList sffiles
>       (preInstCache, errs)
>                        ← formPreInstCache sffiles pergmsI_
>
>       mapM_ putStrLn (reverse errs)
>       return preInstCache
>
>     initZones          :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PerGMKey PreInstrument
>                           → IO (Map PreZoneKey PreZone)
>     initZones sffiles preSampleCache preInstCache
>                        = do
>       (preZoneCache, errs)
>                        ← formPreZoneCache sffiles preSampleCache preInstCache
>
>       mapM_ putStrLn (reverse errs)
>       return preZoneCache
>
>     associateZones     :: Map PerGMKey PreInstrument → Map PreZoneKey PreZone → IO (Map PerGMKey PreInstrument)
>     associateZones preInstCache_ preZoneCache
>                                          = do
>       print "associateZones"
>       let preInstCache = Map.mapWithKey addZones preInstCache_
>       print (length preInstCache_)
>       print (length preInstCache)
>       return preInstCache
>       
>       where
>         inverter       :: Map PerGMKey [PreZone]
>         inverter                        = Map.foldlWithKey inverseFolder Map.empty preZoneCache
>
>         addZones       :: PerGMKey → PreInstrument → PreInstrument
>         addZones pergm preI             =
>           let
>             inverted                    = Map.lookup pergm inverter
>             newPreZones                 = maybe [] reverse inverted
>           in
>             preI{iPreZones = newPreZones}
>
>         inverseFolder  :: Map PerGMKey [PreZone] → PreZoneKey → PreZone → Map PerGMKey [PreZone]
>         inverseFolder target key new@PreZone{pzPergm}
>           | traceNot trace_IF False      = undefined
>           | otherwise                    = Map.insert pzPergm now target
>           where
>             trace_IF                     = unwords ["soFar", show key, show pzPergm, show $ length now]
>             soFar, now :: [PreZone]
>             soFar                        = fromMaybe [] (Map.lookup pzPergm target)
>             now                          = new : soFar

> writeTournamentReport  :: Array Word SFFile
>                           → Map InstrumentName [PerGMScored]
>                           → Map PercussionSound [PerGMScored]
>                           → IO ()
> writeTournamentReport sffiles pContI pContP
>                                          = do
>   tsStarted            ← getCurrentTime
>
>   -- output all selections to the report file
>   let legend           =
>           emitComment     [   Unblocked "legend = [hints, stereo, 24-bit, resolution, conformant, fuzzy]"]
>        ++ emitNextComment [   Unblocked "weights = "
>                             , Unblocked (show ssWeights)] 
>   let esFiles          = emitFileListC
>   let esI              = concatMap dumpContestants (Map.toList pContI)
>   let esP              = concatMap dumpContestants (Map.toList pContP)
>   let esQ              = emitSettingses
>   let esTail           = singleton $ Unblocked "\n\nThe End\n\n"
>   let eol              = singleton EndOfLine
>
>   writeFileBySections reportName [esFiles, legend, esI, eol, esFiles, legend, esP, esQ, esTail]
>
>   tsFinished            ← getCurrentTime
>
>   putStrLn ("___report tournament results: " ++ show (diffUTCTime tsFinished tsStarted))
>   traceIO ("wrote " ++ reportName)
>
>   where
>     emitFileListC      :: [Emission]
>     emitFileListC                        = concatMap (uncurry doF) (zip [0..] (toList sffiles))
>
>     doF                :: Int → SFFile → [Emission]
>     doF nth sffile                       = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>         
> decideWinners          :: Array Word SFFile
>                           → Map PerGMKey PreInstrument
>                           → Map PreSampleKey PreSample
>                           → Map PerGMKey PerInstrument
>                           → ([InstrumentName], [PercussionSound]) 
>                           → [PerGMKey]
>                           → [PerGMKey]
>                           → IO ((Map InstrumentName [PerGMScored], [String])
>                               , (Map PercussionSound [PerGMScored], [String]))
> decideWinners sffiles preInstCache preSampleCache zc rost pergmsI pergmsP
>                                          = do
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
>         PreInstrument{iName, iMatches}   =
>           deJust (unwords["wiFolder", "PreInstrument"]) (Map.lookup pergmI preInstCache)
>         fuzzMap                          = getFuzzMap iMatches
>
>         as             :: Map InstrumentName Fuzz
>         as                               =
>           Map.filterWithKey (\k v → k `elem` select rost) fuzzMap
>
>         as'            :: [InstrumentName]
>         as'                              =
>           profess
>             (not $ null as)
>             (unwords ["unexpected empty matches for", show pgkwFile, iName]) 
>             (if multipleCompetes
>                then Map.keys as
>                else (singleton . fst) (Map.findMax as))
>     
>     wpFolder           :: (Map PercussionSound [PerGMScored], [String])
>                           → PerGMKey
>                           → (Map PercussionSound [PerGMScored], [String])
>     wpFolder wIn pergmP@PerGMKey{pgkwFile, pgkwBag}
>       | traceNot trace_WP False          = undefined
>       | otherwise                        = xaEnterTournament fuzzMap pergmP [] wIn kind
>       where
>         mz             :: Maybe (ZoneHeader, SFZone)
>         mz                               = pgkwBag >>= zfindByBagIndex pZones
>         mkind          :: Maybe PercussionSound
>         mkind                            = mz >>= getAP >>= pitchToPerc
>         kind                             = deJust (unwords["wpFolder", "mkind"]) mkind
>
>         mffm           :: Maybe FFMatches
>         mffm                             =
>           mz >>= (zSampleIndex . snd)
>              >>= Just . PreSampleKey pgkwFile
>              >>= (`Map.lookup` preSampleCache)
>              >>= Just . sMatches
>         fuzzMap        :: Map PercussionSound Fuzz
>         fuzzMap                          = getFuzzMap $ deJust "mffm" mffm
>
>         PerInstrument{pZones}              =
>           deJust (unwords["wpFolder", "PerInstrument"]) (Map.lookup (pergmP{pgkwBag = Nothing}) zc)
>
>         getAP          :: (ZoneHeader, SFZone) → Maybe AbsPitch
>         getAP (_, SFZone{zKeyRange})     = (Just . fst) =<< zKeyRange
>
>         trace_WP                         = unwords ["wpFolder", show pergmP]

tournament among GM instruments and percussion from SoundFont files ===================================================

>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           Map a Fuzz
>                           → PerGMKey
>                           → [SSHint]
>                           → (Map a [PerGMScored], [String])
>                           → a
>                           → (Map a [PerGMScored], [String])
>     xaEnterTournament fuzzMap pergm@PerGMKey{pgkwFile, pgkwBag} hints (wins, ss) kind
>       | traceIf trace_XAET False         = undefined
>       | otherwise                        = (Map.insert kind now wins, ss)
>       where
>         pergm_                           = pergm{pgkwBag = Nothing}
>         PreInstrument{iName}             =
>           deJust (unwords["xaEnterTournament", "PreInstrument"]) (Map.lookup pergm_ preInstCache)
>         PerInstrument{pZones}        =
>           deJust (unwords["xaEnterTournament", "PerInstrument"]) (Map.lookup pergm_ zc)
>
>         soFar, now     :: [PerGMScored]
>         soFar                            = fromMaybe [] (Map.lookup kind wins)
>         now                              = scoredP : soFar
>         scoredP        :: PerGMScored    =
>           PerGMScored (computeGrade scope akResult) (toKind kind) akResult pergm iName mnameZ
>
>         akResult                         = fromMaybe 0 (Map.lookup kind fuzzMap)
>
>         scope_, scope  :: [(ZoneHeader, SFZone)]
>         scope_                           =
>           case pgkwBag of
>             Nothing                      → tail pZones
>             Just bagI                    →
>                    maybe
>                      (error $ unwords [ "xaEnterTournament: zfindByBagIndex returned a Nothing for"
>                                       , show pgkwFile, iName])
>                      singleton
>                      (zfindByBagIndex pZones bagI)
>         scope                            =
>           profess
>             (not (null scope_))
>             (unwords["xaEnterTournament", "null scope", iName, show (length pZones)])
>             scope_
>             
>
>         mnameZ         :: Maybe String   =     pgkwBag
>                                            >>= zfindByBagIndex pZones
>                                            >>= \(ZoneHeader{zhShdr}, _) → Just (F.sampleName zhShdr)
>         zName                            = deJust "mnameZ" mnameZ
>
>         trace_XAET                       =
>           unwords ["xaEnterTournament", iName, show mnameZ, show kind, show scoredP.pArtifactGrade
>                  , "scope", show $ length scope]
>
>         computeGrade   :: [(ZoneHeader, SFZone)] → AgainstKindResult → ArtifactGrade
>         computeGrade zs akResult         = gradeEmpiricals theGrader empiricals
>           where
>             empiricals :: [Double]       = [   foldHints hints
>                                              , fromRational $ scoreBool $ isStereoInst zs
>                                              , fromRational $ scoreBool $ is24BitInst zs
>                                              , computeResolution kind rost zs
>                                              , fromRational $ scoreBool $ all zoneConforms zs
>                                              , fuzz]
>             howgood                      = akResult - stands
>             fuzz       :: Double
>               | howgood > 0.000_001      = max 0 (logBase 2 howgood) * fuzzFactor kind
>               | otherwise                = 0
>   
> formMasterSampleList   :: Array Word SFFile → IO [PreSampleKey]
> formMasterSampleList sffiles             = return $ concatMap formFS sffiles
>   where
>     formFS             :: SFFile → [PreSampleKey]
>     formFS SFFile{zArrays, zWordF}               =
>       let
>         SoundFontArrays {ssShdrs}           = zArrays
>         (st, en)       :: (Word, Word)   = bounds ssShdrs
>       in
>         map (PreSampleKey zWordF) [st..(en - 1)]
>
> formPreSampleCache     :: Array Word SFFile
>                           → [PreSampleKey]
>                           → IO (Map PreSampleKey PreSample, [String])
> formPreSampleCache sffiles presks        = return $ foldl' preSFolder (Map.empty, []) presks
>   where
>     computePreSample   :: F.Shdr → (Maybe PreSample, String)
>     computePreSample shdr@F.Shdr{ .. }   =
>       if goodName sampleName
>          && sampleRate >= 64
>          && sampleRate < 2^20
>          && isJust (toMaybeSampleType sampleType)
>          && sampleLimitsOk (start, end)
>          && sampleLoopLimitsOk (startLoop, endLoop)
>         then (Just $ PreSample sampleName (computeFFMatches sampleName) Nothing, [])
>         else (Nothing, diagnose shdr)
>
>     diagnose           :: F.Shdr → String
>     diagnose fshdr@F.Shdr{ .. } 
>                                          =
>       unwords ["formPreSampleCache", "problem", show (fshdr, toMaybeSampleType sampleType)]
>
>     loadShdr PreSampleKey{pskwSample, pskwFile}
>                                          = ssShdrs ! pskwSample
>       where
>         SFFile{zArrays}                  = sffiles ! pskwFile
>         SoundFontArrays{ssShdrs}         = zArrays
>
>     preSFolder :: (Map PreSampleKey PreSample, [String]) → PreSampleKey → (Map PreSampleKey PreSample, [String])
>     preSFolder (target, errs) presk@PreSampleKey{pskwFile, pskwSample}
>                                          =
>       let
>         (mpres, err)                     = (computePreSample . loadShdr) presk
>       in
>         case mpres of
>           Just pres                      → (Map.insert presk pres target, errs)
>           Nothing                        → (target, err : errs)
>
> finishPreSampleCache    :: Array Word SFFile
>                            → Map PreSampleKey PreSample
>                            → IO (Map PreSampleKey PreSample, [String])
> finishPreSampleCache sffiles preSampleCache
>                                          =
>   return $ foldl' combine (Map.empty, []) (Map.mapWithKey qualify preSampleCache)
>   where
>     combine            :: (Map PreSampleKey PreSample, [String])
>                           → Either (PreSampleKey, PreSample) String
>                           → (Map PreSampleKey PreSample, [String])
>     combine (target, errs) eith         = (target', errs')
>       where
>         (target', errs')                  =
>           case eith of
>             Left (presk, val)            → (Map.insert presk val target, errs)
>             Right err                    → (target, err : errs)
>
>     qualify            :: PreSampleKey → PreSample → Either (PreSampleKey, PreSample) String
>     qualify key@PreSampleKey{pskwFile, pskwSample} val
>       | not stereo                       = Left (key, val)
>       | isJust other                     = Left (key, val{sOther = Just otherKey})
>       | otherwise                        =
>       Right $ unwords ["finishPreSampleCache", "missing stereo partner", show key]
>       where
>         otherKey                         = PreSampleKey pskwFile sampleLink
>         other                            = Map.lookup otherKey preSampleCache 
>
>         SFFile{zArrays}                  = sffiles ! pskwFile
>         SoundFontArrays{ssShdrs}         = zArrays
>         F.Shdr{sampleType, sampleLink}   = ssShdrs ! pskwSample
>         stype                            = toSampleType sampleType
>         stereo                           = SampleTypeLeft == stype || SampleTypeRight == stype
>
> data ZoneScanner                         =
>   ZoneScanner {
>     zsSFFiles          :: Array Word SFFile
>   , zsPreSampleCache   :: Map PreSampleKey PreSample
>   , zsPreInstCache     :: Map PerGMKey PreInstrument
>   , zswFile            :: Word
>   , zswInst            :: Word
>   , zsmZkz             :: [(PreZoneKey, PreZone)]}
> nextZoneScanner        :: ZoneScanner → ZoneScanner
> nextZoneScanner zsor@ZoneScanner{ .. }   = zsor{zswInst = zswInst + 1, zsmZkz = news}
>   where
>     SoundFontArrays{ .. }                = zArrays (zsSFFiles ! zswFile)
>     iinst                                = ssInsts ! fromIntegral zswInst
>     jinst                                = ssInsts ! fromIntegral (zswInst+1)
>
>     pergm                                = PerGMKey zswFile zswInst Nothing
>     mPreInst                             = Map.lookup pergm zsPreInstCache
>     news                                 = if isJust mPreInst
>                                              then capturePreZones
>                                              else error $ unwords ["nextZoneScanner", show pergm, "has no PreInst"]
>
>     capturePreZones    :: [(PreZoneKey, PreZone)]
>     capturePreZones
>       | traceNever trace_CPZ False       = undefined
>       | otherwise                        =
>       if null pzkO
>         then []
>         else pzkG ++ pzkO
>       where
>         trace_CPZ                        = unwords ["capturePreZones", show $ length pzkO]
>
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
>         pzkG, pzkO     :: [(PreZoneKey, PreZone)]
>         pzkG                             =
>           mapMaybe (producePreZone ZRGlobal) (deriveRange ibagi (ibagi + 1))
>         pzkO                             =
>           mapMaybe (producePreZone ZRNonGlobal) (deriveRange (ibagi + 1) jbagi)
>
>     producePreZone     :: ZoneRole → Word → Maybe (PreZoneKey, PreZone)
>     producePreZone zrole bagIndex
>       | traceNever trace_PPZ False       = undefined
>       | otherwise                        = mpres >> makePreZone zrole presk bagIndex zd 
>       where
>         trace_PPZ                        = unwords ["producePreZone", show bagIndex]
>
>         xgeni                            = F.genNdx $ ssIBags ! bagIndex
>         ygeni                            = F.genNdx $ ssIBags ! (bagIndex + 1)
>
>         gens       :: [F.Generator]      = profess
>                                              (xgeni <= ygeni)
>                                              "SoundFont file corrupt (computePreZone gens)"
>                                              (map (ssIGens !) (deriveRange xgeni ygeni))
>
>         zd@ZoneDigest{zdSampleIndex}     = formDigest gens
>         si                               = fromMaybe nilSampleIndex zdSampleIndex
>         nilSampleIndex                   = 0
>         presk                            = PreSampleKey zswFile si
>         mpres                            = Map.lookup presk zsPreSampleCache
>         
>     makePreZone        :: ZoneRole → PreSampleKey → Word → ZoneDigest → Maybe (PreZoneKey, PreZone)
>     makePreZone zrole presk bagIndex zd
>       | traceNot trace_MPZ False         = undefined
>       | otherwise                        = Just (pzk, pz)
>       where
>         trace_MPZ                        = unwords ["makePreZone", show zrole, show pergm, show bagIndex]
>
>         pzk                              = PreZoneKey zswFile bagIndex
>         pz                               = PreZone zrole presk pergm bagIndex (Just zd) Nothing
> 
> formPreZoneCache       :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PerGMKey PreInstrument
>                           → IO (Map PreZoneKey PreZone, [String])
> formPreZoneCache sffiles preSampleCache preInstCache
>                                          = return $ foldl' formFZ (Map.empty, []) sffiles
>   where
>     -- file to zone
>     formFZ             :: (Map PreZoneKey PreZone, [String]) → SFFile → (Map PreZoneKey PreZone, [String])
>     formFZ (target, errs) sffile@SFFile{zArrays, zWordF}
>                                          = (Map.union target (extractPreZones sffile), errs)
>
>     extractPreZones   :: SFFile → Map PreZoneKey PreZone
>     extractPreZones sffile@SFFile{zArrays, zWordF}
>       | traceIf trace_EPZ False         = undefined
>       | otherwise                        = Map.fromList zkz
>       where
>         trace_EPZ                        =
>           unwords ["extractPreZones", show (stI, enI), show (stB, enB), show (length zkz)]
>
>         SoundFontArrays {ssInsts, ssIBags}
>                                          = zArrays
>         (stI, enI)     :: (Word, Word)   = bounds ssInsts
>         (stB, enB)     :: (Word, Word)   = bounds ssIBags
>
>         seedZoneScanner                  =
>           ZoneScanner sffiles preSampleCache preInstCache zWordF stI []
>
>         infinite                         = iterate' nextZoneScanner seedZoneScanner
>         finite                           = takeWhile (\b → zswInst b <= enI) infinite
>         zkz                              = concatMap zsmZkz finite
>
> {-
> finishPreZoneCache    :: Array Word SFFile
>                          → Map PreSampleKey PreSample
>                          → Map PreZoneKey PreZone
>                          → IO (Map PreZoneKey PreZone, [String])
> finishPreZoneCache sffiles preSampleCache preZoneCache
>                                          = return (Map.mapWithKey attach preZoneCache, [])
>   where
>     zoneInsts          :: Map PreZoneKey PerGMKey
>     zoneInsts                            = makeZoneInsts sffiles
>
>     inverse            :: Map PreSampleKey [PreZoneKey]
>     inverse                              = Map.foldlWithKey inverseFolder Map.empty preZoneCache
>
>     inverseFolder      :: Map PreSampleKey [PreZoneKey] → PreZoneKey → PreZone → Map PreSampleKey [PreZoneKey]
>     inverseFolder target key PreZone{pzPresk}
>                                          = Map.insert key now target
>       where
>         soFar, now     :: [PreZoneKey]
>         soFar                            = fromMaybe [] (Map.lookup pzPresk target)
>         now                              = key : soFar
>
>     attach             :: PreZoneKey → PreZone → PreZone
>     attach key val@PreZone{pzPresk, pzPergm}
>                                          =
>       let
>         others                           = deJust "others" (Map.lookup pzPresk inverse)
>         pergm                            = tracer "me" $ deJust "me" (Map.lookup key zoneInsts)
>       in
>         val{pzmkPartner = Just (favoriteOf others), pzPergm = pergm} {- , pzDigest = Just (formDigest gens)} -}
>       where
>         favoriteOf     :: [PreZoneKey] → PreZoneKey
>         favoriteOf choices               =
>           let
>             local                        = find (\zk → pzPergm == fromJust (Map.lookup zk zoneInsts)) choices
>             global                       = Just $ head choices
>           in
>             deJust "cand" (local `CM.mplus` global)
>
>     makeZoneInsts sffiles                = foldl' Map.union Map.empty (map formFI (toList sffiles)) 
>       where
>         -- file to instrument
>         formFI         :: SFFile → Map PreZoneKey PerGMKey
>         formFI sffile@SFFile{zArrays, zWordF}
>                                          =
>           let
>             SoundFontArrays{ssInsts}     = zArrays
>             boundsI                      = bounds ssInsts
>             rangeI                       = deriveRange (fst boundsI) (snd boundsI-1)
>                 
>             -- instrument to zone
>             formIZ     :: Word → Map PreZoneKey PerGMKey
>             formIZ wI                        =
>               foldl' Map.union Map.empty (map invert (deriveRange ibagi (jbagi-1)))
>               where
>                 iinst                        = ssInsts ! fromIntegral wI
>                 jinst                        = ssInsts ! fromIntegral (wI+1)
>                 ibagi                        = F.instBagNdx iinst
>                 jbagi                        = F.instBagNdx jinst
>
>                 invert wB                    =
>                   Map.insert (PreZoneKey zWordF wB) (PerGMKey zWordF wI Nothing) Map.empty
>           in
>             foldl' Map.union Map.empty (map formIZ rangeI)
> -}
>
> formPreInstCache       :: Array Word SFFile → [PerGMKey] → IO (Map PerGMKey PreInstrument, [String])
> formPreInstCache sffiles pergms
>                                          = return $ foldl' preIFolder (Map.empty, []) pergms
>   where
>     computePreInst     :: F.Inst → Either PreInstrument String
>     computePreInst ii@F.Inst{instName}   =
>       let
>         checkName = goodName instName
>       in
>         if checkName
>           then Left $ PreInstrument ii instName (computeFFMatches instName) []
>           else Right $ unwords ["formPreInstCache", "illegal name", show instName]
>
>     loadInst           :: PerGMKey -> F.Inst
>     loadInst PerGMKey{pgkwFile, pgkwInst}
>                                          = ssInsts ! pgkwInst
>       where
>         SFFile{zArrays}                  = sffiles ! pgkwFile
>         SoundFontArrays{ssInsts}         = zArrays
>
>     preIFolder         :: (Map PerGMKey PreInstrument, [String]) → PerGMKey → (Map PerGMKey PreInstrument, [String])
>     preIFolder (target, errs) pergm       =
>       let
>         eith                             = (computePreInst . loadInst) pergm
>       in
>         case eith of
>           Left preI                      → (Map.insert pergm preI target, errs)
>           Right str                      → (target, str : errs)
>
> formMasterInstList     :: Array Word SFFile → IO [PerGMKey]
> formMasterInstList sffiles               = return $ concatMap formFI sffiles
>   where
>     -- file to instrument
>     formFI             :: SFFile → [PerGMKey]
>     formFI SFFile{zArrays, zWordF}       =
>       let
>         SoundFontArrays{ssInsts}         = zArrays
>         boundsI                          = bounds ssInsts
>         rangeI                           =
>           uncurry
>             (profess
>               (uncurry (<=) boundsI)
>               (unwords ["bad file: invalid bounds", show boundsI])
>               deriveRange)
>             boundsI
>
>         qualifyInst    :: Word → Maybe PerGMKey
>         qualifyInst wordI
>           | (jbagi - ibagi) >= 2         = Just $ PerGMKey zWordF wordI Nothing
>           | otherwise                    = Nothing
>           where
>             iinst                        = ssInsts ! wordI
>             jinst                        = ssInsts ! (wordI+1)
>             ibagi                        = F.instBagNdx iinst
>             jbagi                        = F.instBagNdx jinst
>       in
>         mapMaybe qualifyInst rangeI
>
> sortByCategory         :: Map PerGMKey PerInstrument
>                           → [(PerGMKey, InstCat)]
>                           → IO ([PerGMKey], [PerGMKey])
> sortByCategory zc jobs                   = return $ foldl' pfolder ([], []) jobs
>   where
>     pfolder            :: ([PerGMKey], [PerGMKey]) → (PerGMKey, InstCat) → ([PerGMKey], [PerGMKey])
>     pfolder (pergmsI, pergmsP) (pergmI, icat)
>                                          =
>       let
>         pergmI'                          = pergmI{pgkwBag = Nothing}
>         perI                             = deJust "perI" (Map.lookup pergmI' zc)
>         pergmsP'                         = instrumentPercList pergmI perI
>       in
>         case icat of
>           InstCatPerc _ _                → (pergmsI, pergmsP ++ pergmsP')
>           InstCatInst _                  → (pergmI : pergmsI, pergmsP)
>           _                              → error $ unwords ["sortByCategory", "illegal input", show icat]
>
>     instrumentPercList :: PerGMKey → PerInstrument → [PerGMKey]
>     instrumentPercList pergmI PerInstrument{pZones}
>                                          =
>        map ((\w → pergmI {pgkwBag = Just w}) . zhwBag . fst)  (tail pZones)
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
>       let arrays@SoundFontArrays{ .. }   =
>             SoundFontArrays
>               (F.insts pdata)
>               (F.ibags pdata)
>               (F.igens pdata)
>               (F.imods pdata)
>               (F.shdrs pdata)
>               (F.smpl  sdata)
>               (F.sm24  sdata)
>       let sffile                         = SFFile filename arrays wFile
>       let nBits                          =
>             case ssM24 of
>               Nothing                    → 16
>               Just s24data               → 24
>       ts2                                ← getCurrentTime
>       CM.when diagnosticsEnabled (
>         putStrLn $ unwords [
>                           "openSoundFontFile"
>                       ,   "insts,bags,gens,mods,shdrs"
>                       ,   show $ length ssInsts
>                       ,   show $ length ssIBags
>                       ,   show $ length ssIGens
>                       ,   show $ length ssIMods
>                       ,   show $ length ssShdrs  ])
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
>     ding@Shredding{shRanges, shMsgs}     ← shredMusic (song Map.empty)
>     let dynMap                           = makeDynMap ding
>     CM.unless (null (Map.assocs dynMap)) (traceIO $ unwords ["dynMap", show dynMap])
>     let ks                               = Map.keys shRanges
>     let (is, ps)                         = (map (\i → fromMaybe i (Map.lookup i dynMap)) (lefts ks), rights ks)
>     let (esI, esP)                       = printChoices sfrost is shMsgs ps
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
> computeResolution      :: ∀ a. (Show a, SFScorable a) ⇒
>                           a
>                           → ([InstrumentName], [PercussionSound])
>                           → [(ZoneHeader, SFZone)]
>                           → Double
> computeResolution kind rost zs
>   | traceNot trace_CR False              = undefined
>   | null zs                              = error $ unwords ["null zs"]
>   | otherwise                            =
>   fromRational m1 * measureSplits kind + fromRational m2 * measureSampleSize
>   where
>     theSplit                             = splitScore kind zs
>     measureSplits kind
>       | theSplit <= 1                    = 1
>       | otherwise                        = log (m3 * theSplit)
>     measureSampleSize                    = sum (map sampleDurationScoring zs) / fromIntegral (length zs)
>
>     m1                                   = 1/2
>     m2                                   = 1/2
>     m3                                   = 3 * if isStereoInst zs then 1/2 else 1
>
>     trace_CR                             =
>       unwords ["computeResolution", show kind, show theSplit, show (measureSplits kind, measureSampleSize)]
>
> sampleDurationScoring  :: (ZoneHeader, SFZone) → Double
> sampleDurationScoring (zh@ZoneHeader{zhShdr} , z)
>                                          = if score < 0.01 then -10 else score
>   where
>     F.Shdr{sampleRate}                   = zhShdr
>     score                                = sampleSize (zh, z) / fromIntegral sampleRate
>
> sampleSize         :: (ZoneHeader, SFZone) → Double
> sampleSize (ZoneHeader{zhShdr}, SFZone{zStartOffs, zStartCoarseOffs, zEndOffs, zEndCoarseOffs})
>                                          = fromIntegral $ xEnd - xStart
>   where
>     F.Shdr{start, end}
>                      = zhShdr
>     xStart           = addIntToWord          start                  (sumOfMaybeInts
>                                                                        [zStartOffs, zStartCoarseOffs])
>     xEnd             = addIntToWord          end                    (sumOfMaybeInts [zEndOffs, zEndCoarseOffs])
>
> sampleLimitsOk         :: (Word, Word) → Bool
> sampleLimitsOk (st, en)                  = st >= 0 && en - st >= fst sampleLimits && en - st < 2^22
> sampleLoopLimitsOk     :: (Word, Word) → Bool
> sampleLoopLimitsOk (st, en)              = st >= 0 && en - st >= snd sampleLimits && en - st < 2^22

Note that harsher consequences of unacceptable sample header are enforced earlier. Logically, that would be
sufficient to protect below code from bad data and document the situation. But ... mechanism such as putting
out diagnostics might cause us to execute this code first. So, being crash-free/minimal in isStereoZone et al.

> isStereoInst, is24BitInst
>                        :: [(ZoneHeader, a)] → Bool
>
> isStereoInst zs                          = isJust $ find isStereoZone zs
>       
> isStereoZone z                           = isLeftSample z || isRightSample z
>
> -- see also zfindByBagIndex
>
> accessPreZone          :: Word -> (ZoneHeader, b) -> Maybe PreZoneKey
> accessPreZone wordF (ZoneHeader{zhwBag}, _)
>                                          = Just $ PreZoneKey wordF zhwBag  
> isLeftSample (ZoneHeader{zhShdr}, _)     = SampleTypeLeft == toSampleType (F.sampleType zhShdr)
> isRightSample (ZoneHeader{zhShdr}, _)    = SampleTypeRight == toSampleType (F.sampleType zhShdr)
>
> zoneConforms (ZoneHeader{zhShdr}, SFZone{zSampleMode, zInitQ, zScaleTuning, zExclusiveClass})
>                                          = not $ or unsupported
>   where
>     F.Shdr{end, start, endLoop, startLoop}
>                                          = zhShdr
>
>     unsupported        :: [Bool]
>     unsupported                          =
>       [
>           case zSampleMode of
>             Nothing                      → False
>             Just n                       → n == A.PressLoop
>         , case zInitQ of
>             Nothing                      → False
>             Just n                       → n /= 0
>         , case zScaleTuning of
>             Nothing                      → False
>             Just n                       → n /= 0 -- && n /= 100
>         , case zExclusiveClass of
>             Nothing                      → False
>             Just n                       → n /= 0
>         , end < start
>         , endLoop < startLoop
>       ]
>
> is24BitInst _                     = True -- was isJust $ ssM24 arrays       
 
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
>                           → Map PerGMKey PreInstrument
>                           → ([InstrumentName], [PercussionSound])
>                           → Map PreZoneKey PreZone
>                           → IO [(PerGMKey, InstCat)]
> categorize sffiles preInstCache rost preZoneCache
>                                          = CM.foldM winnow [] (Map.keys preInstCache)
>   where
>     winnow             :: [(PerGMKey, InstCat)] → PerGMKey → IO [(PerGMKey, InstCat)]
>     winnow target pergm@PerGMKey{}       = do
>       CM.when (isJust doShow) (putStrLn $ fromMaybe [] doShow)
>       return $ target ++ news
>       where
>         (cat, doShow)                    = categorizeInst pergm
>         news                             =
>           case cat of
>             InstCatInst _                → singleton (pergm, cat)
>             InstCatPerc _ _              → singleton (pergm, cat)
>             InstCatDisq _                → []
>
>     categorizeInst     :: PerGMKey → (InstCat, Maybe String)
>     categorizeInst pergm@PerGMKey{pgkwFile, pgkwInst}
>       | traceIf trace_CI False           = undefined
>       | otherwise                        =
>       (deJust "categorizeInst icat" icat, doShow >>= (\x → Just $ disqMsg ++ x))
>       where
>         doShow                           =
>           case icat of
>             Just (InstCatDisq reason)    → renderDisqReason reason
>             _                            → Nothing
>         disqMsg                          = unwords ["categorizeInst:", "skipping", show pgkwFile, show iName, ":"]
>
>         trace_CI                         =
>           unwords ["categorizeInst", show pgkwFile, iName, show pgkwInst, show (length zs)]
>  
>         SoundFontArrays{ssIBags, ssIGens, ssShdrs}
>                                          = zArrays (sffiles ! pgkwFile)
>
>         PreInstrument{iName, iMatches, iPreZones}
>                                          =
>           deJust "categorizeInst PreInstrument" (Map.lookup pergm preInstCache)
>         FFMatches{ffInst, ffPerc}        = iMatches
>
>         preZones                         = if length iPreZones < 2
>                                              then []
>                                              else tail iPreZones
>
>         zs             :: [(ZoneHeader, ZoneDigest)]
>         zs                               = map inspectZone preZones
>
>         (zsStereo, zsMono)               = partition isStereoZone zs
>         (zsOutbound, zsLocal)            = partition hasCross zsStereo
>         zsLessLocalRights                = zsMono ++ zsOutbound ++ filter isLeftSample zsLocal
>
>         icatAllKinds, icatRost, icatNarrow, icat
>                        :: Maybe InstCat
>         icatAllKinds                     = foldl' CM.mplus Nothing (alts Nothing allKinds)
>         icatRost                         = foldl' CM.mplus Nothing (alts icatAllKinds rost)
>         icatNarrow                       = Just (InstCatDisq DisqNarrow)
>         icat                             =
>           case (icatAllKinds, icatRost) of
>             (Just (InstCatInst _), Just (InstCatInst _))
>                                          → icatRost
>             (Just (InstCatPerc _ _), Just (InstCatPerc _ _))
>                                          → icatRost
>             (Just (InstCatDisq _), _)    → icatAllKinds
>             (_, Just (InstCatDisq _))    → icatRost
>             _                            → icatNarrow
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
>         corrupt        :: Maybe InstCat
>         corrupt                          =
>           foldl' byZone Nothing zs
>           where
>             byZone     :: Maybe InstCat → (ZoneHeader, ZoneDigest) → Maybe InstCat
>             byZone target (ZoneHeader{zhShdr}, ZoneDigest{zdKeyRange, zdVelRange})
>                                          =
>               let
>                 F.Shdr{sampleName}       = zhShdr
>               in
>                 foldl'
>                   CM.mplus
>                   target
>                   [checkGMRange zdKeyRange, checkGMRange zdVelRange]
>
>         checkGMRange   :: (Num a, Ord a) ⇒ Maybe (a, a) → Maybe InstCat
>         checkGMRange mrng                =
>           mrng >>= \(j, k) → if (0 <= j) && j <= k && k < fromIntegral qMidiSize128
>                                then Nothing
>                                else Just $ InstCatDisq DisqCorruptRange
>
>         hasRom (ZoneHeader{zhShdr}, _)   = F.sampleType zhShdr >= 0x8000
>                                          
>         checkLinkage   :: Maybe InstCat
>         checkLinkage                     =
>           if isSafe sList then Nothing else Just $ InstCatDisq DisqLinkage
>           where
>             sList      :: [(Int, Int)]
>             sList                        = map (\z → (extractIndex z, extractLink z)) zsLocal
>
>         extractIndex, extractLink
>                        :: (ZoneHeader, ZoneDigest) → Int
>         extractIndex (_, zd@ZoneDigest{zdSampleIndex})
>                                          = fromIntegral $ deJust "extractIndex" zdSampleIndex
>         extractLink (zh@ZoneHeader{zhShdr}, _)
>                                          = fromIntegral $ F.sampleLink zhShdr
>
>         rejectCrosses  :: Maybe InstCat
>         rejectCrosses                 =
>           if any hasCross zs then Just $ InstCatDisq DisqIllegalCrossover else Nothing
>
>         hasCross       :: (ZoneHeader, ZoneDigest) → Bool
>         hasCross z                       = isStereoZone z && notElem (extractLink z) (map extractLink zs)
>
>         howLaden       :: [Word] → Double
>         howLaden ws
>           | null zs                      = 0
>           | otherwise                    = (fromIntegral . length) ws / (fromIntegral . length) zs
>
>         maybeSettle    :: (Foldable t) ⇒ Fuzz → InstCat → t Fuzz → Maybe InstCat
>         maybeSettle thresh icat keys     = find (> thresh) keys >> Just icat
>
>         genericScore                     = evalAgainstGeneric iName

   "now", sequence through the alternatives, categorizing encountered instruments as follows:
   a. Just InstCatInst           an inst bearing one inst, or
   b. Just InstCatPerc           an inst bearing one or more percs, or
   c. Just InstCatDisq           an inst disqualified from tournaments, or
   d. Nothing                    undecided

>         alts           :: Maybe InstCat → ([InstrumentName], [PercussionSound]) → [Maybe InstCat]
>         alts seed rost                   =
>           if isNothing seed
>             then structuralAlts ++ alts2 allKinds
>             else alts2 rost
>
>           where
>             structuralAlts               =
>               [if null zs then Just (InstCatDisq DisqNoQualifiedZones) else Nothing
>               , corrupt
>               , if any hasRom zs then Just (InstCatDisq DisqRomBased) else Nothing
>               , if allowStereoCrossovers
>                   then Nothing
>                   else rejectCrosses
>               , checkLinkage
>               ]
>             alts2 rost          =
>               let
>                 ffInst'                  = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) ffInst
>                 ffPerc'                  = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) ffPerc
>                 uZones :: [Word]         = case seed of
>                                              Nothing                 → wZones
>                                              Just (InstCatPerc _ us) → us
>                                              _                       → []
>                 wZones :: [Word]         = mapMaybe (evalForPerc rost) zs
>                 maybeNailAsPerc
>                        :: Double → Maybe InstCat
>                 maybeNailAsPerc frac  =
>                   if frac < howLaden uZones
>                     then
>                       (if 0.05 < howLaden wZones
>                          then Just (catPerc wZones)
>                          else Just (catDisq DisqNoPercZones))
>                     else Nothing
>               in
>                 [ 
>                     maybeSettle isConfirmed catInst                  ffInst'
>                   , maybeSettle isConfirmed (catPerc wZones)         ffPerc'
>                   , maybeSettle stands      catInst                  ffInst'
>                   , maybeNailAsPerc 0.8 
>                   , maybeSettle stands      (catPerc wZones)         ffPerc'
>                   , maybeSettle stands      (catDisq DisqNarrow)     ffInst
>                   , maybeNailAsPerc 0.6 
>                   , if genericScore > 0 then Just catInst                else Nothing
>                   , if genericScore < 0 then Just (catPerc wZones)       else Nothing
>                   , Just $ catDisq DisqUnrecognized
>                 ]
>
>             catInst      :: InstCat      =
>               if length iPreZones < 2
>                 then InstCatDisq DisqNoQualifiedZones
>                 else InstCatInst iPreZones
>             catPerc      :: [Word] → InstCat
>             catPerc ws                   =
>               if null ws
>                 then InstCatDisq DisqNarrow
>                 else InstCatPerc iPreZones ws
>             catDisq    :: DisqReason → InstCat
>             catDisq                      = InstCatDisq
>
>         evalForPerc    :: ([InstrumentName], [PercussionSound]) → (ZoneHeader, ZoneDigest) → Maybe Word
>         evalForPerc rost' (ZoneHeader{zhwBag}, ZoneDigest{zdKeyRange})
>                                          = pinOut =<< integralize zdKeyRange
>           where
>             pinOut range                 = if pinnedKR (select rost') range then Just zhwBag else Nothing
>
>         inspectZone    :: PreZone → (ZoneHeader, ZoneDigest)
>         inspectZone pz@PreZone{pzBagIndex}
>           | traceNot trace_IZ False      = undefined
>           | otherwise                    = (zh, zd)
>           where
>             trace_IZ                     = unwords ["inspectZone", show pz, show zd]
>             xgeni                        = F.genNdx $ ssIBags!pzBagIndex
>             ygeni                        = F.genNdx $ ssIBags!(pzBagIndex + 1)
>
>             gens       :: [F.Generator]  = profess
>                                              (xgeni <= ygeni)
>                                              "SoundFont file corrupt (inspectZone gens)"
>                                              (map (ssIGens !) (deriveRange xgeni ygeni))
>
>             zd                           = formDigest gens
>             si                           = deJust "inspectZone si" (zdSampleIndex zd)
>             zh                           = ZoneHeader pzBagIndex (ssShdrs ! si)
>
> formZoneCache          :: Array Word SFFile
>                           → Map PerGMKey PreInstrument
>                           → Map PreZoneKey PreZone
>                           → ([InstrumentName], [PercussionSound])
>                           → [(PerGMKey, InstCat)]
>                           → IO (Map PerGMKey PerInstrument)
> formZoneCache sffiles preInstCache preZoneCache rost jobs
>                                          =
>   return $ foldr (\(p, i) → Map.insert p (computePerInst (p, i))) Map.empty jobs
>
>   where
>     computePerInst     :: (PerGMKey, InstCat) → PerInstrument
>     computePerInst (pergm@PerGMKey{pgkwFile, pgkwInst}, icat)
>       | traceIf trace_CPI False          = undefined
>       | otherwise                        = PerInstrument (gList ++ oList) {- (mapMaybe (`Map.lookup` preZoneCache)) -} smashup
>       where
>         sffile@SFFile{zFilename, zArrays}
>                                          = sffiles ! pgkwFile
>         PreInstrument{pInst, iName}      = deJust "computePerInst PreInstrument" (Map.lookup pergm preInstCache)
>
>         (pzs, oIx)                       = case icat of
>                                              InstCatPerc pzs_ ws   → (pzs_, ws)
>                                              InstCatInst pzs_      → (pzs_, tail allIx)
>                                              _                     → error "only Inst and Perc are valid here"
>         allIx                            = map pzBagIndex pzs
>         gList                            = [buildZone sffile defZone (head allIx)]
>         gZone                            = (snd . head)                                 gList
>         oList                            = map (buildZone sffile gZone)                 oIx
>
>         trace_CPI                        = unwords ["computePerInst", show pgkwFile, iName]
>
>         smashup        :: Smashing Word
>         smashup@Smashing{ .. }           = smashSubspaces "smashup" dims spaces
>           where
>             dims       :: [Word]
>             spaces     :: [(Word, [Maybe (Word, Word)])]
>             dims                         = [fromIntegral qMidiSize128, fromIntegral qMidiSize128] 
>             spaces                       =
>               map (\(ZoneHeader{ .. }, SFZone{ .. }) → (zhwBag, [integralize zKeyRange, integralize zVelRange])) oList
>
> buildZone              :: SFFile → SFZone → Word → (ZoneHeader, SFZone)
> buildZone SFFile{zArrays, zFilename, zWordF} fromZone bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = (zh, zone)
>   where
>     SoundFontArrays{ssIBags, ssIGens, ssIMods, ssShdrs}
>                                          = zArrays
>
>     xgeni                                = F.genNdx $ ssIBags!bagIndex
>     ygeni                                = F.genNdx $ ssIBags!(bagIndex + 1)
>     xmodi                                = F.modNdx $ ssIBags!bagIndex
>     ymodi                                = F.modNdx $ ssIBags!(bagIndex + 1)
>
>     gens       :: [F.Generator]          =
>       profess
>         (xgeni <= ygeni)
>         (unwords["SoundFont file", show zWordF, zFilename, "corrupt (buildZone gens)"])
>         (map (ssIGens !) (deriveRange xgeni ygeni))
>     mods       :: [(Word, F.Mod)]        =
>       profess
>         (xmodi <= ymodi)
>         (unwords["SoundFont file", show zWordF, zFilename, "corrupt (buildZone mods)"])
>         (zip [10_000..] (map (ssIMods !) (deriveRange xmodi ymodi)))
>
>     zone@SFZone{zSampleIndex}            = foldr addMod (foldl' addGen fromZone gens) mods
>     sh@F.Shdr{sampleName}                = ssShdrs ! fromMaybe 0 zSampleIndex
>     zh                                   = ZoneHeader bagIndex sh
>
>     trace_BZ                             = unwords ["buildZone", show zWordF, show bagIndex, show (fromZone == defZone)]
>

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
> instrumentSF sfrost@SFRoster{zFiles, zPreInstCache, zZoneCache} pergm@PerGMKey{pgkwFile, pgkwInst} dur pchIn volIn params
>   | traceNow trace_ISF False             = undefined
>   | otherwise                            = eutSynthesize (reconX, mreconX) rSampleRate
>                                              dur pchOut volOut params
>                                              (ssData arrays) (ssM24 arrays)
>   where
>     noon@NoteOn{ .. }                    = NoteOn
>                                              (clip (0, 127) volIn)
>                                              (clip (0, 127) pchIn)
>
>     pchOut              :: AbsPitch      = maybe noteOnKey (clip (0, 127)) rForceKey
>     volOut              :: Volume        = maybe noteOnVel (clip (0, 127)) rForceVel
>
>     arrays                               = zArrays (zFiles ! pgkwFile)
>
>     PreInstrument{iPreZones, pInst}      = deJust "instrumentSF1" (Map.lookup pergm zPreInstCache)
>     PerInstrument{pZones, pSmashing}     = deJust "instrumentSF2" (Map.lookup pergm zZoneCache)
>
>     trace_ISF                            =
>       unwords ["instrumentSF", show pgkwFile, F.instName pInst, show (pchIn, volIn), show dur]
>
>     (reconX@Recon{rSampleRate, rForceKey, rForceVel}, mreconX)
>                                          =
>       case setZone of
>         Left (zL, sL)                    → (recon zL sL noon, Nothing)
>         Right ((zL, sL), (zR, sR))       → reconLR ((zL, sL), (zR, sR)) noon

zone selection for rendering ==========================================================================================

>     setZone            :: Either (SFZone, F.Shdr) ((SFZone, F.Shdr), (SFZone, F.Shdr))
>     setZone                              = eor
>       where
>         zs                               = tail pZones
>         ezones                           = selectZoneConfig zs $ selectBestZone zs noon
>         eor                              =
>           case ezones of 
>             Left (zhL, zoneL)            → Left (zoneL, zhShdr zhL)
>             Right ((zhL, zoneL), (zhR, zoneR))
>                                          → Right ((zoneL, zhShdr zhL), (zoneR, zhShdr zhR))
>
>     selectBestZone     :: [(ZoneHeader, SFZone)] → NoteOn → (ZoneHeader, SFZone)
>     selectBestZone zs noon               = deJust "whichZ" (zfindByBagIndex zs bagId)
>       where
>         (bagId, _)                       = lookupCellIndex (noonAsCoords noon) pSmashing
>
>     selectZoneConfig     :: [(ZoneHeader, SFZone)] → (ZoneHeader, SFZone)
>                           → Either (ZoneHeader, SFZone) ((ZoneHeader, SFZone), (ZoneHeader, SFZone))
>     selectZoneConfig zs zone
>        | stype == SampleTypeLeft         = Right (zone, ozone)
>        | stype == SampleTypeRight        = Right (ozone, zone)
>        | otherwise                       = Left zone
>        where
>          F.Shdr{sampleType}              = (zhShdr . fst) zone
>          stype                           = toSampleType sampleType
>          ozone                           = deJust "ozone" (locateStereoPartner sfrost pergm zs zone)
>
> locateStereoPartner    :: SFRoster → PerGMKey → [(ZoneHeader, SFZone)]
>                           → (ZoneHeader, SFZone) → Maybe (ZoneHeader, SFZone)
> locateStereoPartner SFRoster{zPreZoneCache, zZoneCache} PerGMKey{pgkwFile} zs z
>   | traceIf trace_LSP False              = undefined
>   | otherwise                            =
>   case toSampleType (F.sampleType myShdr) of
>     SampleTypeLeft                       → partner
>     SampleTypeRight                      → partner
>     _                                    → error $ unwords["locateStereoPartner", "attempted on non-stereo zone"]
>   where
>     trace_LSP                            = unwords ["locateStereoPartner", show (isJust partner)]
>
>     (ZoneHeader{zhShdr = myShdr}, _)     = z
>     targetSampleIx                       = F.sampleLink myShdr
>     partner                              = zfindBySampleIndex zs targetSampleIx `CM.mplus` hardWay
>
>     hardWay                              =
>       accessPreZone pgkwFile z
>       >>= (`Map.lookup` zPreZoneCache)
>       >>= pzmkPartner
>       >>= (`Map.lookup` zPreZoneCache)
>       >>= (Just . pzPergm)
>       >>= (`Map.lookup` zZoneCache)
>       >>= Just . pZones
>       >>= (`zfindBySampleIndex` targetSampleIx)

reconcile zone and sample header ======================================================================================

> reconLR                :: ((SFZone, F.Shdr), (SFZone, F.Shdr)) → NoteOn → (Recon, Maybe Recon)
> reconLR ((zoneL, shdrL), (zoneR, shdrR)) noon
>   | traceNever trace_RLR False           = undefined
>   | otherwise                            = (recL, Just recR')
>   where
>     recL@Recon{rRootKey = rkL, rPitchCorrection = pcL}
>                                          = recon zoneL shdrL noon
>     recR                                 = recon zoneR shdrR noon
>     recR'                                = recR{
>                                                rRootKey                   = rkL
>                                              , rPitchCorrection           = pcL}
>
>     trace_RLR                            = unwords ["reconLR:\n", show zoneL, "\n", show shdrL]
>
> recon                  :: SFZone → F.Shdr → NoteOn → Recon 
> recon zone@SFZone{ .. } sHdr@F.Shdr{ .. } noon@NoteOn{ .. }
>                                          = reconL
>   where
>     m8n                                  = reconModulation zone sHdr noon
>
>     reconL = Recon {
>     rSampleMode      = fromMaybe             A.NoLoop                zSampleMode
>   , rSampleRate      = fromIntegral          sampleRate
>   , rStart           = addIntToWord          start                  (sumOfMaybeInts
>                                                                        [zStartOffs, zStartCoarseOffs])
>   , rEnd             = addIntToWord          end                    (sumOfMaybeInts
>                                                                        [zEndOffs, zEndCoarseOffs])
>   , rLoopStart       = addIntToWord          startLoop              (sumOfMaybeInts
>                                                                        [zLoopStartOffs, zLoopStartCoarseOffs])
>   , rLoopEnd         = addIntToWord          endLoop                (sumOfMaybeInts
>                                                                        [zLoopEndOffs, zLoopEndCoarseOffs])
>   , rRootKey         = fromIntegral $ fromMaybe
>                                              originalPitch           zRootKey
>   , rForceKey        = fmap                  fromIntegral            zKey
>   , rForceVel        = fmap                  fromIntegral            zVel
>   , rTuning          = fromMaybe             100                     zScaleTuning
>   , rNoteOn          = noon
>   , rAttenuation     = reconAttenuation                              zInitAtten
>   , rVolEnv          = deriveEnvelope                                zDelayVolEnv
>                                                                      zAttackVolEnv
>                                                                      noon
>                                                                      (zHoldVolEnv,  zKeyToVolEnvHold)
>                                                                      (zDecayVolEnv, zKeyToVolEnvDecay)
>                                                                      zSustainVolEnv
>                                                                      zReleaseVolEnv
>                                                                      Nothing
>   , rPitchCorrection = if usePitchCorrection
>                          then Just $ reconPitchCorrection            pitchCorrection
>                                                                      zCoarseTune
>                                                                      zFineTune
>                          else Nothing
>
>   , rM8n             =                                                m8n
>   , rEffects         = deriveEffects                                  m8n
>                                                                       noon
>                                                                       zChorus
>                                                                       zReverb
>                                                                       zPan}
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
> reconModulation        :: SFZone → F.Shdr → NoteOn → Modulation
> reconModulation SFZone{ .. } F.Shdr{sampleName} noon
>   | traceIf trace_RM False               = undefined
>   | otherwise                            = resolveMods m8n zModulators defaultMods
>   where
>     trace_RM                             = unwords ["reconModulation", sampleName]
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
>     curKernelSpec@KernelSpec{ .. }       =
>       KernelSpec
>         (maybe 13_500 (clip (1_500, 13_500)) zInitFc)
>         (maybe 0      (clip (0,     960))    zInitQ)
>         1 
>         useFastFourier
>         (-1) -- must always be replaced
>
>     resonanceType      :: ResonanceType  = if lowpassFc (mLowpass m8n) < 10_000
>                                              then loCutoffReson
>                                              else hiCutoffReson
>
>     nModEnv            :: Maybe Envelope = deriveEnvelope
>                                              zDelayModEnv
>                                              zAttackModEnv
>                                              noon
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
>     showPerGM          :: PerGMScored → [Emission]
>     showPerGM PerGMScored{szI, mszP, pPerGMKey}
>                                          = [emitShowL pgkwFile 4] ++ [ToFieldL szI 22] ++ showmZ
>       where
>         PerGMKey{pgkwFile}               = pPerGMKey
>         showmZ                           = maybe [] showZ mszP
>         showZ name                       = [Unblocked name]
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
>                             , emitShowR      szI                      22
>                   , Blanks 4, emitShowR      (fromMaybe "" mszP)      22
>                   , Blanks 4, emitShowL      pScore                   15
>                             , ToFieldL       showEmp                   n
>                             , emitShowR      showAkr                   8]
>
> showDisqMsgs   :: [String] → IO ()
> showDisqMsgs                             = mapM_ showIfThere
>   where
>     showIfThere str                      = CM.unless (null str) (putStrLn str)
>
> data DisqReason                          =
>     DisqUnrecognized
>   | DisqNoQualifiedZones
>   | DisqCorruptId String String
>   | DisqCorruptHeader 
>   | DisqNarrow
>   | DisqRomBased
>   | DisqNoPercZones
>   | DisqLinkage
>   | DisqCorruptRange
>   | DisqOverRanges
>   | DisqIllegalCrossover deriving Show
>
> qqIncludeUnused          :: Bool         = False
>
> renderDisqReason         :: DisqReason → Maybe String
> renderDisqReason DisqUnrecognized        = Just (unwords["unrecognized"])
> renderDisqReason DisqNoQualifiedZones    = Just (unwords["no qualified zones"])
> renderDisqReason (DisqCorruptId strType strName)
>                                          = Just (unwords["corrupt", strType, strName])
> renderDisqReason DisqCorruptHeader       = Just (unwords["corrupt header; e.g. sample rate"])
> renderDisqReason DisqNarrow              = if qqIncludeUnused then Just (unwords["narrow inst scope"]) else Nothing
> renderDisqReason DisqRomBased            = Just (unwords["ROM-based"])
> renderDisqReason DisqNoPercZones         = if qqIncludeUnused then Just (unwords["unused zone liat"]) else Nothing
> renderDisqReason DisqLinkage             = Just (unwords["zone linkage"])
> renderDisqReason DisqCorruptRange        = Just (unwords["illegal zone range"])
> renderDisqReason DisqOverRanges          = Just (unwords["overlapping zone ranges"])
> renderDisqReason DisqIllegalCrossover    = Just (unwords["illegal crossover"])
>
> data SoundFontSettings =
>   SoundFontSettings {
>     qqAllowStereoCrossovers              :: Bool
>   , qqAllowOverlappingRanges             :: Bool
>   , qqMultipleCompetes                   :: Bool} deriving Show
>
> allowStereoCrossovers                    = qqAllowStereoCrossovers      defF
> -- stereo pair can come from 2 different instruments
> allowOverlappingRanges                   = qqAllowOverlappingRanges     defF
> -- more than one zone can reference a given range cell
> multipleCompetes                         = qqMultipleCompetes           defF
>
> defF                   :: SoundFontSettings
> defF =
>   SoundFontSettings {
>     qqAllowStereoCrossovers              = True
>   , qqAllowOverlappingRanges             = True
>   , qqMultipleCompetes                   = True}

The End
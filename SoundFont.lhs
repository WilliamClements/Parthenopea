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
>   , sPartner           :: Maybe PreSampleKey} deriving Show
>
> data PreZoneKey =
>   PreZoneKey {
>     pzkwFile           :: Word
>   , pzkwBag            :: Word} deriving (Eq, Ord, Show)
> defPreZoneKey                            = PreZoneKey 0 0
>
> data PreZone =
>   PreZone {
>     pzShdr             :: F.Shdr
>   , pzWordF            :: Word
>   , pzWordI            :: Word
>   , pzBix              :: Word
>   , pzDigest           :: ZoneDigest
>   , pzmkPartner        :: Maybe PreZoneKey} deriving (Eq, Show)
> makePreZone shdr wF wI bix zd            = PreZone shdr wF wI bix zd Nothing
> extractInstKey         :: PreZone → PerGMKey
> extractInstKey pz                        = PerGMKey pz.pzWordF pz.pzWordI Nothing
> extractSpace           :: PreZone → (Word, [Maybe(Word, Word)])
> extractSpace pz                          = (pz.pzBix, [pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange])
> showPreZones           :: [PreZone] → String
> showPreZones pzs                         = show $ map pzBix pzs
>
> data PreInstrument                       =
>   PreInstrument {
>     pInst              :: F.Inst
>   , iName              :: String
>   , iMatches           :: FFMatches
>   , iGlobalPreZoneKey  :: PreZoneKey}
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
>   , pKind              :: Kind
>   , pAgainstKindResult :: AgainstKindResult
>   , pPerGMKey          :: PerGMKey
>   , szI                :: String
>   , mszP               :: Maybe String} deriving (Show)
>
> data WinningRecord                       =
>   WinningRecord {
>     pWinningI          :: Map InstrumentName PerGMScored
>   , pWinningP          :: Map PercussionSound PerGMScored
>   , pWarnings          :: [String]} deriving Show
>
> seedWinningRecord      :: WinningRecord
> seedWinningRecord                        = WinningRecord Map.empty Map.empty []
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
> findByBagIndex pzs w                     = find (\PreZone{pzBix} → w == pzBix) pzs
>
> findByBagIndex'        :: [(PreZone, a)] → Word → Maybe (PreZone, a)
> findByBagIndex' zs w                     = find (\(pz, _) → w == pz.pzBix) zs
>
> data PerInstrument                       =
>   PerInstrument {
>     pZones             :: [(PreZone, SFZone)]
>   , pSmashing          :: Smashing Word}

Instrument categories: instrument, percussion, disqualified

> data InstCat                             =
>        InstCatInst InstCatData
>      | InstCatPerc InstCatData
>      | InstCatDisq DisqReason deriving Show
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
>   splitScore kind pzs                    = fromIntegral (length pzs)
>   fuzzFactor _                           = 7/8
>
> instance SFScorable PercussionSound where
>   splitScore kind _                      = 1
>   fuzzFactor _                           = 3/4
>
> data SFRoster                            =
>   SFRoster {
>     zFiles             :: Array Word SFFile
>   , zPreSampleCache    :: Map PreSampleKey PreSample
>   , zPreZoneCache      :: Map PreZoneKey PreZone
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>   , zRost              :: ([InstrumentName], [PercussionSound])
>   , zPerInstCache      :: Map PerGMKey PerInstrument
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
>   , zdSampleIndex      :: Maybe Word} deriving (Eq, Show)
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

> -- (removed...revisit when things settle down)
> profileSF2s            :: IO ()
> profileSF2s                              = print "removed"

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
>     -- track the complete _qualified_ populations of: samples, instruments, percussion
>     finishRoster       :: SFRoster → IO SFRoster
>     finishRoster preR  = do
>       tsStarted        ← getCurrentTime
>
>       putStrLn "initSamples"
>       preSampleCache   ← initSamples preR.zFiles
>       putStrLn "initInsts"
>       preInstCache_    ← initInsts preR.zFiles
>       putStrLn "initZones"
>       (preZoneCache, preInstCache)
>                        ← initZones preR.zFiles preSampleCache preInstCache_
>       inverter         ← associateZones preZoneCache
>
>       jobs             ← categorize preR.zFiles preInstCache inverter preR.zRost preZoneCache
>       tsCatted         ← getCurrentTime
>       putStrLn ("___categorize: " ++ show (diffUTCTime tsCatted tsStarted))
>
>       zc               ← formZoneCache preR.zFiles preInstCache preZoneCache preR.zRost jobs
>       reinverter       ← reassociateZones zc
>
>       (pergmsI, pergmsP)
>                        ← sortByCategory preInstCache jobs
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
>                        ← decideWinners preR.zFiles preSampleCache preInstCache reinverter preZoneCache
>                                        zc preR.zRost pergmsI pergmsP
>       tsDecided        ← getCurrentTime
>       putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsZoned))
>
>       CM.when reportTourney (writeTournamentReport preR.zFiles wI wP)
>       tsReported       ← getCurrentTime
>
>       let wins@WinningRecord{pWarnings}
>                        = WinningRecord (Map.map head wI) (Map.map head wP) (sI ++ sP)
>
>       -- print song/orchestration info to user (can be captured by redirecting standard out)
>       mapM_ putStrLn pWarnings
>
>       let sfrost       = preR{ zPreSampleCache   = preSampleCache
>                              , zPreZoneCache     = preZoneCache
>                              , zPreInstCache     = preInstCache
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
>       print $ Map.keys preInstCache
>
>       mapM_ putStrLn (reverse errs)
>       return preInstCache
>
>     initZones          :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PerGMKey PreInstrument
>                           → IO (Map PreZoneKey PreZone, Map PerGMKey PreInstrument)
>     initZones sffiles preSampleCache preInstCache_
>                        = do
>       (preZoneCache, preInstCache, errs)
>                        ← formPreZoneCache sffiles preSampleCache preInstCache_
>
>       mapM_ putStrLn (reverse errs)
>       return (preZoneCache, preInstCache)
>
>     associateZones     :: Map PreZoneKey PreZone → IO (Map PerGMKey [PreZone])
>     associateZones preZoneCache
>                        = do
>       print "associateZones"
>       return inverter
>       
>       where
>         inverter       :: Map PerGMKey [PreZone]
>         inverter                        = Map.foldlWithKey inverseFolder Map.empty preZoneCache
>
>         inverseFolder  :: Map PerGMKey [PreZone] → PreZoneKey → PreZone → Map PerGMKey [PreZone]
>         inverseFolder target key newpz
>           | traceNot trace_IF False    = undefined
>           | otherwise                    = Map.insert pergm now target
>           where
>             pergm                        = extractInstKey newpz
>             trace_IF                     =
>               unwords ["inverseFolder soFar", show key, show pergm, show $ length now]
>             soFar, now :: [PreZone]
>             soFar                        = fromMaybe [] (Map.lookup pergm target)
>             now                          = newpz : soFar
>
>     reassociateZones    :: Map PerGMKey PerInstrument → IO (Map PerGMKey [PreZone])
>     reassociateZones mpre                = return $ Map.map (\q → map fst q.pZones) mpre
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
>   let esFiles          = emitFileListC
>   let esI              = concatMap dumpContestants (Map.toList pContI)
>   let esP              = concatMap dumpContestants (Map.toList pContP)
>   let esQ              = emitSettingses
>   let esTail           = singleton $ Unblocked "\n\nThe End\n\n"
>   let eol              = singleton EndOfLine
>
>   writeFileBySections reportName [esFiles, legend, esI, eol, esFiles, legend, esP, esQ, esTail]
>
>   tsFinished           ← getCurrentTime
>
>   putStrLn ("___report tournament results: " ++ show (diffUTCTime tsFinished tsStarted))
>   traceIO ("wrote " ++ reportName)
>
>   where
>     emitFileListC      = concatMap (uncurry doF) (zip [0..] (toList sffiles))
>     doF nth sffile     = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>         
> decideWinners          :: Array Word SFFile
>                           → Map PreSampleKey PreSample
>                           → Map PerGMKey PreInstrument
>                           → Map PerGMKey [PreZone]
>                           → Map PreZoneKey PreZone
>                           → Map PerGMKey PerInstrument
>                           → ([InstrumentName], [PercussionSound]) 
>                           → [PerGMKey]
>                           → [PerGMKey]
>                           → IO ((Map InstrumentName [PerGMScored], [String])
>                               , (Map PercussionSound [PerGMScored], [String]))
> decideWinners sffiles preSampleCache preInstCache inverter preZoneCache zc rost pergmsI pergmsP
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
>         pzs                              = deJust "pzs" (Map.lookup pergmI inverter)
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
>         trace_WP                         = unwords ["wpFolder", show preI.iName, show pergmP, "of", show (length perI.pZones)]
>
>         preI                             =
>           deJust (unwords["wpFolder", "PreInst"]) (Map.lookup (pergmP{pgkwBag = Nothing}) preInstCache)
>         pzs                              = deJust "pzs" (Map.lookup pergmP{pgkwBag = Nothing} inverter)
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

tournament among GM instruments and percussion from SoundFont files ===================================================

>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           Map a Fuzz
>                           → PerGMKey
>                           → [SSHint]
>                           → (Map a [PerGMScored], [String])
>                           → a
>                           → (Map a [PerGMScored], [String])
>     xaEnterTournament fuzzMap pergm hints (wins, ss) kind
>       | traceIf trace_XAET False         = undefined
>       | otherwise                        = (Map.insert kind now wins, ss)
>       where
>         pergm_                           = pergm{pgkwBag = Nothing}
>         preI                             =
>           deJust (unwords["xaEnterTournament", "PreInstrument"]) (Map.lookup pergm_ preInstCache)
>         perI                             =
>           deJust (unwords["xaEnterTournament", "PerInstrument"]) (Map.lookup pergm_ zc)
>
>         scope_, scope  :: [(PreZone, SFZone)]
>         scope_                           =
>           case pergm.pgkwBag of
>             Nothing                      → perI.pZones
>             Just bagI                    →
>                    maybe
>                      (error $ unwords [ "xaEnterTournament: findByBagIndex' returned a Nothing for"
>                                       , show pergm.pgkwFile, preI.iName, show bagI])
>                      singleton
>                      (findByBagIndex' perI.pZones bagI)
>         scope                            =
>           profess
>             (not (null scope_))
>             (unwords["xaEnterTournament", "null scope", preI.iName])
>             scope_
>             
>
>         mnameZ         :: Maybe String   = pergm.pgkwBag
>                                            >>= findByBagIndex' perI.pZones
>                                            >>= \(q, _) → Just (F.sampleName q.pzShdr)
>         zName                            = deJust "mnameZ2" mnameZ
>
>         trace_XAET                       =
>           unwords ["xaEnterTournament", preI.iName, fromMaybe "" mnameZ, show kind]
>
>         computeGrade   :: [(PreZone, SFZone)] → AgainstKindResult → ArtifactGrade
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
>         soFar, now     :: [PerGMScored]
>         soFar                            = fromMaybe [] (Map.lookup kind wins)
>         now                              = scoredP : soFar
>         scoredP        :: PerGMScored    =
>           PerGMScored (computeGrade scope akResult) (toKind kind) akResult pergm preI.iName mnameZ
>
>         akResult                         = fromMaybe 0 (Map.lookup kind fuzzMap)
>
> formMasterSampleList   :: Array Word SFFile → IO [PreSampleKey]
> formMasterSampleList sffiles             = return $ concatMap formFS sffiles
>   where
>     formFS             :: SFFile → [PreSampleKey]
>     formFS sffile                        =
>       let
>         sfa                              = sffile.zArrays
>         (st, en)       :: (Word, Word)   = bounds sfa.ssShdrs
>       in
>         map (PreSampleKey sffile.zWordF) (deriveRange st en)
>
> formPreSampleCache     :: Array Word SFFile
>                           → [PreSampleKey]
>                           → IO (Map PreSampleKey PreSample, [String])
> formPreSampleCache sffiles presks        = return $ foldl' preSFolder (Map.empty, []) presks
>   where
>     computePreSample   :: Word → F.Shdr → (Maybe PreSample, String)
>     computePreSample wF shdr@F.Shdr{ .. }
>       | traceNever trace_FPSC False      = undefined
>       | otherwise                        = 
>       if goodName sampleName
>          && sampleRate >= 64
>          && sampleRate < 2^20
>          && isJust (toMaybeSampleType sampleType)
>          && sampleLimitsOk (start, end)
>          && sampleLoopLimitsOk (startLoop, endLoop)
>         then (Just $ PreSample sampleName (computeFFMatches sampleName) Nothing, [])
>         else (Nothing, diagnose wF shdr)
>       where
>         trace_FPSC                       = unwords ["computePreSample", sampleName]
>
>     diagnose           :: Word → F.Shdr → String
>     diagnose wF fshdr@F.Shdr{ .. } 
>                                          =
>       unwords ["formPreSampleCache", "problem", show wF, show (toMaybeSampleType sampleType), show fshdr]
>
>     loadShdr PreSampleKey{pskwSample, pskwFile}
>                                          = sfa.ssShdrs ! pskwSample
>       where
>         sffile                           = sffiles ! pskwFile
>         sfa                              = sffile.zArrays
>
>     preSFolder :: (Map PreSampleKey PreSample, [String]) → PreSampleKey → (Map PreSampleKey PreSample, [String])
>     preSFolder (target, errs) presk@PreSampleKey{pskwFile, pskwSample}
>                                          =
>       let
>         (mpres, err)                     = (computePreSample pskwFile . loadShdr) presk
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
>       | isJust other                     = Left (key, val{sPartner = Just otherKey})
>       | otherwise                        =
>       Right $ unwords ["finishPreSampleCache", "missing stereo partner", show key]
>       where
>         otherKey                         = PreSampleKey pskwFile shdr.sampleLink
>         other                            = Map.lookup otherKey preSampleCache 
>
>         sffile                           = sffiles ! pskwFile
>         sfa                              = sffile.zArrays
>         shdr                             = sfa.ssShdrs ! pskwSample
>         stype                            = toSampleType shdr.sampleType
>         stereo                           = SampleTypeLeft == stype || SampleTypeRight == stype
>
> data ZoneScanRecord                      =
>   ZoneScanRecord {
>     zswFile            :: Word
>   , zswInst            :: Word
>   , zswBix             :: Word
>   , zsPreZones         :: [(PreZoneKey, PreZone)]}
> instance Show ZoneScanRecord where
>   show (ZoneScanRecord{ .. })            = unwords ["ZoneScanRecord", show zswFile, show zswInst, show zswBix]
>
> extractInstKeyFromZSR zsr                = PerGMKey zsr.zswFile zsr.zswInst Nothing
>         
> formPreZoneCache       :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PerGMKey PreInstrument
>                           → IO (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
> formPreZoneCache sffiles preSampleCache preInstCache
>                                          = return $ foldl' formFZ (Map.empty, preInstCache, []) sffiles
>   where
>     -- FZ = file to zone
>     formFZ             :: (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
>                           → SFFile
>                           → (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
>     formFZ (pzcache_, picache_, errs_) sffile
>                                          =
>       let
>         (pzcache, picache, errs)         = extract sffile
>       in
>         (Map.union pzcache pzcache_, picache, errs_ ++ errs)
>          
>     extract            :: SFFile → (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
>     extract sffile                       = (Map.fromList allpzs, foldl' markGlobalZone newPreInstCache goodzsrs, errs)
>       where
>         sfa                              = sffile.zArrays
>         (stI, enI)     :: (Word, Word)   = bounds sfa.ssInsts
>
>         results                          = map capture (deriveRange stI enI)
>         goodzsrs                         = lefts results
>         errs                             = map fst (rights results)
>         badzsrs                          = tracer "badzsrs" (map snd (rights results))
>
>         allpzs                           = foldl' (\x y → x ++ y.zsPreZones ) [] goodzsrs
>         newPreInstCache                  = foldl' shrink preInstCache badzsrs
>
>         shrink         :: Map PerGMKey PreInstrument → ZoneScanRecord → Map PerGMKey PreInstrument
>         shrink preIs badzsr              = Map.delete (extractInstKeyFromZSR badzsr) preIs
>
>         capture         :: Word → Either ZoneScanRecord (String, ZoneScanRecord)
>         capture wI                       = if isJust target
>                                              then Left zsr
>                                              else Right (unwords ["formPreZoneCache", "problem"], zsr)
>           
>           where
>             target                       = Map.lookup (PerGMKey sffile.zWordF wI Nothing) preInstCache
>
>             iinst                        = sfa.ssInsts ! fromIntegral wI
>             jinst                        = sfa.ssInsts ! fromIntegral (wI+1)
>
>             ibagi                        = F.instBagNdx iinst
>             jbagi                        = F.instBagNdx jinst
>
>             zsr                          = ZoneScanRecord sffile.zWordF wI ibagi pzsRemaining
>             pzsRemaining                 = mapMaybe produce (deriveRange (ibagi+1) jbagi)
>
>             produce    :: Word → Maybe (PreZoneKey, PreZone)
>             produce bix                  =
>               Map.lookup (PreSampleKey sffile.zWordF si) preSampleCache >> pzPair
>               where
>                 xgeni                    = F.genNdx $ sfa.ssIBags ! bix
>                 ygeni                    = F.genNdx $ sfa.ssIBags ! (bix + 1)
>
>                 gens   :: [F.Generator]  = profess
>                                              (xgeni <= ygeni)
>                                              "SoundFont file corrupt (computePreZone gens)"
>                                              (map (sfa.ssIGens !) (deriveRange xgeni ygeni))
>
>                 zd                       = formDigest gens
>                 si                       = deJust "produce si" zd.zdSampleIndex
>                 shdr                     = sfa.ssShdrs ! si
>                 pzPair                   =
>                   Just (PreZoneKey sffile.zWordF bix, makePreZone shdr sffile.zWordF wI bix zd)
>
>     markGlobalZone     :: Map PerGMKey PreInstrument → ZoneScanRecord → Map PerGMKey PreInstrument
>     markGlobalZone preic zsr
>       | traceNot trace_MGZ False         = undefined
>       | otherwise                        =
>       Map.insert pergm oldpreI{iGlobalPreZoneKey = PreZoneKey zsr.zswFile zsr.zswBix} preic
>       where
>         pergm                            = PerGMKey zsr.zswFile zsr.zswInst Nothing
>         moldpreI                         = Map.lookup pergm preic
>         oldpreI                          = deJust (unwords["mold", show pergm]) moldpreI
>         trace_MGZ                        = unwords ["markGlobalZone", show zsr.zswBix, show pergm]
>         
> formPreInstCache       :: Array Word SFFile → [PerGMKey] → IO (Map PerGMKey PreInstrument, [String])
> formPreInstCache sffiles pergms          = return $ foldl' preIFolder (Map.empty, []) pergms
>   where
>     computePreInst     :: F.Inst → Either PreInstrument String
>     computePreInst iinst                 =
>       if goodName iinst.instName
>         then Left $ PreInstrument iinst iinst.instName (computeFFMatches iinst.instName) defPreZoneKey
>         else Right $ unwords ["formPreInstCache", "illegal name", show iinst.instName]
>
>     loadInst           :: PerGMKey -> F.Inst
>     loadInst PerGMKey{pgkwFile, pgkwInst}
>                                          = sfa.ssInsts ! pgkwInst
>       where
>         sffile                           = sffiles ! pgkwFile
>         sfa                              = sffile.zArrays
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
>     formFI sffile                        =
>       let
>         sfa                              = sffile.zArrays
>         boundsI                          = bounds sfa.ssInsts
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
>           | (jbagi - ibagi) >= 2         = Just $ PerGMKey sffile.zWordF wordI Nothing
>           | otherwise                    = Nothing
>           where
>             iinst                        = sfa.ssInsts ! wordI
>             jinst                        = sfa.ssInsts ! (wordI+1)
>             ibagi                        = F.instBagNdx iinst
>             jbagi                        = F.instBagNdx jinst
>       in
>         mapMaybe qualifyInst rangeI
>
> sortByCategory         :: Map PerGMKey PreInstrument
>                           → [(PerGMKey, InstCat)]
>                           → IO ([PerGMKey], [PerGMKey])
> sortByCategory preInstCache jobs         = return $ foldl' catFolder ([], []) jobs
>   where
>     catFolder            :: ([PerGMKey], [PerGMKey]) → (PerGMKey, InstCat) → ([PerGMKey], [PerGMKey])
>     catFolder (pergmsI, pergmsP) (pergmI_, icat)
>                                          =
>       let
>         pergmI                           = pergmI_{pgkwBag = Nothing}
>       in
>         case icat of
>           InstCatPerc icd                → (pergmsI, pergmsP ++ instrumentPercList pergmI icd.inPercBixen)
>           InstCatInst _                  → (pergmI : pergmsI, pergmsP)
>           _                              → error $ unwords ["sortByCategory", "illegal input", show icat]
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
>       let sfa                            =
>             SoundFontArrays
>               (F.insts pdata) (F.ibags pdata)
>               (F.igens pdata) (F.imods pdata)
>               (F.shdrs pdata) (F.smpl  sdata) (F.sm24  sdata)
>       let sffile                         = SFFile filename sfa wFile
>       let nBits                          =
>             case sfa.ssM24 of
>               Nothing                    → 16
>               Just s24data               → 24
>       ts2                                ← getCurrentTime
>       CM.when diagnosticsEnabled (
>         putStrLn $ unwords [
>                           "openSoundFontFile"
>                       ,   "insts,bags,gens,mods,shdrs"
>                       ,   show $ length sfa.ssInsts
>                       ,   show $ length sfa.ssIBags
>                       ,   show $ length sfa.ssIGens
>                       ,   show $ length sfa.ssIMods
>                       ,   show $ length sfa.ssShdrs ])
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
>                           → [(PreZone, SFZone)]
>                           → Double
> computeResolution kind rost zs
>   | traceNot trace_CR False              = undefined
>   | null zs                              = error $ unwords ["null zs"]
>   | otherwise                            =
>   fromRational m1 * measureSplits kind + fromRational m2 * measureSampleSize
>   where
>     theSplit                             = splitScore kind (map fst zs)
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
> sampleDurationScoring  :: (PreZone, SFZone) → Double
> sampleDurationScoring (pz, zone)         = if score < 0.01 then -10 else score
>   where
>     shdr                                 = deJust "sampleDurationScoring" 
>     score                                = sampleSize / fromIntegral pz.pzShdr.sampleRate
>
>     sampleSize         :: Double
>     sampleSize                           = fromIntegral $ xEnd - xStart
>       where
>         xStart         = addIntToWord    pz.pzShdr.start   (sumOfMaybeInts [zone.zStartOffs, zone.zStartCoarseOffs])
>         xEnd           = addIntToWord    pz.pzShdr.end     (sumOfMaybeInts [zone.zEndOffs,   zone.zEndCoarseOffs])
>
> sampleLimitsOk         :: (Word, Word) → Bool
> sampleLimitsOk (st, en)                  = st >= 0 && en - st >= fst sampleLimits && en - st < 2^22
> sampleLoopLimitsOk     :: (Word, Word) → Bool
> sampleLoopLimitsOk (st, en)              = st >= 0 && en - st >= snd sampleLimits && en - st < 2^22

Note that harsher consequences of unacceptable sample header are enforced earlier. Logically, that would be
sufficient to protect below code from bad data and document the situation. But ... mechanism such as putting
out diagnostics might cause us to execute this code first. So, being crash-free/minimal in isStereoZone et al.

> isStereoInst, is24BitInst
>                        :: [(PreZone, SFZone)] → Bool
>
> isStereoInst zs                           = isJust $ find isStereoZone (map fst zs)
>       
> isStereoZone z                            = isLeftPreZone z || isRightPreZone z
>
> -- see also zfindByBagIndex
>
> isLeftPreZone pz                         = SampleTypeLeft == toSampleType (F.sampleType pz.pzShdr)
> isRightPreZone pz                        = SampleTypeRight == toSampleType (F.sampleType pz.pzShdr)
>
> zoneConforms (pz, SFZone{zSampleMode, zInitQ, zScaleTuning, zExclusiveClass})
>                                          = not $ or unsupported
>   where
>     F.Shdr{end, start, endLoop, startLoop}
>                                          = pz.pzShdr
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
>                           → Map PerGMKey [PreZone] 
>                           → ([InstrumentName], [PercussionSound])
>                           → Map PreZoneKey PreZone
>                           → IO [(PerGMKey, InstCat)]
> categorize sffiles preInstCache inverter rost preZoneCache
>                                          = CM.foldM winnow [] (Map.keys preInstCache)
>   where
>     winnow             :: [(PerGMKey, InstCat)] → PerGMKey → IO [(PerGMKey, InstCat)]
>     winnow target pergm                  = do
>       CM.when (isJust doShow) (putStrLn $ fromMaybe [] doShow)
>       return (target ++ news)
>       where
>         (cat, doShow)                    = categorizeInst pergm
>         news                             =
>           case cat of
>             InstCatInst _                → singleton (pergm, cat)
>             InstCatPerc _                → singleton (pergm, cat)
>             InstCatDisq _                → []
>
>     categorizeInst     :: PerGMKey → (InstCat, Maybe String)
>     categorizeInst pergm
>       | traceIf trace_CI False           = undefined
>       | otherwise                        =
>       (deJust "categorizeInst icat" icat, doShow >>= (\x → Just $ disqMsg ++ x))
>       where
>         preI                             = deJust "categorizeInst PreInstrument" (Map.lookup pergm preInstCache)
>         mpzs                             = Map.lookup pergm inverter
>         pzs                              = deJust "categorizeInst inverter" mpzs
>
>         doShow                           =
>           case icat of
>             Just (InstCatDisq reason)    → renderDisqReason reason
>             _                            → Nothing
>         disqMsg                          =
>           unwords ["categorizeInst:", "skipping", show pergm.pgkwFile, show pergm.pgkwInst, show preI.iName, ":"]
>
>         trace_CI                         =
>           unwords ["categorizeInst", show pergm.pgkwFile, preI.iName, show pergm.pgkwInst]
>  
>         -- Put the Instrument, of either category, through a gauntlet of checks.
>         -- This diverges, and then we have qualInstZone and qualPercZone 
>
>         (zsStereo, zsMono)               = partition isStereoZone pzs
>         (zsOutbound, zsLocal)            = partition hasCross zsStereo
>         zsLessLocalRights                = zsMono ++ zsOutbound ++ filter isLeftPreZone zsLocal
>
>         -- Determine which category will belong to the Instrument, based on its performance for
>         -- 1. all kinds
>         -- 2. "rost" subset, could be same as 1.
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
>         checkGMRange   :: (Num a, Ord a) ⇒ Maybe (a, a) → Maybe InstCat
>         checkGMRange mrng                =
>           mrng >>= \(j, k) → if (0 <= j) && j <= k && k < fromIntegral qMidiSize128
>                                then Nothing
>                                else Just $ InstCatDisq DisqCorruptRange
>
>         hasRom pz                        = F.sampleType pz.pzShdr >= 0x8000
>                                          
>         checkLinkage   :: Maybe InstCat
>         checkLinkage                     =
>           if isSafe sList || requiredZoneLinkage < 1 then Nothing else Just $ InstCatDisq DisqLinkage
>           where
>             sList      :: [(Int, Int)]
>             sList                        = map (\z → (extractIndex z, extractLink z)) zsLocal
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
>         extractLink pz                   = fromIntegral $ F.sampleLink pz.pzShdr
>
>         rejectCrosses  :: Maybe InstCat
>         rejectCrosses                 =
>           if any hasCross pzs then Just $ InstCatDisq DisqIllegalCrossover else Nothing
>
>         hasCross       :: PreZone → Bool
>         hasCross pz                      = isStereoZone pz && notElem (extractLink pz) (map extractLink pzs)
>
>         howLaden       :: [Word] → Double
>         howLaden ws
>           | null pzs                     = 0
>           | otherwise                    = (fromIntegral . length) ws / (fromIntegral . length) pzs
>
>         maybeSettle    :: (Foldable t) ⇒ Fuzz → InstCat → t Fuzz → Maybe InstCat
>         maybeSettle thresh icat keys     = find (> thresh) keys >> Just icat
>
>         genericScore                     = evalAgainstGeneric preI.iName

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
>               [if isNothing mpzs || null (fromJust mpzs)
>                  then Just (InstCatDisq DisqNoQualifiedZones)
>                  else Nothing
>               , corrupt
>               , if any hasRom pzs then Just (InstCatDisq DisqRomBased) else Nothing
>               , if allowStereoCrossovers
>                   then Nothing
>                   else rejectCrosses
>               , checkLinkage
>               ]
>             alts2 rost                   =
>               let
>                 ffInst'                  = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) preI.iMatches.ffInst
>                 ffPerc'                  = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) preI.iMatches.ffPerc
>                 uZones :: [Word]         = case seed of
>                                              Nothing                 → wZones
>                                              Just (InstCatPerc icd)  → icd.inPercBixen
>                                              _                       → []
>                 wZones :: [Word]         = mapMaybe (qualPercZone rost) pzs
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
>
>                   , maybeNailAsPerc 0.8 
>                   , maybeSettle stands      (catPerc wZones)         ffPerc'
>                   , maybeSettle stands      (catDisq DisqNarrow)     preI.iMatches.ffInst
>
>                   , maybeNailAsPerc 0.6 
>                   , if genericScore > 0 then Just catInst            else Nothing
>                   , if genericScore < 0 then Just (catPerc wZones)   else Nothing
>                   , Just $ catDisq DisqUnrecognized
>                 ]
>
>             catInst      :: InstCat      =
>               if null pzs
>                 then InstCatDisq DisqNoQualifiedZones
>                 else (case checkSmashing smashup of
>                        Left _            → InstCatInst icd
>                        Right reason      → InstCatDisq reason)
>               where
>                 smashup                  = computeInstSmashup pzs
>                 icd                      = InstCatData pzs smashup []
>                 eith                     = checkSmashing smashup
>
>             catPerc      :: [Word] → InstCat
>             catPerc ws                   =
>               if null pzs || null ws || (null . init) ws
>                 then InstCatDisq DisqNarrow
>                 else (case eith of
>                        Left _            → InstCatPerc icd
>                        Right reason      → InstCatDisq reason)
>               where
>                 smashup                  = computeInstSmashup pzs
>                 icd                      = InstCatData pzs smashup ws
>                 eith                     = checkSmashing smashup
>
>             catDisq    :: DisqReason → InstCat
>             catDisq                      = InstCatDisq
>
>         qualPercZone   :: ([InstrumentName], [PercussionSound]) → PreZone → Maybe Word
>         qualPercZone rost' prez
>           | traceNot trace_EFP False     = undefined
>           | otherwise                    = result
>           where
>             result                       =
>               prez.pzDigest.zdKeyRange
>               >>= (\(x,y) → Just (fromIntegral x, fromIntegral y))
>               >>= pinnedKR (select rost')
>               >> Just prez.pzBix
>
>             trace_EFP                    = unwords ["qualPercZone", show prez.pzBix, show result]
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
>     computePerInst (pergm, icat)
>       | traceIf trace_CPI False          = undefined
>       | otherwise                        = PerInstrument (zip pzs oList) icd.inSmashup
>       where
>         sffile@SFFile{zFilename, zArrays}
>                                          = sffiles ! pergm.pgkwFile
>         preI                             = deJust "computePerInst PreInstrument" (Map.lookup pergm preInstCache)
>
>         icd            :: InstCatData
>         allBix         :: [Word]
>
>         (icd, allBix)                    =
>           case icat of
>             InstCatPerc x                → (x, x.inPercBixen)
>             InstCatInst x                → (x, map pzBix x.inPreZones)
>             _                            → error $ unwords ["formZoneCache", "only Inst and Perc are valid here"]
>
>         gZone                            = buildZone sffile defZone preI.iGlobalPreZoneKey.pzkwBag
>         oList                            = map (buildZone sffile gZone) allBix
>         pzs                              = mapMaybe plookup allBix
>         plookup        :: Word → Maybe PreZone
>         plookup wBix                     = Map.lookup (PreZoneKey sffile.zWordF wBix) preZoneCache
>
>         trace_CPI                        =
>           unwords ["computePerInst", show pergm.pgkwFile, preI.iName, show (length oList)]
>
> checkSmashing          :: Smashing Word → Either Bool DisqReason
> checkSmashing smashup
>   | not ok1                              = Right DisqUnderRanges
>   | not ok2                              = Right DisqOverRanges
>   | otherwise                            = Left True
>   where
>     ok1                                  = allowOutOfRange || smashup.smashStats.countNothings == 0
>     ok2                                  = allowOverlappingRanges || smashup.smashStats.countMultiples == 0
>
> computeInstSmashup     :: [PreZone] → Smashing Word
> computeInstSmashup pzs
>   | traceNot trace_CIS False             = undefined
>   | otherwise                            = smashup
>   where
>     -- create smashup consisting of 16_384 (128 x 128) Word pairs - adds up to 131_072 bytes
>     dims               :: [Word]
>     subs               :: [(Word, [Maybe (Word, Word)])]
>
>     dims                                 = [fromIntegral qMidiSize128, fromIntegral qMidiSize128]
>     subs                                 = map extractSpace pzs
>
>     smashup            :: Smashing Word
>     smashup                              = smashSubspaces (unwords["computePerInst", "smashup"]) dims subs
>
>     trace_CIS                            = unwords ["computeInstSmashup", showPreZones pzs, show subs]
>
> buildZone              :: SFFile → SFZone → Word → SFZone
> buildZone sffile fromZone bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = foldr addMod (foldl' addGen fromZone gens) mods
>   where
>     sfa                                  = sffile.zArrays
>
>     xgeni                                = F.genNdx $ sfa.ssIBags!bagIndex
>     ygeni                                = F.genNdx $ sfa.ssIBags!(bagIndex + 1)
>     xmodi                                = F.modNdx $ sfa.ssIBags!bagIndex
>     ymodi                                = F.modNdx $ sfa.ssIBags!(bagIndex + 1)
>
>     gens               :: [F.Generator]  =
>       profess
>         (xgeni <= ygeni)
>         (unwords["SoundFont file", show sffile.zWordF, sffile.zFilename, "corrupt (buildZone gens)"])
>         (map (sfa.ssIGens !) (deriveRange xgeni ygeni))
>     mods               :: [(Word, F.Mod)]
>                                          =
>       profess
>         (xmodi <= ymodi)
>         (unwords["SoundFont file", show sffile.zWordF, sffile.zFilename, "corrupt (buildZone mods)"])
>         (zip [10_000..] (map (sfa.ssIMods !) (deriveRange xmodi ymodi)))
>
>     trace_BZ                             =
>       unwords ["buildZone", show sffile.zWordF, show bagIndex, show (fromZone == defZone)]

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
> instrumentSF sfrost pergm dur pchIn volIn params
>   | traceNow trace_ISF False             = undefined
>   | otherwise                            = eutSynthesize (reconX, mreconX) reconX.rSampleRate
>                                              dur pchOut volOut params
>                                              sfa.ssData sfa.ssM24
>   where
>     noon                                 = NoteOn
>                                              (clip (0, 127) volIn)
>                                              (clip (0, 127) pchIn)
>     pchOut              :: AbsPitch      = maybe noon.noteOnKey (clip (0, 127)) reconX.rForceKey
>     volOut              :: Volume        = maybe noon.noteOnVel (clip (0, 127)) reconX.rForceVel
>
>     sffile                               = sfrost.zFiles ! pergm.pgkwFile
>     sfa                                  = sffile.zArrays
>
>     preI                                 = deJust "instrumentSF preI" (Map.lookup pergm sfrost.zPreInstCache)
>     perI                                 = deJust "instrumentSF perI" (Map.lookup pergm sfrost.zPerInstCache)
>
>     trace_ISF                            =
>       unwords ["instrumentSF", show pergm.pgkwFile, show preI.pInst, show (pchIn, volIn), show dur]
>
>     (reconX, mreconX)                    =
>       case setZone of
>         Left zplus                       → (recon zplus noon, Nothing)
>         Right zsPlus                     → reconLR zsPlus noon

zone selection for rendering ==========================================================================================

>     setZone            :: Either (SFZone, F.Shdr) ((SFZone, F.Shdr), (SFZone, F.Shdr))
>     setZone                              =
>       case selectZoneConfig (selectBestZone noon) of 
>         Left (pzL, zoneL)                → Left (zoneL, pzL.pzShdr)
>         Right ((pzL, zoneL), (pzR, zoneR))
>                                          → Right ((zoneL, pzL.pzShdr), (zoneR, pzR.pzShdr))
>
>     selectBestZone     :: NoteOn → (PreZone, SFZone)
>     selectBestZone noon                  =
>       deJust (unwords ["selectBestZone", show bagId]) (findByBagIndex' perI.pZones bagId)
>       where
>         (bagId, _)                       = lookupCellIndex (noonAsCoords noon) perI.pSmashing
>
>     selectZoneConfig     :: (PreZone, SFZone) → Either (PreZone, SFZone) ((PreZone, SFZone), (PreZone, SFZone))
>     selectZoneConfig z
>        | isNothing mpartner              = Left z -- WOX error $ unwords["selectZoneConfig", "no partner"]
>        | stype == SampleTypeLeft         = Right (z, oz)
>        | stype == SampleTypeRight        = Right (oz, z)
>        | otherwise                       = Left z
>        where
>          mpartner                        = getStereoPartner z
>          shdr                            = (pzShdr . fst) z
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
>         _                                → Nothing -- WOX error $ unwords["getStereoPartner", "attempted on non-stereo zone"]
>       where
>         trace_GSP                        = unwords ["getStereoPartner", show partnerKey, showable]
>
>         showable                         =
>           case partner of
>             Just (pz, _)                 → show pz.pzBix
>             Nothing                      → "Nothing" 
>
>         targetSampleIx                   = F.sampleLink shdr
>         partner                          = findBySampleIndex' perI.pZones (F.sampleLink shdr) `CM.mplus` Just (pz, zone)
>         shdr                             = (pzShdr . fst) z
>
>         partnerKey                       = (Just . fst) z >>= pzmkPartner
>         pz                               = deJust "getStereoPartner1" (partnerKey >>= (`Map.lookup` sfrost.zPreZoneCache))
>         zone                             = deJust "getStereoPartner2" $ 
>           Just (extractInstKey pz)
>           >>= (`Map.lookup` sfrost.zPerInstCache)
>           >>= Just . pZones
>           >>= Just . map snd
>           >>= (`findBySampleIndex` targetSampleIx)

reconcile zone and sample header ======================================================================================

> reconLR                :: ((SFZone, F.Shdr), (SFZone, F.Shdr)) → NoteOn → (Recon, Maybe Recon)
> reconLR ((zoneL, shdrL), (zoneR, shdrR)) noon
>   | traceNever trace_RLR False           = undefined
>   | otherwise                            = (recL, Just recR')
>   where
>     recL@Recon{rRootKey = rkL, rPitchCorrection = pcL}
>                                          = recon (zoneL, shdrL) noon
>     recR                                 = recon (zoneR, shdrR) noon
>     recR'                                = recR{
>                                                rRootKey                   = rkL
>                                              , rPitchCorrection           = pcL}
>
>     trace_RLR                            = unwords ["reconLR:\n", show zoneL, "\n", show shdrL]
>
> recon                  :: (SFZone, F.Shdr) → NoteOn → Recon 
> recon (zone@SFZone{ .. }, sHdr@F.Shdr{ .. }) noon@NoteOn{ .. }
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
> reconModulation SFZone{ .. } shdr noon
>   | traceIf trace_RM False               = undefined
>   | otherwise                            = resolveMods m8n zModulators defaultMods
>   where
>     trace_RM                             = unwords ["reconModulation", shdr.sampleName]
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
>   | DisqUnderRanges
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
> renderDisqReason DisqUnderRanges         = Just (unwords["incomplete coverage for zone ranges"])
> renderDisqReason DisqIllegalCrossover    = Just (unwords["illegal crossover"])
>
> data SoundFontSettings =
>   SoundFontSettings {
>     qqAllowStereoCrossovers              :: Bool
>   , qqAllowOverlappingRanges             :: Bool
>   , qqAllowOutOfRange                    :: Bool
>   , qqMultipleCompetes                   :: Bool} deriving Show
>
> allowStereoCrossovers                    = qqAllowStereoCrossovers      defF
> -- stereo pair can come from 2 different instruments
> allowOverlappingRanges                   = qqAllowOverlappingRanges     defF
> allowOutOfRange                          = qqAllowOutOfRange            defF
> -- more than one zone can reference a given range cell
> multipleCompetes                         = qqMultipleCompetes           defF
>
> defF                   :: SoundFontSettings
> defF =
>   SoundFontSettings {
>     qqAllowStereoCrossovers              = False
>   , qqAllowOverlappingRanges             = True
>   , qqAllowOutOfRange                    = True
>   , qqMultipleCompetes                   = True}

The End
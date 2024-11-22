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
>   , sMatches           :: FFMatches} deriving Show
>
> data PreZoneKey =
>   PreZoneKey {
>     pzkwFile           :: Word
>   , pzkwBag            :: Word} deriving (Eq, Ord, Show)
> defPreZoneKey                            = PreZoneKey 0 0
>
> data PreZone =
>   PreZone {
>     pzWordF            :: Word
>   , pzWordS            :: Word
>   , pzWordI            :: Word
>   , pzWordB            :: Word
>   , pzDigest           :: ZoneDigest
>   , pzShdr             :: F.Shdr
>   , pzmkPartners       :: [PreZoneKey]} deriving (Eq, Show)
> extractSampleKey       :: PreZone → PreSampleKey
> extractSampleKey pz                     = PreSampleKey pz.pzWordF pz.pzWordS
> extractZoneKey         :: PreZone → PreZoneKey
> extractZoneKey pz                       = PreZoneKey pz.pzWordF pz.pzWordB
> reformPreZoneCache     :: [PreZone] → Map PreZoneKey PreZone
> reformPreZoneCache                      = foldl' (\xs y → Map.insert (extractZoneKey y) y xs) Map.empty
> extractInstKey         :: PreZone → PerGMKey
> extractInstKey pz                        = PerGMKey pz.pzWordF pz.pzWordI Nothing
> extractSpace           :: PreZone → (Word, [Maybe(Word, Word)])
> extractSpace pz                          = (pz.pzWordB, [pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange])
> showPreZones           :: [PreZone] → String
> showPreZones pzs                         = show $ map pzWordB pzs
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
>   , pKind              :: Kind
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
>   , zPartnerCache      :: Map PreZoneKey SFZone
>   , zPreInstCache      :: Map PerGMKey PreInstrument
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
>   , zdStart            :: Word
>   , zdEnd              :: Word
>   , zdStartLoop        :: Word
>   , zdEndLoop          :: Word} deriving (Eq, Show)
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
>                                          = zd {zdStart = zd.zdStart + 32_768 * fromIntegral i}
>     inspectGen (F.StartAddressOffset i)                  zd
>                                          = zd {zdStart = zd.zdStart + fromIntegral i}
>     inspectGen (F.EndAddressCoarseOffset i)              zd
>                                          = zd {zdEnd = zd.zdEnd + 32_768 * fromIntegral i}
>     inspectGen (F.EndAddressOffset i)                    zd
>                                          = zd {zdEnd = zd.zdEnd + fromIntegral i}
>
>     inspectGen (F.LoopStartAddressCoarseOffset i)        zd
>                                          = zd {zdStartLoop = zd.zdStartLoop + 32_768 * fromIntegral i}
>     inspectGen (F.LoopStartAddressOffset i)              zd
>                                          = zd {zdStartLoop = zd.zdStartLoop + fromIntegral i}
>     inspectGen (F.LoopEndAddressCoarseOffset i)          zd
>                                          = zd {zdEndLoop = zd.zdEndLoop + 32_768 * fromIntegral i}
>     inspectGen (F.LoopEndAddressOffset i)                zd
>                                          = zd {zdEndLoop = zd.zdEndLoop + fromIntegral i}
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
> theGrader              :: Grader         = grader ssWeights 500

profiler ==============================================================================================================

> -- (removed...revisit when things settle down)
> profileSF2s            :: IO ()
> profileSF2s                              = print "removed"

executive =============================================================================================================

> doEverything           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> doEverything songs     = do
>
>   putStrLn "everything..."
>   putStrLn $ reapEmissions emitSettingses
>
>   tsStarted            ← getCurrentTime
>
>   rost                 ← qualifyKinds songs
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
>       (preSampleCache, sPartnerMap)
>                        ← initSamples preR.zFiles
>       putStrLn "initInsts"
>       preInstCache_    ← initInsts preR.zFiles
>       putStrLn "initZones"
>       (preZoneCache, preInstCache)
>                        ← initZones preR.zFiles preSampleCache sPartnerMap preInstCache_
>       inverter         ← associateZones preZoneCache
>
>       jobs             ← categorize preR.zFiles preInstCache inverter preR.zRost
>       tsCatted         ← getCurrentTime
>       putStrLn ("___categorize: " ++ show (diffUTCTime tsCatted tsStarted))
>
>       zc               ← formZoneCache preR.zFiles preInstCache preR.zRost jobs
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
>                        ← decideWinners preR.zFiles preSampleCache preInstCache reinverter
>                                        zc preR.zRost pergmsI pergmsP
>       tsDecided        ← getCurrentTime
>       putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsZoned))
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
>                              , zPreZoneCache     = preZoneCache  -- WOX not up to date
>                              , zPartnerCache     = formZPartnerCache zc preZoneCache
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

>     initSamples        :: Array Word SFFile → IO (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey)
>     initSamples sffiles
>                        = do
>       print "initSamples"
>       presks           ← formMasterSampleList sffiles
>       (preSampleCache_, errs)
>                        ← formPreSampleCache sffiles presks
>       (preSampleCache, sPartnerMap, errs')
>                        ← finishPreSampleCache sffiles preSampleCache_
>
>       mapM_ putStrLn (reverse errs ++ reverse errs')
>       return (preSampleCache, sPartnerMap)
>         
>     initInsts          :: Array Word SFFile → IO (Map PerGMKey PreInstrument)
>     initInsts sffiles
>                        = do
>       print "initInsts"
>       pergmsI_         ← formMasterInstList sffiles
>       (preInstCache, errs)
>                        ← formPreInstCache sffiles pergmsI_
>       mapM_ putStrLn (reverse errs)
>
>       return preInstCache
>
>     initZones          :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PreSampleKey PreSampleKey → Map PerGMKey PreInstrument
>                           → IO (Map PreZoneKey PreZone, Map PerGMKey PreInstrument)
>     initZones sffiles preSampleCache sPartnerMap preInstCache_
>                        = do
>       print "initZones"
>       (preZoneCache, preInstCache, errs)
>                        ← formPreZoneCache sffiles preSampleCache sPartnerMap preInstCache_
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
>   tsFinished           ← getCurrentTime
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
>                           → Map PerGMKey PerInstrument
>                           → ([InstrumentName], [PercussionSound]) 
>                           → [PerGMKey]
>                           → [PerGMKey]
>                           → IO ((Map InstrumentName [PerGMScored], [String])
>                               , (Map PercussionSound [PerGMScored], [String]))
> decideWinners sffiles preSampleCache preInstCache inverter zc rost pergmsI pergmsP
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
>                                              , computeResolution kind rost preI zs
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

bootstrapping methods =================================================================================================

> formMasterSampleList   :: Array Word SFFile → IO [PreSampleKey]
> formMasterSampleList sffiles             = return $ concatMap formFS sffiles
>   where
>     formFS             :: SFFile → [PreSampleKey]
>     formFS sffile                        =
>       let
>         (st, en)       :: (Word, Word)   = bounds sffile.zBoot.ssShdrs
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
>                                          = 
>       if qualifySampleHeader
>         then (Just $ PreSample sampleName (computeFFMatches sampleName), [])
>         else (Nothing, diagnose wF shdr)
>       where
>         qualifySampleHeader              =
>             goodName sampleName
>          && sampleRate >= 64
>          && sampleRate < 2^20
>          && isJust (toMaybeSampleType sampleType)
>          && sampleLimitsOk (start, end)
>          && sampleLoopLimitsOk (startLoop, endLoop)
>
>     diagnose           :: Word → F.Shdr → String
>     diagnose wF shdr                     =
>       unwords ["formPreSampleCache", "problem", show wF, show (toMaybeSampleType shdr.sampleType), show shdr]
>
>     loadShdr presk                       = sffile.zBoot.ssShdrs ! presk.pskwSampleIndex
>       where
>         sffile                           = sffiles ! presk.pskwFile
>
>     preSFolder :: (Map PreSampleKey PreSample, [String]) → PreSampleKey → (Map PreSampleKey PreSample, [String])
>     preSFolder (target, errs) presk@PreSampleKey{pskwFile, pskwSampleIndex}
>                                          =
>       let
>         (mpres, err)                     = (computePreSample pskwFile . loadShdr) presk
>       in
>         case mpres of
>           Just pres                      → (Map.insert presk pres target, errs)
>           Nothing                        → (target, err : errs)
>
> finishPreSampleCache   :: Array Word SFFile
>                           → Map PreSampleKey PreSample
>                           → IO (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, [String])
> finishPreSampleCache sffiles preSampleCache
>                                          =
>   return $ foldl' psFolder (Map.empty, Map.empty, []) (Map.mapWithKey qualifyPartnering preSampleCache)
>   where
>     fName                                = "finishPreSampleCache"
>     psFolder           :: (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, [String])
>                           → Either (PreSampleKey, PreSample, Maybe PreSampleKey) String
>                           → (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, [String])
>     psFolder (target, sPartnerMap, errs) eith
>                                          = (target', sPartnerMap', errs')
>       where
>         (target', sPartnerMap', errs')    =
>           case eith of
>             Left (presk, val, mpreskPartner)
>                                          → consumeLeft presk val mpreskPartner
>             Right err                    → (target, sPartnerMap, err : errs)
>
>         consumeLeft presk val mpreskPartner
>                                          =
>           (Map.insert presk val target
>           , case mpreskPartner of
>               Nothing                    → sPartnerMap
>               Just preskPartner          → Map.insert presk preskPartner sPartnerMap
>           , errs)
>
>     qualifyPartnering  :: PreSampleKey → PreSample → Either (PreSampleKey, PreSample, Maybe PreSampleKey) String
>     qualifyPartnering key val
>       | not stereo                       = Left (key, val, Nothing)
>       | isNothing other                  = Right problemMissing
>       | isNothing backLink               = Right problemDisqual
>       | otherwise                        = Left (key, val, Just otherKey)
>       where
>         problemMissing                   =
>           unwords [fName, "problem", "missing stereo partner", show key, show shdr.sampleLink]
>         problemDisqual                   =
>           unwords [fName, "problem", "stereo partner disqual", show key, show otherKey]
>         otherKey                         = PreSampleKey key.pskwFile shdr.sampleLink
>         other                            = Map.lookup otherKey preSampleCache
>         oBackLink                        = if F.sampleLink oshdr == key.pskwSampleIndex
>                                              then Just otherKey
>                                              else Nothing
>         backLink                         = other >> oBackLink
>
>         sffile                           = sffiles ! key.pskwFile
>         shdr                             = sffile.zBoot.ssShdrs ! key.pskwSampleIndex
>         oshdr                            = sffile.zBoot.ssShdrs ! otherKey.pskwSampleIndex
>         stype                            = toSampleType shdr.sampleType
>         stereo                           = SampleTypeLeft == stype || SampleTypeRight == stype
>
> data FileInstScan                        =
>   FileInstScan {
>     isResults          :: [Either InstZoneScan (String, InstZoneScan)]
>   , isTasks            :: [[Either InstZoneScan (String, InstZoneScan)]
>                            → [Either InstZoneScan (String, InstZoneScan)]]}
> unfinished             :: FileInstScan → Bool
> unfinished iscan                         = not (null iscan.isTasks)
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
>
> instKey zscan                            = PerGMKey zscan.zswFile zscan.zswInst Nothing
>         
> formPreZoneCache       :: Array Word SFFile
>                           → Map PreSampleKey PreSample → Map PreSampleKey PreSampleKey → Map PerGMKey PreInstrument
>                           → IO (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
> formPreZoneCache sffiles preSampleCache sPartnerMap preInstCache
>                                          = return $ foldl' formFZ (Map.empty, preInstCache, []) sffiles
>   where
>     fName                                = "formPreZoneCache"
>     -- FZ = file to zone
>     formFZ             :: (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
>                           → SFFile
>                           → (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
>     formFZ (pzcache_, picache_, errs_) sffile
>                                          =
>       let
>         (pzcache, picache, errs)         = captureFile sffile
>       in
>         (Map.union pzcache pzcache_, picache, errs_ ++ errs)
>          
>     makePreZone wF wS wI wB zd shdr      = PreZone wF wS wI wB zd shdr []
>
>     captureFile        :: SFFile → (Map PreZoneKey PreZone, Map PerGMKey PreInstrument, [String])
>     captureFile sffile                   =
>       (reformPreZoneCache (foldl' (\x y → x ++ y.zsPreZones ) [] (lefts isFinal.isResults))
>                          , foldl' markGlobalZone newPreInstCache (lefts isFinal.isResults), errs)
>       where
>         wF                               = sffile.zWordF
>         boota                            = sffile.zBoot
>
>         (stI, enI)     :: (Word, Word)   = bounds boota.ssInsts
>
>         results                          = map capture (deriveRange stI enI)
>         tasks                            = [groomTask, map (forward vetSuccess), reorgTask]
>         isInitial                        = FileInstScan results tasks
>         isSeries                         = iterate' advance isInitial
>         isFinal                          = head $ dropWhile unfinished isSeries
>
>         groomTask resultsIn              = groom (makeBack (lefts resultsIn)) resultsIn
>         reorgTask                        = map reorg
>
>         allPzs                           = foldl' (\x y → x ++ y.zsPreZones ) [] (lefts isFinal.isResults)
>         mapAllPzs                        = reformPreZoneCache allPzs
>         mapStereo                        = Map.filter isStereoZone mapAllPzs
>
>         (errs, badzscans)                = unzip (rights isFinal.isResults)
>         newPreInstCache                  = foldl' (\x y → Map.delete (instKey y) x) preInstCache badzscans
>
>         advance        :: FileInstScan → FileInstScan
>         advance iscan_@FileInstScan{ .. }
>                                          =
>           let
>             fun                          = head iscan_.isTasks
>             res                          = fun iscan_.isResults
>           in
>             iscan_{isResults = res, isTasks = tail isTasks}
>         
>         forward ffun res                 =
>           case res of
>             Left zscan                   → ffun zscan
>             Right x                      → Right x
>
>         capture        :: Word → Either InstZoneScan (String, InstZoneScan)
>         capture wI                       = cTry
>           where
>             cTry
>               | isNothing target         = Right (unwords [fName, "orphaned by instrument", show wI], zscan)
>               | null pzsRemaining        = Right (unwords [fName, "problem", "no qualified zones", show wI], zscan)
>               | otherwise                = Left zscan
>
>             target                       = Map.lookup (PerGMKey sffile.zWordF wI Nothing) preInstCache
>
>             iinst                        = boota.ssInsts ! wI
>             jinst                        = boota.ssInsts ! (wI+1)
>
>             ibagi                        = F.instBagNdx iinst
>             jbagi                        = F.instBagNdx jinst
>             results                      = map produce (deriveRange ibagi jbagi)
>
>             mGBKey                       = if head results == Right "global zone"
>                                              then Just ibagi
>                                              else Nothing
>             zscan                        = makeZScan sffile.zWordF wI pzsRemaining
>
>             bads                         = rights results
>             pzsRemaining                 = lefts results
>             
>             produce    :: Word → Either PreZone String
>             produce bix                  = pTry
>               where
>                 pTry
>                   | isNothing zd.zdSampleIndex
>                                          = Right "global zone"
>                   | not limitsCheckedOk  = Right (unwords [fName, "problem", "corrupt adjusted limits"]) 
>                   | isNothing presTarget = Right (unwords [fName, "orphaned by sample"])
>                   | otherwise            = Left (makePreZone wF si wI bix zd shdr)
>
>                 limitsCheckedOk          = adjustedSampleLimitsOk zd shdr
>                 presTarget               = Map.lookup (PreSampleKey sffile.zWordF si) preSampleCache
>
>                 xgeni                    = F.genNdx $ boota.ssIBags ! bix
>                 ygeni                    = F.genNdx $ boota.ssIBags ! (bix + 1)
>
>                 gens   :: [F.Generator]  = profess
>                                              (xgeni <= ygeni)
>                                              "SoundFont file corrupt (computePreZone gens)"
>                                              (map (boota.ssIGens !) (deriveRange xgeni ygeni))
>                 zd                       = formDigest gens
>                 si                       = deJust "produce si" zd.zdSampleIndex
>                 shdr                     = boota.ssShdrs ! si
>
>         groom          :: Map PreSampleKey [PreZoneKey]
>                           → [Either InstZoneScan (String, InstZoneScan)]
>                           → [Either InstZoneScan (String, InstZoneScan)]
>         groom back                       = map (forward groomScan)
>           where
>             groomScan  :: InstZoneScan → Either InstZoneScan (String, InstZoneScan)
>             groomScan zscan
>               | traceNot trace_GS False  = undefined
>               | null newPzs              = Right (unwords [fName, "problem"
>                                                          , show (PerGMKey sffile.zWordF zscan.zswInst Nothing)
>                                                          , "no flipped zones"], zscan)
>               | otherwise                = Left zscan{zsPreZones = newPzs}
>               where
>                 newPzs                   = groomPreZones back zscan.zsPreZones
>                 trace_GS                 = unwords ["groomScan", show (length zscan.zsPreZones), show (length newPzs)]
>
>         vetSuccess     :: InstZoneScan → Either InstZoneScan (String, InstZoneScan)
>         vetSuccess zscan                 =
>           if null newPzs
>             then Right (unwords["vet diminished to zero", show zscan.zswInst], zscan)
>             else Left zscan{zsPreZones = newPzs}
>           where
>             newPzs                       =
>               let
>                 (pzsStereo, pzsMono)     = partition isStereoZone zscan.zsPreZones
>                 vetPreZone pz            =
>                   if null newPartners
>                     then Nothing
>                     else Just pz{pzmkPartners = newPartners}
>                   where
>                     newPartners          = filter (okPartner mapStereo pz) pz.pzmkPartners
>               in
>                 mapMaybe vetPreZone pzsStereo ++ pzsMono
>
>         reorg          :: Either InstZoneScan (String, InstZoneScan)
>                           → Either InstZoneScan (String, InstZoneScan)
>         reorg                            = id
> {-
>         reorg res                        =
>           case res of
>             Left zscan                   → reorgSuccess zscan
>             Right x                      → Right x
> -}
>
>     okPartner      :: Map PreZoneKey PreZone → PreZone → PreZoneKey → Bool
>     okPartner pzCache pz pzk
>       | traceNot trace_OKP False         = undefined
>       | otherwise                        =
>       case mpzPartner of
>         Nothing                          → False
>         Just pzPartner                   → goodPartners pz pzPartner
>       where
>         mpzPartner                       = Map.lookup pzk pzCache
>         trace_OKP                        =
>           unwords ["okPartner", show (extractSampleKey (deJust "not in partner cache" mpzPartner))]
>
>     goodPartners   :: PreZone → PreZone → Bool
>     goodPartners pzMe pzYou              =
>       let
>         mySPartner                       = PreSampleKey pzMe.pzWordF   (F.sampleLink pzMe.pzShdr)
>         yrSPartner                       = PreSampleKey pzYou.pzWordF  (F.sampleLink pzYou.pzShdr)
>       in
>         (Just yrSPartner == Map.lookup mySPartner sPartnerMap)
>         && (Just mySPartner == Map.lookup yrSPartner sPartnerMap)
>
> makeBack               :: [InstZoneScan] → Map PreSampleKey [PreZoneKey]
> makeBack zscans                          = foldl' Map.union Map.empty (map zscan2back zscans)
>   where
>     zscan2back zscan                     = foldl' backFolder Map.empty (filter isStereoZone zscan.zsPreZones)
>     backFolder target pz                 =
>       let
>         presk                            = PreSampleKey pz.pzWordF pz.pzWordS
>         prezk                            = extractZoneKey pz
>         sofar                            = fromMaybe [] (Map.lookup presk target)
>       in
>         Map.insert presk (prezk : sofar) target
>
> groomPreZones          :: Map PreSampleKey [PreZoneKey] → [PreZone] → [PreZone]
> groomPreZones back preZones
>   | traceNot trace_GPZ False             = undefined
>   | otherwise                            = pzsStereo ++ pzsMono
>   where
>     trace_GPZ                            =
>       unwords ["groomPreZones", show (length pzsMono), show (length pzsStereo_)]
>     (pzsStereo_, pzsMono)                = partition isStereoZone preZones
>     pzsStereo                            = map partnerUp pzsStereo_
>
>     partnerUp          :: PreZone → PreZone
>     partnerUp pz                         = pz{pzmkPartners = fromMaybe [] mpartners}
>       where
>         mpartners                        = Map.lookup (PreSampleKey pz.pzWordF (F.sampleLink pz.pzShdr)) back
>     
> markGlobalZone         :: Map PerGMKey PreInstrument → InstZoneScan → Map PerGMKey PreInstrument
> markGlobalZone preic zscan
>   | traceNot trace_MGZ False             = undefined
>   | otherwise                            =
>   if isNothing zscan.zswGBix || isNothing moldpreI
>     then preic
>     else Map.insert pergm oldpreI{iGlobalKey = Just $ PreZoneKey zscan.zswFile (fromJust zscan.zswGBix)} preic
>   where
>     pergm                                = PerGMKey zscan.zswFile zscan.zswInst Nothing
>     moldpreI                             = Map.lookup pergm preic
>     oldpreI                              = deJust (unwords["mold", show pergm]) moldpreI
>     trace_MGZ                            = unwords ["markGlobalZone", show zscan.zswGBix, show pergm]
>
> formZPartnerCache      :: Map PerGMKey PerInstrument → Map PreZoneKey PreZone → Map PreZoneKey SFZone
> formZPartnerCache perIs preZoneCache_   = Map.mapMaybe chaseIt preZoneCache
>   where
>     preZoneCache                         = Map.filter isStereoZone preZoneCache_
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
> formPreInstCache       :: Array Word SFFile → [PerGMKey] → IO (Map PerGMKey PreInstrument, [String])
> formPreInstCache sffiles pergms          = return $ foldl' preIFolder (Map.empty, []) pergms
>   where
>     computePreInst     :: F.Inst → Either PreInstrument String
>     computePreInst iinst                 =
>       if goodName iinst.instName
>         then Left $ PreInstrument iinst iinst.instName (computeFFMatches iinst.instName) Nothing
>         else Right $ unwords ["formPreInstCache", "illegal name", show iinst.instName]
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm
>                                          = boota.ssInsts ! pergm.pgkwInst
>       where
>         boota                            = (sffiles ! pergm.pgkwFile).zBoot
>
>     preIFolder         :: (Map PerGMKey PreInstrument, [String]) → PerGMKey → (Map PerGMKey PreInstrument, [String])
>     preIFolder (target, errs) pergm      =
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
>         boota                            = sffile.zBoot
>         boundsI                          = bounds boota.ssInsts
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
>             iinst                        = boota.ssInsts ! wordI
>             jinst                        = boota.ssInsts ! (wordI+1)
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
>                           → PreInstrument
>                           → [(PreZone, SFZone)]
>                           → Double
> computeResolution kind rost preI zs
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
>       unwords ["computeResolution", preI.iName, show kind, show theSplit
>              , show (map sampleDurationScoring zs), ">", show (measureSplits kind, measureSampleSize)]
>
> sampleDurationScoring  :: (PreZone, SFZone) → Double
> sampleDurationScoring (pz, zone)
>   | traceNot trace_SDS False             = undefined
>   | otherwise                            = if score < 0.01 then -10 else score
>   where
>     trace_SDS                            = unwords ["sampleDurationScoring", show sampleSize, show score, show zone, show pz.pzShdr]
>     score                                = sampleSize / fromIntegral pz.pzShdr.sampleRate
>
>     sampleSize         :: Double
>     sampleSize                           = fromIntegral $ xEnd - xStart
>       where
>         xStart         = addIntToWord    pz.pzShdr.start   (sumOfWeightedInts [zone.zStartOffs, zone.zStartCoarseOffs] qOffsetWeights)
>         xEnd           = addIntToWord    pz.pzShdr.end     (sumOfWeightedInts [zone.zEndOffs,   zone.zEndCoarseOffs]   qOffsetWeights)
>
> sampleLimitsOk         :: (Word, Word) → Bool
> sampleLimitsOk (st, en)                  = st >= 0 && en - st >= fst sampleLimits && en - st < 2^22
> sampleLoopLimitsOk     :: (Word, Word) → Bool
> sampleLoopLimitsOk (st, en)              = st >= 0 && en - st >= snd sampleLimits && en - st < 2^22
> adjustedSampleLimitsOk :: ZoneDigest → F.Shdr → Bool
> adjustedSampleLimitsOk zd shdr           = 0 <= st && st <= en && 0 <= stl && stl <= enl
>   where
>     st                                   = shdr.start     + zd.zdStart
>     en                                   = shdr.end       + zd.zdEnd
>     stl                                  = shdr.startLoop + zd.zdStartLoop
>     enl                                  = shdr.endLoop   + zd.zdEndLoop

Note that harsher consequences of unacceptable sample header are enforced earlier. Logically, that would be
sufficient to protect below code from bad data and document the situation. But ... mechanism such as putting
out diagnostics might cause us to execute this code first. So, being crash-free/minimal in isStereoZone et al.

> isStereoInst, is24BitInst
>                        :: [(PreZone, SFZone)] → Bool
>
> isStereoInst zs                           = isJust $ find isStereoZone (map fst zs)
>       
> isStereoZone pz                           = isLeftPreZone pz || isRightPreZone pz
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
>                           → IO [(PerGMKey, InstCat)]
> categorize sffiles preInstCache inverter rost
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
>         icatAllKinds                     = foldl' CM.mplus Nothing (provideAlts Nothing allKinds)
>         icatRost                         = foldl' CM.mplus Nothing (provideAlts icatAllKinds rost)
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

>         provideAlts    :: Maybe InstCat → ([InstrumentName], [PercussionSound]) → [Maybe InstCat]
>         provideAlts seed rost
>           | traceNot trace_PA False      = undefined
>           | otherwise                    =
>           if isNothing seed
>             then structuralAlts ++ alts2 allKinds
>             else alts2 rost
>
>           where
>             trace_PA                     = unwords ["provideAlts", show (alts2 rost)]
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
>               >> Just prez.pzWordB
>
>             trace_EFP                    = unwords ["qualPercZone", show prez.pzWordB, show result]
>
> formZoneCache          :: Array Word SFFile
>                           → Map PerGMKey PreInstrument
>                           → ([InstrumentName], [PercussionSound])
>                           → [(PerGMKey, InstCat)]
>                           → IO (Map PerGMKey PerInstrument)
> formZoneCache sffiles preInstCache rost jobs
>                                          =
>   return $ foldr (\(p, i) → Map.insert p (computePerInst (p, i))) Map.empty jobs
>
>   where
>     computePerInst     :: (PerGMKey, InstCat) → PerInstrument
>     computePerInst (pergm, icat)
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
>     name               :: Maybe String   = zone.zSampleIndex >>= \x → Just (boota.ssShdrs ! x) >>= Just . F.sampleName

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
>   | traceAlways trace_ISF False          = undefined
>   | otherwise                            = eutSynthesize (reconX, mreconX) reconX.rSampleRate
>                                              dur pchOut volOut params
>                                              samplea.ssData samplea.ssM24
>   where
>     noon                                 = NoteOn
>                                              (clip (0, 127) volIn)
>                                              (clip (0, 127) pchIn)
>     pchOut              :: AbsPitch      = maybe noon.noteOnKey (clip (0, 127)) reconX.rForceKey
>     volOut              :: Volume        = maybe noon.noteOnVel (clip (0, 127)) reconX.rForceVel
>
>     sffile                               = sfrost.zFiles ! pergm.pgkwFile
>     samplea                              = sffile.zSample
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
>     selectBestZone noon
>       | traceNot trace_SBZ False         = undefined
>       | otherwise                        =
>       if count == 0
>         then error "out of range"
>         else if isNothing foundInInst
>                then error "not found in inst"
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
>         _                                → error $ unwords [fName, "attempted on non-stereo zone"]
>       where
>         fName                            = "getStereoPartner"
>
>         shdr                             = (pzShdr . fst) z
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
>             evalCand pzk                 =
>               let
>                 pz                       = pzk `Map.lookup` sfrost.zPreZoneCache
>                 zone                     = pzk `Map.lookup` sfrost.zPartnerCache
>               in
>                 if isJust pz && isJust zone
>                   then Just (fromJust pz, fromJust zone)
>                   else Nothing

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
>   , rStart           = addIntToWord          start                  (sumOfWeightedInts
>                                                                        [zStartOffs,     zStartCoarseOffs]     qOffsetWeights)
>   , rEnd             = addIntToWord          end                    (sumOfWeightedInts
>                                                                        [zEndOffs,       zEndCoarseOffs]       qOffsetWeights)
>   , rLoopStart       = addIntToWord          startLoop              (sumOfWeightedInts
>                                                                        [zLoopStartOffs, zLoopStartCoarseOffs] qOffsetWeights)
>   , rLoopEnd         = addIntToWord          endLoop                (sumOfWeightedInts
>                                                                        [zLoopEndOffs,   zLoopEndCoarseOffs]   qOffsetWeights)
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
> -- stereo pair can come from 2 different instruments in the same file
> allowOverlappingRanges                   = qqAllowOverlappingRanges     defF
> allowOutOfRange                          = qqAllowOutOfRange            defF
> -- more than one zone can reference a given range cell
> multipleCompetes                         = qqMultipleCompetes           defF
>
> defF                   :: SoundFontSettings
> defF =
>   SoundFontSettings {
>     qqAllowStereoCrossovers              = True
>   , qqAllowOverlappingRanges             = True
>   , qqAllowOutOfRange                    = True
>   , qqMultipleCompetes                   = True}

The End
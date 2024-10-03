> {-# LANGUAGE InstanceSigs #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
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
> import Data.Char
> import Data.Either
> import Data.Foldable ( toList, for_ )
> import Data.Int ( Int8, Int16 )
> import Data.IntSet (IntSet)
> import qualified Data.IntSet             as IntSet
> import Data.List ( find, foldl', foldr, last, minimumBy, partition, singleton, sortOn )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( fromJust, fromMaybe, isJust, isNothing, mapMaybe )
> import Data.MemoTrie
> import Data.Ord ( Down(Down), comparing )
> import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
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
>
> data PreSample =
>   PreSample {
>     sName              :: String
>   , sMatches           :: FFMatches} deriving Show
>
> data PreInstrument =
>   PreInstrument {
>     iName              :: String
>   , iMatches           :: FFMatches} deriving Show
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
>
> zfindByBagIndex        :: [(ZoneHeader, a)] → Word → Maybe (ZoneHeader, a)
> zfindByBagIndex zs w                     =
>   find (\(ZoneHeader{zhwBag}, zd) → w == zhwBag) zs
>
> data PerInstrument =
>   PerInstrument {
>     pInst              :: F.Inst
>   , pZonePairs         :: [(ZoneHeader, SFZone)]}

Instrument categories: instrument, percussion, disqualified

> data InstCat =
>        InstCatInst
>      | InstCatPerc [Word]
>      | InstCatDisq DisqReason deriving Show
>     
> class GMPlayable a ⇒ SFScorable a where
>   splitScore           :: a → [(ZoneHeader, SFZone)] → Double
>   fuzzFactor           :: a → Double
>
> instance SFScorable InstrumentName where
>   splitScore     kind zs                 = fromIntegral (length zs)
>   fuzzFactor :: InstrumentName → Double 
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
>   , zPreInstCache      :: Map PerGMKey PreInstrument
>   , zRost              :: ([InstrumentName], [PercussionSound])
>   , zZoneCache         :: Map PerGMKey PerInstrument
>   , zSample2Insts      :: Map PreSampleKey [PerGMKey]
>   , zWinningRecord     :: WinningRecord}
>
> seedRoster :: Array Word SFFile → ([InstrumentName], [PercussionSound]) → SFRoster
> seedRoster vFile rost                    =
>   SFRoster vFile Map.empty Map.empty rost Map.empty Map.empty seedWinningRecord
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
>   , zModulators        :: [Modulator]} deriving Show
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
>   sffile@SFFile{ .. }                    ← openSoundFontFile 0 fp
>   let SoundFontArrays{ .. }              = zArrays
>   let lInst                              = length ssInsts
>   putStrLn ""
>   mapM_ (profileInst sffile) (deriveRange 0 (lInst-1))
>   where
>     profileInst            :: SFFile → Int → IO ()
>     profileInst sffile@SFFile{ .. } ii = do
>       CM.when diagnosticsEnabled (print ii)
>       mapM_ doZone oList
>       where
>         SoundFontArrays{ssInsts}         = zArrays
>         iinst                            = ssInsts ! fromIntegral ii
>         jinst                            = ssInsts ! fromIntegral (ii+1)
>
>         sInst                            = F.instName iinst
>
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>         (gIx, oIx)                       = profess
>                                              (ibagi <= jbagi)
>                                              (unwords["SoundFont file corrupt (profileInst)"])
>                                              (singleton ibagi, deriveRange (ibagi+1) (jbagi-1))
>         gList                            = map (buildZone sffile defZone)        gIx
>         gZone                            = (snd . head)                          gList
>         oList                            = map (buildZone sffile gZone)          oIx
>
>         doZone         :: (ZoneHeader, SFZone) → IO ()
>         doZone zone                      = do
>           let mout                       = profileZone zone
>           CM.forM_ mout putStrLn
> 
>         profileZone    :: (ZoneHeader, SFZone) → Maybe String
>         profileZone (ZoneHeader{ .. }, SFZone{ .. })
>                                          =
>           let
>             sSample                      = F.sampleName zhShdr
>             sout                         =
>               unwords [show sInst, ","
>                      , show sSample, "======="
>                      , show zInitFc
>                      , show zInitQ, "=======", show zKeyRange]
>           in
>             (zInitQ >>= (\x -> if x == 0 then Nothing else Just sSample))
>             >> (if goodName sInst && goodName sSample then Just sout else Nothing)

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
>   sffilesp             ← CM.zipWithM openSoundFontFile [0..] fps
>
>   let boundsF::(Word, Word)
>                        = (0, fromIntegral (length sffilesp - 1))
>   let preRoster        = seedRoster (listArray boundsF sffilesp) rost
>
>   tsLoaded             ← getCurrentTime
>   putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>   -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>   sfrost               ← finishRoster preRoster
>
>   CM.when doRender (doRendering sfrost)
>
>   tsRendered           ← getCurrentTime
>   putStrLn ("___overall: " ++ show (diffUTCTime tsRendered tsStarted))
>
>   where
>     -- track the complete populations of: samples, instruments, percussion
>     finishRoster       :: SFRoster → IO SFRoster
>     finishRoster preRoster@SFRoster{ .. }
>                                          = do
>       tsStarted        ← getCurrentTime
>
>       presks           ← formMasterSampleList zFiles
>       pergmsI_         ← formMasterInstList   zFiles
>
>       preSampleCache   ← formPreSampleCache zFiles presks
>       preInstCache     ← formPreInstCache   zFiles pergmsI_
>
>       jobs             ← categorize zFiles preInstCache zRost pergmsI_
>       tsCatted         ← getCurrentTime
>       putStrLn ("___categorize: " ++ show (diffUTCTime tsCatted tsStarted))
>
>       zc               ← formZoneCache zFiles preInstCache zRost jobs
>       (pergmsI, pergmsP)
>                        ← arrangeCategorizationResults zc jobs
>       CM.when diagnosticsEnabled
>         (do
>           print "pergmsI"
>           print pergmsI
>           print "pergmsP"
>           print pergmsP)
>
>       spc              ← formSampleParentCache zc
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
>       let wins@WinningRecord{ .. }
>                        = WinningRecord (Map.map head wI) (Map.map head wP) (sI ++ sP)
>
>       -- print song/orchestration info to user (can be captured by redirecting standard out)
>       mapM_ putStrLn pWarnings
>
>       let sfrost       = preRoster{zPreSampleCache   = preSampleCache
>                                  , zPreInstCache     = preInstCache
>                                  , zZoneCache        = zc
>                                  , zSample2Insts     = spc
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
>
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
>     wiFolder accum pergmI@PerGMKey{pgkwFile}
>                                          = foldl' (xaEnterTournament fuzzMap pergmI []) accum as'
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
>     wpFolder wIn pergmP@PerGMKey{ .. }
>       | traceNot trace_WP False          = undefined
>       | otherwise                        = xaEnterTournament fuzzMap pergmP [] wIn kind
>       where
>         mz             :: Maybe (ZoneHeader, SFZone)
>         mz                               = pgkwBag >>= zfindByBagIndex pZonePairs
>         mkind          :: Maybe PercussionSound
>         mkind                            = mz >>= getAP >>= pitchToPerc
>         kind                             = deJust (unwords["wpFolder", "mkind"]) mkind
>
>         mffm           :: Maybe FFMatches
>         mffm                             =
>           mz >>= (zSampleIndex . snd)
>              >>= Just . PreSampleKey pgkwFile
>              >>= flip Map.lookup preSampleCache
>              >>= Just . sMatches
>         fuzzMap        :: Map PercussionSound Fuzz
>         fuzzMap                          = getFuzzMap $ deJust "mffm" mffm
>
>         PerInstrument{ .. }              =
>           deJust (unwords["wpFolder", "PerInstrument"]) (Map.lookup (pergmP{pgkwBag = Nothing}) zc)
>
>         getAP          :: (ZoneHeader, SFZone) → Maybe AbsPitch
>         getAP (_, zone@SFZone{ .. })     = (Just . fst) =<< zKeyRange
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
>     xaEnterTournament fuzzMap pergm@PerGMKey{ .. } hints (wins, ss) kind
>       | traceIf trace_XAET False         = undefined
>       | otherwise                        = (Map.insert kind now wins, ss)
>       where
>         pergm_                           = pergm{pgkwBag = Nothing}
>         PreInstrument{ .. }              =
>           deJust (unwords["xaEnterTournament", "PreInstrument"]) (Map.lookup pergm_ preInstCache)
>         PerInstrument{ .. }              =
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
>         scope          :: [(ZoneHeader, SFZone)]
>         scope                            =
>           case pgkwBag of
>             Nothing                      → tail pZonePairs
>             Just bagI                    →
>                    maybe
>                      (error $ unwords [ "xaEnterTournament: zfindByBagIndex returned a Nothing for"
>                                       , show pgkwFile, iName])
>                      singleton
>                      (zfindByBagIndex pZonePairs bagI)
>
>         mnameZ         :: Maybe String   =     pgkwBag
>                                            >>= zfindByBagIndex pZonePairs
>                                            >>= \(ZoneHeader{zhShdr}, _) → Just (F.sampleName zhShdr)
>         zName                            = deJust "mnameZ" mnameZ
>
>         trace_XAET                       = unwords ["xaEnterTournament", iName, show kind, show scoredP.pArtifactGrade]
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
>     formFS SFFile{ .. }                  =
>       let
>         SoundFontArrays { .. }           = zArrays
>         (st, en)       :: (Word, Word)   = bounds ssShdrs
>       in
>         map (PreSampleKey zWordF) [st..en]
>
> formPreSampleCache     :: Array Word SFFile
>                           → [PreSampleKey]
>                           → IO (Map PreSampleKey PreSample)
> formPreSampleCache sffiles presks        = return $ foldl' preSFolder Map.empty presks
>   where
>     computePreSample   :: F.Shdr → PreSampleKey → PreSample
>     computePreSample F.Shdr{sampleName} k
>                                          = PreSample sampleName (computeFFMatches sampleName)
>
>     preSFolder accum presk@PreSampleKey{pskwFile, pskwSample}
>                                          =
>       let
>         SFFile{zArrays}                  = sffiles ! pskwFile
>         SoundFontArrays{ssShdrs}         = zArrays
>       in
>         Map.insert presk (computePreSample (ssShdrs ! pskwSample) presk) accum
>
> formPreInstCache       :: Array Word SFFile → [PerGMKey] → IO (Map PerGMKey PreInstrument)
> formPreInstCache sffiles pergms          = return $ foldl' preIFolder Map.empty pergms
>   where
>     computePreInstrument   :: SFFile → PerGMKey → PreInstrument
>     computePreInstrument SFFile{zArrays} PerGMKey{pgkwInst}
>                                          = PreInstrument instName (computeFFMatches instName)
>       where
>         SoundFontArrays{ .. }            = zArrays
>         F.Inst{ .. }                     = ssInsts ! pgkwInst
>
>     preIFolder accum pergm@PerGMKey{pgkwFile}
>                                          =
>       Map.insert pergm (computePreInstrument (sffiles ! pgkwFile) pergm) accum
>
> formMasterInstList     :: Array Word SFFile → IO [PerGMKey]
> formMasterInstList sffiles               = return $ concatMap formFI sffiles
>
> -- file to instrument
> formFI                 :: SFFile → [PerGMKey]
> formFI SFFile{zArrays, zWordF}           = mapMaybe qualifyInst rangeI
>   where
>     SoundFontArrays{ssInsts}             = zArrays
>     boundsI                              = bounds ssInsts
>     rangeI                               =
>       profess
>         (uncurry (<=) boundsI)
>         (unwords ["bad file: invalid bounds", show boundsI])
>         [fst boundsI..snd boundsI-1]
>
>     qualifyInst        :: Word → Maybe PerGMKey
>     qualifyInst wordI
>       | (jbagi - ibagi) >= 2             = Just $ PerGMKey zWordF wordI Nothing
>       | otherwise                        = Nothing
>       where
>         iinst                            = ssInsts ! wordI
>         jinst                            = ssInsts ! (wordI+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
> arrangeCategorizationResults
>                        :: Map PerGMKey PerInstrument
>                           → [(PerGMKey, InstCat)]
>                           → IO ([PerGMKey], [PerGMKey])
> arrangeCategorizationResults zc jobs     = return $ foldl' pfolder ([], []) jobs
>   where
>     pfolder            :: ([PerGMKey], [PerGMKey]) → (PerGMKey, InstCat) → ([PerGMKey], [PerGMKey])
>     pfolder (pergmsI, pergmsP) (pergmI, icat)
>                                          = (pergmsI', pergmsP'')
>       where
>         pergmI'                          = pergmI{pgkwBag = Nothing}
>         perI                             = deJust "perI" (Map.lookup pergmI' zc)
>         pergmsP'                         = instrumentPercList pergmI perI
>         (pergmsI', pergmsP'')            =
>           case icat of
>             InstCatPerc _                → (pergmsI, pergmsP ++ pergmsP')
>             InstCatInst                  → (pergmI : pergmsI, pergmsP)
>             _   → error $ unwords ["arrangeCategorizationResults", "illegal input", show icat]
>
>     instrumentPercList :: PerGMKey → PerInstrument → [PerGMKey]
>     instrumentPercList pergmI@PerGMKey{ .. } PerInstrument{ .. }
>                                          =
>        map ((\w → pergmI {pgkwBag = Just w}) . zhwBag . fst)  (tail pZonePairs)
>
> openSoundFontFile      :: Word → FilePath → IO SFFile
> openSoundFontFile wFile filename = do
>   putStr (show wFile ++ " " ++ filename)
>   ts1                                    ← getCurrentTime
>   result                                 ← F.importFile filename
>   case result of
>     Left s               → error $ "SoundFont decoding error: " ++ s ++ show filename
>     Right soundFont      → do
>       let pdata = F.pdta soundFont
>       let sdata = F.sdta soundFont
>       let arrays@SoundFontArrays{ .. }
>                 = SoundFontArrays
>                      (F.insts pdata)
>                      (F.ibags pdata)
>                      (F.igens pdata)
>                      (F.imods pdata)
>                      (F.shdrs pdata)
>                      (F.smpl sdata)
>                      (F.sm24 sdata)
>       let sffile = SFFile filename arrays wFile
>       let nBits = case ssM24 of
>             Nothing          → 16
>             Just s24data     → 24
>       ts2 ← getCurrentTime
>       CM.when diagnosticsEnabled (
>         putStrLn $ unwords [   show $ length ssInsts
>                            ,   show $ length ssIBags
>                            ,   show $ length ssIGens
>                            ,   show $ length ssIMods
>                            ,   show $ length ssShdrs  ])
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
>     ding@Shredding{ .. }                 ← shredMusic (song Map.empty)
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
>                                a
>                                → ([InstrumentName], [PercussionSound])
>                                → [(ZoneHeader, SFZone)]
>                                → Double
> computeResolution kind rost zs
>   | traceIf trace_CR False               = undefined
>   | null zs                              = error $ unwords ["null zs"]
>   | otherwise                            = m1 * measureSplits kind + m2 * measureSampleSize
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
>       unwords ["computeResolution", show kind, show (measureSplits kind, measureSampleSize)]
>
> sampleDurationScoring  :: (ZoneHeader, SFZone) → Double
> sampleDurationScoring (zh@ZoneHeader{ .. } , z)
>                                          = min 100_000 (sampleSize (zh, z)) / fromIntegral sampleRate
>   where
>     F.Shdr{ .. }                         = zhShdr  
>
> sampleSize         :: (ZoneHeader, SFZone) → Double
> sampleSize (ZoneHeader{ .. }, SFZone{ .. })
>                                          = fromIntegral $ xEnd - xStart
>   where
>     F.Shdr{ .. }     = zhShdr
>     xStart           = addIntToWord          start                  (sumOfMaybeInts
>                                                                        [zStartOffs, zStartCoarseOffs])
>     xEnd             = addIntToWord          end                    (sumOfMaybeInts
>                                                                        [zEndOffs, zEndCoarseOffs])

Note that harsher consequences of unacceptable sample header live elsewhere. Logically, that would be
sufficient protection. But diagnostic output might cause us to execute this first. So, being careful
but not punitive in isStereoZone.

> isStereoInst, is24BitInst
>                        :: [(ZoneHeader, a)] → Bool
>
> isStereoInst zs                          = isJust $ find isStereoZone zs
>       
> isStereoZone z                           = isLeftSample z || isRightSample z
>
>
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
>             Just n                       → n /= 0 -- WOX && n /= 100
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
> addMod (mId, F.Mod{ .. }) iz@SFZone{zModulators} 
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
>                           → [PerGMKey]
>                           → IO [(PerGMKey, InstCat)]
> categorize sffiles preInstCache rost     = CM.foldM winnow []
>   where
>     winnow             :: [(PerGMKey, InstCat)] → PerGMKey → IO [(PerGMKey, InstCat)]
>     winnow accum pergm@PerGMKey{ .. }    = do
>       CM.when (isJust doShow) (putStrLn $ fromMaybe [] doShow)
>       return $ accum ++ news
>       where
>         (cat, doShow)                    = categorizeInst pergm
>         news                             =
>           case cat of
>             InstCatInst                  → singleton (pergm, cat)
>             InstCatPerc _                → singleton (pergm, cat)
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
>         disqMsg                          = unwords ["disq:", show pgkwFile, show iName, ":"]
>
>         SoundFontArrays{ssInsts, ssIBags, ssIGens, ssShdrs}
>                                          = zArrays (sffiles ! pgkwFile)
>         PreInstrument{iName, iMatches}   =
>           deJust "categorizeInst PreInstrument" (Map.lookup pergm preInstCache)
>         FFMatches{ffInst, ffPerc}        = iMatches
>
>         iinst                            = ssInsts ! pgkwInst
>         jinst                            = ssInsts ! (pgkwInst+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
>         zs, zsLessCross, zsLessLocalRights
>                        :: [(ZoneHeader, ZoneDigest)]
>         zs                               = map inspectZone (tail (deriveRange ibagi jbagi))
>         zsLessCross                      = filter (not . hasCross) zs
>         zsLessLocalRights                = filter (\z → hasCross z || not (isRightSample z)) zs
>
>         icatU, icatR, icat
>                        :: Maybe InstCat
>         icatU                            = foldl' CM.mplus Nothing (alts Nothing allKinds)
>         icatR                            = foldl' CM.mplus Nothing (alts icatU rost)
>         icat                             =
>           case (icatU, icatR) of
>             (Just InstCatInst, Just InstCatInst)
>                                          → icatR
>             (Just (InstCatPerc _), Just (InstCatPerc _))
>                                          → icatR
>             (Just (InstCatDisq _), _)    → icatU
>             (_, Just (InstCatDisq _))    → icatR
>             _                            → Just (InstCatDisq DisqNarrow)
>
>         corrupt        :: Maybe InstCat
>         corrupt                          =
>           foldl' byZone Nothing zs `CM.mplus` checkName "Inst" iName
>           where
>             byZone     :: Maybe InstCat → (ZoneHeader, ZoneDigest) → Maybe InstCat
>             byZone accum (ZoneHeader{zhShdr}, ZoneDigest{zdKeyRange, zdVelRange})
>                                          =
>               let
>                 F.Shdr{sampleName}       = zhShdr
>               in
>                 foldl'
>                   CM.mplus
>                   accum
>                   [checkShdr zhShdr, checkName "Sample" sampleName, checkGMRange zdKeyRange, checkGMRange zdVelRange]
>
>         checkGMRange   :: (Num a, Ord a) ⇒ Maybe (a, a) → Maybe InstCat
>         checkGMRange mrng                =
>           mrng >>= \(j, k) → if (0 <= j) && j <= k && k < fromIntegral qMidiSize128
>                                then Nothing
>                                else Just $ InstCatDisq DisqCorruptRange
>
>         checkName      :: String → String → Maybe InstCat
>         checkName strType strName        =
>           if goodName strName
>             then Nothing
>             else Just (InstCatDisq $ DisqCorruptId strType (show strName))
>
>         checkShdr      :: F.Shdr → Maybe InstCat
>         checkShdr F.Shdr{sampleRate, sampleType}
>                                          =
>           if sampleRate > 0 && sampleRate < 2^30 && isJust (toMaybeSampleType sampleType)
>             then Nothing
>             else Just (InstCatDisq DisqCorruptHeader)
>
>         hasRom (ZoneHeader{zhShdr}, _)   = F.sampleType zhShdr >= 0x8000
>                                          
>         checkLinkage   :: Maybe InstCat
>         checkLinkage
>           | traceNot trace_CL False      = undefined
>           | otherwise                    =
>           if IntSet.isSubsetOf sOut sIn then Nothing else Just $ InstCatDisq DisqLinkage
>           where
>             sIn, sOut  :: IntSet
>
>             sIn                          = IntSet.fromList $ mapMaybe kindices zsLessCross
>             sOut                         = IntSet.fromList $ mapMaybe klinks zsLessCross
>
>             trace_CL                     = unwords ["checkLinkage", show sIn, show sOut]       
>
>         rejectCrosses  :: Maybe InstCat
>         rejectCrosses                 =
>           if any hasCross zs then Just $ InstCatDisq DisqIllegalCrossover else Nothing
>
>         hasCross       :: (ZoneHeader, ZoneDigest) → Bool
>         hasCross z                       = isStereoZone z && notElem myLink (map extractLink zs)
>           where
>             ZoneHeader{zhShdr}           = fst z
>             myLink                       = F.sampleLink zhShdr
>
>         rejectOverlaps :: Maybe InstCat
>         rejectOverlaps                   =
>           if all (<= 1) histResult then Nothing else Just $ InstCatDisq DisqOverlappingRanges
>           where
>             histSeed, histResult
>                        :: Array Int Int
>             histSeed                     =
>               listArray (0, qMidiSize128 * qMidiSize128 - 1) (repeat 0)
>             histResult                   =
>               foldl' histFolder histSeed (map (realizeRanges . snd) zsLessLocalRights)
>
>             realizeRanges
>                        :: ZoneDigest → ((Int, Int), (Int, Int))
>             realizeRanges ZoneDigest{ .. }
>                                          = (realize zdKeyRange, realize zdVelRange)
>
>             realize    :: Maybe (Word, Word) → (Int, Int)
>             realize mRange               =
>               case mRange of
>                 Nothing                  → (0, 127)
>                 Just (x, y)              → if x < 0 || x > y || y >= fromIntegral qMidiSize128
>                                              then (0, 127)
>                                              else (fromIntegral x, fromIntegral y)
>
>             histFolder :: Array Int Int → ((Int, Int), (Int, Int)) → Array Int Int
>             histFolder hist ((p1,p2),(v1,v2))
>                                          = accum (+) hist ([(i + 128 * j, 1) | i ← [p1..p2], j ← [v1..v2]])
>
>         kindices, klinks
>                        :: (ZoneHeader, ZoneDigest) → Maybe Int
>         kindices (zh, zd@ZoneDigest{ .. })
>                                          =
>           if isStereoZone (zh, zd)
>             then Just $ fromIntegral $ deJust "kindices" zdSampleIndex
>             else Nothing
>         klinks (zh@ZoneHeader{ .. }, zd)
>                                          =
>           if isStereoZone (zh, zd)
>             then Just $ fromIntegral $ F.sampleLink zhShdr
>             else Nothing
>
>         extractLink (ZoneHeader{zhShdr}, _)
>                                          = F.sampleLink zhShdr
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
>               [
>                 corrupt
>               , if any hasRom zs then Just (InstCatDisq DisqRomBased) else Nothing
>               , if allowStereoCrossovers
>                   then Nothing
>                   else rejectCrosses
>               , checkLinkage
>               , if allowOverlappingRanges
>                   then Nothing
>                   else rejectOverlaps -- disqualifying instruments with GM range redundancies
>                                       -- (issue: ranges covered twice due to stereo duplication)
>               ]
>             alts2 rost          =
>               let
>                 ffInst'                  = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) ffInst
>                 ffPerc'                  = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) ffPerc
>                 uZones :: [Word]         = case seed of
>                                              Nothing                 → wZones
>                                              Just (InstCatPerc us)   → us
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
>             catInst      :: InstCat        = InstCatInst
>             catPerc      :: [Word] → InstCat
>             catPerc ws                   = if null ws
>                                              then InstCatDisq DisqNarrow
>                                              else InstCatPerc ws
>             catDisq    :: DisqReason → InstCat
>             catDisq                      = InstCatDisq
>
>         evalForPerc    :: ([InstrumentName], [PercussionSound]) → (ZoneHeader, ZoneDigest) → Maybe Word
>         evalForPerc rost' (ZoneHeader{zhwBag}, ZoneDigest{zdKeyRange})
>                                          = pinOut =<< integralize zdKeyRange
>           where
>             pinOut range                 = if pinnedKR (select rost') range then Just zhwBag else Nothing
>
>         inspectZone    :: Word → (ZoneHeader, ZoneDigest)
>         inspectZone bagIndex             = (zh, zd)
>           where
>             xgeni                        = F.genNdx $ ssIBags!bagIndex
>             ygeni                        = F.genNdx $ ssIBags!(bagIndex + 1)
>
>             gens       :: [F.Generator]  = profess
>                                              (xgeni <= ygeni)
>                                              "SoundFont file corrupt (inspectZone gens)"
>                                              (map (ssIGens !) (deriveRange xgeni ygeni))
>
>             zd                           = foldr inspectGen defDigest gens
>             si                           = deJust "inspectZone si" (zdSampleIndex zd)
>             zh                           = ZoneHeader bagIndex (ssShdrs ! si)
>
>         inspectGen     :: F.Generator → ZoneDigest → ZoneDigest 
>         inspectGen (F.KeyRange i j) zd   = zd {zdKeyRange = Just (i, j)}
>         inspectGen (F.VelRange i j) zd   = zd {zdVelRange = Just (i, j)}
>         inspectGen (F.SampleIndex w) zd  = zd {zdSampleIndex = Just w}
>         inspectGen _ zd                  = zd
>
>         trace_CI                         = unwords ["categorizeInst", show pgkwFile, iName, show (ffInst, ffPerc)]
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
>     computePerInst (pergm@PerGMKey{ .. }, icat)
>       | traceIf trace_CPI False          = undefined
>       | otherwise                        = PerInstrument iinst (gList ++ oList) 
>       where
>         sffile@SFFile{zFilename, zArrays}
>                                          = sffiles ! pgkwFile
>         PreInstrument{iName}             = fromJust $ Map.lookup pergm preInstCache
>         SoundFontArrays{ssInsts}         = zArrays
>         iinst                            = ssInsts ! pgkwInst
>         jinst                            = ssInsts ! (pgkwInst+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
>         (gIx, oIx_)                      =
>           profess
>             (ibagi <= jbagi)
>             (unwords["SoundFont file", show pgkwFile, zFilename, "corrupt (computePerInst)"])
>             (singleton ibagi, deriveRange (ibagi+1) jbagi)
>         oIx                              = case icat of
>                                              InstCatPerc ws        → ws
>                                              _                     → oIx_
>         gList                            = map (buildZone sffile defZone)               gIx
>         gZone                            = (snd . head)                                 gList
>         oList                            = map (buildZone sffile gZone)                 oIx
>
>         trace_CPI                        = unwords ["computePerInst", show pgkwFile, zFilename, iName]
>
> buildZone              :: SFFile → SFZone → Word → (ZoneHeader, SFZone)
> buildZone SFFile{ .. } fromZone bagIndex
>                                  = (zh, zone)
>   where
>     SoundFontArrays{ .. }        = zArrays
>
>     xgeni                        = F.genNdx $ ssIBags!bagIndex
>     ygeni                        = F.genNdx $ ssIBags!(bagIndex + 1)
>     xmodi                        = F.modNdx $ ssIBags!bagIndex
>     ymodi                        = F.modNdx $ ssIBags!(bagIndex + 1)
>
>     gens       :: [F.Generator]  =
>       profess
>         (xgeni <= ygeni)
>         (unwords["SoundFont file", show zWordF, zFilename, "corrupt (buildZone gens)"])
>         (map (ssIGens !) (deriveRange xgeni ygeni))
>     mods       :: [(Word, F.Mod)]
>                                  =
>       profess
>         (xmodi <= ymodi)
>         (unwords["SoundFont file", show zWordF, zFilename, "corrupt (buildZone mods)"])
>         (zip [10_000..] (map (ssIMods !) (deriveRange xmodi ymodi)))
>
>     zone@SFZone{ .. }                    = foldr addMod (foldl' addGen fromZone gens) mods
>     si                                   = fromMaybe 0 zSampleIndex
>     zh                                   = ZoneHeader bagIndex (ssShdrs ! si)
>
> formSampleParentCache  :: Map PerGMKey PerInstrument → IO (Map PreSampleKey [PerGMKey])
> formSampleParentCache zc                 = return $ Map.foldlWithKey spFolder Map.empty zc
>   where
>     spFolder           ::  Map PreSampleKey [PerGMKey] → PerGMKey → PerInstrument → Map PreSampleKey [PerGMKey]
>     spFolder accum pergm@PerGMKey{pgkwFile} PerInstrument{pZonePairs}
>                                          = 
>       let
>         parentFolder   :: Map PreSampleKey [PerGMKey] → (ZoneHeader, SFZone) → Map PreSampleKey [PerGMKey]
>         parentFolder accum'  (ZoneHeader{zhwBag, zhShdr}, SFZone{zSampleIndex})
>                                          = Map.insert presk now accum'
>           where
>             presk                        =
>               PreSampleKey
>                 pgkwFile
>                 (deJust (unwords ["formSampleParentCache", "zSampleIndex"]) zSampleIndex) 
>             pergm'                       = pergm{pgkwBag = Just zhwBag}
>             soFar, now :: [PerGMKey]
>             soFar                        = fromMaybe [] (Map.lookup presk accum')
>             now                          = pergm' : soFar
>       in
>         foldl' parentFolder accum (tail pZonePairs) 

define signal functions and instrument maps to support rendering ======================================================

> prepareInstruments     :: SFRoster → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments sfrost@SFRoster{ .. } = 
>     return $ (Percussion, assignPercussion pmap)                                                 : imap
>   where
>     WinningRecord{ .. }                  = zWinningRecord
>     imap                                 = Map.foldrWithKey imapFolder [] pWinningI
>     pmap                                 = Map.foldrWithKey pmapFolder [] pWinningP
>
>     imapFolder kind PerGMScored{ .. } accum
>                                          = (kind, assignInstrument pPerGMKey)                    : accum
>
>     pmapFolder kind PerGMScored{ .. } accum
>                                          = (kind, (pgkwFile pPerGMKey, pgkwInst pPerGMKey))      : accum
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
> instrumentSF sfrost@SFRoster{zFiles, zZoneCache, zSample2Insts} pergm@PerGMKey{ .. } dur pchIn volIn params
>   | traceIf trace_ISF False              = undefined
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
>     perI                                 = fromJust $ Map.lookup pergm zZoneCache
>     arrays                               = zArrays (zFiles ! pgkwFile)
>     nameI                                = F.instName $ ssInsts arrays ! pgkwInst
>     trace_ISF                            =
>       unwords ["instrumentSF", show pgkwFile, nameI, show (pchIn, volIn), show dur]
>
>     (reconX@Recon{ .. }, mreconX)        =
>       case setZone of
>         Left (zL, sL)                    → (recon zL sL noon, Nothing)
>         Right ((zL, sL), (zR, sR))       → reconLR ((zL, sL), (zR, sR)) noon

zone selection for rendering ==========================================================================================

>     setZone            :: Either (SFZone, F.Shdr) ((SFZone, F.Shdr), (SFZone, F.Shdr))
>     setZone                              = eor
>       where
>         zs                               = tail $ pZonePairs perI
>         ezones                           = selectZonePair $ selectBestZone zs noon
>         eor                              =
>           case ezones of 
>             Left (zhL, zoneL)            → Left (zoneL, zhShdr zhL)
>             Right ((zhL, zoneL), (zhR, zoneR))
>                                          → Right ((zoneL, zhShdr zhL), (zoneR, zhShdr zhR))
>
>     selectBestZone     :: [(ZoneHeader, SFZone)] → NoteOn → (ZoneHeader, SFZone)
>     selectBestZone zs noon
>       | traceNot trace_SBZ False         = undefined
>       | otherwise                        = snd whichZ
>       where
>         scores         :: [(Int, (ZoneHeader, SFZone))]
>                                          = mapMaybe (scoreOneZone noon) zs
>         whichZ                           = profess
>                                              (not $ null scores)
>                                              (unwords ["scores should not be null (selectBestZone)"])
>                                              (minimumBy (comparing fst) scores)
>         shdr                             = zhShdr ((fst . snd) whichZ)
>         trace_SBZ                        =
>           unwords ["selectBestZone", show $ F.sampleName shdr, show $ F.sampleType shdr]
>
>     selectZonePair         :: (ZoneHeader, SFZone)
>                               → Either (ZoneHeader, SFZone) ((ZoneHeader, SFZone), (ZoneHeader, SFZone))
>     selectZonePair zone
>        | stype == SampleTypeLeft         = Right (zone, ozone)
>        | stype == SampleTypeRight        = Right (ozone, zone)
>        | otherwise                       = Left zone
>        where
>          F.Shdr{sampleType}              = (zhShdr . fst) zone
>          stype                           = toSampleType sampleType
>          ozone                           = deJust "ozone" (findStereoPartner sfrost pergm zone)
>
> findStereoPartner      :: SFRoster → PerGMKey → (ZoneHeader, SFZone) → Maybe (ZoneHeader, SFZone)
> findStereoPartner SFRoster{zZoneCache, zSample2Insts} PerGMKey{pgkwFile = myFile, pgkwInst = myInst} myZonePair
>                                          =
>   if not $ isStereoZone myZonePair
>     then error $ unwords["findStereoPartner", "attempted on non-stereo zone"]
>     else
>       case toSampleType (F.sampleType myShdr) of
>         SampleTypeLeft                   → Just partner
>         SampleTypeRight                  → Just partner
>         _                                → Nothing 
>   where
>     (ZoneHeader{zhShdr = myShdr}, SFZone{zSampleIndex = mmySI})
>                                          = myZonePair
>     targetSI                             = F.sampleLink myShdr
>
>     presk                                = PreSampleKey myFile targetSI
>     theirP@PerGMKey{pgkwFile = theirFile, pgkwInst = theirInst}
>                                          = last $ deJust "mtargetPs" (Map.lookup presk zSample2Insts)
>       
>     mtheirPerI                           =
>       profess
>         (myFile == theirFile && (allowStereoCrossovers || (myInst == theirInst)))
>         (unwords ["findStereoPartner:", "illegal crossover"])
>         (Map.lookup theirP{pgkwBag = Nothing} zZoneCache)
>     they                                 = (deJust "mtheirPerI" mtheirPerI).pZonePairs
>     them                                 =
>       find (\(_, SFZone{zSampleIndex = mtheirSI}) → mtheirSI == Just targetSI) they
>     partner                              = deJust "them" them      
>
> scoreOneZone           :: NoteOn
>                           → (ZoneHeader, SFZone)
>                           → Maybe (Int, (ZoneHeader, SFZone))
> scoreOneZone NoteOn{ .. } (zh, zone@SFZone{zKeyRange, zVelRange})
>   | traceNot trace_SOZ False             = undefined
>   | otherwise                            =
>     if DAllOn /= qqDesireReStereo defT || isStereoZone (zh, zone)
>       then Just (scoreByPitch + scoreByVelocity, (zh, zone))
>       else Nothing
>   where
>     scoreByPitch                         = scorePitchDistance    noteOnKey zKeyRange
>     scoreByVelocity                      = scoreVelocityDistance noteOnVel zVelRange
>
>     trace_SOZ                             = unwords ["scoreOneZone", show scoreByPitch
>                                                    , "+",            show scoreByVelocity]

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
>                                          = recon
>   where
>     xSampleRate      = fromIntegral          sampleRate
>     xStart           = addIntToWord          start                  (sumOfMaybeInts
>                                                                        [zStartOffs, zStartCoarseOffs])
>     xEnd             = addIntToWord          end                    (sumOfMaybeInts
>                                                                        [zEndOffs, zEndCoarseOffs])
>     m8n                                  = reconModulation zone sHdr noon
>     recon = Recon {
>     rSampleMode      = fromMaybe             A.NoLoop                zSampleMode
>   , rSampleRate      = xSampleRate
>   , rStart           = xStart
>   , rEnd             = xEnd
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
>     reconPitchCorrection alt mps mpc     = fromMaybe ((fromCents . fromIntegral) alt) (fromCents' mps mpc)
>
>     reconAttenuation   :: Maybe Int → Double
>     reconAttenuation matten              = if useAttenuation
>                                              then maybe 0 fromIntegral zInitAtten
>                                              else 0.0
>
> reconModulation        :: SFZone → F.Shdr → NoteOn → Modulation
> reconModulation sfz@SFZone{ .. } F.Shdr{ .. } noon
>   | traceIf trace_RM False               = undefined
>   | otherwise                            = resolveMods m8n zModulators defaultMods
>   where
>     trace_RM                             = unwords ["reconModulation", sampleName, show sfz]
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
>     nModLfo            :: Maybe LFO      =
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
> printChoices SFRoster{ .. } is msgs ps   = (map (showI zWinningRecord) is, map (showP zWinningRecord) ps)
>   where
>     showI              :: WinningRecord → InstrumentName → (Bool, [Emission])
>     showI WinningRecord{ .. } kind       = result
>       where
>         mpergm                           = Map.lookup kind pWinningI
>         result
>           | isJust mpergm                = (True, [gmId kind, Unblocked " -> "] 
>                                                    ++ showPerGM (fromJust mpergm)
>                                                    ++ [EndOfLine]
>                                                    ++ emitMsgs kind msgs)
>           | kind == Percussion           = (True, [gmId kind, Unblocked "(pseudo-instrument)", EndOfLine])
>           | otherwise                    = (False, [gmId kind, Unblocked " not found", EndOfLine])
>
>     showP               :: WinningRecord → PercussionSound → (Bool, [Emission])
>     showP WinningRecord{ .. } kind       = result
>       where
>         mpergm                           = Map.lookup kind pWinningP
>         result
>           | isJust mpergm                = (True, [gmId kind, Unblocked " -> "]
>                                                    ++ showPerGM (fromJust mpergm)
>                                                    ++ [EndOfLine])
>           | otherwise                    = (False, [gmId kind, Unblocked " not found", EndOfLine])
>
>     showPerGM          :: PerGMScored → [Emission]
>     showPerGM PerGMScored{ .. }          = [emitShowL pgkwFile 4] ++ [ToFieldL szI 22] ++ showmZ
>       where
>         PerGMKey{ .. }                   = pPerGMKey
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
> dumpContestant PerGMScored{ .. }         = es
>   where
>     ArtifactGrade{ .. }                  = pArtifactGrade
>     PerGMKey{ .. }                       = pPerGMKey
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
> showDisqMsgs ms                = do
>   mapM_ showIfThere ms
>   where
>     showIfThere str                  = CM.unless (null str) (putStrLn str)
>
> data DisqReason                          =
>     DisqUnrecognized
>   | DisqCorruptId String String
>   | DisqCorruptHeader 
>   | DisqNarrow
>   | DisqRomBased
>   | DisqNoPercZones
>   | DisqLinkage
>   | DisqCorruptRange
>   | DisqOverlappingRanges
>   | DisqIllegalCrossover deriving Show
>
> qqIncludeUnused          :: Bool         = False
>
> renderDisqReason         :: DisqReason → Maybe String
> renderDisqReason DisqUnrecognized        = Just (unwords["unrecognized"])
> renderDisqReason (DisqCorruptId strType strName)
>                                          = Just (unwords["corrupt", strType, strName])
> renderDisqReason DisqCorruptHeader       = Just (unwords["corrupt header; e.g. sample rate"])
> renderDisqReason DisqNarrow              = if qqIncludeUnused then Just (unwords["narrow inst scope"]) else Nothing
> renderDisqReason DisqRomBased            = Just (unwords["ROM-based"])
> renderDisqReason DisqNoPercZones         = if qqIncludeUnused then Just (unwords["unused zone liat"]) else Nothing
> renderDisqReason DisqLinkage             = Just (unwords["zone linkage"])
> renderDisqReason DisqCorruptRange        = Just (unwords["illegal zone range"])
> renderDisqReason DisqOverlappingRanges   = Just (unwords["overlapping zone ranges"])
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
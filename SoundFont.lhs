> {-# LANGUAGE InstanceSigs #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
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

> module SoundFont (doEverything) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Char
> import Data.Either
> import Data.Foldable ( toList )
> import Data.Int ( Int8, Int16 )
> import Data.IntSet (IntSet)
> import qualified Data.IntSet             as IntSet
> import Data.List ( find, foldr, minimumBy, singleton, foldl', sortOn, partition )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, mapMaybe, listToMaybe )
> import Data.Ord ( Down(Down), comparing )
> import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
> import Debug.Trace ( traceIO )
> import Discrete
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, Mono, Stereo, Clock, Signal )
> import Euterpea.Music
> import Modulation
> import Parthenopea
> import Synthesizer
> import qualified System.FilePattern.Directory
>                                          as FP
  
notes on three kinds of scoring =======================================================================================

In order of when they occur in the overall process:

1. FuzzyFind        - For each *.sf2, we record qualifying items into roster whose names match keywords for GM
                      instrument and percussion names -- e.g. "Iowa Viola-pp" maps to Viola. The highest FuzzyFind
                      results figure into "tournament" selection. But only the instrument selections are _profoundly_
                      affected by fuzziness. PercussionSound winners go mostly by matching "pitch" with zonal key
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
>   , pskwSample         :: Word} deriving (Eq, Ord, Show)
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
>   , pgkwBag            :: Maybe Word} deriving (Eq, Ord, Show)
>
> data PreCategory =
>   PreCategory {
>     pcPergmsI         :: [PerGMKey]
>   , pcFinishedIP      :: [PerGMKey]
>   , pcPermitted       :: Map PerGMKey [Word]}
> defPreCategory = PreCategory [] [] Map.empty
>
> data ArtifactGrade =
>   ArtifactGrade {
>     pScore             :: Int
>   , pEmpiricals        :: [Double]} deriving (Show)
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
> seedWinningRecord                        = WinningRecord Map.empty Map.empty []
>
> data ZoneHeader =
>   ZoneHeader {
>     zhwBag             :: Word
>   , zhShdr             :: F.Shdr} deriving Show
>
> data PerInstrument =
>   PerInstrument {
>     pInst              :: F.Inst
>   , pZonePairs         :: [(ZoneHeader, SFZone)]}

Instrument categories: instrument, percussion, disqualified

> data InstCat =
>        InstCatInst
>      | InstCatPerc
>      | InstCatDisq deriving Show
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
> type PreSampleCache                      = Map PreSampleKey PreSample
> type PreInstCache                        = Map PerGMKey     PreInstrument
> type ZoneCache                           = Map PerGMKey     PerInstrument
>
> data SFRoster =
>   SFRoster {
>     zFiles             :: Array Word SFFile
>   , zPreSampleCache    :: PreSampleCache
>   , zPreInstCache      :: PreInstCache
>   , zRoster            :: ([InstrumentName], [PercussionSound])
>   , zZoneCache         :: ZoneCache
>   , zWinningRecord     :: WinningRecord
>   , zPlayCache         :: Map PlayKey PlayValue}
>
> seedRoster :: Array Word SFFile → ([InstrumentName], [PercussionSound]) → SFRoster
> seedRoster vFile rost                    =
>   SFRoster vFile Map.empty Map.empty rost Map.empty seedWinningRecord Map.empty
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
>   , zdSampleIndex      :: Maybe Word} deriving Show
> defDigest              :: ZoneDigest
> defDigest                                = ZoneDigest Nothing Nothing
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
> data PlayKey =
>   PlayKey {
>     xItem              :: PerGMKey
>   , xNoteOn            :: NoteOn} deriving (Eq, Ord, Show)
> type PlayValue                           = (Recon, Maybe Recon)

executive =============================================================================================================

> doEverything           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> doEverything songs = do
>
>     putStrLn "everything..."
>     putStrLn $ reapEmissions emitSettingses
>
>     tsStarted          ← getCurrentTime
>
>     rost               ← qualifyKinds songs
>     putStrLn $ unwords ["rost", show rost]
>
>     -- represent all input SoundFont files in ordered list, thence a vector
>     fps                ← FP.getDirectoryFiles "." (singleton "*.sf2")
>     sffilesp           ← CM.zipWithM openSoundFontFile [0..] fps
>
>     let boundsF::(Word, Word)
>                        = (0, fromIntegral (length sffilesp - 1))
>     let preRoster      = seedRoster (listArray boundsF sffilesp) rost
>
>     tsLoaded           ← getCurrentTime
>     putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>     -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>     sfrost             ← finishRoster preRoster
>
>     CM.when doRender (doRendering sfrost)
>
>     tsRendered         ← getCurrentTime
>     putStrLn ("___overall: "             ++ show (diffUTCTime tsRendered tsStarted))
>
>     where
>       -- track the complete populations of: samples, instruments, percussion
>       finishRoster     :: SFRoster → IO SFRoster
>       finishRoster preRoster@SFRoster{ .. }
>                                          = do
>         tsStarted      ← getCurrentTime
>
>         presks         ← formMasterSampleList zFiles
>         pergmsI_       ← formMasterInstList   zFiles
>
>         preSampleCache ← formPreSampleCache zFiles presks
>         preInstCache   ← formPreInstCache   zFiles pergmsI_
>
>         preCategory    ← categorize zFiles preSampleCache preInstCache zRoster pergmsI_
>         let PreCategory{ .. }            = preCategory
>         tsCatted       ← getCurrentTime
>         putStrLn ("___categorize: " ++ show (diffUTCTime tsCatted tsStarted))
>
>         (pergmsP, skMap, zc)
>                        ← formZoneCache zFiles preSampleCache preInstCache zRoster preCategory
>
>         CM.when diagnosticsEnabled
>           (do
>             print "pergmsI"
>             print pcPergmsI
>             print "pergmsP"
>             print pergmsP)
>
>         tsZoned        ← getCurrentTime
>         putStrLn ("___cache zones: " ++ show (diffUTCTime tsZoned tsCatted))
>
>         -- actually conduct the tournament
>         ((wI, sI), (wP, sP))
>                        ← decideWinners zFiles preInstCache preSampleCache zc skMap zRoster pcPergmsI pergmsP
>         tsDecided      ← getCurrentTime
>         putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsZoned))
>
>         CM.when reportTourney (writeTournamentReport zFiles wI wP)
>         tsReported     ← getCurrentTime
>
>         let wins@WinningRecord{ .. }
>                        = WinningRecord (Map.map head wI) (Map.map head wP) (sI ++ sP)
>
>         -- print song/orchestration info to user (can be captured by redirecting standard out)
>         mapM_ putStrLn pWarnings
>
>         playCacheI     ← createPlayCache zFiles zc preSampleCache preInstCache skMap pWinningI
>         playCacheP     ← createPlayCache zFiles zc preSampleCache preInstCache skMap pWinningP
>
>         let sfrost = preRoster{zPreSampleCache     = preSampleCache
>                                , zPreInstCache     = preInstCache
>                                , zZoneCache        = zc
>                                , zWinningRecord    = wins
>                                , zPlayCache        = Map.union playCacheI playCacheP}
>
>         tsRecond   ← getCurrentTime
>         putStrLn ("___create play cache: " ++ show (diffUTCTime tsRecond tsReported))
>         
>         return sfrost
>
>       -- get it on
>       doRendering      :: SFRoster → IO ()
>       doRendering sfrost                 = do
>         tsStarted      ← getCurrentTime
>
>         -- readying instrument maps to be accessed from song renderer
>         traceIO         "prepareInstruments"
>         imap           ← prepareInstruments sfrost
>         tsPrepared     ← getCurrentTime
>         putStrLn ("___prepare instruments: " ++ show (diffUTCTime tsPrepared tsStarted))
>
>         -- here's the heart of the coconut
>         mapM_ (uncurry (renderSong sfrost imap)) songs
>
>         tsRendered     ← getCurrentTime
>         putStrLn ("___render songs: "        ++ show (diffUTCTime tsRendered tsPrepared))
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
>                           → PreInstCache
>                           → PreSampleCache
>                           → ZoneCache
>                           → Map PerGMKey PreSampleKey
>                           → ([InstrumentName], [PercussionSound]) 
>                           → [PerGMKey]
>                           → [PerGMKey]
>                           → IO ((Map InstrumentName [PerGMScored], [String])
>                               , (Map PercussionSound [PerGMScored], [String]))
> decideWinners sffiles preInstCache preSampleCache zc skMap rost pergmsI pergmsP
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
>     wiFolder accum pergmI@PerGMKey{ .. } = foldl' (xaEnterTournament fuzzMap pergmI []) accum as'
>       where
>         -- access potentially massive amount of processed information regarding instrument
>         PreInstrument{ .. }              =
>           professIsJust (Map.lookup pergmI preInstCache) (unwords ["no PreInstrument?!"])
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
>         mz                               = pgkwBag >>= lookupZone pZonePairs
>         mkind          :: Maybe PercussionSound
>         mkind                            = mz >>= getAP >>= pitchToPerc
>         kind                             = professIsJust mkind (unwords["wpFolder: pitchToPerc returned a Nothing"])
>
>         mffm           :: Maybe FFMatches
>         mffm                             =
>           mz >>= (zSampleIndex . snd)
>              >>= Just . PreSampleKey pgkwFile
>              >>= flip Map.lookup preSampleCache
>              >>= Just . sMatches
>         fuzzMap        :: Map PercussionSound Fuzz
>         fuzzMap                          = getFuzzMap $ professIsJust mffm (unwords ["couldn't get PreSample?"])
>
>         PerInstrument{ .. }              =
>           professIsJust (Map.lookup (pergmP{pgkwBag = Nothing}) zc) (unwords["no PerInstrument?!"])
>
>         getAP          :: (ZoneHeader, SFZone) → Maybe AbsPitch
>         getAP (_, zone@SFZone{ .. })     = (Just . fst) =<< zKeyRange
>
>         trace_WP                         = unwords ["wpFolder", show pergmP]
>
>     lookupZone         :: [(ZoneHeader, SFZone)] → Word → Maybe (ZoneHeader, SFZone)
>     lookupZone zs bagI                   = find (\(ZoneHeader{ .. }, _) → bagI == zhwBag) zs

tournament among GM instruments and percussion from SoundFont files ===================================================

>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           Map a Fuzz
>                           → PerGMKey
>                           → [SSHint]
>                           → (Map a [PerGMScored], [String])
>                           → a
>                           → (Map a [PerGMScored], [String])
>     xaEnterTournament fuzzMap pergm@PerGMKey{ .. } hints (wins, ss) kind
>       | traceNot trace_XAET False        = undefined
>       | otherwise                        = (Map.insert kind now wins, ss)
>       where
>         pergm_                           = pergm{pgkwBag = Nothing}
>         PreInstrument{ .. }              =
>           professIsJust (Map.lookup pergm_ preInstCache)        (unwords["no PreInstrument?!"])
>         PerInstrument{ .. }              =
>           professIsJust (Map.lookup pergm_ zc)                  (unwords["no PerInstrument?!"])
>
>         soFar          :: [PerGMScored]  = fromMaybe [] (Map.lookup kind wins)
>         now                              = scoredP : soFar
>         akResult                         = fromMaybe 0 (Map.lookup kind fuzzMap)
>
>         scope          :: [(ZoneHeader, SFZone)]
>         scope                            =
>           case pgkwBag of
>             Nothing                      → tail pZonePairs
>             Just bagI                    →
>                    maybe
>                      (error $ unwords [ "xaEnterTournament: lookupZone returned a Nothing for"
>                                       , show pgkwFile, iName, zName])
>                      singleton (lookupZone pZonePairs bagI)
>         pk                               = fromJust $ Map.lookup pergm skMap
>         ps                               = fromJust $ Map.lookup pk preSampleCache
>         mnameZ         :: Maybe String   = pgkwBag >>= \w → Just (sName ps)
>         zName                            =
>           professIsJust mnameZ (unwords ["xaEnterTournament: bad pgkwBag"])
>
>         scoredP        :: PerGMScored    =
>           PerGMScored
>             (computeGrade scope akResult)
>             (toKind kind)
>             akResult pergm iName mnameZ 
>
>         trace_XAET                       = unwords ["xaEnterTournament", show kind, show mnameZ]
>
>         computeGrade   :: [(ZoneHeader, SFZone)] → AgainstKindResult → ArtifactGrade
>         computeGrade zs againstKindResult
>           | traceIf trace_CG False       = undefined
>           | otherwise                    = ArtifactGrade (round (500 * sum weightedScores)) baseScores
>           where
>             empiricals :: [Double]       = [   fromRational $ scoreBool $ isStereoInst zs
>                                              , fromRational $ scoreBool $ is24BitInst zs
>                                              , computeResolution kind rost zs
>                                              , fromRational $ scoreBool $ all zoneConforms zs
>                                              , fuzz]
>             howgood                      = againstKindResult - stands
>             fuzz       :: Double
>               | howgood > 0.000_001      = max 0 (logBase 2 howgood) * fuzzFactor kind
>               | otherwise                = 0
>             ss         :: [Double]       = zipWith (*) qqDesires' empiricals
>             baseScores :: [Double]       = [  foldHints hints
>                                             , head ss
>                                             , ss !! 1
>                                             , ss !! 2
>                                             , ss !! 3
>                                             , ss !! 4]
>             weightedScores
>                        :: [Double]       = zipWith (*) baseScores (map fromRational ssWeights)
>
>             trace_CG                     =
>               unwords [   "computeGrade " , show pgkwFile, show iName
>                         , " and "         , show baseScores
>                         , " X "           , show ssWeights
>                         , " = "           , show weightedScores]
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
>                           → IO PreSampleCache
> formPreSampleCache sffiles presks
>                                          = return $ foldl' smFolder Map.empty presks
>   where
>     smFolder accum presk@PreSampleKey{pskwFile, pskwSample}
>                                          =
>       let
>         SFFile{zArrays}                  = sffiles ! pskwFile
>         SoundFontArrays{ssShdrs}         = zArrays
>       in
>         Map.insert presk (computePreSample (ssShdrs ! pskwSample) presk) accum
>
> formPreInstCache       :: Array Word SFFile → [PerGMKey] → IO PreInstCache
> formPreInstCache sffiles pergms          = return $ foldl' imFolder Map.empty pergms
>   where
>     imFolder accum pergm@PerGMKey{pgkwFile}
>                                          =
>       Map.insert pergm (computePreInstrument (sffiles ! pgkwFile) pergm) accum
>
> computePreSample       :: F.Shdr → PreSampleKey → PreSample
> computePreSample F.Shdr{originalPitch, sampleName} k
>   | traceNot trace_CPS False             = undefined
>   | otherwise                            = PreSample inp ffs
>   where
>     ap                                   = fromIntegral originalPitch
>     apLow                                = fromIntegral $ min 127 (ap - 6)
>     apHigh                               = fromIntegral $ max 0   (ap + 4)
>
>     inp                                  = sampleName
>     ffs                                  = computeFFMatches inp
>
>     trace_CPS                            = unwords ["computePreSample", show (inp, k)]
>
> computePreInstrument       :: SFFile → PerGMKey → PreInstrument
> computePreInstrument SFFile{ .. } PerGMKey{ .. }
>                                          = PreInstrument instName (computeFFMatches instName)
>   where
>     SoundFontArrays{ .. }                = zArrays
>     F.Inst{ .. }                         = ssInsts ! pgkwInst
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
> formMasterPercussionList
>                        :: ZoneCache
>                           → PreSampleCache
>                           → [PerGMKey]
>                           → ([PerGMKey], Map PerGMKey PreSampleKey)
> formMasterPercussionList zc _            = foldr pfolder ([], Map.empty)
>   where
>     pfolder            :: PerGMKey
>                           → ([PerGMKey], Map PerGMKey PreSampleKey)
>                           → ([PerGMKey], Map PerGMKey PreSampleKey)
>     pfolder pergmI (pergmsP, skMap)      =
>       let
>         pergmI'                          = pergmI{pgkwBag = Nothing}
>         mperI                            = Map.lookup pergmI' zc
>         perI                             =
>           professIsJust mperI (unwords["no PerInstrument in cache for", show pergmI'])
>         (pergmsP', skMap')               = instrumentPercList pergmI perI
>       in
>         (pergmsP ++ pergmsP', Map.union skMap skMap')
>
>     instrumentPercList :: PerGMKey → PerInstrument → ([PerGMKey], Map PerGMKey PreSampleKey)
>     instrumentPercList pergmI PerInstrument{ .. }
>                                          =
>       let
>         pWords         :: [(Word, Word)]
>         pWords                           =
>           map (BF.bimap zhwBag (flip professIsJust "zSampleIndex" . zSampleIndex)) (tail pZonePairs)
>         pKeys          :: [(PerGMKey, PreSampleKey)]
>         pKeys                            =
>           map (BF.bimap (\w → pergmI {pgkwBag = Just w}) (PreSampleKey (pgkwFile pergmI))) pWords
>       in
>         (map fst pKeys, Map.fromList pKeys)
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
>       let arrays = SoundFontArrays
>                      (F.insts pdata)
>                      (F.ibags pdata)
>                      (F.igens pdata)
>                      (F.imods pdata)
>                      (F.shdrs pdata)
>                      (F.smpl sdata)
>                      (F.sm24 sdata)
>       let sffile = SFFile filename arrays wFile
>       let nBits = case ssM24 (zArrays sffile) of
>             Nothing          → 16
>             Just s24data     → 24
>       ts2 ← getCurrentTime
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
>     CM.unless (null (Map.assocs dynMap)) (traceIO ("dynMap " ++ show dynMap))
>     let ks                               = Map.keys shRanges
>     let (is, ps)                         = (map (\i → fromMaybe i (Map.lookup i dynMap)) (lefts ks), rights ks)
>     let (esI, esP)                       = printChoices sfrost is shMsgs ps
>     let es                               = emitCounts is ps name ++ concatMap snd esI ++ concatMap snd esP
>     putStr (reapEmissions es)
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let path                         = name ++ ".wav"
>         putStr path
>         let (d,s)                        = renderSF (song dynMap) imap
>         if scanningOutput
>           then findOutliers d s
>           else do
>             traceIO (unwords ["-> outFile*", path, show d])
>             if normalizingOutput
>               then outFileNorm path d s
>               else outFile     path d s
>             traceIO (unwords ["<- outFile*", path, show d])
>         ts2                              ← getCurrentTime
>         putStrLn (" (dur=" ++ show d ++ ") written in " ++ show (diffUTCTime ts2 ts1))
>       else
>         putStrLn "skipping..."
>     return ()
>
> emitCounts             :: [InstrumentName] → [PercussionSound] → String → [Emission]
> emitCounts is ps name                         =
>   [EndOfLine]
>     ++ [emitShowL name 22, emitShowL (length is) 4]
>     ++ [Unblocked " /- instruments, percussion -/ ", emitShowL (length ps) 4, EndOfLine]
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
> isStereoZone (ZoneHeader{ .. }, _)       =
>   maybe False (\t → t == SampleTypeRight || t == SampleTypeLeft) (toMaybeSampleType $ F.sampleType zhShdr)
>         
> zoneConforms (ZoneHeader{ .. }, SFZone{ .. })
>                                          = not $ or unsupported
>   where
>     F.Shdr{ .. }                         = zhShdr
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
>             Just n                       → n /= 0 && n /= 100
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
>                           → PreSampleCache
>                           → PreInstCache
>                           → ([InstrumentName], [PercussionSound])
>                           → [PerGMKey]
>                           → IO PreCategory
> categorize sffiles preSampleCache preInstCache rost pergms
>                                          = do CM.foldM winnow defPreCategory pergms
>   where
>     winnow             :: PreCategory → PerGMKey → IO PreCategory
>     winnow prec@PreCategory{ .. } pergm@PerGMKey{ .. }
>                                          = do
>       let doShow                         = case cat of
>                                               InstCatDisq          → renderDisqReason (InstCatDisq, reason)
>                                               _                    → Nothing
>
>       CM.when (isJust doShow)
>               (putStrLn $ unwords ["disq:", show pgkwFile, show iName, ":", fromMaybe [] doShow])
>
>       return prec'
>
>       where
>         PreInstrument{iName}             =
>           professIsJust (Map.lookup pergm preInstCache) (unwords ["no PreInstrument?!"])
>         (cat, words, reason)             = categorizeInst pergm
>         prec'                            =
>           case cat of
>             InstCatInst                  → PreCategory (pergm : pcPergmsI) pcFinishedIP pcPermitted
>             InstCatPerc                  → PreCategory pcPergmsI (pergm : pcFinishedIP) (Map.insert pergm words pcPermitted)
>             InstCatDisq                  → prec
>
>     categorizeInst     :: PerGMKey → (InstCat, [Word], DisqReason)
>     categorizeInst pergm@PerGMKey{ .. }
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = (icat', words', reason')
>       where
>         SoundFontArrays{ .. }            = zArrays (sffiles ! pgkwFile)
>
>         iinst                            = ssInsts ! pgkwInst
>         jinst                            = ssInsts ! (pgkwInst+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
>         zs             :: [(ZoneHeader, ZoneDigest)]
>         zs                               = map inspectZone (tail (deriveRange ibagi jbagi))
>
>         PreInstrument{ .. }              =
>           professIsJust (Map.lookup pergm preInstCache) (unwords ["no PreInstrument?!"])
>         FFMatches{ .. }                  = iMatches
>
>         (icat, reason)                   = fromMaybe (InstCatDisq, DisqUnknown) (foldr CM.mplus Nothing alts)
>
>         (icat', words')                  =
>           case icat of
>             InstCatPerc                  → if null wZones
>                                              then (InstCatDisq, [])
>                                              else (icat, wZones)
>             _                            → (icat, [])
>
>         reason'                          =
>           case icat' of
>             InstCatDisq                  → reason
>             _                            → DisqOk                 
>
>         corrupt        :: Maybe (InstCat, DisqReason)
>         corrupt                          =
>           foldl' sampler Nothing zs `CM.mplus` checkName "Inst" iName
>           where
>             sampler    :: Maybe (InstCat, DisqReason) → (ZoneHeader, ZoneDigest) → Maybe (InstCat, DisqReason)
>             sampler accum (ZoneHeader{ .. }, _)
>                                          =
>               let
>                 F.Shdr{ .. }             = zhShdr
>               in
>                 checkShdr zhShdr `CM.mplus` checkName "Sample" sampleName
>
>         checkName      :: String → String → Maybe (InstCat, DisqReason)
>         checkName strType strName        =
>           if length strName <= 20 && length (show strName) <= 22
>             then Nothing
>             else Just (InstCatDisq, DisqNameCorrupt strType (show strName))
>
>         checkShdr      :: F.Shdr → Maybe (InstCat, DisqReason)
>         checkShdr F.Shdr{ .. }           =
>           if sampleRate > 0 && sampleRate < 2^31 && isJust (toMaybeSampleType sampleType)
>             then Nothing
>             else Just (InstCatDisq, DisqSampleHeaderCorrupt)
>
>         howLaden       :: [Word] → Double
>         howLaden ws
>           | null zs                      = 0
>           | otherwise                    = (fromIntegral . length) ws / (fromIntegral . length) zs
>
>         uZones         :: [Word]         = mapMaybe (evalForPerc allKinds) zs
>         wZones         :: [Word]         = mapMaybe (evalForPerc rost) zs
>
>         ffInst'                          = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) ffInst
>         ffPerc'                          = Map.filterWithKey (\k v → k `elem` select rost && isPossible' v) ffPerc
>
>         ffAllInst                        = Map.elems ffInst
>
>         xreason reason _                 = Just reason
>
>         maybeSettle    :: (Foldable t) ⇒ Fuzz → (InstCat, DisqReason) → t Fuzz → Maybe (InstCat, DisqReason)
>         maybeSettle thresh (icat, reason) keys
>                                          = embed icat $ find (> thresh) keys >>= xreason reason

   "now", sequence through the alternatives, categorizing as follows
   a. Just InstCatInst           an inst bearing one inst, or
   b. Just InstCatPerc           an inst bearing one or more percs, or
   c. Just InstCatDisq           an inst disqualified from all tournaments, or else
   d. Nothing                    undecided

>         alts           :: [Maybe (InstCat, DisqReason)]
>         alts                             =
>           [ corrupt
>           , if any hasRom zs then Just (InstCatDisq, DisqRomBased) else Nothing
>           , if IntSet.isSubsetOf sOut sIn then Nothing else Just (InstCatDisq, DisqZoneLinkage)
>           , maybeSettle isConfirmed (InstCatInst, DisqOk) ffInst'
>           , maybeSettle isConfirmed (InstCatPerc, DisqOk) ffPerc'
>           , maybeSettle stands      (InstCatInst, DisqOk) ffInst'
>           , maybeSettle stands      (InstCatDisq, DisqNarrow) ffAllInst
>           , if 0.75 < howLaden uZones
>               then (if 0.05 < howLaden wZones then Just (InstCatPerc, DisqOk) else Just (InstCatDisq, DisqNoPercZones))
>               else Nothing
>           , maybeSettle stands      (InstCatPerc, DisqOk) ffPerc'
>           -- WOX , if evalAgainstGeneric iName > 0 then Just (InstCatInst, DisqOk) else Nothing
>           -- WOX , if evalAgainstGeneric iName < 0 then Just (InstCatPerc, DisqOk) else Nothing
>           ]
>
>         evalForPerc    :: ([InstrumentName], [PercussionSound]) → (ZoneHeader, ZoneDigest) → Maybe Word
>         evalForPerc rost' (ZoneHeader{ .. }, ZoneDigest { .. })
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
>             si                           = professIsJust (zdSampleIndex zd) (unwords ["sample index"])
>             zh                           = ZoneHeader bagIndex (ssShdrs ! si)
>
>         inspectGen     :: F.Generator → ZoneDigest → ZoneDigest 
>         inspectGen (F.KeyRange i j) zd   = zd {zdKeyRange = Just (i, j)}
>         inspectGen (F.SampleIndex w) zd  = zd {zdSampleIndex = Just w}
>         inspectGen _ zd                  = zd
>
>         hasRom (ZoneHeader{ .. }, _)     = F.sampleType zhShdr >= 0x8000
>                                          
>         indices, links :: (ZoneHeader, ZoneDigest) → Maybe Int
>         sIn, sOut      :: IntSet
>
>         sIn                              = IntSet.fromList $ mapMaybe indices zs
>         sOut                             = IntSet.fromList $ mapMaybe links zs
>
>         indices (zh, zd@ZoneDigest{ .. })
>                                          =
>           if isStereoZone (zh, zd)
>             then Just $ fromIntegral $ professIsJust zdSampleIndex "no sample index?!"
>             else Nothing
>         links (zh@ZoneHeader{ .. }, zd)  =
>           if isStereoZone (zh, zd)
>             then Just $ fromIntegral $ F.sampleLink zhShdr
>             else Nothing
>
>         trace_CI                         = unwords ["categorizeInst", show pgkwFile, iName, show alts]
>
> formZoneCache          :: Array Word SFFile
>                           → PreSampleCache
>                           → PreInstCache
>                           → ([InstrumentName], [PercussionSound])
>                           → PreCategory
>                           → IO ([PerGMKey], Map PerGMKey PreSampleKey, ZoneCache)
> formZoneCache sffiles preSampleCache preInstCache rost PreCategory{ .. }
>                                          = 
>   return (pergmsP, skMap, zc)
>   where
>     pergmsP            :: [PerGMKey]
>     skMap              :: Map PerGMKey PreSampleKey
>     zc                 :: ZoneCache
>
>     zc                                   =
>       foldr (\p → Map.insert p (computePerInst p)) Map.empty (pcPergmsI ++ pcFinishedIP)
>     (pergmsP, skMap)                     = formMasterPercussionList zc preSampleCache pcFinishedIP
>
>     computePerInst     :: PerGMKey → PerInstrument
>     computePerInst pergm@PerGMKey{ .. }
>       | traceIf trace_CPI False          = undefined
>       | otherwise                        = PerInstrument iinst (gList ++ oList) 
>       where
>         trace_CPI                        = unwords ["computePerInst", show pgkwFile, iName]
>         PreInstrument{ .. }              = fromJust $ Map.lookup pergm preInstCache
>         SoundFontArrays{ .. }            = zArrays (sffiles ! pgkwFile)
>         iinst                            = ssInsts ! pgkwInst
>         jinst                            = ssInsts ! (pgkwInst+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
>         (gIx, oIx_)                      = profess
>                                              (ibagi <= jbagi)
>                                              "SoundFont file corrupt (computePerInst)"
>                                              (singleton ibagi, deriveRange (ibagi+1) jbagi)
>         oIx                              = fromMaybe oIx_                        (Map.lookup pergm pcPermitted)
>         gList                            = map (buildZone defZone)               gIx
>         gZone                            = (snd . head)                          gList
>         oList                            = map (buildZone gZone)                 oIx
>
>         buildZone      :: SFZone → Word → (ZoneHeader, SFZone)
>         buildZone fromZone bagIndex
>           | traceIf trace_BZ False       = undefined
>           | otherwise                    = (zh, zone)
>           where
>             xgeni                        = F.genNdx $ ssIBags!bagIndex
>             ygeni                        = F.genNdx $ ssIBags!(bagIndex + 1)
>             xmodi                        = F.modNdx $ ssIBags!bagIndex
>             ymodi                        = F.modNdx $ ssIBags!(bagIndex + 1)
>
>             gens       :: [F.Generator]  = profess
>                                              (xgeni <= ygeni)
>                                              "SoundFont file corrupt (buildZone gens)"
>                                              (map (ssIGens !) (deriveRange xgeni ygeni))
>             mods       :: [(Word, F.Mod)]
>                                          = profess
>                                              (xmodi <= ymodi)
>                                              "SoundFont file corrupt (buildZone mods)"
>                                              (zip [10_000..] (map (ssIMods !) (deriveRange xmodi ymodi)))
>
>             zone@SFZone{ .. }            = foldr addMod (foldl' addGen fromZone gens) mods
>             si                           = fromMaybe 0 zSampleIndex
>             zh                           = ZoneHeader bagIndex (ssShdrs ! si)
>
>             trace_BZ                     = unwords ["buildZone", show pgkwFile, iName, show bagIndex]
>
> pinnedKR               :: [PercussionSound] → (AbsPitch, AbsPitch) → Bool
> pinnedKR pss (p1, p2)                    = (p2 < p1 + 4) && all available [p1 .. p2]
>   where
>     available          :: AbsPitch → Bool
>     available ap                         = maybe False (`elem` pss) (pitchToPerc ap)

define signal functions and instrument maps to support rendering ======================================================

> prepareInstruments     :: SFRoster → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments sfrost@SFRoster{ .. } = 
>     return $ (Percussion, assignPercussion pmap)                                                   : imap
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
> instrumentSF SFRoster{ .. } pergm@PerGMKey{ .. } dur pchIn volIn params
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
>     arrays                               = zArrays (zFiles ! pgkwFile)
>     nameI                                = F.instName $ ssInsts arrays ! pgkwInst
>     trace_ISF                            =
>       unwords ["instrumentSF", show pgkwFile, nameI, show (pchIn, volIn), show dur]
>
>     (reconX@Recon{ .. }, mreconX)                    =
>       if usingPlayCache
>         then fromMaybe
>                (error $ unwords ["Note missing from play cache:", show noon])
>                (Map.lookup (PlayKey pergm noon) zPlayCache)
>         else computePlayValue pergm{pgkwBag = Nothing} zZoneCache noon
>
> computePlayValue       :: PerGMKey → ZoneCache → NoteOn → PlayValue
> computePlayValue pergm@PerGMKey{ .. } zc noon@NoteOn{ .. }
>   | traceNot trace_CPV False             = undefined
>   | otherwise                            =
>     case eZones of
>       Left (zoneL, shdrL)                → (recon zoneL shdrL noon, Nothing)
>       Right ((zoneL, shdrL), (zoneR, shdrR))
>                                          → reconLR ((zoneL, shdrL), (zoneR, shdrR)) noon
>   where
>     trace_CPV                            = unwords ["computePlayValue", show pergm]
>     eZones                               = setZone pergm zc noon

zone selection for rendering ==========================================================================================

> setZone                :: PerGMKey
>                           → ZoneCache
>                           → NoteOn
>                           → Either (SFZone, F.Shdr) ((SFZone, F.Shdr), (SFZone, F.Shdr))
> setZone pergm@PerGMKey{ .. } zc noon     = eor
>   where
>     PerInstrument{ .. }                  = fromJust $ Map.lookup pergm zc
>     zs                                   = tail pZonePairs
>     ezones                               = selectZonePair zs (selectBestZone zs noon)
>     eor                                  =
>       case ezones of 
>         Left (zhL, zoneL)                → Left (zoneL, zhShdr zhL)
>         Right ((zhL, zoneL), (zhR, zoneR))
>                                          → Right ((zoneL, zhShdr zhL), (zoneR, zhShdr zhR))
>
> selectBestZone         :: [(ZoneHeader, SFZone)] → NoteOn → (ZoneHeader, SFZone)
> selectBestZone zs noon
>   | traceNot trace_SBZ False             = undefined
>   | otherwise                            = snd whichZ
>   where
>     scores             :: [(Int, (ZoneHeader, SFZone))]
>                                          = mapMaybe (scoreOneZone noon) zs
>     whichZ                               = profess
>                                              (not $ null scores)
>                                              (unwords ["scores should not be null (selectBestZone)"])
>                                              (minimumBy (comparing fst) scores)
>     shdr                                 = zhShdr ((fst . snd) whichZ)
>     trace_SBZ                            =
>       unwords ["selectBestZone", show $ F.sampleName shdr, show $ F.sampleType shdr]
>
> selectZonePair         :: [(ZoneHeader, SFZone)]
>                           → (ZoneHeader, SFZone)
>                           → Either (ZoneHeader, SFZone) ((ZoneHeader, SFZone), (ZoneHeader, SFZone))
> selectZonePair zs zone
>    | stype == SampleTypeLeft             = Right (zone, ozone)
>    | stype == SampleTypeRight            = Right (ozone, zone)
>    | otherwise                           = Left zone
>    where
>      F.Shdr{sampleLink, sampleType}      = (zhShdr . fst) zone
>      stype                               = toSampleType sampleType
>      mozone                              = find (withSlink sampleLink) zs
>      ozone                               = professIsJust mozone (unwords["zone linkage"])
>
>      withSlink          :: Word → (ZoneHeader, SFZone) → Bool
>      withSlink toMatch czone             =
>        professIsJust
>          ((zSampleIndex . snd) czone)
>          "sampleIndex undefined" == toMatch
>
> scoreOneZone           :: NoteOn
>                           → (ZoneHeader, SFZone)
>                           → Maybe (Int, (ZoneHeader, SFZone))
> scoreOneZone NoteOn{ .. } (zh@ZoneHeader{ .. }, zone@SFZone{ .. })
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
>                                                    , "+",            show scoreByVelocity
>                                                    , "for",          show zhwBag]
>
> scorePitchDistance     :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> scorePitchDistance cand mrange           =
>   case mrange of
>     Nothing                   → 1000
>     Just (rangeMin, rangeMax) → let
>                                   dist1  = abs $ cand - rangeMin
>                                   dist2  = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 100 * max dist1 dist2
>                                   else 1000 * min dist1 dist2
>
> scoreVelocityDistance  :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> scoreVelocityDistance cand mrange        =
>   case mrange of
>     Nothing                   → 100
>     Just (rangeMin, rangeMax) → let
>                                   dist1  = abs $ cand - rangeMin
>                                   dist2  = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 10 * max dist1 dist2
>                                   else 100 * min dist1 dist2

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

"calculate and pre-cache" comprehensive play situations/results =======================================================

> createPlayCache        :: ∀ a. (Ord a) ⇒
>                           Array Word SFFile
>                           → ZoneCache
>                           → PreSampleCache
>                           → PreInstCache
>                           → Map PerGMKey PreSampleKey
>                           → Map a PerGMScored
>                           → IO (Map PlayKey (Recon, Maybe Recon))
> createPlayCache sffiles zc preSampleCache preInstCache _ wins
>                                          =
>   return
>     $ if usingPlayCache
>         then Map.foldrWithKey precalcFolder Map.empty wins
>         else Map.empty
>   where
>     precalcFolder      :: a
>                           → PerGMScored
>                           → Map PlayKey (Recon, Maybe Recon)
>                           → Map PlayKey (Recon, Maybe Recon)
>     precalcFolder kind pergm ps          = Map.union ps (precalcNotes kind pergm)
>
>     precalcNotes       :: a → PerGMScored → Map PlayKey (Recon, Maybe Recon)
>     precalcNotes kind PerGMScored{ .. }  = foldl' (playFolder pPerGMKey{pgkwBag = Nothing})
>                                                   Map.empty
>                                                   [NoteOn v k | v ← [0..127], k ← [0..127]]
>
>     playFolder         :: PerGMKey
>                           → Map PlayKey (Recon, Maybe Recon)
>                           → NoteOn
>                           → Map PlayKey (Recon, Maybe Recon)
>     playFolder pergm ps noon             =
>       Map.insert (PlayKey pergm noon) (computePlayValue pergm{pgkwBag = Nothing} zc noon) ps

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
>     , emitSettings defT
>     , emitSettings defD]
>
> emitSettings           :: Show a ⇒ a → [Emission]
> emitSettings def_                        =
>   [ Unblocked "\n\n"
>   , emitShowL def_ 1000]
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
> showEmpiricals         :: [Double] → (String, Int)
> showEmpiricals ds                        = (concatMap fun ds, 7 * length ds)
>   where
>     fun                :: Double → String
>     fun x                                = fillFieldL 6 (show $ roundBy 10 x)
>
> showDisqMsgs   :: [String] → IO ()
> showDisqMsgs ms                = do
>   mapM_ showIfThere ms
>   where
>     showIfThere str                  = CM.unless (null str) (putStrLn str)
>
> data DisqReason                          =
>     DisqOk
>   | DisqUnknown
>   | DisqNameCorrupt String String
>   | DisqSampleHeaderCorrupt 
>   | DisqNarrow
>   | DisqRomBased
>   | DisqNoPercZones
>   | DisqZoneLinkage deriving Show
>
> qqIncludeUnused          :: Bool         = False
>
> renderDisqReason         :: (InstCat, DisqReason) → Maybe String
> renderDisqReason (_, DisqOk)             = Nothing
> renderDisqReason (_, DisqUnknown)        = Just (unwords["unrecognized"])
> renderDisqReason (_, DisqNameCorrupt strType strName)
>                                          = Just (unwords["corrupt", strType, strName])
> renderDisqReason (_, DisqSampleHeaderCorrupt)
>                                          = Just (unwords["corrupt e.g. sample rate"])
> renderDisqReason (_, DisqNarrow)         = if qqIncludeUnused then Just (unwords["narrow inst scope"]) else Nothing
> renderDisqReason (_, DisqRomBased)       = Just (unwords["ROM-based"])
> renderDisqReason (_, DisqNoPercZones)    = if qqIncludeUnused then Just (unwords["unused zone liat"]) else Nothing
> renderDisqReason (_, DisqZoneLinkage)    = Just (unwords["zone linkage"])

Scoring stuff =========================================================================================================
 
> data SSHint =
>   DLow
>   | DMed
>   | DHigh
>   | DScore Double deriving Show
>
> scoreHint              :: SSHint → Rational
> scoreHint h                              = case h of
>                                            DLow            → -1
>                                            DMed            → 0
>                                            DHigh           → 1
>                                            DScore x        → 0 
>             
> foldHints              :: [SSHint] → Double
> foldHints                                = foldr ((+) . fromRational . scoreHint) 0
>
> ssWeights              :: [Rational]     = [ weighHints
>                                            , weighStereo
>                                            , weigh24Bit
>                                            , weighResolution
>                                            , weighConformance
>                                            , weighFuzziness ]
>
> -- hints
> data HintId =
>   HintId {
>     pNameF             :: String
>   , pNameI             :: String
>   , mpNameZ            :: Maybe String} deriving (Eq, Ord, Show)
>
> type HintBody          = String
>
> myHints                :: [(HintId, HintBody)]
>                                          =
>   [
>       (HintId "editHiDef.sf2"             "Rock Tom"            (Just "TOM_S446.446T.L08")      , "analyze")
>     , (HintId "editHiDef.sf2"             "Tuba"                (Just "Tuba.A-A*B")             , "analyze")
>   ]
>                           
> qqHints                :: Map HintId HintBody
> qqHints                                  = Map.fromList myHints

The End
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE NumericUnderscores #-}
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
> import Data.Either
> import Data.Foldable ( toList )
> import Data.Int ( Int8, Int16 )
> import Data.IntSet (IntSet)
> import qualified Data.IntSet             as IntSet
> import Data.List ( find, foldr, minimumBy, singleton, foldl', sortOn, partition )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, mapMaybe )
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
> data ArtifactGrade =
>   ArtifactGrade {
>     pScore             :: Int
>   , pEmpiricals        :: [Int]} deriving (Show)
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
>   , zhShdr             :: F.Shdr
>   , zhNonPitched       :: Maybe Bool} deriving Show
>
> data PerInstrument =
>   PerInstrument {
>     pInst              :: F.Inst
>   , pZonePairs         :: [(ZoneHeader, SFZone)]}

Instrument categories: instrument, percussion, disqualified

> data InstCat =
>        InstCatInst
>      | InstCatPerc
>      | InstCatDisq deriving (Show,Eq,Ord,Enum)
>     
> class GMPlayable a ⇒ SFScorable a where
>   splitScore           :: a → ([InstrumentName], [PercussionSound]) → [(ZoneHeader, SFZone)] → Double
>   fuzzFactor           :: a → Double
>
> instance SFScorable InstrumentName where
>   splitScore                             = instrumentSplitScore
>   fuzzFactor _                           = 7/8
>
> instance SFScorable PercussionSound where
>   splitScore                             = percussionSplitScore
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
>   , zQualified         :: ([InstrumentName], [PercussionSound])
>   , zZoneCache         :: ZoneCache
>   , zWinningRecord     :: WinningRecord
>   , zPlayCache         :: Map PlayKey (Recon, Maybe Recon)}
>
> seedRoster :: Array Word SFFile → ([InstrumentName], [PercussionSound]) → SFRoster
> seedRoster vFile qualified               =
>   SFRoster vFile Map.empty Map.empty qualified Map.empty seedWinningRecord Map.empty
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
>
> type PlayValue                           = (Recon, Maybe Recon)

executive =============================================================================================================

> doEverything           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> doEverything songs = do
>
>     putStrLn "everything..."
>     tsStarted          ← getCurrentTime
>
>     qualified          ← qualifyKinds songs
>     putStrLn $ unwords ["qualified", show qualified]
>
>     -- represent all input SoundFont files in ordered list, thence a vector
>     fps                ← FP.getDirectoryFiles "." (singleton "*.sf2")
>     sffilesp           ← CM.zipWithM openSoundFontFile [0..] fps
>
>     let boundsF::(Word, Word)
>                        = (0, fromIntegral (length sffilesp - 1))
>     let preRoster      = seedRoster (listArray boundsF sffilesp) qualified
>
>     tsLoaded           ← getCurrentTime
>     putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>     -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>     (tsRecond, sfrost) ← finishRoster (tsLoaded, preRoster)
>
>     -- readying instrument maps to be accessed from song renderer
>     traceIO             "prepareInstruments"
>     imap               ← prepareInstruments sfrost
>
>     tsPrepared         ← getCurrentTime
>     putStrLn ("___prepare instruments: " ++ show (diffUTCTime tsPrepared tsRecond))
>
>     -- here's the heart of the coconut
>     mapM_ (uncurry (renderSong sfrost imap)) songs
>
>     tsRendered         ← getCurrentTime
>     putStrLn ("___render songs: "        ++ show (diffUTCTime tsRendered tsPrepared))
>     putStrLn ("___overall: "             ++ show (diffUTCTime tsRendered tsStarted))
>
>     where
>       -- track the complete populations of: samples, instruments, percussion
>       finishRoster     :: (UTCTime, SFRoster) → IO (UTCTime, SFRoster)
>       finishRoster (tsLoaded, preRoster@SFRoster{ .. })
>                                          = do
>         presks         ← formMasterSampleList zFiles
>         pergmsI_       ← formMasterInstList   zFiles
>
>         preSampleCache ← formPreSampleCache zFiles zQualified presks
>         preInstCache   ← formPreInstCache   zFiles zQualified pergmsI_
>
>         (pergmsI, pergmsP, skMap, zc)
>                        ← formZoneCache zFiles preSampleCache preInstCache zQualified pergmsI_
>
>         CM.when diagnosticsEnabled
>           (do
>             print "pergmsI"
>             print pergmsI
>             print "pergmsP"
>             print pergmsP)
>
>         tsZoned        ← getCurrentTime
>         putStrLn ("___cache zones: " ++ show (diffUTCTime tsZoned tsLoaded))
>
>         -- actually conduct the tournament
>         ((wI, sI), (wP, sP))
>                        ← decideWinners zFiles preInstCache preSampleCache zc skMap zQualified pergmsI pergmsP
>         tsDecided      ← getCurrentTime
>         putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsZoned))
>
>         CM.when reportTourney (writeTournamentReport zFiles wI wP)
>         tsDecided'     ← getCurrentTime
>
>         let ws@WinningRecord{ .. }
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
>                                , zWinningRecord    = ws
>                                , zPlayCache        = Map.union playCacheI playCacheP}
>
>         tsRecond   ← getCurrentTime
>         putStrLn ("___create play cache: " ++ show (diffUTCTime tsRecond tsDecided'))
>         
>         return (tsRecond, sfrost)
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
>   let legend :: [Emission] =
>           emitComment [Unblocked "legend = [hints, stereo, 24-bit, many zones, conformant, fuzzy]"]
>        ++ emitNextComment [ToFieldL "weights = " 10, Unblocked (show ssWeights)] 
>   let esFiles          = emitFileListC
>   let esI              = concatMap dumpContestants (Map.toList pContI)
>   let esP              = concatMap dumpContestants (Map.toList pContP)
>   let esQ              = [ Unblocked "\n\n"
>                          , emitShowL defS 1000
>                          , Unblocked "\n\n"
>                          , emitShowL defT 1000
>                          , Unblocked "\n\n"
>                          , emitShowL defM 1000]
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
> decideWinners sffiles pic psc zc skMap qs pergmsI pergmsP
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
>     wiFolder wIn pergmI
>       | traceNot trace_WI False          = undefined
>       | otherwise                        = xaEnterTournament iMatches pergmI kind [] wIn
>       where
>         -- access potentially massive amount of processed information regarding instrument
>         PreInstrument{ .. }              = fromJust $ Map.lookup pergmI pic
>         prom           :: [(InstrumentName, (String, Fuzz))]
>         prom                             = getProMatches iMatches
>         -- what Instrument is closest fit, name-wise, for this artifact
>         mk             :: Maybe (InstrumentName, (String, Double))
>                                          = bestQualifying prom stands
>         kind                             = fst (professIsJust mk (unwords ["bestQualifying returned Nothing"]))
>
>         trace_WI                         = unwords ["wiFolder", show pergmI]
>     
>     wpFolder           :: (Map PercussionSound [PerGMScored], [String])
>                           → PerGMKey
>                           → (Map PercussionSound [PerGMScored], [String])
>     wpFolder wIn pergmP@PerGMKey{ .. }
>       | traceNot trace_WP False          = undefined
>       | otherwise                        = xaEnterTournament sffm pergmP kind [] wIn
>       where
>         mkind          :: Maybe PercussionSound
>                                          =
>           pgkwBag >>= lookupZone >>= getAP >>= pitchToPerc
>         kind                             = professIsJust mkind $ unwords["pitchToPerc returned Nothing"]
>
>         mffm           :: Maybe FFMatches
>         sffm           :: FFMatches
>         mffm                             =
>           pgkwBag >>= lookupZone
>                   >>= zSampleIndex
>                   >>= Just . PreSampleKey pgkwFile
>                   >>= flip Map.lookup psc
>                   >>= Just . sMatches
>         sffm                             = professIsJust mffm $ unwords ["couldn't get PreSample?"]
>
>         PerInstrument{ .. }              = fromJust $ Map.lookup (pergmP{pgkwBag = Nothing}) zc
>
>         lookupZone     :: Word → Maybe SFZone
>         lookupZone bagI                  = lookup bagI (map (BF.first zhwBag) pZonePairs)
>
>         getAP          :: SFZone → Maybe AbsPitch
>         getAP zone@SFZone{ .. }          = (Just . fst) =<< zKeyRange
>
>         trace_WP                         = unwords ["wpFolder", show pergmP]

tournament among GM instruments and percussion from SoundFont files ===================================================

>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           FFMatches
>                           → PerGMKey
>                           → a
>                           → [SSHint]
>                           → (Map a [PerGMScored], [String])
>                           → (Map a [PerGMScored], [String])
>     xaEnterTournament ffs pergm@PerGMKey{ .. } kind hints (wix, ss)
>       | traceIf trace_XAET False         = undefined
>       | otherwise                        = (Map.insert kind now wix, ss)
>       where
>         pergm_                           = pergm{pgkwBag = Nothing}
>         PreInstrument{ .. }              = fromJust $ Map.lookup pergm_ pic
>         PerInstrument{ .. }              = fromJust $ Map.lookup pergm_ zc
>
>         soFar          :: [PerGMScored]  = fromMaybe [] (Map.lookup kind wix)
>         now                              = scoredP : soFar
>         akResult                         = evalAgainstKind kind ffs
>
>         pk                               = fromJust $ Map.lookup pergm skMap
>         ps                               = fromJust $ Map.lookup pk psc
>         mnameZ         :: Maybe String   = pgkwBag >>= \w → Just (sName ps)
>
>         scoredP        :: PerGMScored    =
>           PerGMScored
>             (computeGrade pZonePairs akResult)
>             (toKind kind)
>             akResult pergm iName mnameZ 
>
>         trace_XAET                       = unwords ["xaEnterTournament", show kind, show mnameZ]
>
>         computeGrade   :: [(ZoneHeader, SFZone)] → AgainstKindResult → ArtifactGrade
>         computeGrade zps againstKindResult
>           | traceNot trace_CG False      = undefined
>           | otherwise                    = ArtifactGrade (sum weightedScores) baseScores
>           where
>             zs                           = tail zps
>             zhs                          = map fst zs
>             empiricals :: [Int]          = [   scoreBool $ isStereoInst zhs
>                                              , scoreBool $ is24BitInst zhs
>                                              , round $ computeSplitCharacteristic kind qs zs
>                                              , scoreBool $ all zoneConforms zs
>                                              , round fuzz]
>             howgood                      = againstKindResult - stands
>             fuzz       :: Double
>               | howgood > 0.000_001      = max 0 (logBase 2 howgood) * fuzzFactor kind
>               | otherwise                = 0
>             ss         :: [Int]  = zipWith (*) qqDesires' empiricals
>             baseScores :: [Int]  = [  foldHints hints
>                                                  , head ss
>                                                  , ss !! 1
>                                                  , ss !! 2
>                                                  , ss !! 3
>                                                  , ss !! 4]
>             weightedScores
>                        :: [Int]          = zipWith (*) baseScores ssWeights
>
>             trace_CG                             =
>               unwords [   "computeGrade " , show iName
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
>                           → ([InstrumentName], [PercussionSound])
>                           → [PreSampleKey]
>                           → IO PreSampleCache
> formPreSampleCache sffiles qualified presks
>                                          = return $ foldl' smFolder Map.empty presks
>   where
>     smFolder accum presk@PreSampleKey{ .. } =
>       let 
>         sf@SFFile{.. }                   = sffiles ! pskwFile
>         SoundFontArrays{ .. }            = zArrays
>       in
>         Map.insert presk (computePreSample sf qualified (ssShdrs ! pskwSample) presk) accum
>
> formPreInstCache       :: Array Word SFFile → ([InstrumentName], [PercussionSound]) → [PerGMKey] → IO PreInstCache
> formPreInstCache sffiles qualified pergms
>                                          = return $ foldl' imFolder Map.empty pergms
>   where
>     imFolder im pergm@PerGMKey{ .. }     =
>       Map.insert pergm (computePreInstrument (sffiles ! pgkwFile) qualified pergm) im
>
> getPreSampleFromCache :: Map PreSampleKey PreSample → PreSampleKey → PreSample
> getPreSampleFromCache preSampleCache presk
>                                          = fromMaybe (error $ "PreSample missing from cache: " ++ show presk)
>                                                      (Map.lookup presk preSampleCache)
>
> getPreInstrumentFromCache ic pergm       = fromMaybe (error $ "PreInstrument missing from cache: " ++ show pergm)
>                                                      (Map.lookup pergm ic)
>
> computePreSample       :: SFFile → ([InstrumentName], [PercussionSound]) → F.Shdr → PreSampleKey → PreSample
> computePreSample SFFile{ .. } qualified shdr k
>   | traceNot trace_CPS False             = undefined
>   | otherwise                            = PreSample inp ffs
>   where
>     SoundFontArrays{ .. }                = zArrays
>     ap                                   = fromIntegral $ F.originalPitch shdr
>     apLow                                = fromIntegral $ min 127 (ap - 6)
>     apHigh                               = fromIntegral $ max 0   (ap + 4)
>
>     inp                                  = F.sampleName shdr
>     ffs                                  = computeFFMatches inp qualified
>
>     trace_CPS                            = unwords ["computePreSample", show (inp, k)]
>
> computePreInstrument       :: SFFile → ([InstrumentName], [PercussionSound]) → PerGMKey → PreInstrument
> computePreInstrument SFFile{ .. } qualified PerGMKey{ .. }
>                                          = PreInstrument instName (computeFFMatches instName qualified)
>   where
>     SoundFontArrays{ .. }                = zArrays
>     F.Inst{ .. }                         = ssInsts ! pgkwInst
>
> formMasterInstList     :: Array Word SFFile → IO [PerGMKey]
> formMasterInstList sffiles               = return $ concatMap formFI sffiles
>
> -- file to instrument
> formFI                 :: SFFile → [PerGMKey]
> formFI SFFile{ .. }                      = mapMaybe qualifyInst rangeI
>   where
>     SoundFontArrays{ .. }                = zArrays
>     boundsI                              = bounds ssInsts
>     rangeI                               = profess
>                                              (uncurry (<=) boundsI)
>                                              "bad file: invalid bounds"
>                                              [fst boundsI..snd boundsI-1]
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
>     ++ [emitShowL name 20, emitShowL (length is) 4]
>     ++ [Unblocked " /- instruments, percussion -/ ", emitShowL (length ps) 4, EndOfLine]
>
> computeSplitCharacteristic  :: ∀ a. (SFScorable a) ⇒
>                                a
>                                → ([InstrumentName], [PercussionSound])
>                                → [(ZoneHeader, SFZone)]
>                                → Double
> computeSplitCharacteristic kind qs zs = log (3 * splitScore kind qs zs * factor)
>   where
>     factor             :: Double         = if isStereoInst (map fst zs)
>                                              then 1/2
>                                              else 1
>
> isStereoInst, is24BitInst
>                        :: [ZoneHeader] → Bool
>
> isStereoInst zhs                         = isJust $ find isStereoZone zhs
>       
> isStereoZone ZoneHeader{ .. }            = stype == SampleTypeRight || stype == SampleTypeLeft
>   where
>     stype = toSampleType $ F.sampleType zhShdr 
>         
> zoneConforms (ZoneHeader{ .. }, SFZone{ .. })
>                                          = not $ or unsupported
>   where
>     F.Shdr{ .. }                           = zhShdr
>     unsupported        :: [Bool]
>     unsupported                          =
>       [
>           A.PressLoop == fromMaybe A.NoLoop zSampleMode
>         , 0 /= fromMaybe 0 zInitQ
>         , 0 /= fromMaybe 0 zScaleTuning
>         , 0 /= fromMaybe 0 zExclusiveClass
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

> formZoneCache          :: Array Word SFFile
>                           → PreSampleCache
>                           → PreInstCache
>                           → ([InstrumentName], [PercussionSound])
>                           → [PerGMKey]
>                           → IO ([PerGMKey], [PerGMKey], Map PerGMKey PreSampleKey, ZoneCache)
> formZoneCache sffiles preSampleCache preInstCache roster pergms
>                                          = 
>   return (pergmsI, pergmsP, skMap, zc)
>   where
>     pergmsI, filteredIP, pergmsP
>                        :: [PerGMKey]
>     skMap              :: Map PerGMKey PreSampleKey
>     zc                 :: ZoneCache
>     zq                 :: Map PerGMKey [Word]
>
>     (pergmsI, filteredIP, zq)            = foldr winnow ([], [], Map.empty) pergms
>     zc                                   =
>       foldr (\p → Map.insert p (computePerInst p)) Map.empty (pergmsI ++ filteredIP)
>     (pergmsP, skMap)                     = formMasterPercussionList zc preSampleCache filteredIP
>
>     winnow             :: PerGMKey
>                           → ([PerGMKey], [PerGMKey], Map PerGMKey [Word])
>                           → ([PerGMKey], [PerGMKey], Map PerGMKey [Word])
>     winnow pergm@PerGMKey{ .. } (is, pis, zq')
>       | traceNot trace_W False           = undefined
>       | otherwise                        = (is', pis', zq'')
>       where
>         (cat, ws)                        = categorizeInst pergm
>         is'                              = if InstCatInst == cat then is ++ [pergm] else is
>         pis'                             = if InstCatPerc == cat then pis ++ [pergm] else pis
>         zq''                             = if InstCatPerc == cat
>                                              then Map.insert pergm ws zq'
>                                              else zq'
>         
>         trace_W                          = unwords ["winnow", show pergm]
>
>     computePerInst     :: PerGMKey → PerInstrument
>     computePerInst pergm@PerGMKey{ .. }
>       | traceIf trace_CPI False          = undefined
>       | otherwise                        = PerInstrument iinst (gList ++ oList) 
>       where
>         trace_CPI                        = unwords ["computePerInst", iName]
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
>         oIx                              = fromMaybe oIx_ (Map.lookup pergm zq)
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
>             zh                           = ZoneHeader bagIndex (ssShdrs ! si) Nothing
>
>             trace_BZ                     = unwords ["buildZone", show bagIndex]
>
>     categorizeInst :: PerGMKey → (InstCat, [Word])
>     categorizeInst pergm@PerGMKey{ .. }
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = (icat', ws')
>       where
>         SoundFontArrays{ .. }            = zArrays (sffiles ! pgkwFile)
>         PreInstrument{ .. }              = fromJust $ Map.lookup pergm preInstCache
>
>         iinst                            = ssInsts ! pgkwInst
>         jinst                            = ssInsts ! (pgkwInst+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
>         zs             :: [(ZoneHeader, ZoneDigest)]
>         zs                               = map inspectZone (tail (deriveRange ibagi jbagi))
>
>         zhs            :: [ZoneHeader]   = map fst zs
>         zds            :: [ZoneDigest]   = map snd zs
>
>         icat                             = fromMaybe InstCatDisq latched
>         ws                               = if InstCatPerc == icat
>                                              then wZones
>                                              else []
>         (icat', ws')                     = if InstCatPerc == icat && null ws
>                                              then (InstCatDisq, [])
>                                              else (icat, ws)
>         sIn, sOut      :: IntSet
>         sIn                              = IntSet.fromList $ mapMaybe (uncurry indices) zs
>         sOut                             = IntSet.fromList $ mapMaybe (uncurry links) zs
>
>         badrom         :: Maybe InstCat  =
>           if any (uncurry hasRom) zs then Just InstCatDisq else Nothing
>         badLinks       :: Maybe InstCat  =
>           if IntSet.isSubsetOf sOut sIn then Nothing else Just InstCatDisq
>         hasRom ZoneHeader{ .. } _        = F.sampleType zhShdr >= 0x8000
>                                          
>         indices, links :: ZoneHeader → ZoneDigest → Maybe Int
>         indices zh ZoneDigest{ .. }      =
>           if isStereoZone zh then Just $ fromIntegral $ professIsJust zdSampleIndex "no sample index?!" else Nothing
>         links zh@ZoneHeader{ .. } _      =
>           if isStereoZone zh then Just $ fromIntegral $ F.sampleLink zhShdr else Nothing
>
>         xinst x                          = Just InstCatInst
>         xperc x                          = Just InstCatPerc
>         xdisq x                          = Just InstCatDisq
>
>         alts       :: [Maybe InstCat]
>         alts                             =
>           [ badrom
>           , badLinks
>           , xinst =<< bestQualifying (instAs iMatches) isConfirmed
>           , xperc =<< bestQualifying (percAs iMatches) isConfirmed
>           , if 0.8 < howPercish then Just InstCatPerc else Nothing
>           , xdisq =<< bestQualifying (instUs iMatches) stands
>           , if 0.4 < howPercish then Just InstCatPerc else Nothing
>           , xinst =<< bestQualifying (instAs iMatches) stands
>           , xperc =<< bestQualifying (percAs iMatches) stands]
>
>         wZones     :: [Word]             = mapMaybe (uncurry computeCanBePerc) zs
>         howPercish :: Double             = fromIntegral (length wZones) / fromIntegral (length zs)

   "now", run the alternatives to categorize as follows
   a. Just InstCatInst           an inst bearing one inst, or
   b. Just InstCatPerc           an inst bearing one or more percs, or
   c. Just InstCatDisq           an inst disqualified from all tournaments, or else
   d. Nothing                    undecided

>         latched    :: Maybe InstCat      = foldr CM.mplus Nothing alts
>
>         trace_CI                         =
>           unwords ["categorizeInst", iName, show wZones]
>
>         computeCanBePerc
>                        :: ZoneHeader → ZoneDigest → Maybe Word
>         computeCanBePerc ZoneHeader{ .. } ZoneDigest { .. }
>           | traceNot trace_CCBP False    = undefined
>           | otherwise                    =
>           mwOut =<< integralize zdKeyRange
>           where
>             mwOut range                  = if pinnedKR pss range then Just zhwBag else Nothing
>             pss        :: [PercussionSound]
>             pss                          = select roster
>
>             trace_CCBP                   =
>               unwords ["computeCanBePerc", show pss]
>
>         inspectZone    :: Word → (ZoneHeader, ZoneDigest)
>         inspectZone bagIndex
>           | traceNot trace_IZ False      = undefined
>           | otherwise                    = (zh, zd)
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
>             si                           =
>               professIsJust (zdSampleIndex zd) (unwords ["sample index"])
>             zh                           =
>               ZoneHeader bagIndex (ssShdrs ! si) Nothing
>
>             trace_IZ                     =
>               unwords ["inspectZone", show gens]
>
>         inspectGen     :: F.Generator → ZoneDigest → ZoneDigest 
>         inspectGen (F.KeyRange i j) zd   = zd {zdKeyRange = Just (i, j)}
>         inspectGen (F.SampleIndex w) zd  = zd {zdSampleIndex = Just w}
>         inspectGen _ zd                  = zd
>
> pinnedKR               :: [PercussionSound] → (AbsPitch, AbsPitch) → Bool
> pinnedKR pss (p1, p2)                    = any available cands
>   where
>     available          :: AbsPitch → Bool
>     available ap                         = maybe False (`elem` pss) (pitchToPerc ap)
>
>     cands              :: [AbsPitch]
>     cands
>       | p1 == p2                         = singleton p1
>       | 1 == abs (p2 - p1) && matchedPair (p1, p2)
>                                          = [p1, p2]
>       | otherwise                        = []
>       
>     matchedPair        :: (AbsPitch, AbsPitch) → Bool
>     matchedPair p0                       = any matching getPairs
>
>     matching           :: PercussionPair → Bool
>     matching ps                          =
>       let
>         p3             :: AbsPitch       = 35 + fromEnum (head ps)
>         p4             :: AbsPitch       = 35 + fromEnum (last ps)
>       in
>         min p1 p2 == min p3 p4
>
> getPerInstrumentFromCache
>                        :: ZoneCache → PerGMKey → PerInstrument
> getPerInstrumentFromCache zc pergm       = zc Map.! pergm

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
>   | otherwise                            = eutSynthesize (reconL, reconR) rSampleRate
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
>     (reconX, mreconX)                    =
>       if usingPlayCache
>         then fromMaybe
>                (error $ unwords ["Note missing from play cache:", show noon])
>                (Map.lookup (PlayKey pergm noon) zPlayCache)
>         else computePlayValue pergm{pgkwBag = Nothing} zZoneCache noon
>     (reconL@Recon{ .. }, reconR)        = (reconX, fromMaybe reconX mreconX)
>
> computePlayValue       :: PerGMKey → ZoneCache → NoteOn → PlayValue
> computePlayValue pergm@PerGMKey{ .. } zc noon@NoteOn{ .. }
>   | traceNot trace_CPV False             = undefined
>   | otherwise                            =
>   let
>     ((zoneL, shdrL), (zoneR, shdrR))
>                        :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                                          = setZone pergm zc noon
>     (reconL, reconR)   :: (Recon, Recon)
>                                          = reconLR ((zoneL, shdrL), (zoneR, shdrR)) noon
>   in
>     (reconL, if reconR == reconL
>                then Nothing
>                else Just reconR)
>   where
>     trace_CPV                            = unwords ["computePlayValue", show pergm]

zone selection for rendering ==========================================================================================

> setZone                :: PerGMKey
>                           → ZoneCache
>                           → NoteOn
>                           → ((SFZone, F.Shdr), (SFZone, F.Shdr))
> setZone pergm@PerGMKey{ .. } zc noon
>                                          = ((snd zoneL, sampleL) ,(snd zoneR, sampleR))
>   where
>     PerInstrument{ .. }                  = fromJust $ Map.lookup pergm zc
>     zs                                   = tail pZonePairs
>     (zoneL, zoneR)                       = selectZonePair zs (selectBestZone zs noon)
>     (sampleL, sampleR)                   = ((zhShdr . fst) zoneL, (zhShdr . fst) zoneR)
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
>
>     trace_SBZ                            =
>       unwords ["selectBestZone", show (F.sampleName (zhShdr ((fst . snd) whichZ)))]
>
> selectZonePair         :: [(ZoneHeader, SFZone)]
>                           → (ZoneHeader, SFZone)
>                           → ((ZoneHeader, SFZone), (ZoneHeader, SFZone))
> selectZonePair zs zone
>    | stype == SampleTypeLeft             = (zone, ozone)
>    | stype == SampleTypeRight            = (ozone, zone)
>    | otherwise                           = (zone, zone)
>    where
>      F.Shdr{ .. }                        = (zhShdr . fst) zone
>      stype                               = toSampleType sampleType
>      ozone                               = fromMaybe zone $ find (withSlink sampleLink) zs
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
>     if DAllOn /= qqDesireReStereo defT || isStereoZone zh
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
> instrumentSplitScore   :: InstrumentName → ([InstrumentName], [PercussionSound]) → [(ZoneHeader, SFZone)] → Double
> instrumentSplitScore kind _ zs           = fromIntegral (length zs)
>
> percussionSplitScore   :: PercussionSound → ([InstrumentName], [PercussionSound]) → [(ZoneHeader, SFZone)] → Double
> percussionSplitScore kind qs zs          = max 1 (fromIntegral $ length $ mapMaybe (match . snd) zs)
>   where
>     pss                :: [PercussionSound]
>     pss                                  = select qs
>
>     match              :: SFZone → Maybe PercussionSound
>     match SFZone{zKeyRange}
>       | isNothing mkind                  = Nothing
>       | kind == fromJust mkind           = Just kind
>       | otherwise                        = Nothing
>       where
>         mkind                            = qualifyKeyRange pss =<< zKeyRange
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

> reconLR                :: ((SFZone, F.Shdr), (SFZone, F.Shdr)) → NoteOn → (Recon, Recon)
> reconLR ((zoneL, shdrL), (zoneR, shdrR)) noon
>   | traceNever trace_RLR False           = undefined
>   | otherwise                            = (recL, recR')
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
> reconModulation SFZone{ .. } F.Shdr{ .. } noon
>                                          = resolveMods m8n zModulators defaultMods
>   where
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
>     nVibLfo            :: Maybe LFO      = deriveLFO zDelayVibLfo zFreqVibLfo zVibLfoToPitch Nothing Nothing
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
> createPlayCache sffiles zc preSampleCache preInstCache _ ws
>                                          =
>   return
>     $ if usingPlayCache
>       then Map.foldrWithKey precalcFolder Map.empty ws
>       else Map.empty
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
> emitMsgs               :: InstrumentName → [(InstrumentName, [String])] → [Emission]
> emitMsgs kind msgs                       = concatMap (\s → [Unblocked s, EndOfLine]) imsgs
>   where
>     imsgs              :: [String]       = fromMaybe [] (lookup kind msgs)
>
> qualifyKeyRange        :: [PercussionSound] → (AbsPitch, AbsPitch) → Maybe PercussionSound
> qualifyKeyRange pss (p1, p2)
>   | pinnedKR pss (p1, p2)
>     && ap >= fromEnum AcousticBassDrum
>     && ap <= fromEnum OpenTriangle       = Just (toEnum ap)
>   | otherwise                            = Nothing
>   where
>     ap = p1 - 35    
>
> dumpContestants        :: ∀ a. (Ord a, Show a, SFScorable a) ⇒ (a, [PerGMScored]) → [Emission]
> dumpContestants (kind, contestants)      = prolog ++ es ++ epilog
>   where
>     prolog, es, epilog :: [Emission]
>
>     prolog = emitLine [emitShowL kind 50]
>     es =  concatMap dumpContestant contestants
>     epilog = emitLine []
>
> dumpContestant         :: PerGMScored → [Emission]
> dumpContestant PerGMScored{ .. }         = es
>   where
>     es = emitLine [ Blanks 8, emitShowL (pgkwFile pPerGMKey) 4, emitShowR szI 22
>                   , Blanks 8, emitShowR (fromMaybe "" mszP) 22
>                   , Blanks 8, emitShowL (pScore pArtifactGrade) 8
>                   , emitShowL (pEmpiricals pArtifactGrade) 16
>                   , emitShowR pAgainstKindResult 20]

Scoring stuff =========================================================================================================

> data SSHint =
>   DLow
>   | DMed
>   | DHigh
>   | DScore Double deriving Show
>
> scoreHint              :: SSHint → Int
> scoreHint h                              = case h of
>                                            DLow            → -1
>                                            DMed            → 0
>                                            DHigh           → 1
>                                            DScore x        → 0 
>             
> foldHints              :: [SSHint] → Int
> foldHints                                = foldr foldHint 0
>
> foldHint               :: SSHint → Int → Int
> foldHint hint accum                      = accum + scoreHint hint   
>
> ssWeights              :: [Int]          = [ weighHints
>                                            , weighStereo
>                                            , weigh24Bit
>                                            , weighSplits
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
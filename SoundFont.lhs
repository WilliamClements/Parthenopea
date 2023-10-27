> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE UnicodeSyntax #-}

SoundFont support =====================================================================================================

> module SoundFont where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed ( listArray, Array, (!), bounds )
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Foldable ( toList )
> import Data.Int ( Int8, Int16 )
> import Data.List ( find, foldr, minimumBy, singleton, foldl', sortOn )
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, mapMaybe )
> import Data.Ord ( Down(Down), comparing )
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import Debug.Trace ( trace, traceIO )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, Mono, Stereo, Clock, Signal )
> import Euterpea.Music
> import GHC.Base ( (<|>) )
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
>     sWordF             :: Word
>   , sWordS             :: Word} deriving (Eq, Ord, Show)
>
> data PreSample =
>   PreSample {
>     sName              :: String
>   , psBmas             :: BothMatchingAs
>   , dLow               :: Double
>   , dTarget            :: Double
>   , dHigh              :: Double} deriving Show
>
> data PreInstrument =
>   PreInstrument {
>     iName              :: String
>   , isBmas             :: BothMatchingAs} deriving Show
>
> data PerGMKey =
>   PerGMKey {
>     pWordF             :: Word
>   , pWordI             :: Word
>   , mpWordZ            :: Maybe Word} deriving (Eq, Ord, Show)
>
> data ArtifactGrade =
>   ArtifactGrade {
>     pScore             :: Int
>   , pEmpiricals        :: [Int]} deriving (Show)
>
> type Kind                                = Either InstrumentName PercussionSound
> type AgainstKindResult                   = Double
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
>     pWinningI          :: Map.Map InstrumentName PerGMScored
>   , pWinningP          :: Map.Map PercussionSound PerGMScored
>   , pWarnings          :: [String]} deriving Show
>
> seedWinningRecord                        = WinningRecord Map.empty Map.empty []
>
> data ZoneHeader =
>   ZoneHeader {
>     pwZone             :: Word
>   , pSample            :: F.Shdr
>   , pbIsNonPitched     :: Bool} deriving Show
>
> data PerInstrument =
>   PerInstrument {
>     pInst              :: F.Inst
>   , pInstCat           :: InstCat
>   , pZonePairs         :: [(ZoneHeader, SFZone)]}
>
> data InstCat =
>        InstCatInst
>      | InstCatPerc
>      | InstCatDisq deriving (Show,Eq,Ord,Enum)
>     
> class GMPlayable a ⇒ SFScorable a where
>   splitCount           :: a → [(ZoneHeader, SFZone)] → Double
>   fuzzFactor           :: a → Double
>   toKind               :: a → Kind
>
> instance SFScorable InstrumentName where
>   splitCount                              = instrumentSplitCount
>   fuzzFactor _                            = 7/8
>   toKind                                  = Left
>
> instance SFScorable PercussionSound where
>   splitCount                              = percussionSplitCount
>   fuzzFactor _                            = 3/4
>   toKind                                  = Right
>
> type PreSampleCache                       = Map.Map PreSampleKey PreSample
> type PreInstCache                         = Map.Map PerGMKey     PreInstrument
> type ZoneCache                            = Map.Map PerGMKey     PerInstrument
>
> data SFRoster =
>   SFRoster {
>     zFiles             :: Array Word SFFile
>   , zPreSampleCache    :: PreSampleCache
>   , zPreInstCache      :: PreInstCache
>   , zZoneCache         :: ZoneCache
>   , zWinningRecord     :: WinningRecord
>   , zPlayCache         :: Map.Map PlayKey (Reconciled, Maybe Reconciled)}
>
> seedSFRoster vFile                       = SFRoster vFile Map.empty Map.empty Map.empty seedWinningRecord Map.empty
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
>   , zChorus            :: Maybe Int -- designated M
>   , zReverb            :: Maybe Int -- designated VH
>   , zPan               :: Maybe Int -- designated VH
>
>   , zRootKey           :: Maybe Word
>
>   , zModLfoToPitch     :: Maybe Int
>   , zVibLfoToPitch     :: Maybe Int
>   , zModEnvToPitch     :: Maybe Int
>   , zInitFc            :: Maybe Int
>   , zInitQ             :: Maybe Int -- designated L
>   , zModLfoToFc        :: Maybe Int
>   , zModEnvToFc        :: Maybe Int -- done
>   , zModLfoToVol       :: Maybe Int
>   , zDelayModLfo       :: Maybe Int
>   , zFreqModLfo        :: Maybe Int -- designated M
>   , zDelayVibLfo       :: Maybe Int
>   , zFreqVibLfo        :: Maybe Int -- designated M
>   , zDelayModEnv       :: Maybe Int
>   , zAttackModEnv      :: Maybe Int -- designated M
>   , zHoldModEnv        :: Maybe Int
>   , zDecayModEnv       :: Maybe Int
>   , zSustainModEnv     :: Maybe Int -- designated M
>   , zReleaseModEnv     :: Maybe Int -- designated L
>   , zKeyToModEnvHold   :: Maybe Int
>   , zKeyToModEnvDecay  :: Maybe Int
>   , zKeyToVolEnvHold   :: Maybe Int
>   , zKeyToVolEnvDecay  :: Maybe Int
>
>   , zModulators        :: Maybe (Word, Word)} deriving Show
>
> defInstrumentZone      :: SFZone
> defInstrumentZone                        = SFZone Nothing Nothing Nothing Nothing
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
>                                            Nothing
>
> data PlayKey =
>   PlayKey {
>     xItem              :: PerGMKey
>   , xPitch             :: AbsPitch
>   , xVolume            :: Volume} deriving (Eq, Ord, Show)
>
> type PlayValue                           = (Reconciled, Maybe Reconciled)

sample pitching scaffold ==============================================================================================

> pitchSamples           :: Int → IO ()
> pitchSamples _ = do
>     putStrLn "pitch..."
>     ts1                ← getCurrentTime
>
>     fps                ← FP.getDirectoryFiles "." (singleton "*.sf2")
>     sffilesp           ← CM.zipWithM openSoundFontFile [0..] fps
>
>     ts2                ← getCurrentTime
>     putStrLn ("___load files: " ++ show (diffUTCTime ts2 ts1))
>
>     mapM_ pitchFile sffilesp
>
>     ts3                ← getCurrentTime
>     putStrLn ("___pitch files: " ++ show (diffUTCTime ts3 ts2))
>
> pitchFile           :: SFFile → IO ()
> pitchFile sffile = do
>     let hdrs                             = ssShdrs $ zArrays sffile
>     
>     let st          :: Word              = (fst . bounds) hdrs
>     let en          :: Word              = (snd . bounds) hdrs
>
>     mapM_ (pitchOne sffile) [st..en - 1]
>   
> pitchOne           :: SFFile → Word → IO ()
> pitchOne sffile ix
>   | traceIf msg False                    = undefined
>   | otherwise = do
>
>     let arrays                           = zArrays sffile
>     let shdr                             = ssShdrs arrays ! ix
>
>     print $ F.sampleName shdr
>
>     let vFft                             = eutAnalyzeSample (ssData arrays)
>                                                             (ssM24 arrays)
>                                                             shdr
>                                                             (fromIntegral $ F.originalPitch shdr)
>     if null vFft
>       then putStrLn " ...\n"
>       else
>         do
>           putStrLn (" vSum = "   ++ show (sumUpFft vFft) ++ "\n")
>   where
>     msg = unwords ["pitchOne ", show ix]
>   

executive =============================================================================================================

> doEverything           :: [(String, Music (Pitch, [NoteAttribute]))] → IO ()
> doEverything songs = do
>
>     putStrLn "everything..."
>
>     tsStarted          ← getCurrentTime
>
>     -- represent all input SoundFont files in ordered list, thence a vector
>     fps                ← FP.getDirectoryFiles "." (singleton "*.sf2")
>     sffilesp           ← CM.zipWithM openSoundFontFile [0..] fps
>
>     let boundsF::(Word, Word) = (0, fromIntegral (length sffilesp - 1))
>     let preRoster = seedSFRoster (listArray boundsF sffilesp)
>
>     tsLoaded           ← getCurrentTime
>     putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>     -- track the complete populations of: samples, instruments, percussion
>     presks             ← formMasterSampleList preRoster
>     pergmsI            ← formMasterInstList   preRoster
>
>     let preRoster'  = preRoster  {  zPreSampleCache = formPreSampleCache preRoster presks
>                                   , zPreInstCache   = formPreInstCache   preRoster pergmsI}
>     let preRoster'' = preRoster' {  zZoneCache = formZoneCache preRoster' pergmsI}
>
>     tsZoned            ← getCurrentTime
>     putStrLn ("___cache zones: " ++ show (diffUTCTime tsZoned tsLoaded))
>
>     -- filter master lists down to appropriate candidates
>     let filteredI =  filter (flavor InstCatInst (zZoneCache preRoster'')) pergmsI
>     let filteredIP = filter (flavor InstCatPerc (zZoneCache preRoster'')) pergmsI
>     pergmsP            ← formMasterPercussionList preRoster'' filteredIP
>     let pergmsI' = filteredI
>     let pergmsP' = pergmsP
>
>     -- actually conduct the tournament
>     ws                 ← decideWinners preRoster'' pergmsI' pergmsP'
>
>     -- print song/orchestration info to user (can be stored by redirecting standard out)
>     mapM_ putStrLn $ pWarnings ws
>
>     tsDecided          ← getCurrentTime
>     putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsZoned))
>
>     let playCacheI = createPlayCache preRoster'' (pWinningI ws)
>     let playCacheP = createPlayCache preRoster'' (pWinningP ws)
>
>     let sfrost = preRoster''{zWinningRecord = ws, zPlayCache = Map.union playCacheI playCacheP}
>
>     tsReconciled       ← getCurrentTime
>     putStrLn ("___create play cache: " ++ show (diffUTCTime tsReconciled tsDecided))
>
>     -- readying instrument maps to be accessed from song renderer
>     let imap = prepareInstruments sfrost
>
>     tsPrepared         ← getCurrentTime
>     putStrLn ("___prepare instruments: " ++ show (diffUTCTime tsPrepared tsReconciled))
>
>     -- here's the heart of the coconut
>     mapM_ (uncurry (renderSong sfrost imap)) songs
>
>     tsRendered         ← getCurrentTime
>     putStrLn ("___render songs: "        ++ show (diffUTCTime tsRendered tsPrepared))
>     putStrLn ("___overall: "             ++ show (diffUTCTime tsRendered tsStarted))
>
>     where
>       flavor           :: InstCat → ZoneCache → PerGMKey → Bool
>       flavor icat zc pergmI              = icat == pInstCat (getPerInstrumentFromCache zc pergmI)
>
> writeTournamentReport  :: SFRoster
>                           → Map.Map InstrumentName [PerGMScored]
>                           → Map.Map PercussionSound [PerGMScored]
>                           → IO ()
> writeTournamentReport sfrost@SFRoster{zFiles} pContI pContP
>                                          = do
>   tsStarted            ← getCurrentTime
>   -- output all selections to the report file
>   let legend :: [Emission] =
>           emitComment [Unblocked "legend = [hints, stereo, 24-bit, many zones, conformant, fuzzy]"]
>        ++ emitNextComment [ToFieldL "weights = " 10, Unblocked (show ssWeights)] 
>   let esH            = emitFileListC zFiles
>   let esI            = concatMap dumpContestants (Map.toList pContI)
>   let esP            = concatMap dumpContestants (Map.toList pContP)
>   let esQ            = [Unblocked "\n\n", emitShowL defS 1000, Unblocked "\n\n", emitShowL defT 1000]
>   let esTail         = singleton $ Unblocked "\n\nThe End\n\n"
>   let eol            = singleton EndOfLine
>
>   writeFileBySections "TournamentReport'.lhs" [esH, legend, esI, eol, esH, legend, esP, esQ, esTail]

>   tsFinished            ← getCurrentTime
>   putStrLn ("___report tournament results: " ++ show (diffUTCTime tsFinished tsStarted))
>
> decideWinners          :: SFRoster → [PerGMKey] → [PerGMKey] → IO WinningRecord
> decideWinners sfrost@SFRoster{zFiles, zPreSampleCache, zPreInstCache, zZoneCache} pergmsI pergmsP = do
>   let (wI, sI) = foldl' wiFolder (Map.empty, []) pergmsI         
>   let (wP, sP) = foldl' wpFolder (Map.empty, []) pergmsP
>   let wI' = finalize wI
>   let wP' = finalize wP
>
>   CM.unless skipReporting (writeTournamentReport sfrost wI' wP')
>
>   let ws = WinningRecord (Map.map head wI') (Map.map head wP') (sI ++ sP)
>   return ws
>   where
>     wiFolder           :: (Map.Map InstrumentName [PerGMScored], [String])
>                           → PerGMKey
>                           → (Map.Map InstrumentName [PerGMScored], [String])
>     wiFolder wip pergm                   = if null mk then wip else aresult
>       where
>         -- access potentially massive amount of processed information regarding instrument
>         preI@PreInstrument{isBmas}       = getPreInstrumentFromCache zPreInstCache pergm{mpWordZ = Nothing}
>         perI@PerInstrument{pZonePairs}   = getPerInstrumentFromCache zZoneCache    pergm{mpWordZ = Nothing}
>         -- what Instrument is closest fit, name-wise, for this artifact
>         mk             :: Maybe (InstrumentName, (String, Double))
>                                          = bestQualifying (matchingAs isBmas) stands
>         kind                             = (fst . fromJust) mk
>         aresult                          = xaEnterTournament sfrost isBmas pergm kind [] wip
>     
>     wpFolder           :: (Map.Map PercussionSound [PerGMScored], [String])
>                           → PerGMKey
>                           → (Map.Map PercussionSound [PerGMScored], [String])
>     wpFolder wip pergm
>       | traceNever msg' False            = undefined
>       | otherwise                        = if isNothing mkind then wip else aresult
>       where
>         msg'                             = unwords ["wpFolder ", show pergm]
>
>         -- access potentially massive amount of processed information regarding instrument
>         perI@PerInstrument{pZonePairs}   = getPerInstrumentFromCache zZoneCache pergm{mpWordZ = Nothing}
>         preS@PreSample{psBmas}           = getPreSampleFromCache zPreSampleCache pergm
>         mkind          :: Maybe PercussionSound
>                                          = mpWordZ pergm >>= lookupZone pZonePairs >>= getAP >>= pitchToPerc
>         aresult                          = xaEnterTournament sfrost psBmas pergm (fromJust mkind) [] wip
>
>         lookupZone     :: [(ZoneHeader, SFZone)] → Word → Maybe SFZone
>         lookupZone zs wZ                 = lookup wZ (map (BF.first pwZone) zs)
>
>         getAP          :: SFZone → Maybe AbsPitch
>         getAP zone                       = (Just . fst) =<< zKeyRange zone
>     
>     finalize           :: Map.Map a [PerGMScored] → Map.Map a [PerGMScored]
>     finalize                             = Map.map (sortOn (Down . pScore . pArtifactGrade))
>
> formMasterSampleList   :: SFRoster → IO [PreSampleKey]
> formMasterSampleList sfrost@SFRoster{zFiles}
>                                          = do
>   return $ concatMap formFS zFiles
>   where
>     formFS                 :: SFFile → [PreSampleKey]
>     formFS sffile = map (PreSampleKey (zWordF sffile)) [st..en - 1]
>       where
>         hdrs                             = ssShdrs $ zArrays sffile
>         st             :: Word           = (fst . bounds) hdrs
>         en             :: Word           = (snd . bounds) hdrs
>   
> formPreSampleCache     :: SFRoster → [PreSampleKey] → PreSampleCache
> formPreSampleCache sfrost@SFRoster{zFiles}
>                                          = foldl' smFolder Map.empty
>   where
>     smFolder           :: PreSampleCache → PreSampleKey → PreSampleCache
>     smFolder sm k@PreSampleKey{sWordF}   = Map.insert k (computePreSample (zFiles ! sWordF) k) sm
>
> formPreInstCache       :: SFRoster → [PerGMKey] → PreInstCache
> formPreInstCache sfrost@SFRoster{zFiles}
>                                          = foldl' imFolder Map.empty
>   where
>     imFolder           :: PreInstCache → PerGMKey → PreInstCache
>     imFolder im pergm                    = Map.insert pergm (computePreInstrument (zFiles ! pWordF pergm) pergm) im
>
> getPreSampleFromCache sc pergm           = fromMaybe (error $ "PreSample missing from cache: " ++ show pergm)
>                                                      (Map.lookup (toPreSampleKey pergm) sc)
>
> getPreInstrumentFromCache ic pergm       = fromMaybe (error $ "PreInstrument missing from cache: " ++ show pergm)
>                                                      (Map.lookup pergm ic)
>
> toPreSampleKey         :: PerGMKey → PreSampleKey
> toPreSampleKey pergm                     = PreSampleKey (pWordF pergm) (fromJust (mpWordZ pergm))
>
> computePreSample       :: SFFile → PreSampleKey → PreSample
> computePreSample sffile k
>   | traceNever msg False                 = undefined
>   | otherwise                            = PreSample inp bmas dLow dTarget dHigh
>   where
>     arrays                               = zArrays sffile
>     shdr                                 = ssShdrs arrays ! sWordS k
>
>     ap                                   = fromIntegral $ F.originalPitch shdr
>     apLow                                = fromIntegral $ min 127 (ap - 6)
>     apHigh                               = fromIntegral $ max 0   (ap + 4)
>
>     inp                                  = F.sampleName shdr
>     bmas                                 = BothMatchingAs (computeMatchingAs inp)
>                                                           (computeMatchingAs inp)
>     dLow                                 = sumUpFft $ eutAnalyzeSample (ssData arrays) (ssM24 arrays) shdr apLow
>     dTarget                              = sumUpFft $ eutAnalyzeSample (ssData arrays) (ssM24 arrays) shdr ap
>     dHigh                                = sumUpFft $ eutAnalyzeSample (ssData arrays) (ssM24 arrays) shdr apHigh
>
>     msg = unwords ["computePreSample ", show inp, " ", show k]
>
> computePreInstrument       :: SFFile → PerGMKey → PreInstrument
> computePreInstrument sffile@SFFile{zArrays} pergm@PerGMKey{pWordI}
>   | traceIf msg False                    = undefined
>   | otherwise                            = PreInstrument inp bmas
>   where
>     iinst                                = ssInsts zArrays ! pWordI
>
>     inp                                  = F.instName iinst    
>     bmas                                 = BothMatchingAs (computeMatchingAs inp)
>                                                           (computeMatchingAs inp)
> 
>     msg = unwords ["computePreInstrument ", show inp, " ", show pergm, " ", show bmas]
>
> isNonPitchedByFft        :: PreSample → Maybe Bool
> isNonPitchedByFft pres@PreSample{dLow, dTarget, dHigh}
>   | traceIf msg False                    = undefined
>   | 0.0 == dLow + dTarget + dHigh        = Nothing
>   | otherwise                            = Just bCrit
>   where
>     bCrit1                               = dTarget < 3.0
>     bCrit2                               = (dTarget - dLow) / dTarget < 0.1
>     bCrit3                               = (dHigh - dTarget) / dTarget < 0.1
>     bCrit                                = bCrit1 || bCrit2 || bCrit3
>     msg                                  = unwords ["isNonPitchedByFft ",   show (dLow, dTarget, dHigh)
>                                                                    , " ",   show (bCrit1, bCrit2, bCrit3)
>                                                                    , " = ", show bCrit]
>
> formMasterInstList     :: SFRoster → IO [PerGMKey]
> formMasterInstList sfrost@SFRoster{zFiles}
>                                          = do
>   return $ concatMap formFI zFiles
>
> formFI                 :: SFFile → [PerGMKey]
> formFI sffile@SFFile{zArrays, zWordF}    = pergmsI
>   where
>     boundsI                              = bounds (ssInsts zArrays)
>     pergmsI                              = mapMaybe qualifyInst [fst boundsI..snd boundsI-1]
>
>     qualifyInst        :: Word → Maybe PerGMKey
>     qualifyInst wordI
>       | (jbagi - ibagi) >= 2             = Just $ PerGMKey zWordF wordI Nothing
>       | otherwise                        = Nothing
>       where
>         iinst                            = ssInsts zArrays ! wordI
>         jinst                            = ssInsts zArrays ! (wordI+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
> formMasterPercussionList        :: SFRoster → [PerGMKey] → IO [PerGMKey]
> formMasterPercussionList sfrost@SFRoster{zFiles, zZoneCache, zPreSampleCache} pergmsI 
>                                          = do
>   let pergmsP = foldl' ffFolder [] pergmsI
>   return pergmsP
>   where
>     ffFolder           :: [PerGMKey] → PerGMKey → [PerGMKey]
>     ffFolder pergmsP pergmI              =
>       let
>         perI = getPerInstrumentFromCache zZoneCache pergmI{mpWordZ = Nothing}
>       in
>         pergmsP ++ instrumentPercList pergmI perI 
>
> instrumentPercList     :: PerGMKey → PerInstrument → [PerGMKey]
> instrumentPercList pergmI perI           = pergmPs
>   where
>     words                                = map (pwZone . fst) (tail (pZonePairs perI))
>     pergmPs                              = map addmwP words
>     addmwP             :: Word → PerGMKey
>     addmwP wP                            = pergmI {mpWordZ = Just wP}
>
> openSoundFontFile      :: Word → FilePath → IO SFFile
> openSoundFontFile wFile filename = do
>   putStr (show wFile ++ " " ++ filename)
>   ts1 ← getCurrentTime
>   maybeAudio ← F.importFile filename
>   case maybeAudio of
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
>       putStrLn (".. ssInsts" ++ show (bounds $ ssInsts arrays))
>       putStrLn (".. ssIBags" ++ show (bounds $ ssIBags arrays))
>       putStrLn (".. ssIGens" ++ show (bounds $ ssIGens arrays))
>       putStrLn (".. ssIMods" ++ show (bounds $ ssIMods arrays))
>       putStrLn (".. ssShdrs" ++ show (bounds $ ssShdrs arrays))
>       return sffile
>     
> renderSong             :: ∀ p . Clock p ⇒
>                           SFRoster
>                           → InstrMap (Stereo p)
>                           → FilePath
>                           → Music (Pitch, [NoteAttribute])
>                           → IO ()
> renderSong sfrost imap name song =
>   do
>     traceIO ("renderSong " ++ name)
>     ts1 ← getCurrentTime
>     let is = Map.keys $ fst $ listI song initCase (Map.empty, Map.empty)
>     let ps = Map.keys $ listP song initCase Map.empty
>     let (esI, esP) = printChoices sfrost is ps
>     let es = emitCounts is ps name ++ concatMap snd esI ++ concatMap snd esP
>     putStr (reapEmissions es)
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let path = name ++ ".wav"
>         putStr path
>         let (d,s) = renderSF song imap
>         if scanningOutput
>           then findOutliers d s
>           else if normalizingOutput
>                  then outFileNorm path d s
>                  else outFile     path d s
>         ts2 ← getCurrentTime
>         putStrLn (" (dur=" ++ show d ++ ") written in " ++ show (diffUTCTime ts2 ts1))
>       else
>         putStrLn "skipping..."
>     return ()
>
> emitCounts             :: [InstrumentName] → [PercussionSound] → String → [Emission]
> emitCounts is ps name                         =
>   [EndOfLine]
>     ++ [emitShowL name 20, emitShowL (length is) 4]
>     ++ [Unblocked " <- instruments, percussion -> ", emitShowL (length ps) 4, EndOfLine]

tournament among GM instruments and percussion from SoundFont files ===================================================

> xaEnterTournament      :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           SFRoster
>                           → BothMatchingAs
>                           → PerGMKey
>                           → a
>                           → [SSHint]
>                           → (Map.Map a [PerGMScored], [String])
>                           → (Map.Map a [PerGMScored], [String])
> xaEnterTournament sfrost@SFRoster{zFiles} bmas pergm kind hints (wix, ss)
>                                          = (Map.insert kind now wix, ss)
>   where
>     soFar              :: [PerGMScored]  = fromMaybe [] (Map.lookup kind wix)
>     now                :: [PerGMScored]  = pergm' : soFar
>     akResult                             = getEvalAgainstKind kind (matchingAs bmas) 
>
>     grade                                = computeGrade sfrost pergm kind hints akResult
>
>     arrays             :: SoundFontArrays
>                                          = zArrays (zFiles ! pWordF pergm)
>     nameI                                = F.instName $ ssInsts arrays ! pWordI pergm
>     mnameZ             :: Maybe String   = (\w → Just (F.sampleName (ssShdrs arrays ! w))) =<< mpWordZ pergm
>     pergm'             :: PerGMScored    = PerGMScored grade (toKind kind) akResult pergm nameI mnameZ 
>
> computeGrade           :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           SFRoster
>                           → PerGMKey
>                           → a
>                           → [SSHint]
>                           → AgainstKindResult
>                           → ArtifactGrade
> computeGrade sfrost@SFRoster{zFiles, zZoneCache} pergm kind hints againtKindResult
>   | traceIf msg False                    = undefined
>   | otherwise                            = ArtifactGrade (sum weightedScores) baseScores
>   where
>     perI@PerInstrument{pZonePairs, pInstCat}
>                                          = getPerInstrumentFromCache zZoneCache pergm{mpWordZ = Nothing}
>     zs                                   = tail pZonePairs
>     empiricals         :: [Int]          = [   scoreBool $ isStereoInst zs
>                                              , scoreBool $ is24BitInst zs
>                                              , round $ computeSplitCharacteristic kind zs
>                                              , scoreBool $ instConforms zs
>                                              , round fuzz]
>     howgood                              = againtKindResult - stands
>     fuzz               :: Double
>       | howgood > 0.000001               = max 0 (logBase 2 howgood) * fuzzFactor kind
>       | otherwise                        = 0
>
>     ss                 :: [Int]          = zipWith (*) qqDesires' empiricals
>
>     baseScores         :: [Int]          = [  foldHints hints
>                                              , head ss
>                                              , ss !! 1
>                                              , ss !! 2
>                                              , ss !! 3
>                                              , ss !! 4]
>
>     weightedScores     :: [Int]          = zipWith (*) baseScores ssWeights
>
>     msg = unwords [   "computeGrade " , show pInstCat
>                     , " and "         , show baseScores
>                     , " X "           , show ssWeights
>                     , " = "           , show weightedScores]
>
> computeSplitCharacteristic  :: ∀ a. (SFScorable a) ⇒ a → [(ZoneHeader, SFZone)] → Double
> computeSplitCharacteristic kind zs = log (3 * splitCount kind zs * factor)
>   where
>     factor             :: Double         = if isStereoInst zs
>                                              then 1/2
>                                              else 1
>
> isStereoInst, is24BitInst
>                        :: [(ZoneHeader, SFZone)] → Bool
>
> isStereoInst zs                          = isJust $ find isStereoZone zs
>       
> isStereoZone           :: (ZoneHeader, SFZone) → Bool
> isStereoZone (zh@ZoneHeader{pSample}, zone)
>                                          = stype == SampleTypeRight || stype == SampleTypeLeft
>   where
>     stype = toSampleType $ F.sampleType pSample 
>         
> instConforms           :: [(ZoneHeader, SFZone)] → Bool
> instConforms                             = all zoneConforms
>
> itemViolates           :: SFZone → (SFZone → Maybe a) → Bool
> itemViolates z f                         = isJust (f z)
>
> zoneConforms           :: (ZoneHeader, SFZone) → Bool
> zoneConforms (_, zone)            = not $ or unsupported
>   where
>     violates                             = itemViolates zone
>
>     unsupported        :: [Bool]
>     unsupported                          =
>       [
>           A.PressLoop == fromMaybe A.NoLoop (zSampleMode zone)
>         , violates zScaleTuning
>         , violates zExclusiveClass
>         , violates zInitQ
>         , violates zModLfoToVol
>         , violates zKeyToModEnvHold
>         , violates zKeyToModEnvDecay
>         , violates zKeyToVolEnvHold
>         , violates zKeyToVolEnvDecay
>         , violates zModulators
>       ]
>
> is24BitInst _                     = True -- WOX isJust $ ssM24 arrays       
 
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

prepare the specified instruments and percussion ======================================================================

> formZoneCache          :: SFRoster → [PerGMKey] → ZoneCache
> formZoneCache sfrost@SFRoster{ zFiles, zPreSampleCache, zPreInstCache}
>                                          = foldr zcFolder Map.empty
>   where
>     zcFolder           :: PerGMKey → ZoneCache → ZoneCache
>     zcFolder pergm                       = Map.insert pergm (computePerInst pergm)
>
>     computePerInst     :: PerGMKey → PerInstrument
>     computePerInst pergm@PerGMKey{pWordF, pWordI} 
>                                          = PerInstrument iinst (categorizeInst oList) (gList ++ oList)
>       where
>         arrays@SoundFontArrays{ssInsts, ssIBags, ssIGens, ssIMods, ssShdrs}
>                                          = zArrays (zFiles ! pWordF)
>         preI@PreInstrument{iName, isBmas}
>                                          = getPreInstrumentFromCache zPreInstCache pergm
>
>         iinst                            = ssInsts ! pWordI
>         jinst                            = ssInsts ! (pWordI+1)
>         ibagi                            = F.instBagNdx iinst
>         jbagi                            = F.instBagNdx jinst
>
>         (gIx, oIx)                       = profess
>                                              (ibagi <= jbagi)
>                                              "SoundFont file corrupt (computePerInst)"
>                                              (singleton ibagi, [ibagi+1..jbagi-1])
>
>         gList                            = map (buildZone defInstrumentZone)   gIx
>         oList                            = map (buildZone ((snd.head) gList))  oIx
>
>         buildZone      :: SFZone → Word → (ZoneHeader, SFZone)
>         buildZone fromZone bagIndex      = (zh, zone)
>           where
>             xgeni                        = F.genNdx $ ssIBags!bagIndex
>             ygeni                        = F.genNdx $ ssIBags!(bagIndex + 1)
>             xmodi                        = F.modNdx $ ssIBags!bagIndex
>             ymodi                        = F.modNdx $ ssIBags!(bagIndex + 1)
>
>             gens       :: [F.Generator]  = profess
>                                              (xgeni <= ygeni)
>                                              "SoundFont file corrupt (buildZone gens)"
>                                              (map (ssIGens !) [xgeni..ygeni-1])
>             mods       :: [F.Mod]        = profess
>                                              (xmodi <= ymodi)
>                                              "SoundFont file corrupt (buildZone mods)"
>                                              (map (ssIMods !) [xgeni..ygeni-1])
>
>             meval      :: Maybe Bool     = zSampleIndex
>                                            >>= \w → Just (PreSampleKey pWordF w)
>                                            >>= flip Map.lookup zPreSampleCache
>                                            >>= isNonPitchedByFft
>
>             fromZone'                    = fromZone {zModulators =
>                                              if xmodi == ymodi
>                                                then Nothing
>                                                else Just (xmodi, ymodi)}
>
>             zone@SFZone{zSampleIndex}    = foldl' addGen fromZone' gens
>
>             wZ                           = fromMaybe 0 zSampleIndex
>             zh                           = ZoneHeader wZ (ssShdrs ! wZ) (fromMaybe True meval)
>
>         categorizeInst :: [(ZoneHeader, SFZone)] → InstCat
>         categorizeInst zs
>           | traceNever msg False         = undefined
>           | otherwise                    = fromMaybe InstCatPerc latched
>           where
>             confirmed                    = bqForce isConfirmed
>             standing                     = bqForce stands
>             possible                     = bqForce isPossible
>             unknown                      = (Nothing, Nothing) == possible
>
>             badrom     :: Maybe InstCat  = if any hasRom zs
>                                              then Just InstCatDisq
>                                              else Nothing
>             alts       :: [Maybe InstCat]
>                                          = [ badrom
>                                            , fst confirmed
>                                            , snd confirmed
>                                            , if 0.8 < howPercish then Just InstCatPerc else Nothing
>                                            , fst standing
>                                            , snd standing
>                                            , if unknown then Just InstCatDisq else Nothing
>                                            , if 0.5 > howPercish then Just InstCatInst else Nothing
>                                            , fst possible
>                                            , snd possible]
>
>             byZone     :: [Bool]         = map computeCanBePerc zs
>             howPercish :: Double         = fromIntegral (length (filter id byZone)) / fromIntegral (length byZone)
>             latched    :: Maybe InstCat  = foldr (<|>) Nothing alts
>             msg                          = unwords ["categorizeInst ", show iName, " ", show pergm
>                                                    , "---\n", show latched, " = ", show alts]
>
>         bqForce        :: Double → (Maybe InstCat, Maybe InstCat)
>         bqForce thresh =
>           (            (\x → Just InstCatInst) =<< bestQualifying (instAs isBmas) thresh
>           ,            (\x → Just InstCatPerc) =<< bestQualifying (percAs isBmas) thresh)
>
>
>         hasRom         :: (ZoneHeader, SFZone) → Bool
>         hasRom (zh@ZoneHeader {pSample}, zone)
>                                          = F.sampleType pSample >= 0x8000
>
>         computeCanBePerc
>                        :: (ZoneHeader, SFZone) → Bool
>         computeCanBePerc (zh@ZoneHeader{pwZone}, zone)
>           | traceNever msg' False        = undefined
>           | otherwise                    = pinned || nonPitchedByFuzz || nonPitchedByFft
>           where
>             preS@PreSample{sName, psBmas}
>                                          = getPreSampleFromCache zPreSampleCache pergm{mpWordZ = Just pwZone}
>             pinned                       = maybe False pinnedKR (zKeyRange zone)
>             nonPitchedByFuzz             = any (isPossible' . flip getEvalAgainstKind (matchingAs psBmas))
>                                                nonPitchedInstruments
>             nonPitchedByFft              = sampleAnalyisEnabled && pbIsNonPitched zh
>
>             msg'                         = unwords ["computeCanBePerc "       ++ show sName
>                                                  ++ " "                       ++ show pinned
>                                                  ++ " "                       ++ show nonPitchedByFuzz
>                                                  ++ " "                       ++ show nonPitchedByFft]
>
> pinnedKR               :: (AbsPitch, AbsPitch) → Bool
> pinnedKR (p1, p2)                        = p1 == p2 || 1 == abs (p2 - p1) && matchedPair (p1, p2)
>   where
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

> prepareInstruments     :: SFRoster → [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments sfrost@SFRoster{zFiles, zWinningRecord}
>                                          = imap ++ singleton (Percussion, assignPercussion pmap)
>   where
>     imap                                 = Map.foldrWithKey imapFolder [] (pWinningI zWinningRecord)
>     pmap                                 = Map.foldrWithKey pmapFolder [] (pWinningP zWinningRecord)
>
>     imapFolder kind pergm accum          = (kind, assignInstrument (pPerGMKey pergm))            : accum
>
>     pmapFolder kind pergm accum          = (kind, (wF, wI))                                      : accum
>       where
>         (wF, wI)                         = (pWordF pergm', pWordI pergm')
>         pergm'                           = pPerGMKey pergm
>
>     assignInstrument   :: ∀ p . Clock p ⇒ PerGMKey → Instr (Stereo p)
>     assignInstrument pergm dur pch vol params
>                                          =
>       proc _ → do
>         (zL, zR) ← instrumentSF sfrost pergm dur pch vol params ⤙ ()
>         outA ⤙ (zL, zR)
>
>     assignPercussion   :: ∀ p . Clock p ⇒ [(PercussionSound, (Word, Word))] → Instr (Stereo p)
>     assignPercussion pmap dur pch vol params
>                                          =
>       proc _ → do
>         (zL, zR) ← instrumentSF sfrost (PerGMKey wF wI Nothing) dur pch vol params ⤙ ()
>         outA ⤙ (zL, zR)
>       where
>         kind           :: PercussionSound
>                                          = toEnum (pch - 35)
>         (wF, wI) = case lookup kind pmap of
>                    Nothing   → error (   "Percussion does not have "
>                                          ++ show kind ++ " in the supplied pmap.")
>                    Just x    → x
>
> instrumentSF           :: ∀ p . Clock p ⇒
>                           SFRoster
>                           → PerGMKey
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → Signal p () (Double, Double)
> instrumentSF sfrost@SFRoster{zFiles, zPlayCache} pergm@PerGMKey{pWordF, pWordI} dur pch vol params
>   | traceIf msg False                    = undefined
>   | otherwise                            = eutSynthesize (reconL, reconR) rSampleRate
>                                              dur pch'' vol'' params
>                                              (ssData arrays) (ssM24 arrays)
>   where
>     pch'                                 = clip (0, 127) pch
>     vol'                                 = clip (0, 127) vol
>
>     playKey                              = PlayKey pergm pch' vol'
>     (reconX, mreconX)  :: (Reconciled, Maybe Reconciled)
>                                          = fromMaybe (error $ "Note missing from play cache: "
>                                                               ++ show (pch, vol))
>                                                      (Map.lookup playKey zPlayCache)
>     (reconL@Reconciled{rSampleRate, rForceKey, rForceVel}, reconR)
>                        :: (Reconciled, Reconciled)
>                                          = (reconX, fromMaybe reconX mreconX)
>
>     pch''               :: AbsPitch       = maybe pch' (clip (0, 127)) rForceKey
>     vol''               :: Volume         = maybe vol' (clip (0, 127)) rForceVel
>
>     arrays                               = zArrays (zFiles ! pWordF)
>     nameI                                = F.instName $ ssInsts arrays ! pWordI
>     msg                                  = unwords ["instrumentSF ",  show nameI , " = ", show pergm, " / ", show (pch, vol)]

zone selection for rendering ==========================================================================================

> setZone                :: SFRoster
>                           → PerGMKey
>                           → AbsPitch
>                           → Volume
>                           → ((SFZone, F.Shdr), (SFZone, F.Shdr))
> setZone sfrost@SFRoster{zZoneCache} pergm pch vol
>                                          = ((snd zoneL, sampleL) ,(snd zoneR, sampleR))
>   where
>     perI@PerInstrument{pZonePairs} 
>                                          = getPerInstrumentFromCache zZoneCache pergm{mpWordZ = Nothing}
>     zs                 :: [(ZoneHeader, SFZone)]
>                                          = tail pZonePairs
>
>     (wF, wI)                             = (pWordF pergm, pWordI pergm)
>     (zoneL, zoneR)                       = selectZonePair zs (selectBestZone zs pch vol)
>     (sinL, sinR)                         = (  fromJust $ (zSampleIndex . snd) zoneL
>                                             , fromJust $ (zSampleIndex . snd) zoneR)
>     (sampleL, sampleR)                   = ((pSample . fst) zoneL, (pSample . fst) zoneR)
>
> selectBestZone         :: [(ZoneHeader, SFZone)] → AbsPitch → Volume → (ZoneHeader, SFZone)
> selectBestZone zs pch vol
>                                          = snd whichZ
>   where
>     scores                               = mapMaybe (scoreOneZone pch vol) zs
>     whichZ                               = profess
>                                              (not $ null scores)
>                                              "scores should not be null (selectBestZone)"
>                                              (minimumBy (comparing fst) scores)
>
> selectZonePair         :: [(ZoneHeader, SFZone)]
>                           → (ZoneHeader, SFZone)
>                           → ((ZoneHeader, SFZone), (ZoneHeader, SFZone))
> selectZonePair zs zone                   = if toSampleType stype == SampleTypeLeft
>                                              then (zone, ozone)
>                                              else (ozone, zone)
>   where
>     (zh@ZoneHeader{pSample}, z)          = zone
>     stype                                = F.sampleType pSample
>     slink                                = F.sampleLink pSample
>     mlinked                              = find (withSlink slink) zs
>     -- TODO: should base it strictly on Stereo sample type?
>     ozone                                = case mlinked of
>                                              Nothing → zone
>                                              Just _  → fromJust $ find (withSlink slink) zs
>
>     withSlink          :: Word → (ZoneHeader, SFZone) → Bool
>     withSlink toMatch (zh, zone)         = fromJust (zSampleIndex zone) == toMatch
>
> scoreOneZone           :: AbsPitch
>                           → Volume
>                           → (ZoneHeader, SFZone)
>                           → Maybe (Int, (ZoneHeader, SFZone))
> scoreOneZone pch vol (zh, zone)                  =
>     if qualify
>     then Just (score, (zh, zone))
>     else Nothing
>   where
>     qualify            :: Bool
>     qualify                              = DAllOn /= qqDesireReStereo defT || isStereoZone (zh, zone)
>
>     score = score1 + score2
>
>     score1 = scorePitchDistance pch (zKeyRange zone)
>     score2 = scoreVelocityDistance vol (zVelRange zone)
>
> instrumentSplitCount   :: InstrumentName → [(ZoneHeader, SFZone)] → Double
> instrumentSplitCount kind zs             = fromIntegral (length zs)
>
> percussionSplitCount   :: PercussionSound → [(ZoneHeader, SFZone)] → Double
> percussionSplitCount kind zs             = max 1 (fromIntegral $ length $ mapMaybe (match . snd) zs)
>   where
>     match              :: SFZone → Maybe PercussionSound
>     match zone
>       | isNothing mkind                  = Nothing
>       | kind == fromJust mkind           = Just kind
>       | otherwise                        = Nothing
>       where
>         mkind                            = qualifyKeyRange =<< zKeyRange zone
>
> scorePitchDistance     :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> scorePitchDistance cand mrange           =
>   case mrange of
>     Nothing                   → 1000
>     Just (rangeMin, rangeMax) → let
>                                   dist1 = abs $ cand - rangeMin
>                                   dist2 = abs $ cand - rangeMax
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
>                                   dist1 = abs $ cand - rangeMin
>                                   dist2 = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 10 * max dist1 dist2
>                                   else 100 * min dist1 dist2

reconcile zone and sample header ======================================================================================

> reconcileLR            :: ((SFZone, F.Shdr), (SFZone, F.Shdr)) → (Reconciled, Reconciled)
> reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>   | traceNever msg False                 = undefined
>   | otherwise                            = (recL, recR')
>   where
>     recL = reconcile (zoneL, shdrL)
>     recR = reconcile (zoneR, shdrR)
>     recR' = recR{
>               rRootKey                   = rRootKey recL
>             , rPitchCorrection           = rPitchCorrection recL}
>     msg = unwords ["reconcileLR recL=", show recL, "\n  zoneL=", show zoneL, "\n  shdrL=", show shdrL]
>
> reconcile              :: (SFZone, F.Shdr) → Reconciled 
> reconcile (zone, shdr)                   =
>   Reconciled {
>     rSampleMode      = fromMaybe             A.NoLoop
>                                                (zSampleMode    zone)
>   , rSampleRate      = fromIntegral          (F.sampleRate
>                                                shdr)
>   , rStart           = addIntToWord          (F.start shdr)
>                                                (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd             = addIntToWord          (F.end shdr)
>                                                (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart       = addIntToWord          (F.startLoop shdr)
>                                                (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd         = addIntToWord          (F.endLoop shdr)
>                                                (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey         = fromIntegral (fromMaybe
>                                              (F.originalPitch shdr)   (zRootKey              zone))
>   , rForceKey        = fmap                  fromIntegral             (zKey                  zone)
>   , rForceVel        = fmap                  fromIntegral             (zVel                  zone)
>   , rAttenuation     = resAttenuation                                 (zInitAtten            zone)
>   , rVolEnv          = deriveEnvelope                                 (zDelayVolEnv          zone)
>                                                                       (zAttackVolEnv         zone)
>                                                                       (zHoldVolEnv           zone)
>                                                                       (zDecayVolEnv          zone)
>                                                                       (zSustainVolEnv        zone)
>                                                                       (zReleaseVolEnv        zone)
>                                                                       Nothing
>   , rPitchCorrection = if usePitchCorrection
>                          then Just $ resPitchCorrection               (F.pitchCorrection     shdr)
>                                                                       (zCoarseTune           zone)
>                                                                       (zFineTune             zone)
>                          else Nothing
>
>   , rModulation      =                                                (resModulation         zone)
>   , rEffects         = deriveEffects                                  (zChorus               zone)
>                                                                       (zReverb               zone)
>                                                                       (zPan                  zone)}
>
>   where
>     resPitchCorrection :: Int → Maybe Int → Maybe Int → Double
>     resPitchCorrection alt mps mpc       = fromMaybe ((fromCents . fromIntegral) alt) (fromCents' mps mpc)
>
>     resAttenuation     :: Maybe Int → Double
>     resAttenuation matten                = if useAttenuation
>                                              then fromCentibels matten
>                                              else 1.0
>
>     resModulation     :: SFZone → Modulation
>     resModulation z@SFZone{
>                           zModLfoToPitch, zVibLfoToPitch, zModEnvToPitch
>                         , zInitFc, zInitQ
>                         , zModLfoToFc, zModEnvToFc, zFreqModLfo, zFreqVibLfo, zDelayModLfo, zDelayVibLfo
>                         , zDelayModEnv, zAttackModEnv, zHoldModEnv, zDecayModEnv, zSustainModEnv, zReleaseModEnv}
>                                          = Modulation mLowPass
>                                                       mModEnv
>                                                       mModLfo
>                                                       mVibLfo
>                                                       (summarize toPitch    mModEnv mModLfo mVibLfo)
>                                                       (summarize toFilterFc mModEnv mModLfo mVibLfo)
>                                                       (summarize toVolume   mModEnv mModLfo mVibLfo)
>
>       where
>         initFc         :: Double         = fromAbsoluteCents $ fromMaybe 13500 zInitFc
>         initQ          :: Double         = maybe 0 (fromIntegral . clip (0, 960)) zInitQ
>
>         mLowPass       :: Maybe LowPass  = if useLowPass && (isJust zInitFc || isJust zInitQ)
>                                              then Just $ LowPass initFc initQ
>                                              else Nothing
>         mModEnv        :: Maybe Envelope = deriveEnvelope
>                                              zDelayModEnv
>                                              zAttackModEnv
>                                              zHoldModEnv
>                                              zDecayModEnv
>                                              zSustainModEnv
>                                              zReleaseModEnv
>                                              (Just (zModEnvToPitch, zModEnvToFc))
>         mModLfo        :: Maybe LFO      = deriveLFO zDelayModLfo zFreqModLfo zModLfoToPitch zModLfoToFc Nothing
>         mVibLfo        :: Maybe LFO      = deriveLFO zDelayVibLfo zFreqVibLfo zVibLfoToPitch Nothing     Nothing
>
>         summarize      :: (ModTarget → Double) → Maybe Envelope → Maybe LFO → Maybe LFO → Maybe [Double]
>         summarize funWhich menv mmodlfo mviblfo
>                                          = if any (/= 0.0) targetList
>                                              then Just targetList
>                                              else Nothing
>           where
>             targetList = [  funWhich $ maybe defModTarget eModTarget mModEnv
>                           , funWhich $ maybe defModTarget lModTarget mModLfo
>                           , funWhich $ maybe defModTarget lModTarget mVibLfo]

carry out and cache play situations ===================================================================================

> createPlayCache        :: ∀ a. (Ord a) ⇒
>                           SFRoster
>                           → Map.Map a PerGMScored
>                           → Map.Map PlayKey (Reconciled, Maybe Reconciled)
> createPlayCache sfrost                   = Map.foldrWithKey cpFolder Map.empty
>   where
>     cpFolder           :: a
>                           → PerGMScored
>                           → Map.Map PlayKey (Reconciled, Maybe Reconciled)
>                           → Map.Map PlayKey (Reconciled, Maybe Reconciled)
>     cpFolder kind pergm ps               = Map.union ps (precalcNotes kind pergm)
>
>     precalcNotes       :: a → PerGMScored → Map.Map PlayKey (Reconciled, Maybe Reconciled)
>     precalcNotes kind pergm@PerGMScored{pPerGMKey}
>                                          = foldl' (pvFolder pPerGMKey{mpWordZ = Nothing})
>                                                   Map.empty
>                                                   [(p,v) | p ← [0..127], v ← [0..127]]
>
>     pvFolder           :: PerGMKey
>                           → Map.Map PlayKey (Reconciled, Maybe Reconciled)
>                           → (AbsPitch, Volume)
>                           → Map.Map PlayKey (Reconciled, Maybe Reconciled)
>     pvFolder pergm ps (pch, vol)         = Map.insert playKey playValue ps
>       where
>         playKey                          = PlayKey pergm pch vol
>         ((zoneL, shdrL), (zoneR, shdrR))
>                        :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                                          = setZone sfrost pergm pch vol
>         (reconL, reconR)
>                        :: (Reconciled, Reconciled)
>                                          = reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>         playValue                        = (reconL, if reconR == reconL
>                                                       then Nothing
>                                                       else Just reconR)

emit standard output text detailing what choices we made for rendering GM items =======================================

> printChoices           :: SFRoster
>                           → [InstrumentName]
>                           → [PercussionSound]
>                           → ([(Bool, [Emission])], [(Bool, [Emission])])
> printChoices sfrost@SFRoster{zWinningRecord} is ps
>                                          = (map (showI zWinningRecord) is, map (showP zWinningRecord) ps)
>   where
>     showI              :: WinningRecord → InstrumentName → (Bool, [Emission])
>     showI wr@WinningRecord{pWinningI} kind
>                                          = result
>       where
>         mpergm                           = Map.lookup kind pWinningI
>         result
>           | isJust mpergm                = (True, showPerGM sfrost (fromJust mpergm)
>                                                   ++ [Unblocked " -> ", gmId kind, EndOfLine])
>           | kind == Percussion           = (True, [gmId kind, Unblocked "(pseudo-instrument)", EndOfLine])
>           | otherwise                    = (False, [gmId kind, Unblocked " not found", EndOfLine])
>
>     showP               :: WinningRecord → PercussionSound → (Bool, [Emission])
>     showP wr@WinningRecord{pWinningP} kind
>                                          = result
>       where
>         mpergm                           = Map.lookup kind pWinningP
>         result
>           | isJust mpergm                = (True, showPerGM sfrost (fromJust mpergm)
>                                                   ++ [Unblocked " -> ", gmId kind, EndOfLine])
>           | otherwise                    = (False, [gmId kind, Unblocked " not found", EndOfLine])
>
> showPerGM              :: SFRoster → PerGMScored → [Emission]
> showPerGM sfrost@SFRoster{zFiles} pergm'@PerGMScored{pPerGMKey}
>                                          = [emitShowL pWordF 4] ++ showI pWordI ++ showmZ
>   where
>     pergm@PerGMKey{pWordF, pWordI, mpWordZ}
>                                          = pPerGMKey
>     arrays@SoundFontArrays{ssInsts, ssShdrs}
>                                          = zArrays (zFiles ! pWordF)
>
>     showI              :: Word → [Emission]
>     showI wordI                          = [Unblocked ".", gmName (F.instName (ssInsts ! wordI))]
>
>     showmZ                               = maybe [] showZ mpWordZ
>
>     showZ              :: Word → [Emission]
>     showZ wordZ                          = [Unblocked ".", gmName (F.sampleName (ssShdrs ! wordZ))]
>
> emitFileListC          :: Array Word SFFile → [Emission]
> emitFileListC vFile                      = comment
>   where
>     comment                              = concatMap (uncurry doF) (zip [0..] (toList vFile))
>     doF                :: Int → SFFile → [Emission]
>     doF nth sffile                       = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>         
> qualifyZone            :: (Word, SFZone) → Maybe (PercussionSound, Word)
> qualifyZone (wordP, zone)                = fmap (, wordP) (qualifyKeyRange =<< zKeyRange zone)
>
> qualifyKeyRange        :: (AbsPitch, AbsPitch) → Maybe PercussionSound
> qualifyKeyRange (p1, p2)
>   | pinnedKR (p1, p2)
>     && ap >= fromEnum AcousticBassDrum
>     && ap <= fromEnum OpenTriangle       = Just (toEnum ap)
>   | otherwise                            = Nothing
>   where
>     ap = p1 - 35    
>
> dumpContestants        :: ∀ a. (Ord a, Show a, SFScorable a) ⇒ (a, [PerGMScored]) → [Emission]
> dumpContestants (kind, contestants) = prolog ++ es ++ epilog
>   where
>     prolog, es, epilog :: [Emission]
>     prolog = []
>     es = emitLine [emitShowL kind 50] ++ concatMap dumpContestant contestants ++ emitLine []
>     epilog = []
>
> dumpContestant         :: PerGMScored → [Emission]
> dumpContestant pergm@PerGMScored{pArtifactGrade, pAgainstKindResult, pPerGMKey, szI, mszP}
>                                          = es
>   where
>     es = emitLine [
>                     Blanks 8, emitShowL (pWordF pPerGMKey) 4, emitShowR szI 22
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
> qqHints                :: Map.Map HintId HintBody
> qqHints                                  = Map.fromList myHints
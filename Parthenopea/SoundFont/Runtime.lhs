> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

Runtime
William Clements
February 1, 2025

> module Parthenopea.SoundFont.Runtime ( bootNRender ) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Either
> import Data.Foldable ( toList )
> import Data.List hiding (insert)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord ( Down(Down) )
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import Debug.Trace ( traceIO )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, Stereo, Clock, Signal )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.Repro.Synthesizer
> import Parthenopea.Music.Siren
> import Parthenopea.SoundFont.Boot ( equipInstruments )
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
  
executive =============================================================================================================

> bootNRender            :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> bootNRender songs                        = do
>   tsStarted                              ← getCurrentTime
>
>   rost                                   ← qualifyKinds songs
>   mbundle                                ← equipInstruments rost
>   if isNothing mbundle
>     then do
>       return ()
>     else do 
>       let (prerunt, matches, rd)         = fromJust mbundle
>       runt                               ← finishRuntime prerunt rost matches rd
>
>       CM.when diagnosticsEnabled         (putStrLn $ unwords [fName, show runt])
>       CM.when doRender                   (doRendering runt)
>
>       tsRendered                         ← getCurrentTime
>       putStrLn ("___overall: " ++ show (diffUTCTime tsRendered tsStarted))
>   where
>     fName                                = "bootNRender"
>
>     -- track the complete _qualified_ populations of: samples, instruments, percussion
>     finishRuntime      ::  SFRuntime
>                            → ([InstrumentName], [PercussionSound])
>                            → Matches
>                            → ResultDispositions
>                            → IO SFRuntime
>     finishRuntime prerunt rost matches rd
>                                          = do
>       CM.when (howVerboseScanReport > (1/10)) (writeScanReport prerunt rd)
>       tsScanned                          ← getCurrentTime
>      
>       -- actually conduct the tournament
>       (wI, wP)                           ← decideWinners prerunt rost matches 
>       tsDecided                          ← getCurrentTime
>       putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsScanned))
>
>       CM.when (howVerboseTournamentReport > (1/10)) (writeTournamentReport prerunt.zFiles wI wP)
>       tsReported                         ← getCurrentTime
>
>       let wins                           = WinningRecord (Map.map head wI) (Map.map head wP)
>
>       tsRecond                           ← getCurrentTime
>       putStrLn ("___create winning record: " ++ show (diffUTCTime tsRecond tsReported))
>         
>       return prerunt{zWinningRecord = wins}
>
>     -- get it on
>     doRendering        :: SFRuntime → IO ()
>     doRendering runt
>                                          = do
>       tsStarted                          ← getCurrentTime
>
>       -- readying instrument maps to be accessed from song renderer
>       traceIO                            "prepareInstruments"
>       imap                               ← prepareInstruments runt
>       tsPrepared                         ← getCurrentTime
>       putStrLn ("___prepare instruments: " ++ show (diffUTCTime tsPrepared tsStarted))
>
>       -- here's the heart of the coconut
>       mapM_ (renderSong runt imap) songs
>
>       tsRendered                         ← getCurrentTime
>       putStrLn ("___render songs: "        ++ show (diffUTCTime tsRendered tsPrepared))
>
> writeScanReport        :: SFRuntime → ResultDispositions → IO ()
> writeScanReport runt rd@ResultDispositions{ .. }
>                                          = do
>   CM.when diagnosticsEnabled (putStrLn $ unwords [fName, show rd])
>   tsStarted                              ← getCurrentTime
>
>   -- output all selections to the report file
>   let esTimeStamp                        = [Unblocked (show tsStarted), EndOfLine, EndOfLine]
>   let esSampleSummary                    = summarize preSampleDispos ++ [EndOfLine]
>   let esInstSummary                      = summarize preInstDispos ++ [EndOfLine]
>   let esPreZoneSummary                   = summarize preZoneDispos ++ [EndOfLine]
>   let esSampleScan                       = procMap preSampleDispos ++ [EndOfLine]
>   let esInstScan                         = procMap preInstDispos ++ [EndOfLine]
>   let esPreZoneScan                      = procMap preZoneDispos ++ [EndOfLine]
>   let esTail                             = [EndOfLine, EndOfLine]
>
>   writeFileBySections
>     reportScanName
>     ([esTimeStamp, esSampleSummary, esInstSummary, esPreZoneSummary]
>      ++ if howVerboseScanReport < (1/3) then [] else [esSampleScan, esInstScan, esPreZoneScan]
>      ++ [esTail])
>
>   tsFinished                             ← getCurrentTime
>   putStrLn (unwords ["___report scan results:", show (diffUTCTime tsFinished tsStarted)])
>   traceIO (unwords ["wrote", reportScanName])
>
>   where
>     fName                                = "writeScanReport"
>
>     summarize          :: ∀ r . (SFResource r) ⇒ Map r [Scan] → [Emission]
>     summarize sm                         =
>       let
>         hs             :: [((Disposition, Impact, String), Int)]
>         hs                               = sortOn (Down . snd) $ Map.toList $ Map.foldr histoFold Map.empty sm
>
>         histoFold      :: [Scan] → Map (Disposition, Impact, String) Int → Map (Disposition, Impact, String) Int 
>         histoFold ss mfold               = foldr (\dispo m → Map.insertWith (+) dispo 1 m) mfold ts
>           where
>             ts                           = map getTriple ss
>
>         emitHisto      :: ((Disposition, Impact, String), Int) → [Emission]
>         emitHisto ((dispo, impact, function), count)
>                                          =
>           [  emitShowL   count        16
>            , emitShowL   dispo        24
>            , emitShowL   impact       32
>            , ToFieldL    function     52
>            , EndOfLine]
>       in
>         concatMap emitHisto hs
>         
>     procMap            :: ∀ r . (SFResource r, Show r) ⇒ Map r [Scan] → [Emission]
>     procMap sm                           = concat $ Map.mapWithKey procr sm
>
>     procr              :: ∀ r . (SFResource r, Show r) ⇒ r → [Scan] → [Emission]
>     procr k ss_                          = if null ss
>                                              then []
>                                              else emit ++ [EndOfLine] ++ concatMap procs ss ++ [EndOfLine]
>       where
>         ss                               = filter (\s → s.sDisposition `notElem` elideset) ss_
>         sffile                           = runt.zFiles ! wfile k
>
>         procs          :: Scan → [Emission]
>         procs scan                       =
>           [  emitShowL scan.sDisposition 24
>            , emitShowL scan.sImpact      32
>            , ToFieldL scan.sFunction     52
>            , Unblocked scan.sClue
>            , EndOfLine]
>
>         emit           :: [Emission]
>         emit                             = 
>           [  Unblocked (show k)
>            , Blanks 5
>            , Unblocked sffile.zFilename
>            , Blanks 5]
>           ++ kname k sffile
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
>   let esQ              = [] -- formerly emitSettingses
>   let esTail           = singleton $ Unblocked "\n\nThe End\n\n"
>   let eol              = singleton EndOfLine
>
>   writeFileBySections reportTournamentName [esFiles, legend, esI, eol, esFiles, legend, esP, esQ, esTail]
>   tsFinished           ← getCurrentTime
>   putStrLn ("___report tournament results: " ++ show (diffUTCTime tsFinished tsStarted))
>   traceIO ("wrote " ++ reportTournamentName)
>
>   where
>     nfs                :: [(Int, SFFile)]
>     nfs                = zip [0..] (toList sffiles)
>     emitFileListC      = concatMap doF nfs
>     doF (nth, sffile)  = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>
> renderSong             :: ∀ p . Clock p ⇒
>                           SFRuntime
>                           → InstrMap (Stereo p)
>                           → (String, DynMap → Music (Pitch, [NoteAttribute]))
>                           → IO ()
> renderSong runt imap (name, song)           =
>   do
>     traceIO ("renderSong " ++ name)
>     ts1                                  ← getCurrentTime
>     ding                                 ← shredMusic (song Map.empty)
>     let dynMap                           = makeDynMap ding
>     CM.unless (null dynMap)              (traceIO $ unwords ["dynMap", show dynMap])
>     let ks                               = Map.keys ding.shRanges
>     let (is, ps)                         = (map (\i → fromMaybe i (Map.lookup i dynMap)) (lefts ks), rights ks)
>     let (esI, esP)                       = printChoices runt is ding.shMsgs ps
>     let ex                               = [Unblocked name, EndOfLine] ++ concatMap snd esI ++ concatMap snd esP
>     putStr (reapEmissions ex)
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let path                         = name ++ ".wav"
>         putStr path
>         let (durS,s)                     = renderSF (song dynMap) imap
>         traceIO (unwords ["-> outFile*", path, show durS])
>         if normalizingOutput
>           then outFileNorm path durS s
>           else outFile     path durS s
>         traceIO (unwords ["<- outFile*", path, show durS])
>         ts2                              ← getCurrentTime
>         putStrLn (" (dur=" ++ show durS ++ ") written in " ++ show (diffUTCTime ts2 ts1))
>       else
>         putStrLn "skipping..."
>     return ()

define signal functions and instrument maps to support rendering ======================================================

> prepareInstruments     :: SFRuntime → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments runt                  = 
>     return $ (Percussion, assignPercussion)                                                      : imap
>   where
>     WinningRecord{pWinningI, pWinningP}  = runt.zWinningRecord
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
>     assignInstrument pergm durI pch vol params
>                                          =
>       proc _ → do
>         (zL, zR)                         ← instrumentSF runt pergm durI pch vol params ⤙ ()
>         outA                             ⤙ (zL, zR)
>
>     assignPercussion   :: ∀ p . Clock p ⇒ Instr (Stereo p)
>     assignPercussion durP pch vol params
>                                          = assignInstrument pergm durP pch vol params
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
>                           SFRuntime
>                           → PerGMKey
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → Signal p () (Double, Double)
> instrumentSF SFRuntime{zFiles, zBoot} pergm durI pchIn volIn nps
>   | traceIf trace_ISF False              = undefined
>   | otherwise                            = eutSynthesize (reconX, mreconX) reconX.rSampleRate
>                                              durI pchOut volOut nps
>                                              zSample.ssData zSample.ssM24
>   where
>     fName_                               = "instrumentSF"
>     trace_ISF                            =
>       unwords [fName_, show pergm.pgkwFile, show preI.piChanges.cnSource, show (pchIn, volIn), show durI]
>
>     noon                                 = NoteOn
>                                              (clip (0, 127) volIn)
>                                              (clip (0, 127) pchIn)
>     pchOut              :: AbsPitch      = fromMaybe noon.noteOnKey reconX.rForceKey
>     volOut              :: Volume        = fromMaybe noon.noteOnVel reconX.rForceVel
>
>     SFFile{zSample}                      = zFiles ! pergm.pgkwFile
>
>     SFBoot{zPreInstCache, zPerInstCache} = zBoot
>     preI                                 = zPreInstCache Map.! pergm
>     perI                                 = zPerInstCache Map.! pergm
>
>     (reconX, mreconX)                    =
>       case setZone of
>         Left (pz, sfz, shdr)             → (recon (pz, sfz, shdr) noon nps (fromRational durI), Nothing)
>         Right zsPlus                     → reconLR zsPlus noon nps (fromRational durI)

zone selection for rendering ==========================================================================================

>     setZone            :: Either (PreZone, SFZone, F.Shdr) ((PreZone, SFZone, F.Shdr), (PreZone, SFZone, F.Shdr))
>     setZone                              =
>       case doFlyEye of 
>         Left (pzL, zoneL)                → Left (pzL, zoneL, effPZShdr pzL)
>         Right ((pzL, zoneL), (pzR, zoneR))
>                                          → Right ((pzL, zoneL, effPZShdr pzL), (pzR, zoneR, effPZShdr pzR))
>
>     doFlyEye           :: Either (PreZone, SFZone) ((PreZone, SFZone), (PreZone, SFZone))
>     doFlyEye
>       | bagIdL <= 0 || cntL <= 0 || bagIdR <= 0 || cntR <= 0
>                                          = error $ unwords [fName, "cell is nonsense"]
>       | isNothing foundL || isNothing foundR
>                                          = error $ unwords [fName, "zone not present"] 
>       | foundL == foundR                 = (Left . fromJust) foundL
>       | otherwise                        = Right (fromJust foundL, fromJust foundR)
>       where
>         fName                            = unwords [fName_, "doFlyEye"]
>
>         (index1, index2)                 = noonAsCoords noon
>         ((bagIdL, cntL), (bagIdR, cntR)) =
>           (lookupCellIndex index1 perI.pSmashing, lookupCellIndex index2 perI.pSmashing)
>         foundL                           = findByBagIndex' perI.pZones bagIdL
>         foundR                           = findByBagIndex' perI.pZones bagIdR

reconcile zone and sample header ======================================================================================

> reconLR                :: ((PreZone, SFZone, F.Shdr), (PreZone, SFZone, F.Shdr))
>                           → NoteOn
>                           → [Double]
>                           → Dur
>                           → (Recon, Maybe Recon)
> reconLR ((pzL, zoneL, shdrL), (pzR, zoneR, shdrR)) noon nps durR
>                                          = (recL, Just recR')
>   where
>     secsScored         :: Double         = fromRational durR
>     recL@Recon{rRootKey = rkL, rPitchCorrection = pcL}
>                                          = recon (pzL, zoneL, shdrL) noon nps secsScored
>     recR                                 = recon (pzR, zoneR, shdrR) noon nps secsScored
>     recR'                                = recR{
>                                                rRootKey                   = rkL
>                                              , rPitchCorrection           = pcL}
>
> recon                  :: (PreZone, SFZone, F.Shdr) → NoteOn → [Double] → Double → Recon
> recon (pz, zone_, sHdr@F.Shdr{ .. }) noon nps secsScored
>                                          = reconL
>   where
>     zd                                   = pz.pzDigest
>     zone@SFZone{ .. }
>                                          =
>       (\case
>         Just np                          → applyNoteParameter noon zone_ np secsScored
>         Nothing                          → zone_) (listToMaybe nps)
>     m8n                                  = reconModulation zone sHdr noon
>
>     reconL = Recon {
>     rSampleMode    = fromMaybe           A.NoLoop           zSampleMode
>   , rSampleRate    = fromIntegral        sampleRate
>   , rStart         = (+)                 start              (fromIntegral zd.zdStart)
>   , rEnd           = (+)                 end                (fromIntegral zd.zdEnd)
>   , rLoopStart     = (+)                 startLoop          (fromIntegral zd.zdStartLoop)
>   , rLoopEnd       = (+)                 endLoop            (fromIntegral zd.zdEndLoop)
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
>                                                             (zHoldVolEnv,  zKeyToVolEnvHold)
>                                                             (zDecayVolEnv, zKeyToVolEnvDecay)
>                                                             zSustainVolEnv
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
>     reconAttenuation _     {- WOX -}       = if useAttenuation
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
> reconModulation        :: SFZone → F.Shdr → NoteOn → Modulation
> reconModulation SFZone{ .. } shdr noon
>   | traceIf trace_RM False               = undefined
>   | otherwise                            = resolveMods m8n zModulators defaultMods
>   where
>     fName                                = "recon"
>     trace_RM                             = unwords [fName, show resonanceType, shdr.sampleName]
>
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
>         (fromMaybe 13_500 zInitFc)
>         (fromMaybe 0 zInitQ)
>         1 
>         useFastFourier
>         (-1) -- must always be replaced
>
>     resonanceType      :: ResonanceType  = ResonanceSVF
>     nModEnv            :: Maybe FEnvelope
>     nModEnv                              = deriveEnvelope
>                                              zDelayModEnv
>                                              zAttackModEnv
>                                              noon
>                                              (zHoldModEnv, zKeyToModEnvHold) 
>                                              (zDecayModEnv, zKeyToModEnvDecay)
>                                              zSustainModEnv
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
>         (coAccess toWhich $ maybe defModTriple (fromJust . fModTriple) nModEnv)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nModLfo)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nVibLfo)

The End
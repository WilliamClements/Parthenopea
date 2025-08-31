> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

Runtime
William Clements
February 1, 2025

> module Parthenopea.SoundFont.Runtime ( prepareInstruments
>                                      , runUnitTests
>                                      , writeRangesReport
>                                      , writeScanReport
>                                      , writeTournamentReport) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed ( (!), inRange )
> import qualified Data.Audio              as A
> import Data.Either
> import Data.Foldable ( toList )
> import Data.List ( sortOn, singleton )
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe ( isNothing, fromJust, fromMaybe )
> import Data.Ord ( Down(Down) )
> import Data.Time.Clock ( getCurrentTime )
> import qualified Data.Vector             as VB
> import Debug.Trace ( traceIO )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Render ( Instr )
> import Euterpea.IO.Audio.Types ( AudRate, Stereo, Clock, Signal )
> import Euterpea.IO.MIDI.MEvent ( MEvent(ePitch) )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Passage
> import Parthenopea.Music.PassageTest ( passageTests )
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Envelopes ( deriveEnvelope )
> import Parthenopea.Repro.EnvelopesTest ( envelopesTests )
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.ModulationTest ( modulationTests )
> import Parthenopea.Repro.Smashing ( lookupCellIndex )
> import Parthenopea.Repro.SmashingTest ( smashingTests )
> import Parthenopea.Repro.Synthesizer
> import Parthenopea.Repro.SynthesizerTest ( synthesizerTests )
> import Parthenopea.SoundFont.BootTest ( bootTests )
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
  
executive =============================================================================================================

> runUnitTests           :: IO ()
> runUnitTests                             = do
>   resultBoot                             ← runTestsQuietly bootTests
>   resultEnvelopes                        ← runTestsQuietly envelopesTests
>   resultModulation                       ← runTestsQuietly modulationTests     
>   resultSmashing                         ← runTestsQuietly smashingTests
>   resultSynthesizer                      ← runTestsQuietly synthesizerTests
>   resultsPassage                         ← runTestsQuietly passageTests
>   let resultDiscrete                     = True -- runTestsQuietly discreteTests
>   putStrLn $ unwords [show
>      (profess
>        (and [resultSmashing, resultBoot, resultModulation, resultSynthesizer
>            , resultsPassage, resultEnvelopes, resultDiscrete])
>        (unwords ["one or more unit tests failed"])
>        True)]
>   putStrLn "Unit tests completed successfully"
>
> writeRangesReport      :: [Song] → Map GMKind Shred → IO ()
> writeRangesReport songs ding             = do
>   let rollup                             =
>         Song "rollup" (const (foldr ((:+:) . uncap . songMusic) (rest 0) songs)) ding
>   let esAll                              = concatMap doSong songs
>   let esPrefix                           =
>         [ToFieldL "GMKind" 20
>        , ToFieldL "(range)" 22
>        , ToFieldR "lowest" 8, Blanks 3
>        , ToFieldL "*status" 8
>        , ToFieldR "highest" 8, Blanks 3
>        , ToFieldL "*status" 8
>        , ToFieldL "note count" 15
>        , ToFieldL "alternative" 20, EndOfLine]
>   let esSuffix                          = if 1 < length songs
>                                             then doSong rollup
>                                             else []
>   writeFileBySections reportRangesName [esPrefix, esAll, esSuffix]
>
>   where
>     uncap              :: (DynMap → Music1) → Music1
>     uncap m                              = m Map.empty
>
>     doSong             :: Song → [Emission]
>     doSong song                          =
>       [EndOfLine
>      , ToFieldL song.songName 60
>      , ToFieldL (songTimeAndNoteCount song) 60
>      , EndOfLine, EndOfLine] ++ doMusic song.songShredding
>     doMusic            :: Map GMKind Shred → [Emission]
>     doMusic ding'                        = concatMap (uncurry doGMKind) (Map.assocs ding')
>     doGMKind           :: GMKind → Shred → [Emission]
>     doGMKind gmkind shred                = either doInstrument doPercussion gmkind
>       where
>         lo                               = shred.shLowNote.ePitch
>         hi                               = shred.shHighNote.ePitch
>
>         this                             = fromLeft (error "writeRangeReport") gmkind
>         alt                              = findBetterInstrument this (lo, hi)
>         strAlt                           = if isLeft gmkind && (alt /= this) 
>                                              then show alt
>                                              else ""  
>         doInstrument       :: InstrumentName → [Emission]
>         doInstrument kind                =
>           [emitShowL kind 20
>          , emitShowL (instrumentPitchRange kind) 22
>          , emitShowR (pitch lo) 8, Blanks 3
>          , ToFieldL (indicator lo) 8
>          , emitShowR (pitch hi) 8, Blanks 3
>          , ToFieldL (indicator hi) 8
>          , emitShowR shred.shCount 10, Blanks 5
>          , ToFieldL strAlt 20, EndOfLine]
>           
>         doPercussion       :: PercussionSound → [Emission]
>         doPercussion kind                =
>           [emitShowL kind 20
>          , emitShowL (fromEnum kind + 35) 22
>          , Blanks 38
>          , emitShowL shred.shCount 15, EndOfLine]
>
>         mrange                           =
>           case gmkind of
>             Left iname                   → instrumentAbsPitchRange iname
>             _                            → Nothing
>         indicator p                      = if isNothing mrange || inRange (deJust "range" mrange) p
>                                              then "*in"
>                                              else "*out!!"
>
> writeScanReport        :: SFRuntime → ResultDispositions → IO ()
> writeScanReport runt rd                  = do
>   CM.when diagnosticsEnabled             (traceIO $ unwords [fName, show rd])
>
>   -- output all selections to the report file
>   tsStarted                              ← getCurrentTime
>   let esTimeStamp                        = [Unblocked (show tsStarted), EndOfLine, EndOfLine]
>   let esSampleSummary                    = summarize rd.preSampleDispos ++ [EndOfLine]
>   let esInstSummary                      = summarize rd.preInstDispos ++ [EndOfLine]
>   let esPreZoneSummary                   = summarize rd.preZoneDispos ++ [EndOfLine]
>   let esSampleScan                       = procMap rd.preSampleDispos ++ [EndOfLine]
>   let esInstScan                         = procMap rd.preInstDispos ++ [EndOfLine]
>   let esPreZoneScan                      = procMap rd.preZoneDispos ++ [EndOfLine]
>   let esTail                             = [EndOfLine, EndOfLine]
>
>   writeFileBySections
>     reportScanName
>     ([esTimeStamp, esSampleSummary, esInstSummary, esPreZoneSummary]
>      ++ if howVerboseScanReport < (1/3) then [] else [esSampleScan, esInstScan, esPreZoneScan]
>      ++ [esTail])
>   where
>     fName                                = "writeScanReport"
>
>     summarize          :: ∀ r . (SFResource r) ⇒ Map r [Scan] → [Emission]
>     summarize sm                         =
>       let
>         hs                               = sortOn (Down . snd) $ Map.toList $ Map.foldr histoFold Map.empty sm
>
>         histoFold ss mfold               = foldr (foldfun . getTriple) mfold ss
>           where
>             foldfun dispo                = Map.insertWith (+) dispo 1
>             
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
>     procMap sm                           = concat $ Map.mapWithKey procKey sm
>
>     procKey k ssIn                       = if null ssOut
>                                              then []
>                                              else prolog ++ [EndOfLine] ++ concatMap procScan ssIn ++ [EndOfLine]
>       where
>         ssOut                            = filter (\s → s.sDisposition `notElem` elideset) ssIn
>         sffile                           = runt.zFiles ! wfile k
>
>         prolog                           = 
>           [  Unblocked (show k)
>            , Blanks 5
>            , Unblocked sffile.zFilename
>            , Blanks 5]
>           ++ kname k sffile
>
>     procScan scan                    =
>       [  emitShowL scan.sDisposition 24
>        , emitShowL scan.sImpact      32
>        , ToFieldL scan.sFunction     52
>        , Unblocked scan.sClue
>        , EndOfLine]
>
> writeTournamentReport  :: SFRuntime
>                           → Map InstrumentName [PerGMScored]
>                           → Map PercussionSound [PerGMScored]
>                           → IO ()
> writeTournamentReport runt pContI pContP
>                        = do
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
>
>   where
>     nfs                :: [(Int, SFFile)]
>     nfs                = zip [0..] (toList runt.zFiles)
>     emitFileListC      = concatMap doF nfs
>     doF (nth, sffile)  = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]

define signal functions and instrument maps to support rendering ======================================================

> prepareInstruments     :: SFRuntime → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments runt                  = 
>     return $ (Percussion, assignPercussion)                                                               : imap
>   where
>     winners                              = runt.zWinningRecord
>     imap                                 = Map.foldrWithKey imapFolder [] winners.pWinningI
>     pmap                                 = Map.foldrWithKey pmapFolder [] winners.pWinningP
>
>     imapFolder kind scored target        = (kind, assignInstrument scored.pPerGMKey)                      : target
>     pmapFolder kind scored target        = (kind, (pgkwFile scored.pPerGMKey, pgkwInst scored.pPerGMKey)) : target
>
>     assignInstrument   :: ∀ p . Clock p ⇒ PerGMKey → Instr (Stereo p)
>     assignInstrument pergm durI pch vol params
>                                          =
>       proc _ → do
>         (zL, zR)                         ← instrumentSF runt pergm durI pch vol params ⤙ ()
>         outA                             ⤙ (zL, zR)
>
>     assignPercussion   :: ∀ p . Clock p ⇒ Instr (Stereo p)
>     assignPercussion pDur pch vol ps     = assignInstrument pergm pDur pch vol ps
>       where
>         pergm                            = PerGMKey wF wI Nothing
>         (wF, wI)                         =
>           case lookup kind pmap of
>             Nothing    → error $ unwords ["Percussion does not have", show kind, "in the supplied pmap."]
>             Just x     → x
>         kind           :: PercussionSound
>         kind                             = toEnum (pch - 35)
>
> instrumentSF           :: ∀ p . Clock p ⇒
>                           SFRuntime
>                           → PerGMKey
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → Signal p () (Double, Double)
> instrumentSF runt pergm durI pchIn volIn ps_
>   | traceIf trace_ISF False              = undefined
>   | otherwise                            = eutSynthesize (reconX, mreconX) reconX.rSampleRate
>                                              durI sffile.zSample.ssData sffile.zSample.ssM24
>   where
>     fName_                               = "instrumentSF"
>     trace_ISF                            =
>       unwords [fName_, show pergm, show preI.piChanges.cnName, show (pchIn, volIn), show durI, show ps]
>
>     ps                                   = VB.fromList ps_
>     noonIn                               = carefulNoteOn volIn pchIn
>     fly                                  = doFlyEye noonIn
>
>     noonOutL, noonOutR
>                         :: NoteOn
>     (noonOutL, noonOutR)                 = case fly of
>                                              Left (_, z)                     → (calcNoteOn z, undefined)
>                                              Right ((_, zL), (_, zR))        → (calcNoteOn zL, calcNoteOn zR)
>       where
>         calcNoteOn z                     = NoteOn (maybe (clip (0, 127) volIn) fromIntegral z.zVel) 
>                                                   (maybe (clip (0, 127) pchIn) fromIntegral z.zKey)
>
>     sffile                               = runt.zFiles ! pergm.pgkwFile
>
>     preI                                 = runt.zBoot.zPreInstCache Map.! pergm
>     perI                                 = runt.zBoot.zPerInstCache Map.! pergm
>
>     (reconX, mreconX)                    =
>       case fly of
>         Left (pz, sfz)                   → (makeRecon (pz, sfz) noonOutL ps (fromRational durI), Nothing)
>         Right ((pzL, sfzL), (pzR, sfzR)) → reconLR ((pzL, sfzL), (pzR, sfzR))
>                                                    (noonOutL, noonOutR)
>                                                    ps (fromRational durI)

zone selection for rendering ==========================================================================================

>     doFlyEye           :: NoteOn → Either (PreZone, SFZone) ((PreZone, SFZone), (PreZone, SFZone))
>     doFlyEye noonFly
>       | traceIf trace_DFE False          = undefined
>       | bagIdL <= 0 || cntL <= 0 || bagIdR <= 0 || cntR <= 0
>                                          = error $ unwords [fName, "cell is nonsense"]
>       | isNothing foundL || isNothing foundR
>                                          =
>         error $ unwords [fName
>                        , "zone"
>                        , show (bagIdL, bagIdR)
>                        , "not both present in"
>                        , show (map (pzWordB . fst) perI.pZones)] 
>       | foundL == foundR                 = (Left . fromJust) foundL
>       | otherwise                        = Right (fromJust foundL, fromJust foundR)
>       where
>         fName                            = unwords [fName_, "doFlyEye"]
>         trace_DFE                        = unwords [fName, show noonFly, show perI.pSmashing]
>
>         (index1, index2)                 = noonAsCoords noonFly
>         (bagIdL, cntL)                   = lookupCellIndex index1 perI.pSmashing
>         (bagIdR, cntR)                   = lookupCellIndex index2 perI.pSmashing
>         foundL                           = findByBagIndex' perI.pZones bagIdL
>         foundR                           = findByBagIndex' perI.pZones bagIdR

reconcile zone and sample header ======================================================================================

> reconLR                :: ((PreZone, SFZone), (PreZone, SFZone))
>                           → (NoteOn, NoteOn)
>                           → VB.Vector Double
>                           → Dur
>                           → (Recon, Maybe Recon)
> reconLR ((pzL, zoneL), (pzR, zoneR)) (noonL, noonR) ps durR
>                                          = (recL, Just recR')
>   where
>     secsScored         :: Double         = fromRational durR
>     recL@Recon{rRootKey = rkL, rPitchCorrection = pcL}
>                                          = makeRecon (pzL, zoneL) noonL ps secsScored
>     recR                                 = makeRecon (pzR, zoneR) noonR ps secsScored
>     recR'                                = recR{
>                                                rRootKey                   = rkL
>                                              , rPitchCorrection           = pcL}
>
> makeRecon              :: (PreZone, SFZone) → NoteOn → VB.Vector Double → Double → Recon
> makeRecon (pz, z_) noon ps secs
>   | traceIf trace_MR False               = undefined
>   | otherwise                            = reconL
>   where
>     fName                                = "makeRecon"
>     trace_MR                             = unwords [fName, shdr.sampleName, show z]
>
>     zd                                   = pz.pzDigest
>     shdr                                 = effPZShdr pz
>     
>     bend                                 = (getAnswers ps).aNoteBend
>     z                                    = if not (VB.null bend)
>                                              then implementNoteBending noon z_ (bend VB.! 0) secs
>                                              else z_
>     m8n                                  = reconModulation z noon
>
>     reconL = Recon {
>     rSampleMode    = fromMaybe           A.NoLoop           z.zSampleMode
>   , rSampleRate    = fromIntegral        shdr.sampleRate
>   , rStart         = (+)                 shdr.start         (fromIntegral zd.zdStart)
>   , rEnd           = (+)                 shdr.end           (fromIntegral zd.zdEnd)
>   , rLoopStart     = (+)                 shdr.startLoop     (fromIntegral zd.zdStartLoop)
>   , rLoopEnd       = (+)                 shdr.endLoop       (fromIntegral zd.zdEndLoop)
>   , rRootKey       = fromIntegral $ fromMaybe
>                                          shdr.originalPitch z.zRootKey
>   , rTuning        = fromMaybe           100                z.zScaleTuning
>   , rDynamics      = ps
>   , rNoteOn        = noon
>   , rAttenuation   = reconAttenuation                       z.zInitAtten
>   , rVolEnv        = deriveEnvelope                         z.zDelayVolEnv
>                                                             z.zAttackVolEnv
>                                                             noon
>                                                             (z.zHoldVolEnv,  z.zKeyToVolEnvHold)
>                                                             (z.zDecayVolEnv, z.zKeyToVolEnvDecay)
>                                                             z.zSustainVolEnv
>                                                             Nothing
>   , rPitchCorrection
>                    = if usePitchCorrection
>                        then Just $ reconPitchCorrection     shdr.pitchCorrection
>                                                             z.zCoarseTune
>                                                             z.zFineTune
>                        else Nothing
>
>   , rM8n           =                                        m8n
>   , rEffects       = deriveEffects                          m8n
>                                                             noon
>                                                             z.zChorus
>                                                             z.zReverb
>                                                             z.zPan}
>
>     reconPitchCorrection
>                        :: Int → Maybe Int → Maybe Int → Double
>     reconPitchCorrection sub mps mpc     = fromMaybe ((fromCents . fromIntegral) sub) (fromCents' mps mpc)
>
>     reconAttenuation   :: Maybe Int → Double
>     reconAttenuation _     {- WOX -}       = if useAttenuation
>                                              then maybe 0 fromIntegral z.zInitAtten
>                                              else 0.0
>
> implementNoteBending   :: NoteOn → SFZone → Double → Double → SFZone
> implementNoteBending noon zone bend secs = zone'
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
> reconModulation        :: SFZone → NoteOn → Modulation
> reconModulation z noon                   = resolveMods m8n z.zModulators defaultMods
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
>     curKernelSpec                        =
>       KernelSpec
>         (fromMaybe 13_500 z.zInitFc)
>         (fromMaybe 0 z.zInitQ)
>         1 
>         useFastFourier
>         (-1) -- must always be replaced
>
>     resonanceType      :: ResonanceType  = ResonanceSVF
>     nModEnv            :: Maybe FEnvelope
>     nModEnv                              = deriveEnvelope
>                                              z.zDelayModEnv
>                                              z.zAttackModEnv
>                                              noon
>                                              (z.zHoldModEnv, z.zKeyToModEnvHold) 
>                                              (z.zDecayModEnv, z.zKeyToModEnvDecay)
>                                              z.zSustainModEnv
>                                              (Just (z.zModEnvToPitch, z.zModEnvToFc))
>     nModLfo, nVibLfo   :: Maybe LFO
>     nModLfo                              =
>       deriveLFO z.zDelayModLfo z.zFreqModLfo z.zModLfoToPitch z.zModLfoToFc z.zModLfoToVol
>     nVibLfo            :: Maybe LFO      =
>       deriveLFO z.zDelayVibLfo z.zFreqVibLfo z.zVibLfoToPitch Nothing     Nothing
>
>     summarize          :: ModDestType → ModCoefficients
>     summarize toWhich                    =
>       ModCoefficients
>         (coAccess toWhich $ maybe defModTriple (fromJust . fModTriple) nModEnv)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nModLfo)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nVibLfo)

The End
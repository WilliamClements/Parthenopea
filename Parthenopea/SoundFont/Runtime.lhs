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
> {-# LANGUAGE TypeFamilies #-} 
> {-# LANGUAGE UnicodeSyntax #-}

Runtime
William Clements
February 1, 2025

> module Parthenopea.SoundFont.Runtime
>        (  computeCross
>         , bootNRender
>         )
>         where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Either
> import Data.Foldable ( toList )
> import Data.List hiding (insert)
> import Data.Map (Map)
> import Data.Map.Strict (insertWith)
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
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Synthesizer
> import Parthenopea.Music.Siren
> import Parthenopea.SoundFont.Boot
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec

executive =============================================================================================================

> bootNRender            :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> bootNRender songs                        = do
>   tsStarted                              ← getCurrentTime
>
>   rost                                   ← qualifyKinds songs
>   mbundle                                ← equipInstruments rost
>
>   if isNothing mbundle
>     then do
>       return ()
>     else do 
>       let (prerunt, matches, pergmsI, pergmsP, rd)
>                                          = fromJust mbundle
>       runt                               ← finishRuntime matches rost prerunt pergmsI pergmsP rd
>
>       CM.when doRender (doRendering runt)
>
>       tsRendered                         ← getCurrentTime
>       putStrLn ("___overall: " ++ show (diffUTCTime tsRendered tsStarted))
>   where
>     -- track the complete _qualified_ populations of: samples, instruments, percussion
>     finishRuntime      ::  Matches
>                            → ([InstrumentName], [PercussionSound])
>                            → SFRuntime
>                            → [PerGMKey] → [PerGMKey]
>                            → ResultDispositions
>                            → IO SFRuntime
>     finishRuntime matches rost prerunt@SFRuntime{ .. } pergmsI pergmsP rdGen03
>                                          = do
>       tsStarted                          ← getCurrentTime
>
>       tsZoned                            ← getCurrentTime
>       putStrLn ("___cache zones: " ++ show (diffUTCTime tsZoned tsStarted))
>
>       CM.when (howVerboseScanReport > (1/10)) (writeScanReport prerunt rdGen03)
>       tsScanned                          ← getCurrentTime
>      
>       -- actually conduct the tournament
>       ((wI, sI), (wP, sP))
>                                          ← decideWinners zBoot.zPreSampleCache zBoot.zPreInstCache zBoot.zOwners
>                                                          zBoot.zPerInstCache matches rost pergmsI pergmsP
>       tsDecided                          ← getCurrentTime
>       putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsScanned))
>
>       CM.when (howVerboseTournamentReport > (1/10)) (writeTournamentReport prerunt.zFiles wI wP)
>       tsReported                         ← getCurrentTime
>
>       -- print song/orchestration info to user (can be captured by redirecting standard out)
>       mapM_ putStrLn (sI ++ sP)
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
>       mapM_ (uncurry (renderSong runt imap)) songs
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
>   let esSampleScan                       = procMap preSampleDispos ++ [EndOfLine]
>   let esInstScan                         = procMap preInstDispos ++ [EndOfLine]
>   let esTail                             = [EndOfLine, EndOfLine]
>
>   writeFileBySections
>     reportScanName
>     ([esTimeStamp, esSampleSummary, esInstSummary]
>      ++ if howVerboseScanReport < (1/3) then [] else [esSampleScan, esInstScan]
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
>         histo          :: [((Disposition, Impact, String), Int)]
>         histo                            =
>           sortOn (Down . snd) $ Map.toList $ Map.foldr (\ss → insertWith (+) (autopsy ss) 1) Map.empty sm
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
>         concatMap emitHisto histo
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
>            , Blanks 5
>            , Unblocked (show (kname k sffile))]
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
>     emitFileListC      = concatMap (uncurry doF) (zip ([0..]::[Int]) (toList sffiles))
>     doF nth sffile     = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>
> writeFileBySections    :: FilePath → [[Emission]] → IO ()
> writeFileBySections fp eSections         = do
>   mapM_ (appendFile fp . reapEmissions) eSections
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
> renderSong             :: ∀ p . Clock p ⇒
>                           SFRuntime
>                           → InstrMap (Stereo p)
>                           → FilePath
>                           → (DynMap → Music (Pitch, [NoteAttribute]))
>                           → IO ()
> renderSong runt imap name song           =
>   do
>     traceIO ("renderSong " ++ name)
>     ts1                                  ← getCurrentTime
>     ding                                 ← shredMusic (song Map.empty)
>     let dynMap                           = makeDynMap ding
>     CM.unless (null (Map.assocs dynMap)) (traceIO $ unwords ["dynMap", show dynMap])
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
>         let (durS,s)                        = renderSF (song dynMap) imap
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
> prepareInstruments runt@SFRuntime{zWinningRecord}
>                                          = 
>     return $ (Percussion, assignPercussion)                                                      : imap
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
> instrumentSF SFRuntime{ .. } pergm durI pchIn volIn nps
>   | traceNot trace_ISF False             = undefined
>   | otherwise                            = eutSynthesize (reconX, mreconX) reconX.rSampleRate
>                                              durI pchOut volOut nps
>                                              samplea.ssData samplea.ssM24
>   where
>     fName_                               = "instrumentSF"
>     noon                                 = NoteOn
>                                              (clip (0, 127) volIn)
>                                              (clip (0, 127) pchIn)
>     pchOut              :: AbsPitch      = maybe noon.noteOnKey (clip (0, 127)) reconX.rForceKey
>     volOut              :: Volume        = maybe noon.noteOnVel (clip (0, 127)) reconX.rForceVel
>
>     sffile                               = zFiles ! pergm.pgkwFile
>     samplea                              = sffile.zSample
>
>     preI                                 = zBoot.zPreInstCache Map.! pergm
>     perI                                 = zBoot.zPerInstCache Map.! pergm
>
>     trace_ISF                            =
>       unwords [fName_, show pergm.pgkwFile, show preI.pInst, show (pchIn, volIn), show durI]
>
>     (reconX, mreconX)                    =
>       case setZone of
>         Left zplus                       → (recon zplus noon nps (fromRational durI), Nothing)
>         Right zsPlus                     → reconLR zsPlus noon nps (fromRational durI)

zone selection for rendering ==========================================================================================

>     setZone            :: Either (SFZone, F.Shdr) ((SFZone, F.Shdr), (SFZone, F.Shdr))
>     setZone                              =
>       case selectZoneConfig selectBestZone of 
>         Left (pzL, zoneL)                → Left (zoneL, shdr pzL)
>         Right ((pzL, zoneL), (pzR, zoneR))
>                                          → Right ((zoneL, shdr pzL), (zoneR, shdr pzR))
>       where
>         shdr                             = effShdr zBoot.zPreSampleCache
>
>     selectBestZone     :: (PreZone, SFZone)
>     selectBestZone
>       | traceNot trace_SBZ False         = undefined
>       | otherwise                        =
>       if cnt == 0
>         then error "out of range"
>         else if isNothing foundInInst
>                then error $ unwords[fName, show bagId, "not found in inst", preI.iName, showBags perI]
>                else deJust "foundInInst" foundInInst
>       where
>         fName                            = unwords [fName_, "selectBestZone"]
>
>         (bagId, cnt)                     = lookupCellIndex (noonAsCoords noon) perI.pSmashing
>         foundInInst                      = findByBagIndex' perI.pZones bagId
>
>         trace_SBZ                        = unwords ["selectBestZone", show (bagId, cnt)]
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
>          shdr                            = (effShdr zBoot.zPreSampleCache . fst) z
>          stype                           = toSampleType shdr.sampleType
>          oz                              = deJust "oz" mpartner
>
>     getStereoPartner   :: (PreZone, SFZone) → Maybe (PreZone, SFZone)
>     getStereoPartner (pz, _)
>       | traceNow trace_GSP False         = undefined
>       | otherwise                        =
>       case toSampleType (F.sampleType shdr) of
>         SampleTypeLeft                   → partner
>         SampleTypeRight                  → partner
>         _                                → error $ unwords [fName, "attempted on non-stereo zone"]
>       where
>         fName                            = unwords [fName_, "getStereoPartner"]
>
>         shdr                             = effShdr zBoot.zPreSampleCache pz
>         partnerKeys                      = pzmkPartners pz
>
>         trace_GSP                        = unwords [fName, showable, showPreZones (singleton pz)]
>         showable                         =
>           case partner of
>             Just (pzc, _)                → show pzc.pzWordB
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
>             evalCand _                   = Nothing -- WOX
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
> reconLR ((zoneL, shdrL), (zoneR, shdrR)) noon nps durR
>   | traceNever trace_RLR False           = undefined
>   | otherwise                            = (recL, Just recR')
>   where
>     secsScored         :: Double         = fromRational durR
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
> reconModulation        :: SFZone → F.Shdr → NoteOn → [Double] → Double → Modulation
> reconModulation SFZone{ .. } shdr noon nps _
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

The End
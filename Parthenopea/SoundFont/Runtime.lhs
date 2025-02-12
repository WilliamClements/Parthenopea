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
>         , doEverything
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
  
importing sampled sound (from SoundFont (*.sf2) files) ================================================================

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
> theGrader              :: Grader
> theGrader                                = Grader ssWeights 500

executive =============================================================================================================

> doEverything           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> doEverything songs     = do
>   putStrLn "everything..."
>   putStrLn ""
>
>   rost                 ← qualifyKinds songs
>   putStrLn $ unwords ["rost", show rost]
>   putStrLn ""
>
>   tsStarted            ← getCurrentTime
>
>   (mrunt, mmatches, pergmsI, pergmsP, rdGen03)
>                        ← equipInstruments rost
>
>   if isNothing mrunt
>     then do
>       return ()
>     else do 
>       let prerunt      = deJust "mrunt" mrunt
>       let matches      = deJust "mmatches" mmatches
>       -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>       runt             ← finishRuntime matches rost prerunt pergmsI pergmsP rdGen03
>
>       CM.when doRender (doRendering runt)
>
>       tsRendered       ← getCurrentTime
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
>                        = do
>       tsStarted        ← getCurrentTime
>
>       tsZoned          ← getCurrentTime
>       putStrLn ("___cache zones: " ++ show (diffUTCTime tsZoned tsStarted))
>
>       CM.when reportScan (writeScanReport prerunt rdGen03)
>       tsScanned        ← getCurrentTime
>      
>       -- actually conduct the tournament
>       ((wI, sI), (wP, sP))
>                        ← decideWinners zBoot.zPreSampleCache zBoot.zPreInstCache zBoot.zOwners
>                                        zBoot.zPerInstCache matches rost pergmsI pergmsP
>       tsDecided        ← getCurrentTime
>       putStrLn ("___decide winners: " ++ show (diffUTCTime tsDecided tsScanned))
>
>       CM.when reportTourney (writeTournamentReport prerunt.zFiles wI wP)
>       tsReported       ← getCurrentTime
>
>       -- print song/orchestration info to user (can be captured by redirecting standard out)
>       mapM_ putStrLn (sI ++ sP)
>
>       let wins         = WinningRecord (Map.map head wI) (Map.map head wP)
>
>       tsRecond         ← getCurrentTime
>       putStrLn ("___create winning record: " ++ show (diffUTCTime tsRecond tsReported))
>         
>       return prerunt{
>         zWinningRecord = wins}
>
>     -- get it on
>     doRendering      :: SFRuntime → IO ()
>     doRendering runt
>                        = do
>       tsStarted        ← getCurrentTime
>
>       -- readying instrument maps to be accessed from song renderer
>       traceIO          "prepareInstruments"
>       imap             ← prepareInstruments runt
>       tsPrepared       ← getCurrentTime
>       putStrLn ("___prepare instruments: " ++ show (diffUTCTime tsPrepared tsStarted))
>
>       -- here's the heart of the coconut
>       mapM_ (uncurry (renderSong runt imap)) songs
>
>       tsRendered       ← getCurrentTime
>       putStrLn ("___render songs: "        ++ show (diffUTCTime tsRendered tsPrepared))
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

tournament starts here ================================================================================================

> decideWinners          :: Map PreSampleKey PreSample
>                           → Map PerGMKey PreInstrument
>                           → Map PerGMKey [PreZone]
>                           → Map PerGMKey PerInstrument
>                           → Matches
>                           → ([InstrumentName], [PercussionSound]) 
>                           → [PerGMKey]
>                           → [PerGMKey]
>                           → IO ((Map InstrumentName [PerGMScored], [String])
>                               , (Map PercussionSound [PerGMScored], [String]))
> decideWinners preSampleCache preInstCache owners zc matches rost pergmsI pergmsP
>                                          = do
>   traceIO ("decideWinners" ++ show (length zc))
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
>                                          = foldl' (xaEnterTournament fuzzMap pergmI []) target i2Fuzz'
>       where
>         -- access potentially massive amount of processed information regarding instrument
>         preI                             = deJust (unwords["wiFolder", "preI"]) (Map.lookup pergmI preInstCache)
>         iMatches                         = deJust "mIMatches" (Map.lookup pergmI matches.mIMatches)
>         fuzzMap                          = getFuzzMap iMatches
>
>         i2Fuzz         :: Map InstrumentName Fuzz
>         i2Fuzz                           =
>           Map.filterWithKey (\k _ → k `elem` select rost) fuzzMap
>
>         i2Fuzz'         :: [InstrumentName]
>         i2Fuzz'                          =
>           profess
>             (not $ null i2Fuzz)
>             (unwords ["unexpected empty matches for", show pgkwFile, preI.iName]) 
>             (if multipleCompetes
>                then Map.keys i2Fuzz
>                else (singleton . fst) (Map.findMax i2Fuzz))
>     
>     wpFolder           :: (Map PercussionSound [PerGMScored], [String])
>                           → PerGMKey
>                           → (Map PercussionSound [PerGMScored], [String])
>     wpFolder wIn pergmP@PerGMKey{pgkwFile, pgkwBag}
>       | traceIf trace_WP False           = undefined
>       | otherwise                        = xaEnterTournament fuzzMap pergmP [] wIn kind
>       where
>         trace_WP                         =
>           unwords ["wpFolder", show preI.iName, show pergmP, "of", show (length perI.pZones)]
>
>         preI                             =
>           deJust (unwords["wpFolder", "PreInst"]) (Map.lookup (pergmP{pgkwBag = Nothing}) preInstCache)
>         pzs                              = deJust "pzs" (Map.lookup pergmP{pgkwBag = Nothing} owners)
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
>              >>= (`Map.lookup` matches.mSMatches)
>         fuzzMap        :: Map PercussionSound Fuzz
>         fuzzMap                          = getFuzzMap $ deJust (unwords ["mffm"]) mffm
>
>         getAP          :: PreZone → Maybe AbsPitch
>         getAP pz                         = pz.pzDigest.zdKeyRange >>= (Just . fromIntegral . fst)
>
>     xaEnterTournament  :: ∀ a. (Ord a, Show a, SFScorable a) ⇒
>                           Map a Fuzz
>                           → PerGMKey
>                           → [SSHint]
>                           → (Map a [PerGMScored], [String])
>                           → a
>                           → (Map a [PerGMScored], [String])
>     xaEnterTournament fuzzMap pergm hints (wins, ss) kind
>       | traceIf trace_XAET False         = undefined
>       | otherwise                        = (Map.insertWith (++) kind [scored] wins, ss)
>       where
>         fName                            = "xaEnterTournament"
>
>         pergm_                           = pergm{pgkwBag = Nothing}
>         preI                             =
>           deJust (unwords[fName, "PreInstrument", show pergm_]) (Map.lookup pergm_ preInstCache)
>         perI                             =
>           deJust (unwords[fName, "PerInstrument", show pergm_]) (Map.lookup pergm_ zc)
>
>         scope_, scope  :: [(PreZone, SFZone)]
>         scope_                           =
>           case pergm.pgkwBag of
>             Nothing                      → perI.pZones
>             Just bagI                    →
>                    maybe
>                      (error $ unwords [fName, "findByBagIndex' returned a Nothing for"
>                                       , show pergm.pgkwFile, preI.iName, show bagI])
>                      singleton
>                      (findByBagIndex' perI.pZones bagI)
>         scope                            =
>           profess
>             (not (null scope_))
>             (unwords[fName, "null scope", preI.iName])
>             scope_
>             
>         mnameZ         :: Maybe String   = pergm.pgkwBag
>                                            >>= findByBagIndex' perI.pZones
>                                            >>= \(q, _) → Just (F.sampleName (effShdr preSampleCache q))
>
>         trace_XAET                       =
>           unwords [fName, preI.iName, fromMaybe "" mnameZ, show kind]
>
>         computeGrade   :: [(PreZone, SFZone)] → ArtifactGrade
>         computeGrade zs                  = gradeEmpiricals theGrader empiricals
>           where
>             empiricals :: [Double]       = [   foldHints hints
>                                              , fromRational $ scoreBool $ isStereoInst preSampleCache zs
>                                              , fromRational $ scoreBool $ is24BitInst preSampleCache zs
>                                              , computeResolution zs
>                                              , fromRational $ scoreBool $ all (zoneConforms preSampleCache) zs
>                                              , fuzz]
>             howgood                      = akResult - stands
>             fuzz       :: Double
>               | howgood > 0.000_001      = max 0 (logBase 2 howgood) * fuzzFactor kind
>               | otherwise                = 0
>   
>         scored         :: PerGMScored    =
>           PerGMScored (computeGrade scope) (toGMKind kind) akResult pergm preI.iName mnameZ
>
>         computeResolution
>                        :: [(PreZone, SFZone)]
>                           → Double
>         computeResolution zs
>           | null zs                      = error $ unwords ["null zs"]
>           | otherwise                    = fromRational m1 * evalSplits kind + fromRational m2 * evalSampleSize
>           where
>             theSplit                     = splitScore kind (map fst zs)
>             evalSplits _
>               | theSplit <= 1            = 1
>               | otherwise                = log (m3 * theSplit)
>             evalSampleSize               = sum (map durScoring zs) / fromIntegral (length zs)
>
>             m1                           = 1/2
>             m2                           = 1/2
>             m3                           = 3 * if isStereoInst preSampleCache zs then 1/2 else 1
>
>         durScoring     :: (PreZone, SFZone) → Double
>         durScoring (pz, zone) = if score < 0.01 then -10 else score
>           where
>             shdr                         = effShdr preSampleCache pz
>             score                        = sampleSize / fromIntegral shdr.sampleRate
>
>             sampleSize :: Double
>             sampleSize                   = fromIntegral $ xEnd - xStart
>               where
>                 xStart                   =
>                   addIntToWord    shdr.start
>                                   (sumOfWeightedInts [zone.zStartOffs, zone.zStartCoarseOffs] qOffsetWeights)
>                 xEnd                     =
>                   addIntToWord    shdr.end
>                                   (sumOfWeightedInts [zone.zEndOffs,   zone.zEndCoarseOffs]   qOffsetWeights)
>
>         akResult                         = fromMaybe 0 (Map.lookup kind fuzzMap)
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
>
> zoneConforms :: Map PreSampleKey PreSample → (PreZone, SFZone) → Bool
> zoneConforms preSampleCache (pz, zone)   = not $ or unsupported
>   where
>     F.Shdr{end, start, endLoop, startLoop}
>                                          = effShdr preSampleCache pz
>
>     unsupported        :: [Bool]
>     unsupported                          =
>       [
>           case zone.zSampleMode of
>             Nothing                      → False
>             Just n                       → n == A.PressLoop
>         , case zone.zInitQ of
>             Nothing                      → False
>             Just n                       → n /= 0
>         , case zone.zScaleTuning of
>             Nothing                      → False
>             Just n                       → n /= 0 -- && n /= 100
>         , case zone.zExclusiveClass of
>             Nothing                      → False
>             Just n                       → n /= 0
>         , end < start
>         , endLoop < startLoop
>       ]
 
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
>     preI                                 = deJust (unwords [fName_, "preI"]) (Map.lookup pergm zBoot.zPreInstCache)
>     perI                                 = deJust (unwords [fName_, "perI"]) (Map.lookup pergm zBoot.zPerInstCache)
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
>       | traceNot trace_GSP False         = undefined
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

emit standard output text detailing what choices we made for rendering GM items =======================================

> printChoices           :: SFRuntime
>                           → [InstrumentName]
>                           → [(InstrumentName, [String])]
>                           → [PercussionSound]
>                           → ([(Bool, [Emission])], [(Bool, [Emission])])
> printChoices SFRuntime{zWinningRecord} is msgs ps
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
> showPerGM              :: PerGMScored → [Emission]
> showPerGM PerGMScored{szI, mszP, pPerGMKey}
>                                          = [emitShowL pgkwFile 4] ++ [ToFieldL szI 22] ++ showmZ
>   where
>     PerGMKey{pgkwFile}                   = pPerGMKey
>     showmZ                               = maybe [] showZ mszP
>     showZ name                           = [Unblocked name]
>
> dumpContestants        :: ∀ a. (Ord a, Show a, SFScorable a) ⇒ (a, [PerGMScored]) → [Emission]
> dumpContestants (kind, contestants)      = prolog ++ ex ++ epilog
>   where
>     prolog, ex, epilog :: [Emission]
>
>     prolog                               = emitLine [emitShowL kind 50]
>     ex                                   = concatMap dumpContestant contestants
>     epilog                               = emitLine []
>
> dumpContestant         :: PerGMScored → [Emission]
> dumpContestant PerGMScored{ .. }
>                                          = ex
>   where
>     ArtifactGrade{pEmpiricals, pScore}   = pArtifactGrade
>     PerGMKey{pgkwFile}                   = pPerGMKey
>     showAkr            :: Double         = roundBy 10 pAgainstKindResult
>     (showEmp, n)                         = showEmpiricals pEmpiricals
> 
>     ex = emitLine [ Blanks 4, emitShowL      pgkwFile                  8
>                             , ToFieldR       szI                      22
>                   , Blanks 4, ToFieldR      (fromMaybe "" mszP)       22
>                   , Blanks 4, emitShowL      pScore                   15
>                             , ToFieldL       showEmp                   n
>                             , emitShowR      showAkr                   8]

The End
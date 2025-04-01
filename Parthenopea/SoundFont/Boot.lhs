> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-} 
> {-# LANGUAGE UnicodeSyntax #-}

Boot
William Clements
January 21, 2025

> module Parthenopea.SoundFont.Boot ( equipInstruments ) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Either
> import Data.Foldable
> import Data.List hiding (insert)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import Debug.Trace
> import Euterpea.IO.MIDI.GeneralMidi()
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import qualified System.FilePattern.Directory
>                                          as FP
  
importing sampled sound (from SoundFont (*.sf2) files) ================================================================

> data FileWork =
>   FileWork {
>     fwBoot             :: SFBoot
>   , fwZRecs            :: [InstZoneRecord]
>   , fwMatches          :: Matches
>   , fwDispositions     :: ResultDispositions}
> instance Show FileWork where
>   show (FileWork{ .. })                  =
>     unwords [  "FileWork"
>              , show fwBoot
>              , show (length fwZRecs), "=zrecs"
>              , show fwDispositions]
> defFileWork            :: FileWork
> defFileWork                              = FileWork dasBoot [] defMatches virginrd
>
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
> instance Show FileIterate where
>   show (FileIterate{ .. })               =
>     unwords ["FileIterate", show fiFw]
>
> preSampleTaskIf, preInstTaskIf, surveyTaskIf, captureTaskIf, markTaskIf
>                , partner1TaskIf, partner2TaskIf, partner3TaskIf, partner4TaskIf, reorgTaskIf
>                , shaveTaskIf, matchTaskIf, catTaskIf
>                , rejobsTaskIf, zoneTaskIf, repartnerTaskIf
>                        :: SFFile → ([InstrumentName], [PercussionSound]) → FileWork → FileWork
>
> makeFileIterate        :: SFFile → ([InstrumentName], [PercussionSound]) → FileIterate
> makeFileIterate sffile rost              =
>   FileIterate
>     defFileWork 
>     [  ("preSample",  preSample)
>      , ("preInst",    preInst)
>      , ("capture",    mark . capture . survey)
>      , ("partner1",   partner1)
>      , ("reorg",      reorg) 
>      , ("partner2",   partner2)
>      , ("partner3",   partner3)
>      , ("partner4",   partner4)
>      , ("shave",      shave)
>      , ("match",      match)
>      , ("cat",        rejobs . cat)
>      , ("zone",       repartner . zone)]
>   where
>     mark                                 = markTaskIf         sffile rost
>     rejobs                               = rejobsTaskIf       sffile rost
>     repartner                            = repartnerTaskIf    sffile rost
>
>     preSample                            = preSampleTaskIf    sffile rost
>     partner1                             = partner1TaskIf     sffile rost
>     partner2                             = partner2TaskIf     sffile rost
>     partner3                             = partner3TaskIf     sffile rost
>     partner4                             = partner4TaskIf     sffile rost
>     preInst                              = preInstTaskIf      sffile rost
>     survey                               = surveyTaskIf       sffile rost
>     capture                              = captureTaskIf      sffile rost
>     reorg                                = reorgTaskIf        sffile rost
>     shave                                = shaveTaskIf        sffile rost
>     match                                = matchTaskIf        sffile rost
>     cat                                  = catTaskIf          sffile rost
>     zone                                 = zoneTaskIf         sffile rost
>
> reduceFileIterate      :: FileIterate → (SFBoot, ResultDispositions, Matches)
> reduceFileIterate fiIn                   = (fwBoot, fwDispositions, fwMatches)
>   where
>     FileWork{ .. }                       = fiIn.fiFw

executive =============================================================================================================

To support extracting from flawed SoundFont files, we - up front - withdraw unrecoverable items from their
respective collections. An item's presence may be critical to some instrumentation. So it entails further deletion
and recovery.

> equipInstruments       :: ([InstrumentName], [PercussionSound])
>                           → IO (Maybe (SFRuntime, Matches, ResultDispositions))
> equipInstruments rost                    = do
>   putStrLn ""
>   putStrLn $ unwords [fName, "rost", show rost]
>   putStrLn ""
>
>   tsStarted                              ← getCurrentTime
>
>   -- represent all input SoundFont files in ordered list, thence a vector
>   fps                                    ← FP.getDirectoryFiles "." (singleton "*.sf2")
>   if null fps
>     then do
>       putStrLn "no *.sf2 files found"
>       return Nothing
>     else do
>       sffilesp                           ← CM.zipWithM openSoundFontFile [0..] fps
>       let vFiles                         = listArray (0, fromIntegral (length fps - 1)) sffilesp
>
>       tsLoaded                           ← getCurrentTime
>       putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>       -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>       let (bootAll, rdGen03, matchesAll) = foldl' bootFolder (dasBoot, virginrd, defMatches) vFiles
>       CM.when diagnosticsEnabled (traceIO $ show bootAll)
>       let runt                           = SFRuntime vFiles bootAll seedWinningRecord
>
>       tsBooted                           ← getCurrentTime
>       putStrLn ("___booted: " ++ show (diffUTCTime tsBooted tsLoaded))
>
>       return (Just (runt, matchesAll, rdGen03))
>   where
>     fName = "equipInstruments"
>
>     bootFolder (bootIn, rdIn, matchesIn) sffile
>                                          = 
>       let
>         (bootOut, rdOut, matchesOut)     = reduceFileIterate (ingestFile sffile)     
>       in
>         (combineBoot bootIn bootOut, combinerd rdIn rdOut, combineMatches matchesIn matchesOut)
>
>     ingestFile sffile                    =
>       let
>         unfinished fiIn                  = not (null fiIn.fiTaskIfs)
>         nextGen fiIn@FileIterate{ .. }   = notracer name fiIn{ fiFw = userFun fiFw, fiTaskIfs = tail fiTaskIfs}
>           where
>             (name, userFun)              = head fiTaskIfs
>       in
>         head $ dropWhile unfinished (iterate nextGen (makeFileIterate sffile rost))
>
> openSoundFontFile      :: Word → FilePath → IO SFFile
> openSoundFontFile wFile filename = do
>   putStr (unwords [show wFile, filename])
>   ts1                                    ← getCurrentTime
>   result                                 ← F.importFile filename
>   case result of
>     Left s                               →
>       error $ unwords ["openSoundFontFile", "decoding error", s, show filename]
>     Right soundFont                      → do
>       let pdata                          = F.pdta soundFont
>       let sdata                          = F.sdta soundFont
>       let boota                          =
>             FileArrays
>               (F.insts pdata) (F.ibags pdata)
>               (F.igens pdata) (F.imods pdata)
>               (F.shdrs pdata)
>       let samplea                        = SampleArrays (F.smpl  sdata) (F.sm24  sdata)
>       let sffile                         = SFFile wFile filename boota samplea
>       let nBits::Word                      =
>             case samplea.ssM24 of
>               Nothing                    → 16
>               Just _                     → 24
>       ts2                                ← getCurrentTime
>       CM.when diagnosticsEnabled (
>         putStr $ unwords ["lengths insts,bags,gens,mods,shdrs:"
>                         , show $ length boota.ssInsts
>                         , show $ length boota.ssIBags
>                         , show $ length boota.ssIGens
>                         , show $ length boota.ssIMods
>                         , show $ length boota.ssShdrs ])
>       putStrLn (unwords ["(", show nBits, ") loaded in", show (diffUTCTime ts2 ts1)])
>       return sffile
>
> checkSmashing          :: Smashing Word → Maybe Impact
> checkSmashing smashup
>   | not ok1                              = Just UndercoveredRanges
>   | not ok2                              = Just OverCoveredRanges
>   | otherwise                            = Nothing
>   where
>     ok1                                  = allowOutOfRange || smashup.smashStats.countNothings == 0
>     ok2                                  = allowOverlappingRanges || smashup.smashStats.countMultiples == 0
>
> computeInstSmashup     :: [PreZone] → Smashing Word
> computeInstSmashup pzs
>   | traceNot trace_CIS False             = undefined
>   | otherwise                            = computeSmashup fName subs
>   where
>     fName                                = "computeInstSmashup"
>
>     -- create smashup consisting of 16_384 (128 x 128) word pairs - (131_072 bytes)
>     subs               :: [(Word, [Maybe (Word, Word)])]
>     subs                                 = map extractSpace pzs
>
>     trace_CIS                            = unwords [fName, showPreZones pzs, show subs]
>
> {- WOX
> lookupStereoIndex        :: ∀ i . (Integral i, Ix i, Show i) ⇒ [i] → Smashing i → (i, i)
> lookupStereoIndex coords                 = lookupCellIndex (1 : coords)
> -}
>
> smush                  :: [([PreZone], Smashing Word)] → Smashing Word
> smush pears                              = smashSubspaces allTags dims allSpaces
>   where
>     allTags            :: String
>     allSpaces          :: [(Word, [Maybe (Word, Word)])]
>     (allTags, allSpaces)                 =
>       foldl' (\(at, ax) (pzs, smashup) → (at ++ smashup.smashTag, ax ++ map extractSpace pzs)) ([], []) pears
>     dims                                 = [fromIntegral qMidiSize128, fromIntegral qMidiSize128]
>
> extractSpace           :: PreZone → (Word, [Maybe (Word, Word)])
> extractSpace pz                          = (pz.pzWordB, [pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange])

support sample and instance ===========================================================================================

> formComprehension      :: ∀ r a . SFResource r ⇒ SFFile → (FileArrays → Array Word a) → [r]
> formComprehension sffile blobfun         = map (sfkey sffile.zWordF) bRange
>   where
>     fName                                = "formComprehension"
>
>     (stF, enF)                           = bounds $ blobfun sffile.zFileArrays
>     bRange                               =
>       profess
>         ((stF == 0) && (stF <= enF) && (enF < 2_147_483_648))
>         (error $ unwords [fName, "corrupt blob indexing"])
>         (deriveRange stF enF)

pre-sample task =======================================================================================================
          critique all Sample records in the file

> preSampleTaskIf sffile _ fwIn            = foldl' sampleFolder fwIn (formComprehension sffile ssShdrs)
>   where
>     sampleFolder       :: FileWork → PreSampleKey → FileWork
>     sampleFolder fwForm@FileWork{ .. } presk
>                                          = fwForm{ fwBoot = fwBoot', fwDispositions = dispose presk ss fwDispositions}
>       where
>         fName                            = "sampleFolder"
>
>         fwBoot'                          =
>           if dead ss
>             then fwBoot
>             else fwBoot{zPreSampleCache = Map.insert presk ps fwBoot.zPreSampleCache}
>         ps                               = ChangeName shdr changes name
>         shdr                             = sffile.zFileArrays.ssShdrs ! presk.pskwSampleIndex
>
>         raw                              = shdr.sampleName
>         good                             = fixName raw
>
>         ss_
>           | not (goodSampleRate shdr.sampleRate)
>                                          = [Scan Violated BadSampleRate fName (show shdr.sampleRate)]
>           | isNothing (toMaybeSampleType shdr.sampleType)
>                                          = [Scan Violated BadSampleType fName (show shdr.sampleType)]
>           | not (sampleSizeOk (shdr.start, shdr.end))
>                                          = [Scan Violated BadSampleLimits fName (show (shdr.start, shdr.end))]
>           | not (goodName raw)           = badButMaybeFix fixBadNames CorruptName fName raw good
>           | otherwise                    = [Scan Accepted Ok fName (show presk.pskwSampleIndex)]
>
>         (ss, changes, name)              = if wasRescued CorruptName ss_
>                                              then (ss_, singleton FixCorruptName, good)
>                                              else (ss_, [],                       raw)

pre-instance task =====================================================================================================
          access and critique all Instrument "records" in the file 

> preInstTaskIf sffile _ fwIn@FileWork{ .. }
>                                          =
>   fwIn{  fwBoot = fwBoot{zPreInstCache = preInstCache}
>        , fwDispositions = rdFinal}
>   where
>     pergms                               = formComprehension sffile ssInsts
>     (preInstCache, rdFinal)              = foldl' preIFolder (Map.empty, fwDispositions) pergms
>
>     preIFolder         :: (Map PerGMKey PreInstrument, ResultDispositions)
>                           → PerGMKey
>                           → (Map PerGMKey PreInstrument, ResultDispositions)
>     preIFolder (m, rdFold) pergm@PerGMKey{ .. }
>                                          =
>       if iinst.instBagNdx <= jinst.instBagNdx
>         then (m', rd'')
>         else error $ unwords [fName, "corrupt instBagNdx"]
>       where
>         fName                            = "preIFolder"
>
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pgkwInst + 1}
>
>         raw                              = iinst.instName
>         good                             = fixName raw
>
>         m'                               =
>           if dead ss
>              then m 
>              else Map.insert pergm (PreInstrument (ChangeName iinst changes finalName) Nothing) m
>
>         ss
>           | iinst.instBagNdx == jinst.instBagNdx
>                                          = [Scan Violated NoZones fName (show iinst.instName)]
>           | not (goodName raw)           = badButMaybeFix fixBadNames CorruptName fName raw good
>           | otherwise                    = [Scan Accepted Ok fName (show pergm.pgkwInst)]
>
>         changes                          = if wasRescued CorruptName ss then singleton FixCorruptName else []
>         finalName                        = if wasRescued CorruptName ss then good else raw
>
>         rd''                             = dispose pergm ss rdFold   
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm                       = boota.ssInsts ! pergm.pgkwInst
>       where
>         boota                            = sffile.zFileArrays

PreZone administration ================================================================================================

> goodZRecs              :: ResultDispositions → [InstZoneRecord] → [InstZoneRecord]
> goodZRecs rdNow                          = filter (\x → not (deadrd (instKey x) rdNow))
>
> badZRecs               :: ResultDispositions → [InstZoneRecord] → [InstZoneRecord]
> badZRecs rdNow                           = filter (\x → deadrd (instKey x) rdNow)
>
> data InstZoneRecord                      =
>   InstZoneRecord {
>     zswFile            :: Word
>   , zswInst            :: Word
>   , zsInstCat          :: Maybe InstCat
>   , zsGlobalKey        :: Maybe PreZoneKey
>   , zsPreZones         :: [PreZone]}
> instance Show InstZoneRecord where
>   show (InstZoneRecord{ .. })            =
>     unwords ["InstZoneRecord", show (zswFile, zswInst), showMaybeInstCat zsInstCat, show zsGlobalKey]
> makeZRec               :: PerGMKey → InstZoneRecord
> makeZRec pergm                           = InstZoneRecord pergm.pgkwFile pergm.pgkwInst Nothing Nothing []
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             = PerGMKey zrec.zswFile zrec.zswInst Nothing       

survey task ===========================================================================================================
          instantiate zrecs

> surveyTaskIf _ _ fwIn@FileWork{ .. }     =
>   fwIn{fwZRecs = map makeZRec (Map.keys fwBoot.zPreInstCache)}

iterating InstZoneRecord list =========================================================================================

> zrecTask               :: (InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions))
>                           → FileWork → FileWork
> zrecTask userFun fwIn@FileWork{ .. }     = fwIn{fwZRecs = workedOn, fwDispositions = rd'}
>   where
>     (workedOn, rd')                      = foldl' taskRunner ([], fwDispositions) (goodZRecs fwDispositions fwZRecs)
>
>     taskRunner (zrecs, rdFold) zrec      =
>       let
>         (zrec', rdFold')                 = userFun zrec rdFold
>       in
>         (zrec': zrecs, rdFold')
>
> zrecCompute            :: ∀ a . FileWork → (a → InstZoneRecord → a) → a → a
> zrecCompute FileWork{ .. } userFun seed  = foldl' userFun seed (goodZRecs fwDispositions fwZRecs)
>
> zoneTask               :: (PreZone → Bool)
>                           → (PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions))
>                           → [PreZone] → ResultDispositions
>                           → ([PreZone], ResultDispositions)
> zoneTask zfilter zxform pzs rdIn         = (catMaybes mworkedOn ++ ignore, rd')
>   where
>     (workOn, ignore)                     = partition zfilter pzs
>    
>     mworkedOn          :: [Maybe PreZone]
>     (mworkedOn, rd')                     = foldl' taskFolder ([], rdIn) workOn
>
>     taskFolder         :: ([Maybe PreZone], ResultDispositions) → PreZone → ([Maybe PreZone], ResultDispositions)
>     taskFolder (mpzs, rdFold) pz         = (mpz : mpzs, rdFold')
>       where
>         (mpz, rdFold')                   = zxform pz rdFold
>
> zoneCompute            :: ∀ a . (PreZone → Bool) → (a → PreZone → a) → InstZoneRecord → a → a
> zoneCompute filterFun userFun zrec seed  = foldl' userFun seed (filter filterFun zrec.zsPreZones)

capture task ==========================================================================================================
          populate zrec with PreZones

> captureTaskIf sffile _ fwIn@FileWork{ .. }
>                                          = zrecTask capturer fwIn 
>   where
>     capturer           :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     capturer zrec rdIn                   =
>       let
>         (newPzs, rdOut, mglobalKey)      = captureZones (instKey zrec) rdIn
>       in
>         (zrec{zsPreZones = newPzs, zsGlobalKey = mglobalKey}, rdOut)
>
>     captureZones       :: PerGMKey → ResultDispositions → ([PreZone], ResultDispositions, Maybe PreZoneKey)
>     captureZones pergm rdCap             = (pzs', rdCap'', globalKey)
>       where
>         fName_                           = unwords["captureZones"]
>
>         preI                             = fwBoot.zPreInstCache Map.! pergm
>         iName                            = preI.piChanges.cnName
>         results                          = map captureZone (deriveRange ibagi jbagi)
>
>         pzs                              = lefts results
>         globalKey                        = if head results == Right (Accepted, GlobalZone)
>                                              then Just $ PreZoneKey sffile.zWordF pergm.pgkwInst ibagi 0
>                                              else Nothing
>
>         iinsts                           = sffile.zFileArrays.ssInsts
>         ibagi                            = F.instBagNdx (iinsts ! pgkwInst pergm)
>         jbagi                            = F.instBagNdx (iinsts ! (pgkwInst pergm + 1))
>
>         ss
>           | null pzs                     = [Scan Violated NoZones         fName_ noClue]
>           | illegalRange pzs             = [Scan Violated CorruptGMRange  fName_ noClue]
>           | illegalLinkage pzs           = [Scan Violated BadLinkage      fName_ noClue]
>           | any hasRom pzs               = [Scan Violated RomBased        fName_ noClue]
>           | not (all adjustedSampleSizeOk pzs)
>                                          = [Scan Violated BadSampleLimits fName_ noClue]
>           | otherwise                    = [Scan Accepted Adopted fName_ (show iName)]
>         rdCap'                           = dispose pergm ss rdCap
>         (pzs', rdCap'')                  = zoneTask (const True) capFolder pzs rdCap'
>
>         capFolder      :: PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions)
>         capFolder pz rdFold              =
>           let
>             impact                       = if wasSwitchedToMono pz
>                                              then AdoptedAsMono
>                                              else Adopted
>           in
>             (Just pz, dispose (extractSampleKey pz) [Scan Modified impact fName_ (show iName)] rdFold)
>             
>         captureZone    :: Word → Either PreZone (Disposition, Impact)
>         captureZone bix
>           | isNothing pz.pzDigest.zdSampleIndex
>                                          = Right (Accepted, GlobalZone)
>           | isNothing mpres              = Right (Dropped, OrphanedBySample)
>           | otherwise                    = Left pz{pzChanges = ChangeEar (effPSShdr pres) []}
>           where
>             fName                        = unwords [fName_, "captureZone"]
>                 
>             ibags                        = sffile.zFileArrays.ssIBags
>             xgeni                        = F.genNdx $ ibags ! bix
>             ygeni                        = F.genNdx $ ibags ! (bix + 1)
>             gens   :: [F.Generator]
>             gens                         = profess
>                                              (xgeni <= ygeni)
>                                              (unwords [fName, "SoundFont file corrupt (gens)"])
>                                              (map (sffile.zFileArrays.ssIGens !) (deriveRange xgeni ygeni))
>
>             pz                           = makePreZone sffile.zWordF si (pgkwInst pergm) bix gens pres.cnSource
>             si                           = deJust fName pz.pzDigest.zdSampleIndex
>             presk                        = PreSampleKey sffile.zWordF si
>             mpres                        = presk `Map.lookup` fwBoot.zPreSampleCache
>             pres                         = deJust fName mpres
>
>     illegalRange pzs                     =
>       let
>         zoneOk pz                        = okGMRange pz.pzDigest.zdKeyRange && okGMRange pz.pzDigest.zdVelRange
>
>         okGMRange      :: (Num a, Ord a) ⇒ Maybe (a, a) → Bool
>         okGMRange mrng                   =
>           case mrng of
>             Just (j, k)                  → (0 <= j) && j <= k && k < fromIntegral qMidiSize128
>             Nothing                      → True
>       in
>         not $ all zoneOk pzs
>
>     illegalLinkage pzs                   =
>       let
>         sList          :: [(Int, Int)]
>         sList                            = map (\z → (extractIndex z, extractLink z)) pzs
>
>         isSafe         :: ∀ a . (Eq a, Ord a, Show a) ⇒ [(a,a)] → Bool
>         isSafe pairs                     = closed && allPaired
>           where
>             uniquer    :: Map a a
>             uniquer                      =
>               foldl' (\target (k, t) → Map.insert k t target) Map.empty pairs
>
>             closed                       = all (\x → isJust (Map.lookup x uniquer)) uniquer
>             allPaired                    = all (paired uniquer) uniquer
>
>             paired     :: Map a a → a → Bool
>             paired target x              = x == z
>               where
>                 y                        = target Map.! x
>                 z                        = target Map.! y
>       in
>         requiredZoneLinkage >= 1 && not (isSafe sList)
>
>     extractIndex, extractLink
>                        :: PreZone → Int
>     extractIndex pz                      = fromIntegral $ deJust "extractIndex" pz.pzDigest.zdSampleIndex
>     extractLink pz                       = fromIntegral $ F.sampleLink (effPZShdr pz)
>
>     hasRom pz                            = F.sampleType (effPZShdr pz) >= 0x8000

mark task =============================================================================================================
          copy global zone markers from zrecs to preInstCache

> markTaskIf _ _ fwIn@FileWork{ .. }       = fwIn{fwBoot = fwBoot{zPreInstCache = preInstCache'}}
>   where
>     preInstCache'                        = zrecCompute fwIn markFolder fwBoot.zPreInstCache
>     markFolder preInstCache zrec         =
>       let
>         pergm                            = instKey zrec
>         preI                             = preInstCache Map.! pergm
>       in
>         Map.insert pergm (preI{iGlobalKey = zrec.zsGlobalKey}) preInstCache

partnering 1 task =====================================================================================================
          prepare for all possible stereo pairs

> partner1TaskIf _ _ fwIn                  = zrecTask partnerer1 fwIn
>   where
>     backMap            :: Map PreSampleKey [PreZoneKey]
>     backMap                              = zrecCompute fwIn backFolder Map.empty
>     backFolder q zrec                    = Map.union q (zoneCompute isStereoZone pzFolder zrec Map.empty)
>     pzFolder qFold pz                    = Map.insertWith (++) (extractSampleKey pz) [extractZoneKey pz] qFold
>
>     partnerer1         :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     partnerer1 zrec@InstZoneRecord{ .. } rdFold
>                                          = (zrec', rd')
>       where
>         (pzs, rd')                       = zoneTask (const True) partnerUp zsPreZones rdFold
>         zrec'                            = zrec{zsPreZones = pzs}
>
>     partnerUp          :: PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions)
>     partnerUp pz rdFold                  = (Just $ pz{pzmkPartners = partners}, rd')
>       where
>         fName                            = "partnerUp"
>
>         myPartners                       = fromMaybe [] (Map.lookup yrSampleKey backMap)       
>
>         mySampleKey                      = extractSampleKey pz
>         myZoneKey                        = extractZoneKey pz
>         yrSampleKey                      = mySampleKey{pskwSampleIndex = F.sampleLink (effPZShdr pz)}
>
>         (partners, rd')                  =
>           if isStereoZone pz
>             then (Right myPartners, dispose myZoneKey [Scan Modified Ok fName "stereo"] rdFold)
>             else (Left myZoneKey,   dispose myZoneKey [Scan NoChange Ok fName "mono"]   rdFold)

partnering 2 task =====================================================================================================
          take care of valid partners (and specified crossovers if applicable)
          use full partner map to find and vet

> partner2TaskIf _ _ fwIn                  = zrecTask partnerer2 fwIn
>   where
>     partnerMap         :: Map PreZoneKey [PreZoneKey]
>     partnerMap                           = zrecCompute fwIn pFolder Map.empty
>     pFolder m zrec                       = zoneCompute isStereoZone zFolder zrec m
>     zFolder m' pz                        = Map.insertWith (++) (extractZoneKey pz) (fromRight [] pz.pzmkPartners) m'
>         
>     partnerer2         :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     partnerer2 zrec@InstZoneRecord{ .. } rdIn
>       | traceNever trace_P2 False          = undefined
>       | otherwise                        = (zrec', rd')
>       where
>         fName                            = "partnerer2"
>         counts                           = map length (Map.elems partnerMap)
>         trace_P2                         = unwords [fName, show counts]
>
>         (pzs, rd')                       = zoneTask isUnpartnered partnerDown zsPreZones rdIn
>         zrec'                            = zrec{zsPreZones = pzs}
>
>     partnerDown pz rdFold                = (Just $ pz{pzmkPartners = partners}, rdFold')
>       where
>         fName                            = "partnerDown"
>
>         myZoneKey                        = extractZoneKey pz
>         myInstKey                        = extractInstKey pz
>
>         pzks                             = fromRight [] pz.pzmkPartners
>
>         mpartner                         = find (perfect True) pzks `CM.mplus` find (perfect False) pzks
>         (partners, rdFold')              =
>           case mpartner of
>             Just pzk@PreZoneKey{ .. }    →
>               (Left pzk,         dispose myZoneKey
>                                          [Scan Modified Paired fName (show (pzkwFile, pzkwInst, pzkwBag))]
>                                          rdFold)
>             Nothing                      →
>               (pz.pzmkPartners,  dispose myZoneKey [Scan NoChange Unpaired fName "nonconforming"]  rdFold)
>
>         perfect nocross pzk
>           | null otherZoneKeys           = False
>           | otherwise                    = (myZoneKey `elem` otherZoneKeys) && permitted
>           where
>             otherZoneKeys                = fromMaybe [] (Map.lookup pzk partnerMap)
>             otherInstKey                 = PerGMKey pzk.pzkwFile pzk.pzkwInst Nothing
>             permitted                    = 
>               if nocross
>                 then myInstKey == otherInstKey
>                 else allowSpecifiedCrossovers

partnering 3 task =====================================================================================================
          inferring crossovers

> partner3TaskIf _ _ fwIn                  = if allowInferredCrossovers
>                                              then zrecTask partnerer3 fwIn
>                                              else fwIn
>   where
>     partnerer3         :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     partnerer3 zrec@InstZoneRecord{ .. } rdFold
>                                          = (zrec', rd')
>       where
>         (pzs, rd')                       = zoneTask isUnpartnered partnerSideways zsPreZones rdFold
>         zrec'                            = zrec{zsPreZones = pzs}
>
>         pLeft, pRight  :: Map String PreZoneKey
>         (pLeft, pRight)                  = zrecCompute fwIn sideways1 (Map.empty, Map.empty)
>
>         sideways1 (mL, mR) zrecFold      = foldl' sideways2 (mL, mR) zrecFold.zsPreZones
>         sideways2 (mL, mR) pz
>           | isUnpartnered pz             = if isLeftPreZone pz
>                                              then (Map.insert mySampleName myZoneKey mL, mR)
>                                              else (mL, Map.insert mySampleName myZoneKey mR)
>           | otherwise                    = (mL, mR)
>           where
>             mySampleName                 = (effPZShdr pz).sampleName
>             myZoneKey                    = extractZoneKey pz
>
>         partnerSideways pz rdPartner     = (Just pz{pzmkPartners = partners}, rdPartner')
>           where
>             fName                        = "partnerSideways"
>
>             mySampleName                 = (effPZShdr pz).sampleName
>             myZoneKey                    = extractZoneKey pz
>
>             try1, try2 :: Maybe (String, PreZoneKey)
>             try1                         =
>               find (\(k, _) → isJust (find (== k) (fuzzToTheRight mySampleName))) (Map.assocs pLeft)
>             try2                         =
>               find (\(k, _) → isJust (find (== k) (fuzzToTheLeft mySampleName)))  (Map.assocs pRight)
>             try                          = try1 `CM.mplus` try2
>
>             (partners, rdPartner')       =
>               case try of
>                 Just (_, pzk)            →
>                   ( Left pzk,        dispose myZoneKey [Scan Modified Paired   fName "+crossover"]     rdPartner)
>                 Nothing                  →
>                   ( pz.pzmkPartners, dispose myZoneKey [Scan NoChange Unpaired fName "no crossover"]   rdPartner)        

partnering 4 task =====================================================================================================
          devolving to mono as a last resort

> partner4TaskIf _ _                       = zrecTask partnerer4
>   where
>     partnerer4         :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     partnerer4 zrec@InstZoneRecord{ .. } rdFold
>                                          = (zrec', rd')
>       where
>         (pzs, rd')                       = zoneTask isUnpartnered devolver zsPreZones rdFold
>         zrec'                            = zrec{zsPreZones = pzs}
>
>     devolver           :: PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions)
>     devolver pz rdFold                   = (Just pz'{pzmkPartners = Left myZoneKey}, rd')
>       where
>         fName                            = "devolver"
>
>         myZoneKey                        = extractZoneKey pz
>
>         (pz', rd')                       =
>           if canDevolveToMono
>             then ( makeMono pz, dispose myZoneKey [Scan Modified DevolveToMono    fName noClue] rdFold)
>             else ( pz,          dispose myZoneKey [Scan Violated BadStereoPartner fName noClue] rdFold)

reorg task ============================================================================================================
          where indicated, make one instrument out of many

Overview: the member instruments of a qualified group will be absorbed into the lead (member). She takes all of their 
zones, in effect. The mapping (member → lead) (Word → Word) is turned into the absorption map (aMap).

To build the map
 1. collect all instrument names

 2. group by similar names

 3. drop the strings, retaining member → lead structure

 4. filter any proposed absorptions via #5 qualify

 5. implement a suitability calculation 

> reorgTaskIf sffile _ fwIn@FileWork{ .. }
>                                          = zrecTask reorger fwIn
>   where
>     instNames          :: [(String, Word)]
>     instNames                            = sort $ zrecCompute fwIn extractFolder []
>     extractFolder ns zrec                =
>       let
>         preI                             = fwBoot.zPreInstCache Map.! instKey zrec
>         iName                            = preI.piChanges.cnName
>       in
>         (iName, zrec.zswInst) : ns
>     
>     owners             :: Map PerGMKey [PreZone]
>     owners                               = zrecCompute fwIn ownerFolder Map.empty 
>     ownerFolder m zrec                   = Map.insert (instKey zrec) zrec.zsPreZones m
>     
>     qualify            :: [Word] → Bool
>     qualify memberIs                     = fracAll / fracOne > 1.25
>       where
>         fracAll, fracOne
>                        :: Rational
>         fracAll                          = 100 * fromRational (fractionCovered $ smush $ zip pzLoads smashups)
>         fracOne                          = 100 * fromRational ((maximum . map fractionCovered) smashups)
>
>         smashups                         = map smash pergms
>         smash pergm                      = computeInstSmashup $ owners Map.! pergm
>
>         pergms                           = map (\wI → PerGMKey sffile.zWordF wI Nothing) memberIs
>         pzLoads                          = map (owners Map.!) pergms
>
>     qualified          :: [(Word, [Word])]
>     grouped                              = groupBy (\x y → absorbRatio < howClose (fst x) (fst y)) instNames
>     filtered                             = map (map snd) (filter (\x → 1 < length x) grouped)
>     qualified                            = filter (qualify . snd) (map (\y → (head y, y)) filtered)
>
>     aMap               :: Map Word Word
>     aMap                                 = foldl' fold1Fun Map.empty qualified
>       where
>         fold1Fun       :: Map Word Word → (Word, [Word]) → Map Word Word
>         fold1Fun qIn (wLead, wMembers)   = foldl' fold2Fun qIn wMembers
>           where
>             fold2Fun   :: Map Word Word → Word → Map Word Word
>             fold2Fun qFold wMember       = Map.insert wMember wLead qFold
> 
>     makeHolds          :: Map Word [PreZone] → InstZoneRecord → Map Word [PreZone]
>     makeHolds iHold zrec                 =
>       if deadrd (instKey zrec) fwDispositions then iHold else exert
>       where
>         exert                            =
>           (\case
>             Just target                  → upd target
>             Nothing                      → iHold) macts
>           where
>             macts                        = Map.lookup zrec.zswInst aMap
>             rebased                      = map rebase zrec.zsPreZones
>             rebase pz                    =
>               (\case
>                 Just owner               → pz{pzWordI = owner}
>                 Nothing                  → pz) macts
>
>             upd target                   = Map.insertWith (++) target rebased iHold
>
>     hMap                                 = foldl' makeHolds Map.empty (goodZRecs fwDispositions fwZRecs)
>
>     reorger            :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     reorger zrec rdFold
>       | not doAbsorption                 = (zrec,                       rdFold)
>       | isNothing aprobe                 = (zrec,                       rdFold)
>       | aresult == zrec.zswInst          = (zrec{zsPreZones = hresult}, dispose pergm scansIng rdFold)
>       | otherwise                        = (zrec,                       dispose pergm scansEd rdFold)
>       where
>         fName                            = "reorger"
>
>         aprobe                           = Map.lookup zrec.zswInst aMap
>         hprobe                           = Map.lookup zrec.zswInst hMap
>
>         aresult                          = deJust "aprobe" aprobe
>         hresult                          = deJust "hprobe" hprobe
>
>         pergm                            = instKey zrec
>         scansIng                         = [Scan Modified Absorbing fName noClue]
>         scansEd                          = [Scan Dropped Absorbed   fName noClue]

shave task ============================================================================================================
          remove dropped or violated instruments from the preInstCache

> shaveTaskIf _ _ fwIn@FileWork{ .. }      = fwIn{fwBoot = fwBoot{zPreInstCache = preInstCache}}
>   where
>     preInstCache                         =
>       foldl' shaveFolder fwBoot.zPreInstCache (badZRecs fwDispositions fwZRecs)
>     shaveFolder m zrec                   = Map.delete (instKey zrec) m

match task ============================================================================================================
          track all fuzzy matches

> matchTaskIf _ _ fwIn@FileWork{ .. }      = fwIn{fwMatches = Matches sMatches iMatches}
>   where
>     sMatches                             =
>       Map.foldlWithKey (\m k v → Map.insert k (computeFFMatches v.cnName) m)           Map.empty fwBoot.zPreSampleCache
>     iMatches                             =
>       Map.foldlWithKey (\m k v → Map.insert k (computeFFMatches v.piChanges.cnName) m) Map.empty fwBoot.zPreInstCache

categorization task ===================================================================================================
          assign each instrument to one of the three categories
          a. Just InstCatInst              an inst bearing one inst, or
          b. Just InstCatPerc              an inst bearing one or more percs, or
          c. Just InstCatDisq              an inst disqualified from tournaments, or
          d. Nothing                       undecided

> catTaskIf _ rost fwIn@FileWork{ .. }     = zrecTask catter fwIn
>   where
>     catter zrec rdFold                   = (zrec{zsInstCat = icat}, dispose (instKey zrec) ss rdFold)
>       where
>         (icat, ss)                       = categorizeInst zrec
>
>     categorizeInst     :: InstZoneRecord → (Maybe InstCat, [Scan])
>     categorizeInst zrec                  = (icat', ss')
>       where
>         fName                            = "categorizeInst"
>
>         pergm                            = instKey zrec
>         pzs                              = zrec.zsPreZones
>         preI                             = fwBoot.zPreInstCache Map.! pergm
>         iName                            = preI.piChanges.cnName

      Determine which category will belong to the Instrument, based on its "provideAlts" performance for both
      1. all kinds
      2. "rost" subset, could be same as 1.

>         icatAllKinds, icatRost, icatNarrow, icat'
>                        :: Maybe InstCat
>         icatAllKinds                     = foldl' CM.mplus Nothing (provideAlts Nothing allKinds)
>         icatRost                         = if rost == allKinds
>                                              then icatAllKinds
>                                              else foldl' CM.mplus Nothing (provideAlts icatAllKinds rost)
>         icatNarrow                       = Just (InstCatDisq Narrow noClue)
>         (icat', ss')                     =
>           case (icatAllKinds, icatRost) of
>             (Just (InstCatInst _)   , Just (InstCatInst _))
>                                          → (icatRost, [Scan Modified CatIsInst fName noClue])
>             (Just (InstCatPerc _)   , Just (InstCatPerc _))
>                                          → (icatRost, [Scan Modified CatIsPerc fName noClue])
>             (Just (InstCatDisq imp why), _)
>                                          → (icatAllKinds, [Scan Dropped imp fName why])
>             (_                      , Just (InstCatDisq imp why))
>                                          → (icatRost, [Scan Dropped imp fName why])
>             _                            → (icatNarrow, [Scan Dropped Narrow fName noClue])
>
>         provideAlts    :: Maybe InstCat → ([InstrumentName], [PercussionSound]) → [Maybe InstCat]
>         provideAlts seed rostAlts        =
>           let
>             iMatches                     = fromJust $ Map.lookup pergm fwMatches.mIMatches
>             ffInst'                      =
>               Map.filterWithKey (\k v → k `elem` select rostAlts && isPossible' v) iMatches.ffInst
>             ffPerc'                  =
>               Map.filterWithKey (\k v → k `elem` select rostAlts && isPossible' v) iMatches.ffPerc
>           in
>             [ 
>                 maybeSettle isConfirmed catInst                  ffInst'
>               , maybeSettle isConfirmed (catPerc wZones)         ffPerc'
>               , maybeNailAsPerc 0.6 
>               , maybeSettle stands      catInst                  ffInst'
>
>               , maybeSettle stands      (catPerc wZones)         ffPerc'
>               , maybeSettle stands      (catDisq Narrow noClue)  iMatches.ffInst
>
>               , maybeNailAsPerc 0.4
>               , maybeSettle stands      (catPerc wZones)         [evalGenericPerc iName]
>               , Just $ catDisq Unrecognized noClue
>             ]
>           where
>             uZones :: [Word]         = fromMaybe wZones (getMaybePercList seed)
>             wZones :: [Word]         = mapMaybe (qualPercZone rostAlts) pzs
>
>             maybeNailAsPerc
>                    :: Double → Maybe InstCat
>             maybeNailAsPerc fr       =
>               let
>                 uFrac                = howLaden uZones
>                 wFrac                = howLaden wZones
>               in if canBePercI pzs && fr < uFrac
>                 then
>                   (if 0.2 < wFrac
>                      then Just (catPerc wZones)
>                      else Just (catDisq NoPercZones (show (uFrac, wFrac))))
>                 else Nothing
>
>             maybeSettle
>                        :: (Foldable t, Show (t Fuzz)) ⇒ Fuzz → InstCat → t Fuzz → Maybe InstCat
>             maybeSettle thresh ic keys   = find (> thresh) keys >> Just ic
>
>             canBePercI :: [PreZone] → Bool
>             canBePercI                   = all canBePercZ
>             canBePercZ :: PreZone → Bool
>             canBePercZ x                 =
>               case x.pzDigest.zdKeyRange of
>                 Just rng                 →    inRange rng ((fromIntegral . fst) percPitchRange)
>                                            && inRange rng ((fromIntegral . snd) percPitchRange)
>                 Nothing                  → True
>      
>             howLaden   :: [Word] → Double
>             howLaden ws
>               | null pzs                 = 0
>               | otherwise                = (fromIntegral . length) ws / (fromIntegral . length) pzs
>
>             catInst      :: InstCat      =
>               if null pzs
>                 then InstCatDisq NoZones noClue
>                 else (case checkSmashing smashup of
>                        Nothing           → InstCatInst icd
>                        Just imp          → InstCatDisq imp noClue)
>               where
>                 smashup                  = computeInstSmashup pzs
>                 icd                      = InstCatData pzs smashup []
>
>             catPerc      :: [Word] → InstCat
>             catPerc ws                   =
>               if null pzs || null ws
>                 then InstCatDisq NoZones fNameCatPerc
>                 else (case checkSmashing smashup of
>                        Nothing           → InstCatPerc icd
>                        Just imp          → InstCatDisq imp noClue)
>               where
>                 fNameCatPerc             = "catPerc"
>
>                 pzs'                     = filter (\x → x.pzWordB `elem` ws) pzs
>                 smashup                  = computeInstSmashup pzs'
>                 icd                      = InstCatData pzs' smashup ws
>
>             catDisq    :: Impact → String → InstCat
>             catDisq                      = InstCatDisq
>
>         qualPercZone   :: ([InstrumentName], [PercussionSound]) → PreZone → Maybe Word
>         qualPercZone rost' prez          = mrange >>= pinnedKR (select rost') >> Just prez.pzWordB
>           where
>             mrange                       = prez.pzDigest.zdKeyRange >>= (Just . BF.bimap fromIntegral fromIntegral)

zone task =============================================================================================================
          generate the PerInstrument map from jobs map

> zoneTaskIf sffile _ fwIn@FileWork{ .. }  = fwIn{  fwBoot = fwBoot{zPerInstCache = fst formZoneCache}
>                                                 , fwDispositions = snd formZoneCache}
>   where
>     fName                                = "computePerInst"
>
>     formZoneCache      :: (Map PerGMKey PerInstrument, ResultDispositions)
>     formZoneCache                        = Map.foldlWithKey perFolder (Map.empty, fwDispositions) fwBoot.zJobs
>
>     perFolder          :: (Map PerGMKey PerInstrument, ResultDispositions)
>                           → PerGMKey → InstCat
>                           → (Map PerGMKey PerInstrument, ResultDispositions)
>     perFolder (zc, rdFold) pergm icat    =
>             ( Map.insert pergm (computePerInst pergm icat) zc
>             , dispose pergm [Scan Accepted ToZoneCache fName noClue] rdFold)
>
>     computePerInst     :: PerGMKey → InstCat → PerInstrument
>     computePerInst pergm icat            = PerInstrument (zip pzs oList) icd.inSmashup
>       where
>         preI                             = fwBoot.zPreInstCache Map.! pergm
>
>         icd            :: InstCatData
>         bixen          :: [Word]
>
>         (icd, bixen)                     =
>           case icat of
>             InstCatPerc x                → (x, x.inPercBixen)
>             InstCatInst x                → (x, map pzWordB x.inPreZones)
>             _                            → error $ unwords [fName, "only Inst and Perc are valid here"]
>
>         gZone                            =
>           case preI.iGlobalKey of
>             Nothing                      → defZone
>             Just pzk                     → buildZone sffile defZone Nothing pzk.pzkwBag
>         oList                            = map (\pz → buildZone sffile gZone (Just pz) pz.pzWordB) pzs
>
>         pzs                              = filter (\pz → pz.pzWordB `elem` bixen) icd.inPreZones
>
> buildZone              :: SFFile → SFZone → Maybe PreZone → Word → SFZone
> buildZone sffile fromZone mpz bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = zone
>   where
>     zName                                =
>       case mpz of
>         Nothing                          → "<global>"
>         Just pz                          → (effPZShdr pz).sampleName
>     zone                                 = foldr addMod (foldl' addGen fromZone gens) mods
>     boota                                = sffile.zFileArrays
>
>     xgeni                                = F.genNdx $ boota.ssIBags!bagIndex
>     ygeni                                = F.genNdx $ boota.ssIBags!(bagIndex + 1)
>     xmodi                                = F.modNdx $ boota.ssIBags!bagIndex
>     ymodi                                = F.modNdx $ boota.ssIBags!(bagIndex + 1)
>
>     gens               :: [F.Generator]
>     gens                                 =
>       profess
>         (xgeni <= ygeni)
>         (unwords["SoundFont file", show sffile.zWordF, sffile.zFilename, "corrupt (buildZone gens)"])
>         (map (boota.ssIGens !) (deriveRange xgeni ygeni))
>     mods               :: [(Word, F.Mod)]
>     mods                                 =
>       profess
>         (xmodi <= ymodi)
>         (unwords["SoundFont file", show sffile.zWordF, sffile.zFilename, "corrupt (buildZone mods)"])
>         (zip [10_000..] (map (boota.ssIMods !) (deriveRange xmodi ymodi)))
>
>     trace_BZ                             =
>       unwords ["buildZone", show (sffile.zWordF, bagIndex), show zName, show (fromZone == defZone)]
>
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
>   F.KeyRange x y                 → iz {zKeyRange =                 Just (fromIntegral x, fromIntegral y)}
>   F.VelRange x y                 → iz {zVelRange =                 Just (fromIntegral x, fromIntegral y)}
>   F.Key i                        → iz {zKey =                      Just i}
>   F.Vel i                        → iz {zVel =                      Just i}
>   F.InitAtten i                  → iz {zInitAtten =                Just i}
>   F.CoarseTune i                 → iz {zCoarseTune =               Just i}
>   F.FineTune i                   → iz {zFineTune =                 Just i}
>   F.SampleIndex w                → iz {zSampleIndex =              Just w}
>   F.SampleMode m                 → iz {zSampleMode =               Just m}
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
> addMod (mId, F.Mod{..}) iz@SFZone{zModulators} 
>                                          = maybe iz addModulator makeModulator
>   where
>     addModulator       :: Modulator → SFZone
>     addModulator m8r                     = iz{zModulators = m8r : zModulators}
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

rejobs task ===========================================================================================================
          generate the jobs map, from the zrec list

> rejobsTaskIf _ _ fwIn@FileWork{ .. }     = fwIn{fwBoot = fwBoot{zJobs = zrecCompute fwIn foldJob Map.empty}}
>   where
>     foldJob m zrec                       = Map.insert (instKey zrec) (deJust "InstCat" zrec.zsInstCat) m

repartner task ========================================================================================================
          generate partial partner map with and for crossovers only, from the zrec list

> repartnerTaskIf _ _ fwIn@FileWork{ .. }  = fwIn{fwBoot = fwBoot{zPartnerMap = zrecCompute fwIn foldPard Map.empty}}
>   where
>     foldPard           :: Map PreZoneKey PreZoneKey → InstZoneRecord → Map PreZoneKey PreZoneKey
>     foldPard m zrec                      =
>       let
>         zFolder        :: Map PreZoneKey PreZoneKey → PreZone → Map PreZoneKey PreZoneKey
>         zFolder m' pz                    = if isStereoZone pz && not (inSameInstrument myPzk otherPzk)
>                                              then Map.insert myPzk otherPzk m'
>                                              else m'
>           where
>             myPzk                        = extractZoneKey pz
>             otherPzk                     = fromLeft (PreZoneKey 0 0 0 0) pz.pzmkPartners
>       in
>         foldl' zFolder m zrec.zsPreZones
>
> sampleSizeOk           :: (Word, Word) → Bool
> sampleSizeOk (stS, enS)                  = stS >= 0 && enS - stS >= 0 && enS - stS < 2 ^ (22::Word)
>
> adjustedSampleSizeOk   :: PreZone → Bool
> adjustedSampleSizeOk pz                  = 0 <= stA && stA <= enA && 0 <= stL && stL <= enL
>                                                     && enA - stA < 2  ^ (22::Word)
>                                                     && (zd.zdSampleMode == Just A.NoLoop || enL - stL < 2  ^ (22::Word))
>   where
>     stA                                  = shdr.start     + fromIntegral zd.zdStart
>     enA                                  = shdr.end       + fromIntegral zd.zdEnd
>     stL                                  = shdr.startLoop + fromIntegral zd.zdStartLoop
>     enL                                  = shdr.endLoop   + fromIntegral zd.zdEndLoop
>
>     shdr                                 = effPZShdr pz
>     zd                                   = pz.pzDigest
>
> goodSampleRate         :: Word → Bool
> goodSampleRate x                         = x == clip (n64, n2 ^ n20) x
>   where
>     n64, n2, n20       ::Word
>     n64                                  = 64
>     n2                                   = 2
>     n20                                  = 20
> canDevolveToMono       :: Bool
> canDevolveToMono                         = True
> requiredZoneLinkage    :: Double
> requiredZoneLinkage                      = 0
>
> allowOutOfRange        :: Bool
> allowOutOfRange                          = True
> allowOverlappingRanges :: Bool
> allowOverlappingRanges                   = True
> doAbsorption           :: Bool
> doAbsorption                             = True
> fixBadNames            :: Bool
> fixBadNames                              = True

The End
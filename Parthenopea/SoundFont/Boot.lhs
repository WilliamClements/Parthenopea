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
> import qualified Data.Bifunctor          as BF
> import Data.Char
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
> defFileWork                              =
>   FileWork dasBoot [] defMatches virginrd
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
>                , shaveTaskIf, matchTaskIf, catTaskIf, encatTaskIf
>                , rejobsTaskIf, zoneTaskIf, repartnerTaskIf
>                        :: SFFile → ([InstrumentName], [PercussionSound]) → FileWork → FileWork
>
> makeFileIterate        :: SFFile → ([InstrumentName], [PercussionSound]) → FileIterate
> makeFileIterate sffile rost              =
>   FileIterate
>     defFileWork 
>     [  ("preSample",  preSample)
>      , ("preInst",    preInst)
>      , ("survey",     survey)
>      , ("capture",    mark . capture)
>      , ("partner1",   partner1)
>      , ("partner2",   partner2)
>      , ("partner3",   partner3)
>      , ("partner4",   partner4)
>      , ("reorg",      reorg) 
>      , ("shave",      shave)
>      , ("match",      match)
>      , ("cat",        cat)
>      , ("encat",      rejobs . encat)
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
>     encat                                = encatTaskIf        sffile rost
>     zone                                 = zoneTaskIf         sffile rost
>
> reduceFileIterate      :: FileIterate → (SFBoot, ResultDispositions, Matches)
> reduceFileIterate fiIn                   = (fwBoot, fwDispositions, fwMatches)
>   where
>     FileWork{ .. }                       = fiIn.fiFw

executive =============================================================================================================

To support extracting from flawed SoundFont files, we - up front - withdraw unrecoverable items from their
respective collections. The withdrawn items may be critical to some instruments. So that entails further deletion
and recovery.

> equipInstruments       :: ([InstrumentName], [PercussionSound])
>                           → IO (Maybe (SFRuntime, Matches, [PerGMKey], [PerGMKey], ResultDispositions))
> equipInstruments rost                    = do
>   putStrLn $ unwords ["rost\n", show rost]
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
>       (pergmsI, pergmsP)                 ← enumGMs bootAll.zJobs
>
>       tsFinished                         ← getCurrentTime
>       putStrLn ("___GMs enumerated: " ++ show (diffUTCTime tsFinished tsBooted))
>
>       return (Just (runt, matchesAll, pergmsI, pergmsP, rdGen03))
>   where
>     bootFolder         :: (SFBoot, ResultDispositions, Matches) → SFFile → (SFBoot, ResultDispositions, Matches)
>     bootFolder (bootIn, rdIn, matchesIn) sffile
>                                          = 
>       let
>         (bootOut, rdOut, matchesOut)     = reduceFileIterate (ingestFile sffile)     
>       in
>         (combineBoot bootIn bootOut, combinerd rdIn rdOut, combineMatches matchesIn matchesOut)
>
>     ingestFile         :: SFFile → FileIterate
>     ingestFile sffile                    =
>       let
>         unfinished fiIn                  = not (null fiIn.fiTaskIfs)
>         nextGen fiIn@FileIterate{ .. }   = tracer name fiIn{ fiFw = userFun fiFw, fiTaskIfs = tail fiTaskIfs}
>           where
>             duo                          = head fiTaskIfs
>             name                         = fst duo
>             userFun                      = snd duo
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
>       let samplea                          =
>             SampleArrays
>               (F.smpl  sdata) (F.sm24  sdata)
>       let sffile                         = SFFile wFile filename boota samplea
>       let nBits::Word                      =
>             case samplea.ssM24 of
>               Nothing                    → 16
>               Just _                     → 24
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
>       putStrLn (unwords ["(", show nBits, ") loaded in", show (diffUTCTime ts2 ts1)])
>       return sffile
>
> enumGMs                :: Map PerGMKey InstCat → IO ([PerGMKey], [PerGMKey])
> enumGMs jobs                             = return $ Map.foldlWithKey enumFolder ([], []) jobs
>   where
>     enumFolder         :: ([PerGMKey], [PerGMKey]) → PerGMKey → InstCat → ([PerGMKey], [PerGMKey])
>     enumFolder (pergmsI, pergmsP) pergmI_ icat
>                                          =
>       let
>         pergmI                           = pergmI_{pgkwBag = Nothing}
>       in
>         case icat of
>           InstCatPerc icd                → (pergmsI, pergmsP ++ instrumentPercList pergmI icd.inPercBixen)
>           InstCatInst _                  → (pergmI : pergmsI, pergmsP)
>           InstCatDisq _ _                → (pergmsI, pergmsP)
>
>     instrumentPercList :: PerGMKey → [Word] → [PerGMKey]
>     instrumentPercList pergmI            = map (\w → pergmI {pgkwBag = Just w})
>
> checkSmashing          :: PerGMKey → Smashing Word → Maybe [Scan]
> checkSmashing pergm smashup
>   | not ok1                              = Just $ violated pergm UndercoveredRanges
>   | not ok2                              = Just $ violated pergm OverCoveredRanges 
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
>     -- create smashup consisting of 16_384 (128 x 128) Word pairs - adds up to 131_072 bytes
>     subs               :: [(Word, [Maybe (Word, Word)])]
>     subs                                 = map extractSpace pzs
>
>     trace_CIS                            = unwords [fName, showPreZones pzs, show subs]
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

> preSampleTaskIf sffile _ fwIn            = foldl' formFolder fwIn (formComprehension sffile ssShdrs)
>   where
>     fName_                               = "preSampleTaskIf"
>
>     formFolder         :: FileWork → PreSampleKey → FileWork
>     formFolder fwForm@FileWork{ .. } presk
>                                          = fwForm{ fwBoot = fwBoot', fwDispositions = dispose presk ss fwDispositions}
>       where
>         fName                            = unwords [fName_, "formFolder"]
>
>         fwBoot'                          =
>           if dead ss
>             then fwBoot
>             else fwBoot{zPreSampleCache = Map.insert presk ps fwBoot.zPreSampleCache}
>         ps                               = ChangeName shdr changes name
>         shdr                             = sffile.zFileArrays.ssShdrs ! presk.pskwSampleIndex
>
>         bad                              = shdr.sampleName
>         good                             = fixName bad
>
>         (ss_, clue)                      
>           | not (goodSampleRate shdr.sampleRate)
>                                          = (violated presk BadSampleRate, show shdr.sampleRate)
>           | isNothing (toMaybeSampleType shdr.sampleType)
>                                          = (violated presk BadSampleType, show shdr.sampleType)
>           | not (sampleSizeOk (shdr.start, shdr.end))
>                                          = (violated presk BadSampleLimits, show (shdr.start, shdr.end))
>           | not (goodName bad)           = (badButMaybeFix fixBadNames CorruptName fName bad good, noClue)
>           | otherwise                    = (accepted presk Ok, show presk.pskwSampleIndex)
>
>         (ss, changes, name)              = if wasRescued CorruptName ss_
>                                              then (ss_, singleton FixCorruptName, good)
>                                              else (finishScans fName clue ss_, [], bad)

pre-instance task =====================================================================================================
          access and critique all Instrument "records" in the file 

> preInstTaskIf sffile _ fwIn@FileWork{ .. }
>                                          =
>   fwIn{  fwBoot = fwBoot{zPreInstCache = preInstCache}
>        , fwDispositions = rdFinal}
>   where
>     fName_                               = "preInstTaskIf"
>
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
>         fName                            = unwords [fName_, "preIFolder"]
>
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pgkwInst + 1}
>
>         bad                              = iinst.instName
>         good                             = fixName bad
>
>         m'                               =
>           if dead ss
>              then m 
>              else Map.insert pergm (PreInstrument (ChangeName iinst changes finalName) Nothing) m
>
>         (ss_, clue)
>           | iinst.instBagNdx == jinst.instBagNdx
>                                          = (violated pergm NoZones, show iinst.instName)
>           | not (goodName bad)           = (badButMaybeFix fixBadNames CorruptName fName bad good, noClue)
>           | otherwise                    = (accepted pergm Ok, show pergm.pgkwInst)
>
>         ss                               = if wasRescued CorruptName ss_ then ss_ else finishScans fName clue ss_
>         changes                          = if wasRescued CorruptName ss_ then singleton FixCorruptName else []
>         finalName                        = if wasRescued CorruptName ss_ then good else bad
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
>
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             = PerGMKey zrec.zswFile zrec.zswInst Nothing       

survey task ===========================================================================================================
          instantiate zrecs

> surveyTaskIf _ _ fwIn@FileWork{ .. }     =
>   fwIn{fwZRecs = map makeZRec (Map.keys fwBoot.zPreInstCache)}

iterating on InstZoneRecord list ======================================================================================

> zrecTask               :: (InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions))
>                           → FileWork → FileWork
> zrecTask userFun fwIn@FileWork{ .. }     = fwIn{fwZRecs = workedOn, fwDispositions = rd'}
>   where
>     (workedOn, rd')                      = foldl' taskRunner ([], fwDispositions) (goodZRecs fwDispositions fwZRecs)
>
>     taskRunner (zrecs, rdFold) zrec  = 
>       let
>         (zrec', rdFold')                 = userFun zrec rdFold
>       in
>         if deadrd (instKey zrec) rdFold' 
>           then (zrec : zrecs, rdFold')
>           else (zrec': zrecs, rdFold')
>
> zrecCompute            :: ∀ a . FileWork → (a → InstZoneRecord → a) → a → a
> zrecCompute FileWork{ .. } userFun seed  = foldl' userFun seed (goodZRecs fwDispositions fwZRecs)
>
> zoneTask               :: (PreZone → Bool)
>                           → (PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions))
>                           → [PreZone] → ResultDispositions
>                           → ([PreZone], ResultDispositions)
> zoneTask zfilter zxform pzs rdIn
>   | traceNot trace_ZT False              = undefined
>   | otherwise                            = (catMaybes mworkedOn ++ ignore, rd')
>   where
>     fName                                = "zoneTask"
>     trace_ZT                             = unwords[fName, show $ length pzs, show $ length workOn]
>
>     (workOn, ignore)                     = partition zfilter pzs
>    
>     mworkedOn          :: [Maybe PreZone]
>     (mworkedOn, rd')                     = foldl' taskFolder ([], rdIn) workOn
>
>     taskFolder         :: ([Maybe PreZone], ResultDispositions) → PreZone → ([Maybe PreZone], ResultDispositions)
>     taskFolder (mpzs, rdFold) pz         = (mpz : mpzs, rdFold')
>       where
>         (mpz, rdFold')                   = zxform pz rdFold

capture task ==========================================================================================================
          populate zrec with PreZones

> captureTaskIf sffile _ fwIn@FileWork{ .. }
>                                          = zrecTask capturer fwIn 
>   where
>     capturer           :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     capturer zrec rdIn                   =
>       let
>         (newPzs, rdOut, globalKey)       = captureZones (instKey zrec) rdIn
>       in
>         (zrec{zsPreZones = newPzs, zsGlobalKey = globalKey}, rdOut)
>
>     captureZones       :: PerGMKey → ResultDispositions → ([PreZone], ResultDispositions, Maybe PreZoneKey)
>     captureZones pergm rdCap             = (pzsRemaining, rdCap'', globalKey)
>       where
>         fName_                           = unwords["captureZones"]
>
>         results                          = map captureZone (deriveRange ibagi jbagi)
>
>         pzsRemaining                     = lefts results
>         rdCap''                          = foldl' sampleFolder rdCap' pzsRemaining
>         globalKey                        = if head results == Right (Accepted, GlobalZone)
>                                              then Just $ PreZoneKey sffile.zWordF ibagi
>                                              else Nothing
>
>         ibagi                            = F.instBagNdx (sffile.zFileArrays.ssInsts ! pgkwInst pergm)
>         jbagi                            = F.instBagNdx (sffile.zFileArrays.ssInsts ! (pgkwInst pergm+1))
>
>         (ss_, clue)
>           | null pzsRemaining            = (violated pergm NoZones, "null")
>           | otherwise                    = (accepted pergm Ok, show (length pzsRemaining))
>         ss                               = finishScans fName_ clue ss_ 
>         rdCap'                           = dispose pergm ss rdCap
>
>         sampleFolder   :: ResultDispositions → PreZone → ResultDispositions
>         sampleFolder rdFold pz           =
>           let
>             impact                       = if wasSwitchedToMono pz
>                                              then AdoptedAsMono
>                                              else Adopted
>           in
>             dispose (extractSampleKey pz) [Scan Modified impact fName_ (show pergm.pgkwInst)] rdFold
>             
>         captureZone    :: Word → Either PreZone (Disposition, Impact)
>         captureZone bix                  = eith
>           where
>             fName                        = unwords [fName_, "captureZone"]
>                 
>             eith
>                                          -- TODO: corrupt adjusted limits?
>               | isNothing pz.pzDigest.zdSampleIndex
>                                          = Right (Accepted, GlobalZone)
>               | isNothing mpres          = Right (Dropped, OrphanedBySample)
>               | otherwise                = Left pz{pzChanges = ChangeEar (effPSShdr pres) []}
>
>             xgeni                        = F.genNdx $ sffile.zFileArrays.ssIBags ! bix
>             ygeni                        = F.genNdx $ sffile.zFileArrays.ssIBags ! (bix + 1)
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

mark task =============================================================================================================
          copy global zone markers from zrecs to preInstCache

> markTaskIf _ _ fwIn@FileWork{ .. }       = fwIn{fwBoot = fwBoot{zPreInstCache = preInstCache'}}
>   where
>     preInstCache'                        = foldl' markFolder fwBoot.zPreInstCache (goodZRecs fwDispositions fwZRecs)
>     markFolder preInstCache zrec         =
>       let
>         pergm                            = instKey zrec
>         preI                             = preInstCache Map.! pergm
>       in
>         Map.insert pergm (preI{iGlobalKey = zrec.zsGlobalKey}) preInstCache

partnering 1 task =====================================================================================================
          prepare for all possible stereo pairs

> partner1TaskIf _ _ fwIn                  = fwOut
>   where
>     fwOut                                = zrecTask partnerer1 fwIn
>
>     backMap            :: Map PreSampleKey [PreZoneKey]
>     backMap                              = zrecCompute fwIn backFolder Map.empty
>
>     backFolder m zrec                    = Map.union m m'
>       where
>         m'                               = 
>           foldl' pzFolder Map.empty (filter isStereoZone zrec.zsPreZones)
>
>     pzFolder target pz                   =
>       Map.insertWith (++) (PreSampleKey pz.pzWordF pz.pzWordS) [extractZoneKey pz] target
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
>         mySampleKey                      = extractSampleKey pz
>         myZoneKey                        = extractZoneKey pz
>         mySampleLink                     = mySampleKey{pskwSampleIndex = F.sampleLink (effPZShdr pz)}
>         myPartners                       = fromMaybe [] (Map.lookup mySampleLink backMap)        
>
>         (partners, rd')                  =
>           if isStereoZone pz
>             then (Right myPartners, rdFold)
>             else (Left myZoneKey, dispose mySampleKey [Scan NoChange Ok fName "mono"] rdFold)

partnering 2 task =====================================================================================================
          take care of valid partners (and specified crossovers if applicable)
          save full (stereo) partnerMap which will later be used to make partial map

> partner2TaskIf _ _ fwIn@FileWork{ .. }   = (zrecTask partnerer2 fwIn){fwBoot = fwBoot{zPartnerMap = partnerMap}}
>   where
>     partnerMap                           = formFullPartnerMap fwIn 
>
>     partnerer2         :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     partnerer2 zrec@InstZoneRecord{ .. } rdFold
>                                          = (zrec', rd')
>       where
>         (pzs, rd')                       = zoneTask isUnpartnered partnerDown zsPreZones rdFold
>         zrec'                            = zrec{zsPreZones = pzs}
>
>     partnerDown pz rdFold                = (Just $ pz{pzmkPartners = partners}, rd')
>       where
>         fName                            = "partnerDown"
>
>         mySampleKey                      = extractSampleKey pz
>         myZoneKey                        = extractZoneKey pz
>         myInstKey                        = extractInstKey pz
>         mpartner                         = find perfect (fromRight [] pz.pzmkPartners)
>         (partners, rd')                  =
>           case mpartner of
>             Just pzk                     →
>               (Left pzk,        dispose mySampleKey [Scan Modified Paired fName (status pzk)]    rdFold)
>             Nothing                      →
>               (pz.pzmkPartners, dispose mySampleKey [Scan NoChange Unpaired fName "nonconforming"] rdFold)
>
>         status pzk                       =
>           let
>             pzOther                      = partnerMap Map.! pzk
>             otherInstKey                 = extractInstKey pzOther
>           in
>             if myInstKey == otherInstKey
>               then "stereo" 
>               else "crossover"
>
>         perfect pzk                      =
>           let
>             pzOther                      = partnerMap Map.! pzk
>             otherInstKey                 = extractInstKey pzOther
>             found                        =
>               find (\x → x == myZoneKey && (allowSpecifiedCrossovers || myInstKey == otherInstKey))
>                    (fromRight [] pzOther.pzmkPartners)
>           in
>             isJust found
>

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
>             mySampleKey                  = extractSampleKey pz
>             mySampleName                 = (effPZShdr pz).sampleName
>
>             try1, try2 :: Maybe (String, PreZoneKey)
>             try1                         =
>               find (\(k, _) → isJust (find (== k) (fuzzToTheRight mySampleName))) (Map.assocs pLeft)
>             try2                         =
>               find (\(k, _) → isJust (find (== k) (fuzzToTheLeft mySampleName))) (Map.assocs pRight)
>             try                          = try1 `CM.mplus` try2
>
>             (partners, rdPartner')       =
>               case try of
>                 Just (_, pzk)            →
>                   ( Left pzk
>                   , dispose mySampleKey [Scan Modified Paired fName "crossed over"] rdPartner)
>                 Nothing                  →
>                   ( pz.pzmkPartners
>                   , dispose mySampleKey [Scan NoChange Unpaired fName "no suitable pard"] rdPartner)        

partnering 4 task =====================================================================================================
          devolving to mono as a last resort

> partner4TaskIf _ _                       = zrecTask partnerer4
>   where
>     partnerer4         :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     partnerer4 zrec@InstZoneRecord{ .. } rdFold
>                                          = (zrec', rd')
>       where
>         (pzs, rd')                       = zoneTask isUnpartnered littlePartner zsPreZones rdFold
>         zrec'                            = zrec{zsPreZones = pzs}
>
>     littlePartner      :: PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions)
>     littlePartner pz rdFold              = (Just pz'{pzmkPartners = Left myZoneKey}, rd')
>       where
>         fName                            = "littlePartner"
>
>         mySampleKey                      = extractSampleKey pz
>         myZoneKey                        = extractZoneKey pz
>
>         (pz', rd')                       =
>           if canDevolveToMono
>             then ( makeMono pz
>                  , dispose mySampleKey [Scan Modified DevolveToMono fName "not fixed per se"] rdFold)
>             else ( pz
>                  , dispose mySampleKey [Scan Violated BadStereoPartner fName noClue] rdFold)

reorg task ============================================================================================================
          where indicated, make one instrument out of many

Overview: the member instruments of a qualified group will be absorbed into the lead (member). She takes all of their 
zones, in effect. The mapping (member → lead) (Word → Word) is turned into the absorption map (aMap).

To build the map
 1. collect all instrument names

 2. build fragsets by grouping similar names

 3. drop the strings, retaining member → lead structure

 4. filter any proposed absorptions through qualifyAbsorptionGroup

> buildAbsorptionMap     :: SFFile → ([InstrumentName], [PercussionSound]) → FileWork → Map Word Word
> buildAbsorptionMap sffile _ fwIn@FileWork{ .. }
>                                          = foldl' fold1Fun Map.empty dissected
>   where
>     zrecs                                = goodZRecs fwDispositions fwZRecs
>
>     owners                               = zrecCompute fwIn harvestFolder Map.empty 
>
>     harvestFolder      :: Map PerGMKey [PreZone] → InstZoneRecord → Map PerGMKey [PreZone]
>     harvestFolder m zrec                 = Map.insert (instKey zrec) zrec.zsPreZones m
>
>     fold1Fun           :: Map Word Word → (Word, [Word]) → Map Word Word
>     fold1Fun mIn (wLead, wMembers)       = foldl' fold2Fun mIn wMembers
>       where
>         fold2Fun       :: Map Word Word → Word → Map Word Word
>         fold2Fun m' wMember              = Map.insert wMember wLead m'
>
>     dissected          :: [(Word, [Word])]
>     dissected                            = tracer "dissected" (filter qualifyAbsorptionGroup groupedC)
>       where
>         frags                            = map enFrag zrecs
>         fragsets                         = sort $ mapMaybe deFrag frags
>         groupedA                         = filter (\x → 1 < length x) (groupBy (\x y → fst x == fst y) fragsets)
>         groupedB                         = map (map snd) groupedA
>         groupedC                         = map (\y → (head y, y)) groupedB        
>
>     enFrag             :: InstZoneRecord → (String, Word)
>     enFrag zrec                          =
>       let
>         preI                             = fwBoot.zPreInstCache Map.! instKey zrec
>         iName                            = preI.piChanges.cnName
>       in
>         (iName, zrec.zswInst)
>
>     deFrag             :: (String, Word) → Maybe (String, Word)
>     deFrag (str, w)                     =
>       let
>         str', okstr, str''
>                        :: String
>         str'                             = shorten isDigit str
>         okstr                            = "_-"
>         str''                            = shorten (\x → isSpace x || x `elem` okstr) str'
>       in
>         if str /= str'
>           then Just (str'', w)
>           else Nothing
>
>     qualifyAbsorptionGroup
>                         :: (Word, [Word]) → Bool
>     qualifyAbsorptionGroup (leadI, memberIs)
>       | traceIf trace_QAG False          = undefined
>       | otherwise                        = answer
>       where
>         fName                            = unwords ["qualifyAbsorptionGroup"]
>
>         pergms                           = map (\wI → PerGMKey sffile.zWordF wI Nothing) memberIs
>         smashups                         = map smash pergms
>         pzLoads                          = map (owners Map.!) pergms
>
>         fracOne        :: Rational
>         fracOne                          = 100 * fromRational ((maximum . map fractionCovered) smashups)
>
>         smashAll                         = smush (zip pzLoads smashups)
>         fracAll                          = 100 * fromRational (fractionCovered smashAll)
>         answer                           = fracAll / fracOne > 1.25
>
>         smash pergm                      = computeInstSmashup $ owners Map.! pergm
>
>         trace_QAG                        =
>           unwords [fName, show leadI, show (fracOne, fracAll)
>                  , show "...."
>                  , show answer, show (fracAll / fracOne)]
>
> buildHoldMap           :: SFFile → ([InstrumentName], [PercussionSound]) → FileWork → Map Word Word → Map Word [PreZone]
> buildHoldMap _ _ FileWork{ .. } aMap     = foldl' makeHolds Map.empty zrecs
>   where
>     zrecs                                = goodZRecs fwDispositions fwZRecs
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
> reorgTaskIf sffile rost fwIn             = fwTask
>   where
>     fName_                               = "reorgTaskIf"
>
>     fwTask                               = zrecTask reorger fwIn
>
>     aMap                                 = buildAbsorptionMap sffile rost fwIn
>     hMap                                 = buildHoldMap sffile rost fwIn aMap
>
>     reorger            :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     reorger zrec rdFold                  = (zrec', rdFold')
>       where
>         fName                            = unwords [fName_, "reorger"]
>
>         aprobe                           = Map.lookup zrec.zswInst aMap
>         hprobe                           = Map.lookup zrec.zswInst hMap
>
>         aresult                          = deJust "aprobe" aprobe
>         hresult                          = deJust "hprobe" hprobe
>
>         pergm                            = instKey zrec
>         scansIng                         = finishScans fName noClue (modified pergm Absorbing)
>         scansEd                          = finishScans fName noClue (dropped pergm Absorbed)
>
>         (zrec', rdFold')
>           | not doAbsorption             = (zrec,                       rdFold)
>           | isNothing aprobe             = (zrec,                       rdFold)
>           | aresult == zrec.zswInst      = (zrec{zsPreZones = hresult}, dispose pergm scansIng rdFold)
>           | otherwise                    = (zrec,                       dispose pergm scansEd rdFold)
>
> shorten        :: (Char → Bool) → [Char] → [Char] 
> shorten qual chars                       = reverse (dropWhile qual (reverse chars))

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
>       Map.foldlWithKey (\m k v → Map.insert k (computeFFMatches v.cnName) m) Map.empty fwBoot.zPreSampleCache
>     iMatches                             =
>       Map.foldlWithKey (\m k v → Map.insert k (computeFFMatches v.piChanges.cnName) m) Map.empty fwBoot.zPreInstCache

categorization task ===================================================================================================
          assign each instrument to one of the three categories

> catTaskIf _ rost fwIn@FileWork{ .. }     = zrecTask catter fwIn
>   where
>     fName___                             = "catTaskIf"
>
>     catter             :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     catter zrec rdFold                   = (zrec{zsInstCat = Just (categorizeInst zrec)}, rdFold)
>
>     categorizeInst     :: InstZoneRecord → InstCat
>     categorizeInst zrec
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = deJust (unwords[fName__, "icat"]) icat
>       where
>         fName__                          = unwords [fName___, "categorizeInst"]
>         trace_CI                         =
>           unwords [fName__, iName, show (pergm.pgkwFile, pergm.pgkwInst)]
>
>         pergm                            = instKey zrec
>         preI                             = fwBoot.zPreInstCache Map.! pergm
>         iName                            = preI.piChanges.cnName
>         pzs                              = zrec.zsPreZones
>
>         canBePercZ     :: PreZone → Bool
>         canBePercZ x                     =
>           case x.pzDigest.zdKeyRange of
>             Just rng                     →    inRange rng ((fromIntegral . fst) percPitchRange)
>                                            && inRange rng ((fromIntegral . snd) percPitchRange)
>             Nothing                      → True
>         canBePercI     :: [PreZone] → Bool
>         canBePercI                       = all canBePercZ
>
>         -- Put the Instrument, of either category, through a gauntlet of checks.
>         -- This diverges, and then we have qualInstZone and qualPercZone 
>
>         pzsLocal                         = filter isLocal pzs
>         isLocal pz                       = not (isStereoZone pz) && not (hasCross pz)
>                                            
>         -- Determine which category will belong to the Instrument, based on its performance for
>         -- 1. all kinds
>         -- 2. "rost" subset, could be same as 1.
>
>         icatAllKinds, icatRost, icatNarrow, icat
>                        :: Maybe InstCat
>         icatAllKinds                     = foldl' CM.mplus Nothing (provideAlts Nothing allKinds)
>         icatRost                         = foldl' CM.mplus Nothing (provideAlts icatAllKinds rost)
>         icatNarrow                       = Just (InstCatDisq "narrow" (dropped pergm Narrow))
>         icat                             =
>           case (icatAllKinds, icatRost) of
>             (Just (InstCatInst _), Just (InstCatInst _))
>                                          → icatRost
>             (Just (InstCatPerc _), Just (InstCatPerc _))
>                                          → icatRost
>             (Just (InstCatDisq _ _), _)  → icatAllKinds
>             (_                   , Just (InstCatDisq _ _))
>                                          → icatRost
>             _                            → icatNarrow
>
>         corrupt                          = foldl' byZone Nothing pzs
>           where
>             byZone     :: Maybe InstCat → PreZone → Maybe InstCat
>             byZone target prez           =
>               foldl' CM.mplus target [checkGMRange prez.pzDigest.zdKeyRange, checkGMRange prez.pzDigest.zdVelRange]
>
>         checkGMRange   :: (Num a, Ord a, Show a) ⇒ Maybe (a, a) → Maybe InstCat
>         checkGMRange mrng                =
>           mrng >>= \(j, k) → if (0 <= j) && j <= k && k < fromIntegral qMidiSize128
>                                then Nothing
>                                else Just $ InstCatDisq (show mrng) (violated pergm CorruptGMRange)
>
>         hasRom pz                        = F.sampleType (effPZShdr pz) >= 0x8000
>                                          
>         checkLinkage   :: Maybe InstCat
>         checkLinkage                     =
>           if isSafe sList || requiredZoneLinkage < 1
>             then Nothing
>             else Just $ InstCatDisq noClue (violated pergm BadLinkage)
>           where
>             sList      :: [(Int, Int)]
>             sList                        = map (\z → (extractIndex z, extractLink z)) pzsLocal
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
>         paired         :: ∀ a . (Eq a, Ord a, Show a) ⇒ Map a a → a → Bool
>         paired target x                  = x == z
>           where
>             y                            = target Map.! x
>             z                            = target Map.! y
>
>         extractIndex, extractLink
>                        :: PreZone → Int
>         extractIndex pz                  = fromIntegral $ deJust "extractIndex" pz.pzDigest.zdSampleIndex
>         extractLink pz                   = fromIntegral $ F.sampleLink (effPZShdr pz)
>
>         rejectCrosses  :: Maybe InstCat
>         rejectCrosses                 =
>           if any hasCross pzs
>             then Just $ InstCatDisq noClue (violated pergm IllegalCrossover)
>             else Nothing
>
>         hasCross       :: PreZone → Bool
>         hasCross pz                      =
>           isStereoZone pz && notElem (extractLink pz) (map extractLink pzs)
>
>         howLaden       :: [Word] → Double
>         howLaden ws
>           | null pzs                     = 0
>           | otherwise                    = (fromIntegral . length) ws / (fromIntegral . length) pzs
>
>         maybeSettle    :: (Foldable t, Show (t Fuzz)) ⇒ Fuzz → InstCat → t Fuzz → Maybe InstCat
>         maybeSettle thresh ic keys       = find (> thresh) keys >> Just ic

   "now", sequence through the alternatives, categorizing encountered instruments as follows:
   a. Just InstCatInst           an inst bearing one inst, or
   b. Just InstCatPerc           an inst bearing one or more percs, or
   c. Just InstCatDisq           an inst disqualified from tournaments, or
   d. Nothing                    undecided

>         provideAlts    :: Maybe InstCat → ([InstrumentName], [PercussionSound]) → [Maybe InstCat]
>         provideAlts seed srost
>           | traceNot trace_PA False      = undefined
>           | otherwise                    =
>           if isNothing seed
>             then structuralAlts ++ functionalAlts allKinds
>             else functionalAlts srost
>           where
>             fName_                       = unwords [fName__, "provideAlts"]
>             trace_PA                     =
>               unwords [fName_, showMaybeInstCat seed, show (length pzs), show (BF.bimap length length srost)]
>
>             structuralAlts               =
>               [if null pzs
>                  then Just $ InstCatDisq noClue (violated pergm NoZones)
>                  else Nothing
>               , corrupt
>               , if any hasRom pzs
>                   then Just (InstCatDisq noClue (violated pergm RomBased))
>                   else Nothing
>               , if allowSpecifiedCrossovers || allowInferredCrossovers
>                   then Nothing
>                   else rejectCrosses
>               , checkLinkage]
>             functionalAlts frost
>               | traceNot trace_FA False  = undefined
>               | otherwise                =
>               let
>                 iMatches                 = fromJust $ Map.lookup pergm fwMatches.mIMatches
>                 ffInst'                  =
>                   Map.filterWithKey (\k v → k `elem` select frost && isPossible' v) iMatches.ffInst
>                 ffPerc'                  =
>                   Map.filterWithKey (\k v → k `elem` select frost && isPossible' v) iMatches.ffPerc
>               in
>                 [ 
>                     maybeSettle isConfirmed catInst                  ffInst'
>                   , maybeSettle isConfirmed (catPerc wZones)         ffPerc'
>                   , maybeNailAsPerc 0.6 
>                   , maybeSettle stands      catInst                  ffInst'
>
>                   , maybeSettle stands      (catPerc wZones)         ffPerc'
>                   , maybeSettle stands      (catDisq noClue (dropped pergm Narrow)) iMatches.ffInst
>
>                   , maybeNailAsPerc 0.4
>                   , Just $ catDisq noClue (dropped pergm Unrecognized)
>                 ]
>               where
>                 uZones :: [Word]         = fromMaybe wZones (getMaybePercList seed)
>                 wZones :: [Word]         = mapMaybe (qualPercZone frost) pzs
>
>                 maybeNailAsPerc
>                        :: Double → Maybe InstCat
>                 maybeNailAsPerc fr       =
>                   let
>                     uFrac                = howLaden uZones
>                     wFrac                = howLaden wZones
>                   in if canBePercI pzs && fr < uFrac
>                     then
>                       (if 0.2 < wFrac
>                          then Just (catPerc wZones)
>                          else Just (catDisq (show (uFrac, wFrac)) (violated pergm NoPercZones)))
>                     else Nothing
>
>                 trace_FA = unwords [fName_, iName, show frost, show (length uZones, length wZones)]
>
>             catInst      :: InstCat      =
>               if null pzs
>                 then InstCatDisq noClue (violated pergm NoZones)
>                 else (case checkSmashing pergm smashup of
>                        Nothing           → InstCatInst icd
>                        Just scans        → InstCatDisq "smashing" scans)
>               where
>                 smashup                  = computeInstSmashup pzs
>                 icd                      = InstCatData pzs smashup []
>
>             catPerc      :: [Word] → InstCat
>             catPerc ws
>               | traceNot trace_CP False  = undefined
>               | otherwise                =
>               if null pzs || null ws
>                 then InstCatDisq "catPerc" (dropped pergm NoZones)
>                 else (case check of
>                        Nothing           → InstCatPerc icd
>                        Just scans        → InstCatDisq "smashing" scans)
>               where
>                 fName                    = unwords[fName_, "catPerc"]
>                 trace_CP                 = unwords [fName, show (length ws, length pzs)]
>
>                 pzs'                     = filter (\x → x.pzWordB `elem` ws) pzs
>                 smashup                  = computeInstSmashup pzs'
>                 icd                      = InstCatData pzs' smashup ws
>                 check                    = checkSmashing pergm smashup
>
>             catDisq    :: String → [Scan] → InstCat
>             catDisq                      = InstCatDisq
>
>         qualPercZone   :: ([InstrumentName], [PercussionSound]) → PreZone → Maybe Word
>         qualPercZone qrost prez
>           | traceNot trace_QPZ False     = undefined
>           | otherwise                    = result
>           where
>             mrange                       =
>               notracer
>                 "mrange" (prez.pzDigest.zdKeyRange >>= (Just . BF.bimap fromIntegral fromIntegral))
>             result                       =
>               mrange
>               >>= pinnedKR (select qrost)
>               >> Just prez.pzWordB
>
>             trace_QPZ                    = unwords ["qualPercZone", show prez.pzWordB, show result]

encat task ============================================================================================================
          react to categorization now present in the zrecs

> encatTaskIf _ _                          = zrecTask encatter
>   where
>     fName_                               = "encatTaskIf"
>
>     encatter           :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     encatter zrec rdFold                 =
>       let
>         fName                            = unwords [fName_, "encatter"]
>
>         pergm                            = instKey zrec
>         icat                             = deJust fName zrec.zsInstCat
>       in
>         case icat of
>           InstCatPerc _                  → (zrec, dispose pergm [Scan Modified CatIsPerc fName noClue] rdFold)
>           InstCatInst _                  → (zrec, dispose pergm [Scan Modified CatIsInst fName noClue] rdFold)
>           InstCatDisq why ss             → (zrec, dispose pergm (finishScans fName why ss)             rdFold)

zone task =============================================================================================================
          generate the PerInstrument map from jobs map

> zoneTaskIf sffile _ fwIn@FileWork{ .. }  =
>   fwIn{  fwBoot = fwBoot{zPerInstCache = fst formZoneCache}
>        , fwDispositions = snd formZoneCache}
>   where
>     fName                                = unwords ["zoneTaskIf", "formZoneCache", "computePerInst"]
>
>     formZoneCache      :: (Map PerGMKey PerInstrument, ResultDispositions)
>     formZoneCache                        = 
>       Map.foldlWithKey formFolder (Map.empty, fwDispositions) fwBoot.zJobs
>       where
>         formFolder     :: (Map PerGMKey PerInstrument, ResultDispositions)
>                           → PerGMKey → InstCat
>                           → (Map PerGMKey PerInstrument, ResultDispositions)
>         formFolder (zc, rdFold) pergm icat
>                                          =
>           (Map.insert pergm (computePerInst pergm icat) zc, dispose pergm [Scan Accepted Ok fName noClue] rdFold)
>
>         computePerInst :: PerGMKey → InstCat → PerInstrument
>         computePerInst pergm icat
>           | traceIf trace_CPI False      = undefined
>           | otherwise                    = PerInstrument (zip pzs oList) icd.inSmashup
>           where
>             preI                         = fwBoot.zPreInstCache Map.! pergm
>             iName                        = preI.piChanges.cnName
>
>             icd        :: InstCatData
>             bixen      :: [Word]
>
>             (icd, bixen)                 =
>               case icat of
>                 InstCatPerc x            → (x, x.inPercBixen)
>                 InstCatInst x            → (x, map pzWordB x.inPreZones)
>                 _                        → error $ unwords [fName, "only Inst and Perc are valid here"]
>
>             gZone                        =
>               case preI.iGlobalKey of
>                 Nothing                  → defZone
>                 Just pzk                 → buildZone sffile defZone Nothing pzk.pzkwBag
>             oList                        = map (\pz → buildZone sffile gZone (Just pz) pz.pzWordB) pzs
>
>             pzs                          = filter (\pz → pz.pzWordB `elem` bixen) icd.inPreZones
>
>             trace_CPI=
>               unwords [fName, show pergm.pgkwFile, iName, show (length oList)]
>
> buildZone              :: SFFile → SFZone → Maybe PreZone → Word → SFZone
> buildZone sffile fromZone mpz bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = zone
>   where
>     zName                                 =
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
> addMod (mId, F.Mod{srcOper, destOper, amtSrcOper, amount}) iz@SFZone{zModulators} 
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

> rejobsTaskIf _ _ fwIn@FileWork{ .. }     =
>   fwIn{fwBoot = fwBoot{zJobs = zrecCompute fwIn foldJob Map.empty}}
>     where
>       foldJob          :: Map PerGMKey InstCat → InstZoneRecord → Map PerGMKey InstCat
>       foldJob m zrec                     = Map.insert (instKey zrec) (deJust "InstCat" zrec.zsInstCat) m

repartner task ========================================================================================================
          generate partial partner map with and for crossovers only, from the full map and zrec list

> formPartnerMap         :: FileWork → (PreZone → Bool) → Map PreZoneKey PreZone
> formPartnerMap FileWork{ .. } pzFun      = 
>   let
>     filePzs                              =
>       foldl' (\pzs zrec → pzs ++ filter pzFun zrec.zsPreZones)
>              []
>              (goodZRecs fwDispositions fwZRecs)
>   in
>     formPreZoneMap filePzs
>
> formFullPartnerMap, formPartialPartnerMap
>                        :: FileWork -> Map PreZoneKey PreZone
> formFullPartnerMap fw                    = formPartnerMap fw isStereoZone
> formPartialPartnerMap fw                 = formPartnerMap fw (isStereoCrossoverZone fw.fwBoot.zPartnerMap)
>
> repartnerTaskIf _ _ fwIn@FileWork{ .. }  = fwIn{fwBoot = fwBoot{zPartnerMap = partnerMap}}
>   where
>     partnerMap                           = formPartialPartnerMap fwIn
>
> sampleSizeOk           :: (Word, Word) → Bool
> sampleSizeOk (stS, enS)                  = stS >= 0 && enS - stS >= 0 && enS - stS < 2 ^ (22::Word)
>
> {-
> sampleLoopSizeOk       :: (Word, Word, A.SampleMode) → Bool
> sampleLoopSizeOk (stL, enL, smode)       = 
>   A.NoLoop == smode || (stL >= 0 && enL - stL >= sampleSizeMin && enL - stL < 2 ^ (22::Word))
>
> adjustedSampleSizeOk   :: ZoneDigest → F.Shdr → Bool
> adjustedSampleSizeOk zd shdr             = 0 <= stA && stA <= enA && 0 <= stL && stL <= enL
>   where
>     stA                                  = shdr.start     + fromIntegral zd.zdStart
>     enA                                  = shdr.end       + fromIntegral zd.zdEnd
>     stL                                  = shdr.startLoop + fromIntegral zd.zdStartLoop
>     enL                                  = shdr.endLoop   + fromIntegral zd.zdEndLoop
> -}
>
> goodChar               :: Char → Bool
> goodChar cN                              = isAscii cN && not (isControl cN)
>
> goodName               :: String → Bool
> goodName                                 = all goodChar
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
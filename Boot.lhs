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

> module Boot
>        (  equipInstruments
>         , extractInstKey
>         , extractZoneKey
>         , listInstruments
>         , shorten
>         , sLength
>         , writeCategorizationReport
>         )
>         where
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
> import Debug.Trace ( traceIO )
> import Euterpea.IO.MIDI.GeneralMidi()
> import Euterpea.Music
> import Modulation
> import Parthenopea.Debug
> import SoundFont
> import qualified System.FilePattern.Directory
>                                          as FP
  
importing sampled sound (from SoundFont (*.sf2) files) ================================================================

> data FileWork =
>   FileWork {
>     fwBoot             :: SFBoot
>   , fwZRecs            :: [InstZoneRecord]
>   , fwDispositions     :: ResultDispositions}
> instance Show FileWork where
>   show (FileWork{ .. })                  =
>     unwords [  "FileWork"
>              , show fwBoot
>              , show (length fwZRecs), "=zrecs"
>              , show fwDispositions]
> defFileWork            :: FileWork
> defFileWork                              =
>   FileWork dasBoot [] virginrd
>
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
>
> preSampleTaskIf, partneringTaskIf, preInstTaskIf, surveyTaskIf, captureTaskIf, pregroomTaskIf
>                , groomTaskIf, vetTaskIf, prereorg1TaskIf, prereorg2TaskIf, reorgTaskIf
>                , harvestTaskIf, shavingTaskIf, catTaskIf {-, zoneTaskIf -}
>                        :: SFFile → ([InstrumentName], [PercussionSound]) → FileWork → FileWork
>
> makeFileIterate        :: SFFile → ([InstrumentName], [PercussionSound]) → FileIterate
> makeFileIterate sffile rost              =
>   FileIterate
>     defFileWork 
>     [  ("preSample",  preSampleTaskIf    sffile rost)
>      , ("partnering", partneringTaskIf   sffile rost)
>      , ("preInst",    preInstTaskIf      sffile rost)
>      , ("survey",     surveyTaskIf       sffile rost)
>      , ("capture",    captureTaskIf      sffile rost)
>      , ("pregroom",   pregroomTaskIf     sffile rost)
>      , ("groom",      groomTaskIf        sffile rost)
>      , ("vet",        vetTaskIf          sffile rost)
>      , ("harvest 1",  harvestTaskIf      sffile rost)
>      , ("prereorg1",  prereorg1TaskIf    sffile rost) 
>      , ("prereorg2",  prereorg2TaskIf    sffile rost) 
>      , ("reorg",      reorgTaskIf        sffile rost) 
>      , ("harvest 2",  harvestTaskIf      sffile rost)
>      , ("shaving",    shavingTaskIf      sffile rost)
>      , ("cat",        catTaskIf          sffile rost) {-, zoneTaskIf -} ]
>
> reduceFileIterate      :: FileIterate → IO (SFBoot, ResultDispositions)
> reduceFileIterate fiIn                   = do
>   let FileWork{ .. }                     = fiIn.fiFw
>   return (fwBoot, fwDispositions)

executive =============================================================================================================

> listInstruments        :: IO ()
> listInstruments                          = do
>   (mrunt, pergmsI, pergmsP, rdGen03)     ← equipInstruments allKinds
>   if isJust mrunt
>     then do
>       let runt                           = deJust "mrunt" mrunt
>       writeCategorizationReport runt pergmsI pergmsP
>       CM.when reportScan (writeScanReport runt rdGen03)
>     else do
>       return ()

To support extracting from flawed SoundFont files, we - up front - withdraw unrecoverable items from
their respective collections. The withdrawn items are ignored by all later phases. When constructing those
later items, some critical data may thereby be missing. So that entails deletion-recovery also.

> equipInstruments       :: ([InstrumentName], [PercussionSound])
>                           → IO (Maybe SFRuntime, [PerGMKey], [PerGMKey], ResultDispositions)
> equipInstruments rost                    = do
>   tsStarted                              ← getCurrentTime
>
>   putStrLn $ unwords ["rost", show rost]
>
>   -- represent all input SoundFont files in ordered list, thence a vector
>   fps                                    ← FP.getDirectoryFiles "." (singleton "*.sf2")
>   if null fps
>     then do
>       putStrLn "no *.sf2 files found"
>       return (Nothing, [], [], virginrd)
>     else do
>       let nfiles                         = length fps
>       let boundsF::(Word, Word)          = (0, fromIntegral (nfiles - 1))
>       sffilesp                           ← CM.zipWithM openSoundFontFile [0..] fps
>       let vFiles                         = listArray boundsF sffilesp
>
>       tsLoaded                           ← getCurrentTime
>       putStrLn ("___load files: " ++ show (diffUTCTime tsLoaded tsStarted))
>
>       -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>       (bootAll, rdGen03)                 ← CM.foldM bootFolder (dasBoot, virginrd) vFiles
>       let runt                           = SFRuntime vFiles bootAll Map.empty seedWinningRecord
>
>       tsBooted                           ← getCurrentTime
>       putStrLn ("___booted: " ++ show (diffUTCTime tsBooted tsLoaded))
>
>       (pergmsI, pergmsP)                 ← enumGMs bootAll.zJobs
>       putStrLn $ unwords ["length pergmsI, pergmsP", show (length pergmsI, length pergmsP)]
>
>       tsFinished                         ← getCurrentTime
>       putStrLn ("___sorted: " ++ show (diffUTCTime tsFinished tsBooted))
>
>       return (Just runt, pergmsI, pergmsP, rdGen03 )
>   where
>     bootFolder         :: (SFBoot, ResultDispositions) → SFFile → IO (SFBoot, ResultDispositions)
>     bootFolder (preBoot, rdIn) sffile       = do
>       (boot, rdOut)                         ← reduceFileIterate (ingestFile sffile)
>       CM.when diagnosticsEnabled (putStrLn $ unwords ["bootFolder", show boot])
>       return (combineBoot preBoot boot, combinerd rdIn rdOut)
>
>     ingestFile         :: SFFile → FileIterate
>     ingestFile sffile                    =
>       head $ dropWhile unfinished (iterate' nextGen (makeFileIterate sffile rost))
>       where
>         unfinished fiIn                  = not (null fiIn.fiTaskIfs)
>         nextGen fiIn                     = fiIn{ fiFw = modify fiIn
>                                                , fiTaskIfs = tail fiIn.fiTaskIfs}
>         modify fiIn                      = 
>           let
>             fwIn                         = fiIn.fiFw
>             namedFun                     = head fiIn.fiTaskIfs
>             name                         = fst namedFun
>             taskIf     :: FileWork → FileWork
>             taskIf                       = snd namedFun
>           in
>             taskIf fwIn
>
> enumGMs                :: Map PerGMKey InstCat → IO ([PerGMKey], [PerGMKey])
> enumGMs jobs                             = CM.foldM enumFolder ([], []) (Map.assocs jobs)
>   where
>     enumFolder         :: ([PerGMKey], [PerGMKey]) → (PerGMKey, InstCat) → IO ([PerGMKey], [PerGMKey])
>     enumFolder (pergmsI, pergmsP) (pergmI_, icat)
>                                          =
>       let
>         pergmI                           = pergmI_{pgkwBag = Nothing}
>       in
>         return $
>         case icat of
>           InstCatPerc icd                → (pergmsI, pergmsP ++ instrumentPercList pergmI icd.inPercBixen)
>           InstCatInst _                  → (pergmI : pergmsI, pergmsP)
>           InstCatDisq _ _                → (pergmsI, pergmsP)
>
>     instrumentPercList :: PerGMKey → [Word] → [PerGMKey]
>     instrumentPercList pergmI            = map (\w → pergmI {pgkwBag = Just w})

task interface support ================================================================================================

> formComprehension      :: ∀ r a . SFResource r ⇒ SFFile → (FileArrays → Array Word a) → [r]
> formComprehension sffile blobfun
>   | traceNot trace_FC False               = undefined
>   | otherwise                            = map (sfkey sffile.zWordF) bRange
>   where
>     fName                                = "formComprehension"
>     trace_FC                             = unwords [fName, show bRange]
>
>     (stF, enF)         :: (Word, Word)   = bounds $ blobfun sffile.zFileArrays
>     bRange                               =
>       profess
>         ((stF == 0) && (stF <= enF) && (enF < 2_147_483_648))
>         (error $ unwords [fName, "corrupt blob"])
>         (deriveRange stF enF)

access and critique all Sample records in the file ====================================================================

> preSampleTaskIf sffile _ fwIn            = foldl' formFolder fwIn (formComprehension sffile ssShdrs)
>   where
>     fName_                               = "preSampleTaskIf"
>
>     formFolder         :: FileWork → PreSampleKey → FileWork
>     formFolder fwForm@FileWork{ .. } presk
>       | traceNot trace_FF False          = undefined
>       | otherwise                        = 
>       if dead ss
>         then fwForm{  fwDispositions = rd'}
>         else fwForm{  fwBoot = fwBoot{zPreSampleCache
>                                         = Map.insert presk (computePreSample shdr) fwBoot.zPreSampleCache}
>                     , fwDispositions = rd'}
>       where
>         fName                            = unwords [fName_, "formFolder"]
>         trace_FF                         = unwords [fName, show fwForm]
>
>         shdr@F.Shdr{ .. }                = sffile.zFileArrays.ssShdrs ! presk.pskwSampleIndex
>
>         (ss_, clue)                      
>           | not (goodName sampleName)    = (violated presk CorruptName, sampleName)
>           | not (goodSampleRate sampleRate)
>                                          = (violated presk BadSampleRate, show sampleRate)
>           | isNothing (toMaybeSampleType sampleType)
>                                          = (violated presk BadSampleType, show sampleType)
>           | not (sampleSizeOk (start, end))
>                                          = (violated presk BadSampleLimits, show (start, end))
>           | otherwise                    = (accepted presk Ok, show presk.pskwSampleIndex)
>         ss                               = finishScans fName clue ss_
>
>         rd' = dispose presk ss fwDispositions

assess declared stereo pairs ==========================================================================================

> partneringTaskIf sffile _ fwIn@FileWork{ .. }
>                                          = fwIn{  fwBoot = fwBoot{zPreSampleCache = preSampleCache'
>                                                                 , zPartnerMap     = partnerMap}
>                                                 , fwDispositions   = rd'}
>   where
>     fName_                               = "partneringTaskIf"
>
>     (preSampleCache', partnerMap, rd')   =
>       foldl' partneringFolder (Map.empty, Map.empty, fwDispositions) (Map.assocs fwBoot.zPreSampleCache)
>     partneringFolder   :: (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
>                           → (PreSampleKey, PreSample)
>                           → (Map PreSampleKey PreSample, Map PreSampleKey PreSampleKey, ResultDispositions)
>     partneringFolder (target, sPartnerMap, rdFold) (k, v)
>       | dead ss                          = (target,                sPartnerMap,                  rd'')
>       | cancels ss                       = (Map.insert k v target, sPartnerMap,                  rd'') 
>       | otherwise                        = (Map.insert k v target, makePartner (Just otherKey),  rd'')
>       where
>         fName                            = unwords [fName_, "partneringFolder"]
>
>         makePartner                      =
>           \case
>             Nothing                      → sPartnerMap
>             Just preskPartner            → Map.insert k preskPartner sPartnerMap
>
>         otherKey                         = PreSampleKey k.pskwFile shdr.sampleLink
>         other                            = Map.lookup otherKey fwBoot.zPreSampleCache
>         oBackLink                        = if F.sampleLink oshdr == k.pskwSampleIndex
>                                              then Just otherKey
>                                              else Nothing
>         backLink                         = other >> oBackLink
>
>         shdr                             = sffile.zFileArrays.ssShdrs ! k.pskwSampleIndex
>         oshdr                            = sffile.zFileArrays.ssShdrs ! otherKey.pskwSampleIndex
>         stype                            = toSampleType shdr.sampleType
>         stereo                           = SampleTypeLeft == stype || SampleTypeRight == stype
>         rd''                             = dispose k (finishScans fName clue ss_) rdFold
>         clueOther                        = show otherKey.pskwSampleIndex
>         (ss_, clue)
>           | not stereo                   = (noChange k Ok,                   "mono")
>           | isNothing other              = (violated k MissingStereoPartner, show clueOther)
>           | isNothing backLink           = (violated k BadStereoPartner,     show clueOther)
>           | otherwise                    = (modified k Ok,                   show clueOther)
>         ss                               = finishScans fName clue ss_

access and critique all Instrument "records" in the file ==============================================================

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
>       | traceNot trace_PIF False         = undefined
>       | otherwise                        =
>       if iinst.instBagNdx < jinst.instBagNdx
>         then (m', rd'')
>         else error $ unwords [fName, "corrupt instBagNdx"]
>       where
>         fName                            = unwords [fName_, "preIFolder"]
>         trace_PIF                        = unwords [fName, show pergm]
>
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pgkwInst + 1}
>
>         nm                               = iinst.instName
>
>         m'                               =
>           if cancels ss
>              then m 
>              else Map.insert pergm (PreInstrument iinst nm (computeFFMatches nm) Nothing) m
>
>         (ss_, clue)
>           | not (goodName iinst.instName)
>                                          = (violated pergm CorruptName, iinst.instName)
>           | otherwise                    = (accepted pergm Ok, show pergm.pgkwInst)
>         ss                               = finishScans fName clue ss_
>
>         rd''                             = dispose pergm ss rdFold   
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm                       = boota.ssInsts ! pergm.pgkwInst
>       where
>         boota                            = sffile.zFileArrays

PreZone administration ================================================================================================

> goodZRecs              :: [InstZoneRecord] → ResultDispositions → [InstZoneRecord]
> goodZRecs zrecs rdNow                    = filter (\x → not (deadrd (instKey x) rdNow)) zrecs
>
> badZRecs               :: [InstZoneRecord] → ResultDispositions → [InstZoneRecord]
> badZRecs zrecs rdNow                     = filter (\x → deadrd (instKey x) rdNow) zrecs
>
> data InstZoneRecord                      =
>   InstZoneRecord {
>     zswFile            :: Word
>   , zswInst            :: Word
>   , zswGBix            :: Maybe Word
>   , zsPreZones         :: [PreZone]}
> instance Show InstZoneRecord where
>   show (InstZoneRecord{ .. })            = unwords ["InstZoneRecord", show zswFile, show zswInst, show zswGBix]
> makeZRec               :: PerGMKey → InstZoneRecord
> makeZRec pergm
>   | traceNot trace_MZR False             = undefined
>   | otherwise                            = InstZoneRecord pergm.pgkwFile pergm.pgkwInst Nothing []
>   where
>     fName                                = "makeZRec"
>     trace_MZR                            = unwords [fName, show pergm]
>
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             = PerGMKey zrec.zswFile zrec.zswInst Nothing       

survey task ===========================================================================================================
          instantiate zrecs

> surveyTaskIf _ _ fwIn@FileWork{ .. }     =
>   fwIn{fwZRecs = map makeZRec (Map.keys fwBoot.zPreInstCache)}

iterating on InstZoneRecord list ======================================================================================

> zrecTask               :: (InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions))
>                           → SFFile → FileWork → FileWork
> zrecTask userFun _ fwIn@FileWork{ .. }
>   | traceNow trace_ZT False              = undefined
>   | otherwise                            = fwIn{  fwZRecs = zrecs
>                                                 , fwDispositions = rd' }
>   where
>     fName                                = "zrecTask"
>     trace_ZT                             = unwords [fName, show fwIn]
>
>     (zrecs, rd')                         = foldl' taskRunner ([], fwDispositions) fwZRecs
>
>     taskRunner (zrecsFold, rdFold) zrec  = 
>       let
>         (zrec', rdFold')                    = userFun zrec rdFold
>       in
>         if deadrd (instKey zrec) rdFold 
>           then (zrec : zrecsFold, rdFold)
>           else (zrec': zrecsFold, rdFold')
>
> zrecCompute              :: FileWork → (a → InstZoneRecord → a) → a → a
> zrecCompute FileWork{ .. } folder seed   = foldl' folder seed (goodZRecs fwZRecs fwDispositions)

capture task ==========================================================================================================
          for the first time, populate zrec with PreZones

> captureTaskIf sffile _ fwIn@FileWork{ .. }
>                                          = zrecTask capturer sffile fwIn 
>   where
>     capturer           :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     capturer zrec rdIn                   =
>       let
>         (newPzs, rd')                    = captureZones pergm rdIn
>         pergm                            = instKey zrec
>       in
>         (zrec{zsPreZones = newPzs}, rd')
>
>     captureZones       :: PerGMKey → ResultDispositions → ([PreZone], ResultDispositions)
>     captureZones pergm rdIn              = (pzsRemaining, rd')
>       where
>         fName_                           = unwords["captureInstZones"]
>
>         (ss_, clue)
>           | null pzsRemaining            = (violated pergm NoZones, "null")
>           | otherwise                    = (accepted pergm Ok, show (length pzsRemaining))
>         ss                               = finishScans fName_ clue ss_ 
>         rd'                              = dispose pergm ss rdIn
>         results                          = map captureZone (deriveRange ibagi jbagi)
>
>         ibagi                            = F.instBagNdx (sffile.zFileArrays.ssInsts ! wIn)
>         jbagi                            = F.instBagNdx (sffile.zFileArrays.ssInsts ! (wIn+1))
>
>         wIn                              = pgkwInst pergm
>
> {-
>             mGBKey                       = if head results == Right "global zone"
>                                              then Just ibagi
>                                              else Nothing
> -}
>         pzsRemaining                     = lefts results
>             
>         captureZone    :: Word → Either PreZone String
>         captureZone bix                  = zTry
>           where
>             fName                    = unwords [fName_, "captureZone"]
>                 
>             zTry
> {-
>                   | isNothing starget        =
>                     (  zscan, Map.singleton (instKey zscan) (violate OrphanedBySample  fName ""))
> -}
>               | isNothing pz.pzDigest.zdSampleIndex
>                                          = Right "global zone"
>               | isNothing starget        = Right (unwords [fName, "orphaned by sample"])
>               | not limitsCheckedOk      = Right (unwords [fName, "problem", "corrupt adjusted limits"]) 
>               | otherwise                = Left pz{pzChanges = pres.psChanges}
>
>             xgeni                        = F.genNdx $ sffile.zFileArrays.ssIBags ! bix
>             ygeni                        = F.genNdx $ sffile.zFileArrays.ssIBags ! (bix + 1)
>
>             gens   :: [F.Generator]
>             gens                         = profess
>                                              (xgeni <= ygeni)
>                                              (unwords [fName, "SoundFont file corrupt (gens)"])
>                                              (map (sffile.zFileArrays.ssIGens !) (deriveRange xgeni ygeni))
>             pz                           = makePreZone sffile.zWordF si wIn bix gens
>             si                           = deJust "produce si" pz.pzDigest.zdSampleIndex
>             shdr                         = sffile.zFileArrays.ssShdrs ! si
>
>             limitsCheckedOk              = adjustedSampleSizeOk pz.pzDigest shdr
>             presk                        = PreSampleKey sffile.zWordF si
>             starget                      = Map.lookup presk fwBoot.zPreSampleCache
>             pres                         = deJust "pres" starget

pregroom task =========================================================================================================

> pregroomTaskIf _ _ fwIn                  = fwIn{fwBoot = fwIn.fwBoot{zTempBackMap = back}} 
>   where
>     back               :: Map PreSampleKey [PreZoneKey]
>     back                                 = zrecCompute fwIn pregroomer Map.empty
>
>     pregroomer         :: Map PreSampleKey [PreZoneKey] → InstZoneRecord → Map PreSampleKey [PreZoneKey]
>     pregroomer m zrec                    = Map.union m m'
>       where
>         m'                               = 
>           foldl' pzFolder Map.empty (filter (isStereoZone fwIn.fwBoot.zPreSampleCache) zrec.zsPreZones)
>         pzFolder target pz               =
>           Map.insertWith (++) (PreSampleKey pz.pzWordF pz.pzWordS) [extractZoneKey pz] target

groom task ============================================================================================================

> groomTaskIf sffile _ fwIn@FileWork{ .. } = fwTask{fwBoot = fwBoot{zTempBackMap = Map.empty}}
>   where
>     fName_                               = "groomTaskIf"
>
>     back                                 = fwBoot.zTempBackMap
>
>     fwTask                               = zrecTask groomer sffile fwIn
>
>     groomer            :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     groomer zrec rdGroom_
>       | traceNot trace_G False           = undefined
>       | otherwise                        = (zrec{zsPreZones = newPzs}, rd')
>       where
>         fName                            = unwords [fName_, "groomer"]
>         trace_G                          = unwords [fName, show rdGroom_]
>
>         newPzs                           = groomPreZones zrec.zsPreZones
>
>         pergm                            = instKey zrec
>         rd'                              = dispose pergm ss rdGroom_
>
>         (ss_, clue)
>           | null newPzs                  = (violated pergm NoZones, "newPzs")
>           | otherwise                    = ([], "")
>         ss                               = finishScans fName clue ss_ 
>
>     groomPreZones preZones               = pzsStereo ++ pzsMono
>       where
>         (pzsStereo_, pzsMono)            = partition (isStereoZone fwBoot.zPreSampleCache) preZones
>         pzsStereo                        = map partnerUp pzsStereo_
>
>     partnerUp pz                         =
>       let
>         mpartners                        =
>           Map.lookup (PreSampleKey pz.pzWordF (F.sampleLink (effShdr fwBoot.zPreSampleCache pz))) back 
>       in
>         pz{pzmkPartners = fromMaybe [] mpartners}

vet task ============================================================================================================
          remove bad stereo partners from PreZones per instrument, delete instrument if down to zero PreZones

> vetTaskIf sffile _ fwIn@FileWork{ .. }   = zrecTask vetter sffile fwIn 
>   where
>     fName_                               = "vetTaskIf"
>
>     filePzs                              =
>       foldl'
>         (\x y → x ++ filter (isStereoZone fwBoot.zPreSampleCache) y.zsPreZones)
>         []
>         (goodZRecs fwZRecs fwDispositions)
>     mapStereo                            = formPreZoneMap filePzs
>
>     vetter             :: InstZoneRecord
>                           → ResultDispositions
>                           → (InstZoneRecord, ResultDispositions)
>     vetter zrec rdVet_
>       | traceNot trace_V False           = undefined
>       | otherwise                        = (zrec{zsPreZones = newPzs}, rd')
>       where
>         fName                            = unwords [fName_, "vetter"]
>         trace_V                          = unwords [fName, show rdVet_]
>
>         newPzs                           =
>           let
>             (pzsStereo, pzsMono)         = partition (isStereoZone fwBoot.zPreSampleCache) zrec.zsPreZones
>
>             vetPreZone :: PreZone → Maybe PreZone
>             vetPreZone pz                =
>               if null newPartners
>                 then if canDevolveToMono
>                        then Just $ appendChange pz MakeMono
>                        else Nothing
>                 else Just pz{pzmkPartners = newPartners}
>               where
>                 newPartners              = filter (okPartner pz) pz.pzmkPartners
>           in
>             mapMaybe vetPreZone pzsStereo ++ pzsMono
>
>         pergm                            = instKey zrec
>         netChange                        = length zrec.zsPreZones - length newPzs
>         rd'                              = dispose pergm ss rdVet_
>
>         (ss_, clue)
>           | null newPzs                  = (violated pergm NoZones, "newPzs")
>           | netChange /= 0               = (modified pergm Ok, show netChange) 
>           | otherwise                    = (noChange pergm Ok, show (length newPzs))
>         ss                               = finishScans fName clue ss_
>
>         okPartner      :: PreZone → PreZoneKey → Bool
>         okPartner pz pzk                 =
>           case Map.lookup pzk mapStereo of
>             Nothing                      → False
>             Just pzPartner               → goodPartners pz pzPartner
>
>         goodPartners   :: PreZone → PreZone → Bool
>         goodPartners pzMe pzYou          =
>           let
>             mySPartner                   =
>               PreSampleKey pzMe.pzWordF   (F.sampleLink (effShdr fwBoot.zPreSampleCache pzMe))
>             yrSPartner                   =
>               PreSampleKey pzYou.pzWordF  (F.sampleLink (effShdr fwBoot.zPreSampleCache pzYou))
>           in
>             (Just yrSPartner == Map.lookup mySPartner fwBoot.zPartnerMap)
>             && (Just mySPartner == Map.lookup yrSPartner fwBoot.zPartnerMap)
>

2 prereorg tasks, and then the reorg task itself ======================================================================

Overview: the member instruments of a qualified group will be absorbed into the lead (member).
That mapping (member → lead)  (Word → Word) is turned into aMap

prereorg1TaskIf builds aMap, called zTempWordMap when persisted

To build the map
 1. collects all instrument names

 2. build fragsets by grouping similarly named instruments

 3. groups them, yielding [[(String, Word)]], shrinks that to [[Word]]

 4. expands to [(Word, [Word])]

 5. filters that list via qualifySet

> prereorg1TaskIf sffile _ fwIn
>   | traceNow trace_PR1T False            = undefined
>   | otherwise                            = fwIn{fwBoot = fwIn.fwBoot{zTempWordMap = aMap}}
>   where
>     fName__                              = "prereorg1TaskIf"
>     trace_PR1T                           = unwords [fName__, show (length zrecs)]
>
>     rdNow                                = fwIn.fwDispositions
>     zrecs                                = goodZRecs fwIn.fwZRecs rdNow
>
>     oMap               :: Map PerGMKey [PreZone]
>     oMap                                 = fwIn.fwBoot.zOwners
>
>     aMap               :: Map Word Word
>     aMap                                 = foldl' fold1Fun Map.empty dissected
>
>     fold1Fun           :: Map Word Word → (Word, [Word]) → Map Word Word
>     fold1Fun mIn (wLead, wMembers)       = foldl' fold2Fun mIn wMembers
>       where
>         fold2Fun          :: Map Word Word → Word → Map Word Word
>         fold2Fun m' wMember              = Map.insert wMember wLead m'
>
>     dissected          :: [(Word, [Word])]
>     dissected                            = tracer "dissected" (filter qualifyAbsorptionGroup groupedC)
>       where
>         frags                            = concatMap enFrag zrecs
>         fragsets                         = sort $ mapMaybe deFrag frags
>         groupedA                         = filter (\x → 1 < length x) (groupBy (\x y → fst x == fst y) fragsets)
>         groupedB                         = map (map snd) groupedA
>         groupedC                         = map (\y → (head y, y)) groupedB        
>
>     enFrag             :: InstZoneRecord → [(String, Word)]
>     enFrag zrec                          =
>       let
>         preI                             =
>           deJust (unwords[ fName__, "preI"]) (Map.lookup (instKey zrec) fwIn.fwBoot.zPreInstCache)
>       in
>         singleton (preI.iName, zrec.zswInst)
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
>       | traceNow trace_QAG False         = undefined
>       | otherwise                        = answer
>       where
>         fName                            = unwords ["qualifySet"]
>
>         pergms                           = map (\wI → PerGMKey sffile.zWordF wI Nothing) memberIs
>         smashups                         = map smash pergms
>         pzLoads                          = map (\p → deJust "pzs" (Map.lookup p oMap)) pergms
>
>         fracOne        :: Rational
>         fracOne                          = 100 * fromRational ((maximum . map fractionCovered) smashups)
>
>         smashAll                         = smush (zip pzLoads smashups)
>         fracAll                          = 100 * fromRational (fractionCovered smashAll)
>         answer                           = fracAll / fracOne > 1.25
>
>         smash pergm                      = computeInstSmashup $ deJust "pzs" (Map.lookup pergm oMap)
>
>         trace_QAG                        =
>           unwords [fName, show leadI, show (fracOne, fracAll)
>                  , show "...."
>                  , show answer, show (fracAll / fracOne)]

prereorg2TaskIf builds hMap, called zTempHoldMap when persisted

> prereorg2TaskIf _ _ fwIn
>   | traceNow trace_PR2T False            = undefined
>   | otherwise                            = fwIn{fwBoot = fwIn.fwBoot{zTempHoldMap = hMap}}
>   where
>     fName__                              = "prereorg2TaskIf"
>     trace_PR2T                           = unwords [fName__, show rdNow]
>
>     rdNow                                = fwIn.fwDispositions
>     zrecs                                = goodZRecs fwIn.fwZRecs rdNow
>
>     aMap               :: Map Word Word
>     aMap                                 = fwIn.fwBoot.zTempWordMap
>
>     hMap               :: Map Word [PreZone]
>     hMap                                 = foldl' makeHolds Map.empty zrecs
>       where             
>         makeHolds      :: Map Word [PreZone] → InstZoneRecord → Map Word [PreZone]
>         makeHolds iHold zrec
>           | traceNot trace_MH False      = undefined
>           | otherwise                    =
>           if deadrd (instKey zrec) rdNow then iHold else exert -- WOX
>           where
>             fName_                       = unwords [fName__, "makeHolds"]
>             trace_MH                     = unwords [fName_, "iHold", show (length iHold), show (Map.keys iHold)]
>
>             exert
>               | traceNot trace_E False   = undefined
>               | otherwise                =
>               (\case
>                 Just target              → upd target
>                 Nothing                  → iHold) macts
>               where
>                 fName                    = unwords [fName_, "exert"]
>                 trace_E                  = unwords [fName, show macts]
>
>                 macts                    = Map.lookup zrec.zswInst aMap
>                 rebased                  = map rebase zrec.zsPreZones
>                 rebase pz                =
>                   (\case
>                     Just owner           → pz{pzWordI = owner}
>                     Nothing              → pz) macts
>
>                 upd target
>                   | traceNot trace_U False
>                                          = undefined
>                   | otherwise            = Map.insertWith (++) target rebased iHold
>                   where
>                     trace_U              = unwords [fName, "upd from/to", show (zrec.zswInst, target), show (length rebased)]

reorg task ============================================================================================================
          where indicated, make one instrument out of many

> reorgTaskIf sffile _ fwIn
>   | traceNow trace_RTIF False            = undefined
>   | otherwise                            =
>   fwTask{fwBoot = fwBoot{zTempWordMap = Map.empty
>                        , zTempHoldMap = Map.empty
>                        , zOwners      = Map.empty }}
>   where
>     fName__                              = "reorgTaskIf"
>     trace_RTIF                           = unwords [fName__, show (length hMap), show fwIn]
>
>     fwTask@FileWork{ .. }                = zrecTask reorger sffile fwIn
>
>     aMap                                 = fwBoot.zTempWordMap
>     hMap                                 = fwBoot.zTempHoldMap
>
>     reorger            :: InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions)
>     reorger zrec rdFold
>       | traceNow trace_G False           = undefined
>       | otherwise                        = (zrec', rdFold')
>       where
>         fName_                           = unwords [fName__, "reorger"]
>         trace_G                          = unwords [fName_, show (length hMap), show (instKey zrec)]
>
>         pergm                            = instKey zrec
>         aprobe                           = Map.lookup zrec.zswInst aMap
>         (zrec', rdFold')
>           | isNothing aprobe             = (zrec, rdFold)
>           | deJust "aprobe" aprobe == zrec.zswInst
>                                          = (  zrec{zsPreZones = deJust "hprobe" hprobe}
>                                             , dispose pergm (modified pergm Absorbing) rdFold)
>           | otherwise                    = (  zrec
>                                             , dispose pergm (dropped pergm Absorbed) rdFold)
>         hprobe                           = Map.lookup zrec.zswInst hMap
>
> shorten        :: (Char → Bool) → [Char] → [Char] 
> shorten qual chars                       = reverse (dropWhile qual (reverse chars))

harvest task ==========================================================================================================
          reap owners from zrecs

> harvestTaskIf _ _ fwIn@FileWork{ .. }    =
>   fwIn{fwBoot = fwBoot{zOwners = owners}
>        , fwDispositions = rd'}
>   where
>     fName                                = "harvestTaskIf"
>
>     (owners, rd')                        = zrecCompute fwIn harvestFolder (Map.empty, fwDispositions) 
>
>     harvestFolder      :: (Map PerGMKey [PreZone], ResultDispositions)
>                           → InstZoneRecord
>                           → (Map PerGMKey [PreZone], ResultDispositions)
>     harvestFolder (m, rdFold) zrec       =
>       let
>         pergm                            = instKey zrec
>         pzs                              = zrec.zsPreZones
>         rdFold'                          = dispose pergm [Scan Accepted Harvested fName noClue] rdFold
>         (owns, dispos)
>           | deadrd pergm rdFold          = error "dead instrument cannot participate"
>           | deadrd pergm rdFold'         = error "harvest action cannot kill"
>           | otherwise                    = (Map.insert pergm pzs m, rdFold')
>       in
>         (owns, dispos)

shaving task ==========================================================================================================
          remove dropped or violated instruments from the preInstCache

> shavingTaskIf _ _ fwIn@FileWork{ .. }    =
>   fwIn{fwBoot = fwBoot{zPreInstCache = preInstCache}}
>   where
>     preInstCache                         =
>       foldl' shavingFolder fwBoot.zPreInstCache (badZRecs fwZRecs fwDispositions)
>
>     shavingFolder      :: Map PerGMKey PreInstrument
>                           → InstZoneRecord
>                           → Map PerGMKey PreInstrument
>     shavingFolder m zrec                 = Map.delete (instKey zrec) m

categorization task ===================================================================================================
          assign each instrument to one of the three categories

> catTaskIf _ rost fwIn@FileWork{ .. }     =
>   fwIn{  fwBoot = fwBoot{zJobs = jobs, zOwners = Map.empty}
>        , fwDispositions = rd'}
>   where
>     jobs                                 = categorize fwBoot rost
>     rd'                                  = sortByCategory jobs fwDispositions
>
> sortByCategory         :: Map PerGMKey InstCat → ResultDispositions → ResultDispositions
> sortByCategory jobs rdIn                 = foldl' catFolder rdIn (Map.assocs jobs)
>   where
>     catFolder            :: ResultDispositions → (PerGMKey, InstCat) → ResultDispositions
>     catFolder rdFold (pergmI_, icat)     =
>       let
>         fName                            = "sortByCategory"
>         pergmI                           = pergmI_{pgkwBag = Nothing}
>       in
>         case icat of
>           InstCatPerc _                  → dispose pergmI [Scan Modified CatIsPerc fName noClue] rdFold
>           InstCatInst _                  → dispose pergmI [Scan Modified CatIsInst fName noClue] rdFold
>           InstCatDisq why _              → dispose pergmI [Scan Dropped Disqualified fName why] rdFold
>
> categorize             :: SFBoot → ([InstrumentName], [PercussionSound]) → Map PerGMKey InstCat
> categorize SFBoot{ .. } rost             = Map.mapWithKey categorizeInst zPreInstCache
>   where
>     categorizeInst     :: PerGMKey → a → InstCat
>     categorizeInst pergm _
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = deJust (unwords[fName__, "icat"]) icat
>       where
>         fName__                          = "categorizeInst"
>         trace_CI                         =
>           unwords [fName__, preI.iName, show (pergm.pgkwFile, pergm.pgkwInst)]
>
>         preI                             =
>           deJust (unwords [fName__, "PreInstrument"]) (Map.lookup pergm zPreInstCache)
>         mpzs                             = Map.lookup pergm zOwners
>         pzs                              = deJust (unwords[fName__, "owners"]) mpzs
>
>         -- Put the Instrument, of either category, through a gauntlet of checks.
>         -- This diverges, and then we have qualInstZone and qualPercZone 
>
>         pzsLocal                         = filter isLocal pzs
>         isLocal pz                       = not (isStereoZone zPreSampleCache pz) && not (hasCross pz)
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
>         hasRom pz                        = F.sampleType (effShdr zPreSampleCache pz) >= 0x8000
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
>         extractLink pz                   = fromIntegral $ F.sampleLink (effShdr zPreSampleCache pz)
>
>         rejectCrosses  :: Maybe InstCat
>         rejectCrosses                 =
>           if any hasCross pzs
>             then Just $ InstCatDisq noClue (violated pergm IllegalCrossover)
>             else Nothing
>
>         hasCross       :: PreZone → Bool
>         hasCross pz                      =
>           isStereoZone zPreSampleCache pz && notElem (extractLink pz) (map extractLink pzs)
>
>         howLaden       :: [Word] → Double
>         howLaden ws
>           | null pzs                     = 0
>           | otherwise                    = (fromIntegral . length) ws / (fromIntegral . length) pzs
>
>         maybeSettle    :: (Foldable t, Show (t Fuzz)) ⇒ Fuzz → InstCat → t Fuzz → Maybe InstCat
>         maybeSettle thresh ic keys       = find (> thresh) keys >> Just ic
>
>         genericScore                     = evalAgainstGeneric preI.iName

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
>               [if isNothing mpzs || null (fromJust mpzs)
>                  then Just $ InstCatDisq noClue (violated pergm NoZones)
>                  else Nothing
>               , corrupt
>               , if any hasRom pzs
>                   then Just (InstCatDisq noClue (violated pergm RomBased))
>                   else Nothing
>               , if allowStereoCrossovers
>                   then Nothing
>                   else rejectCrosses
>               , checkLinkage]
>             functionalAlts frost
>               | traceNot trace_FA False  = undefined
>               | otherwise                =
>               let
>                 ffInst'                  =
>                   Map.filterWithKey (\k v → k `elem` select frost && isPossible' v) preI.iMatches.ffInst
>                 ffPerc'                  =
>                   Map.filterWithKey (\k v → k `elem` select frost && isPossible' v) preI.iMatches.ffPerc
>               in
>                 [ 
>                     maybeSettle isConfirmed catInst                  ffInst'
>                   , maybeSettle isConfirmed (catPerc wZones)         ffPerc'
>                   , maybeNailAsPerc 0.6 
>                   , maybeSettle stands      catInst                  ffInst'
>
>                   , maybeSettle stands      (catPerc wZones)         ffPerc'
>                   , maybeSettle stands      (catDisq noClue (dropped pergm Narrow)) preI.iMatches.ffInst
>
>                   , maybeNailAsPerc 0.3
>                   , if genericScore > 0 then Just catInst            else Nothing
>                   , if genericScore < 0 then Just (catPerc wZones)   else Nothing
>                   , Just $ catDisq noClue (dropped pergm Unrecognized)
>                 ]
>               where
>                 uZones :: [Word]         = fromMaybe wZones (getMaybePercList seed)
>                 wZones :: [Word]         = mapMaybe (qualPercZone frost) pzs
>
>                 maybeNailAsPerc
>                        :: Double → Maybe InstCat
>                 maybeNailAsPerc fr       =
>                   if fr < howLaden uZones
>                     then
>                       (if 0.05 < howLaden wZones
>                          then Just (catPerc wZones)
>                          else Just (catDisq noClue (violated pergm NoPercZones)))
>                     else Nothing
>
>                 trace_FA = unwords [fName_, preI.iName, show frost, show (length uZones, length wZones)]
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
>               if null pzs || null ws || (null . init) ws
>                 then InstCatDisq "narrow" (dropped pergm Narrow)
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
>
> writeCategorizationReport
>                        :: SFRuntime → [PerGMKey] → [PerGMKey] → IO ()
> writeCategorizationReport runt pergmsI pergmsP
>                        = do
>   tsStarted            ← getCurrentTime
>
>   -- output all selections to the report file
>   let esFiles          = emitFileListC ++ [EndOfLine]
>   let esI              = concatMap (dumpInstrument runt) pergmsI
>   let esP              = concatMap (dumpPercussion runt) pergmsP
>   let esTail           = singleton $ Unblocked "\n\nThe End\n\n"
>   let eol              = singleton EndOfLine
>
>   writeFileBySections reportCategorizationName [esFiles, esI, eol, esFiles, esP, esTail]
>   tsFinished           ← getCurrentTime
>   putStrLn (unwords ["___report categorization results:", show (diffUTCTime tsFinished tsStarted)])
>   traceIO (unwords ["wrote", reportCategorizationName])
>
>   where
>     emitFileListC      = concatMap (uncurry doF) (zip ([0..]::[Word]) (toList runt.zFiles))
>     doF nth sffile     = [emitShowL nth 5, emitShowL (zFilename sffile) 56, EndOfLine]
>     dumpInstrument, dumpPercussion
>                        :: SFRuntime → PerGMKey → [Emission]
>     dumpInstrument _ pergm
>                        = [Unblocked (show pergm), EndOfLine] 
>     dumpPercussion _ pergm
>                        = [Unblocked (show pergm), EndOfLine] 
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

The End
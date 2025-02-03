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
> preSampleTaskIf, partneringTaskIf, preInstTaskIf, surveyTaskIf, captureTaskIf
>                , groomTaskIf, vetTaskIf, harvestTaskIf, catTaskIf {-, zoneTaskIf -}
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
>      , ("groom",      groomTaskIf        sffile rost)
>      , ("vet",        vetTaskIf          sffile rost)
>      , ("harvest",    harvestTaskIf      sffile rost)
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
>       (pergmsI, pergmsP, ss)             ← sortByCategory bootAll.zJobs
>       putStrLn $ unwords ["length pergmsI, pergmsP", show $ length pergmsI, show $ length pergmsP, show ss]
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
> sortByCategory         :: Map PerGMKey InstCat → IO ([PerGMKey], [PerGMKey], [[Scan]])
> sortByCategory jobs                      = CM.foldM catFolder ([], [], []) (Map.assocs jobs)
>   where
>     catFolder            :: ([PerGMKey], [PerGMKey], [[Scan]])
>                             → (PerGMKey, InstCat)
>                             → IO ([PerGMKey], [PerGMKey], [[Scan]])
>     catFolder (pergmsI, pergmsP, ss) (pergmI_, icat)
>                                          =
>       let
>         pergmI                           = pergmI_{pgkwBag = Nothing}
>       in
>         return $
>         case icat of
>           InstCatPerc icd                → (pergmsI, pergmsP ++ instrumentPercList pergmI icd.inPercBixen, ss)
>           InstCatInst _                  → (pergmI : pergmsI, pergmsP, ss)
>           InstCatDisq scans              → (pergmsI, pergmsP, ss ++ [scans])
>
>     instrumentPercList :: PerGMKey → [Word] → [PerGMKey]
>     instrumentPercList pergmI            = map (\w → pergmI {pgkwBag = Just w})

task interfaces =======================================================================================================

> formComprehension      :: ∀ r a . SFResource r ⇒ SFFile → (FileArrays → Array Word a) → [r]
> formComprehension sffile blobfun
>   | traceNow trace_FC False              = undefined
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
>
> preSampleTaskIf sffile _ fwIn
>                                          = foldl' formFolder fwIn (formComprehension sffile ssShdrs)
>   where
>     fName_                               = "preSampleTaskIf"
>
>     formFolder         :: FileWork → PreSampleKey → FileWork
>     formFolder fwForm@FileWork{ .. } presk
>       | traceNot trace_FF False          = undefined
>       | otherwise                        = 
>       if cancels [Accepted] scanned
>         then fwForm{  fwDispositions = rd'}
>         else fwForm{  fwBoot = fwBoot{zPreSampleCache
>                                         = Map.insert presk (computePreSample shdr) fwBoot.zPreSampleCache}
>                     , fwDispositions = rd'}
>       where
>         fName                            = unwords [fName_, "formFolder"]
>         trace_FF                         = unwords [fName, show fwForm]
>
>         shdr@F.Shdr{ .. }                = sffile.zFileArrays.ssShdrs ! presk.pskwSampleIndex
>         sa                               = ScanAlts presk fName []
>         (scanned, rd')                   = scanAlts sa alts fwForm.fwDispositions
>           where
>             alts                         =
>               [  curate sampleName      goodName                          (violated sa CorruptName)
>                , curate sampleRate      (\x → x == clip (64::Word, (2::Word) ^ (20::Word)) x)
>                                                                           (violated sa BadSampleRate)
>                , curate sampleType      (isJust . toMaybeSampleType)      (violated sa BadSampleType)
>                , curate (start, end)    sampleSizeOk                      (violated sa BadSampleLimits)]
>
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
>     partneringFolder (target, sPartnerMap, rdPartnering) (k, v)
>       | cancels [Accepted, NoChange] scanned
>                                          = (target,                sPartnerMap,                  rdPartnering')
>       | cancels [Accepted] scanned       = (Map.insert k v target, sPartnerMap,                  rdPartnering') 
>       | otherwise                        = (Map.insert k v target, makePartner (Just otherKey),  rdPartnering')
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
>         sa                               = ScanAlts k fName []
>         (scanned, rdPartnering')         = scanAlts sa alts rdPartnering
>           where
>             alts                         =
>               [  curate stereo           id                  (noChange sa Ok)
>                , curate' other           isJust              (violated sa MissingStereoPartner)
>                , curate backLink         isJust              (violated sa BadStereoPartner)]
>
> {-
> markGlobalZone         :: Map PerGMKey PreInstrument → InstZoneRecord → Map PerGMKey PreInstrument
> markGlobalZone preic zrec
>   | traceNot trace_MGZ False             = undefined
>   | otherwise                            =
>   if isNothing zrec.zswGBix || isNothing moldpreI
>     then preic
>     else Map.insert pergm oldpreI{iGlobalKey = Just $ PreZoneKey zrec.zswFile (fromJust zrec.zswGBix)} preic
>   where
>     pergm                                = instKey zrec
>     moldpreI                             = Map.lookup pergm preic
>     oldpreI                              = deJust (unwords["mold", show pergm]) moldpreI
>     trace_MGZ                            = unwords ["markGlobalZone", show zrec.zswGBix, show pergm]
> -}
>
> preInstTaskIf sffile _ fwIn              =
>   fwIn{  fwBoot = fwIn.fwBoot{zPreInstCache = preInstCache}
>        , fwDispositions = rd'}
>   where
>     fName                                = "preInstTaskIf"
>
>     pergms                               = formComprehension sffile ssInsts
>     (preInstCache, rd')                  = foldl' preIFolder (Map.empty, fwIn.fwDispositions) pergms
>
>     preIFolder         :: (Map PerGMKey PreInstrument, ResultDispositions)
>                           → PerGMKey
>                           → (Map PerGMKey PreInstrument, ResultDispositions)
>     preIFolder (m, rdIn) pergm@PerGMKey{ .. }
>                                          =
>       if iinst.instBagNdx < jinst.instBagNdx
>         then (m', rd'')
>         else error $ unwords [fName, "corrupt instBagNdx"]
>       where
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pgkwInst + 1}
>
>         nm                               = iinst.instName
>
>         m'                               =
>           if cancels [Accepted] ss
>              then m 
>              else Map.insert pergm (PreInstrument iinst nm (computeFFMatches nm) Nothing) m
>
>         sa                               = ScanAlts pergm fName []
>         (ss, rd'')                       = scanAlts sa alts rdIn
>           where
>             alts                         =
>               [curate iinst.instName goodName (violated sa CorruptName)]
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm                       = boota.ssInsts ! pergm.pgkwInst
>       where
>         boota                            = sffile.zFileArrays

PreZone administration ================================================================================================

> goodZRecs              :: [InstZoneRecord] → ResultDispositions → [InstZoneRecord]
> goodZRecs zrecs rdNow                    = filter (not . fatalrd [Accepted, NoChange] rdNow . instKey) zrecs
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
> makeZRec pergm                           = InstZoneRecord pergm.pgkwFile pergm.pgkwInst Nothing []
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             = PerGMKey zrec.zswFile zrec.zswInst Nothing       

survey task ===========================================================================================================
          instantiate zrecs

> surveyTaskIf _ _ fwIn@FileWork{ .. }     =
>   fwIn{fwZRecs = map makeZRec (Map.keys fwBoot.zPreInstCache)}

iterating on InstZoneRecord list ======================================================================================

> zrecTask               :: (InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions))
>                           → SFFile → FileWork → FileWork
> zrecTask zrecFun _ fwIn                  = fwIn{  fwZRecs = zrecs
>                                                 , fwDispositions = rd' }
>   where
>     rdIn                                 = (tracer "zrecTask" fwIn).fwDispositions
>     (zrecs, rd')                         = foldl' taskRunner ([], rdIn) fwIn.fwZRecs
>
>     taskRunner (zrecsFold, rdFold) zrec        = 
>       let
>         (zrec', rd'')                     = zrecFun zrec rdFold
>       in
>         if fatalrd [Accepted, NoChange] rdFold (instKey zrec)
>           then (zrec : zrecsFold, rdFold)
>           else (zrec': zrecsFold, rd'')

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
>         sa                               = ScanAlts pergm fName_ []
>         (_, rd')                         = scanAlts sa alts rdIn
>
>         alts                             =
>           [curate pzsRemaining (not . null) (violated sa NoZones)]
>
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

groom task ============================================================================================================

> makeBack               :: FileWork → [InstZoneRecord] → Map PreSampleKey [PreZoneKey]
> makeBack FileWork{ .. } zrecs            = foldl' Map.union Map.empty (map zrec2back zrecs)
>   where
>     zrec2back          :: InstZoneRecord → Map PreSampleKey [PreZoneKey]
>     zrec2back zrec                       =
>       foldl' backFolder Map.empty (filter (isStereoZone fwBoot.zPreSampleCache) zrec.zsPreZones)
>     backFolder         :: Map PreSampleKey [PreZoneKey] → PreZone → Map PreSampleKey [PreZoneKey]
>     backFolder target pz                 =
>       Map.insertWith (++) (PreSampleKey pz.pzWordF pz.pzWordS) [extractZoneKey pz] target
>
> groomTaskIf sffile _ fwIn@FileWork{ .. } = zrecTask groomer sffile fwIn 
>   where
>     fName_                               = "groomTaskIf"
>
>     back                                 = makeBack fwIn (goodZRecs fwZRecs fwDispositions)
>
>     groomer            :: InstZoneRecord
>                           → ResultDispositions
>                           → (InstZoneRecord, ResultDispositions)
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
>         sa                               = ScanAlts pergm fName []
>         (_, rd')                         = scanAlts sa alts rdGroom_
>           where           
>             alts                         = [curate newPzs (not . null) (violated sa NoZones)]
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
>         mapStereo                        = formPreZoneMap filePzs
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
>         sa                               = ScanAlts pergm fName []
>         (_, rd')                         = scanAlts sa alts rdVet_
>           where           
>             alts                         = [curate newPzs (not . null) (violated sa NoZones)]
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

harvest task ==========================================================================================================
          reap owners from zrecs

> harvestTaskIf _ _ fwIn                   = fwIn{fwBoot = fwIn.fwBoot{zOwners = foldl' harvestFolder Map.empty zrecs}}
>   where
>     zrecs                                = fwIn.fwZRecs
>     rdIn                                 = fwIn.fwDispositions
>
>     harvestFolder      :: Map PerGMKey [PreZone] → InstZoneRecord → Map PerGMKey [PreZone]
>     harvestFolder m zrec                 =
>       let
>         pergm                            = instKey zrec
>         pzs                              = zrec.zsPreZones
>       in
>         if fatalrd [Accepted, NoChange] rdIn pergm
>           then m
>           else Map.insert pergm pzs m
>
> shorten        :: (Char → Bool) → [Char] → [Char] 
> shorten qual chars                       = reverse (dropWhile qual (reverse chars))

categorization task ==========================================================================================================
          reap owners from zrecs

> catTaskIf _ rost fwIn                    =
>   let
>     boot                                 = fwIn.fwBoot
>   in
>     fwIn{fwBoot = boot{zJobs = categorize boot.zPreSampleCache boot.zPreInstCache boot.zOwners rost}}
>
> categorize             :: Map PreSampleKey PreSample
>                           → Map PerGMKey PreInstrument
>                           → Map PerGMKey [PreZone] 
>                           → ([InstrumentName], [PercussionSound])
>                           → Map PerGMKey InstCat
> categorize preSampleCache preInstCache owners rost
>                                          = Map.mapWithKey categorizeInst preInstCache
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
>           deJust (unwords [fName__, "PreInstrument"]) (Map.lookup pergm preInstCache)
>         mpzs                             = Map.lookup pergm owners
>         pzs                              = deJust (unwords[fName__, "owners"]) mpzs
>
>         -- Put the Instrument, of either category, through a gauntlet of checks.
>         -- This diverges, and then we have qualInstZone and qualPercZone 
>
>         pzsLocal                         = filter isLocal pzs
>         isLocal pz                       = not (isStereoZone preSampleCache pz) && not (hasCross pz)
>                                            
>         -- Determine which category will belong to the Instrument, based on its performance for
>         -- 1. all kinds
>         -- 2. "rost" subset, could be same as 1.
>
>         icatAllKinds, icatRost, icatNarrow, icat
>                        :: Maybe InstCat
>         icatAllKinds                     = foldl' CM.mplus Nothing (provideAlts Nothing allKinds)
>         icatRost                         = foldl' CM.mplus Nothing (provideAlts icatAllKinds rost)
>         icatNarrow                       = Just (InstCatDisq (dropped sa Narrow noClue))
>         sa                               = ScanAlts pergm fName__ []
>         icat                             =
>           case (icatAllKinds, icatRost) of
>             (Just (InstCatInst _), Just (InstCatInst _))
>                                          → icatRost
>             (Just (InstCatPerc _), Just (InstCatPerc _))
>                                          → icatRost
>             (Just (InstCatDisq _), _)    → icatAllKinds
>             (_, Just (InstCatDisq _))    → icatRost
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
>                                else Just $ InstCatDisq (violated sa CorruptGMRange (show mrng))
>
>         hasRom pz                        = F.sampleType (effShdr preSampleCache pz) >= 0x8000
>                                          
>         checkLinkage   :: Maybe InstCat
>         checkLinkage                     =
>           if isSafe sList || requiredZoneLinkage < 1 then Nothing else Just $ InstCatDisq (violated sa BadLinkage noClue)
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
>         extractLink pz                   = fromIntegral $ F.sampleLink (effShdr preSampleCache pz)
>
>         rejectCrosses  :: Maybe InstCat
>         rejectCrosses                 =
>           if any hasCross pzs then Just $ InstCatDisq (violated sa IllegalCrossover noClue) else Nothing
>
>         hasCross       :: PreZone → Bool
>         hasCross pz                      =
>           isStereoZone preSampleCache pz && notElem (extractLink pz) (map extractLink pzs)
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
>                  then Just $ InstCatDisq (violated sa NoZones noClue)
>                  else Nothing
>               , corrupt
>               , if any hasRom pzs then Just $ InstCatDisq (violated sa RomBased noClue) else Nothing
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
>                   , maybeSettle stands      (catDisq (dropped sa Narrow noClue))     preI.iMatches.ffInst
>
>                   , maybeNailAsPerc 0.3
>                   , if genericScore > 0 then Just catInst            else Nothing
>                   , if genericScore < 0 then Just (catPerc wZones)   else Nothing
>                   , Just $ catDisq (dropped sa Unrecognized noClue)
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
>                          else Just (catDisq (violated sa NoPercZones noClue)))
>                     else Nothing
>
>                 trace_FA = unwords [fName_, preI.iName, show frost, show (length uZones, length wZones)]
>
>             catInst      :: InstCat      =
>               if null pzs
>                 then InstCatDisq (violated sa NoZones noClue)
>                 else (case checkSmashing pergm smashup of
>                        Nothing           → InstCatInst icd
>                        Just reason       → InstCatDisq reason)
>               where
>                 smashup                  = computeInstSmashup pzs
>                 icd                      = InstCatData pzs smashup []
>
>             catPerc      :: [Word] → InstCat
>             catPerc ws
>               | traceNot trace_CP False  = undefined
>               | otherwise                =
>               if null pzs || null ws || (null . init) ws
>                 then InstCatDisq (dropped sa Narrow (show ws))
>                 else (case check of
>                        Nothing           → InstCatPerc icd
>                        Just reason       → InstCatDisq reason)
>               where
>                 fName                    = unwords[fName_, "catPerc"]
>                 trace_CP                 = unwords [fName, show (length ws, length pzs)]
>
>                 pzs'                     = filter (\x → x.pzWordB `elem` ws) pzs
>                 smashup                  = computeInstSmashup pzs'
>                 icd                      = InstCatData pzs' smashup ws
>                 check                    = checkSmashing pergm smashup
>
>             catDisq    :: [Scan] → InstCat
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
> canDevolveToMono       :: Bool
> canDevolveToMono                         = True
> requiredZoneLinkage    :: Double
> requiredZoneLinkage                      = 0

The End
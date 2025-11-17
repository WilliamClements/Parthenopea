> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Boot
William Clements
January 21, 2025

> module Parthenopea.SoundFont.Boot ( surveyInstruments ) where
>
> import qualified Codec.SoundFont         as F
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Foldable
> import Data.IntMap.Strict (IntMap)
> import qualified Data.IntMap.Strict as IntMap
> import Data.IntSet (IntSet)
> import qualified Data.IntSet as IntSet
> import Data.List 
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Euterpea.Music ( InstrumentName, PercussionSound )
> import Numeric ( showHex )
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
  
a Boot problematic ====================================================================================================

Each stage interface function takes a FileWork and returns modified FileWork. Often, stage computes temporary
structure(s) to drive later stages. Dependency graph for this process may have cycles which we need to patch around.

(1) reorg stage depends on previously computed smashups, but the absorption leader's smashup must be updated.

(2) In pipeline sequence, it would be plenty fast to modify smashups when incrementally adding zones. But to REMOVE a
zone means expensively recomputing smashup for all remaining zones.

(3) How does _crossInstrumentPairing_ interact with _doAbsorption_?
a...If both True, a valid cross instrument pairing can still work even if absorption didn't fix it
b...If both False, easy, just don't do either
c...If cross True, absorption False, cross pairing only accomplished explicitly (honoring well-formed crossings)
d...If cross False, absorption True, cross pairing only accomplished implicitly (no longer crossing after absorption)

> data FileWork                            =
>   FileWork {
>     fwDirectives       :: Directives
>   , fwZRecs            :: IntMap InstZoneRecord
>   , fwPreZones         :: IntMap PreZone
>   , fwPreSamples       :: Map PreSampleKey PreSample
>   , fwPerInstruments   :: Map PerGMKey PerInstrument
>   , fwMatches          :: Matches
>   , fwPairing          :: Pairing
>   , fwDispositions     :: ResultDispositions}
> instance Show FileWork where
>   show fw                                =
>     unwords [  "FileWork"
>              , show (length fw.fwZRecs), "=#zrecs"
>              , show (length fw.fwPerInstruments), "=#cached"
>              , show fw.fwDispositions]
> defFileWork            :: Directives → FileWork
> defFileWork dives                        =
>   FileWork
>    dives 
>    IntMap.empty
>    IntMap.empty
>    Map.empty
>    Map.empty
>    defMatches
>    defPairing
>    virginrd
>
> makeSummary            :: IntMap PreZone → IntMap IntSet
> makeSummary                              = IntMap.foldl' summ IntMap.empty
>   where
>     summ               :: IntMap IntSet → PreZone → IntMap IntSet
>     summ m pz                            =
>       IntMap.insertWith IntSet.union (wordI pz) (IntSet.singleton (wordB pz)) m
>
> data Pairing                             =
>   Pairing {
>     fwPartners         :: IntMap Int                   {- [SampleIndex → SampleIndex] -}
>   , fwPairings         :: IntMap Int                   {- [BagIndex → BagIndex]       -}
>   , fwActions          :: IntMap IntSet                {- [InstIndex → [BagIndex]]    -}}
> defPairing             :: Pairing
> defPairing                               =
>   Pairing
>     IntMap.empty 
>     IntMap.empty
>     IntMap.empty
>
> type PairingSlot                         = (Maybe Word, (Word, Word), (Word, Word))
>
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
>
> instance Show FileIterate where
>   show fi                                =
>     unwords ["FileIterate", show fi.fiFw]
> reduceFileIterate      :: FileIterate → Survey
> reduceFileIterate FileIterate{ .. }      
>                                          =
>   Survey
>     fwPreZones
>     fwPerInstruments
>     fwMatches
>     fwDispositions
>   where
>     FileWork{ .. }
>                                          = fiFw
>
> preSampleTaskIf, smellTaskIf, surveyTaskIf, captureTaskIf, pairTaskIf, vetTaskIf
>                , adoptTaskIf, smashTaskIf, reorgTaskIf, matchTaskIf, cleanTaskIf, perITaskIf
>                        :: SFFileBoot → ([InstrumentName], [PercussionSound]) → FileWork → FileWork
>
> makeFileIterate        :: Directives → SFFileBoot → ([InstrumentName], [PercussionSound]) → FileIterate
> makeFileIterate dives sffile rost        =
>   FileIterate
>     (defFileWork dives)
>     [
>        ("preSample",  preSample)
>      , ("smell",      smell)
>      , ("capture",    clean . capture . survey)
>      , ("adopt",      clean . adopt)
>      , ("smash 1",    clean . smash)
>      , ("reorg",      clean . reorg) 
>      , ("match",      match)
>      , ("pair",       clean . pair)
>      , ("vet",        clean . vet)
>      , ("smash 2",    smash)
>      , ("perI",       perI)]
>   where
>     preSample                            = preSampleTaskIf    sffile rost
>     smell                                = smellTaskIf        sffile rost
>     survey                               = surveyTaskIf       sffile rost
>     capture                              = captureTaskIf      sffile rost
>     pair                                 = pairTaskIf         sffile rost
>     vet                                  = vetTaskIf          sffile rost
>     adopt                                = adoptTaskIf        sffile rost
>     smash                                = smashTaskIf        sffile rost
>     reorg                                = reorgTaskIf        sffile rost
>     match                                = matchTaskIf        sffile rost
>     clean                                = cleanTaskIf        sffile rost
>     perI                                 = perITaskIf         sffile rost

Boot executive function ===============================================================================================
          returns overall PerInstrument map

> surveyInstruments      :: Directives
>                           → ([InstrumentName], [PercussionSound])
>                           → VB.Vector SFFileBoot
>                           → IO (VB.Vector SFFileBoot, Survey)
> surveyInstruments dives rost sfs         = do
>   putStr $ reapEmissions
>     [   EndOfLine
>       , Unblocked fName_, EndOfLine
>       , Blanks 2, Unblocked $ show $ fst rost, EndOfLine
>       , Blanks 2, Unblocked $ show $ snd rost, EndOfLine, EndOfLine]
>   return $ VB.foldl' bootFolder (VB.empty, defSurvey) sfs
>   where
>     fName_                               = "surveyInstruments"
>
>     bootFolder         :: (VB.Vector SFFileBoot, Survey) → SFFileBoot → (VB.Vector SFFileBoot, Survey)
>     bootFolder (vFiles, Survey _ cacheIn matchesIn rdIn) sffile
>                                          =
>       let
>         Survey{ .. }                     
>                                          = reduceFileIterate ingestFile
>
>         ingestFile                       = head
>                                            $ dropWhile unfinished
>                                            $ iterate nextGen
>                                            $ makeFileIterate dives sffile rost
>           where
>             unfinished fiIn              = not (null fiIn.fiTaskIfs)
>             nextGen fiIn@FileIterate{ .. }
>                                          = fiIn{ fiFw = (snd . head) fiTaskIfs fiFw
>                                                , fiTaskIfs = tail fiTaskIfs}
>
>         vFiles'                          = vFiles `VB.snoc` sffile{zPreZones = sPreZones}
>         survey'                          =
>           Survey
>             sPreZones
>             (Map.union cacheIn sPerInstruments)
>             (combineMatches matchesIn sMatches)
>             (combinerd rdIn sDispositions)
>       in
>         (vFiles', survey')

support sample and instance ===========================================================================================

> formComprehension      :: ∀ r a . SFResource r ⇒ SFFileBoot → (FileArrays → Array Word a) → [r]
> formComprehension sffile blobfun         = map (sfkey sffile.zWordFBoot) bRange
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

> preSampleTaskIf sffile _ fWork           =
>   foldl' sampleFolder fWork (formComprehension sffile ssShdrs)
>   where
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     sampleFolder fwForm presk            =
>       let
>         fName                            = "sampleFolder"
>
>         preSampleCache                   =
>           if dead ssSample
>             then fwForm.fwPreSamples
>             else Map.insert presk ps fwForm.fwPreSamples
>         ps                               = ChangeName shdr changes name
>         shdr                             = sffile.zFileArrays.ssShdrs ! presk.pskwSampleIndex
>
>         raw                              = shdr.sampleName
>         good                             = fixName raw
>
>         mtype                            = toMaybeSampleType shdr.sampleType
>         stereo                           =
>           case mtype of
>             Just SampleTypeLeft          → "stereo"
>             Just SampleTypeRight         → "stereo"
>             _                            → "mono"
>
>         violated, accepted
>                        :: Impact → String → [Scan]
>         violated impact clue             =
>           [Scan Violated impact fName clue]
>         accepted impact clue             =
>           [Scan Accepted impact fName clue]
>
>         sampleSizeOk (stS, enS)          = stS >= 0 && enS - stS >= 0 && enS - stS < 2 ^ (22::Word)
>
>         goodSampleRate x                 = x == clip (n64, n2 ^ n20) x
>           where
>             n64, n2, n20       :: Word
>             n64                                  = 64
>             n2                                   = 2
>             n20                                  = 20
>
>         ssSample_
>           | not (goodSampleRate shdr.sampleRate)
>                                          = violated BadSampleRate (show shdr.sampleRate)
>           | isNothing mtype              = violated BadSampleType (show shdr.sampleType)
>           | not (sampleSizeOk (shdr.start, shdr.end))
>                                          = violated BadSampleLimits (show (shdr.start, shdr.end))
>           | not (goodName raw)           = badButMaybeFix fixBadNames BadName fName raw good
>           | otherwise                    = accepted Ok stereo
>
>         (ssSample, changes, name)        = if wasRescued BadName ssSample_
>                                              then (ssSample_, singleton FixBadName, good)
>                                              else (ssSample_, [],                       raw)
>       in
>         fwForm{ fwPreSamples = preSampleCache, fwDispositions = dispose presk ssSample fwForm.fwDispositions}

smell task ============================================================================================================
          partner map at sample header level = forms basis for all stereo pairings

> smellTaskIf _ _ fWork                    = fWork{fwPairing = fWork.fwPairing{fwPartners = partnerMap}}
>   where
>     partnerMap                           = Map.foldlWithKey smellFolder IntMap.empty allL
>                                              where allL = Map.filter isLeftPS fWork.fwPreSamples
>     smellFolder m presk pres             =
>       let
>         siIn, siOut    :: Int
>         siIn                             = fromIntegral presk.pskwSampleIndex
>         siOut                            = fromIntegral (effPSShdr pres).sampleLink
>
>         mback_                           =
>           presk{pskwSampleIndex = fromIntegral siOut} `Map.lookup` fWork.fwPreSamples
>         mback                            = mback_ >>= backOk
>           where
>             backOk opres                 =
>               if isRightPS opres && fromIntegral (effPSShdr opres).sampleLink == siIn
>                 then Just siOut
>                 else Nothing
>       in
>         case mback of
>           Nothing                        → m
>           Just siOther                   → IntMap.insert siIn siOther m

InstZoneRecord and PreZone administration =============================================================================

> data InstZoneRecord                      =
>   InstZoneRecord {
>     zswFile            :: Int
>   , zswInst            :: Word
>   , zswChanges         :: ChangeName F.Inst
>   , zsSmashup          :: Maybe (Smashing Word)}
> instance Show InstZoneRecord where
>   show zrec                              =
>     unwords ["InstZoneRecord", show (zrec.zswFile, zrec.zswInst)]
> makeZRec               :: PerGMKey → ChangeName F.Inst → InstZoneRecord
> makeZRec pergm changes                          =
>   InstZoneRecord 
>     pergm.pgkwFile 
>     pergm.pgkwInst
>     changes Nothing
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             =
>   PerGMKey 
>     zrec.zswFile 
>     zrec.zswInst
>     Nothing       

iterating InstZoneRecord list =========================================================================================

> zrecTask               :: (InstZoneRecord → ResultDispositions → (Maybe InstZoneRecord, ResultDispositions))
>                           → FileWork → FileWork
> zrecTask userFun fw                      = fw{fwZRecs = zrecs', fwDispositions = rd'}
>   where
>     zrecs'             :: IntMap InstZoneRecord
>     (zrecs', rd')                        =
>       foldl' taskRunner (fw.fwZRecs, fw.fwDispositions) fw.fwZRecs
>
>     taskRunner (zrecs, rdFold) zrec      =
>       let
>         mzrec          :: Maybe InstZoneRecord
>         (mzrec, rdFold')                 = userFun zrec rdFold
>
>         kfun _                           = mzrec
>       in
>         (IntMap.update kfun (fromIntegral zrec.zswInst) zrecs, rdFold')
>
> zrecCompute            :: ∀ a . FileWork → (a → InstZoneRecord → a) → a → a
> zrecCompute fw userFun seed              = IntMap.foldl' userFun seed fw.fwZRecs

survey task ===========================================================================================================
          instantiate InstZoneRecord per Instrument

> surveyTaskIf sffile _ fWork              = fWork{fwZRecs = zrecs, fwDispositions = rd'}
>   where
>     fName                                = "surveyTaskIf"
>
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>
>     (zrecs, rd')                         =
>       foldl' surveyFolder (IntMap.empty, fWork.fwDispositions) (formComprehension sffile ssInsts)
>     
>     violated impact clue             =
>       [Scan Violated impact fName clue]
>     accepted impact clue             =
>       [Scan Accepted impact fName clue]
>
>     surveyFolder       :: (IntMap InstZoneRecord, ResultDispositions) → PerGMKey → (IntMap InstZoneRecord, ResultDispositions)
>     surveyFolder (zrecsFold, rdFold) pergm
>       | iinst.instBagNdx <= jinst.instBagNdx
>                                          = (zrecs', rd'')
>       | otherwise                        = error $ unwords [fName, "corrupt instBagNdx"]
>       where
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pergm.pgkwInst + 1}
>
>         raw                              = iinst.instName
>         good                             = fixName raw
>
>         zrecs'
>           | dead ssSurvey                = zrecsFold 
>           | otherwise                    =
>             IntMap.insert (fromIntegral newZRec.zswInst) newZRec zrecsFold
>           where
>             newZRec                      = makeZRec pergm (ChangeName iinst changes finalName)
>
>         ssSurvey
>           | iinst.instBagNdx == jinst.instBagNdx
>                                          = violated NoZones (show iinst.instName)
>           | not (goodName raw)           = badButMaybeFix fixBadNames BadName fName raw good
>           | otherwise                    = accepted Ok (show pergm.pgkwInst)
>
>         changes                          = if wasRescued BadName ssSurvey then singleton FixBadName else []
>         finalName                        = if wasRescued BadName ssSurvey then good else raw
>
>         rd''                             = dispose pergm ssSurvey rdFold   
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm                       = sffile.zFileArrays.ssInsts ! pergm.pgkwInst

capture task ==========================================================================================================
          populate zrecs with PreZones

> data Capture                             =
>   Capture {
>     uPzs               :: IntMap PreZone
>  ,  uSFZone            :: SFZone
>  ,  uDispo             :: ResultDispositions}
> instance Show Capture where
>   show capt                            =
>     unwords [  "Capture"
>              , show (IntMap.map pzWordB capt.uPzs)]
> defCapture             :: Capture
> defCapture                               = Capture IntMap.empty defZone virginrd
>
> captureTaskIf sffile _ fWork             = fWork{ fwPreZones = captx.uPzs, fwDispositions = captx.uDispo}
>   where
>     fName_                               = "captureTaskIf"
>
>     accepted impact clue                 =
>       [Scan Accepted impact fName_ clue]
>     dropped impact clue                  =
>       [Scan Dropped impact fName_ clue]
>     modified impact clue                 =
>       [Scan Modified impact fName_ clue]
>     violated impact clue                 =
>       [Scan Violated impact fName_ clue]
>
>     captx                                =
>       zrecCompute
>         fWork
>         captCompute
>         defCapture{uDispo = fWork.fwDispositions}
>       where
>         captCompute    :: Capture → InstZoneRecord → Capture
>         captCompute captFold zrecIn      =
>           let
>             (newPzs, dispo')             = captureZones zrecIn captFold.uDispo
>           in
>             captFold{uPzs = captFold.uPzs `IntMap.union` newPzs, uDispo = dispo'}
>
>     captureZones       :: InstZoneRecord → ResultDispositions → (IntMap PreZone, ResultDispositions)
>     captureZones zrec rdCap              = (capty.uPzs, dispose pergm ssCap capty.uDispo)
>       where
>         pergm                            = instKey zrec
>         iName                            = zrec.zswChanges.cnName
>
>         ssCap                            = if null capty.uPzs
>                                              then violated NoZones noClue
>                                              else modified Captured iName
>
>         captureZone    :: Word → (Word, Either PreZone (PreZoneKey, [Scan]))
>         captureZone bix
>           | traceNot trace_CZ False      = undefined
>           | isNothing pz.pzDigest.zdSampleIndex
>                                          = (bix, Right (pzk, accepted GlobalZone       noClue))
>           | isNothing mpres              = (bix, Right (pzk, dropped  Orphaned         noClue))
>           | not (okGMRanges pz.pzDigest) = (bix, Right (pzk, violated BadGMRange       (rangeClue pz)))
>           | hasRom pz                    = (bix, Right (pzk, violated RomBased         (romClue pz)))
>           | isJust probeLimits           = (bix, Right (pzk, violated BadAppliedLimits (fromJust probeLimits)))
>           | otherwise                    = (bix, Left pz{pzChanges = ChangeEar (effPSShdr pres) []})
>           where
>             fName                        = unwords [fName_, "captureZone"]
>             trace_CZ                     = unwords [fName, iName, show (xgeni, ygeni)]
>
>             ibags                        = sffile.zFileArrays.ssIBags
>             xgeni                        = F.genNdx $ ibags ! bix
>             ygeni                        = F.genNdx $ ibags ! (bix + 1)
>             gens       :: [F.Generator]
>             gens                         = profess
>                                              (xgeni <= ygeni)
>                                              (unwords [fName, "SoundFont file corrupt (gens)"])
>                                              (map (sffile.zFileArrays.ssIGens !) (deriveRange xgeni ygeni))
>             pzk                          = PreZoneKey 
>                                             sffile.zWordFBoot 
>                                             (pgkwInst pergm)
>                                             bix
>                                             si
>             pz                           = makePreZone sffile.zWordFBoot si pergm.pgkwInst bix gens pres.cnSource
>             si                           = deJust (unwords [fName, "si"]) pz.pzDigest.zdSampleIndex
>             presk                        = PreSampleKey sffile.zWordFBoot si
>             mpres                        = presk `Map.lookup` fWork.fwPreSamples
>             pres                         = deJust (unwords [fName, "pres"]) mpres
>
>             probeLimits                  =
>               if ok
>                 then Nothing
>                 else Just $ unwords [showHex stA [], showHex enA [], showHex stL [], showHex enL [], show zd.zdSampleMode]
>               where
>                 shdr                     = effPZShdr pz
>                 zd                       = pz.pzDigest
>
>                 stA                      = shdr.start     + fromIntegral zd.zdStart
>                 enA                      = shdr.end       + fromIntegral zd.zdEnd
>                 stL                      = shdr.startLoop + fromIntegral zd.zdStartLoop
>                 enL                      = shdr.endLoop   + fromIntegral zd.zdEndLoop
>
>                 ok                       =
>                   0 <= stA && stA <= enA && 0 <= stL && stL <= enL
>                   && enA - stA < 2 ^ (22::Word)
>                   && (zd.zdSampleMode == Just A.NoLoop || enL - stL < 2 ^ (22::Word))

process initial capture results =======================================================================================

>         results                          =
>           let
>             iinsts                       = sffile.zFileArrays.ssInsts
>             ibagi                        = F.instBagNdx (iinsts ! pgkwInst pergm)
>             jbagi                        = F.instBagNdx (iinsts ! (pgkwInst pergm + 1))
>           in
>             map captureZone (deriveRange ibagi jbagi)
>
>         capty                           = foldl' rFolder defCapture{uDispo = rdCap} results
>           where
>             rFolder        :: Capture → (Word, Either PreZone (PreZoneKey, [Scan])) → Capture
>             rFolder captFold (bagIndex, eor)
>                                          =
>               let
>                 doNormal pz              =
>                   captFold{uPzs = IntMap.insert
>                                     (wordB pz)
>                                     pz{pzSFZone = buildZone sffile captFold.uSFZone (Just pz) bagIndex}
>                                     captFold.uPzs}
>                 doError (k, ssZone)      =
>                   if hasImpact GlobalZone ssZone
>                     then captFold{uSFZone = buildZone sffile defZone Nothing bagIndex}
>                     else captFold{uDispo = dispose k ssZone captFold.uDispo}
>               in
>                 either doNormal doError eor
>
>         stype pz                         = F.sampleType (effPZShdr pz)
>         hasRom pz                        = stype pz >= 0x8000
>         romClue pz                       = showHex (stype pz) []
>         rangeClue pz                     = show (pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange)
>
> buildZone              :: SFFileBoot → SFZone → Maybe PreZone → Word → SFZone
> buildZone sffile fromZone mpz bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = foldr addMod (foldl' addGen fromZone gens) mods
>   where
>     fName                                = "buildZone"
>     trace_BZ                             =
>       unwords [fName, show (sffile.zWordFBoot, bagIndex), zName, show (fromZone == defZone)]
>
>     zName                                =
>       case mpz of
>         Nothing                          → "<global>"
>         Just pz                          → show (effPZShdr pz).sampleName
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
>         (unwords[fName, "SoundFont file", show sffile.zWordFBoot, sffile.zFilename, "corrupt gens"])
>         (map (boota.ssIGens !) (deriveRange xgeni ygeni))
>     mods               :: [(Word, F.Mod)]
>     mods                                 =
>       profess
>         (xmodi <= ymodi)
>         (unwords[fName, "SoundFont file", show sffile.zWordFBoot, sffile.zFilename, "corrupt mods"])
>         (zip [10_000..] (map (boota.ssIMods !) (deriveRange xmodi ymodi)))
>
> addGen                 :: SFZone → F.Generator → SFZone
> addGen iz gen =
>   case gen of
>   F.InstIndex w                  → iz {zInstIndex =                Just w}
>   F.Key w                        → iz {zKey =                      tmclip w}
>   F.Vel w                        → iz {zVel =                      tnclip w}
>   F.InitAtten i                  → iz {zInitAtten =                tdclip i}
>   F.CoarseTune i                 → iz {zCoarseTune =               t1clip i}
>   F.FineTune i                   → iz {zFineTune =                 t2clip i}
>   F.SampleIndex w                → iz {zSampleIndex =              Just w}
>   F.SampleMode m                 → iz {zSampleMode =               Just m}
>   F.ScaleTuning i                → iz {zScaleTuning =              t3clip i}
>   F.ExclusiveClass i             → iz {zExclusiveClass =           (tnclip . fromIntegral) i}
>
>   F.DelayVolEnv i                → iz {zDelayVolEnv =              tcclip i}
>   F.AttackVolEnv i               → iz {zAttackVolEnv =             tcclip i}
>   F.HoldVolEnv i                 → iz {zHoldVolEnv =               tcclip i}
>   F.DecayVolEnv i                → iz {zDecayVolEnv =              tcclip i}
>   F.SustainVolEnv i              → iz {zSustainVolEnv =            tdclip i}
>   F.ReleaseVolEnv i              → iz {zReleaseVolEnv =            tbclip i}
>
>   F.Chorus i                     → iz {zChorus =                   ticlip i}
>   F.Reverb i                     → iz {zReverb =                   ticlip i}
>   F.Pan i                        → iz {zPan =                      tpclip i}
>
>   F.RootKey w                    → iz {zRootKey =                  tmclip w}
>
>   F.ModLfoToPitch i              → iz {zModLfoToPitch =            teclip i}
>   F.VibLfoToPitch i              → iz {zVibLfoToPitch =            teclip i}
>   F.ModEnvToPitch i              → iz {zModEnvToPitch =            teclip i}
>   F.InitFc i                     → iz {zInitFc =                   tfclip i}
>   F.InitQ i                      → iz {zInitQ =                    tqclip i}
>   F.ModLfoToFc i                 → iz {zModLfoToFc =               teclip i}
>   F.ModEnvToFc i                 → iz {zModEnvToFc =               teclip i}
>   F.ModLfoToVol i                → iz {zModLfoToVol =              tvclip i}
>   F.DelayModLfo i                → iz {zDelayModLfo =              tcclip i}
>   F.FreqModLfo i                 → iz {zFreqModLfo =               taclip i}
>   F.DelayVibLfo i                → iz {zDelayVibLfo =              tcclip i}
>   F.FreqVibLfo i                 → iz {zFreqVibLfo =               taclip i}
>   F.DelayModEnv i                → iz {zDelayModEnv =              tcclip i}
>   F.AttackModEnv i               → iz {zAttackModEnv =             tbclip i}
>   F.HoldModEnv i                 → iz {zHoldModEnv =               tcclip i}
>   F.DecayModEnv i                → iz {zDecayModEnv =              tbclip i}
>   F.SustainModEnv i              → iz {zSustainModEnv =            ticlip i}
>   F.ReleaseModEnv i              → iz {zReleaseModEnv =            tbclip i}
>   F.KeyToModEnvHold i            → iz {zKeyToModEnvHold =          tkclip i}
>   F.KeyToModEnvDecay i           → iz {zKeyToModEnvDecay =         tkclip i}
>   F.KeyToVolEnvHold i            → iz {zKeyToVolEnvHold =          tkclip i}
>   F.KeyToVolEnvDecay i           → iz {zKeyToVolEnvDecay =         tkclip i}
>   _                              → iz
>
> addMod                 :: (Word, F.Mod) → SFZone → SFZone
> addMod (mId, fmod) iz                    = maybe iz addModulator makeModulator
>   where
>     addModulator       :: Modulator → SFZone
>     addModulator m8r                     = iz{zModulators = m8r : iz.zModulators}
>
>     makeModulator      :: Maybe Modulator
>     makeModulator                        = mm'
>       where
>         mm, mm'        :: Maybe Modulator
>         mm                               = unpackModSrc fmod.srcOper
>                                            >>= flip addSrc defModulator{mrModId = mId}
>                                            >>= addDest fmod.destOper
>                                            >>= addAmount (fromIntegral fmod.amount)
>         mm'                              = unpackModSrc fmod.amtSrcOper
>                                            >>= addAmtSrc mm

pair task =============================================================================================================
          enforce discipline for pairs

> pairTaskIf _ _ fWork                     =
>   fWork{fwPairing = fWork.fwPairing{fwPairings = pairings, fwActions = makeActions rejects}}
>   where
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     Pairing{ .. }                        
>                                          = fWork.fwPairing

convenience functions =================================================================================================
          unpair           - cram all Ls and Rs from partner map into single set
          makeActions      - from reject bag indices, form action map (from instrument to affected zones)
          dereference      - grab PreZone from fwPreZones

>     unpair             :: IntMap Int                   {- [BagIndex → BagIndex]         -}
>                           → IntSet                     {- [BagIndex]                    -}
>     unpair                               =
>       let
>         ifolder iset ifrom ito           = (IntSet.insert ito . IntSet.insert ifrom) iset
>       in
>         IntMap.foldlWithKey ifolder IntSet.empty
>
>     makeActions        :: IntSet                       {- [BagIndex]                    -}
>                           → IntMap IntSet              {- [InstIndex → [BagIndex]]      -}
>     makeActions                          = IntSet.foldl' make IntMap.empty
>       where
>         make actions bag                 = IntMap.insertWith IntSet.union (wordI pz) (IntSet.singleton bag) actions
>                                              where pz = dereference bag
>
>     dereference        :: Int → PreZone
>     dereference bag                      = pz
>       where
>         fName                            = "dereference"
>
>         mpz                              = bag `IntMap.lookup` fWork.fwPreZones
>         pz                               =
>           fromMaybe
>             (error $ unwords [fName, "PreZone", show bag, "missing from fwPreZones"])
>             mpz
    
main flow =============================================================================================================
          After somehow generating the pair list for this sffile, reject all other stereo bags - they failed to pair!
          The "somehow" is to make pairs if and only if L and R's excerpted zone data produce identical "pair slots".

>     rejects                              = IntMap.keysSet fWork.fwPreZones `IntSet.difference` unpair pairings
>
>     pairings                             = 
>       let
>         mLeft, mRight  :: IntMap IntSet                {- [SampleIndex → [BagIndex]]    -}
>         (mLeft, mRight)                  =
>           IntMap.foldl' (uncurry fFolder) (IntMap.empty, IntMap.empty) fWork.fwPreZones
>           where
>             fFolder mleft mright pz
>               | isLeftPZ pz              = (putMembers pz mleft, mright)
>               | isRightPZ pz             = (mleft, putMembers pz mright)
>               | otherwise                = (mleft, mright)
>             putMembers pz                =
>               IntMap.insertWith IntSet.union (wordS pz) (IntSet.singleton $ wordB pz)
>         
>         regular                          = IntMap.foldlWithKey (pFolder False) IntMap.empty fwPartners
>         extra                            = IntMap.foldlWithKey (pFolder True)  regular      fwPartners
>
>         pFolder        :: Bool →  IntMap Int → Int → Int → IntMap Int
>         pFolder exo soFar siFrom siTo    = soFar `IntMap.union` qualifyPairs exo fwPairings bsL bsR
>           where
>             bsL, bsR   :: IntSet                       {- [BagIndex]                    -}             
>             bsL                          = fromMaybe IntSet.empty (siFrom `IntMap.lookup` mLeft)
>             bsR                          = fromMaybe IntSet.empty (siTo   `IntMap.lookup` mRight)
>       in
>         regular `IntMap.union` extra
>
>     qualifyPairs       :: Bool                         {- exotic                        -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>                           → IntSet                     {- [BagIndex]                    -}
>                           → IntSet                     {- [BagIndex]                    -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>     qualifyPairs exo done bagsL bagsR    = Map.foldlWithKey (pin $ peg bagsR') IntMap.empty (peg bagsL')
>       where
>         allowCross                       = exo && crossInstrumentPairing
>         allowParallel                    = exo && parallelPairing
> 
>         pairedSoFar                      = unpair done
>         bagsL'                           = bagsL `IntSet.difference` pairedSoFar
>         bagsR'                           = bagsR `IntSet.difference` pairedSoFar
>
>         peg            :: IntSet                       {- [BagIndex]                    -}
>                           → Map PairingSlot IntSet     {- [ps → [BagIndex]]             -}
>         peg bags                         =
>           let
>             pegBag m bag                 = Map.insertWith IntSet.union iSlot (IntSet.singleton bag) m
>               where
>                 iSlot                    = (minst, zdKey, zdVel)
>
>                 pz                       = accessPreZone "peg" fWork.fwPreZones bag
>                 minst                    = if allowCross || allowParallel then Nothing else Just pz.pzWordI
>                 zdKey                    = fromMaybe (0, 127) pz.pzDigest.zdKeyRange
>                 zdVel                    = fromMaybe (0, 127) pz.pzDigest.zdVelRange
>           in
>             IntSet.foldl' pegBag Map.empty bags
>
>         pin            :: Map PairingSlot IntSet       {- [ps → [BagIndex]]             -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>                           → PairingSlot                {- ps                            -}
>                           → IntSet                     {- [BagIndex]                    -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>         pin pegBoard m iSlot bL          =
>           let
>             bR                           = Map.lookup iSlot pegBoard
>             zipped                       = zip (IntSet.toList bL) (IntSet.toList (fromMaybe IntSet.empty bR))
>             newPairs
>               | null zipped              = []
>               | not allowParallel && not allowCross && length zipped == 1
>                                          = zipped
>               | not allowParallel && not allowCross
>                                          = []
>               | otherwise                =
>                 (if allowParallel then zipParallel else []) ++ (if allowCross then zipCross else [])
>             (zipParallel, zipCross)      = partition areParallel zipped
>               where
>                 areParallel (bagL, bagR) =    (accessPreZone "bagL" fWork.fwPreZones bagL).pzWordI
>                                            == (accessPreZone "bagR" fWork.fwPreZones bagR).pzWordI   
>           in
>             m `IntMap.union` IntMap.fromList newPairs

vet task ==============================================================================================================
          switch bad stereo zones to mono, or off altogether

> data Vet                                 =
>   Vet {
>     vPzs               :: IntMap PreZone
>  ,  vDispo             :: ResultDispositions}
> instance Show Vet where
>   show vet                               =
>     unwords [  "Vet"
>              , show (IntMap.map pzWordB vet.vPzs)]
>
> vetTaskIf _ _ fWork                      = fWork{fwPreZones = vet.vPzs, fwDispositions = vet.vDispo}
>   where
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     Pairing{ .. }                      
>                                          = fWork.fwPairing
>     vet                                  =
>       zrecCompute
>         fWork
>         vetCompute
>         (Vet fWork.fwPreZones fWork.fwDispositions)
>       where
>         vetCompute vetFold zrec          =
>           let
>             vactions                     = fromIntegral zrec.zswInst `IntMap.lookup` fwActions
>           in
>             maybe vetFold (vetActions vetFold zrec) vactions
>
>     vetActions         :: Vet → InstZoneRecord → IntSet → Vet
>     vetActions vetIn zrec actions        = vetIn{vPzs = pzsOut, vDispo = rdOut}
>       where
>         fName                            = "vetActions"
>
>         vetResult                        =
>           mapActions $ if switchBadStereoZonesToMono 
>                         then makeThemMono
>                         else killThem        
>         (pzsOut, rdOut)                  = (vetResult.vPzs, vetResult.vDispo)
>
>         makeThemMono, killThem
>                        :: PreZone → ResultDispositions → (IntMap PreZone, ResultDispositions)
>         makeThemMono pz rd               =
>           let
>             pzs'                         = IntMap.update (Just . makeMono) (wordB pz) vetIn.vPzs
>           in
>             (pzs', rd)
>             
>         killThem pz rd                   = 
>           let
>             pzs'                         = IntMap.update (const Nothing) (wordB pz) vetIn.vPzs
>             ssKill                       =
>               [Scan Violated BadStereoPartner fName zrec.zswChanges.cnName]
>             dispo'                       = dispose (extractZoneKey pz) ssKill rd
>           in
>             (pzs', dispo')
>
>         mapActions     :: (PreZone → ResultDispositions → (IntMap PreZone, ResultDispositions)) → Vet
>         mapActions rejectFun             = handleBags
>           where
>             check bag                    = bag `IntSet.member` actions
>             modifyFun                    = rejectFun
>             mapOne     :: (IntMap PreZone, ResultDispositions)
>                           → Int
>                           → (IntMap PreZone, ResultDispositions)
>             mapOne (m, rdFold) bag       =
>               let
>                 pz                       = accessPreZone "mapOne" fWork.fwPreZones bag
>               in
>                 if check bag
>                   then modifyFun pz rdFold
>                   else (m, rdFold)
>
>             handleBags :: Vet
>             handleBags              = vetIn{vPzs = pzs', vDispo = dispo'}
>               where
>                 (pzs', dispo') = IntSet.foldl' mapOne (vetIn.vPzs, vetIn.vDispo) actions

adopt task ============================================================================================================
          mark adoption

> adoptTaskIf _ _ fWork
>   | traceIf trace_ATI False              = undefined
>   | otherwise                            = zrecTask adopter fWork
>   where
>     fName_                               = "adoptTaskIf"
>     trace_ATI                            = unwords [fName_, show $ IntMap.size summary, show $ IntMap.keys summary]
>     summary                              = makeSummary fWork.fwPreZones
>
>     adopter zrec rd                      =
>       let
>         miset                            = fromIntegral zrec.zswInst `IntMap.lookup` summary
>       in
>         case miset of
>           Nothing                        → (Nothing, rd)
>           Just iset                      → (Just zrec, IntSet.foldl' (adopt zrec) rd iset)
>
>     adopt zrec rdFold bag
>       | traceNot trace_A False           = undefined
>       | otherwise                        = dispose (extractSampleKey pz) ssAdopt rdFold
>       where
>         fName                            = "adopt"
>         trace_A                          = unwords[fName, show zrec.zswInst, show bag]
>
>         pz                               = accessPreZone fName fWork.fwPreZones bag
>         impact                           = if wasSwitchedToMono pz then AdoptedAsMono else Adopted
>         ssAdopt                          =
>           [Scan Modified impact fName zrec.zswChanges.cnName]

smash task ============================================================================================================
          compute smashups for each instrument

> smashTaskIf _ _ fWork               = zrecTask smasher fWork
>   where
>     summary                              = makeSummary fWork.fwPreZones
>
>     smasher zrec rdFold                  =
>       let
>         tag                              = show (instKey zrec).pgkwInst
>         miset                            = fromIntegral zrec.zswInst `IntMap.lookup` summary
>         smashVar                         =
>           case miset of
>             Nothing                      → Nothing
>             Just iset                    → Just (computeInstSmashup
>                                                    tag
>                                                    (accessPreZones "smashup" fWork.fwPreZones iset))
>       in
>         (Just zrec{zsSmashup = smashVar}, rdFold)
>
> computeInstSmashup     :: String → IntMap PreZone → Smashing Word
> computeInstSmashup tag pzs_              =
>   profess
>     (not $ IntMap.null pzs_)
>     (unwords [fName, tag, "no zones"])
>     (smashSubspaces tag [qMidiSize128, qMidiSize128, 2] (map extractSpace pzs))
>   where
>     fName                                = "computeInstSmashup"
>
>     pzs                                  = IntMap.elems pzs_

reorg task ============================================================================================================
          where appropriate, make one instrument out of many

Overview: the member instruments of a qualified group will be absorbed into the lead (member). She takes all of their 
zones, in effect. The mapping (member → lead) :: (Word → Word) is turned into the absorption map (aMap).

To build the map
 1. collect all instrument names

 2. group by similar names

 3. drop the strings, retaining member → lead structure

 4. filter any proposed absorptions via #5 qualify

 5. implement a suitability calculation 

> reorgTaskIf _ _ fWork                    = zrecTask reorger fWork
>   where
>     Directives{ .. }  
>                                          = fWork.fwDirectives
>     closeEnough x y                      = absorbThreshold < howClose (fst x) (fst y)
>
>     summary                              = makeSummary fWork.fwPreZones
>
>     reorger zrec rdFold
>       | traceIf trace_R False            = undefined
>       | not doAbsorption                 = (Just zrec,                       rdFold)
>       | isJust dprobe                    = (Just zrec,                       dispose pergm scansBlocked rdFold)
>       | isNothing aprobe                 = (Just zrec,                       rdFold)
>       | party == wInst                   = (Just zrec{zsSmashup = Just hsmash},
>                                                                              dispose pergm scansIng rdFold)
>       | otherwise                        = (Nothing,                         dispose pergm scansEd rdFold)
>       where
>         fName                            = "reorger"
>         trace_R                          =
>           unwords [fName, "headed=", show headed
>                         , "hMap=", show $ length hMap
>                         , "dMap=", show $ length dMap
>                         , "aMap=", show $ length aMap]
>
>         pergm                            = instKey zrec
>         wInst          :: Int
>         wInst                            = fromIntegral zrec.zswInst
>
>         dprobe                           = IntMap.lookup wInst dMap
>         aprobe                           = IntMap.lookup wInst aMap
>         hprobe                           = IntMap.lookup wInst hMap
>
>         disqualified                     = deJust "dprobe" dprobe
>         party                            = deJust "aprobe" aprobe
>         (_, hsmash)                      = deJust "hprobe" hprobe
>
>         scansIng                         =
>           [Scan Modified Absorbing fName noClue]
>         scansEd                          =
>           [Scan Dropped Absorbed   fName (show party)]
>         scansBlocked                     =
>           [Scan NoChange NoAbsorption fName (show disqualified)]
>
>     headed             :: IntMap IntSet
>     headed                               = foldr (IntMap.union . rewire) IntMap.empty groups2
>       where
>         groups1, groups2
>                        :: [[(String, Int)]]
>         groups1                          = groupBy closeEnough instNames
>         groups2                          = filter noSingletons groups1
>                                              where noSingletons x = 1 < length x
>         instNames      :: [(String, Int)]
>         instNames                        = zrecCompute fWork extract []
>                                              where extract ns zrec = (zrec.zswChanges.cnName, wInst) : ns
>                                                      where wInst = fromIntegral zrec.zswInst
>         rewire         :: [(String, Int)] → IntMap IntSet
>         rewire ns                        = IntMap.insert ((snd . head) ns) (IntSet.fromList (map snd ns)) IntMap.empty
>
>     hMap               :: IntMap (IntMap PreZone, Smashing Word)
>     dMap               :: IntMap SmashStats
>     (hMap, dMap)                         = IntMap.mapEitherWithKey qualify headed
>       where
>         townersMap     :: IntMap (IntMap PreZone, Smashing Word)
>         townersMap                       =
>           let
>             tFolder    :: IntMap (IntMap PreZone, Smashing Word)
>                           → InstZoneRecord
>                           → IntMap (IntMap PreZone, Smashing Word)
>             tFolder m zrec               = IntMap.insert k (vpzs, vsmash) m
>               where
>                 k                        = fromIntegral zrec.zswInst
>                 vpzs                     = accessPreZones "towners" fWork.fwPreZones iset
>                 vsmash                   = deJust "townersMap smashup" zrec.zsSmashup
>                 iset                     = summary IntMap.! fromIntegral zrec.zswInst
>           in
>             zrecCompute fWork tFolder IntMap.empty 
>     
>         qualify        :: Int → IntSet → Either (IntMap PreZone, Smashing Word) SmashStats
>         qualify leadI memberIs
>           | 0 == osmashup.smashStats.countMultiples
>                                          = Left (rebased, osmashup)
>           | membersHaveVR                = Left (rebased, osmashup)
>           | otherwise                    = Right osmashup.smashStats
>           where
>             towners    :: [(IntMap PreZone, Smashing Word)]
>             towners                      = map (townersMap IntMap.!) (IntSet.toList memberIs)
>             members    :: IntMap PreZone
>             members                      = foldl' gr IntMap.empty (map fst towners)
>                                              where gr m item = m `IntMap.union` item
>             rebased                      = IntMap.map rebase members
>                                              where rebase pz = pz{pzWordI = fromIntegral leadI}
>             smashups                     = map snd towners
>             osmashup                     = (foldl' smashSmashings (head smashups) (tail smashups))
>                                              {smashTag = unwords [show (leadI, memberIs)]}
>             -- VR = Velocity Range(s)
>             membersHaveVR                =
>               let
>                 zoneHasVR pz             =
>                   case pz.pzDigest.zdVelRange of
>                     Just rng             → rng /= (0, 127)
>                     Nothing              → False
>                 zonesHaveVR              = any zoneHasVR
>               in
>                 all (zonesHaveVR . fst) towners
>
>     ready              :: IntMap IntSet
>     ready                                = IntMap.filterWithKey wasVetted headed
>                                              where wasVetted k _ = IntMap.member k hMap
>     aMap               :: IntMap Int
>     aMap                                 = IntMap.foldlWithKey fold1Fun IntMap.empty ready
>       where
>         fold1Fun       :: IntMap Int → Int → IntSet → IntMap Int
>         fold1Fun qIn wLead               =
>           let
>             fold2Fun qFold wMember       = IntMap.insert wMember wLead qFold
>           in
>             IntSet.foldl' fold2Fun qIn

match task ============================================================================================================
          accumulate all fuzzy matches

> matchTaskIf _ _ fWork                    = fWork{fwMatches = Matches sMatches iMatches}
>   where
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     sMatches                             =
>       Map.foldlWithKey compute Map.empty fWork.fwPreSamples
>         where compute m k v              = Map.insert k (computeFFMatches proConRatio v.cnName narrowRosterForBoot) m
>     iMatches                             =
>       let
>         computeFF      :: Map PerGMKey FFMatches → InstZoneRecord → Map PerGMKey FFMatches 
>         computeFF m zrec                 =
>           Map.insert (instKey zrec) (computeFFMatches proConRatio zrec.zswChanges.cnName narrowRosterForBoot) m
>       in
>         zrecCompute fWork computeFF Map.empty 

clean task ============================================================================================================
          removing zrecs that have gone bad

> cleanTaskIf _ _ fWork                    = zrecTask cleaner fWork
>   where
>     summary                              = makeSummary fWork.fwPreZones
>
>     cleaner zrec rdFold                  =
>       case iset of
>         Nothing                          → (Nothing, dispose (instKey zrec) ssNoZones rdFold)
>         _                                → (Just zrec, rdFold)
>       where
>         fName                            = "cleaner"
>
>         iset                             = fromIntegral zrec.zswInst `IntMap.lookup` summary 
>         ssNoZones                        =
>           [Scan Dropped NoZones fName noClue]

perI task =============================================================================================================
          generating PerInstrument map

> perITaskIf _ _ fWork                     = fWork{  fwPerInstruments   = perIs
>                                                  , fwDispositions     = rdOut}
>   where
>     summary                              = makeSummary fWork.fwPreZones
>
>     (perIs, rdOut)                       = zrecCompute fWork (uncurry perIFolder) (Map.empty, fWork.fwDispositions)
>
>     perIFolder         :: Map PerGMKey PerInstrument → ResultDispositions 
>                           → InstZoneRecord → (Map PerGMKey PerInstrument, ResultDispositions)
>     perIFolder m rdFold zrec             = (Map.insert (instKey zrec) perI m, rdFold'')
>       
>       where
>         fName                            = "perIFolder"
>
>         iset                             = fromMaybe IntSet.empty  miset -- (m, rdFold)
>         smashing                         = deJust fName zrec.zsSmashup
>         miset                            = fromIntegral zrec.zswInst `IntMap.lookup` summary 
>
>         perI                             = 
>           PerInstrument 
>             zrec.zswChanges 
>             iset
>             smashing
>
>         ssBless                          =
>           [Scan Accepted ToCache fName noClue]
>
>         rdFold'                          =
>           let
>             blessZone rd bag             = dispose (extractZoneKey pz) ssBless rd
>                                              where pz = accessPreZone "bless" fWork.fwPreZones bag
>           in
>             IntSet.foldl' blessZone rdFold perI.pZoneBags
>
>         rdFold''                         = dispose (instKey zrec) ssBless rdFold'

The End
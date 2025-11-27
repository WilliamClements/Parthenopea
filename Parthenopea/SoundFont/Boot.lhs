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

Current solution:
1. Make smashups for all Instruments. Remember, lazy! 
2. Use the smashups to drive reorg/absorption.
3. Throw away smashups.
4. Do pairing based on reorg'd Instruments.
5. Make all-new smashups based on reorg.

> data FileWork                            =
>   FileWork {
>     fwDirectives       :: Directives
>   , fwZRecs            :: IntMap InstZoneRecord        {- [InstIndex → zrec] -}
>   , fwPreZones         :: IntMap PreZone               {- [BagIndex → pz] -}
>   , fwOwners           :: IntMap IntSet                {- [InstIndex → [BagIndex]] -}
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
>    IntMap.empty
>    Map.empty
>    Map.empty
>    defMatches
>    defPairing
>    virginrd
>
> makeOwners             :: IntMap PreZone → IntMap IntSet
> makeOwners                               = IntMap.foldl' build IntMap.empty
>   where
>     build              :: IntMap IntSet → PreZone → IntMap IntSet
>     build m pz                           =
>       IntMap.insertWith IntSet.union (wordI pz) (IntSet.singleton (wordB pz)) m
>
> data Pairing                             =
>   Pairing {
>     fwPartners         :: IntMap Int                   {- [SampleIndex → SampleIndex] -}
>   , fwPairings         :: IntMap Int                   {- [BagIndex → BagIndex]       -}
>   , fwActions          :: IntMap IntSet                {- [InstIndex → [BagIndex]]    -}}
>   deriving Show
> defPairing             :: Pairing
> defPairing                               =
>   Pairing
>     IntMap.empty 
>     IntMap.empty
>     IntMap.empty
>
> data PairingSlot                         =
>   PairingSlot {
>     psInst             :: Maybe Word
>   , psKeyRange         :: (Word, Word)
>   , psVelRange         :: (Word, Word)}
>   deriving (Eq, Ord, Show)
>
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
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
>      , ("smash 1",    smash)
>      , ("reorg",      clean . reorg) 
>      , ("match",      match)
>      , ("pair",       pair)
>      , ("vet",        clean . vet)
>      , ("adopt",      adopt)
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

Most of the tasks add dispositions unproblematically - ignoring

Task preSample creates preSampleCache based on file's Sample Headers
Task smell creates sample-level Partner map, based on preSampleCache
Task capture is the composition of three Tasks
   Task survey creates the zrec IntMap based on file's Instrument data
   Task capture creates PreZones' IntMap, based on file's Zone data
   Task clean
      creates the Owners map based on PreZones' IntMap
      deletes empty zrecs based on Owners map
Task adopt adds dispos only, based on Owners map
Task smash creates Smashings based on Owners map and PreZone data
Task reorg modifies some PreZones' parent handles, thus invalidating Owners map
Task match modifies fuzzy data only 
Task pair completes Pairing, creates Action map, based on partners and PreZones, does not modify PreZones
Task vet modifies or deletes PreZones based on Action map
Task perI creates PerInstrument map based on Owners map and PreZone data

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
>         survey@Survey{ .. }                     
>                                          = reduceFileIterate ingestFile
>
>         ingestFile                       = head
>                                            $ dropWhile unfinished
>                                            $ iterate nextGen
>                                            $ makeFileIterate dives sffile rost
>           where
>             unfinished fiIn              = not (null fiIn.fiTaskIfs)
>             nextGen fiIn@FileIterate{ .. }
>               | traceIf trace_NG False   = undefined
>               | otherwise                = fiIn{ fiFw = (snd . head) fiTaskIfs fiFw
>                                                , fiTaskIfs = tail fiTaskIfs}
>               where
>                 fName                    = "nextGen"
>                 trace_NG                 = unwords [fName, show sffile.zWordFBoot
>                                                          , (fst . head) fiTaskIfs]
>
>         vFiles'                          = vFiles `VB.snoc` sffile{zPreZones = sPreZones}
>         survey'                          =
>           survey{sPerInstruments         = Map.union cacheIn sPerInstruments
>                , sMatches                = combineMatches matchesIn sMatches
>                , sDispositions           = combinerd rdIn sDispositions}
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

> zrecTask               :: (InstZoneRecord → ResultDispositions → (Maybe InstZoneRecord, IntMap PreZone, ResultDispositions))
>                           → FileWork → FileWork
> zrecTask userFun fw                      = fw{fwZRecs = zrecs', fwPreZones = preZones, fwDispositions = rd'}
>   where
>     zrecs'             :: IntMap InstZoneRecord
>     preZones           :: IntMap PreZone
>     rd'                :: ResultDispositions
>     (zrecs', preZones, rd')                    =
>       foldl' taskRunner (fw.fwZRecs, fw.fwPreZones, fw.fwDispositions) fw.fwZRecs
>
>     taskRunner         :: (IntMap InstZoneRecord, IntMap PreZone, ResultDispositions)
>                           → InstZoneRecord
>                           → (IntMap InstZoneRecord, IntMap PreZone, ResultDispositions)
>     taskRunner (zrecs, pzsSoFar, rdFold) zrec =
>       let
>         mzrec          :: Maybe InstZoneRecord
>         (mzrec, pzsFold, rdFold')        = userFun zrec rdFold
>
>         kfun _                           = mzrec
>       in
>         (IntMap.update kfun (fromIntegral zrec.zswInst) zrecs
>         , IntMap.union pzsFold pzsSoFar
>         , rdFold')
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
>     captx                                = zrecCompute fWork compute defCapture{uDispo = fWork.fwDispositions}
>       where
>         compute        :: Capture → InstZoneRecord → Capture
>         compute captFold zrecIn          =
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
>             rFolder captFold (bix, eor)  =
>               let
>                 doNormal pz              =
>                   captFold{uPzs = IntMap.insert (wordB pz) pz{pzSFZone = bz} captFold.uPzs}
>                   where bz = buildZone sffile captFold.uSFZone (Just pz) bix
>                 doError (k, ssZone)      =
>                   if hasImpact GlobalZone ssZone
>                     then captFold{uSFZone = bz}
>                     else captFold{uDispo = dispose k ssZone captFold.uDispo}
>                   where bz = buildZone sffile defZone Nothing bix
>               in
>                 either doNormal doError eor
>
>         stype pz                         = F.sampleType (effPZShdr pz)
>         hasRom pz                        = stype pz >= 0x8000
>         romClue pz                       = showHex (stype pz) []
>         rangeClue pz                     = show (pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange)
>
> buildZone              :: SFFileBoot → SFZone → Maybe PreZone → Word → SFZone
> buildZone sffile fromZone mpz bix
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = foldr addMod (foldl' addGen fromZone gens) mods
>   where
>     fName                                = "buildZone"
>     trace_BZ                             =
>       unwords [fName, show (sffile.zWordFBoot, bix), zName, show (fromZone == defZone)]
>
>     zName                                =
>       case mpz of
>         Nothing                          → "<global>"
>         Just pz                          → show (effPZShdr pz).sampleName
>     boota                                = sffile.zFileArrays
>
>     xgeni                                = F.genNdx $ boota.ssIBags!bix
>     ygeni                                = F.genNdx $ boota.ssIBags!(bix + 1)
>     xmodi                                = F.modNdx $ boota.ssIBags!bix
>     ymodi                                = F.modNdx $ boota.ssIBags!(bix + 1)
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
          store the pairings and reject action map, to be used by vet task

> pairTaskIf _ _ fWork
>   | traceIf trace_PTI False              = undefined
>   | otherwise                            =
>   fWork{fwPairing = fWork.fwPairing{fwPairings = pairings, fwActions = makeActions rejects}}
>   where
>     fName__                              = "pairTaskIf"
>     trace_PTI                            = unwords [fName__, show pairings]
>
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     Pairing{ .. }                        
>                                          = fWork.fwPairing

convenience functions =================================================================================================
          unpair           - cram all Ls and Rs from partner map into single set
          makeActions      - from reject bag indices, form action map (from instrument to affected zones)

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
>         make           :: IntMap IntSet → Int → IntMap IntSet 
>         make actions bix                 = IntMap.insertWith IntSet.union (wordI pz) (IntSet.singleton bix) actions
>                                              where pz = accessPreZone fName__ fWork.fwPreZones bix
    
main flow =============================================================================================================
          After somehow generating the pair list for this sffile, reject all other stereo zones - they failed to pair!
          The "somehow" is to make pairs if and only if L and R's excerpted zone data produce identical "pair slots".

>     rejects                              = IntMap.keysSet universe `IntSet.difference` unpair pairings
>                                              where universe = IntMap.filter isStereoPZ fWork.fwPreZones
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
>         regular, extra :: IntMap Int                   {- [BagIndex → BagIndex]         -}         
>         regular                          = IntMap.foldlWithKey (pFolder False IntMap.empty) IntMap.empty fwPartners
>         extra                            = IntMap.foldlWithKey (pFolder True regular)       IntMap.empty fwPartners
>
>         pFolder        :: Bool
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>                           → Int                        {- SampleIndex                   -}
>                           → Int                        {- SampleIndex                   -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>         pFolder exo done soFar siFrom siTo
>                                          = soFar `IntMap.union` qualifyPairs exo done bsL bsR
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
>     qualifyPairs exo done bixenL bixenR
>       | traceNot trace_QP False          = undefined
>       | otherwise                        = Map.foldlWithKey (pin $ peg bixenR') IntMap.empty (peg bixenL') 
>       where
>         fName_                           = unwords [fName__, "qualifyPairs"]
>         trace_QP                         = unwords [fName_, show (bixenL, bixenR)]
>
>         allowCross                       = exo && crossInstrumentPairing
>         allowParallel                    = exo && parallelPairing
> 
>         pairedSoFar                      = unpair done
>         bixenL'                          = bixenL `IntSet.difference` pairedSoFar
>         bixenR'                          = bixenR `IntSet.difference` pairedSoFar
>
>         peg            :: IntSet                       {- [BagIndex]                    -}
>                           → Map PairingSlot IntSet     {- [ps → [BagIndex]]             -}
>         peg                              = IntSet.foldl' pegBix Map.empty
>           where
>             pegBix m bix                 = 
>               let
>                 pz                       = accessPreZone "pegBix" fWork.fwPreZones bix
>                 iSlot                    = PairingSlot
>                                              (if allowCross || allowParallel then Nothing else Just pz.pzWordI)
>                                              (fromMaybe (0, 127) pz.pzDigest.zdKeyRange)
>                                              (fromMaybe (0, 127) pz.pzDigest.zdVelRange)
>               in
>                 Map.insertWith IntSet.union iSlot (IntSet.singleton bix) m
>
>         pin            :: Map PairingSlot IntSet       {- [ps → [BagIndex]]             -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>                           → PairingSlot                {- ps                            -}
>                           → IntSet                     {- [BagIndex]                    -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>         pin pegBoard m iSlot bL
>           | traceNot trace_P False       = undefined
>           | otherwise                    = m `IntMap.union` IntMap.fromList newPairs
>           where
>             fName                        = unwords [fName_, "pin"]
>             trace_P                      = unwords [fName, show pegBoard]
>
>             bR                           = Map.lookup iSlot pegBoard
>             zipped                       = zip (IntSet.toList bL) (maybe [] IntSet.toList bR)
>             newPairs
>               | null zipped              = []
>               | not allowParallel && not allowCross && length zipped == 1
>                                          = zipped
>               | otherwise                =
>                 (if allowParallel then zipParallel else []) ++ (if allowCross then zipCross else [])
>
>             (zipParallel, zipCross)      = partition (uncurry areParallel) zipped
>               where
>                 areParallel bixL bixR    =    (accessPreZone "bixL" fWork.fwPreZones bixL).pzWordI
>                                            == (accessPreZone "bixR" fWork.fwPreZones bixR).pzWordI   

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
> vetTaskIf _ _ fWork
>   | traceIf trace_VTI False              = undefined
>   | otherwise                            =
>   fWork{fwPreZones = vet.vPzs, fwDispositions = vet.vDispo}
>   where
>     fName__                              = "vetTaskIf"
>     trace_VTI                            = unwords [fName__, "before=", show $ length fWork.fwPreZones
>                                                            , "after=",  show $ length vet.vPzs]
>
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     Pairing{ .. }                      
>                                          = fWork.fwPairing
>     vet                                  =
>       zrecCompute fWork vetFolder (Vet fWork.fwPreZones fWork.fwDispositions)
>       where
>         vetFolder vetFold zrec           =
>           let
>             vactions                     = fromIntegral zrec.zswInst `IntMap.lookup` fwActions
>           in
>             maybe vetFold (vetActions vetFold zrec) vactions
>
>     vetActions         :: Vet → InstZoneRecord → IntSet → Vet
>     vetActions vetIn zrec actions        = vetIn{vPzs = pzsOut, vDispo = rdOut}
>       where
>         fName_                           = "vetActions"
>
>         actionFun                        = if switchBadStereoZonesToMono 
>                                              then makeThemMono
>                                              else killThem
>
>         (pzsOut, rdOut)                  = IntSet.foldl' (uncurry actionFun) (vetIn.vPzs, vetIn.vDispo) actions
>
>         makeThemMono, killThem, actionFun
>                        :: IntMap PreZone → ResultDispositions → Int → (IntMap PreZone, ResultDispositions)
>         makeThemMono pzs rd bix           =
>           let
>             pz                           = accessPreZone "makeThemMono" fWork.fwPreZones bix
>           in
>             (IntMap.update (Just . makeMono) (wordB pz) pzs, rd)
>             
>         killThem pzs rd bix              = 
>           let
>             pz                           = accessPreZone "killThem" fWork.fwPreZones bix
>             ssKill                       =
>               [Scan Violated BadStereoPartner fName_ zrec.zswChanges.cnName]
>           in
>             (IntMap.update (const Nothing) (wordB pz) pzs, dispose (extractZoneKey pz) ssKill rd)

adopt task ============================================================================================================
          mark adoption

> adoptTaskIf _ _ fWork
>   | traceIf trace_ATI False              = undefined
>   | otherwise                            = zrecTask adopter fWork
>   where
>     fName_                               = "adoptTaskIf"
>     trace_ATI                            = unwords [fName_, show $ IntMap.keysSet fWork.fwOwners]
>
>     adopter zrec rd                      =
>       let
>         miset                            = fromIntegral zrec.zswInst `IntMap.lookup` fWork.fwOwners
>       in
>         case miset of
>           Nothing                        → (Nothing, IntMap.empty, rd)
>           Just iset                      → (Just zrec, IntMap.empty, IntSet.foldl' (adopt zrec) rd iset)
>
>     adopt zrec rdFold bix
>       | traceNever trace_A False         = undefined
>       | otherwise                        = dispose (extractSampleKey pz) ssAdopt rdFold
>       where
>         fName                            = "adopt"
>         trace_A                          = unwords[fName, show (zrec.zswInst, bix)]
>
>         pz                               = accessPreZone fName fWork.fwPreZones bix
>         impact                           = if wasSwitchedToMono pz then AdoptedAsMono else Adopted
>         ssAdopt                          =
>           [Scan Modified impact fName zrec.zswChanges.cnName]

smash task ============================================================================================================
          compute smashups for each instrument

> smashTaskIf _ _ fWork                    = zrecTask smasher fWork
>   where
>     Pairing{ .. }
>                                          = fWork.fwPairing
>     bothPartners                         = IntMap.foldlWithKey reverser fwPairings fwPairings
>       where
>         reverser pds iLeft iRight        = IntMap.insert iRight iLeft pds
>
>     smasher zrec rdFold                  =
>       let
>         tag                              = show (instKey zrec).pgkwInst
>         miset                            = fromIntegral zrec.zswInst `IntMap.lookup` fWork.fwOwners
>         addPartners from                 = from `IntSet.union` newPartners
>           where
>             newPartners                  =
>               let 
>                 qualify bix              = bix `IntMap.lookup` bothPartners
>               in
>                 IntSet.fromList $ mapMaybe qualify (IntSet.toList from)
>         smashVar                         = miset
>                                            >>= Just . addPartners
>                                            >>= Just . accessPreZones "smasher" fWork.fwPreZones
>                                            >>= Just . computeInstSmashup tag
>       in
>         (Just zrec{zsSmashup = smashVar}, IntMap.empty, rdFold)
>
> computeInstSmashup     :: String → IntMap PreZone → Smashing Word
> computeInstSmashup tag pzs_
>   | traceIf trace_CIS False              = undefined
>   | otherwise                            =
>   profess
>     (not $ IntMap.null pzs_)
>     (unwords [fName, tag, "no zones"])
>     (smashSubspaces tag [qMidiSize128, qMidiSize128, 2] (map extractSpace pzs))
>   where
>     fName                                = "computeInstSmashup"
>     trace_CIS                            = unwords [fName, tag, show (IntMap.keys pzs_)]
>
>     pzs                                  = IntMap.elems pzs_

reorg task ============================================================================================================
          where appropriate, make one instrument out of many

Overview: the member instruments of a qualified group will be absorbed into the lead (member). She takes all of their 
zones, in effect. The mapping (member → lead) :: (Word → Word) is turned into the absorption map.

To build the map
 1. collect all instrument names

 2. group by similar names

 3. drop the strings, retaining member → lead structure

 4. filter any proposed absorptions via #5 qualify

 5. implement a suitability calculation 

> reorgTaskIf _ _ fWork                    = (zrecTask reorger fWork){fwOwners = IntMap.empty}
>   where
>     Directives{ .. }  
>                                          = fWork.fwDirectives
>     closeEnough x y                      = absorbThreshold < howClose (fst x) (fst y)
>
>     reorger zrec rdFold
>       | traceIf trace_R False            = undefined
>       | not doAbsorption                 = (Just zrec,                          IntMap.empty, rdFold)
>       | isJust dprobe                    = (Just zrec,                          IntMap.empty, dispose pergm scansBlocked rdFold)
>       | isNothing aprobe                 = (Just zrec,                          IntMap.empty, rdFold)
>       | party == wInst                   = (Just zrec{zsSmashup = Just hsmash}, rebased, dispose pergm scansIng rdFold)
>       | otherwise                        = (Nothing,                            IntMap.empty, dispose pergm scansEd rdFold)
>       where
>         fName                            = "reorger"
>         trace_R                          =
>           unwords [fName, "headed=", show headed
>                         , "holdMap=", show $ length holdMap
>                         , "disqualMap=", show $ length disqualMap
>                         , "absorptionMap=", show $ length absorptionMap]
>
>         pergm                            = instKey zrec
>         wInst          :: Int
>         wInst                            = fromIntegral zrec.zswInst
>
>         dprobe                           = IntMap.lookup wInst disqualMap
>         aprobe                           = IntMap.lookup wInst absorptionMap
>         hprobe                           = IntMap.lookup wInst holdMap
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
>     headed             :: IntMap IntSet                {- [InstIndex → [InstIndex]]     -}
>     headed                               = foldr (IntMap.union . rewire) IntMap.empty groups
>       where
>         groups         :: [[(String, Int)]]
>         groups                           = filter noSingletons (groupBy closeEnough instNames)
>                                              where noSingletons x = 1 < length x
>         instNames      :: [(String, Int)]
>         instNames                        = zrecCompute fWork extract []
>           where
>             extract ns zrec              = (zrec.zswChanges.cnName, fromIntegral zrec.zswInst) : ns
>
>         rewire         :: [(String, Int)] → IntMap IntSet
>         rewire ns                        =
>           IntMap.insert ((snd . head) ns) (IntSet.fromList (map snd ns)) IntMap.empty
>
>     holdMap            :: IntMap (IntMap PreZone, Smashing Word)
>                                                        {- [InstIndex → ([BagIndex → pz], smash] -}
>     disqualMap         :: IntMap SmashStats
>     (holdMap, disqualMap)                = IntMap.mapEitherWithKey qualify headed
>       where
>         townersMap     :: IntMap (IntMap PreZone, Smashing Word)
>                                                        {- [InstIndex → ([BagIndex → pz], smash] -}
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
>                 iset                     = fWork.fwOwners IntMap.! fromIntegral zrec.zswInst
>           in
>             zrecCompute fWork tFolder IntMap.empty 
>     
>         qualify        :: Int                          {- InstIndex                     -}
>                           → IntSet                     {- [InstIndex]                   -}
>                           → Either (IntMap PreZone, Smashing Word) SmashStats
>         qualify leadI memberIs
>           | 0 == osmashup.smashStats.countMultiples
>                                          = Left (rebased', osmashup)
>           | membersHaveVR                = Left (rebased', osmashup)
>           | otherwise                    = Right osmashup.smashStats
>           where
>             towners    :: [(IntMap PreZone, Smashing Word)]
>             towners                      = map (townersMap IntMap.!) (IntSet.toList memberIs)
>             members    :: IntMap PreZone               {- [BagIndex → pz] -}
>             members                      = foldl' grow IntMap.empty (map fst towners)
>                                              where grow m item = m `IntMap.union` item
>             rebased'                     = IntMap.map rebase members
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
>     ready              :: IntMap IntSet                {- [InstIndex → [InstIndex]]     -}
>     ready                                = IntMap.filterWithKey wasVetted headed
>                                              where wasVetted k _ = IntMap.member k holdMap
>     absorptionMap      :: IntMap Int                   {- [InstIndex → InstIndex]       -}
>     absorptionMap                        = IntMap.foldlWithKey fold1Fun IntMap.empty ready
>       where
>         fold1Fun       :: IntMap Int → Int → IntSet → IntMap Int
>         fold1Fun qIn wLead               =
>           let
>             fold2Fun qFold wMember       = IntMap.insert wMember wLead qFold
>           in
>             IntSet.foldl' fold2Fun qIn
>
>     rebased            :: IntMap PreZone
>     rebased                              = foldl' grow IntMap.empty holdMap
>       where
>         grow soFar oneSet                = soFar `IntMap.union` fst oneSet

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
          (re-)create owner map (instance index to bag indices)
          removing zrecs that have gone bad

> cleanTaskIf _ _ fWork                    = (zrecTask cleaner fWork){fwOwners = owners}
>   where
>     owners                               = makeOwners fWork.fwPreZones
>     cleaner zrec rdFold                  =
>       case iset of
>         Nothing                          → (Nothing, IntMap.empty, dispose (instKey zrec) ssNoZones rdFold)
>         _                                → (Just zrec, IntMap.empty, rdFold)
>       where
>         fName                            = "cleaner"
>
>         iset                             = fromIntegral zrec.zswInst `IntMap.lookup` owners
>         ssNoZones                        =
>           [Scan Dropped NoZones fName noClue]

perI task =============================================================================================================
          generating PerInstrument map

> perITaskIf _ _ fWork                     = fWork{  fwPerInstruments   = perIs
>                                                  , fwDispositions     = rdOut
>                                                  , fwPairing          = defPairing}
>   where
>     (perIs, rdOut)                       = zrecCompute fWork (uncurry perIFolder) (Map.empty, fWork.fwDispositions)
>
>     perIFolder         :: Map PerGMKey PerInstrument → ResultDispositions 
>                           → InstZoneRecord → (Map PerGMKey PerInstrument, ResultDispositions)
>     perIFolder m rdFold zrec             = (Map.insert (instKey zrec) perI m, dispose (instKey zrec) ssBless rdFold')
>       
>       where
>         fName                            = "perIFolder"
>
>         iset                             = fromMaybe IntSet.empty  miset -- (m, rdFold)
>         smashing                         = deJust fName zrec.zsSmashup
>         miset                            = fromIntegral zrec.zswInst `IntMap.lookup` fWork.fwOwners 
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
>             blessZone rd bix             = dispose (extractZoneKey pz) ssBless rd
>                                              where pz = accessPreZone "bless" fWork.fwPreZones bix
>           in
>             IntSet.foldl' blessZone rdFold perI.pBixen

The End
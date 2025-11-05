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
> import qualified Control.Monad           as CM
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
algebraic structure(s) that affect later stages. Dependency graph for this process turns out to have cycles in some
cases, unfortunately.

(1) reorg stage depends on previously computed smashups, but also modifies smashup of each absorption leader.

(2) In pipeline sequence, when is good time to NUKE badly formed stereo zones (assuming switchBadStereoZonesToMono is
False). But nuking a zone INVALIDATES Instrument's smashup! Which is expensive to recompute from scratch.

Note that _incremental_ smashing cheaply accomodates ADDING a zone to an Instrument. (Comes in handy, also, for explicit
cross pairing, which, in effect, adds each partner (zone) to the other's Instrument.)

(3) _crossInstrumentPairing_ is not applicable in individual case if reorg combines the two instruments. How does that
interact with _doAbsorption_?

...If both True, a valid cross instrument pairing can still work even if absorption didn't fix it
...If both False, easy, just don't do either
...If cross True but absorption False, cross pairing only accomplished the "hard" way
...If cross False but absorption True, cross pairing only accomplished implicitly (by not being crossing)

To solve our circular dependence problem, we do the following:
1. generate pairing data before smash or reorg steps, rejecting the unpartnered
2. run smash step, but skip zones in the rejection list.
3. run reorg to completion
4. regenerate revised pairing data that reflects absorptions.
5. incrementally update smashups for rejections that have been fixed by reorg.

> data FileWork                            =
>   FileWork {
>     fwDirectives       :: Directives
>   , fwZRecs            :: [InstZoneRecord]
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
>    []
>    Map.empty
>    Map.empty
>    defMatches
>    defPairing
>    virginrd
>
> data Pairing                             =
>   Pairing {
>     fwPartners         :: IntMap Int                   {- [SampleIndex → SampleIndex] -}
>   , fwPreZones         :: IntMap PreZone               {- [BagIndex → pz]             -}
>   , fwPairings         :: IntMap Int                   {- [BagIndex → BagIndex]       -}
>   , fwRejects          :: IntSet                       {- [BagIndex]                  -}
>   , fwActions          :: IntMap IntSet                {- [InstIndex → [BagIndex]]    -}}
> defPairing             ::Pairing
> defPairing                               =
>   Pairing
>     IntMap.empty
>     IntMap.empty 
>     IntMap.empty
>     IntSet.empty
>     IntMap.empty
>
> type PairingSlot                         = (Maybe Word, (Word, Word), (Word, Word))
>
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
> instance Show FileIterate where
>   show fi                                =
>     unwords ["FileIterate", show fi.fiFw]
> reduceFileIterate      :: FileIterate → (Map PerGMKey PerInstrument, Matches, ResultDispositions)
> reduceFileIterate FileIterate{ .. }      = (fiFw.fwPerInstruments, fiFw.fwMatches, fiFw.fwDispositions)
>
> preSampleTaskIf, smellTaskIf, surveyTaskIf, captureTaskIf, flatMapTaskIf, pairTaskIf, vetTaskIf
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
>      , ("capture",    capture . survey)
>      , ("pair 1",     pair . flatMap . clean)
>      , ("adopt",      adopt)
>      , ("smash",      smash)
>      , ("reorg",      reorg) 
>      , ("match",      match)
>      , ("pair 2",     pair . flatMap . clean)
>      , ("vet",        clean . vet)
>      , ("perI",       perI)]
>   where
>     preSample                            = preSampleTaskIf    sffile rost
>     smell                                = smellTaskIf        sffile rost
>     survey                               = surveyTaskIf       sffile rost
>     capture                              = captureTaskIf      sffile rost
>     flatMap                              = flatMapTaskIf      sffile rost
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
>                           → IO (Map PerGMKey PerInstrument, Matches, ResultDispositions)
> surveyInstruments dives rost vFilesBoot  = do
>   putStr $ reapEmissions
>     [   EndOfLine
>       , Unblocked fName_, EndOfLine
>       , Blanks 2, Unblocked $ show $ fst rost, EndOfLine
>       , Blanks 2, Unblocked $ show $ snd rost, EndOfLine, EndOfLine]
>   return $ foldl' bootFolder (Map.empty, defMatches, virginrd) vFilesBoot
>   where
>     fName_                               = "surveyInstruments"
>
>     bootFolder (icacheIn, matchesIn, rdIn) sffile
>                                          =
>       let
>         (icacheOut, matchesOut, rdOut)   = reduceFileIterate ingestFile
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
>       in
>         (Map.union icacheIn icacheOut, combineMatches matchesIn matchesOut, combinerd rdIn rdOut)

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

> goodZRecs              :: ResultDispositions → [InstZoneRecord] → [InstZoneRecord]
> goodZRecs rdNow                          = filter (\x → not (deadrd (instKey x) rdNow))
>
> data InstZoneRecord                      =
>   InstZoneRecord {
>     zswFile            :: Int
>   , zswInst            :: Word
>   , zswChanges         :: ChangeName F.Inst
>   , zsSmashup          :: Maybe (Smashing Word)
>   , zsPreZones         :: [PreZone]}
> instance Show InstZoneRecord where
>   show zrec                              =
>     unwords ["InstZoneRecord", show (zrec.zswFile, zrec.zswInst), show $ length zrec.zsPreZones]
> makeZRec               :: PerGMKey → ChangeName F.Inst → InstZoneRecord
> makeZRec pergm changes                          =
>   InstZoneRecord 
>     pergm.pgkwFile 
>     pergm.pgkwInst
>     changes Nothing []
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             =
>   PerGMKey 
>     zrec.zswFile 
>     zrec.zswInst
>     Nothing       

iterating InstZoneRecord list =========================================================================================

> zrecTask               :: (InstZoneRecord → ResultDispositions → (InstZoneRecord, ResultDispositions))
>                           → FileWork → FileWork
> zrecTask userFun fw                      = fw{fwZRecs = workedOn, fwDispositions = rd'}
>   where
>     (workedOn, rd')                      =
>       foldl' taskRunner ([], fw.fwDispositions) (goodZRecs fw.fwDispositions fw.fwZRecs)
>
>     taskRunner (zrecs, rdFold) zrec      =
>       let
>         (zrec', rdFold')                 = userFun zrec rdFold
>       in
>         (zrec' : zrecs, rdFold')
>
> zrecCompute            :: ∀ a . FileWork → (a → InstZoneRecord → a) → a → a
> zrecCompute fw userFun seed              = foldl' userFun seed (goodZRecs fw.fwDispositions fw.fwZRecs)
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
>       foldl' surveyFolder ([], fWork.fwDispositions) (formComprehension sffile ssInsts)
>     
>     violated impact clue             =
>       [Scan Violated impact fName clue]
>     accepted impact clue             =
>       [Scan Accepted impact fName clue]
>
>     surveyFolder       :: ([InstZoneRecord], ResultDispositions) → PerGMKey → ([InstZoneRecord], ResultDispositions)
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
>             makeZRec pergm (ChangeName iinst changes finalName) : zrecsFold
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
>     uPzs               :: [PreZone]
>  ,  uSFZone            :: SFZone
>  ,  uDispo             :: ResultDispositions}
> defCapture             :: Capture
> defCapture                               = Capture [] defZone virginrd
>
> captureTaskIf sffile _ fWork             = zrecTask capturer fWork
>   where
>     fName                                = "captureTaskIf"
>
>     accepted impact clue                 =
>       [Scan Accepted impact fName clue]
>     dropped impact clue                  =
>       [Scan Dropped impact fName clue]
>     modified impact clue                 =
>       [Scan Modified impact fName clue]
>     violated impact clue                 =
>       [Scan Violated impact fName clue]
>
>     capturer zrecIn rdIn                 =
>       let
>         (newPzs, rdOut)                  = captureZones zrecIn rdIn
>       in
>         (zrecIn{zsPreZones = newPzs}, rdOut)
>
>     captureZones       :: InstZoneRecord → ResultDispositions → ([PreZone], ResultDispositions)
>     captureZones zrec rdCap              = (capt.uPzs, dispose pergm ssCap capt.uDispo)
>       where
>         pergm                            = instKey zrec
>         iName                            = zrec.zswChanges.cnName
>
>         captureZone    :: Word → (Word, Either PreZone (PreZoneKey, [Scan]))
>         captureZone bix
>           | isNothing pz.pzDigest.zdSampleIndex
>                                          = (bix, Right (pzk, accepted GlobalZone       noClue))
>           | isNothing mpres              = (bix, Right (pzk, dropped  Orphaned         noClue))
>           | not (okGMRanges pz.pzDigest) = (bix, Right (pzk, violated BadGMRange       (rangeClue pz)))
>           | hasRom pz                    = (bix, Right (pzk, violated RomBased         (romClue pz)))
>           | isJust probeLimits           = (bix, Right (pzk, violated BadAppliedLimits (fromJust probeLimits)))
>           | otherwise                    = (bix, Left pz{pzChanges = ChangeEar (effPSShdr pres) []})
>           where
>             ibags                        = sffile.zFileArrays.ssIBags
>             xgeni                        = F.genNdx $ ibags ! bix
>             ygeni                        = F.genNdx $ ibags ! (bix + 1)
>             gens   :: [F.Generator]
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
>         capt                             = foldl' rFolder defCapture{uDispo = rdCap} results
>           where
>             rFolder        :: Capture → (Word, Either PreZone (PreZoneKey, [Scan])) → Capture
>             rFolder captFold (bagIndex, eor)
>                                          =
>               let
>                 doNormal pz              =
>                   captFold{uPzs = pz{pzSFZone = buildZone sffile captFold.uSFZone (Just pz) bagIndex}
>                            : captFold.uPzs}
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
>         noZones, yesCapture
>                        :: Maybe [Scan] 
>         ssCap                            = deJust fName
>                                            $ noZones `CM.mplus` yesCapture
>         noZones
>           | null capt.uPzs               = Just $ violated NoZones noClue
>           | otherwise                    = Nothing
>         yesCapture                       = Just $ modified Captured iName
>
> buildZone              :: SFFileBoot → SFZone → Maybe PreZone → Word → SFZone
> buildZone sffile fromZone mpz bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = foldr addMod (foldl' addGen fromZone gens) mods
>   where
>     fName                                = "buildZone"
>     trace_BZ                             =
>       unwords [fName, show (sffile.zWordFBoot, bagIndex), show zName, show (fromZone == defZone)]
>
>     zName                                =
>       case mpz of
>         Nothing                          → "<global>"
>         Just pz                          → (effPZShdr pz).sampleName
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

flatMap task ==========================================================================================================
          Erect temporary infrastructure for stereo pairings in form of a bag-to-PreZone map. To retain in effect one
          source of truth, this cache is used, only, during each run of pair task (first or second pass).

> flatMapTaskIf _ _ fWork
>   | traceIf trace_FMTI False             = undefined
>   | otherwise                            = fWork{fwPairing = fWork.fwPairing{fwPreZones = preZones}}
>   where
>     fName                                = "flatMapTaskIf"
>     trace_FMTI                           = unwords [fName, show $ IntMap.size preZones]
>
>     preZones                             = zrecCompute fWork aggregate IntMap.empty
>     aggregate soFar zrec                 =
>       soFar `IntMap.union` foldl' assign IntMap.empty stereoOnly
>       where
>         stereoOnly                       = filter isStereoPZ zrec.zsPreZones
>     assign m pz                          = IntMap.insert (wordB pz) pz m

pair task =============================================================================================================
          produce actions list setting aside questionable PreZones - second pass may or may not pair some of those
          "rejects" after all

> pairTaskIf _ _ fWork
>   | isFirstPass                          = updatePairing fWork
>   | otherwise                            = (updatePairing . handleUnrejections) fWork
>   where
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     Pairing{ .. }                        
>                                          = fWork.fwPairing
>
>     isFirstPass                          = IntMap.null fwPairings && IntSet.null fwRejects

convenience functions =================================================================================================
          updatePairing  - close pair task
          unpair         - cram all Ls and Rs from partner map into single set
          makeActions    - from reject bag indices, form action map (from instrument to affected zones)

>     updatePairing      :: FileWork → FileWork
>     updatePairing fwIn                   =
>       fwIn{fwPairing = fwIn.fwPairing{  fwPairings = pairings
>                                       , fwRejects = rejects
>                                       , fwPreZones = IntMap.empty}}
>
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
>                                              where pz = fwPreZones IntMap.! bag
    
meat ==================================================================================================================
          After somehow generating the pair list for this sffile, reject all other stereo bags - they failed to pair!
          The "somehow" is to make pairs if and only if L and R excerpted zone data match exactly; i.e. produce
          identical "pair slots".

>     rejects                              = IntMap.keysSet fwPreZones `IntSet.difference` unpair pairings
>
>     pairings                             = 
>       let
>         mLeft, mRight  :: IntMap IntSet                {- [SampleIndex → [BagIndex]]    -}
>         (mLeft, mRight)                  = IntMap.foldl' (uncurry flatFolder) (IntMap.empty, IntMap.empty) fwPreZones
>           where
>             flatFolder mleft mright pz
>               | isLeftPZ pz              = (putMembers pz mleft, mright)
>               | isRightPZ pz             = (mleft, putMembers pz mright)
>               | otherwise                = error "should already have filtered out mono case"
>             putMembers pz                =
>               IntMap.insertWith IntSet.union (wordS pz) (IntSet.singleton $ wordB pz)
>         
>         regular                          = IntMap.foldlWithKey (pairingFolder False) IntMap.empty fwPartners
>         extra                            = IntMap.foldlWithKey (pairingFolder True)  regular      fwPartners
>
>         pairingFolder  :: Bool →  IntMap Int → Int → Int → IntMap Int
>         pairingFolder exotic soFar siFrom siTo
>                                          = soFar `IntMap.union` qualifyPairs exotic fwPairings bsL bsR
>           where
>             bsL, bsR   :: IntSet                       {- [BagIndex]                    -}             
>             bsL                          = fromMaybe IntSet.empty (siFrom `IntMap.lookup` mLeft)
>             bsR                          = fromMaybe IntSet.empty (siTo   `IntMap.lookup` mRight)
>       in
>         regular `IntMap.union` if not isFirstPass then extra else IntMap.empty -- WOX
>
>     qualifyPairs       :: Bool
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>                           → IntSet                     {- [BagIndex]                    -}
>                           → IntSet                     {- [BagIndex]                    -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]         -}
>     qualifyPairs exotic alreadyDone bagsL bagsR
>                                          = Map.foldlWithKey (pin $ peg bagsR') IntMap.empty (peg bagsL')
>       where
>         allowCross                       = exotic && crossInstrumentPairing
>         allowParallel                    = exotic && parallelPairing
> 
>         pairedSoFar                      = unpair alreadyDone
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
>                 pz                       = fwPreZones IntMap.! bag
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
>             newPairs
>               | null zipped              = []
>               | not allowParallel && not allowCross && length zipped == 1
>                                          = zipped
>               | not allowParallel && not allowCross
>                                          = []
>               | otherwise                =
>                 (if allowParallel then zipParallel else []) ++ (if allowCross then zipCross else [])
>             zipped                       = zip (IntSet.toList bL) (IntSet.toList (fromMaybe IntSet.empty bR))
>             (zipParallel, zipCross)      = partition areParallel zipped
>               where
>                 areParallel (bagL, bagR) =    (fwPreZones IntMap.! bagL).pzWordI
>                                            == (fwPreZones IntMap.! bagR).pzWordI   
>           in
>             m `IntMap.union` IntMap.fromList newPairs
> 
>     handleUnrejections :: FileWork → FileWork
>     handleUnrejections fwIn
>       | traceIf trace_HU False           = undefined
>       | otherwise                        =
>       zrecTask unrejector (fwIn{fwPairing = fwIn.fwPairing{fwActions = makeActions rejects}})
>       where
>         fName_                           = "handleUnrejections"
>         trace_HU                         = unwords [fName_, show (fwRejects, rejects, unrejects)]
>
>         unrejects                        = fwRejects `IntSet.intersection` rejects
>         unactions                        = makeActions $ unrejects `IntSet.union` unpair crossers
>         crossers                         =
>           let
>             crossing iBag jBag           =
>               (fwPreZones IntMap.! iBag).pzWordI /= (fwPreZones IntMap.! jBag).pzWordI
>           in
>             IntMap.filterWithKey crossing pairings
>
>         unrejector zrec rd
>           | traceIf trace_U False       = undefined
>           | otherwise                    = (zrec{zsSmashup = maybe zrec.zsSmashup unreject mactions}, rd)
>           where
>             fName                        = unwords [fName_, "unrejector"]
>             trace_U                      = unwords [fName, show $ instKey zrec, show mactions]
>           
>             mactions                     = fromIntegral zrec.zswInst `IntMap.lookup` unactions
>
>             unreject   :: IntSet → Maybe (Smashing Word)
>             unreject unbags
>               | traceIf trace_UR False  = undefined
>               | otherwise                =
>               Just $ smashSmashings
>                        (fromMaybe (error fName) zrec.zsSmashup)
>                        (computeInstSmashup fName (toList pzs))
>               where
>                 trace_UR                 = unwords["unreject", show $ instKey zrec, show unbags]
>
>                 pzs    :: IntMap PreZone
>                 pzs                      = IntSet.foldl' dereference IntMap.empty unbags
>                 dereference
>                        :: IntMap PreZone → Int → IntMap PreZone
>                 dereference m unbag      =
>                   let
>                     mpz                  = unbag `IntMap.lookup` fwPreZones
>                     pz                   = fromMaybe (error "splaaat") mpz
>                   in
>                     IntMap.insert unbag pz m

vet task ==============================================================================================================
          switch bad stereo zones to mono, or off altogether

> vetTaskIf _ _ fWork                      = (zrecTask vetter fWork){fwPairing = defPairing}
>   where
>     Directives{ .. }
>                                          = fWork.fwDirectives                     
>     Pairing{ .. }                      
>                                          = fWork.fwPairing
>
>     vetter zrec rd                       =
>       let
>         vactions                         = fromIntegral zrec.zswInst `IntMap.lookup` fwActions
>       in
>         maybe (zrec, rd) (vetActions zrec rd) vactions
>
>     vetActions zrec rdIn actions
>       | traceIf trace_VA False           = undefined
>       | otherwise                        = (zrec{zsPreZones = pzsOut}, rdOut)
>       where
>         fName                            = "vetActions"
>         trace_VA                         = unwords [fName, show fwActions]
>
>         (pzsOut, rdOut)                  =
>           mapAction $ if switchBadStereoZonesToMono 
>                         then makeThemMono
>                         else killThem        
>             
>         makeThemMono pz rd               = (Just $ makeMono pz, rd)
>             
>         killThem pz rd                   = (Nothing, dispose (extractZoneKey pz) ssKill rd)
>           where
>             ssKill                       =
>               [Scan Violated BadStereoPartner fName zrec.zswChanges.cnName]
>
>         mapAction rejectFun              =
>           let
>             check pz                     = wordB pz `IntSet.member` actions
>             modifyFun pz rd              =
>               if wordB pz `IntSet.member` fwRejects
>                 then rejectFun pz rd
>                 else pairFun pz rd
>           in
>             zoneTask check modifyFun zrec.zsPreZones rdIn
>
>         pairFun pz rd                    = (Just pz, rd)

adopt task ============================================================================================================
          mark adoption

> adoptTaskIf _ _                          = zrecTask adopter
>   where
>     adopter zrec rd                      = (zrec, foldl' (adopt zrec) rd zrec.zsPreZones)
>
>     adopt zrec rdFold pz                 = 
>       let
>         fName                            = "adopter"
>
>         impact                           = if wasSwitchedToMono pz then AdoptedAsMono else Adopted
>         ssAdopt                          =
>           [Scan Modified impact fName zrec.zswChanges.cnName]
>       in
>         dispose (extractSampleKey pz) ssAdopt rdFold

smash task ============================================================================================================
          compute smashups for each instrument

> smashTaskIf _ _ fWork                    = zrecTask smasher fWork
>   where
>     Pairing{ .. }
>                                          = fWork.fwPairing
>
>     smasher zrec rdFold                  =
>       let
>         tag                              = show (instKey zrec).pgkwInst
>         pzs                              = filter ok zrec.zsPreZones
>                                              where ok pz = wordB pz `IntSet.notMember` fwRejects
>       in
>         (zrec{zsSmashup = Just (computeInstSmashup tag pzs)}, rdFold)
>
> computeInstSmashup     :: String → [PreZone] → Smashing Word
> computeInstSmashup tag pzs               = smashSubspaces tag [qMidiSize128, qMidiSize128, 2] (map extractSpace pzs)

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
>     reorger zrec rdFold
>       | traceIf trace_R False            = undefined
>       | not doAbsorption                 = (zrec,                       rdFold)
>       | isJust dprobe                    = (zrec,                       dispose pergm scansBlocked rdFold)
>       | isNothing aprobe                 = (zrec,                       rdFold)
>       | party == wInst                   = (zrec{zsPreZones = hpzs, zsSmashup = Just hsmash},
>                                                                         dispose pergm scansIng rdFold)
>       | otherwise                        = (zrec{zsPreZones = []},      dispose pergm scansEd rdFold)
>       where
>         fName                            = "reorger"
>         trace_R                          =
>           unwords [fName, "headed=", show headed
>                         , "ready=", show ready
>                         , "hMap=", show $ length hMap
>                         , "dMap=", show $ length dMap
>                         , "aMap=", show $ length aMap]
>
>         pergm                            = instKey zrec
>         wInst                            = zrec.zswInst
>
>         dprobe                           = Map.lookup wInst dMap
>         aprobe                           = Map.lookup wInst aMap
>         hprobe                           = Map.lookup wInst hMap
>
>         disqualified                     = deJust "dprobe" dprobe
>         party                            = deJust "aprobe" aprobe
>         (hpzs, hsmash)                   = deJust "hprobe" hprobe
>
>         scansIng                         =
>           [Scan Modified Absorbing fName noClue]
>         scansEd                          =
>           [Scan Dropped Absorbed   fName (show party)]
>         scansBlocked                     =
>           [Scan NoChange NoAbsorption fName (show disqualified)]
>
>     headed                               = foldr (Map.union . rewire) Map.empty groups
>       where
>         groups                           = filter noSingletons ((groupBy closeEnough . reverse) instNames)
>                                              where noSingletons x = 1 < length x
>         instNames                        = zrecCompute fWork extract []
>                                              where extract ns zrec = (zrec.zswChanges.cnName, zrec.zswInst) : ns
>         rewire ns                        = Map.insert ((snd . head) ns) (map snd ns) Map.empty
>
>     hMap               :: Map Word ([PreZone], Smashing Word)
>     dMap               :: Map Word SmashStats
>     (hMap, dMap)                         = Map.mapEitherWithKey qualify headed
>       where
>         townersMap                       =
>           let
>             tFolder m zrec               = Map.insert zrec.zswInst (zrec.zsPreZones, fromJust zrec.zsSmashup) m
>           in
>             zrecCompute fWork tFolder Map.empty 
>     
>         qualify        :: Word → [Word] → Either ([PreZone], Smashing Word) SmashStats
>         qualify leadI memberIs
>           | 0 == osmashup.smashStats.countMultiples
>                                          = Left (rebased, osmashup)
>           | membersHaveVR                = Left (rebased, osmashup)
>           | otherwise                    = Right osmashup.smashStats
>           where
>             towners                      = map (townersMap Map.!) memberIs
>
>             rebased                      = map rebase (concatMap fst towners)
>                                              where rebase pz = pz{pzWordI = leadI}
>
>             smashups                     = map snd towners
>             osmashup                     = (foldl' smashSmashings (head smashups) (tail smashups))
>                                              {smashTag = unwords [show (leadI, memberIs)]}
>
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
>     ready                                = Map.mapWithKey kingMe hMap
>                                              where kingMe k _ = headed Map.! k
>
>     aMap               :: Map Word Word
>     aMap                                 = Map.foldlWithKey fold1Fun Map.empty ready
>       where
>         fold1Fun qIn wLead               =
>           let
>             fold2Fun qFold wMember       = Map.insert wMember wLead qFold
>           in
>             foldl' fold2Fun qIn

match task ============================================================================================================
          track all fuzzy matches

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

> cleanTaskIf _ _                          = zrecTask cleaner
>   where
>     cleaner zrec rdFold
>       | null zrec.zsPreZones             = (zrec, dispose pergm ssNoZones rdFold)
>       | otherwise                        = (zrec, rdFold)
>       where
>         fName                            = "cleaner"
>
>         pergm                            = instKey zrec
>         ssNoZones                        =
>           [Scan Violated NoZones fName noClue]

perI task =============================================================================================================
          generating PerInstrument map

> perITaskIf _ _ fWork                     = fWork{  fwPerInstruments   = perIs
>                                                  , fwDispositions     = rdOut}
>   where
>     (perIs, rdOut)                        = zrecCompute fWork (uncurry perIFolder) (Map.empty, fWork.fwDispositions)
>
>     perIFolder m rdFold zrec
>       | traceNot trace_PIF False         = undefined
>       | otherwise                        = (m', rdFold')
>       where
>         fName                            = "perIFolder"
>         trace_PIF                        = unwords [fName, show zrec, show (instKey zrec), show perI]
>
>         perI                             =
>           PerInstrument 
>             zrec.zswChanges 
>             zrec.zsPreZones
>             (deJust fName zrec.zsSmashup)
>
>         m'                               = Map.insert (instKey zrec) perI m
>         rdFold'                          =
>           let
>             (_, rdz)                     = zoneTask (const True) blessZone perI.pZones rdFold
>             blessZone pz rdIn            = (Just pz, dispose (extractZoneKey pz) ssBless rdIn)
>             ssBless                      = [Scan Accepted ToCache fName noClue]
>           in
>             dispose (instKey zrec) ssBless rdz

The End
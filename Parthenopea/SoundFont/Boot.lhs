> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE UnicodeSyntax #-}

Boot
William Clements
January 21, 2025

> module Parthenopea.SoundFont.Boot (
>           FileSurvey(..)
>         , surveyInstruments ) where
>
> import qualified Codec.SoundFont         as F
> import Control.Applicative
> import Control.Lens hiding (element)
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Either
> import Data.IntMap ( IntMap )
> import qualified Data.IntMap             as IntMap
> import Data.IntSet ( IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.List 
> import qualified Data.Map.Lazy           as Lazy
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Euterpea.Music ( InstrumentName, PercussionSound )
> import Numeric ( showHex )
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.Repro.Zone
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.Runtime
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
  
a Boot problematic ====================================================================================================

Each stage interface function takes FileWork and returns modified FileWork. 

(1) reorg stage depends on previously computed smashups, but then recreates absorption leaders' smashups to reflect
the new zonage.

(2) Fast to modify smashups when incrementally adding zones. But to REMOVE a zone means expensively recomputing.

(3) two resources in jeopardy - zone owners and instrument smashups
    a. reorg goes ahead and patches up both
    b. vet patches up owners map explicitly
    c. what makes the smashups go wonky?
       1. deleting zones at pair → vet
       2. adding the cross instrument pairings

(4) How does _crossInstrumentPairing_ interact with _doAbsorption_?
a...If both True, a valid cross instrument pairing can still work even if absorption didn't fix it
b...If both False, easy, just don't do either
c...If cross True, absorption False, cross pairing only accomplished explicitly (honoring well-formed crossings)
d...If cross False, absorption True, cross pairing only accomplished implicitly (no longer crossing after absorption)

Garden an instrument collection =======================================================================================

> data InstZoneRecord                      =
>   InstZoneRecord {
>     zswFile            :: !Int
>   , zswInst            :: !Int
>   , zswChanges         :: ChangeName F.Inst
>   , zsSmashup          :: Maybe (Smashing Word)}

Runners keep stage interface functions as a simple fold ===============================================================

> data ZRecRunner                          =
>   ZRecRunner {
>     _iZRecs            :: IntMap InstZoneRecord
>  ,  _iDispo            :: ResultDispositions}
> defZRec                                  = ZRecRunner IntMap.empty virginrd
>
> data Capture                             =
>   Capture {
>     _uZRecs            :: IntMap InstZoneRecord
>  ,  _uPzs              :: IntMap PreZone
>  ,  _uSFZone           :: SFZone
>  ,  _uDispo            :: ResultDispositions}
> defCapture                               = Capture IntMap.empty IntMap.empty defZone virginrd
>
> data Vet                                 =
>   Vet {
>     _ipzdb             :: IntMap PreZone
>  ,  _ipDispo           :: ResultDispositions}
> defVet                                   = Vet IntMap.empty virginrd
>
> data PairsSurvey                         =
>   PairsSurvey {
>     _psUnpaired        :: IntSet                       {- [BagIndex]                             -}
>   , _psPaired          :: IntMap Int                   {- [BagIndex → BagIndex]                  -}
>   , _psDispos          :: ResultDispositions
>   , psTasks            :: [(String, PairsSurvey → IntMap Int)]}
> makeLenses ''PairsSurvey

FileWork ==============================================================================================================

> data Pairing                             =
>   Pairing {
>     _fwSamplePairings  :: IntMap Int                   {- [SampleIndex → SampleIndex]            -}
>   , _fwDirtyZs         :: IntMap IntSet}
>   deriving Show
> defPairing             :: Pairing
> defPairing                               =
>   Pairing
>     IntMap.empty 
>     IntMap.empty
> makeLenses ''Pairing
>
> data FileWork                            =
>   FileWork {
>     _fwDirectives      :: Directives
>   , _fwZRecs           :: IntMap InstZoneRecord        {- [InstIndex → zrec]                     -}
>   , _fwPreZones        :: IntMap PreZone               {- [BagIndex → pz]                        -}
>
>   , _fwZoneOwners      :: IntMap IntSet                {- [InstIndex → [BagIndex]]               -}
>   , _fwZoneCrossing    :: IntMap IntSet                {- [InstIndex → [BagIndex]]               -}
>
>   , _fwPreSamples      :: Map PreSampleKey PreSample
>   , _fwPerInstruments  :: Map PerGMKey PerInstrument
>   , _fwMatches         :: Matches
>   , _fwPairing         :: Pairing
>   , _fwDirtyIs         :: IntSet                       {- [InstIndex]                            -}
>   , _fwDispositions    :: ResultDispositions}
> defFileWork            :: Directives → FileWork
> defFileWork dives                        =
>   FileWork
>    dives 
>    IntMap.empty
>    IntMap.empty
>    IntMap.empty 
>    IntMap.empty
>    Map.empty
>    Map.empty
>    defMatches
>    defPairing
>    IntSet.empty
>    virginrd
> makeLenses ''FileWork
>
> makeLenses ''ZRecRunner
> makeLenses ''Capture
> makeLenses ''Vet

> instance Show FileWork where
>   show fw                                =
>     unwords [  "FileWork"
>              , show (length (fw ^. fwZRecs)), "=#zrecs"
>              , show (length (fw ^. fwPerInstruments)), "=#cached"
>              , show (fw ^. fwDispositions)]
>
> data PairingSlot                         =
>   PairingSlot {
>     psInst             :: Maybe Word
>   , psKeyRange         :: !(Word, Word)
>   , psVelRange         :: !(Word, Word)}
>   deriving (Eq, Ord, Show)
>
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
> instance Show FileIterate where
>   show fi                                =
>     unwords ["FileIterate", show fi.fiFw]
> reduceFileIterate      :: FileIterate → FileSurvey
> reduceFileIterate fi                     =
>   FileSurvey
>     (fi.fiFw ^. fwPreZones)
>     (fi.fiFw ^. fwPerInstruments)
>     (fi.fiFw ^. fwMatches)
>     (fi.fiFw ^. fwDispositions)
>
> data FileSurvey                          =
>   FileSurvey {
>     sPreZones          :: IntMap PreZone
>   , sPerInstruments    :: Map PerGMKey PerInstrument
>   , sMatches           :: Matches
>   , sDispositions      :: ResultDispositions}
> defSurvey                                =
>   FileSurvey
>     IntMap.empty
>     Map.empty
>     defMatches
>     virginrd

(Mostly ignoring dispo contribution as it is unproblematic)

Task *preSample*    caches file's Sample Headers
Task *smell*        creates sample-level Partner map, based on preSampleCache
Task *instrument*   creates the zrec collection (IntMap InstZoneRecord) based on file's Instrument data
Task *capture*      creates pzdb, based on file's Zone data, and generates zone owners
Task *clean*
      processes the dirty instrument list
      deletes empty zrecs based on owners
      repairs owners map
Task *adopt*        (adds dispos only, based on owners)
Task *smash*        creates smashups based on owners and PreZone data
Task *reorg*        invalidates owners by what it does
      deletes Instruments, in effect
      modifies thereby orphaned PreZones to belong to absorbing member Instrument
      repairs owners map afterward
Task *match*        (modifies fuzzy data only) 
Task *pair*
      creates Action map, based on partners and PreZones
      does not modify PreZones
      adjusts for exotic pairing ahead of smash 2 and perI
Task *vet*
      modifies or deletes PreZones based on Action map
Task *shrink*       carries out required smashup invalidations 
Task *perI*         creates PerInstrument map based on owners and PreZone data

FileWork development ==================================================================================================

> preSampleTaskIf, smellTaskIf, instrumentTaskIf, captureTaskIf, pairTaskIf, vetTaskIf
>                , adoptTaskIf, smashTaskIf, reorgTaskIf, matchTaskIf, cleanTaskIf, perITaskIf
>                , shrinkTaskIf
>                        :: SFFileBoot → ([InstrumentName], [PercussionSound]) → FileWork → FileWork
>
> makeFileIterate        :: Directives → SFFileBoot → ([InstrumentName], [PercussionSound]) → FileIterate
> makeFileIterate dives sffile rost        =
>   FileIterate
>     (defFileWork dives)
>     [
>        ("preSample",  preSample)
>      , ("smell",      smell)
>      , ("instrument", instrument)
>      , ("capture",    clean . shrink . capture)
>      , ("smash 1",    smash)
>      , ("reorg",      clean . shrink . reorg) 
>      , ("pair",       pair)
>      , ("vet",        clean . shrink . vet)
>      , ("match",      match)
>      , ("adopt",      adopt)
>      , ("smash 2",    smash)
>      , ("perI",       perI)]
>   where
>     preSample                            = preSampleTaskIf    sffile rost
>     smell                                = smellTaskIf        sffile rost
>     instrument                           = instrumentTaskIf   sffile rost
>     capture                              = captureTaskIf      sffile rost
>     clean                                = cleanTaskIf        sffile rost
>     smash                                = smashTaskIf        sffile rost
>     reorg                                = reorgTaskIf        sffile rost
>     match                                = matchTaskIf        sffile rost
>     pair                                 = pairTaskIf         sffile rost
>     vet                                  = vetTaskIf          sffile rost
>     adopt                                = adoptTaskIf        sffile rost
>     shrink                               = shrinkTaskIf       sffile rost
>     perI                                 = perITaskIf         sffile rost

Boot executive function ===============================================================================================
          returns overall PerInstrument map

> surveyInstruments      :: Directives
>                           → ([InstrumentName], [PercussionSound])
>                           → VB.Vector SFFileBoot
>                           → IO (VB.Vector SFFileBoot, FileSurvey)
> surveyInstruments dives rost sfs         = do
>   putStr $ reapEmissions
>     [   EndOfLine
>       , Unblocked fName_, EndOfLine
>       , Blanks 2, Unblocked $ show $ fst rost, EndOfLine
>       , Blanks 2, Unblocked $ show $ snd rost, EndOfLine, EndOfLine]
>   return $ VB.foldl' bootOne (VB.empty, defSurvey) sfs
>   where
>     fName_                               = "surveyInstruments"
>
>     bootOne            :: (VB.Vector SFFileBoot, FileSurvey) → SFFileBoot → (VB.Vector SFFileBoot, FileSurvey)
>     bootOne (vFiles, FileSurvey _ cacheIn matchesIn rdIn) sffile
>                                          =
>       let
>         sy, sy'        :: FileSurvey
>         sy                               = reduceFileIterate ingestFile
>         ingestFile                       = head
>                                            $ dropWhile unfinished
>                                            $ iterate' nextGen (makeFileIterate dives sffile rost)
>           where
>             unfinished fiIn              = not (null fiIn.fiTaskIfs)
>             nextGen fiIn@FileIterate{ .. }
>                                          = fiIn{ fiFw = (snd . head) fiTaskIfs fiFw
>                                                , fiTaskIfs = tail fiTaskIfs}
>         vFiles'                          = vFiles `VB.snoc` sffile{zPreZonesBoot = sy.sPreZones}
>         sy'                              =
>           sy{sPerInstruments             = Map.union cacheIn sy.sPerInstruments
>            , sMatches                    = combineMatches matchesIn sy.sMatches
>            , sDispositions               = combinerd rdIn sy.sDispositions}
>       in
>         (vFiles', sy')

support sample and instance ===========================================================================================

> combinerd              :: ResultDispositions → ResultDispositions → ResultDispositions
> combinerd rd1 rd2                        =
>   rd1{  preSampleDispos                  = Lazy.unionWith (++) rd1.preSampleDispos rd2.preSampleDispos
>       , preInstDispos                    = Lazy.unionWith (++) rd1.preInstDispos   rd2.preInstDispos
>       , preZoneDispos                    = Lazy.unionWith (++) rd1.preZoneDispos   rd2.preZoneDispos}
>
> formComprehension      :: ∀ r a . SFKeyType r ⇒ SFFileBoot → (FileArrays → Array Word a) → [r]
> formComprehension sffile blobfun         = map (sfkey sffile.zWordFBoot) bRange
>   where
>     fName                                = "formComprehension"
>
>     (stF, enF)                           = bounds $ blobfun sffile.zFileArrays
>     bRange                               =
>       profess
>         ((stF == 0) && (stF <= enF) && (enF < 2_147_483_648))
>         (error $ unwords [fName, "corrupt blob indexing", show (stF, enF)])
>         (deriveRange stF enF)

pre-sample task =======================================================================================================
          critique all Sample records in the file

> preSampleTaskIf sffile _ fWork           = foldl' sample fWork (formComprehension sffile ssShdrs)
>   where
>     Directives{ .. }
>                                          = fWork ^. fwDirectives                     
>
>     fName                                = "preSampleTaskIf"
>
>     violated, accepted :: Impact → String → [Scan]
>     violated imp clue                    =
>       [Scan Violated imp fName clue]
>     accepted imp clue                    =
>       [Scan Accepted imp fName clue]
>
>     sample fwForm presk                  =
>       let
>         preSamples                       = fwForm ^. fwPreSamples
>         preSampleCache                   =
>           if dropped ssSample
>             then preSamples
>             else Map.insert presk ps preSamples
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
>         sampleSizeOk stS enS             = stS >= 0 && enS - stS >= 32 && enS - stS < 2 ^ (22::Int)
>         goodSampleRate x                 = x == clip (64, 2 ^ (20::Int)) x
>
>         ssSample_
>           | not (goodSampleRate shdr.sampleRate)
>                                          = violated BadSampleRate (show shdr.sampleRate)
>           | isNothing mtype              = violated BadSampleType (show shdr.sampleType)
>           | not (sampleSizeOk shdr.start shdr.end)
>                                          = violated BadSampleLimits (show (shdr.start, shdr.end))
>           | not (goodName raw)           = badButMaybeFix fixBadNames BadName fName raw good
>           | otherwise                    = accepted ToProgram stereo
>
>         (ssSample, changes, name)        = if wasRescued BadName ssSample_
>                                              then (ssSample_, singleton FixBadName, good)
>                                              else (ssSample_, [],                   raw)
>       in
>         (  (fwPreSamples .~ preSampleCache)
>          . (fwDispositions .~ dispose presk ssSample (fwForm ^. fwDispositions))) fwForm

smell task ============================================================================================================
          create sample header partner map
          - note: all successful non-linkless PreZone pairings map their L and R to these shdr partners

> smellTaskIf _ _ fWork                    = ((fwPairing . fwSamplePairings) .~ partnerMap) fWork
>   where
>     partnerMap         :: IntMap Int                   {- [SampleIndex → SampleIndex]            -}
>     partnerMap                           = Map.foldlWithKey smell IntMap.empty allLeft
>                                              where allLeft = Map.filter isLeftPS preSamples
>     preSamples                           = fWork ^. fwPreSamples
>
>     smell              :: IntMap Int → PreSampleKey → PreSample → IntMap Int
>     smell m presk pres                   = maybe m enter mback
>       where
>         enter siEnter                    = IntMap.insert siIn siEnter m
>
>         siIn, siOut    :: Int
>         siIn                             = fromIntegral presk.pskwSampleIndex
>         siOut                            = fromIntegral (effPSShdr pres).sampleLink
>
>         mback_                           = presk{pskwSampleIndex = fromIntegral siOut} `Map.lookup` preSamples
>         mback                            = mback_ >>= backOk
>
>         backOk opres                     =
>           if isRightPS opres && fromIntegral (effPSShdr opres).sampleLink == siIn
>             then Just siOut
>             else Nothing

InstZoneRecord administration =========================================================================================

> instance Show InstZoneRecord where
>   show zrec                              =
>     unwords ["InstZoneRecord", show (zrec.zswFile, zrec.zswInst)]
> makeZRec               :: PerGMKey → ChangeName F.Inst → InstZoneRecord
> makeZRec pergm changes                   =
>   InstZoneRecord 
>     pergm.pgkwFile 
>     (fromIntegral pergm.pgkwInst)
>     changes Nothing
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             = stdPerGMKey zrec.zswFile zrec.zswInst
> zrecSmash              :: FileWork → Int → Smashing Word
> zrecSmash fWork inst                     = fromJust ((fWork ^. fwZRecs) IntMap.! inst).zsSmashup

iterating InstZoneRecord collection ===================================================================================

> zrecTask               :: (InstZoneRecord
>                            → ResultDispositions
>                            → (Maybe InstZoneRecord, ResultDispositions))
>                            → FileWork → FileWork
> zrecTask userFun fWork                   = ((fwZRecs .~ zrecs') . (fwDispositions .~ rd')) fWork
>   where
>     zrecs'             :: IntMap InstZoneRecord
>     rd'                :: ResultDispositions
>     (zrecs', rd')                        =
>       foldl' taskRunner (fWork ^. fwZRecs, fWork ^. fwDispositions) (fWork ^. fwZRecs)
>
>     taskRunner         :: (IntMap InstZoneRecord, ResultDispositions)
>                           → InstZoneRecord
>                           → (IntMap InstZoneRecord, ResultDispositions)
>     taskRunner (zrecs, rdFold) zrec      =
>       let
>         mzrec          :: Maybe InstZoneRecord
>         (mzrec, rdFold')                 = userFun zrec rdFold
>       in
>         (IntMap.update (const mzrec) zrec.zswInst zrecs, rdFold')

instrument task =======================================================================================================
          instantiate InstZoneRecord per Instrument

> instrumentTaskIf sffile _ fWork          = work
>   where
>     fName                                = "instrumentTaskIf"
>
>     Directives{ .. }
>                                          = fWork ^. fwDirectives                     
>
>     processed          :: ZRecRunner     = foldl' instInst (spawn fWork defZRec) (formComprehension sffile ssInsts)
>     work               :: FileWork       = imbibe fWork processed
>
>     instInst           :: ZRecRunner → PerGMKey → ZRecRunner
>     instInst inst pergm
>       | iinst.instBagNdx <= jinst.instBagNdx
>                                          = ((iZRecs .~ zrecs') . (iDispo .~ rd')) inst
>       | otherwise                        =
>         error $ unwords [fName, "corrupt instBagNdx", show (iinst.instBagNdx, jinst.instBagNdx)]
>       where
>         loadInst pergm                   = sffile.zFileArrays.ssInsts ! pergm.pgkwInst
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pergm.pgkwInst + 1}
>
>         raw                              = iinst.instName
>         good                             = fixName raw
>
>         zrecs'
>           | dropped ssSurvey             = inst ^. iZRecs
>           | otherwise                    = IntMap.insert newZRec.zswInst newZRec (inst ^. iZRecs)
>           where
>             changes                      = if wasRescued BadName ssSurvey then singleton FixBadName else []
>             finalName                    = if wasRescued BadName ssSurvey then good else raw
>             cn                           =
>               ChangeName
>                 iinst
>                 changes
>                 finalName
>             newZRec                      = makeZRec pergm cn
>         rd'                              = dispose pergm ssSurvey (inst ^. iDispo)
>
>         ssSurvey
>           | iinst.instBagNdx == jinst.instBagNdx
>                                          = violated NoZones (show iinst.instName)
>           | not (goodName raw)           = badButMaybeFix fixBadNames BadName fName raw good
>           | otherwise                    = accepted ToProgram (show pergm.pgkwInst)
>
>     violated imp clue                    =
>       [Scan Violated imp fName clue]
>     accepted imp clue                    =
>       [Scan Accepted imp fName clue]

capture task ==========================================================================================================
          populate zrecs with PreZones

> captureTaskIf sffile _ fWork             = (fwZoneOwners .~ makeOwners (work ^. fwPreZones)) work
>   where
>     fName_                               = "captureTaskIf"
>
>     Directives{ .. }
>                                          = fWork ^. fwDirectives
>     SynthSwitches{ .. }                  
>                                          = synthSwitches
>
>     processed          :: Capture        = IntMap.foldl' capture (spawn fWork defCapture) (fWork ^. fwZRecs)
>     work               :: FileWork       = imbibe fWork processed
>
>     capture            :: Capture → InstZoneRecord → Capture
>     capture captIn zrec                  =
>       ((uZRecs .~ zrecs) . (uDispo .~ dispose pergm ssCap (captOut ^. uDispo))) captOut
>       where
>         pergm                            = instKey zrec
>         iName                            = zrec.zswChanges.cnName
>
>         (zrecs, ssCap)                   =
>           if any (isLeft . snd) results
>             then (captOut ^. uZRecs,                                              ssInstrumentCaptured iName)
>             else (IntMap.update (const Nothing) zrec.zswInst (captOut ^. uZRecs), ssBadZones)
>
>         capturePreZone :: Word → (Word, Either PreZone (PreZoneKey, [Scan]))
>         capturePreZone bix
>           | isNothing digest.zdSampleIndex
>                                          = (bix, Right (prezk, ssGlobalZone))
>           | isNothing mpres              = (bix, Right (prezk, ssOrphaned))
>           | not (okGMRanges digest)      = (bix, Right (prezk, ssBadGMRange      (rangeClue pz)))
>           | hasRom pz                    = (bix, Right (prezk, ssRom             (romClue pz)))
>           | isJust probeLimits           = (bix, Right (prezk, ssApplied         (fromJust probeLimits)))
>           | otherwise                    = (bix, Left pz{pzChanges = ChangeEar (effPSShdr pres) []})
>           where
>             gens       :: [F.Generator]
>             gens                         =
>               let
>                 (xgeni, ygeni)           = (F.genNdx $ ibags ! bix, F.genNdx $ ibags ! (bix + 1))
>                                              where ibags = sffile.zFileArrays.ssIBags
>               in
>                 profess
>                   (0 <= xgeni && xgeni <= ygeni)
>                   (unwords [fName_, "SoundFont file corrupt (gens)"])
>                   (map (sffile.zFileArrays.ssIGens !) (deriveRange xgeni ygeni))
>             
>             digest                       = formDigest gens                                 
>
>             pz                           = makePreZone sffile.zWordFBoot pergm.pgkwInst bix digest pres.cnSource
>
>             prezk                        = PreZoneKey 
>                                             sffile.zWordFBoot 
>                                             pergm.pgkwInst
>                                             bix
>                                             pz.pzWordS
>             presk                        = PreSampleKey 
>                                             sffile.zWordFBoot 
>                                             pz.pzWordS
>
>             mpres                        = presk `Map.lookup` (fWork ^. fwPreSamples)
>             pres                         = deJust (unwords [fName_, "pres"]) mpres
>
>             probeLimits                  =
>               if ok
>                 then Nothing
>                 else Just $ unwords [showHex stA [], showHex enA [], showHex stL [], showHex enL []
>                                    , show digest.zdSampleMode]
>               where
>                 shdr                     = effPZShdr pz
>
>                 stA                      = shdr.start     + fromIntegral digest.zdStart
>                 enA                      = shdr.end       + fromIntegral digest.zdEnd
>                 stL                      = shdr.startLoop + fromIntegral digest.zdStartLoop
>                 enL                      = shdr.endLoop   + fromIntegral digest.zdEndLoop
>
>                 ok                       =
>                   0 <= stA && stA <= enA && 0 <= stL && stL <= enL
>                   && enA - stA < 2 ^ (22::Word)
>                   && (digest.zdSampleMode == Just A.NoLoop || enL - stL < 2 ^ (22::Word))

produce and process capture results ===================================================================================

>         results                          =
>           let
>             iinsts                       = sffile.zFileArrays.ssInsts
>             ibagi                        = F.instBagNdx (iinsts ! pgkwInst pergm)
>             jbagi                        = F.instBagNdx (iinsts ! (pgkwInst pergm + 1))
>           in
>             map capturePreZone (deriveRange ibagi jbagi)
>
>         captOut                           = foldl' process captIn results
>           where
>             process captFold (bix, eor)  =
>               let
>                 doNormal pz              = (uPzs .~ upzs') captFold
>                   where
>                     bz                   = buildZone (captFold ^. uSFZone) (Just pz) bix
>                     upzs'                = IntMap.insert (wordB pz) pz{pzSFZone = bz} (captFold ^. uPzs)
>
>                 doError (k, ssZone)      =
>                   if hasImpact GlobalZone ssZone
>                     then (uSFZone .~ bz) captFold
>                     else (uDispo .~ dispose k ssZone (captFold ^. uDispo)) captFold
>                   where
>                     bz                   = buildZone defZone Nothing bix
>               in
>                 either doNormal doError eor
>
>         stype pz                         = F.sampleType (effPZShdr pz)
>         hasRom pz                        = stype pz >= 0x8000
>         romClue pz                       = showHex (stype pz) []
>         rangeClue pz                     = show (pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange)

consume zone ==========================================================================================================

>     buildZone          :: SFZone → Maybe PreZone → Word → SFZone
>     buildZone fromZone mpz bix
>       | traceIf trace_BZ False           = undefined
>       | otherwise                        = foldr (uncurry addMod) (foldl' addGen fromZone gens) mods
>       where
>         fName                            = "buildZone"
>         trace_BZ                         =
>           unwords [fName, show (sffile.zWordFBoot, bix), zName, show (fromZone == defZone)]
>
>         zName                            =
>           case mpz of
>             Nothing                      → "<global>"
>             Just pz                      → show (effPZShdr pz).sampleName
>         boota                            = sffile.zFileArrays
>
>         xgeni                            = F.genNdx $ boota.ssIBags!bix
>         ygeni                            = F.genNdx $ boota.ssIBags!(bix + 1)
>         xmodi                            = F.modNdx $ boota.ssIBags!bix
>         ymodi                            = F.modNdx $ boota.ssIBags!(bix + 1)
>
>         gens           :: [F.Generator]  =
>           profess
>             (xgeni <= ygeni)
>             (unwords[fName, "SoundFont file", show sffile.zWordFBoot, sffile.zFilename, "corrupt gens"])
>             (map (boota.ssIGens !) (deriveRange xgeni ygeni))
>         baseId         :: Node           = if fromZone == defZone then 10_000 else 20_000
>         mods           :: [(Node, F.Mod)]
>         mods                             =
>           profess
>             (xmodi <= ymodi)
>             (unwords[fName, "SoundFont file", show sffile.zWordFBoot, sffile.zFilename, "corrupt mods"])
>             (zip [baseId..] (map (boota.ssIMods !) (deriveRange xmodi ymodi)))
>
>     addGen             :: SFZone → F.Generator → SFZone
>     addGen iz gen =
>       case gen of
>       F.InstIndex w                      → iz {zInstIndex =                Just w}
>       F.Key w                            → iz {zKey =                      tmclip w}
>       F.Vel w                            → iz {zVel =                      tnclip w}
>       F.InitAtten i                      → iz {zInitAtten =                tdclip i}
>       F.CoarseTune i                     → iz {zCoarseTune =               t1clip i}
>       F.FineTune i                       → iz {zFineTune =                 t2clip i}
>       F.SampleIndex w                    → iz {zSampleIndex =              Just w}
>       F.SampleMode m                     → iz {zSampleMode =               Just m}
>       F.ScaleTuning i                    → iz {zScaleTuning =              t3clip i}
>       F.ExclusiveClass i                 → iz {zExclusiveClass =           (tnclip . fromIntegral) i}
>
>       F.DelayVolEnv i                    → iz {zDelayVolEnv =              tcclip i}
>       F.AttackVolEnv i                   → iz {zAttackVolEnv =             tcclip i}
>       F.HoldVolEnv i                     → iz {zHoldVolEnv =               tcclip i}
>       F.DecayVolEnv i                    → iz {zDecayVolEnv =              tcclip i}
>       F.SustainVolEnv i                  → iz {zSustainVolEnv =            tdclip i}
>       F.ReleaseVolEnv i                  → iz {zReleaseVolEnv =            tbclip i}
>
>       F.Chorus i                         → iz {zChorus =                   ticlip i}
>       F.Reverb i                         → iz {zReverb =                   ticlip i}
>       F.Pan i                            → iz {zPan =                      tpclip i}
>
>       F.RootKey w                        → iz {zRootKey =                  tmclip w}
>
>       F.ModLfoToPitch i                  → iz {zModLfoToPitch =            teclip i}
>       F.VibLfoToPitch i                  → iz {zVibLfoToPitch =            teclip i}
>       F.ModEnvToPitch i                  → iz {zModEnvToPitch =            teclip i}
>       F.InitFc i                         → iz {zInitFc =                   tfclip i}
>       F.InitQ i                          → iz {zInitQ =                    tqclip i}
>       F.ModLfoToFc i                     → iz {zModLfoToFc =               teclip i}
>       F.ModEnvToFc i                     → iz {zModEnvToFc =               teclip i}
>       F.ModLfoToVol i                    → iz {zModLfoToVol =              tvclip i}
>       F.DelayModLfo i                    → iz {zDelayModLfo =              tcclip i}
>       F.FreqModLfo i                     → iz {zFreqModLfo =               taclip i}
>       F.DelayVibLfo i                    → iz {zDelayVibLfo =              tcclip i}
>       F.FreqVibLfo i                     → iz {zFreqVibLfo =               taclip i}
>       F.DelayModEnv i                    → iz {zDelayModEnv =              tcclip i}
>       F.AttackModEnv i                   → iz {zAttackModEnv =             tbclip i}
>       F.HoldModEnv i                     → iz {zHoldModEnv =               tcclip i}
>       F.DecayModEnv i                    → iz {zDecayModEnv =              tbclip i}
>       F.SustainModEnv i                  → iz {zSustainModEnv =            ticlip i}
>       F.ReleaseModEnv i                  → iz {zReleaseModEnv =            tbclip i}
>       F.KeyToModEnvHold i                → iz {zKeyToModEnvHold =          tkclip i}
>       F.KeyToModEnvDecay i               → iz {zKeyToModEnvDecay =         tkclip i}
>       F.KeyToVolEnvHold i                → iz {zKeyToVolEnvHold =          tkclip i}
>       F.KeyToVolEnvDecay i               → iz {zKeyToVolEnvDecay =         tkclip i}
>       _                                  → iz
>
>     addMod             :: Node → F.Mod → SFZone → SFZone
>     addMod mId fmod sfzone               = maybe sfzone hookIn mmod
>       where
>         hookIn m8r                       = sfzone{zModulators = m8r : sfzone.zModulators}
>
>         modSrc                           = unpackModSrc fmod.srcOper
>         amtSrc                           = unpackModSrc fmod.amtSrcOper
>
>         mmod
>           | not useModulators            = Nothing
>           | isNothing modSrc || isNothing amtSrc
>                                          = Nothing
>           | otherwise                    =
>             Just defModulator{mrModId = mId}
>               >>= addAmtSrc                (fromJust amtSrc)
>               >>= addSrc                   (fromJust modSrc)
>               >>= addDest                  fmod.destOper
>               >>= addAmount                (fromIntegral fmod.amount)
>               
>     ssTempl8 dispo imp clue              = [Scan dispo imp fName_ clue]
>     ssGlobalZone                         = ssTempl8 Accepted   GlobalZone   noClue
>     ssOrphaned                           = ssTempl8 Dropped    Orphaned     noClue
>     ssInstrumentCaptured                 = ssTempl8 Modified   Captured
>     ssBadZones                           = ssTempl8 Violated   BadZones     "see PreZone dispositions"
>     ssBadGMRange                         = ssTempl8 Violated   BadGMRange
>     ssRom                                = ssTempl8 Violated   RomBased
>     ssApplied                            = ssTempl8 Violated   BadAppliedLimits

pair task =============================================================================================================
          store (1) reject action map (dirty zs), (2) dispo (3) dirty is (4) crossers, to be used later on

> pairTaskIf _ _ fWork                     =
>   ( (fwPairing . fwDirtyZs         .~ dirtyZs)
>    . (fwDispositions               .~ (sy ^. psDispos))
>    . (fwDirtyIs                    .~ dirtyIs)
>    . (fwZoneCrossing               .~ crossers)) fWork
>   where
>     fName__                              = "pairTaskIf"
>
>     Directives{ .. }
>                                          = fWork ^. fwDirectives 
>                    
>     pzdb                                 = fWork ^. fwPreZones

pairing approach ======================================================================================================
          After somehow generating the pair list for this sffile, reject all other stereo zones - they failed to pair!
          The "somehow" is to make pairs if and only if L and R's zones produce identical "pair slots".

          And remember: peg 'em and pin 'em! I.E. collate (peg) candidates to push (pin) matchers to pairs list.

>     sy                 :: PairsSurvey    = head $ dropWhile unfinished $ iterate' nextGen sinit
>       where
>         sinit                            =
>           PairsSurvey 
>             (IntMap.keysSet $ IntMap.filter isStereoPZ pzdb) 
>             IntMap.empty
>             (fWork ^. fwDispositions)
>             [("nominal", nominal), ("exotic", exotic), ("linkless", linkless)]
>         unfinished sy                    = not (IntSet.null (sy ^. psUnpaired)) && not (null sy.psTasks)
>         nextGen sy                       =
>           let
>             pFunction                    = (fst . head) sy.psTasks
>             newPairs                     = (snd . head) sy.psTasks sy
>             dispos'                      = IntMap.foldlWithKey' announce (sy ^. psDispos) newPairs
>             announce rdFold ifrom ito    = (dispose pzkFrom ssFrom . dispose pzkTo ssTo) rdFold
>               where
>                 pzkFrom                  = extractZoneKey $ pzdb IntMap.! ifrom
>                 pzkTo                    = extractZoneKey $ pzdb IntMap.! ito
>                 clueFrom                 = unwords [pFunction, show ito]
>                 clueTo                   = unwords [pFunction, show ifrom]
>                 ssFrom                   = [Scan Modified Paired fName__ clueFrom] 
>                 ssTo                     = [Scan Modified Paired fName__ clueTo] 
>           in
>             PairsSurvey
>               ((sy ^. psUnpaired) `IntSet.difference` unpair newPairs)
>               ((sy ^. psPaired) `IntMap.union` newPairs)
>               dispos'
>               (tail sy.psTasks)

Pairing algorithm phases ==============================================================================================
      Each of these three functions operates on unpaired set. They are invoked, in sequence, during iterate'.

>     nominal sy                            =
>       IntMap.foldlWithKey (conduce False (sy ^. psUnpaired)) IntMap.empty (fWork ^. (fwPairing . fwSamplePairings))
>     exotic sy                             =
>       IntMap.foldlWithKey
>         (conduce True (sy ^. psUnpaired))
>         IntMap.empty
>         (fWork ^. (fwPairing . fwSamplePairings))
>     linkless sy                           =
>       let
>         (bixenL, bixenR)                  = IntSet.partition isLeft (sy ^. psUnpaired)
>         isLeft bix                        = isLeftPZ $ pzdb IntMap.! bix 
>       in
>         if linklessPairing
>           then inducePairs False bixenL bixenR
>           else IntMap.empty
>
>     mLeft, mRight      :: IntMap IntSet                {- [SampleIndex → [BagIndex]]             -}
>     (mLeft, mRight)                      =
>       IntMap.foldl' (uncurry fFolder) (IntMap.empty, IntMap.empty) pzdb
>       where
>         fFolder mleft mright pz
>           | isLeftPZ pz                  = (putMembers pz mleft, mright)
>           | isRightPZ pz                 = (mleft, putMembers pz mright)
>           | otherwise                    = (mleft, mright)
>         putMembers pz                    =
>           IntMap.insertWith IntSet.union (wordS pz) (IntSet.singleton $ wordB pz)
>
>     conduce            :: Bool                         {- exotic                                 -}
>                           → IntSet                     {- [BagIndex]                             -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]                  -}
>                           → Int                        {- SampleIndex                            -}
>                           → Int                        {- SampleIndex                            -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]                  -}
>     conduce exo unp soFar siFrom siTo    = soFar `IntMap.union` inducePairs exo bsL bsR
>       where
>         bsL, bsR       :: IntSet                       {- [BagIndex]                             -}             
>         bsL                              =
>           unp `IntSet.intersection` fromMaybe IntSet.empty (siFrom `IntMap.lookup` mLeft)
>         bsR                              =
>           unp `IntSet.intersection` fromMaybe IntSet.empty (siTo   `IntMap.lookup` mRight)
>
>     inducePairs        :: Bool                         {- exotic                                 -}
>                           → IntSet                     {- [BagIndex]                             -}
>                           → IntSet                     {- [BagIndex]                             -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]                  -}
>     inducePairs exo bixenL bixenR        = Map.foldlWithKey ((pin . peg) bixenR) IntMap.empty (peg bixenL) 
>       where
>         allowCross                       = exo && crossInstrumentPairing
>         allowParallel                    = exo && parallelPairing
>
>         peg            :: IntSet                       {- [BagIndex]                             -}
>                           → Map PairingSlot IntSet     {- [ps → [BagIndex]]                      -}
>         peg                              = IntSet.foldl' pegBix Map.empty
>           where
>             pegBix m bix                 = 
>               let
>                 pz                       = pzdb IntMap.! bix
>                 iSlot                    = PairingSlot
>                                              (if allowCross || allowParallel then Nothing else Just pz.pzWordI)
>                                              (fromMaybe (0, qMidiWord128 - 1) pz.pzDigest.zdKeyRange)
>                                              (fromMaybe (0, qMidiWord128 - 1) pz.pzDigest.zdVelRange)
>               in
>                 Map.insertWith IntSet.union iSlot (IntSet.singleton bix) m
>
>         pin            :: Map PairingSlot IntSet       {- [ps → [BagIndex]]                      -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]                  -}
>                           → PairingSlot                {- ps                                     -}
>                           → IntSet                     {- [BagIndex]                             -}
>                           → IntMap Int                 {- [BagIndex → BagIndex]                  -}
>         pin pegBoard m iSlot bL          = m `IntMap.union` IntMap.fromList newPairs
>           where
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
>                 areParallel bixL bixR    =    (pzdb IntMap.! bixL).pzWordI
>                                            == (pzdb IntMap.! bixR).pzWordI   

Pairing book-keeping ==================================================================================================
          identify crossers from among all pairs by filtering zone pairs that do not share an instrument owner

>     dirtyZs                              = makeActions pzdb (sy ^. psUnpaired)
>     dirtyIs                              = IntMap.keysSet dirtyZs `IntSet.union` IntMap.keysSet crossers
>     crossers                             = IntMap.foldlWithKey marry IntMap.empty (sy ^. psPaired)
>       where
>         marry m bixL bixR
>           | instL == instR               = m
>           | otherwise                    = include instL bixL (include instR bixR m)
>           where
>             include k v                  = IntMap.insertWith IntSet.union k (IntSet.singleton v)
>
>             (instL, instR)               = (wordI (pzdb IntMap.! bixL), wordI (pzdb IntMap.! bixR))

pairing convenience functions =========================================================================================
          unpair           - cram all Ls and Rs from partner map into single set
          makeActions      - turn input set of bixen into instrument-to-zones "actions list"

> unpair                 :: IntMap Int                   {- [BagIndex → BagIndex]                  -}
>                           → IntSet                     {- [BagIndex]                             -}
> unpair                                   =
>   let
>     consume iset ifrom ito               = (IntSet.insert ito . IntSet.insert ifrom) iset
>   in
>     IntMap.foldlWithKey consume IntSet.empty
>
> makeActions            :: IntMap PreZone               {- [BagIndex → pz]                        -}
>                           → IntSet                     {- [BagIndex]                             -}
>                           → IntMap IntSet              {- [InstIndex → [BagIndex]]               -}
> makeActions pzdb                         =
>   let
>     make actions bix                     = IntMap.insertWith IntSet.union (wordI pz) (IntSet.singleton bix) actions
>                                              where pz = pzdb IntMap.! bix
>   in
>     IntSet.foldl' make IntMap.empty

owners husbandry ======================================================================================================
          makeOwners       - generate owners map from raw PreZones
          repairOwners     - fix owners map after modifying zones

> makeOwners             :: IntMap PreZone               {- [BagIndex → pz]                       -}
>                           → IntMap IntSet              {- [InstIndex → [BagIndex]]              -}
> makeOwners                               = IntMap.foldl' build IntMap.empty
>   where
>     build m pz                           =
>       IntMap.insertWith IntSet.union (wordI pz) (IntSet.singleton (wordB pz)) m
>
> repairOwners           :: IntMap PreZone               {- [BagIndex → pz]                        -}
>                           → IntMap IntSet              {- [InstIndex → [BagIndex]]               -}
>                           → IntSet                     {- [InstIndex]                            -}
>                           → IntMap IntSet              {- [InstIndex → [BagIndex]]               -}
> repairOwners pzdb owners dirtyIs         = invalidated `IntMap.union` makeOwners (IntMap.filter isInteresting pzdb) 
>   where
>     invalidated                          = IntSet.foldl' (flip IntMap.delete) owners dirtyIs
>     isInteresting pz                     = wordI pz `IntSet.member` dirtyIs

vet task ==============================================================================================================
          switch bad stereo zones to mono, or off altogether (modifies pzdb)

> vetTaskIf _ _ fWork                      = (fwPairing . fwDirtyZs .~ IntMap.empty) work
>   where
>     Directives{ .. }
>                                          = fWork ^. fwDirectives                     
>
>     processed          :: Vet            = IntMap.foldl' vetter (spawn fWork defVet) (fWork ^. fwZRecs)
>     work               :: FileWork       = imbibe fWork processed
>
>     vetter             :: Vet → InstZoneRecord → Vet
>     vetter vetIn zrec                    =
>       let
>         zActions                         = zrec.zswInst `IntMap.lookup` (fWork ^. (fwPairing . fwDirtyZs))
>       in
>         maybe vetIn (vetActions vetIn zrec) zActions
>
>     vetActions         :: Vet → InstZoneRecord → IntSet → Vet
>     vetActions vetIn zrec actions        = ((ipzdb .~ pzsOut) . (ipDispo .~ rdOut)) vetIn
>       where
>         fName                            = "vetActions"
>
>         actionFun                        = if switchBadStereoZonesToMono 
>                                              then makeThemMono
>                                              else killThem
>
>         (pzsOut, rdOut)                  =
>           IntSet.foldl' (uncurry actionFun) (vetIn ^. ipzdb, vetIn ^. ipDispo) actions
>
>         makeThemMono, killThem, actionFun
>                        :: IntMap PreZone → ResultDispositions → Int → (IntMap PreZone, ResultDispositions)
>         makeThemMono pzdb rd bix         =
>           let
>             pz                           = pzdb IntMap.! bix
>           in
>             (IntMap.update (Just . makeMono) (wordB pz) pzdb, rd)
>             
>         killThem pzdb rd bix             = 
>           let
>             pz                           = pzdb IntMap.! bix
>             ssKill                       =
>               [Scan Violated BadStereoPartner fName zrec.zswChanges.cnName]
>           in
>             (IntMap.update (const Nothing) (wordB pz) pzdb, dispose (extractZoneKey pz) ssKill rd)

adopt task ============================================================================================================
          mark adoption

> adoptTaskIf _ _ fWork                    = zrecTask adopter fWork
>   where
>     adopter zrec rd                      =
>       case zrec.zswInst `IntMap.lookup` (fWork ^. fwZoneOwners) of
>         Nothing                          → (Nothing,   rd)
>         Just iset                        → (Just zrec, IntSet.foldl' (adopt zrec) rd iset)
>
>     adopt zrec rdFold bix                =
>       let
>         fName                            = "adopt"
>
>         pz                               = (fWork ^. fwPreZones) IntMap.! bix
>         imp                              = if wasSwitchedToMono pz then AdoptedAsMono else Adopted
>         ssAdopt                          =
>           [Scan Modified imp fName zrec.zswChanges.cnName]
>       in
>         dispose (extractSampleKey pz) ssAdopt rdFold

smash task ============================================================================================================
          compute smashups for each instrument
          this is initiated multiple times, and only redoes the zrec's smashup if it currently contains Nothing

> smashTaskIf _ _ fWork                    = zrecTask smasher fWork
>   where
>     smasher zrec rdFold                  =
>       let
>         tag                              = show zrec.zswInst
>
>         bixenOwned                       =
>           fromMaybe IntSet.empty (zrec.zswInst `IntMap.lookup` (fWork ^. fwZoneOwners))
>         bixenCrossing                    =
>           fromMaybe IntSet.empty (zrec.zswInst `IntMap.lookup` (fWork ^. fwZoneCrossing))
>
>         smashVar                         =
>           zrec.zsSmashup
>             <|> Just (computeInstSmashup tag (fWork ^. fwPreZones) (bixenOwned `IntSet.union` bixenCrossing))
>       in
>         (Just zrec{zsSmashup = smashVar}, rdFold)
>
> computeInstSmashup     :: String → IntMap PreZone → IntSet → Smashing Word
> computeInstSmashup tag pzdb bixen
>   | traceIf trace_CIS False              = undefined
>   | otherwise                            =
>   profess
>     (not (IntMap.null pzdb) && not (IntSet.null bixen))
>     (unwords [fName, tag, "no zones"])
>     (smashSubspaces tag [qMidiWord128, qMidiWord128, 2] (IntMap.map extractSpace pzs))
>   where
>     fName                                = "computeInstSmashup"
>     trace_CIS                            = unwords [fName, tag, show (IntMap.keys pzs)]
>
>     pzs                                  = accessPreZones fName pzdb bixen

reorg task ============================================================================================================
          where appropriate, make one instrument out of many

Overview: the member instruments of a qualified group will be absorbed into the lead (member). She takes all of their 
zones, in effect. The mapping (member → lead) :: (Word → Word) is the absorption map.

To build the map
 1. collect all instrument names

 2. group by similar strings

 3. drop the strings, retaining member → lead structure

 4. filter any proposed absorptions via #5 qualify

 5. implement a suitability calculation 

> reorgTaskIf _ _ fWork                    =
>   if not doAbsorption
>     then fWork
>     else ((fwPreZones  .~ rebased)
>           . (fwDirtyIs .~ IntMap.keysSet absorptionMap)) work
>   where
>     Directives{ .. }  
>                                          = fWork ^. fwDirectives
>     closeEnough x y                      = absorbThreshold < howClose (fst x) (fst y)
>     pzdb                                 = fWork ^. fwPreZones
>     work                                 = zrecTask reorger fWork
>
>     reorger zrec rdFold
>       | rprobe                           = (Just zrec,                          dispose pergm scansBlocked rdFold)
>       | isNothing aprobe                 = (Just zrec,                          rdFold)
>       | party == zrec.zswInst            = (Just zrec,                          dispose pergm scansIng rdFold)
>       | otherwise                        = (Nothing,                            dispose pergm scansEd rdFold)
>       where
>         fName                            = "reorger"
>
>         pergm                            = instKey zrec
>
>         rprobe                           = IntSet.member zrec.zswInst dismissed
>         aprobe                           = IntMap.lookup zrec.zswInst absorptionMap
>
>         party                            = deJust "aprobe" aprobe
>
>         ssTempl8 dispo imp clue          = [Scan dispo imp fName clue]
>         scansIng                         = ssTempl8 Modified   Absorbing    noClue
>         scansEd                          = ssTempl8 Dropped    Absorbed     (show party)
>         scansBlocked                     = ssTempl8 NoChange   NoAbsorption noClue
>
>     unvetted, ready, nogood
>                        :: IntMap IntSet                {- [InstIndex → [InstIndex]]              -}
>
>     unvetted                             = foldr (IntMap.union . rewire) IntMap.empty groups
>       where
>         groups                           = filter noSingletons (groupBy closeEnough instNames)
>                                              where noSingletons x = 1 < length x
>         instNames                        = IntMap.foldl' extract [] (fWork ^. fwZRecs)
>           where
>             extract ns zrec              = (zrec.zswChanges.cnName, zrec.zswInst) : ns
>         rewire ns                        =
>           IntMap.insert ((snd . head) ns) (IntSet.fromList (map snd ns)) IntMap.empty
>
>     (ready, nogood)                      = IntMap.partitionWithKey qualify unvetted
>       where
>         qualify        :: Int → IntSet → Bool
>         qualify leadI memberIs
>           | null smashups                = error "null smashups?!?"
>           | 0 == osmashup.smashStats.countMultiples
>                                          = True
>           | membersHaveVR memberIs       = True
>           | otherwise                    = False
>           where
>             smashups                     = map (zrecSmash fWork) (IntSet.toList memberIs)
>             osmashup                     = (foldl' smashSmashings (head smashups) (tail smashups))
>                                              {smashTag = unwords [show (leadI, memberIs)]}
>         -- VR = Velocity Range(s)
>         membersHaveVR iset               = all zonesHaveVR (IntSet.toList iset)
>         zonesHaveVR inst                 = any zoneHasVR (IntSet.toList bixen)
>                                              where bixen = (fWork ^. fwZoneOwners) IntMap.! inst
>         zoneHasVR bix                    = 
>           let
>             pz                           = (fWork ^. fwPreZones) IntMap.! bix
>           in
>             case pz.pzDigest.zdVelRange of
>               Just rng                   → rng /= (0, qMidiWord128 - 1)
>               Nothing                    → False
>
>     dismissed          :: IntSet                       {- [InstIndex]                            -}
>     dismissed                            = IntMap.foldr combine IntSet.empty nogood
>                                              where combine v m = m `IntSet.union` v
>
>     absorptionMap      :: IntMap Int                   {- [InstIndex → InstIndex]                -}
>     absorptionMap                        = IntMap.foldlWithKey fold1Fun IntMap.empty ready
>       where
>         fold1Fun qIn wLead               =
>           let
>             fold2Fun qFold wMember       = IntMap.insert wMember wLead qFold
>           in
>             IntSet.foldl' fold2Fun qIn
>
>     rebased            :: IntMap PreZone               {- [BagIndex → pz]                        -}
>     rebased                              = IntMap.foldlWithKey rebase pzdb pzdb
>       where
>         rebase m k pz                    =
>           let
>             bang leadI                   = IntMap.update change k m
>                                              where change _ = Just pz{pzWordI = fromIntegral leadI}
>           in
>             maybe m bang (IntMap.lookup (wordI pz) absorptionMap)

match task ============================================================================================================
          accumulate all fuzzy matches

> matchTaskIf _ _ fWork                    = (fwMatches .~ Matches sMatches iMatches) fWork
>   where
>     dives                                = fWork ^. fwDirectives
>     procon                               = dives.proConRatio
>     narrow                               = dives.narrowRosterForBoot
>
>     sMatches                             =
>       let
>         computeFF m k v                  = Lazy.insert k (computem v) m
>         computem v                       = computeFFMatches procon v.cnName narrow
>       in
>         Map.foldlWithKey computeFF Lazy.empty (fWork ^. fwPreSamples)
>
>     iMatches                             =
>       let
>         computeFF m zrec                 = Lazy.insert (instKey zrec) (computem zrec) m
>         computem zrec                    = computeFFMatches procon zrec.zswChanges.cnName narrow
>       in
>         IntMap.foldl' computeFF Lazy.empty (fWork ^. fwZRecs)    

shrink task ===========================================================================================================
          Accounts for pairing activity-caused invalidations; when instrument includes unowned zones, below causes
          smashups to be recalculated (in later pass).

> shrinkTaskIf _ _ fWork                   = zrecTask shrinker fWork
>   where
>     shrinker zrec rdFold                 =
>       if zrec.zswInst `IntSet.member` (fWork ^. fwDirtyIs)
>         then (Just zrec{zsSmashup = Nothing}, rdFold)
>         else (Just zrec                     , rdFold)

clean task ============================================================================================================
          1. repair owners
          2. remove zrecs that have gone bad

> cleanTaskIf _ _ fWork                    = (fwDirtyIs .~ IntSet.empty) work
>   where
>     fName                                = "cleanTaskIf"
>     ssNoZones                            = [Scan Dropped NoZones fName noClue]
>
>     owners'                              = repairOwners
>                                              (fWork ^. fwPreZones)
>                                              (fWork ^. fwZoneOwners)
>                                              (fWork ^. fwDirtyIs)
>     work                                 =
>       let      
>         cleaner zrec rdFold              = maybe
>                                              (Nothing, dispose (instKey zrec) ssNoZones rdFold)
>                                              (const (Just zrec, rdFold))
>                                              (zrec.zswInst `IntMap.lookup` owners')
>       in
>         zrecTask cleaner ((fwZoneOwners .~ owners') fWork)

perI task =============================================================================================================
          generating PerInstrument map

> perITaskIf _ _ fWork                     = ((fwPerInstruments .~ perIs) . (fwDispositions .~ rdOut)) fWork
>   where
>     fName                                = "perITaskIf"
>
>     (perIs, rdOut)                       =
>       IntMap.foldl' (uncurry makePerI) (Map.empty, fWork ^. fwDispositions) (fWork ^. fwZRecs)   
>
>     makePerI           :: Map PerGMKey PerInstrument → ResultDispositions 
>                           → InstZoneRecord → (Map PerGMKey PerInstrument, ResultDispositions)
>     makePerI m rdFold zrec               = (Map.insert pergm perI m, dispose pergm ssInstrument rdFold')
>       where
>         pergm          :: PerGMKey       = instKey zrec
>
>         owned                            =
>           fromMaybe IntSet.empty (zrec.zswInst `IntMap.lookup` (fWork ^. fwZoneOwners))
>         crossing                        =
>           fromMaybe IntSet.empty (zrec.zswInst `IntMap.lookup` (fWork ^. fwZoneCrossing))
>         perI                             = 
>           PerInstrument 
>             zrec.zswChanges 
>             owned
>             crossing
>             (deJust fName zrec.zsSmashup)
>
>         ssInstrument                     =
>           [Scan Accepted ToCache fName 
>              (show (IntSet.size owned, IntSet.size crossing))]
>         ssPreZone                        =
>           [Scan Accepted ToCache fName (show zrec.zswInst)]
>
>         rdFold'                          =
>           let
>             blessZone rd bix             = dispose (extractZoneKey pz) ssPreZone rd
>                                              where pz = (fWork ^. fwPreZones) IntMap.! bix
>           in
>             IntSet.foldl' blessZone rdFold (allBixen perI)

Runner boilerplate ====================================================================================================

> class Runner a where
>   spawn                :: FileWork → a → a
>   imbibe               :: FileWork → a → FileWork
>
> instance Runner ZRecRunner where
>   spawn fWork                            =
>       (   iZRecs         .~ (fWork ^. fwZRecs))
>        . (iDispo         .~ (fWork ^. fwDispositions))
>   imbibe fWork iinst                     =
>       (  (fwZRecs        .~ (iinst ^. iZRecs))
>        . (fwDispositions .~ (iinst ^. iDispo)))         fWork
> instance Show ZRecRunner where
>   show inst                              =
>     unwords [  "ZRec"
>              , show (inst ^. iZRecs, inst ^. iDispo) ]
>
> instance Runner Capture where
>   spawn fWork                            =
>          (uZRecs         .~ (fWork ^. fwZRecs))
>        . (uPzs           .~ (fWork ^. fwPreZones))
>        . (uDispo         .~ (fWork ^. fwDispositions))
>   imbibe fWork capt                      =
>       (  (fwZRecs        .~ (capt ^. uZRecs))
>        . (fwPreZones     .~ (capt ^. uPzs))
>        . (fwDispositions .~ (capt ^. uDispo)))          fWork
> instance Show Capture where
>   show capt                              =
>     unwords [  "Capture"
>              , show (IntMap.map pzWordB (capt ^. uPzs))]
>
> instance Runner Vet where
>   spawn fWork                            =
>          (ipzdb          .~ (fWork ^. fwPreZones))
>        . (ipDispo        .~ (fWork ^. fwDispositions))
>   imbibe fWork vet                       =
>       (  (fwPreZones     .~ (vet ^. ipzdb))
>        . (fwDispositions .~ (vet ^. ipDispo)))          fWork
> instance Show Vet where
>   show inst                              =
>     unwords [  "Vet"
>              , show (inst ^. ipzdb), show (inst ^. ipDispo)]

The End
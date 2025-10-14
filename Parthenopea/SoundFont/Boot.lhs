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
> import qualified Data.Bifunctor          as BF
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
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
  
importing sampled sound (from SoundFont (*.sf2) files) ================================================================

> data FileWork                            =
>   FileWork {
>     fwDirectives       :: Directives
>   , fwZRecs            :: [InstZoneRecord]
>   , fwPreSampleCache   :: Map PreSampleKey PreSample
>   , fwInstrumentCache  :: Map PerGMKey PerInstrument
>   , fwMatches          :: Matches
>
>   , fwPairing          :: Pairing
>
>   , fwDispositions     :: ResultDispositions}
> instance Show FileWork where
>   show fw                                =
>     unwords [  "FileWork"
>              , show (length fw.fwZRecs), "=#zrecs"
>              , show (length fw.fwInstrumentCache), "=#cached"
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
>     fwPartners         :: IntMap {- SampleIndex -} Int {- SampleIndex -}
>   , fwPreZones         :: IntMap {- BagIndex -}    PreZone
>   , fwPairings         :: IntMap {- BagIndex -}    Int {- BagIndex -}
>   , fwRejects          :: IntSet {- BagIndex -}
>   , fwActions          :: IntMap {- InstIndex -}   IntSet {- [BagIndex] -}}
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
> reduceFileIterate FileIterate{ .. }                   =
>   (fiFw.fwInstrumentCache, fiFw.fwMatches, fiFw.fwDispositions)
>
> preSampleTaskIf, smellTaskIf, surveyTaskIf, captureTaskIf, flatMapTaskIf, pairTaskIf, vetTaskIf
>                , adoptTaskIf, smashTaskIf, reorgTaskIf, matchTaskIf, catTaskIf, perITaskIf
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
>      , ("flatMap",    flatMap)
>      , ("pair",       pair)
>      , ("vet",        vet)
>      , ("adopt",      adopt)
>      , ("smash",      smash)
>      , ("reorg",      reorg) 
>      , ("match",      match)
>      , ("cat",        cat)
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
>     cat                                  = catTaskIf          sffile rost
>     perI                                 = perITaskIf         sffile rost

executive =============================================================================================================

To support extracting from flawed SoundFont files, we - up front - withdraw unrecoverable items from their
respective collections. An item's presence may be critical to some instrumentation. So it entails further deletion
and recovery.

> surveyInstruments      :: Directives
>                           → ([InstrumentName], [PercussionSound])
>                           → VB.Vector SFFileBoot
>                           → IO (Map PerGMKey PerInstrument, Matches, ResultDispositions)
> surveyInstruments dives rost vFilesBoot  = do
>   putStr $ reapEmissions
>     [   EndOfLine
>       , Unblocked fName, EndOfLine
>       , Blanks 2, Unblocked $ show $ fst rost, EndOfLine
>       , Blanks 2, Unblocked $ show $ snd rost, EndOfLine, EndOfLine]
>   return $ foldl' bootFolder (Map.empty, defMatches, virginrd) vFilesBoot
>   where
>     fName                                = "surveyInstruments"
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

> preSampleTaskIf sffile _ fwIn            =
>   foldl' sampleFolder fwIn (formComprehension sffile ssShdrs)
>   where
>     sampleFolder fwForm presk            =
>       let
>         fName                            = "sampleFolder"
>
>         preSampleCache                   =
>           if dead ss
>             then fwForm.fwPreSampleCache
>             else Map.insert presk ps fwForm.fwPreSampleCache
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
>         ss_
>           | not (goodSampleRate shdr.sampleRate)
>                                          = violated BadSampleRate (show shdr.sampleRate)
>           | isNothing mtype              = violated BadSampleType (show shdr.sampleType)
>           | not (sampleSizeOk (shdr.start, shdr.end))
>                                          = violated BadSampleLimits (show (shdr.start, shdr.end))
>           | not (goodName raw)           = badButMaybeFix fwIn.fwDirectives.fixBadNames CorruptName fName raw good
>           | otherwise                    = accepted Ok stereo
>
>         (ss, changes, name)              = if wasRescued CorruptName ss_
>                                              then (ss_, singleton FixCorruptName, good)
>                                              else (ss_, [],                       raw)
>       in
>         fwForm{ fwPreSampleCache = preSampleCache, fwDispositions = dispose presk ss fwForm.fwDispositions}

smell task ============================================================================================================
          partner map at sample header level = driver for stereo pairings

> smellTaskIf _ _ fWork                    = fWork{fwPairing = pairing}
>   where
>     pairing                              = 
>       fWork.fwPairing{fwPartners = Map.foldlWithKey smellFolder IntMap.empty allLeft}
>     allLeft                              = Map.filter isLeftPS fWork.fwPreSampleCache
>
>     smellFolder m presk pres             =
>       let
>         siIn, siOut    :: Int
>         siIn                             = fromIntegral presk.pskwSampleIndex
>         siOut                            = fromIntegral (effPSShdr pres).sampleLink
>
>         mback_                           =
>           presk{pskwSampleIndex = fromIntegral siOut} `Map.lookup` fWork.fwPreSampleCache
>         mback                            = mback_ >>= backOk
>
>         backOk opres                     =
>           if isRightPS opres && fromIntegral (effPSShdr opres).sampleLink == siIn
>             then Just siOut
>             else Nothing
>       in
>         case mback of
>           Nothing                        → m
>           Just x                         → IntMap.insert siIn x m

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
>   , zsInstCat          :: Maybe InstCat
>   , zsPreZones         :: [PreZone]}
> instance Show InstZoneRecord where
>   show zrec                              =
>     unwords ["InstZoneRecord", show (zrec.zswFile, zrec.zswInst)
>                              , showMaybeInstCat zrec.zsInstCat, show $ length zrec.zsPreZones]
> makeZRec               :: PerGMKey → ChangeName F.Inst → InstZoneRecord
> makeZRec pergm changes                          =
>   InstZoneRecord 
>     pergm.pgkwFile 
>     pergm.pgkwInst
>     changes Nothing Nothing []
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

> surveyTaskIf sffile _ fwIn               = fwIn{fwZRecs = zrecs, fwDispositions = rd'}
>   where
>     fName                                = "surveyTaskIf"
>
>     (zrecs, rd')                         =
>       foldl' preIFolder ([], fwIn.fwDispositions) (formComprehension sffile ssInsts)
>     
>     violated, accepted :: Impact → String → [Scan]
>     violated impact clue             =
>       [Scan Violated impact fName clue]
>     accepted impact clue             =
>       [Scan Accepted impact fName clue]
>
>     preIFolder         :: ([InstZoneRecord], ResultDispositions) → PerGMKey → ([InstZoneRecord], ResultDispositions)
>     preIFolder (zrecsFold, rdFold) pergm     =
>       if iinst.instBagNdx <= jinst.instBagNdx
>         then (zrecs', rd'')
>         else error $ unwords [fName, "corrupt instBagNdx"]
>       where
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pergm.pgkwInst + 1}
>
>         raw                              = iinst.instName
>         good                             = fixName raw
>
>         zrecs'                           =
>           if dead ss
>              then zrecsFold 
>              else makeZRec pergm (ChangeName iinst changes finalName) : zrecsFold
>
>         ss
>           | iinst.instBagNdx == jinst.instBagNdx
>                                          = violated NoZones (show iinst.instName)
>           | not (goodName raw)           = badButMaybeFix fwIn.fwDirectives.fixBadNames CorruptName fName raw good
>           | otherwise                    = accepted Ok (show pergm.pgkwInst)
>
>         changes                          = if wasRescued CorruptName ss then singleton FixCorruptName else []
>         finalName                        = if wasRescued CorruptName ss then good else raw
>
>         rd''                             = dispose pergm ss rdFold   
>
>     loadInst           :: PerGMKey → F.Inst
>     loadInst pergm                       = sffile.zFileArrays.ssInsts ! pergm.pgkwInst

capture task ==========================================================================================================
          populate zrecs with PreZones

> captureTaskIf sffile _ fwIn              = zrecTask capturer fwIn
>   where
>     fName                                = "captureTaskIf"
>
>     violated, modified :: Impact → String → [Scan]
>     violated impact clue                 =
>       [Scan Violated impact fName clue]
>     modified impact clue             =
>       [Scan Modified impact fName clue]
>
>     capturer zrecIn rdIn                 =
>       let
>         (newPzs, rdOut)                  = captureZones zrecIn rdIn
>       in
>         (zrecIn{zsPreZones = newPzs}, rdOut)
>
>     captureZones       :: InstZoneRecord → ResultDispositions → ([PreZone], ResultDispositions)
>     captureZones zrec rdCap              = (pzs, dispose pergm ss rdCap)
>       where
>         pergm                            = instKey zrec
>         iName                            = zrec.zswChanges.cnName
>
>         results                          = map captureZone (deriveRange ibagi jbagi)
>           where
>             iinsts                       = sffile.zFileArrays.ssInsts
>             ibagi                        = F.instBagNdx (iinsts ! pgkwInst pergm)
>             jbagi                        = F.instBagNdx (iinsts ! (pgkwInst pergm + 1))
>
>         captureZone    :: Word → (Word, Either PreZone (Disposition, Impact))
>         captureZone bix
>           | isNothing pz.pzDigest.zdSampleIndex
>                                          = (bix, Right (Accepted, GlobalZone))
>           | isNothing mpres              = (bix, Right (Dropped, OrphanedBySample))
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
>
>             pz                           = makePreZone sffile.zWordFBoot si (pgkwInst pergm) bix gens pres.cnSource
>             si                           = deJust (unwords [fName, "si"]) pz.pzDigest.zdSampleIndex
>             presk                        = PreSampleKey sffile.zWordFBoot si
>             mpres                        = presk `Map.lookup` fwIn.fwPreSampleCache
>             pres                         = deJust (unwords [fName, "pres"]) mpres
>
>         pzs                              = fst $ foldl' consume ([], defZone) results
>           where
>             consume (spzs, foldZone) (bagIndex, eor)
>                                          =
>               let
>                 handleNormal pz          =
>                   (spzs ++ [pz{pzSFZone = buildZone sffile foldZone (Just pz) bagIndex}], foldZone)
>                 handleError (_, imp)     =
>                   (spzs, if imp == GlobalZone then buildZone sffile defZone Nothing bagIndex else foldZone)
>               in
>                 either handleNormal handleError eor
>
>         noZones, illegalRange, hasRoms, illegalLimits, yesCapture
>                        :: Maybe [Scan] 
>         ss = deJust fName
>              $ noZones `CM.mplus` illegalRange
>                        `CM.mplus` hasRoms
>                        `CM.mplus` illegalLimits
>                        `CM.mplus` yesCapture
>         noZones
>           | null pzs                     = Just $ violated NoZones noClue
>           | otherwise                    = Nothing
>         illegalRange
>           | isJust mpz                   = Just $ violated CorruptGMRange clue
>           | otherwise                    = Nothing
>           where
>             mpz                          = find zoneBad pzs
>
>             zoneBad pz                   = not (okGMRanges pz.pzDigest)
>             clue                         = showBad $ fromJust mpz
>         hasRoms
>           | isJust mpz                   = Just $ violated RomBased clue
>           | otherwise                    = Nothing
>           where
>             mpz                          = find zoneRom pzs
>
>             stype pz                     = F.sampleType (effPZShdr pz)
>             zoneRom pz                   = stype pz >= 0x8000
>             pzBad                        = fromJust mpz
>             clue                         = showHex (stype pzBad) []
>         illegalLimits
>           | isJust result                = Just $ violated BadSampleLimits $ fromJust result
>           | otherwise                    = Nothing
>           where
>             tested                       = map illegalSampleSize pzs
>             result                       = foldr CM.mplus Nothing tested
>         yesCapture                       = Just $ modified Captured iName

flatMap task ==========================================================================================================
          erect temporary infrastructure for stereo pairings

> flatMapTaskIf _ _ fWork                  = fWork{fwPairing = fWork.fwPairing{fwPreZones = preZones}}
>   where
>     preZones                             = zrecCompute fWork aggregate IntMap.empty
>     aggregate soFar zrec                 =
>       soFar `IntMap.union` foldl' assign IntMap.empty (filter isStereoPZ zrec.zsPreZones)
>     assign m pz                          = IntMap.insert (wordB pz) pz m

pair task =============================================================================================================
          produce BagIndex list identifying PreZones switching to mono

> pairTaskIf _ _ fWork                     =
>   fWork{fwPairing = fWork.fwPairing{fwPairings = pairings, fwRejects = rejects, fwActions = actions}}
>   where
>     Pairing{ .. }                        
>                                          = fWork.fwPairing
>
>     paired                               = unpair pairings
>     rejects                              = IntMap.keysSet fwPreZones `IntSet.difference` paired
>
>     actions                              = IntSet.foldl' register IntMap.empty rejects
>       where
>         register acts iBag               =
>           let
>             pz                           = fwPreZones IntMap.! iBag
>           in
>             IntMap.insertWith IntSet.union (wordI pz) (IntSet.singleton iBag) acts
>
>     pairings                             = IntMap.foldlWithKey pairingFolder IntMap.empty fwPartners
>       where
>         pairingFolder soFar siFrom siTo  =
>           let
>             bagsL                        = fromMaybe IntSet.empty (siFrom `IntMap.lookup` mLeft)
>             bagsR                        = fromMaybe IntSet.empty (siTo   `IntMap.lookup` mRight)
>           in
>             soFar `IntMap.union` vetPairs bagsL bagsR
>
>     (mLeft, mRight)                      =
>       let
>         flatMapFolder (mleft, mright) pz
>           | isLeftPZ pz                  = (computeMembers pz mleft, mright)
>           | isRightPZ pz                 = (mleft, computeMembers pz mright)
>           | otherwise                    = error "should already have filtered out mono case"
>         computeMembers pz                =
>           IntMap.insertWith IntSet.union (wordS pz) (IntSet.singleton $ wordB pz)
>       in
>         IntMap.foldl' flatMapFolder (IntMap.empty, IntMap.empty) fwPreZones
>
>     vetPairs bagsL bagsR                 =
>       let
>         makePairs ignoreI lb rb          = Map.foldlWithKey (pairThem ignoreI rSurvey) IntMap.empty lSurvey
>           where  
>             lSurvey                      = survey ignoreI lb
>             rSurvey                      = survey ignoreI rb
>         regularPairs                     = makePairs False bagsL bagsR
>         pairedSoFar                      = unpair regularPairs
>         bagsL'                           = bagsL `IntSet.difference` pairedSoFar
>         bagsR'                           = bagsR `IntSet.difference` pairedSoFar
>       in
>         if fWork.fwDirectives.crossInstrumentPairing
>           then regularPairs `IntMap.union` makePairs True bagsL' bagsR'
>           else regularPairs
>
>     pairThem           :: Bool → Map PairingSlot IntSet → IntMap Int → PairingSlot → IntSet → IntMap Int
>     pairThem ignoreI osurv m iSlot bagsL =
>       let
>         bagsR                            = Map.lookup iSlot osurv
>         newPairs_                        = zip (IntSet.toList bagsL) (IntSet.toList (fromMaybe IntSet.empty bagsR))
>         newPairs                         = if not ignoreI && not fWork.fwDirectives.parallelPairing
>                                              then take 1 newPairs_
>                                              else newPairs_
>       in
>         m `IntMap.union` IntMap.fromList newPairs
> 
>     survey             :: Bool → IntSet {- [BagIndex] -} → Map PairingSlot IntSet {- [BagIndex] -}
>     survey ignoreI bags                         =
>       let
>         makeSurvey m bag                   =
>           Map.insertWith IntSet.union slot (IntSet.singleton bag) m
>           where
>             pz                           = fwPreZones IntMap.! bag
>             slot                         =
>               (if ignoreI then Nothing else Just pz.pzWordI
>              , fromMaybe (0, 127) pz.pzDigest.zdKeyRange
>              , fromMaybe (0, 127) pz.pzDigest.zdVelRange)
>       in
>         IntSet.foldl' makeSurvey Map.empty bags
>
> unpair                 :: IntMap Int → IntSet
> unpair                                   = IntMap.foldlWithKey ifolder IntSet.empty
>   where
>     ifolder iset ifrom ito               = (IntSet.insert ito . IntSet.insert ifrom) iset

vet task ==============================================================================================================
          execute: switch bad stereo zones to mono, or off altogether

> vetTaskIf _ _ fWork                      = zrecTask vetter fWork
>   where
>     Pairing{ .. }                      
>                                          = fWork.fwPairing
>
>     vetter zrec rd                       =
>       let
>         mactions                         = fromIntegral zrec.zswInst `IntMap.lookup` fwActions
>       in
>         case mactions of
>            Nothing                       → (zrec, rd)
>            Just actions                  → vetActions zrec rd actions
>
>     vetActions zrec rdIn actions         = (zrec{zsPreZones = pzsOut}, rdOut)
>       where
>         fName                            = "vetActions"
>
>         (pzsOut, rdOut)                  =
>           mapAction $ if fWork.fwDirectives.switchBadStereoZonesToMono 
>                         then makeThemMono
>                         else killThem        
>             
>         makeThemMono pz rd               = (Just $ makeMono pz, rd)
>             
>         killThem pz rd                   = (Nothing, dispose (extractZoneKey pz) scan rd)
>           where
>             scan                         =
>               [Scan Violated BadStereoPartner fName zrec.zswChanges.cnName]
>
>         bothPartners                     =
>           fwPairings `IntMap.union` IntMap.foldlWithKey reverser IntMap.empty fwPairings
>           where
>             reverser pds iLeft iRight    = IntMap.insert iRight iLeft pds
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
>         pairFun pz rd                    =
>           let
>             mpartner                     = wordB pz `IntMap.lookup` bothPartners
>           in
>             profess
>               (isJust mpartner)
>               (unwords [fName, "paired missing", show (wordB pz)])
>               (Just pz{pzPartner = fmap fromIntegral mpartner}, rd)

adopt task ============================================================================================================
          mark adoption

> adoptTaskIf _ _ fWork                    = fWork'{fwPairing = defPairing}
>   where
>     fWork'                               =
>       let
>         adopter zrec rd                  = (zrec, foldl' (adopt zrec) rd zrec.zsPreZones)
>       in
>         zrecTask adopter fWork
>
>     adopt zrec rdFold pz                 = 
>       let
>         fName                            = "adopter"
>
>         impact                           = if wasSwitchedToMono pz then AdoptedAsMono else Adopted
>         ssImpact                         =
>           [Scan Modified impact fName zrec.zswChanges.cnName]
>       in
>         dispose (extractSampleKey pz) ssImpact rdFold

smash task ============================================================================================================
          compute smashups for each instrument

> smashTaskIf _ _                          = zrecTask smasher
>   where
>     smasher zrec rdFold                  =
>       (zrec{zsSmashup = Just (computeInstSmashup (show (instKey zrec).pgkwInst) zrec.zsPreZones)}, rdFold)
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

> reorgTaskIf _ _ fwIn                     = zrecTask reorger fwIn
>   where
>     reorger zrec rdFold
>       | not fwIn.fwDirectives.doAbsorption
>                                          = (zrec,                       rdFold)
>       | isJust dprobe                    = (zrec,                       dispose pergm scansBlocked rdFold)
>       | isNothing aprobe                 = (zrec,                       rdFold)
>       | party == zrec.zswInst            = (zrec{zsPreZones = hpzs, zsSmashup = Just hsmash},
>                                                                         dispose pergm scansIng rdFold)
>       | otherwise                        = (zrec{zsPreZones = []},      dispose pergm scansEd rdFold)
>       where
>         fName                            = "reorger"
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
>     instNames          :: Map String Word
>     instNames                            =
>       let
>         extractFolder ns zrec            = Map.insert zrec.zswChanges.cnName zrec.zswInst ns
>       in
>         zrecCompute fwIn extractFolder Map.empty
>     
>     townersMap         :: Map Word ([PreZone], Smashing Word)
>     townersMap                           =
>       let
>         tFolder m zrec                   =
>           Map.insert zrec.zswInst (zrec.zsPreZones, fromJust zrec.zsSmashup) m
>       in
>         zrecCompute fwIn tFolder Map.empty 
>     
>     grouped                              = groupBy grouper (Map.toList instNames)
>                                              where grouper x y = absorbRatio < howClose (fst x) (fst y)
>     filteredByGroupSize                  = filter (\x → 1 < length x) grouped
>     stringsDropped                       = map (map snd) filteredByGroupSize
>
>     headed, ready      :: Map Word [Word]
>     headed                               = Map.fromList $ map (\q → (head q, q)) stringsDropped
>     ready                                = Map.mapWithKey (\k _ → headed Map.! k) hMap
>
>     hMap               :: Map Word ([PreZone], Smashing Word)
>     dMap               :: Map Word SmashStats
>     (hMap, dMap)                         = Map.mapEitherWithKey qualify headed
>       where
>         qualify        :: Word → [Word] → Either ([PreZone], Smashing Word) SmashStats
>         qualify leadI memberIs
>           | 0 == osmashup.smashStats.countMultiples
>                                          = Left (rebased, osmashup)
>           | membersHaveVelocityRanges    = Left (rebased, osmashup)
>           | otherwise                    = Right osmashup.smashStats
>           where
>             towners                      = map (townersMap Map.!) memberIs
>
>             combined                     = concatMap fst towners
>             rebased                      = map rebase combined
>                                              where rebase pz = pz{pzWordI = leadI}
>
>             smashups                     = map snd towners
>             osmashup                     = (foldl' smashSmashings (head smashups) (tail smashups))
>                                              {smashTag = unwords [show leadI, show memberIs]}
>
>             membersHaveVelocityRanges    = all (zonesHaveVelocityRange . fst) towners
>             zonesHaveVelocityRange pzs   = isJust $ find zoneHasVelocityRange pzs   
>             zoneHasVelocityRange pz      =
>               case pz.pzDigest.zdVelRange of
>                 Just rng                 → rng /= (0, 127)
>                 Nothing                  → False
>
>     aMap               :: Map Word Word
>     aMap                                 = Map.foldlWithKey fold1Fun Map.empty ready
>       where
>         fold1Fun       :: Map Word Word → Word → [Word] → Map Word Word
>         fold1Fun qIn wLead               = foldl' fold2Fun qIn
>           where
>             fold2Fun   :: Map Word Word → Word → Map Word Word
>             fold2Fun qFold wMember       = Map.insert wMember wLead qFold

match task ============================================================================================================
          track all fuzzy matches

> matchTaskIf _ _ fwIn                     = fwIn{fwMatches = Matches sMatches iMatches}
>   where
>     narrow                               = fwIn.fwDirectives.narrowInstrumentScope
>
>     sMatches                             =
>       Map.foldlWithKey (\m k v → Map.insert k (computeFFMatches v.cnName narrow) m)
>         Map.empty fwIn.fwPreSampleCache
>     iMatches                             =
>       let
>         computeFF      :: Map PerGMKey FFMatches → InstZoneRecord → Map PerGMKey FFMatches 
>         computeFF m zrec                 = Map.insert (instKey zrec) (computeFFMatches zrec.zswChanges.cnName narrow) m
>       in
>         zrecCompute fwIn computeFF Map.empty 

categorization task ===================================================================================================
          assign each instrument to one of the three categories
          a. Just InstCatInst              an inst bearing one inst, or
          b. Just InstCatPerc              an inst bearing one or more percs, or
          c. Just InstCatDisq              an inst disqualified from tournaments, or
          d. Nothing                       undecided

> catTaskIf _ rost fwIn                    = zrecTask catter fwIn
>   where
>     catter zrec rdFold                   = (zrec{zsInstCat = icat}, dispose (instKey zrec) ss rdFold)
>       where
>         (icat, ss)                       = categorizeInst zrec
>
>     narrow                               = fwIn.fwDirectives.narrowInstrumentScope
>
>     categorizeInst     :: InstZoneRecord → (Maybe InstCat, [Scan])
>     categorizeInst zrec
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = (icat', ss')
>       where
>         fName                            = "categorizeInst"
>         trace_CI                         = unwords [fName, iName, show icat']
>
>         modified, dropped
>                        :: Impact → String → [Scan]
>         modified impact clue             =
>           [Scan Modified impact fName clue]
>         dropped impact clue              =
>           [Scan Dropped impact fName clue]
>
>         pergm                            = instKey zrec
>         pzs                              = zrec.zsPreZones
>         iName                            = zrec.zswChanges.cnName

      Categorization based on Instrument's "provideAlts" results for:
      1. all kinds
      2. "rost" subset, could be same as 1.

>         icatAllKinds, icatRost, icatNarrow, icat'
>                        :: Maybe InstCat
>         icatAllKinds                     = foldr CM.mplus Nothing (provideAlts Nothing allKinds)
>         icatRost                         = if rost == allKinds
>                                              then icatAllKinds
>                                              else foldr CM.mplus Nothing (provideAlts icatAllKinds rost)
>         icatNarrow                       = Just (InstCatDisq Narrow noClue)
>         (icat', ss')                     =
>           case (icatAllKinds, icatRost) of
>             (Just InstCatInst       , Just InstCatInst)
>                                          → (icatRost, modified CatIsInst noClue)
>             (Just (InstCatPerc _)   , Just (InstCatPerc _))
>                                          → (icatRost, modified CatIsPerc noClue)
>             (Just (InstCatDisq imp why), _)
>                                          → (icatAllKinds, dropped imp why)
>             (_                      , Just (InstCatDisq imp why))
>                                          → (icatRost, dropped imp why)
>             _                            → (icatNarrow, dropped Narrow noClue)
>
>         provideAlts    :: Maybe InstCat → ([InstrumentName], [PercussionSound]) → [Maybe InstCat]
>         provideAlts seed rostAlts        =
>           let
>             iMatches                     = fromJust $ Map.lookup pergm fwIn.fwMatches.mIMatches
>             ffInst'                      =
>               Map.filterWithKey (\k v → k `elem` select rostAlts narrow && isPossible' v) iMatches.ffInst
>             ffPerc'                      =
>               Map.filterWithKey (\k v → k `elem` select rostAlts narrow && isPossible' v) iMatches.ffPerc
>           in
>             [ 
>                 maybeSettle isConfirmed catInst                  ffInst'
>               , maybeSettle isConfirmed (catPerc wZones)         ffPerc'
>               , maybeSettle stands      catInst                  ffInst'
>               , maybeNailAsPerc 0.6 
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
>                 myLaden              = howLaden pzsLeft
>                 pzsLeft              = filter canBePercZ pzs
>               in if (myLaden > 0.4) && fr < uFrac
>                 then
>                   (if not (null wZones)
>                      then Just (catPerc wZones)
>                      else Just (catDisq NoPercZones (show (uFrac, wFrac))))
>                 else Nothing
>
>             maybeSettle
>                        :: (Foldable t, Show (t Fuzz)) ⇒ Fuzz → InstCat → t Fuzz → Maybe InstCat
>             maybeSettle thresh ic keys   = find (> thresh) keys >> Just ic
>
>             canBePercZ :: PreZone → Bool
>             canBePercZ x                 =
>               maybe True (\y → isJust $ intersectRanges [y, percPitchRange]) x.pzDigest.zdKeyRange
>      
>             howLaden   :: ∀ a . [a] → Double
>             howLaden ws
>               | null pzs                 = 0
>               | otherwise                = (fromIntegral . length) ws / (fromIntegral . length) pzs
>
>             catInst      :: InstCat      =
>               if null pzs
>                 then InstCatDisq NoZones noClue
>                 else InstCatInst
>
>             catPerc      :: [Word] → InstCat
>             catPerc ws                   =
>               if null pzs || null ws
>                 then InstCatDisq NoZones fNameCatPerc
>                 else InstCatPerc ws
>               where
>                 fNameCatPerc             = "catPerc"
>
>             catDisq    :: Impact → String → InstCat
>             catDisq                      = InstCatDisq
>
>         qualPercZone   :: ([InstrumentName], [PercussionSound]) → PreZone → Maybe Word
>         qualPercZone rost' prez          =
>           mrange >>= pinnedKR (select rost' narrow) >> Just prez.pzWordB
>           where
>             mrange                       = prez.pzDigest.zdKeyRange >>= (Just . BF.bimap fromIntegral fromIntegral)

build zone task =======================================================================================================
          generate the PerInstrument map (aka zone cache)

> perITaskIf _ _ fWork                     = fWork{  fwInstrumentCache =    fst formInstrumentCache
>                                                  , fwDispositions =       snd formInstrumentCache}
>   where
>     formInstrumentCache
>                        :: (Map PerGMKey PerInstrument, ResultDispositions)
>     formInstrumentCache                  = zrecCompute fWork zcFolder (Map.empty, fWork.fwDispositions)
>
>     zcFolder (zc, rdFold) zrec
>       | traceIf trace_ZCF False          = undefined
>       | otherwise                        =
>         (   Map.insert pergm perI zc
>           , dispose pergm [Scan Accepted ToCache fName (show icat)] rdz)
>       where
>         fName                            = "zcFolder"
>         trace_ZCF                        = unwords [fName, show zrec, show pergm, show zrec.zsInstCat]
>
>         pergm                            = instKey zrec
>         icat                             = fromJust zrec.zsInstCat
>         smashup                          = fromJust zrec.zsSmashup
>         perI                             = computePerInst zrec icat smashup
>         pzs                              = perI.pZones
>         (_, rdz)                         = zoneTask (const True) blessZone pzs rdFold
>         blessZone      :: PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions)
>         blessZone pz rdIn                =
>           (   Just pz
>             , dispose (extractZoneKey pz) [Scan Accepted ToCache fName (show pz.pzWordB)] rdIn)
>
>     computePerInst     :: InstZoneRecord → InstCat → Smashing Word → PerInstrument
>     computePerInst zrec icat smashup     =
>         PerInstrument 
>          zrec.zswChanges 
>          pzs 
>          icat
>          smashup
>       where
>         bixen          :: [Word]
>         bixen                            =
>           case icat of
>             InstCatPerc x                → x
>             InstCatInst                  → map pzWordB zrec.zsPreZones
>             _                            → error $ unwords ["only Inst and Perc are valid here"]
>         pzs                              = filter (\pz → pz.pzWordB `elem` bixen) zrec.zsPreZones
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
> sampleSizeOk           :: (Word, Word) → Bool
> sampleSizeOk (stS, enS)                  = stS >= 0 && enS - stS >= 0 && enS - stS < 2 ^ (22::Word)
>
> illegalSampleSize      :: PreZone → Maybe String
> illegalSampleSize pz                     =
>   if ok
>     then Nothing
>     else Just $ unwords [showHex stA [], showHex enA [], showHex stL [], showHex enL [], show zd.zdSampleMode]
>   where
>     shdr                                 = effPZShdr pz
>     zd                                   = pz.pzDigest
>
>     stA                                  = shdr.start     + fromIntegral zd.zdStart
>     enA                                  = shdr.end       + fromIntegral zd.zdEnd
>     stL                                  = shdr.startLoop + fromIntegral zd.zdStartLoop
>     enL                                  = shdr.endLoop   + fromIntegral zd.zdEndLoop
>
>     ok                                   =
>       0 <= stA && stA <= enA && 0 <= stL && stL <= enL
>       && enA - stA < 2 ^ (22::Word)
>       && (zd.zdSampleMode == Just A.NoLoop || enL - stL < 2 ^ (22::Word))
>
> goodSampleRate         :: Word → Bool
> goodSampleRate x                         = x == clip (n64, n2 ^ n20) x
>   where
>     n64, n2, n20       :: Word
>     n64                                  = 64
>     n2                                   = 2
>     n20                                  = 20

The End
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
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
> import Data.Either ( lefts )
> import Data.Foldable
> import Data.List hiding (insert)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Debug.Trace ( traceIO )
> import Euterpea.Music
> import Numeric ( showHex )
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
  
importing sampled sound (from SoundFont (*.sf2) files) ================================================================

> data FileWork =
>   FileWork {
>     fwBoot             :: SFBoot
>   , fwZRecs            :: [InstZoneRecord]
>   , fwMatches          :: Matches
>   , fwDispositions     :: ResultDispositions}
> instance Show FileWork where
>   show fw                                =
>     unwords [  "FileWork"
>              , show fw.fwBoot
>              , show (length fw.fwZRecs), "=zrecs"
>              , show fw.fwDispositions]
> defFileWork            :: FileWork
> defFileWork                              =
>   FileWork dasBoot [] defMatches virginrd
> data FileIterate =
>   FileIterate {
>     fiFw               :: FileWork
>   , fiTaskIfs          :: [(String, FileWork → FileWork)]}
> instance Show FileIterate where
>   show fi                                =
>     unwords ["FileIterate", show fi.fiFw]
>
> preSampleTaskIf, preInstTaskIf, surveyTaskIf, captureTaskIf, markTaskIf, smashTaskIf, reorgTaskIf
>                , shaveTaskIf, matchTaskIf, catTaskIf, perITaskIf
>                        :: SFFile → ([InstrumentName], [PercussionSound]) → FileWork → FileWork
>
> makeFileIterate        :: SFFile → ([InstrumentName], [PercussionSound]) → FileIterate
> makeFileIterate sffile rost              =
>   FileIterate
>     defFileWork 
>     [  ("preSample",  preSample)
>      , ("preInst",    preInst)
>      , ("capture",    mark . capture . survey)
>      , ("smash",      smash)
>      , ("reorg",      reorg) 
>      , ("shave1",     shave)
>      , ("match",      match)
>      , ("cat",        cat)
>      , ("shave2",     shave)
>      , ("perI",       perI)]
>   where
>     mark                                 = markTaskIf         sffile rost
>
>     preSample                            = preSampleTaskIf    sffile rost
>     preInst                              = preInstTaskIf      sffile rost
>     survey                               = surveyTaskIf       sffile rost
>     capture                              = captureTaskIf      sffile rost
>     smash                                = smashTaskIf        sffile rost
>     reorg                                = reorgTaskIf        sffile rost
>     shave                                = shaveTaskIf        sffile rost
>     match                                = matchTaskIf        sffile rost
>     cat                                  = catTaskIf          sffile rost
>     perI                                 = perITaskIf         sffile rost
>
> reduceFileIterate      :: FileIterate → (SFBoot, Matches, ResultDispositions)
> reduceFileIterate fiIn                   = (fiIn.fiFw.fwBoot, fiIn.fiFw.fwMatches, fiIn.fiFw.fwDispositions)

executive =============================================================================================================

To support extracting from flawed SoundFont files, we - up front - withdraw unrecoverable items from their
respective collections. An item's presence may be critical to some instrumentation. So it entails further deletion
and recovery.

> surveyInstruments      :: Array Word SFFile
>                           → ([InstrumentName], [PercussionSound])
>                           → IO (SFRuntime, Matches, ResultDispositions)
> surveyInstruments vFiles rost            = do
>   putStrLn ""
>   putStrLn $ unwords [fName, "rost", show rost]
>   putStrLn ""
>
>   -- compute lazy caches (Maps); coded in "eager" manner, so _looks_ scary, performance-wise
>   let (bootAll, matchesAll, rdAll) = foldl' bootFolder (dasBoot, defMatches, virginrd) vFiles
>   CM.when diagnosticsEnabled (traceIO $ show bootAll)
>   let runt                           = SFRuntime vFiles bootAll seedWinningRecord
>   return (runt, matchesAll, rdAll)
>   where
>     fName                                = "equipInstruments"
>
>     bootFolder (bootIn, matchesIn, rdIn) sffile
>                                          = 
>       let
>         (bootOut, matchesOut, rdOut)     = reduceFileIterate (ingestFile sffile)     
>       in
>         (combineBoot bootIn bootOut, combineMatches matchesIn matchesOut, combinerd rdIn rdOut)
>
>     ingestFile sffile                    =
>       let
>         unfinished fiIn                  = not (null fiIn.fiTaskIfs)
>         nextGen fiIn                     = fiIn{ fiFw = userFun fiIn.fiFw, fiTaskIfs = tail fiIn.fiTaskIfs}
>           where
>             (_, userFun)                 = head fiIn.fiTaskIfs
>       in
>         head $ dropWhile unfinished (iterate nextGen (makeFileIterate sffile rost))

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
>     sampleFolder fwForm presk            =
>       let
>         fName                            = "sampleFolder"
>
>         fwBoot'                          =
>           if dead ss
>             then fwForm.fwBoot
>             else fwForm.fwBoot{zPreSampleCache = Map.insert presk ps fwForm.fwBoot.zPreSampleCache}
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
>       in
>         fwForm{ fwBoot = fwBoot', fwDispositions = dispose presk ss fwForm.fwDispositions}

pre-instance task =====================================================================================================
          access and critique all Instrument "records" in the file 

> preInstTaskIf sffile _ fwIn
>                                          =
>   fwIn{  fwBoot = fwIn.fwBoot{zPreInstCache = preInstCache}
>        , fwDispositions = rdFinal}
>   where
>     pergms                               = formComprehension sffile ssInsts
>     (preInstCache, rdFinal)              = foldl' preIFolder (Map.empty, fwIn.fwDispositions) pergms
>
>     preIFolder (m, rdFold) pergm
>                                          =
>       if iinst.instBagNdx <= jinst.instBagNdx
>         then (m', rd'')
>         else error $ unwords [fName, "corrupt instBagNdx"]
>       where
>         fName                            = "preIFolder"
>
>         iinst                            = loadInst pergm
>         jinst                            = loadInst pergm{pgkwInst = pergm.pgkwInst + 1}
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
>     loadInst pergm                       = sffile.zFileArrays.ssInsts ! pergm.pgkwInst

PreZone administration ================================================================================================

> goodZRecs              :: ResultDispositions → [InstZoneRecord] → [InstZoneRecord]
> goodZRecs rdNow                          = filter (\x → not (deadrd (instKey x) rdNow))
>
> data InstZoneRecord                      =
>   InstZoneRecord {
>     zswFile            :: Word
>   , zswInst            :: Word
>   , zsSmashup          :: Maybe (Smashing Word)
>   , zsInstCat          :: Maybe InstCat
>   , zsGlobalKey        :: Maybe PreZoneKey
>   , zsPreZones         :: [PreZone]}
> instance Show InstZoneRecord where
>   show zrec                              =
>     unwords ["InstZoneRecord", show (zrec.zswFile, zrec.zswInst)
>                              , showMaybeInstCat zrec.zsInstCat, show $ length zrec.zsPreZones]
> makeZRec               :: PerGMKey → InstZoneRecord
> makeZRec pergm                           =
>   InstZoneRecord pergm.pgkwFile pergm.pgkwInst Nothing Nothing Nothing []
> instKey                :: InstZoneRecord → PerGMKey
> instKey zrec                             = PerGMKey zrec.zswFile zrec.zswInst Nothing       

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
>         (zrec': zrecs, rdFold')
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
          instantiate zrecs

> surveyTaskIf _ _ fwIn                    = fwIn{fwZRecs = map makeZRec (Map.keys fwIn.fwBoot.zPreInstCache)}

capture task ==========================================================================================================
          populate zrecs with PreZones

> captureTaskIf sffile _ fwIn              = zrecTask capturer fwIn
>   where
>     capturer zrec rdIn                   =
>       let
>         (newPzs, rdOut, mglobalKey)      = captureZones (instKey zrec) rdIn
>       in
>         (zrec{zsPreZones = newPzs, zsGlobalKey = mglobalKey}, rdOut)
>
>     captureZones       :: PerGMKey → ResultDispositions → ([PreZone], ResultDispositions, Maybe PreZoneKey)
>     captureZones pergm rdCap__           = (pzs', rdCap'', globalKey)
>       where
>         fName_                           = "captureZones"
>
>         preI                             = fwIn.fwBoot.zPreInstCache Map.! pergm
>         iName                            = preI.piChanges.cnName
>         results                          = map captureZone (deriveRange ibagi jbagi)
>
>         pzs                              = lefts results
>         globalKey                        =
>           if head results == Right (Accepted, GlobalZone)
>             then Just $ PreZoneKey sffile.zWordF pergm.pgkwInst ibagi 0
>             else Nothing
>
>         iinsts                           = sffile.zFileArrays.ssInsts
>         ibagi                            = F.instBagNdx (iinsts ! pgkwInst pergm)
>         jbagi                            = F.instBagNdx (iinsts ! (pgkwInst pergm + 1))
>
>         noZones, illegalRange, hasRoms, illegalLimits, yesAdopt
>                        :: Maybe [Scan] 
>         ss = deJust fName_
>              $ noZones `CM.mplus` illegalRange
>                        `CM.mplus` hasRoms
>                        `CM.mplus` illegalLimits
>                        `CM.mplus` yesAdopt
>         noZones
>           | null pzs                     = Just [Scan Violated NoZones fName_ noClue]
>           | otherwise                    = Nothing
>         illegalRange
>           | isJust mpz                   = Just [Scan Violated CorruptGMRange  fName_ clue]
>           | otherwise                    = Nothing
>           where
>             mpz                          = find zoneBad pzs
>
>             zoneBad pz                   = not (okGMRanges pz.pzDigest)
>             clue                         = showBad $ fromJust mpz
>         hasRoms
>           | isJust mpz                   = Just [Scan Violated RomBased fName_ clue]
>           | otherwise                    = Nothing
>           where
>             mpz                          = find zoneRom pzs
>
>             stype pz                     = F.sampleType (effPZShdr pz)
>             zoneRom pz                   = stype pz >= 0x8000
>             pzBad                        = fromJust mpz
>             clue                         = showHex (stype pzBad) []
>         illegalLimits
>           | isJust result                = Just [Scan Violated BadSampleLimits fName_ (fromJust result)]
>           | otherwise                    = Nothing
>           where
>             tested                       = map illegalSampleSize pzs
>             result                       = foldr CM.mplus Nothing tested
>         yesAdopt                         = Just [Scan Modified Adopted fName_ iName]
>
>         rdCap'                           = dispose pergm ss rdCap__
>         (pzs', rdCap'')                  = zoneTask (const True) capFolder pzs rdCap'
>
>         capFolder      :: PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions)
>         capFolder pz rdFold              =
>           let
>             impact                       = if wasSwitchedToMono pz
>                                              then AdoptedAsMono
>                                              else Adopted
>             ssImpact                     = [Scan Modified impact fName_ (show iName)]
>           in
>             (Just pz, dispose (extractSampleKey pz) ssImpact rdFold)
>             
>         captureZone    :: Word → Either PreZone (Disposition, Impact)
>         captureZone bix
>           | isNothing pz.pzDigest.zdSampleIndex
>                                          = Right (Accepted, GlobalZone)
>           | isNothing mpres              = Right (Dropped, OrphanedBySample)
>           | otherwise                    = Left pz{pzChanges = ChangeEar (effPSShdr pres) []}
>           where
>             fName                        = unwords ["captureZone"]
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
>             si                           = deJust (unwords [fName, "si"]) pz.pzDigest.zdSampleIndex
>             presk                        = PreSampleKey sffile.zWordF si
>             mpres                        = presk `Map.lookup` fwIn.fwBoot.zPreSampleCache
>             pres                         = deJust (unwords [fName, "pres"]) mpres

mark task =============================================================================================================
          copy global zone markers from zrecs to preInstCache

> markTaskIf _ _ fwIn@FileWork{fwBoot}     = fwIn{fwBoot = fwBoot{zPreInstCache = preInstCache'}}
>   where
>     preInstCache'                        = zrecCompute fwIn markFolder fwBoot.zPreInstCache
>     markFolder preInstCache zrec         =
>       let
>         pergm                            = instKey zrec
>         preI                             = preInstCache Map.! pergm
>       in
>         Map.insert pergm (preI{iGlobalKey = zrec.zsGlobalKey}) preInstCache

smash task ============================================================================================================
          compute smashups for each instrument

> smashTaskIf _ _                          = zrecTask smasher
>   where
>     smasher zrec rdFold                  =
>       (zrec{zsSmashup = Just (computeInstSmashup (show $ instKey zrec) zrec.zsPreZones)}, rdFold)
>
> computeInstSmashup     :: String → [PreZone] → Smashing Word
> computeInstSmashup tag pzs               = smashSubspaces tag [128, 128, 2] (map extractSpace pzs)

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
>       | not doAbsorption                 = (zrec,                       rdFold)
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
>     instNames          :: [(String, Word)]
>     instNames                            =
>       let
>         extractFolder ns zrec            = (preI.piChanges.cnName, zrec.zswInst) : ns
>           where
>             preI                         = fwIn.fwBoot.zPreInstCache Map.! instKey zrec
>       in
>         sort $ zrecCompute fwIn extractFolder []
>     
>     townersMap         :: Map Word ([PreZone], Smashing Word)
>     townersMap                           =
>       let
>         tFolder m zrec                   = Map.insert zrec.zswInst (zrec.zsPreZones, fromJust zrec.zsSmashup) m
>       in
>         zrecCompute fwIn tFolder Map.empty 
>     
>     grouped                              = groupBy (\x y → absorbRatio < howClose (fst x) (fst y)) instNames
>     filteredByGroupSize                  = filter (\x → 1 < length x) grouped
>     stringsDropped                       = map (map snd) filteredByGroupSize
>     headed, ready        :: Map Word [Word]
>     headed                               = Map.fromList $ map (\q → (head q, q)) stringsDropped
>
>     hMap               :: Map Word ([PreZone], Smashing Word)
>     dMap               :: Map Word SmashStats
>     (hMap, dMap)                         = Map.mapEitherWithKey qualify headed
>       where
>         qualify        :: Word → [Word] → Either ([PreZone], Smashing Word) SmashStats
>         qualify leadI memberIs
>           | 0 == osmashup.smashStats.countMultiples
>                                          = Left (rebased, osmashup)
>           | memberHasVelocityRanges      = Left (rebased, osmashup)
>           | otherwise                    = Right osmashup.smashStats
>           where
>             towners                      = map (townersMap Map.!) memberIs
>
>             combined                     = concatMap fst towners
>             rebased                      = map rebase combined where rebase pz = pz{pzWordI = leadI}
>
>             smashups                     = map snd towners
>             osmashup                     = foldl' smashSmashings (head smashups) (tail smashups)
>
>             memberHasVelocityRanges      = isJust $ find zonesHaveVelocityRange (map fst towners)
>             zonesHaveVelocityRange pzs   = isJust $ find zoneHasVelocityRange pzs   
>             zoneHasVelocityRange pz      =
>               case pz.pzDigest.zdVelRange of
>                 Just rng                 → rng /= (0, 127)
>                 Nothing                  → False
>
>     ready                                = Map.mapWithKey (\k _ → headed Map.! k) hMap
>
>     aMap               :: Map Word Word
>     aMap                                 = Map.foldlWithKey fold1Fun Map.empty ready
>       where
>         fold1Fun       :: Map Word Word → Word → [Word] → Map Word Word
>         fold1Fun qIn wLead               = foldl' fold2Fun qIn
>           where
>             fold2Fun   :: Map Word Word → Word → Map Word Word
>             fold2Fun qFold wMember       = Map.insert wMember wLead qFold

shave task ============================================================================================================
          remove dropped or violated instruments from the preInstCache

> shaveTaskIf _ _ fwIn                     = fwIn{fwBoot = fwIn.fwBoot{zPreInstCache = preInstCache}}
>   where
>     validPergms        :: [PerGMKey]
>     validPergms                          = zrecCompute fwIn (\x y → instKey y : x) []
>
>     preInstCache                         =
>       Map.filterWithKey (\ x _ → x `elem` validPergms) fwIn.fwBoot.zPreInstCache

match task ============================================================================================================
          track all fuzzy matches

> matchTaskIf _ _ fwIn                     = fwIn{fwMatches = Matches sMatches iMatches}
>   where
>     sMatches                             =
>       Map.foldlWithKey (\m k v → Map.insert k (computeFFMatches v.cnName) m)           Map.empty fwIn.fwBoot.zPreSampleCache
>     iMatches                             =
>       Map.foldlWithKey (\m k v → Map.insert k (computeFFMatches v.piChanges.cnName) m) Map.empty fwIn.fwBoot.zPreInstCache

categorization task ===================================================================================================
          assign each instrument to one of the three categories
          a. Just InstCatInst              an inst bearing one inst, or
          b. Just InstCatPerc              an inst bearing one or more percs, or
          c. Just InstCatDisq              an inst disqualified from tournaments, or
          d. Nothing                       undecided

> catTaskIf _ rost fwIn@FileWork{fwBoot, fwMatches}     
>                                          = zrecTask catter fwIn
>   where
>     catter zrec rdFold                   = (zrec{zsInstCat = icat}, dispose (instKey zrec) ss rdFold)
>       where
>         (icat, ss)                       = categorizeInst zrec
>
>     categorizeInst     :: InstZoneRecord → (Maybe InstCat, [Scan])
>     categorizeInst zrec
>       | traceIf trace_CI False           = undefined
>       | otherwise                        = (icat', ss')
>       where
>         fName                            = "categorizeInst"
>         trace_CI                         = unwords [fName, show pergm, show $ length pzs]
>
>         pergm                            = instKey zrec
>         pzs                              = zrec.zsPreZones
>         preI                             = fwBoot.zPreInstCache Map.! pergm
>         iName                            = preI.piChanges.cnName

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
>             ffPerc'                      =
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
>         qualPercZone rost' prez          = mrange >>= pinnedKR (select rost') >> Just prez.pzWordB
>           where
>             mrange                       = prez.pzDigest.zdKeyRange >>= (Just . BF.bimap fromIntegral fromIntegral)

build zone task =======================================================================================================
          generate the PerInstrument map (aka zone cache)

> perITaskIf sffile _ fwIn@FileWork{fwBoot, fwDispositions}  
>                                          = fwIn{  fwBoot = fwBoot{zPerInstCache = fst formZoneCache}
>                                                 , fwDispositions = snd formZoneCache}
>   where
>     formZoneCache      :: (Map PerGMKey PerInstrument, ResultDispositions)
>     formZoneCache                        = zrecCompute fwIn zcFolder (Map.empty, fwDispositions)
>
>     zcFolder (zc, rdFold) zrec
>       | traceIf trace_ZCF False          = undefined
>       | otherwise                        =
>         (   Map.insert pergm perI zc
>           , dispose pergm [Scan Accepted ToZoneCache fName (show icat)] rdz)
>       where
>         fName                            = "zcFolder"
>         trace_ZCF                        = unwords [fName, show zrec, show pergm, show zrec.zsInstCat]
>
>         pergm                            = instKey zrec
>         icat                             = fromJust zrec.zsInstCat
>         smashup                          = fromJust zrec.zsSmashup
>         perI                             = computePerInst zrec icat smashup
>         pzs                              = map fst perI.pZones
>         (_, rdz)                         = zoneTask (const True) blessZone pzs rdFold
>         blessZone      :: PreZone → ResultDispositions → (Maybe PreZone, ResultDispositions)
>         blessZone pz rdIn                = (Just pz, dispose
>                                              (extractZoneKey pz)
>                                              [Scan Accepted ToZoneCache fName (show pz.pzWordB)]
>                                              rdIn)
>
>     computePerInst     :: InstZoneRecord → InstCat → Smashing Word → PerInstrument
>     computePerInst zrec icat smashup
>       | traceIf trace_CPI False          = undefined
>       | otherwise                        = PerInstrument (zip pzs oList) icat smashup
>       where
>         fName                            = "computePerInst"
>         trace_CPI                        = unwords [fName, show pergm, show $ length zrec.zsPreZones, show $ length oList]
>
>         pergm                            = instKey zrec
>         preI                             = fwBoot.zPreInstCache Map.! pergm
>
>         bixen          :: [Word]
>         bixen                            =
>           case icat of
>             InstCatPerc x                → x
>             InstCatInst                  → map pzWordB zrec.zsPreZones
>             _                            → error $ unwords [fName, "only Inst and Perc are valid here"]
>
>         gZone                            =
>           case preI.iGlobalKey of
>             Nothing                      → defZone
>             Just pzk                     → buildZone sffile defZone Nothing pzk.pzkwBag
>         oList                            = map (\pz → buildZone sffile gZone (Just pz) pz.pzWordB) pzs
>
>         pzs                              = filter (\pz → pz.pzWordB `elem` bixen) zrec.zsPreZones
>
> buildZone              :: SFFile → SFZone → Maybe PreZone → Word → SFZone
> buildZone sffile fromZone mpz bagIndex
>   | traceIf trace_BZ False               = undefined
>   | otherwise                            = zone
>   where
>     fName                                = "buildZone"
>     trace_BZ                             =
>       unwords [fName, show (sffile.zWordF, bagIndex), show zName, show (fromZone == defZone)]
>
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
>     stA                                  = shdr.start     + fromIntegral zd.zdStart
>     enA                                  = shdr.end       + fromIntegral zd.zdEnd
>     stL                                  = shdr.startLoop + fromIntegral zd.zdStartLoop
>     enL                                  = shdr.endLoop   + fromIntegral zd.zdEndLoop
>
>     ok                                   =
>       0 <= stA && stA <= enA && 0 <= stL && stL <= enL
>       && enA - stA < 2  ^ (22::Word)
>       && (zd.zdSampleMode == Just A.NoLoop || enL - stL < 2 ^ (22::Word))
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
> doAbsorption           :: Bool
> doAbsorption                             = True
> fixBadNames            :: Bool
> fixBadNames                              = True

The End
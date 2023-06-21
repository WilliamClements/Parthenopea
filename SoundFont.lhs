> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

SoundFont support =========================================================================

> module SoundFont where
>
> import qualified Codec.SoundFont      as F
> import Control.Monad.IO.Class
> import qualified Data.Audio           as A
> import Data.Array.Unboxed ( array, Array, (!), IArray(bounds) )
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List ( find, foldr, minimumBy )
> import qualified Data.Map             as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, catMaybes, mapMaybe )
> import Data.Time.Clock
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, AudSF, Mono, Stereo )
> import Euterpea.Music
> import Parthenopea ( traceIf, traceAlways, listI, initCase, listP )
> import Synthesizer
  
importing sampled sound (from SoundFont (*.sf2) files) ====================================

> data PerGMKey =
>   PerGMKey {
>     pWordF             :: Word
>   , pWordI             :: Word
>   , pWordZ             :: Maybe Word} deriving (Eq, Ord, Show)
>
> data PerGMScored =
>   PerGMScored {
>     pStaticScore       :: Int
>   , pPerGMKey          :: PerGMKey} deriving (Eq, Ord, Show)
>
> type InstrLocator   = Map.Map InstrumentName PerGMScored
> type PercLocator    = Map.Map PercussionSound PerGMScored
> type Locators       = (InstrLocator, PercLocator)
>
> type NameMap        = Map.Map String Word
> type NameMaps       = (NameMap, NameMap)
>
> type ZoneCache      = Map.Map PerGMKey [(Word, SFZone)]
>
> data SFRoster =
>   SFRoster {
>     zDatabase          :: SoundFontDatabase
>   , zFiles             :: Array Int SFFile
>   , zZoneCache         :: ZoneCache
>   , zLocs              :: Locators}
>
> data SFFile =
>   SFFile {
>     zFilename          :: FilePath
>   , zArrays            :: SoundFontArrays
>   , zWordF             :: Word
>   , zScore             :: Int}
>
> data SoundFontArrays = 
>   SoundFontArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssShdrs            :: Array Word F.Shdr
>   , ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
>
> data SFZone =
>   SFZone {
>     zStartOffs         :: Maybe Int
>   , zEndOffs           :: Maybe Int
>   , zLoopStartOffs     :: Maybe Int
>   , zLoopEndOffs       :: Maybe Int
>
>   , zStartCoarseOffs   :: Maybe Int
>   , zEndCoarseOffs     :: Maybe Int
>   , zLoopStartCoarseOffs
>                        :: Maybe Int
>   , zLoopEndCoarseOffs :: Maybe Int
>
>   , zInstIndex         :: Maybe Word
>   , zKeyRange          :: Maybe (AbsPitch, AbsPitch)
>   , zVelRange          :: Maybe (Volume, Volume)
>   , zCoarseTune        :: Maybe Int
>   , zFineTune          :: Maybe Int
>   , zSampleIndex       :: Maybe Word
>   , zSampleMode        :: Maybe A.SampleMode
>
>   , zDelayVolEnv       :: Maybe Int
>   , zAttackVolEnv      :: Maybe Int
>   , zHoldVolEnv        :: Maybe Int
>   , zDecayVolEnv       :: Maybe Int
>   , zSustainVolEnv     :: Maybe Int 
>   , zReleaseVolEnv     :: Maybe Int
>
>   , zChorus            :: Maybe Int -- designated M
>   , zReverb            :: Maybe Int -- designated VH
>   , zPan               :: Maybe Int -- designated VH
>
>   , zRootKey           :: Maybe Word
>
>   , zModLfoToPitch     :: Maybe Int
>   , zVibLfoToPitch     :: Maybe Int
>   , zModEnvToPitch     :: Maybe Int
>   , zInitFc            :: Maybe Int
>   , zInitQ             :: Maybe Int -- designated L
>   , zModLfoToFc        :: Maybe Int
>   , zModEnvToFc        :: Maybe Int
>   , zModLfoToVol       :: Maybe Int
>   , zDelayModLfo       :: Maybe Int
>   , zFreqModLfo        :: Maybe Int -- designated M
>   , zDelayVibLfo       :: Maybe Int
>   , zFreqVibLfo        :: Maybe Int -- designated M
>   , zDelayModEnv       :: Maybe Int
>   , zAttackModEnv      :: Maybe Int -- designated M
>   , zHoldModEnv        :: Maybe Int
>   , zDecayModEnv       :: Maybe Int
>   , zSustainModEnv     :: Maybe Int -- designated M
>   , zReleaseModEnv     :: Maybe Int -- designated L
>   , zKeyToModEnvHold   :: Maybe Int
>   , zKeyToModEnvDecay  :: Maybe Int
>   , zKeyToVolEnvHold   :: Maybe Int
>   , zKeyToVolEnvDecay  :: Maybe Int} deriving Show
>
> defInstrumentZone      :: SFZone
> defInstrumentZone = SFZone Nothing Nothing Nothing Nothing
>
>                            Nothing Nothing Nothing Nothing
>
>                            Nothing Nothing Nothing Nothing
>                            Nothing Nothing Nothing
>
>                            Nothing Nothing Nothing Nothing
>                            Nothing Nothing
>
>                            Nothing Nothing Nothing
>
>                            Nothing
>
>                            Nothing Nothing Nothing Nothing
>                            Nothing Nothing Nothing Nothing
>                            Nothing Nothing Nothing Nothing
>                            Nothing Nothing Nothing Nothing
>                            Nothing Nothing Nothing Nothing
>                            Nothing Nothing
>
> fileName               :: SFRoster → Word → String
> fileName sfrost wordF = zFilename (zFiles sfrost ! fromIntegral wordF)
>
> instrName              :: SFFile → F.Inst → String
> instrName sffile = F.instName
>
> zoneName               :: SFFile → F.Inst → SFZone → String
> zoneName sffile iinst sfzone =
>   let
>     arrays = zArrays sffile
>     msin = zSampleIndex sfzone
>   in
>     instrName sffile iinst
>     ++ "/"
>     ++ maybe "Global" (\a → F.sampleName (ssShdrs arrays ! a)) msin
>
> getStaticScore         :: PerGMScored → Int
> getStaticScore = pStaticScore
>
> computeStaticScore     :: ZoneCache → SFFile → PerGMKey → Int
> computeStaticScore zc sffile pergm = sum h''
>   where
>     zs                                   = tail $ fromJust $ Map.lookup pergm zc
>     arrays                               = zArrays sffile
>
>     desires                              =  [desireReStereo
>                                            , desireRe24Bit
>                                            , desireReMaxSplits]
>     weights                              = [4, 2, 1]
>     empiricals                           = [scoreBool $ isStereoInst arrays zs
>                                           , scoreBool $ is24BitInst arrays zs
>                                           , scoreBool $ hasMaxSplits arrays zs]
>
>     h                                    = map scoreDesire    desires
>     h'                                   = zipWith (*) h      weights
>     h''                                  = zipWith (*) h'     empiricals
>
>     isStereoInst, is24BitInst, hasMaxSplits
>                        :: SoundFontArrays → [(Word, SFZone)] → Bool
>
>     isStereoInst arrays zs =
>       let
>         mStereo = find (isStereoZone arrays) zs
>       in
>         isJust mStereo 
>       
>     isStereoZone       :: SoundFontArrays → (Word, SFZone) → Bool
>     isStereoZone arrays (_, zone) =
>       let
>         sin = fromJust $ zSampleIndex zone
>         shdr = ssShdrs arrays ! sin
>         stype = toSampleType $ F.sampleType shdr 
>       in
>         stype == SampleTypeRight || stype == SampleTypeLeft
>         
>     is24BitInst arrays _ =
>       let
>         m24 = ssM24 arrays
>       in
>         isJust m24
>       
>     hasMaxSplits _ zs = length zs > fromIntegral splitThreshold

slurp in instruments from SoundFont (*.sf2) files =========================================

> fileByIndex            :: SFRoster → Word → SFFile
> fileByIndex sfrost wFile = zFiles sfrost ! fromIntegral wFile
>
> doSoundFont            :: SoundFontDatabase
>                           → [(String, Music (Pitch, [NoteAttribute]))]
>                           → IO ()
> doSoundFont sfdb songs =
>   do
>     putStrLn "processing..."
>     ts1 ← getCurrentTime
>     let numberedDB = zip [0..] sfdb
>     sffilesp ← mapM (uncurry readSoundFontFile) numberedDB
>     let zonesP = cacheZones (map fst sffilesp)
>     putStrLn ("# available instr =" ++ show (Map.size zonesP))
>     ts2 ← getCurrentTime
>     putStrLn ("___load files: " ++ show (diffUTCTime ts2 ts1))
>     let ((iloc, ploc), probs) = foldr (uncurry (chooseIAndP zonesP))
>                                       ((Map.empty, Map.empty), [])
>                                       sffilesp
>     mapM_ putStrLn probs
>     let sfrost = SFRoster sfdb
>                           (array (0, length sffilesp - 1) (zip [0..] (map fst sffilesp)))
>                           zonesP
>                           (iloc, ploc)
>     let imap = assignInstruments sfrost iloc
>     let pmap = assignAllPercussion sfrost ploc
>     let imap' = imap ++ [doAssignP sfrost pmap]
>     ts3 ← getCurrentTime
>     putStrLn ("___prepare instruments: " ++ show (diffUTCTime ts3 ts2))
>     _  ← renderSongs sfrost imap' songs
>     ts4 ← getCurrentTime
>     putStrLn ("___render songs: " ++ show (diffUTCTime ts4 ts3))
>     return ()
>  
> readSoundFontFile      :: Word
>                           → ( FilePath
>                             , (   [Hints]
>                               , (   [(String, ([Hints], InstrumentName))]
>                                 ,   [(String, [(String, ([Hints], PercussionSound))])])))
>                           → IO (SFFile
>                                 , (   [(String, ([Hints], InstrumentName))]
>                                    ,  [(String, [(String, ([Hints], PercussionSound))])]))
> readSoundFontFile wFile (filename, (filehints, (ilist, plist))) =
>   do
>     putStr filename
>     ts1 ← getCurrentTime
>     maybeAudio ← F.importFile filename
>     case maybeAudio of
>       Left s               → error $ "SoundFont decoding error: " ++ s
>       Right soundFont      → do
>         let pdata = F.pdta soundFont
>         let sdata = F.sdta soundFont
>         let arrays = SoundFontArrays
>                        (F.insts pdata)
>                        (F.ibags pdata)
>                        (F.igens pdata)
>                        (F.shdrs pdata)
>                        (F.smpl sdata)
>                        (F.sm24 sdata)
>         let sffile = SFFile filename arrays wFile (2 * sfscore filehints)
>
>         ts2 ← getCurrentTime
>         let nBits = case ssM24 arrays of
>               Nothing          → 16
>               Just s24data     → 24
>         putStrLn (" (" ++ show nBits ++ ") loaded in " ++ show (diffUTCTime ts2 ts1))
>
>         return (sffile, (ilist, plist))
>
> renderSong             :: SFRoster
>                           → InstrMap (Stereo AudRate)
>                           → FilePath
>                           → Music (Pitch, [NoteAttribute])
>                           → IO ()
> renderSong sfrost imap name song =
>   do
>     ts1 ← getCurrentTime
>     let is = Map.keys $ fst $ listI song initCase (Map.empty, Map.empty)
>     let ps = Map.keys $ listP song initCase Map.empty
>     printChoices sfrost is ps
>     let path = name ++ ".wav"
>     putStr path
>     let (d,s) = renderSF song imap
>     outFile path d s
>     ts2 ← getCurrentTime
>     putStrLn (" (dur=" ++ show d ++ ") written in " ++ show (diffUTCTime ts2 ts1))
>     return ()
>
> renderSongs            :: SFRoster
>                           → InstrMap (Stereo AudRate)
>                           → [(String, Music (Pitch, [NoteAttribute]))]
>                           → IO ()
> renderSongs sfrost imap songs =
>   do
>     mapM_ (uncurry (renderSong sfrost imap)) songs
  
extract data from SoundFont per instrument ================================================

> chooseI                :: ZoneCache
>                           → SFFile
>                           → [(String, ([Hints], InstrumentName))]
>                           → NameMaps
>                           → (Locators, [String])
>                           → (Locators, [String])
> chooseI _ _ [] _ cur                     = cur
> chooseI zc sffile (x:xs) (nMapI, nMapZ) ((iloc, ploc), probs) =
>   let
>     zF = zWordF sffile
>     mwInstr            :: Maybe Word     = Map.lookup (fst x) nMapI
>     ((iloc', ploc'), probs')
>                        :: (Locators, [String])
>       | isNothing mwInstr
>                        = ((iloc, ploc), ("Instrument "
>                                          ++ fst x
>                                          ++ " not found in "
>                                          ++ zFilename sffile) : probs)
>       | isNothing mPrevious || myScore > oldScore
>                        = ((Map.insert iname
>                                       (PerGMScored myScore
>                                                    (PerGMKey zF
>                                                              (fromJust mwInstr)
>                                                              Nothing))
>                                       iloc, ploc), probs)
>       | otherwise      = ((iloc, ploc), probs)
>     iname              :: InstrumentName = (snd.snd) x
>     mPrevious          :: Maybe PerGMScored
>                                          = Map.lookup ((snd.snd) x) iloc
>     myScore            :: Int            = computeStaticScore zc
>                                                               sffile
>                                                               (PerGMKey zF (fromJust mwInstr) Nothing)
>                                          + zScore sffile + sfscore ((fst.snd) x)
>     oldScore           :: Int            = getStaticScore $ fromJust mPrevious
>   in
>     chooseI zc sffile xs (nMapI, nMapZ) ((iloc', ploc), probs')
>
> chooseP                  :: ZoneCache
>                           → SFFile
>                           → [(String, [(String, ([Hints], PercussionSound))])]
>                           → NameMaps
>                           → (Locators, [String])
>                           → (Locators, [String])
> chooseP _ _ [] _ cur                  = cur
> chooseP zc sffile (x:xs) (nMapI, nMapZ) ((iloc, ploc), probs)
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     mwInstr            :: Maybe Word     = Map.lookup (fst x) nMapI
>     ((iloc', ploc'), probs')
>                        :: (Locators, [String])
>       | isNothing mwInstr
>                        =  ((iloc, ploc), ("Instrument "
>                                           ++ fst x
>                                           ++ "not found in "
>                                           ++ zFilename sffile) : probs)
>       | otherwise      = chooseSounds zc sffile
>                                         (fromJust mwInstr)
>                                         (snd x)
>                                         (nMapI, nMapZ)
>                                         ((iloc, ploc), probs)
>   in
>     chooseP zc sffile xs (nMapI, nMapZ) ((iloc', ploc'), probs')
>   where
>     msg = unwords ["nMapZ=", show nMapZ]
>     chooseSounds       :: ZoneCache
>                           → SFFile
>                           → Word
>                           → [(String, ([Hints], PercussionSound))]
>                           → NameMaps
>                           → (Locators, [String])
>                           → (Locators, [String])
>     chooseSounds _ _ _ [] _ cur = cur
>     chooseSounds zc sffile wordI (x:xs) (nMapI, nMapZ) ((iloc, ploc), probs) =
>       let
>         mwZone         :: Maybe Word     = Map.lookup (fst x) nMapZ
>         zF = zWordF sffile
>         ((iloc', ploc'), probs')
>                        :: (Locators, [String])
>           | isNothing mwZone
>                        = ((iloc, ploc), ("Zone "
>                                          ++ fst x
>                                          ++ " not found in "
>                                          ++ zFilename sffile) : probs)
>           | isNothing mPrevious || myScore > oldScore
>                        = ((iloc, Map.insert ((snd.snd) x)
>                                             (PerGMScored myScore
>                                                            (PerGMKey zF
>                                                                      wordI
>                                                                      Nothing))
>                                             ploc), probs)
>           | otherwise  = ((iloc, ploc), probs)
>         mPrevious      :: Maybe PerGMScored
>                                          = Map.lookup ((snd.snd) x) ploc
>         myScore        :: Int            = computeStaticScore zc
>                                                               sffile
>                                                               (PerGMKey zF wordI Nothing)
>                                            + zScore sffile + sfscore ((fst.snd) x)
>         oldScore       :: Int            = getStaticScore (fromJust mPrevious)
>       in
>         chooseSounds zc sffile wordI xs (nMapI, nMapZ) ((iloc', ploc'), probs')
>
> chooseIAndP            :: ZoneCache
>                           → SFFile
>                           → (  [(String, ([Hints], InstrumentName))]
>                              , [(String, [(String, ([Hints], PercussionSound))])])
>                           → (Locators, [String])
>                           → (Locators, [String])
> chooseIAndP zc sffile (is, ps) ((iloc, ploc), probs)
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     arrays = zArrays sffile
>     (nMapI, nMapZ)                 = makeNameMaps arrays
>     ((iloc', ploc'), probs')       = chooseI zc sffile is
>                                              (nMapI, nMapZ) ((iloc, ploc), probs)
>     ((iloc'', ploc''), probs'')    = chooseP zc sffile ps
>                                              (nMapI, nMapZ) ((iloc', ploc'), probs')
>   in
>     ((iloc'', ploc''), probs'')
>   where
>     msg = unwords ["chooseIAndP ", show (is, ps)]
>  
> buildZone              :: SFFile → F.Inst → SFZone → Word → (Word, SFZone)
> buildZone sffile iinst fromZone bagIndex =
>   let
>     arrays = zArrays sffile
>     ibags = ssIBags arrays
>     xgeni = F.genNdx $ ibags!bagIndex
>     ygeni = F.genNdx $ ibags!(bagIndex + 1)
>     gens = if ygeni - xgeni < 0
>            then error "degenerate generator list"
>            else getGens sffile iinst [xgeni..ygeni-1]
>     zone = fromGens fromZone gens
>   in (bagIndex, zone)
>
> getGens                :: SFFile → F.Inst → [Word] → [F.Generator]
> getGens sffile iinst words
>   | traceIf msg False = undefined
>   | otherwise = gens
>   where
>     arrays = zArrays sffile
>     selected = map (doGenerator sffile iinst) words
>     filtered = filter isJust selected
>     gens = map fromJust filtered
>
>     msg = unwords ["getGens=", show words]
> 
> doGenerator :: SFFile → F.Inst → Word → Maybe F.Generator
> doGenerator sffile iinst zw = Just $ ssIGens (zArrays sffile) ! zw
>
> fromGens               :: SFZone → [F.Generator] → SFZone
> fromGens iz [] = iz
> fromGens iz (g:gs) = fromGens iz' gs
>   where iz' = addGen iz g
>   
> addGen                 :: SFZone → F.Generator → SFZone
> addGen iz gen =
>   case gen of
>   F.StartAddressOffset i         → iz {zStartOffs =                Just i}
>   F.EndAddressOffset i           → iz {zEndOffs =                  Just i}
>   F.LoopStartAddressOffset i     → iz {zStartOffs =                Just i}
>   F.LoopEndAddressOffset i       → iz {zEndOffs =                  Just i}
>
>   F.StartAddressCoarseOffset i   → iz {zStartCoarseOffs =          Just i}
>   F.EndAddressCoarseOffset i     → iz {zEndCoarseOffs =            Just i}
>   F.LoopStartAddressCoarseOffset i
>                                  → iz {zLoopStartCoarseOffs =      Just i}
>   F.LoopEndAddressCoarseOffset i
>                                  → iz {zLoopEndCoarseOffs =        Just i}
>
>   F.InstIndex w                  → iz {zInstIndex =                Just w}
>   F.KeyRange a b                 → iz {zKeyRange = Just (fromIntegral a, fromIntegral b)}
>   F.VelRange a b                 → iz {zVelRange = Just (fromIntegral a, fromIntegral b)}
>   F.CoarseTune i                 → iz {zCoarseTune =               Just i}
>   F.FineTune i                   → iz {zFineTune =                 Just i}
>   F.SampleIndex w                → iz {zSampleIndex =              Just w}
>   F.SampleMode a                 → iz {zSampleMode =               Just a}
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
> sfscore                :: [Hints] → Int
> sfscore = foldr sfscorehint 0
>
> sfscorehints            :: Hints → Int
> sfscorehints h = case h of
>                  DLow → -1
>                  DMed → 0
>                  DHigh → 1
>             
> sfscorehint            :: Hints → Int →  Int
> sfscorehint hint accum = accum + sfscorehints hint   
>
> makeNameMap            :: forall a. () ⇒ Array Word a → (a → String) → NameMap
> makeNameMap as getString =
>   let
>     (ast, aen) = bounds as 
>     avs = map akv [ast..aen-1]
>   in
>     Map.fromList avs
>   where
>     akv                :: Word → (String, Word)
>     akv anum = (getString (as ! anum), anum)
> 
> makeNameMapI arrays = makeNameMap (ssInsts arrays) F.instName
> makeNameMapP arrays = makeNameMap (ssShdrs arrays) F.sampleName
>     
> makeNameMaps           :: SoundFontArrays → NameMaps
> makeNameMaps arrays = (makeNameMapI arrays, makeNameMapP arrays)

prepare the specified instruments and percussion ==========================================

> cacheZones             :: [SFFile] → ZoneCache
> cacheZones sffiles = Map.fromList $ concatMap cacheF sffiles
>   where
>     cacheF             :: SFFile → [(PerGMKey, [(Word, SFZone)])]
>     cacheF sffile = concatMap (cacheI sffile) [fst boundsI..snd boundsI]
>       where
>         arrays = zArrays sffile
>         boundsI = bounds (ssInsts arrays)
>
>     cacheI         :: SFFile → Word → [(PerGMKey, [(Word, SFZone)])]
>     cacheI sffile wI = [(PerGMKey wF wI Nothing, zs)]
>       where
>         wF         :: Word           = zWordF sffile
>         zs         :: [(Word, SFZone)]
>                                      = getZones sffile (wF, wI)
>        
>     getZones           :: SFFile → (Word, Word) → [(Word, SFZone)]
>     getZones sffile (zF, zI) = gList ++ oList
>       where
>         arrays = zArrays sffile
>         iinst  = ssInsts arrays ! zI
>         jinst  = ssInsts arrays ! (zI+1)
>         ibagi  = F.instBagNdx iinst
>         jbagi  = F.instBagNdx jinst
>         gIx    = if jbagi - ibagi < 2
>                  then error "must have one global zone and at least one other zone"
>                  else [ibagi]
>         oIx   = [ibagi+1..jbagi-1]
>         gList = map (buildZone sffile iinst defInstrumentZone)   gIx
>         oList = map (buildZone sffile iinst (snd (head gList)))  oIx
>
> assignInstruments      :: SFRoster
>                           → InstrLocator
>                           → [(InstrumentName, Instr (Stereo AudRate))]
> assignInstruments sfrost = Map.foldrWithKey (ipal2imap sfrost) []
>
> ipal2imap              :: SFRoster
>                           → InstrumentName
>                           → PerGMScored
>                           → [(InstrumentName, Instr (Stereo AudRate))]
>                           → [(InstrumentName, Instr (Stereo AudRate))]
> ipal2imap sfrost iname pergm accum = (iname, assignInstrument sfrost pergm') : accum
>   where
>     pergm' = pPerGMKey pergm
>
> assignAllPercussion    :: SFRoster
>                           → PercLocator
>                           → [(PercussionSound, (Word, Word))]
> assignAllPercussion sfrost = Map.foldrWithKey (ppal2pmap sfrost) []
>
> ppal2pmap              :: SFRoster
>                           → PercussionSound
>                           → PerGMScored
>                           → [(PercussionSound, (Word, Word))]
>                           → [(PercussionSound, (Word, Word))]
> ppal2pmap sfrost psound pergm accum = (psound, (pWordF pergm', pWordI pergm')) : accum
>   where
>     pergm' = pPerGMKey pergm
>
> doAssignP              :: SFRoster
>                           → [(PercussionSound, (Word, Word))]
>                           → (InstrumentName, Instr (Stereo AudRate))
> doAssignP sfrost pmap = (Percussion, assignPercussion sfrost pmap)

define signal functions for playing instruments ===========================================

> assignInstrument       :: SFRoster → PerGMKey → Instr (Stereo AudRate)
> assignInstrument sfrost pergm dur pch vol params =
>   let
>     sig                :: AudSF () (Double, Double)
>                                          = constructSig sfrost
>                                                         (pWordF pergm, pWordI pergm)
>                                                         dur pch vol params
>   in proc _ → do
>     (zL, zR) ← sig ⤙ ()
>     outA ⤙ (zL, zR)
>
> constructSig           :: SFRoster
>                           → (Word, Word)
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → AudSF () (Double, Double)
> constructSig sfrost (zF, zI) dur pch vol params
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     zones              :: [(Word, SFZone)]
>                                          = fromJust $ Map.lookup (PerGMKey zF zI Nothing)
>                                                                  (zZoneCache sfrost)
>     ((zoneL, shdrL), (zoneR, shdrR))
>                        :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                                          = setZone sfrost (zF, zI) (tail zones) pch vol
>     (rDataL, rDataR)   :: (Reconciled, Reconciled)
>                                          = reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>     sr                 :: Double         = fromIntegral $ F.sampleRate shdrL
>     sig                :: AudSF () (Double, Double)
>     ok                 :: Bool           = checkReconcile
>                                              ((zoneL, shdrL), (zoneR, shdrR))
>                                              rDataL
>                                              rDataR
>     sffile             :: SFFile         = fileByIndex sfrost zF
>     arrays             :: SoundFontArrays= zArrays sffile 
>     sig = if not ok
>           then error "SFZone and F.Shdr could not be reconciled"
>           else eutSynthesize (rDataL, rDataR) sr dur pch vol params
>                              (ssData arrays) (ssM24 arrays)
>   in sig
>   where
>     msg = unwords ["constructSig ", show (zF, zI)]
>
> assignPercussion       :: SFRoster
>                           → [(PercussionSound, (Word, Word))]
>                           → Instr (Stereo AudRate)
> assignPercussion sfrost pmap dur pch vol params =
>   let
>     ps                 :: PercussionSound
>                                          = toEnum (pch - 35)
>     (wF, wI) = case lookup ps pmap of
>                Nothing   → error (   "Percussion does not have "
>                                      ++ show ps ++ " in the supplied pmap.")
>                Just x    → x
>     sig                :: AudSF () (Double, Double)
>                                          = constructSig sfrost (wF, wI) dur pch vol params
>   in proc _ → do
>     (zL, zR) ← sig ⤙ ()
>     outA ⤙ (zL, zR)

zone selection ============================================================================

> setZone                :: SFRoster
>                           → (Word, Word)
>                           → [(Word, SFZone)]
>                           → AbsPitch
>                           → Volume
>                           → ((SFZone, F.Shdr), (SFZone, F.Shdr))
> setZone sfrost (wF, wI) zones pch vol
>   | traceAlways msg False = undefined
>   | otherwise = ((zoneL, sampleL) ,(zoneR, sampleR))
>   where
>     zone = selectBestZone sfrost wF zones pch vol
>     (zoneL, zoneR) = selectLinkedZone sfrost (wF, wI) zones zone
>     sffile = fileByIndex sfrost wF
>     arrays = zArrays sffile
>     iinst = ssInsts arrays ! wI
>     sampleL = ssShdrs arrays ! fromJust (zSampleIndex zoneL)
>     sampleR = ssShdrs arrays ! fromJust (zSampleIndex zoneR)
>     msg = unwords [
>             "setZone (", show (wF, wI)
>             , " = ",     show (zFilename sffile)
>             , " , ",     show (zoneName sffile iinst zoneL)
>             , ")"]
>
> selectBestZone         :: SFRoster
>                           → Word
>                           → [(Word, SFZone)]
>                           → AbsPitch
>                           → Volume
>                           → SFZone
> selectBestZone sfrost zF zones pch vol =
>   let
>     sffile = fileByIndex sfrost zF
>     scores = mapMaybe (scoreOneZone sffile pch vol) zones
>     whichZ = if null scores
>              then error "no qualified zone"
>              else minimumBy compareScores scores
>   in
>     snd whichZ
>
> selectLinkedZone       :: SFRoster
>                           → (Word, Word)
>                           → [(Word, SFZone)]
>                           → SFZone
>                           → (SFZone, SFZone)
> selectLinkedZone sfrost (zF, zI) zones zone = (zoneL, zoneR)
>   where
>     sffile = fileByIndex sfrost zF
>     arrays = zArrays sffile 
>     sin = fromJust (zSampleIndex zone)
>     shdr = ssShdrs arrays ! sin
>     stype = F.sampleType shdr
>     slink = F.sampleLink shdr
>     mlinked = find (withslink slink) zones
>     -- TODO: should base it strictly on Stereo sample type?
>     ozone = case mlinked of
>             Nothing → zone
>             Just _  → snd $ fromJust $ find (withslink slink) zones
>     (zoneL, zoneR) = if stype == 2
>                      then (zone, ozone)
>                      else (ozone, zone)
>     withslink          :: Word → (Word, SFZone) → Bool
>     withslink tomatch (wZ, zone) =
>       let
>         sin = fromJust (zSampleIndex zone)
>       in
>         sin == tomatch
>
> compareScores          :: (Ord a) ⇒ (a, b) → (a, b) → Ordering
> compareScores (a1, b1) (a2, b2) = compare a1 a2 
>                         
> scoreOneZone           :: SFFile
>                           → AbsPitch
>                           → Volume
>                           → (Word, SFZone)
>                           → Maybe (Int, SFZone)
> scoreOneZone sffile pch vol dzone =
>     if qualify zone
>     then Just (score, zone)
>     else Nothing
>   where
>     qualify            :: SFZone → Bool
>     qualify z = DAllOn /= desireReStereo || isStereoZone sffile z
>
>     score = score1 + score2
>     zone = snd dzone
>
>     score1 = computePitchDistance pch (zKeyRange zone)
>     score2 = computeVolumeDistance vol (zVelRange zone)
>
> computePitchDistance   :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> computePitchDistance cand mrange =
>   case mrange of
>     Nothing                   → 1000
>     Just (rangeMin, rangeMax) → let
>                                   dist1 = abs $ cand - rangeMin
>                                   dist2 = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 100 * max dist1 dist2
>                                   else 1000 * min dist1 dist2
>
> computeVolumeDistance  :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> computeVolumeDistance cand mrange =
>   case mrange of
>     Nothing                   → 100
>     Just (rangeMin, rangeMax) → let
>                                   dist1 = abs $ cand - rangeMin
>                                   dist2 = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 10 * max dist1 dist2
>                                   else 100 * min dist1 dist2

reconcile zone and sample header ==========================================================

> sumOfMaybeInts         :: [Maybe Int] → Int
> sumOfMaybeInts = foldr ((+).fromMaybe 0) 0
>       
> addIntToWord           :: Word → Int → Word
> addIntToWord w i =
>   let iw               :: Int = fromIntegral w
>       sum              :: Int = iw + i
>   in fromIntegral sum
>
> reconcileLR            :: ((SFZone, F.Shdr), (SFZone, F.Shdr)) → (Reconciled, Reconciled)
> reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     recL = reconcile (zoneL, shdrL)
>     recR = reconcile (zoneR, shdrR)
>     recR' = recR{
>               rRootKey         = rRootKey recL
>             , rPitchCorrection = rPitchCorrection recL} 
>   in
>     (recL, recR')
>   where
>     msg = unwords ["reconcileLR zoneL=", show zoneL
>                  , ", shdrL=",           show shdrL
>                  , ", zoneR=",           show zoneR
>                  , ", shdrR=",            show shdrR]
>
> reconcile              :: (SFZone, F.Shdr) → Reconciled 
> reconcile (zone, shdr) =
>   Reconciled {
>     rSampleMode      = fromMaybe             (A.NoLoop)               (zSampleMode zone)
>   , rStart           = addIntToWord          (F.start shdr)           (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd             = addIntToWord          (F.end shdr)             (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart       = addIntToWord          (F.startLoop shdr)       (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd         = addIntToWord          (F.endLoop shdr)         (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey         = fromMaybe             (F.originalPitch shdr)   (zRootKey zone)
>   , rPitchCorrection = resolvePitchCorrection(F.pitchCorrection shdr) (zCoarseTune zone) (zFineTune zone)
>   , rEnvelope        = deriveEnvelope        (zDelayVolEnv   zone)
>                                              (zAttackVolEnv  zone)
>                                              (zHoldVolEnv    zone)
>                                              (zDecayVolEnv   zone)
>                                              (zSustainVolEnv zone)
>                                              (zReleaseVolEnv zone)
>   , rEffects         = deriveEffects         (zChorus zone)
>                                              (zReverb zone)
>                                              (zPan    zone)}
>
> checkReconcile         :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                           → Reconciled
>                           → Reconciled
>                           → Bool
> checkReconcile ((zoneL, shdrL), (zoneR, shdrR)) reconL reconR
>   | traceIf msg False = undefined
>   | otherwise = True
>   where
>     msg = unwords ["checkReconcile=", show shdrL
>                                     , show zoneL
>                                     , show reconL]
>
> data Hints =
>   DLow | DMed | DHigh deriving Show
>
> type SoundFontDatabase = [ (FilePath
>                             , ([Hints]
>                                , ([  (String, ([Hints], InstrumentName))]
>                                ,  [  (String, [(String, ([Hints], PercussionSound))])])))]
>
> printChoices           :: SFRoster → [InstrumentName] → [PercussionSound] → IO ()
> printChoices sfrost is ps = do
>   let istrings = map (showI sfrost) is
>   let pstrings = map (showP sfrost) ps
>   mapM_ putStrLn istrings
>   mapM_ putStrLn pstrings
>   where
>     showI              :: SFRoster → InstrumentName → String
>     showI sfrost iname =
>       let
>         (iloc, _) = zLocs sfrost
>         mpergm = Map.lookup iname iloc
>         result = if isJust mpergm
>                  then show iname ++ "/" ++ showPerGM sfrost (fromJust mpergm)
>                  else show iname ++ " not found"
>       in
>         result       
>
>     showP               :: SFRoster → PercussionSound → String
>     showP sfrost psound =
>       let
>         (_, ploc) = zLocs sfrost
>         mpergm = Map.lookup psound ploc
>         result = if isJust mpergm
>                  then show psound ++ "/" ++ showPerGM sfrost (fromJust mpergm)
>                  else show psound ++ " not found"
>       in
>         result
>
> showPerGM              :: SFRoster → PerGMScored → String
> showPerGM sfrost pergm =
>   let
>     pergm' = pPerGMKey pergm 
>     wFile = pWordF pergm'
>     wInst = pWordI pergm'
>     wZone = pWordZ pergm'
>
>     sffile = zFiles sfrost ! fromIntegral wFile
>     arrays = zArrays sffile
>     iinst = ssInsts arrays ! fromIntegral wInst
>     ishdr = ssShdrs arrays ! fromIntegral (fromJust wZone)
>     showZ = case wZone of
>             Nothing     → ""
>             Just zWord  → "/" ++ F.sampleName ishdr
>   in
>     fileName sfrost (pWordF pergm') ++ "/" ++ instrName sffile iinst ++ showZ
>
> digAll                 :: IO ()
> digAll = do
>   putStrLn "not yet implemented"
>
> isStereoZone           :: SFFile → SFZone → Bool
> isStereoZone sffile zone =    stype == fromSampleType SampleTypeLeft
>                               || stype == fromSampleType SampleTypeRight
>   where
>     arrays = zArrays sffile 
>     sin = fromJust (zSampleIndex zone)
>     shdr = ssShdrs arrays ! sin
>     stype              :: Word
>     stype = fromIntegral $ F.sampleType shdr
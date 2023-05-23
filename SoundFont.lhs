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
> import Control.Arrow
> import Control.Monad.Writer ( runWriter, MonadWriter(tell, writer), Writer )
> import qualified Data.Audio           as A
> import Data.Array.Unboxed ( array, Array, (!), IArray(bounds) )
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List ( find, foldr, minimumBy )
> import qualified Data.Map             as Map
> import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, AudSF, Mono, Stereo )
> import Euterpea.Music
> import Parthenopea ( traceIf, traceAlways )
> import Synthesizer
> import System.Environment(getArgs)  
  
importing sampled sound (from SoundFont (*.sf2) files) ====================================

> data PerGMInstr =
>   PerGMInstr {
>       pScore           :: Int
>     , pWordF           :: Word
>     , pWordI           :: Word
>     , pWordZ           :: Word} deriving Show
>
> type SFInstrLocator = Map.Map InstrumentName PerGMInstr
> type SFPercLocator  = Map.Map PercussionSound PerGMInstr
> type SFLocators     = (SFInstrLocator, SFPercLocator)
>
> type SFIdentityMap     = Map.Map String Word
>
> data SFRoster =
>   SFRoster {
>     zFiles             :: Array Int SFFile
>   , zMaps              :: SFLocators}
>
> data SFFile =
>   SFFile {
>     zFilename          :: String
>   , zArrays            :: SoundFontArrays
>   , zWordF             :: Word
>   , zScore             :: Int
>   , zInstrumentLookup  :: Map.Map String Word}
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
> instrumentName         :: SFFile → F.Inst → String
> instrumentName sffile = F.instName
>
> zoneName               :: SFFile → F.Inst → SFZone → String
> zoneName sffile iinst sfzone =
>   let
>     arrays = zArrays sffile
>   in
>     instrumentName sffile iinst
>     ++ "/"
>     ++ if isJust (zSampleIndex sfzone)
>        then F.sampleName (ssShdrs arrays ! fromJust (zSampleIndex sfzone))
>        else "Global"

slurp in instruments from SoundFont (*.sf2) files =========================================

> fileByIndex            :: SFRoster → Word → SFFile
> fileByIndex sfrost wFile = zFiles sfrost ! fromIntegral wFile
>
> doSoundFont            :: SoundFontDatabase → [(String, Music (Pitch, [NoteAttribute]))] → IO ()
> doSoundFont sfdb exposed =
>   do
>     let numberedDB = zip [0..] sfdb
>     sffilesp ← mapM (uncurry readSoundFontFile) numberedDB
>     let (iloc, ploc) = foldr chooseIAndP (Map.empty, Map.empty) sffilesp
>     let sfrost = SFRoster
>                    (array (0, length sffilesp - 1) (zip [0..] (map fst sffilesp)))
>                    (iloc, ploc)
>     let imap = assignInstruments sfrost iloc
>     let pmap = assignAllPercussion sfrost ploc
>     let imap' = imap ++ [doAssignP sfrost pmap]
>     _  ← doPlayInstruments imap' exposed
>     return ()
>  
> readSoundFontFile      :: Word
>                           → ( FilePath
>                             , (   [Hints]
>                               , (   [(String, ([Hints], InstrumentName))]
>                                 ,   [(String, [(String, ([Hints], PercussionSound))])])))
>                           → IO (SFFile, (   [(String, ([Hints], InstrumentName))]
>                                          ,  [(String, [(String, ([Hints], PercussionSound))])]))
> readSoundFontFile wFile (filename, (filehints, (ilist, plist))) =
>   do
>     putStrLn "entering readSoundFontFile"
>     putStrLn ("inFile=" ++ filename)
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
>         let sffile = SFFile filename arrays wFile (1000 * sfscore filehints) Map.empty
>         case ssM24 arrays of
>           Nothing          → print "16-bit datapoints"
>           Just s24data     → print "24-bit datapoints"
>         putStrLn "leaving readSoundFontFile"
>         return (sffile, (ilist, plist))
>
> doPlaySong             :: InstrMap (Stereo AudRate) → String → Music (Pitch, [NoteAttribute]) → IO ()
> doPlaySong imap name song
>   | traceAlways msg False = undefined
>   | otherwise = do
>       let (d,s) = renderSF song imap
>       putStrLn ("duration=" ++ show d ++ " seconds")
>       outFileNorm (name ++ ".wav") d s
>       return ()
>   where
>     msg = unwords ["doPlaySong ", name]
>
> doPlayInstruments      :: InstrMap (Stereo AudRate) → [(String, Music (Pitch, [NoteAttribute]))] → IO ()
> doPlayInstruments imap exposed
>   | traceAlways msg False = undefined
>   | otherwise = do
>       mapM_ (uncurry (doPlaySong imap)) exposed
>   where
>     msg = unwords ["doPlayInstruments ", show $ length imap
>                             , " insts=", concatMap (show . fst) imap]
  
extract data from SoundFont per instrument ================================================

> chooseI                  :: SFFile
>                           → [(String, ([Hints], InstrumentName))]
>                           → SFIdentityMap
>                           → SFLocators
>                           → SFLocators
> chooseI sffile []     _     (imap, pmap) = (imap, pmap)
> chooseI sffile (x:xs) idmap (imap, pmap) =
>   let
>     mwInstr            :: Maybe Word     = Map.lookup (fst x) idmap
>     imap'              :: SFInstrLocator =
>       if isNothing mwInstr
>       then imap
>       else
>         if isNothing mPrevious || myScore > pScore (fromJust mPrevious)
>         then Map.insert iname (PerGMInstr myScore (zWordF sffile) (fromJust mwInstr) 0) imap
>         else imap
>     iname              :: InstrumentName = (snd.snd) x
>     mPrevious          :: Maybe PerGMInstr
>                                          = Map.lookup ((snd.snd) x) imap
>     myScore            :: Int            = zScore sffile + 100 * sfscore ((fst.snd) x)
>   in
>     chooseI sffile xs idmap (imap', pmap)
>
> chooseP                  :: SFFile
>                           → [(String, [(String, ([Hints], PercussionSound))])]
>                           → SFIdentityMap
>                           → SFLocators
>                           → SFLocators
> chooseP sffile []     _     (imap, pmap) = (imap, pmap)
> chooseP sffile (x:xs) idmap (imap, pmap) =
>   let
>     mwInstr            :: Maybe Word     = Map.lookup (fst x) idmap
>     pmap'              :: SFPercLocator  =
>       if isNothing mwInstr
>       then pmap
>       else chooseSounds sffile (fromJust mwInstr) (snd x) pmap
>   in
>     chooseP sffile xs idmap (imap, pmap')
>   where
>     chooseSounds       :: SFFile
>                           → Word
>                           → [(String, ([Hints], PercussionSound))]
>                           → SFPercLocator
>                           → SFPercLocator
>     chooseSounds sffile wordI []     pmap = pmap
>     chooseSounds sffile wordI (x:xs) pmap =
>       let
>         myScore = zScore sffile + 10 * sfscore ((fst.snd) x)
>         pmap' = 
>           if isNothing mPrevious || myScore > pScore (fromJust mPrevious)
>           then Map.insert ((snd.snd) x) (PerGMInstr myScore (zWordF sffile) wordI 0) pmap
>           else pmap
>         mPrevious      :: Maybe PerGMInstr
>                                          = Map.lookup ((snd.snd) x) pmap
>       in
>         chooseSounds sffile wordI xs pmap'
>
> chooseIAndP            :: (SFFile, ([(String, ([Hints], InstrumentName))], [(String, [(String, ([Hints], PercussionSound))])]))
>                           → SFLocators
>                           → SFLocators
> chooseIAndP (sffile, (is, ps)) (imap, pmap)
>   | traceAlways msg False = undefined
>   | otherwise =
>   let
>     idmap = makeIdentityMap sffile
>     (imap', pmap')   = chooseI sffile is idmap (imap, pmap)
>     (imap'', pmap'') = chooseP sffile ps idmap (imap', pmap')
>   in
>     (imap'', pmap'')
>   where
>     msg = unwords ["chooseIAndP ", show ps]
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
> makeIdentityMap        :: SFFile → Map.Map String Word
> makeIdentityMap sffile =
>   let
>     arrays = zArrays sffile
>     is = ssInsts arrays
>     (ist, ien) = bounds is 
>     kvpairs = map kv [ist..ien-1]
>
>     kv                 :: Word → (String, Word)
>     kv inum =
>       let
>        iinst = is ! inum
>       in 
>        (F.instName iinst, inum)
>   in
>     Map.fromList kvpairs 
>     

prepare the specified instruments and percussion ==========================================

> getZones        :: SFRoster → (Word, Word) → [(Word, SFZone)]
> getZones sfrost (zF, zI) =
>   let
>     sffile             :: SFFile         = fileByIndex sfrost zF
>     wInst              :: Word           = zI
>     arrays = zArrays sffile
>     iinst  = ssInsts arrays ! wInst
>     jinst  = ssInsts arrays ! (wInst+1)
>     ibagi  = F.instBagNdx iinst
>     jbagi  = F.instBagNdx jinst
>     gIx    = if jbagi - ibagi < 2
>               then error "must have one global zone and at least one other zone"
>               else [ibagi]
>     oIx   = [ibagi+1..jbagi-1]
>     gList = map (buildZone sffile iinst defInstrumentZone)   gIx
>     oList = map (buildZone sffile iinst (snd (head gList)))  oIx
>   in gList ++ oList
>
> assignInstruments      :: SFRoster
>                           → Map.Map InstrumentName PerGMInstr
>                           → [(InstrumentName, Instr (Stereo AudRate))]
> assignInstruments sfrost = Map.foldrWithKey (ipal2imap sfrost) []
>
> ipal2imap              :: SFRoster
>                           → InstrumentName
>                           → PerGMInstr
>                           → [(InstrumentName, Instr (Stereo AudRate))]
>                           → [(InstrumentName, Instr (Stereo AudRate))]
> ipal2imap sfrost iname pergm accum = (iname, assignInstrument sfrost pergm) : accum
>
> assignAllPercussion    :: SFRoster
>                           → Map.Map PercussionSound PerGMInstr
>                           → [(PercussionSound, (Word, Word))]
> assignAllPercussion sfrost ppal
>   | traceAlways msg False = undefined
>   | otherwise = Map.foldrWithKey (ppal2pmap sfrost) [] ppal
>   where
>     msg = unwords ["assignAllPercussion ", show ppal]
>
> ppal2pmap              :: SFRoster
>                           → PercussionSound
>                           → PerGMInstr
>                           → [(PercussionSound, (Word, Word))]
>                           → [(PercussionSound, (Word, Word))]
> ppal2pmap sfrost psound pergm accum = (psound, (pWordF pergm, pWordI pergm)) : accum
>
> doAssignP              :: SFRoster
>                           → [(PercussionSound, (Word, Word))]
>                           → (InstrumentName, Instr (Stereo AudRate))
> doAssignP sfrost pmap
>   | traceAlways msg False = undefined
>   | otherwise = (Percussion, assignPercussion sfrost pmap)
>   where
>     msg = unwords ["doAssignP ", concatMap (show . fst) pmap]

define signal functions for playing instruments ===========================================

> assignInstrument       :: SFRoster → PerGMInstr → Instr (Stereo AudRate)
> assignInstrument sfrost pergm dur pch vol params =
>   let
>     sig                :: AudSF () (Double, Double)     = constructSig sfrost (pWordF pergm, pWordI pergm) dur pch vol params
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
>     zones              :: [(Word, SFZone)] = getZones sfrost (zF, zI)
>     ((zoneL, shdrL), (zoneR, shdrR))
>                        :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                                            = setZone sfrost (zF, zI) (tail zones) pch vol
>     (rDataL, rDataR)   :: (Reconciled, Reconciled)
>                                            = reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>     sr                 :: Double           = fromIntegral $ F.sampleRate shdrL
>     sig                :: AudSF () (Double, Double)
>     ok                 :: Bool             = checkReconcile
>                                                ((zoneL, shdrL), (zoneR, shdrR))
>                                                rDataL
>                                                rDataR
>     sffile             :: SFFile           = fileByIndex sfrost zF
>     arrays             :: SoundFontArrays  = zArrays sffile 
>     sig = if not ok
>           then error "SFZone and F.Shdr could not be reconciled"
>           else eutSynthesize (rDataL, rDataR) sr dur pch vol params (ssData arrays) (ssM24 arrays)
>   in sig
>   where
>     msg = unwords ["constructSig ", show (zF, zI)]
>
> assignPercussion       :: SFRoster
>                           → [(PercussionSound, (Word, Word))]
>                           → Instr (Stereo AudRate)
> assignPercussion sfrost pmap dur pch vol params
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     ps                 :: PercussionSound  = toEnum (pch - 35)
>     (wF, wI) = case lookup ps pmap of
>                Nothing   → error (   "Percussion does not have "
>                                      ++ show ps ++ " in the supplied pmap.")
>                Just x    → x
>     sig                :: AudSF () (Double, Double)     = constructSig sfrost (wF, wI) dur pch vol params
>   in proc _ → do
>     (zL, zR) ← sig ⤙ ()
>     outA ⤙ (zL, zR)
>   where
>     msg = unwords ["assignPercussion ", show pch, " ", show (pch - 35), "wI, wZ = ", show (lookup (toEnum (pch - 35)) pmap)]

zone selection ============================================================================

> setZone                :: SFRoster
>                           → (Word, Word)
>                           → [(Word, SFZone)]
>                           → AbsPitch
>                           → Volume
>                           → ((SFZone, F.Shdr), (SFZone, F.Shdr))
> setZone sfrost (wF, wI) zones pch vol
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     sffile = fileByIndex sfrost wF
>     arrays = zArrays sffile 
>     zone = selectBestZone sfrost zones pch vol
>     (zoneL, zoneR) = selectLinkedZone sfrost (wF, wI) zones zone
>   in
>     ((zoneL, ssShdrs arrays ! fromJust (zSampleIndex zoneL))
>     ,(zoneR, ssShdrs arrays ! fromJust (zSampleIndex zoneR)))
>   where
>     msg = unwords ["setZone ", show (wF, wI)]
>
> selectBestZone         :: SFRoster
>                           → [(Word, SFZone)]
>                           → AbsPitch
>                           → Volume
>                           → SFZone
> selectBestZone sfrost zones pch vol =
>   let
>     scores = map (scoreOneZone pch vol) zones
>   in
>     snd $ minimumBy compareScores scores
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
>     -- WOX should base it on sample type?
>     ozone = case mlinked of
>             Nothing → zone
>             Just _ → snd $ fromJust $ find (withslink slink) zones
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
> compareScores          :: (Num a, Ord a) ⇒ (a, b) → (a, b) → Ordering
> compareScores (a1, b1) (a2, b2) = compare a1 a2 
>                         
> scoreOneZone           :: AbsPitch → Volume → (Word, SFZone) → (Int, SFZone)
> scoreOneZone pch vol dzone = (score, zone)
>   where
>     zone = snd dzone
>     score = score1 + score2
>     score1 = computePitchDistance pch (zKeyRange zone)
>     score2 = computeVolumeDistance vol (zVelRange zone)
>
> computePitchDistance        :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
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
> computeVolumeDistance        :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
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
> sumOfMaybeInts = foldr ((+) . fromMaybe 0) 0
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
>     msg = unwords ["reconcileLR zoneL=", show zoneL, " shdrL=", show shdrL, ", zoneR=", show zoneR, " shdrR=", show shdrR]
>
> reconcile              :: (SFZone, F.Shdr) → Reconciled 
> reconcile (zone, shdr) =
>   Reconciled {
>     rStart           = addIntToWord          (F.start shdr)           0 -- WOX (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd             = addIntToWord          (F.end shdr)             0 -- WOX (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart       = addIntToWord          (F.startLoop shdr)       0 -- WOX (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd         = addIntToWord          (F.endLoop shdr)         0 -- WOX (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey         = fromMaybe             (F.originalPitch shdr)   (zRootKey zone)
>   , rPitchCorrection = resolvePitchCorrection(F.pitchCorrection shdr) (zCoarseTune zone) (zFineTune zone)
>   , rEnvelope        = deriveEnvelope        (zDelayVolEnv zone)
>                                              (zAttackVolEnv zone)
>                                              (zHoldVolEnv zone)
>                                              (zDecayVolEnv zone)
>                                              (zSustainVolEnv zone)
>                                              (zReleaseVolEnv zone)}
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
> {-
> newtype Writer w a = Writer { runWriter :: (a,w) }
>
> instance (Monoid w) => Monad (Writer w) where
>   return a             = Writer (a,mempty)
>   (Writer (a,w)) >>= f = let (a',w') = runWriter $ f a in Writer (a',w `mappend` w')
> -}
> logNumber :: Int → Writer [String] Int  
> logNumber x = writer (x, ["Got number: " ++ show x])  -- here
>
> logNumber2 :: Int → Writer [String] Int  
> logNumber2 x = do
>   tell ["Got number: " ++ show x]
>   return x
>
> multWithLog :: Writer [String] Int  
> multWithLog = do  
>   a <- logNumber2 3  
>   b <- logNumber2 5
>   tell ["multiplying " ++ show a ++ " and " ++ show b ]
>   return (a*b)
>
> zmain                  :: IO ()
> -- main = print $ runWriter multWithLog
> --        (15,["Got number: 3","Got number: 5","multiplying 3 and 5"])>   return (a*b)
> zmain = print $ runWriter multWithLog
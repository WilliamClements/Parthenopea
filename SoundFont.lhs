> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>

SoundFont support =========================================================================

> module SoundFont where
>
> import Cecil
> import qualified Codec.SoundFont      as F
> import Control.Arrow
> import Control.Monad.Writer ( runWriter, MonadWriter(tell, writer), Writer )
> import Covers
> import qualified Data.Audio           as A
> import Data.Array.Unboxed ( Array, (!), IArray(bounds) )
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List ( find, foldr, groupBy, minimumBy, sort, sortBy, sortOn )
> import Data.Maybe (isJust, fromJust, fromMaybe)
> import Debug.Trace ( traceIO, traceM )
> import Euterpea.IO.Audio.BasicSigFuns ( envASR, envLineSeg, filterLowPass )
> import Euterpea.IO.Audio.Basics ( apToHz, outA )
> import Euterpea.IO.Audio.IO ( outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, AudSF, Mono, Stereo )
> import Euterpea.Music
> import Fanfare
> import Parthenopea ( traceIf, traceAlways, pow )
> import Signals
> import SunPyg
> import Synthesizer
> import System.Environment(getArgs)  
  
importing sampled sound (from SoundFont (*.sf2) file) =====================================

> -- useFileIndex = 0 -- lofi
> useFileIndex = 1 -- hidef
> -- useFileIndex = 2 -- dsound
> -- useFileIndex = 3 -- essentials
>
> data SFFile =
>   SFFile {
>     zFilename          :: String
>   , zArrays            :: SoundFontArrays
>   , zWordF             :: Word
>   , zInstruments       :: [SFInstrument]}
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
> data SFInstrument =
>   SFInstrument {
>     zWordI             :: Word
>   , zZones             :: [(Word, SFZone)]} deriving Show
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
> instrumentName sffile iinst =
>   let
>     arrays = zArrays sffile
>   in
>     F.instName iinst
>
> instrumentName'         :: SFFile → SFInstrument → String
> instrumentName' sffile sfinst =
>   let
>     arrays = zArrays sffile
>     iinst = ssInsts arrays ! zWordI sfinst
>   in
>     instrumentName sffile iinst
>
> zoneNameI               :: SFFile → F.Inst → SFZone → String
> zoneNameI sffile iinst sfzone =
>   let
>     arrays = zArrays sffile
>   in
>     instrumentName sffile iinst
>     ++ "/"
>     ++ if isJust (zSampleIndex sfzone)
>        then F.sampleName (ssShdrs arrays ! fromJust (zSampleIndex sfzone))
>        else "Global"
>
> zoneNameI'              :: SFFile → SFInstrument → SFZone → String
> zoneNameI' sffile sfinst sfzone =
>   let
>     arrays = zArrays sffile
>     iinst = ssInsts arrays ! zWordI sfinst
>   in
>     instrumentName' sffile sfinst
>     ++ "/"
>     ++ if isJust (zSampleIndex sfzone)
>        then F.sampleName (ssShdrs arrays ! fromJust (zSampleIndex sfzone))
>        else "Global"

slurp in instruments from one SoundFont (*.sf2) file ======================================

> doSoundFont            :: SoundFontDatabase  → IO ()
> doSoundFont sfdb =
>   do
>     let curData = sfdb !! useFileIndex
>     let curFilename = fst curData
>     let curInst = (fst.snd.snd) curData
>     let curPerc = (snd.snd.snd) curData
>
>     putStrLn "entering doSoundFont"
>     putStrLn ("inFile=" ++ curFilename)
>     maybeAudio ← F.importFile curFilename
>     case maybeAudio of
>       Left s               → putStrLn $ "SoundFont decoding error: " ++ s
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
>         let sffile = SFFile curFilename arrays 0 []
>         case ssM24 arrays of
>           Nothing      → print "16-bit datapoints"
>           Just s24data → print "24-bit datapoints"
>         instruments ← buildInstruments sffile
>         let sffile' = sffile {zInstruments = instruments}
>         imap ← assignInstruments   sffile' curInst
>         pmap ← assignAllPercussion sffile' curPerc
>         let imap' = imap ++ [doAssignP sffile' pmap]
>         _  ← doPlayInstruments imap'
>         return ()
>     putStrLn "leaving doSoundFont"
>
> doPlayInstruments      :: InstrMap (Stereo AudRate) → IO ()
> doPlayInstruments imap
>   | traceAlways msg False = undefined
>   | otherwise = do
>       let (d,s) = renderSF basicLick imap
>       putStrLn ("duration=" ++ show d ++ " seconds")
>       outFileNorm "blaat.wav" d s
>       return ()
>   where
>     msg = unwords ["doPlayInstruments ", show $ length imap
>                             , " insts=", concatMap (show . fst) imap]
  
extract data from SoundFont per instrument ================================================

> buildInstruments       :: SFFile → IO [SFInstrument]
> buildInstruments sffile = do
>   let arrays = zArrays sffile
>   let is = ssInsts arrays
>   let (ist, ien) = bounds is 
>   putStrLn ("instrument start/stop=" ++ show (ist, ien)
>          ++ "\nfirst instrument name=" ++ show (F.instName (is!ist)))
>   let ilist = map (buildInstrument sffile) [ist..ien-1]
>   return ilist
>
> buildInstrument        :: SFFile → Word → SFInstrument
> buildInstrument sffile w =
>   let
>     arrays = zArrays sffile
>     iinst  = ssInsts arrays ! w
>     jinst  = ssInsts arrays ! (w+1)
>     ibagi  = F.instBagNdx iinst
>     jbagi  = F.instBagNdx jinst
>     gIx    = if jbagi - ibagi < 2
>               then error "must have one global zone and at least one other zone"
>               else [ibagi]
>     oIx   = [ibagi+1..jbagi-1]
>     gList = map (buildZone sffile iinst defInstrumentZone)   gIx
>     oList = map (buildZone sffile iinst (snd (head gList)))  oIx
>     iList = gList ++ oList
>   in SFInstrument w iList
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
>     zone = checkBuiltZone sffile iinst $ fromGens fromZone gens
>   in (bagIndex, zone)
>   where
>     checkBuiltZone     :: SFFile → F.Inst → SFZone → SFZone
>     checkBuiltZone sffile iinst zone
>       | traceIf msg False = undefined
>       | otherwise = zone
>        where 
>          msg = unwords [
>            "checkBuiltZone ", show (zoneNameI sffile iinst zone)
>           , " ", show $ hasRejects sffile iinst zone]
>     hasRejects         :: SFFile → F.Inst → SFZone → String
>     hasRejects sffile iinst zone =
>       let
>         suspiciousOnes = [
>                             ("Chorus",             zChorus zone)
>                           , ("Reverb",             zReverb zone)
>                           , ("Pan",                zPan zone)
>
>                           , ("ModLfoToPitch",      zModLfoToPitch zone)
>                           , ("VibLfoToPitch",      zVibLfoToPitch zone)
>                           , ("ModEnvToPitch",      zModEnvToPitch zone)
>                           , ("InitFc",             zInitFc zone)
>                           , ("InitQ",              zInitQ zone)
>                           , ("ModLfoToFc",         zModLfoToFc zone)
>                           , ("ModEnvToFc",         zModEnvToFc zone)
>                           , ("ModLfoToVol",        zModLfoToVol zone)
>                           , ("DelayModLfo",        zDelayModLfo zone)
>                           , ("FreqModLfo",         zFreqModLfo zone)
>                           , ("DelayVibLfo",        zDelayVibLfo zone)
>                           , ("FreqVibLfo",         zFreqVibLfo zone)
>                           , ("DelayModEnv",        zDelayModEnv zone)
>                           , ("AttackModEnv",       zAttackModEnv zone)
>                           , ("HoldModEnv",         zHoldModEnv zone)
>                           , ("DecayModEnv",        zDecayModEnv zone)
>                           , ("SustainModEnv",      zSustainModEnv zone)
>                           , ("ReleaseModEnv",      zReleaseModEnv zone)
>                           , ("KeyToModEnvHold",    zKeyToModEnvHold zone)
>                           , ("KeyToModEnvDecay",   zKeyToModEnvDecay zone)
>                           , ("KeyToVolEnvHold",    zKeyToVolEnvHold zone)
>                           , ("KeyToVolEnvDecay",   zKeyToVolEnvDecay zone)
>                          ]
>         agg = concatMap showIf suspiciousOnes
>         showIf a = if isJust (snd a) then show a else ""
>        in agg
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

prepare the specified instruments and percussion ==========================================

> getSFInstrument        :: SFFile
>                           → Word
>                           → SFInstrument
> getSFInstrument sffile w = fromJust $ find (\i → w == zWordI i) sfis
>   where
>     sfis   = zInstruments sffile
>
> assignInstruments      :: SFFile
>                           → [(String, ([Hints], InstrumentName))]
>                           → IO [(InstrumentName, Instr (Stereo AudRate))]
> assignInstruments sffile ipal = do
>   let arrays = zArrays sffile
>   let sfis   = zInstruments sffile
>   let is = ssInsts arrays
>   let selectedI = map (shouldAssignI is ipal) sfis
>   let filteredI = filter isJust selectedI
>   let readyI = map (doAssignI sffile) filteredI
>
>   putStrLn ("SFFile "                    ++ zFilename sffile
>          ++ ": loaded "                  ++ show (length readyI)
>          ++ " (mismatches = "            ++ show (length ipal - length filteredI)
>          ++ ") from total of "           ++ show (length is)
>          ++ " Instruments")
>          
>   return readyI 
>
> assignAllPercussion    :: SFFile
>                           → [(String, [(String, ([Hints], PercussionSound))])]
>                           → IO [(PercussionSound, (Word, Word))]
> assignAllPercussion sffile ppal = do
>   let sfis        = zInstruments sffile
>   let countSought = sum $ map (length.snd) ppal
>   let withDupes   = sortOn (fst.snd) $ concatMap (shouldAssignP sffile) ppal
>   let readyP      = map head (groupBy areSame withDupes)
>
>   putStrLn ("SFFile "                    ++ zFilename sffile
>          ++ ": loaded "                  ++ show (length readyP)
>          ++ " (mismatches = "            ++ show (countSought - length readyP)
>          ++ ") percussion sounds from "  ++ show (length ppal)
>          ++ " (mismatches = "            ++ show (length ppal - length (groupBy haveSameInst (sortOn (fst.snd) readyP )))
>          ++ ") instruments")
>
>   return readyP
>
>   where
>     areSame          :: (PercussionSound, (Word, Word)) → (PercussionSound, (Word, Word)) → Bool
>     areSame x y = (fst x == fst y) && (fst.snd) x == (fst.snd) y
>     haveSameInst     :: (PercussionSound, (Word, Word)) → (PercussionSound, (Word, Word)) → Bool
>     haveSameInst x y = (fst.snd) x == (fst.snd) y
>
> shouldAssignI          :: Array Word F.Inst 
>                           → [(String, ([Hints], InstrumentName))]
>                           → SFInstrument
>                           → Maybe (InstrumentName, Word)
> shouldAssignI is ipal sfinst =
>   let
>     w = zWordI sfinst
>     iinst = is ! w
>     ilist = filter (match (F.instName iinst)) ipal
>   in case ilist of
>     [] → Nothing
>     _  → Just (snd (snd (head ilist)), w)
>   where
>     match :: String → (String, ([Hints], InstrumentName)) → Bool
>     match iname cand = iname == fst cand
>
> shouldAssignP          :: SFFile
>                           → (String, [(String, ([Hints], PercussionSound))])
>                           → [(PercussionSound, (Word, Word))]
> shouldAssignP sffile pentry =
>   let
>     arrays = zArrays    sffile 
>     sfis = zInstruments sffile
>     -- what is the name of target instrument? and is there a match versus ppal??
>     target = fst pentry
>     maybePresent = find (matchzp arrays target) sfis
>   in case maybePresent of
>      Nothing     → []
>      Just sfinst → concatMap (shouldAssignZone sffile sfinst (snd pentry)) (tail (zZones sfinst))
>   where
>     matchzp            :: SoundFontArrays → String → SFInstrument → Bool
>     matchzp arrays target sfinst = target == itag
>       where
>         itag = F.instName (ssInsts arrays ! zWordI sfinst)
> 
> shouldAssignZone       :: SFFile
>                           → SFInstrument
>                           → [(String, ([Hints], PercussionSound))]
>                           → (Word, SFZone)
>                           → [(PercussionSound, (Word, Word))]
> shouldAssignZone sffile sfinst plist (wZ, zone)
>   | traceIf msg False = undefined
>   | otherwise = result
>   where
>     arrays = zArrays sffile
>     wI = zWordI sfinst
>     sampleIndex = zSampleIndex zone
>     zname = F.sampleName (ssShdrs arrays ! fromJust sampleIndex)
>     mFound = lookup zname plist
>     result = case mFound of
>              Nothing      → []
>              Just matched → [(snd matched, (wI, wZ))]
>     msg = unwords ["plist=", show plist, "zname=", show zname, " mFound=", show mFound]
>
> doAssignI              :: SFFile
>                           → Maybe (InstrumentName, Word)
>                           → (InstrumentName, Instr (Stereo AudRate))
> doAssignI sffile mis = (iname, assignInstrument sffile sfinst Nothing)
>   where 
>     (iname, w) = fromJust mis
>     sfinst = getSFInstrument sffile w
>
> doAssignP              :: SFFile
>                           → [(PercussionSound, (Word, Word))]
>                           → (InstrumentName, Instr (Stereo AudRate))
> doAssignP sffile pmap = (Percussion, assignPercussion sffile pmap)

define signal functions for playing instruments ===========================================

> assignInstrument       :: SFFile → SFInstrument → Maybe (Word, Word) → Instr (Stereo AudRate)
> assignInstrument sffile sfinst mww dur pch vol params =
>   let
>     sig                :: AudSF () (Double, Double)     = constructSig sffile sfinst mww dur pch vol params
>   in proc _ → do
>     (zL, zR) ← sig ⤙ ()
>     outA ⤙ (zL, zR)
>
> constructSig           :: SFFile
>                           → SFInstrument
>                           → Maybe (Word, Word)
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → AudSF () (Double, Double)
> constructSig sffile sfinst mww dur pch vol params =
>   let
>     arrays             :: SoundFontArrays  = zArrays sffile 
>     ((zoneL, shdrL), (zoneR, shdrR))
>                        :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                                            = setZone sffile sfinst mww pch vol
>     (rDataL, rDataR)   :: (Reconciled, Reconciled)
>                                            = reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>     sr                 :: Double           = fromIntegral $ F.sampleRate shdrL
>     sig                :: AudSF () (Double, Double)
>
>     ok = checkReconcile ((zoneL, shdrL), (zoneR, shdrR)) rDataL rDataR
>     sig = if not ok
>           then error "SFZone and F.Shdr could not be reconciled"
>           else eutSynthesize (rDataL, rDataR) sr dur pch vol params (ssData arrays) (ssM24 arrays)
>   in sig
>
> assignPercussion       :: SFFile
>                           → [(PercussionSound, (Word, Word))]
>                           → Instr (Stereo AudRate)
> assignPercussion sffile pmap dur pch vol params
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     ps :: PercussionSound
>     ps = toEnum (pch - 35)
>     (wI, wZ) = case lookup ps pmap of
>                Nothing       → error (   "Percussion does not have "
>                                          ++ show ps ++ " in the supplied pmap.")
>                Just x → x
>     sfinst = getSFInstrument sffile wI
>     sig                :: AudSF () (Double, Double)     = constructSig sffile sfinst (Just (wI, wZ)) dur pch vol params
>   in proc _ → do
>     (zL, zR) ← sig ⤙ ()
>     outA ⤙ (zL, zR)
>   where
>     msg = unwords ["assignPercussion ", show pch, " ", show (pch - 35), "wI, wZ = ", show (lookup (toEnum (pch - 35)) pmap)]

zone selection ============================================================================

> getZone                :: SFInstrument → Word → SFZone
> getZone sfinst w = 
>   case lookup w (zZones sfinst) of
>     Just i → i
>     Nothing → error "Instrument in supplied InstrMap does not have specified zone."
>
> setZone                :: SFFile
>                           → SFInstrument
>                           → Maybe (Word, Word)
>                           → AbsPitch
>                           → Volume
>                           → ((SFZone, F.Shdr), (SFZone, F.Shdr))
> setZone sffile sfinst mww pch vol =
>   let
>     arrays = zArrays sffile 
>     zone = selectBestZone sffile sfinst pch vol
>     (zoneL, zoneR) = selectLinkedZone sffile sfinst zone
>   in
>     ((zoneL, ssShdrs arrays ! fromJust (zSampleIndex zoneL))
>     ,(zoneR, ssShdrs arrays ! fromJust (zSampleIndex zoneR)))
>
> selectBestZone         :: SFFile
>                           → SFInstrument
>                           → AbsPitch
>                           → Volume
>                           → SFZone
> selectBestZone sffile sfinst pch vol =
>   let
>     -- skip the Global zone as we usually do
>     zones = map snd (tail (zZones sfinst))
>   in
>     selectBestZone' sffile zones pch vol 
>     
> selectBestZone'        :: SFFile
>                           → [SFZone]
>                           → AbsPitch
>                           → Volume
>                           → SFZone
> selectBestZone' sffile zones pch vol =
>   let
>     scores = map (scoreOneZone pch vol) zones
>   in
>     snd $ minimumBy compareScores scores
>
> selectLinkedZone       :: SFFile
>                           → SFInstrument
>                           → SFZone
>                           → (SFZone, SFZone)
> selectLinkedZone sffile sfinst zone = (zoneL, zoneR)
>   where
>     arrays = zArrays sffile 
>     sin = fromJust (zSampleIndex zone)
>     shdr = ssShdrs arrays ! sin
>     stype = F.sampleType shdr
>     slink = if stype == 2 || stype == 4
>             then F.sampleLink shdr
>             else error "sample must be L or R"
>     -- skip the Global zone as we usually do
>     mzone = find (matchPair sffile sfinst sin) (tail (zZones sfinst))
>     (zoneL, zoneR) =
>       case mzone of
>         Nothing           → (zone, zone)
>         Just (w, ozone)   → if stype == 2
>                             then (zone, ozone)
>                             else (ozone, zone)
>     matchPair          :: SFFile → SFInstrument → Word → (Word, SFZone) → Bool
>     matchPair sffile sfinst sin (w, ezone) = mylink == sin
>       where
>         arrays = zArrays sffile 
>         sin' = fromJust (zSampleIndex ezone)
>         shdr = ssShdrs arrays ! sin'
>         stype = F.sampleType shdr
>         mylink = if stype == 2 || stype == 4
>                  then F.sampleLink shdr
>                  else 0
>
> compareScores          :: (Num a, Ord a) ⇒ (a, b) → (a, b) → Ordering
> compareScores (a1, b1) (a2, b2) = compare a1 a2 
>                         
> scoreOneZone           :: AbsPitch → Volume → SFZone → (Int, SFZone)
> scoreOneZone pch vol zone = (score, zone)
>   where
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

>
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
>               rRootKey = rRootKey recL
>             , rPitchCorrection = rPitchCorrection recL} 
>   in
>     (recL, recR')
>   where
>     msg = unwords ["reconcileLR zoneL=", show zoneL, " shdrL=", show shdrL, ", zoneR=", show zoneR, " shdrR=", show shdrR]
>
> reconcile              :: (SFZone, F.Shdr) → Reconciled 
> reconcile (zone, shdr) =
>   Reconciled {
>     rStart           = addIntToWord          (F.start shdr)           0 -- (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd             = addIntToWord          (F.end shdr)             0 -- (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart       = addIntToWord          (F.startLoop shdr)       0 -- (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd         = addIntToWord          (F.endLoop shdr)         0 -- (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey         = fromMaybe             (F.originalPitch shdr)   (zRootKey zone)
>   , rPitchCorrection = resolvePitchCorrection(F.pitchCorrection shdr) (zCoarseTune zone) (zFineTune zone)
>   , rEnvelope        = deriveEnvelope        (zDelayVolEnv zone)
>                                              (zAttackVolEnv zone)
>                                              (zHoldVolEnv zone)
>                                              (zDecayVolEnv zone)
>                                              (zSustainVolEnv zone)
>                                              (zReleaseVolEnv zone)}
>
> checkSampling         :: Double → Double → Double → Double → Bool
> checkSampling  ns secs ns' secs'
>   | traceAlways msg False = undefined
>   | otherwise = True
>   where
>     msg = unwords ["ns=", show ns, " secs=", show secs, " ns'=", show ns', " secs'=", show secs' ]
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
> main :: IO ()
> -- main = print $ runWriter multWithLog
> --        (15,["Got number: 3","Got number: 5","multiplying 3 and 5"])>   return (a*b)
> main = print $ runWriter multWithLog
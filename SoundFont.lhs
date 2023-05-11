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
> import Control.Arrow.ArrowP ( ArrowP(ArrowP), strip )
> import Control.Monad.Writer
> import qualified Control.SF.SF        as C
> import Covers
> import Data.Array.Unboxed ( Array, (!), IArray(bounds) )
> import qualified Data.Audio           as A
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
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA )
> import FRP.UISF.UISF
> import Parthenopea ( traceIf, traceAlways, envDAHdSR, pow )
> import Signals
> import SunPyg
> import System.Environment(getArgs)  
  
importing sampled sound (from SoundFont (*.sf2) file) =====================================

> useEnvelopes       = False
> useLowPassFilter   = False
> usePitchCorrection = True
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
>   , zChorus            :: Maybe Int
>   , zReverb            :: Maybe Int
>   , zPan               :: Maybe Int
>
>   , zRootKey           :: Maybe Word
>
>   , zModLfoToPitch     :: Maybe Int
>   , zVibLfoToPitch     :: Maybe Int
>   , zModEnvToPitch     :: Maybe Int
>   , zInitFc            :: Maybe Int
>   , zInitQ             :: Maybe Int
>   , zModLfoToFc        :: Maybe Int
>   , zModEnvToFc        :: Maybe Int
>   , zModLfoToVol       :: Maybe Int
>   , zDelayModLfo       :: Maybe Int
>   , zFreqModLfo        :: Maybe Int
>   , zDelayVibLfo       :: Maybe Int
>   , zFreqVibLfo        :: Maybe Int
>   , zDelayModEnv       :: Maybe Int
>   , zAttackModEnv      :: Maybe Int
>   , zHoldModEnv        :: Maybe Int
>   , zDecayModEnv       :: Maybe Int
>   , zSustainModEnv     :: Maybe Int
>   , zReleaseModEnv     :: Maybe Int
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
> data Reconciled =
>   Reconciled {
>     rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: Word
>   , rPitchCorrection   :: Double
>   , rEnvelope          :: Envelope} deriving (Eq, Show)
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
> data Envelope =
>   Envelope {
>     eDelayT            :: Double
>   , eAttackT           :: Double
>   , eHoldT             :: Double
>   , eDecayT            :: Double
>   , eSustainLevel      :: Double
>   , eReleaseT          :: Double} deriving (Eq, Show)
>
  
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
>       let (d,s) = renderSF whelpNarp imap
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
>     zone = checkBuiltZone $ fromGens fromZone gens
>   in (bagIndex, zone)
>   where
>     checkBuiltZone     :: SFZone → SFZone
>     checkBuiltZone zone
>       | traceAlways msg False = undefined
>       | otherwise = zone
>        where 
>          msg = unwords ["checkBuiltZone ", hasRejects zone]
>     hasRejects         :: SFZone → String
>     hasRejects zone =
>       let
>         suspiciousOnes = [
>                             ("Chorus",          zChorus zone)
>                           , ("Reverb",          zReverb zone)
>                           , ("Pan",             zPan zone)
>
>                           , ("ModLfoToPitch", zModLfoToPitch zone)
>                           , ("VibLfoToPitch", zVibLfoToPitch zone)
>                           , ("ModEnvToPitch", zModEnvToPitch zone)
>                           , ("InitFc",        zInitFc zone)
>                           , ("InitQ",         zInitQ zone)
>                           , ("ModLfoToFc",    zModLfoToFc zone)
>                           , ("ModEnvToFc",    zModEnvToFc zone)
>                           , ("ModLfoToVol",   zModLfoToVol zone)
>                           , ("DelayModLfo",   zDelayModLfo zone)
>                           , ("FreqModLfo",    zFreqModLfo zone)
>                           , ("DelayVibLfo",   zDelayVibLfo zone)
>                           , ("FreqVibLfo",    zFreqVibLfo zone)
>                           , ("DelayModEnv",   zDelayModEnv zone)
>                           , ("AttackModEnv",  zAttackModEnv zone)
>                           , ("HoldModEnv",    zHoldModEnv zone)
>                           , ("DecayModEnv",   zDecayModEnv zone)
>                           , ("SustainModEnv", zSustainModEnv zone)
>                           , ("ReleaseModEnv", zReleaseModEnv zone)
>                           , ("KeyToModEnvHold", zKeyToModEnvHold zone)
>                           , ("KeyToModEnvDecay", zKeyToModEnvDecay zone)
>                           , ("KeyToVolEnvHold", zKeyToVolEnvHold zone)
>                           , ("KeyToVolEnvDecay", zKeyToVolEnvDecay zone)
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

> eutPhaserNormal        :: Double
>                           → Double
>                           → AudSF () Double 
> eutPhaserNormal iphs delta =
>   let frac             :: RealFrac r ⇒ r → r
>       frac                           = snd . properFraction
>   in proc () → do
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> eutPhaserLooping       :: Double
>                           → Double
>                           → (Double, Double)
>                           → AudSF () Double 
> eutPhaserLooping iphs delta (st, en) =
>   let frac             :: RealFrac r ⇒ r → r
>       frac                           = snd . properFraction
>   in proc () → do
>     rec
>       let phase = if next > en then frac next + st else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> eutPhaser              :: Reconciled
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → (Double, Double)
>                           → AudSF () Double 
> eutPhaser recon secsSample secsTotal sr iphs freqFactor (st, en) =
>   let frac             :: RealFrac r ⇒ r → r
>       frac                               = snd . properFraction
>       delta            :: Double         = 1 / (secsSample * freqFactor * sr)
>   in proc () → do
>     env1   ← envLineSeg  [0, 1, 0] 
>                           [0, secsSample]               ⤙ ()
>     env2   ← envLineSeg  [0, 1, 1] 
>                           [secsSample, secsTotal]       ⤙ ()
>     pnorm  ← eutPhaserNormal 0 delta                    ⤙ ()
>     ploop  ← eutPhaserLooping st delta (normalizeLooping recon)
>                                                         ⤙ ()
>     outA ⤙ env1*pnorm + env2*ploop
>
> eutRelayStereo         :: A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Double
>                           → (Reconciled, Reconciled)
>                           → Volume
>                           → Dur
>                           → AudSF Double (Double, Double)
> eutRelayStereo s16 ms8 stime (rDataL, rDataR) vol dur
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     (stL, enL)         :: (Word, Word)     = (rStart rDataL, rEnd rDataL)
>     (stR, enR)         :: (Word, Word)     = (rStart rDataR, rEnd rDataR)
>     numS               :: Double           = fromIntegral (enL - stL + 1)
>     amp                :: Double           = fromIntegral vol / 100
>
>   in proc pos → do
>     let saddrL         :: Int              = fromIntegral stL + truncate (numS * pos)
>     let saddrR         :: Int              = fromIntegral stR + truncate (numS * pos)
>     let (a1L, a1R) =
>              if isJust ms8
>              then (compute24 (s16 ! saddrL) (fromJust ms8 ! saddrL)
>                  , compute24 (s16 ! saddrR) (fromJust ms8 ! saddrR))
>              else (fromIntegral (s16 ! saddrL)
>                  , fromIntegral (s16 ! saddrR))
>     rec
>       aenvL ← if useEnvelopes
>               then doEnvelope (rEnvelope rDataL) stime ⤙ ()
>               else constA 1                            ⤙ ()
>       aenvR ← if useEnvelopes
>               then doEnvelope (rEnvelope rDataR) stime ⤙ ()
>               else constA 1                            ⤙ ()
>       a2L   ← if useLowPassFilter
>               then filterLowPass         ⤙ (a1L,20000*aenvL)
>               else delay 0 ⤙ a1L
>       a2R   ← if useLowPassFilter
>               then filterLowPass         ⤙ (a1R,20000*aenvR)
>               else delay 0 ⤙ a1R
>     outA ⤙ (a2L*amp*aenvL, a2R*amp*aenvR)
>
>   where
>     msg = unwords ["eutRelayStereo=", show rDataL, "<-L...", show (rDataL == rDataR), "...R->", show rDataR]
>     compute24          :: Int16 → Int8 → Double
>     compute24 i16 i8 = d24
>       where
>         f8to32         :: Int8 → Int32
>         f8to32 = fromIntegral
>         f16to32         :: Int16 → Int32
>         f16to32 = fromIntegral
>         d24            :: Double
>         d24 = fromIntegral (f16to32 i16 * 32768 + f8to32 i8)       
>      
>     doEnvelope         :: Envelope → Double → AudSF () Double
>     doEnvelope renv secs
>       | traceIf msg False = undefined
>       | otherwise = envDAHdSR secs
>                               (eDelayT       renv)
>                               (eAttackT      renv)
>                               (eHoldT        renv)
>                               (eDecayT       renv)
>                               (eSustainLevel renv)
>                               (eReleaseT     renv)
>       where
>         msg = unwords ["Envelope=", show renv]
>
>
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
>     ns                 :: Double           = fromIntegral $ rEnd rDataL - rStart rDataL + 1
>     secsSamplePoints   :: Double           = ns / sr
>     secsTotal          :: Double           = 2 * fromRational dur
>     ap                 :: AbsPitch
>
>     ok = checkReconcile ((zoneL, shdrL), (zoneR, shdrR)) rDataL rDataR secsSamplePoints
>     ap = if not ok
>          then error "SFZone and F.Shdr could not be reconciled"
>          else fromIntegral (rRootKey rDataL)
>
>     (nst, nen)         :: (Double, Double)    = (fromIntegral $ rStart     rDataL, fromIntegral $ rEnd rDataL)
>     (pst, pen)         :: (Double, Double)    = (fromIntegral $ rLoopStart rDataL, fromIntegral $ rLoopEnd rDataL)
>
>     ns'                :: Double              = nst - nen + 1
>     secs'              :: Double              = ns' / sr
>     freqFactor         :: Double              = if usePitchCorrection
>                                                 then freqRatio * rateRatio / rPitchCorrection rDataL
>                                                 else freqRatio * rateRatio
>     freqRatio          :: Double              = apToHz ap / apToHz pch
>     rateRatio          :: Double              = 44100 / sr
>     sig                :: AudSF () (Double, Double)
>                                               = eutPhaser rDataL secsSamplePoints secsTotal sr 0 freqFactor (pst, pen)
>                                             >>> eutRelayStereo (ssData arrays) (ssM24 arrays)
>                                                                secs'
>                                                                (rDataL, rDataR)
>                                                                vol dur
>   in sig
>
> assignPercussion       :: SFFile
>                           → [(PercussionSound, (Word, Word))]
>                           → Instr (Stereo AudRate)
> assignPercussion sffile pmap dur pch vol params =
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
>

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
>     score = score1 + score2 + score3
>     score1 = computeDistance pch (zKeyRange zone)
>     score2 = computeDistance vol (zVelRange zone)
>     anInt              :: Int = if isJust (zRootKey zone)
>                                 then fromIntegral $ fromJust $ zRootKey zone
>                                 else 0
>     score3 = if isJust (zRootKey zone)
>              then min 128 $ 10 * abs (anInt - pch)
>              else 128
>
> computeDistance        :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> computeDistance cand mrange =
>   case mrange of
>     Nothing                   → 128
>     Just (rangeMin, rangeMax) → if cand >= rangeMin && cand <= rangeMax
>                                 then 0
>                                 else min (abs $ rangeMin - cand) (abs $ cand - rangeMax)

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
> reconcileLR ((zoneL, shdrL), (zoneR, shdrR)) =
>   let
>     recL = reconcile (zoneL, shdrL)
>     recR = reconcile (zoneR, shdrR)
>     recR' = recR{
>               rRootKey = rRootKey recL
>             , rPitchCorrection = rPitchCorrection recL} 
>   in
>     (recL, recR')
>
> reconcile              :: (SFZone, F.Shdr) → Reconciled 
> reconcile (zone, shdr) =
>   Reconciled {
>     rStart           = addIntToWord          (F.start shdr)           (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd             = addIntToWord          (F.end shdr)             (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart       = addIntToWord          (F.startLoop shdr)       (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd         = addIntToWord          (F.endLoop shdr)         (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey         = fromMaybe             (F.originalPitch shdr)   (zRootKey zone)
>   , rPitchCorrection = resolvePitchCorrection(F.pitchCorrection shdr) (zCoarseTune zone) (zFineTune zone)
>   , rEnvelope        = deriveEnvelope        (zDelayVolEnv zone)
>                                              (zAttackVolEnv zone)
>                                              (zHoldVolEnv zone)
>                                              (zDecayVolEnv zone)
>                                              (zSustainVolEnv zone)
>                                              (zReleaseVolEnv zone)}
>
> normalizeLooping       :: Reconciled → (Double, Double)
> normalizeLooping recon =
>   let
>     nst                :: Double = fromIntegral $ rStart recon 
>     nen                :: Double = fromIntegral $ rEnd recon 
>     pst                :: Double = fromIntegral $ rLoopStart recon 
>     pen                :: Double = fromIntegral $ rLoopEnd recon 
>     len                :: Double = nen - nst + 1
>     mst                :: Double = (pst - nst) / len
>     men                :: Double = (pen - nen) / len
>   in
>     (mst, men)
>     
> resolvePitchCorrection :: Int → Maybe Int → Maybe Int → Double
> resolvePitchCorrection alt mps mpc = 2 ** (fromIntegral cents/12/100)
>   where
>     cents              :: Int = if isJust mps || isJust mpc
>                                 then coarse * 100 + fine
>                                 else alt
>     coarse             :: Int = fromMaybe 0 mps
>     fine               :: Int = fromMaybe 0 mpc
>
> deriveEnvelope         :: Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Envelope
> deriveEnvelope mDelay mAttack mHold mDecay mSustain mRelease
>   | traceIf msg False = undefined
>   | otherwise =
>     let
>       iDelay =   fromMaybe (-12000) mDelay
>       iAttack =  fromMaybe (-12000) mAttack
>       iHold   =  fromMaybe (-12000) mHold
>       iDecay  =  fromMaybe (-12000) mDecay
>       iSustain = fromMaybe        0 mSustain
>       iRelease = fromMaybe (-12000) mRelease
>     in 
>       Envelope (conv iDelay) (conv iAttack)           (conv iHold)
>                (conv iDecay) (acceptSustain iSustain) (conv iRelease)
>     where
>       conv             :: Int → Double
>       conv k = 2**(fromIntegral k/1200)
>       msg = unwords ["mDelay=",   show mDelay,   "mAttack=",    show mAttack
>                     ,"mHold=",    show mHold,    "mDecay=",     show mDecay
>                     ,"mSustain=", show mSustain, "mRelease=",   show mRelease]
>
> acceptSustain          :: Int → Double
> acceptSustain iS = 1/raw iS
>   where
>     raw                :: Int → Double
>     raw iS = pow 10 (fromIntegral jS/200)
>       where jS
>               | iS <= 0 = 0
>               | iS >= 1000 = 1000
>               | otherwise = iS
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
>                           → Double
>                           → Bool
> checkReconcile ((zoneL, shdrL), (zoneR, shdrR)) reconL reconR secs
>   | traceIf msg False = undefined
>   | otherwise = True
>   where
>     msg = unwords ["checkReconcile=", show shdrL
>                                     , show zoneL
>                                     , show reconL
>                                     , show secs]
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
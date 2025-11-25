> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

Runtime
William Clements
February 1, 2025

> module Parthenopea.SoundFont.Runtime ( implementNoteBending
>                                      , prepareRuntime
>                                      , SFRuntime(..)) where
>
> import qualified Codec.SoundFont         as F
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Foldable
> import Data.IntMap.Strict ( IntMap )
> import qualified Data.IntMap.Strict as IntMap
> import Data.IntSet (IntSet )
> import qualified Data.IntSet as IntSet
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Set (Set)
> import qualified Data.Set                as Set
> import qualified Data.Vector.Strict      as VB
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Render ( Instr )
> import Euterpea.IO.Audio.Types ( AudRate, Stereo, Clock, Signal )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Envelopes ( deriveEnvelope )
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing
> import Parthenopea.Repro.Synthesizer ( deriveEffects, eutSynthesize )
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
  
> data SFRuntime                           =
>   SFRuntime {
>     zDirectives        :: Directives
>   , zRoster            :: ([InstrumentName], [PercussionSound])
>   , zRuntimeFiles      :: IntMap SFFileRuntime         {- [FileIndex → SFFileRuntime]   -}
>   , zInstrumentMap     :: [(InstrumentName, Instr (Stereo AudRate))]}
> instance Show SFRuntime where
>   show runt                 =
>     unwords ["SFRuntime", show (length runt.zRuntimeFiles, length runt.zInstrumentMap)]

cache SoundFont data that is only needed for Runtime ==================================================================

> prepareRuntime     :: Directives
>                       → ([InstrumentName], [PercussionSound]) 
>                       → VB.Vector SFFileBoot
>                       → Map PerGMKey PerInstrument
>                       → ( Map InstrumentName (Bool, Maybe PerGMKey, [Emission])
>                         , Map PercussionSound (Bool, Maybe PerGMKey, [Emission]))
>                       → IO SFRuntime
> prepareRuntime dives rost vFilesBoot cache choices
>                                          = do
>   let runtimeFiles                       = IntMap.fromList $ mapMaybe supply (VB.toList vFilesBoot)
>   let prerunt                            =
>         SFRuntime
>           dives
>           rost
>           runtimeFiles
>           []
>   instrumentMap                          ← prepareInstruments prerunt choices
>   return prerunt{zInstrumentMap = instrumentMap}
>   where
>     actions            :: IntMap IntSet                {- [FileIndex → [InstIndex]]     -}
>     actions                              =
>       let
>         extract        :: Map a (Bool, Maybe PerGMKey, [Emission]) → Set PerGMKey
>         extract m                        = Set.fromList $ foldl' buildUp [] m
>           where
>             buildUp    :: [PerGMKey] → (Bool, Maybe PerGMKey, [Emission]) → [PerGMKey]
>             buildUp s (_, mpergm, _)     =
>               case mpergm of
>                 Nothing                  → s
>                 Just pergm               → pergm : s
>
>         selectI m pergm                  =
>           IntMap.insertWith IntSet.union pergm.pgkwFile ((IntSet.singleton . fromIntegral) pergm.pgkwInst) m
>       in
>         foldl' selectI IntMap.empty (extract (fst choices) `Set.union` extract (snd choices))
>
>     supply             :: SFFileBoot → Maybe (Int, SFFileRuntime)
>     supply sffile                        = probe >>= savePerInstruments
>       where
>         fName                            = "supply"
>
>         probe                            = sffile.zWordFBoot `IntMap.lookup` actions
>         savePerInstruments insts         = 
>           let
>             getPerI inst                 =
>               cache Map.! PerGMKey sffile.zWordFBoot (fromIntegral inst) Nothing
>
>             newpi                        = IntMap.fromSet getPerI insts
>
>             savePreZones
>                        :: IntMap PreZone → Int → IntMap PreZone
>             savePreZones m inst          =
>               let
>                 wrapUp pz                = pz{pzRecon = Just $ resolvePreZone dives pz}
>                 save                     = wrapUp . accessPreZone fName sffile.zPreZones
>                 bixen                    = (newpi IntMap.! inst).pBixen
>               in
>                 m `IntMap.union` IntMap.fromSet save bixen
>
>             preZone                      = IntSet.foldl' savePreZones IntMap.empty insts
>           in
>             Just ( sffile.zWordFBoot
>                  , SFFileRuntime 
>                      sffile.zWordFBoot
>                      newpi
>                      preZone
>                      sffile.zSquirrelSample)

define signal functions and instrument maps to support rendering ======================================================

> prepareInstruments     :: SFRuntime
>                           → ( Map InstrumentName (Bool, Maybe PerGMKey, [Emission])
>                             , Map PercussionSound (Bool, Maybe PerGMKey, [Emission]))
>                           → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments runt choices          = do
>     return $ (Percussion, assignPercussion)                                                               : imap
>   where
>     (zI, zP)                             = BF.bimap middle middle choices
>                                              where middle = Map.mapMaybe (\(_, pp, _) → pp)
>     imap                                 = Map.foldrWithKey imapFolder [] zI
>     pmap                                 = Map.foldrWithKey pmapFolder [] zP
>
>     imapFolder kind _ target             = (kind, assignInstrument (zI Map.! kind))                       : target
>     pmapFolder kind _ target             = (kind, (pergm.pgkwFile, pergm.pgkwInst))                       : target
>                                              where pergm = zP Map.! kind
>
>     assignInstrument   :: ∀ p . Clock p ⇒ PerGMKey → Instr (Stereo p)
>     assignInstrument pergm durI pch vol params
>                                          =
>       proc _ → do
>         (zL, zR)                         ← instrumentSF runt pergm durI pch vol params ⤙ ()
>         outA                             ⤙ (zL, zR)
>
>     assignPercussion   :: ∀ p . Clock p ⇒ Instr (Stereo p)
>     assignPercussion pDur pch vol ps     = assignInstrument pergm pDur pch vol ps
>       where
>         pergm                            = PerGMKey wF wI Nothing
>         (wF, wI)                         =
>           case lookup kind pmap of
>             Nothing    → error $ unwords ["Percussion does not have", show kind, "in the supplied pmap."]
>             Just x     → x
>         kind           :: PercussionSound
>         kind                             =
>           let
>             percNum                      = pch - 35
>             spread                       = fromEnum OpenTriangle - fromEnum AcousticBassDrum + 1
>           in
>             profess
>               (inZRange percNum spread)
>               (unwords ["assignPercussion illegal pch", show pch])
>               (toEnum percNum)
>
> instrumentSF           :: ∀ p . Clock p ⇒
>                           SFRuntime
>                           → PerGMKey
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → Signal p () (Double, Double)
> instrumentSF runt pergm durI pchIn volIn ps_
>   | traceIf trace_ISF False              = undefined
>   | otherwise                            =
>   eutSynthesize synthSwitches (reconX, mreconX) noonOut reconX.rSampleRate durI sffile 
>   where
>     fName_                               = "instrumentSF"
>     trace_ISF                            =
>       unwords [fName_, show (pergm.pgkwFile, pergm.pgkwInst)
>                      , show perI.piChanges.cnName, show (pchIn, volIn), show durI, show ps]
>
>     Directives{ .. }
>                                          = runt.zDirectives
>
>     sffile                               = runt.zRuntimeFiles IntMap.! pergm.pgkwFile
>     perI                                 = sffile.zPerInstrument IntMap.! fromIntegral pergm.pgkwInst
>
>     ps                                   = VU.fromList ps_
>     noonIn                               = carefulNoteOn volIn pchIn
>     fly                                  = eyeOnTheFly noonIn
>     noonOut                              = case fly of
>                                              Left z                     → calcNoteOn z.pzSFZone
>                                              Right (z, _)               → calcNoteOn z.pzSFZone
>       where
>         calcNoteOn z                     = NoteOn (maybe (clip (0, 127) volIn) fromIntegral z.zVel) 
>                                                   (maybe (clip (0, 127) pchIn) fromIntegral z.zKey)
>
>     (reconX, mreconX)                    =
>       case fly of
>         Left pz                          → (receiveRecon pz, Nothing)
>         Right (pzL, pzR)                 → (reconL, Just $ copyRoot reconL reconR)
>           where
>             reconL                       = receiveRecon pzL
>             reconR                       = receiveRecon pzR
>
>             copyRoot pz1 pz2             = pz2{rRootKey                   = pz1.rRootKey
>                                              , rPitchCorrection           = pz1.rPitchCorrection}

zone selection for rendering ==========================================================================================

>     eyeOnTheFly        :: NoteOn → Either PreZone (PreZone, PreZone)
>     eyeOnTheFly noonFly
>       | traceIf trace_DFE False          = undefined
>       | null smashSpaces                 = error $ unwords [fName, "smashup", show smashup, "has no subspaces"]
>       | spL < 0 || cntL <= 0 || spR < 0 || cntR <= 0
>                                          = error $ unwords [fName, show noonFly, "cell contains nonsense"
>                                                                  , show ((spL, cntL), (spR, cntR))
>                                                                  , show smashStats]
>       | isNothing foundL || isNothing foundR
>                                          = error $ unwords [fName, "spL, spR"
>                                                                  , show (spL, spR)
>                                                                  , "not both present in"
>                                                                  , show perI.pBixen] 
>       | foundL == foundR                 = (Left . fromJust) foundL
>       | otherwise                        = Right (fromJust foundL, fromJust foundR)
>       where
>         fName                            = unwords [fName_, "eyeOnTheFly"]
>         trace_DFE                        = unwords [fName, show (spL, spR), smashTag]
>
>         smashup@Smashing{ .. }
>                                          = perI.pSmashing
>         (index1, index2)                 = noonAsCoords noonFly
>
>         ((spL, cntL), (spR, cntR))       = if hackWildJumps
>                                              then useTwoLookupCells
>                                              else useOneGetLeafCells
>           where
>             useTwoLookupCells            = (lookupCell index1 smashup, lookupCell index2 smashup)
>             useOneGetLeafCells           =
>               let
>                 vu                       = getLeafCells index1 smashup
>                 lvu                      = VU.length vu
>               in
>                 profess
>                   (lvu == 2)
>                   (error $ unwords [fName, "Leaf dimension", show lvu, "not two!?!"])
>                   (vu VU.! 0, vu VU.! 1)
>         foundL                           = fromIntegral spL `IntMap.lookup` sffile.zPreZone
>         foundR                           = fromIntegral spR `IntMap.lookup` sffile.zPreZone

reconcile zone and sample header ======================================================================================

> receiveRecon           :: PreZone → Recon
> receiveRecon pz                          = deJust "receiveRecon" pz.pzRecon
>
> resolvePreZone         :: Directives → PreZone → Recon
> resolvePreZone dives pz                  = reconL
>   where
>     switches                             = dives.synthSwitches
>     zd                                   = pz.pzDigest
>     z                                    = pz.pzSFZone
>     shdr                                 = effPZShdr pz
>     
>     m8n                                  = resolveModulation z
>
>     reconL                               =
>       Recon
>         (fromMaybe A.NoLoop z.zSampleMode)
>         (fromIntegral shdr.sampleRate)
>
>         (AppliedLimits
>                     ((+) shdr.start (fromIntegral zd.zdStart))
>                     ((+) shdr.end (fromIntegral zd.zdEnd))
>                     ((+) shdr.startLoop (fromIntegral zd.zdStartLoop))
>                     ((+) shdr.endLoop (fromIntegral zd.zdEndLoop)))
>         
>         (fromIntegral $ fromMaybe shdr.originalPitch z.zRootKey)
>         (fromMaybe 100 z.zScaleTuning)
>         VB.empty
>         (reconAttenuation z.zInitAtten)
>         (deriveEnvelope z.zDelayVolEnv z.zAttackVolEnv z.zHoldVolEnv z.zDecayVolEnv z.zSustainVolEnv Nothing)                         
>         (if switches.usePitchCorrection
>            then Just $ reconPitchCorrection shdr.pitchCorrection z.zCoarseTune z.zFineTune
>            else Nothing)
>         m8n
>         (deriveEffects switches m8n z.zChorus z.zReverb z.zPan)
>
>     reconPitchCorrection
>                        :: Int → Maybe Int → Maybe Int → Double
>     reconPitchCorrection sub mps mpc     = fromMaybe ((fromCents . fromIntegral) sub) (fromCents' mps mpc)
>
>     reconAttenuation   :: Maybe Int → Double
>     reconAttenuation _     {- WOX -}       = if switches.useAttenuation
>                                              then maybe 0 fromIntegral z.zInitAtten
>                                              else 0.0
>
> implementNoteBending   :: NoteOn → SFZone → Double → Double → SFZone
> implementNoteBending noon zone bend secs = zone'
>   where
>     zone'                                =
>       zone{  zModEnvToPitch = (Just . round) (bend * 100)
>            , zDelayModEnv   = Nothing
>            , zAttackModEnv  = Just $ toTimecents secs - 1
>            , zHoldModEnv    = Nothing
>            , zDecayModEnv   = Nothing
>            , zSustainModEnv = Just 0
>            , zKey           = (Just . fromIntegral) noon.noteOnKey
>            , zReleaseModEnv = Nothing}
>
> resolveModulation      :: SFZone → Modulation
> resolveModulation z                      = resolveMods m8n z.zModulators defaultMods
>   where
>     m8n                :: Modulation     =
>       defModulation{
>         mLowpass                         = Lowpass resonanceType curKernelSpec
>       , mModEnv                          = nModEnv
>       , mModLfo                          = nModLfo
>       , mVibLfo                          = nVibLfo
>       , toPitchCo                        = summarize ToPitch
>       , toFilterFcCo                     = summarize ToFilterFc
>       , toVolumeCo                       = summarize ToVolume}
>
>     curKernelSpec                        =
>       KernelSpec
>         (fromMaybe 13_500 z.zInitFc)
>         (fromMaybe 0 z.zInitQ)
>         1 
>         useFastFourier
>         (-1) -- must always be replaced
>
>     resonanceType      :: ResonanceType  = ResonanceSVF
>     nModEnv            :: Maybe FEnvelope
>     nModEnv                              =   deriveEnvelope
>                                              z.zDelayModEnv
>                                              z.zAttackModEnv
>                                              z.zHoldModEnv
>                                              z.zDecayModEnv
>                                              z.zSustainModEnv
>                                              (Just (z.zModEnvToPitch, z.zModEnvToFc))
>     nModLfo, nVibLfo   :: Maybe LFO
>     nModLfo                              =
>       deriveLFO z.zDelayModLfo z.zFreqModLfo z.zModLfoToPitch z.zModLfoToFc z.zModLfoToVol
>     nVibLfo            :: Maybe LFO      =
>       deriveLFO z.zDelayVibLfo z.zFreqVibLfo z.zVibLfoToPitch Nothing Nothing
>
>     summarize          :: ModDestType → ModCoefficients
>     summarize toWhich                    =
>       ModCoefficients
>         (coAccess toWhich $ maybe defModTriple (fromJust . fModTriple) nModEnv)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nModLfo)
>         (coAccess toWhich $ maybe defModTriple lfoModTriple nVibLfo)

The End
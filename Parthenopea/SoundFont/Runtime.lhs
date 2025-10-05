> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
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
> import Data.Foldable
> import Data.IntMap (IntMap)
> import qualified Data.IntMap.Strict as IntMap
> import Data.IntSet (IntSet)
> import qualified Data.IntSet as IntSet
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Set (Set)
> import qualified Data.Set                as Set
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Render ( Instr )
> import Euterpea.IO.Audio.Types ( AudRate, Stereo, Clock, Signal )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Envelopes ( deriveEnvelope )
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Smashing ( lookupCellIndex )
> import Parthenopea.Repro.Synthesizer
> import Parthenopea.SoundFont.SFSpec
  
executive =============================================================================================================

> data SFRuntime                           =
>   SFRuntime {
>     zBootFiles         :: VB.Vector SFFileBoot
>   , zRuntimeFiles      :: VB.Vector SFFileRuntime
>   , zChoicesI          :: Map InstrumentName (Bool, Maybe PerGMKey, [Emission])
>   , zChoicesP          :: Map PercussionSound (Bool, Maybe PerGMKey, [Emission])
>   , zInstrumentCache   :: Map PerGMKey PerInstrument
>   , zInstrumentMap     :: [(InstrumentName, Instr (Stereo AudRate))]}
> instance Show SFRuntime where
>   show runt                 =
>     unwords ["SFRuntime", show (length runt.zBootFiles, length runt.zInstrumentMap)]

define signal functions and instrument maps to support rendering ======================================================

> prepareRuntime     :: SFRuntime → IO SFRuntime
> prepareRuntime runt                      = do
>   runtimeFiles                           ← VB.mapM supply runt.zRuntimeFiles
>   instrumentMap                          ← prepareInstruments (runt{zRuntimeFiles = runtimeFiles})
>   return runt{zRuntimeFiles = runtimeFiles, zInstrumentMap = instrumentMap}
>   where
>     allInsts           :: IntMap IntSet
>     allInsts                             =
>       let
>         extract        :: Map a (Bool, Maybe PerGMKey, [Emission]) → Set PerGMKey
>         extract                          = foldl' buildUp Set.empty
>         buildUp s (_, mpergm, _)         = s `Set.union` (Set.singleton . fromJust) mpergm 
>
>         allPergms                        = extract runt.zChoicesI `Set.union` extract runt.zChoicesP
>
>         selectI m pergm                  =
>           IntMap.insertWith IntSet.union pergm.pgkwFile ((IntSet.singleton . fromIntegral) pergm.pgkwInst) m
>       in
>         foldl' selectI IntMap.empty allPergms
>
>     supply sffile                        =
>       let
>         populate insts                   = sffile{zPerInstrument = newpr, zPreZone = preZone newpr insts}
>           where
>             newpr                        = perInstrument insts
>
>         perInstrument                    = IntMap.fromSet getPerI
>           where
>             getPerI inst                 =
>               runt.zInstrumentCache Map.! PerGMKey sffile.zWordFRuntime (fromIntegral inst) Nothing
>
>         preZone newpr                    = IntSet.foldl' (grow newpr) IntMap.empty
>
>         grow newpr m inst                =
>           let
>             toPzPair   :: PreZone → (Int, PreZone)
>             toPzPair pz                  = (fromIntegral pz.pzWordB, pz)
>           in
>             m `IntMap.union` IntMap.fromList (map toPzPair (newpr IntMap.! inst).pZones)
>       in
>         return $ maybe sffile populate (sffile.zWordFRuntime `IntMap.lookup` allInsts)
>
> prepareInstruments     :: SFRuntime → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments runt                  = do
>     return $ (Percussion, assignPercussion)                                                               : imap
>   where
>     imap                                 = Map.foldrWithKey imapFolder [] runt.zChoicesI
>     pmap                                 = Map.foldrWithKey pmapFolder [] runt.zChoicesP
>
>     imapFolder kind _ target             = (kind, assignInstrument pergm)                                 : target
>       where
>        (_, pPerGMKey, _)                 = runt.zChoicesI Map.! kind
>        pergm                             = fromJust pPerGMKey
>     pmapFolder kind _ target             = (kind, (pgkwFile pergm, pgkwInst pergm))                       : target
>       where
>        (_, pPerGMKey, _)                 = runt.zChoicesP Map.! kind
>        pergm                             = fromJust pPerGMKey
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
>         kind                             = toEnum (pch - 35)
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
>   | otherwise                            = eutSynthesize (reconX, mreconX) noonOut reconX.rSampleRate durI sffile 
>   where
>     fName_                               = "instrumentSF"
>     trace_ISF                            =
>       unwords [fName_, show (pergm.pgkwFile, pergm.pgkwInst)
>                      , show perI.piChanges.cnName, show (pchIn, volIn), show durI, show ps]
>
>     ps                                   = VB.fromList ps_
>     noonIn                               = carefulNoteOn volIn pchIn
>     fly                                  = doFlyEye noonIn
>     noonOut                              = case fly of
>                                              Left z                     → calcNoteOn z.pzSFZone
>                                              Right (z, _)               → calcNoteOn z.pzSFZone
>       where
>         calcNoteOn z                     = NoteOn (maybe (clip (0, 127) volIn) fromIntegral z.zVel) 
>                                                   (maybe (clip (0, 127) pchIn) fromIntegral z.zKey)
>
>     sffile                               = runt.zRuntimeFiles VB.! pergm.pgkwFile
>     perI                                 = sffile.zPerInstrument IntMap.! fromIntegral pergm.pgkwInst
>
>     (reconX, mreconX)                    =
>       case fly of
>         Left pz                          → (resolvePreZone pz, Nothing)
>         Right (pzL, pzR)                 → (reconL, Just $ copyRoot reconL reconR)
>           where
>             reconL                       = resolvePreZone pzL
>             reconR                       = resolvePreZone pzR
>
>             copyRoot   :: Recon → Recon → Recon
>             copyRoot pz1 pz2             = pz2{rRootKey                   = pz1.rRootKey
>                                              , rPitchCorrection           = pz1.rPitchCorrection}

zone selection for rendering ==========================================================================================

>     doFlyEye           :: NoteOn → Either PreZone (PreZone, PreZone)
>     doFlyEye noonFly
>       | traceIf trace_DFE False          = undefined
>       | bagIdL <= 0 || cntL <= 0 || bagIdR <= 0 || cntR <= 0
>                                          = error $ unwords [fName, "cell is nonsense"]
>       | isNothing foundL || isNothing foundR
>                                          =
>         error $ unwords [fName
>                        , "zone"
>                        , show (bagIdL, bagIdR)
>                        , "not both present in"
>                        , show (map pzWordB perI.pZones)] 
>       | foundL == foundR                 = (Left . fromJust) foundL
>       | otherwise                        = Right (fromJust foundL, fromJust foundR)
>       where
>         fName                            = unwords [fName_, "doFlyEye"]
>         trace_DFE                        = unwords [fName, show (bagIdL, bagIdR), show perI.pSmashing]
>
>         (index1, index2)                 = noonAsCoords noonFly
>         (bagIdL, cntL)                   = lookupCellIndex index1 perI.pSmashing
>         (bagIdR, cntR)                   = lookupCellIndex index2 perI.pSmashing
>         foundL                           = fromIntegral bagIdL `IntMap.lookup` sffile.zPreZone
>         foundR                           = fromIntegral bagIdR `IntMap.lookup` sffile.zPreZone

reconcile zone and sample header ======================================================================================

> resolvePreZone         :: PreZone → Recon
> resolvePreZone pz                        = reconL
>   where
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
>         ((+) shdr.start (fromIntegral zd.zdStart))
>         ((+) shdr.end (fromIntegral zd.zdEnd))
>         ((+) shdr.startLoop (fromIntegral zd.zdStartLoop))
>         ((+) shdr.endLoop (fromIntegral zd.zdEndLoop))
>
>         (fromIntegral $ fromMaybe shdr.originalPitch z.zRootKey)
>         (fromMaybe 100 z.zScaleTuning)
>         VB.empty
>         (reconAttenuation z.zInitAtten)
>         (deriveEnvelope z.zDelayVolEnv z.zAttackVolEnv z.zHoldVolEnv z.zDecayVolEnv z.zSustainVolEnv Nothing)                         
>         (if usePitchCorrection
>            then Just $ reconPitchCorrection shdr.pitchCorrection z.zCoarseTune z.zFineTune
>            else Nothing)
>         m8n
>         (deriveEffects m8n z.zChorus z.zReverb z.zPan)
>
>     reconPitchCorrection
>                        :: Int → Maybe Int → Maybe Int → Double
>     reconPitchCorrection sub mps mpc     = fromMaybe ((fromCents . fromIntegral) sub) (fromCents' mps mpc)
>
>     reconAttenuation   :: Maybe Int → Double
>     reconAttenuation _     {- WOX -}       = if useAttenuation
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
> resolveModulation z
>   | traceNot trace_RM False              = undefined
>   | otherwise                            = resolveMods m8n z.zModulators defaultMods
>   where
>     fName                                = "resolveModulation"
>     trace_RM                             = unwords [fName, show z]
>
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
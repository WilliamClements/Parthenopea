> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Zone
William Clements
February 15, 2026

> module Parthenopea.Repro.Zone
>        ( AppliedLimits(..)
>        , ChangeEar(..)
>        , ChangeEarItem(..)
>        , defApplied
>        , defZone
>        , formDigest
>        , makeMono
>        , makePreZone
>        , normalizeLooping
>        , okGMRanges
>        , PreZone(..)
>        , PreZoneKey(..)
>        , receiveRecon
>        , Recon(..)
>        , resolvePreZone
>        , SFZone(..)
>        , wasSwitchedToMono
>        , wordS, wordI, wordB
>        , ZoneDigest(..)
>        ) where
>
> import qualified Codec.SoundFont         as F
> import qualified Data.Audio              as A
> import Data.Maybe
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Envelopes ( deriveEnvelope )
> import Parthenopea.Repro.Modulation
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Utility
>
> data PreZone                             =
>   PreZone {
>     pzWordF            :: !Int
>   , pzWordS            :: !Word
>   , pzWordI            :: !Word
>   , pzWordB            :: !Word
>   , pzDigest           :: ZoneDigest
>   , pzSFZone           :: SFZone
>   , pzChanges          :: ChangeEar F.Shdr
>   , pzRecon            :: Maybe Recon}
> instance Show PreZone where
>   show pz                                =
>     unwords ["PreZone", show (pz.pzWordF, pz.pzWordS, pz.pzWordI, pz.pzWordB), show pz.pzDigest]
>
> extractZoneKey         :: PreZone → PreZoneKey
> extractZoneKey pz                        =
>   PreZoneKey pz.pzWordF pz.pzWordI pz.pzWordB pz.pzWordS
>
> showBad                :: PreZone → String
> showBad pz                               = show (pz.pzWordB, (pz.pzDigest.zdKeyRange, pz.pzDigest.zdVelRange))
>
> makePreZone            :: Int → Word → Word → ZoneDigest → F.Shdr → PreZone
> makePreZone wF wI bix digest shdr        =
>   PreZone
>     wF (fromJust digest.zdSampleIndex) wI bix 
>      digest defZone (ChangeEar shdr [])
>      Nothing
>
> wordS, wordI, wordB    :: PreZone → Int
> wordS pz                                 = fromIntegral pz.pzWordS
> wordI pz                                 = fromIntegral pz.pzWordI
> wordB pz                                 = fromIntegral pz.pzWordB
> makeMono               :: PreZone → PreZone
> makeMono pz@PreZone{pzChanges}           = pz{pzChanges = pzChanges{ceChanges = MakeMono : pzChanges.ceChanges}}
> wasSwitchedToMono      :: PreZone → Bool
> wasSwitchedToMono PreZone{pzChanges}     = MakeMono `elem` pzChanges.ceChanges
> showPreZones           :: [PreZone] → String
> showPreZones pzs                         = show $ map pzWordB pzs
>
> data ZoneDigest                          =
>   ZoneDigest {
>     zdKeyRange         :: Maybe (Word, Word)
>   , zdVelRange         :: Maybe (Word, Word)
>   , zdPan              :: Maybe Int
>   , zdSampleIndex      :: Maybe Word
>   , zdSampleMode       :: Maybe A.SampleMode
>   , zdStart            :: !Int
>   , zdEnd              :: !Int
>   , zdStartLoop        :: !Int
>   , zdEndLoop          :: !Int} deriving (Eq, Show)
> defDigest              :: ZoneDigest
> defDigest                                = ZoneDigest Nothing Nothing Nothing Nothing Nothing 0 0 0 0
> formDigest             :: [F.Generator] → ZoneDigest
> formDigest                               = foldr inspectGen defDigest
>   where
>     inspectGen         :: F.Generator → ZoneDigest → ZoneDigest 
>     inspectGen (F.KeyRange i j)                          zd
>                                          = zd {zdKeyRange = Just(i, j)}
>     inspectGen (F.VelRange i j)                          zd
>                                          = zd {zdVelRange = Just(i, j)}
>     inspectGen (F.Pan i)                                 zd
>                                          = zd {zdPan = Just i}
>     inspectGen (F.SampleIndex w)                         zd
>                                          = zd {zdSampleIndex = Just w}
>     inspectGen (F.SampleMode m)                          zd
>                                          = zd {zdSampleMode = Just m}
>
>     inspectGen (F.StartAddressCoarseOffset i)            zd
>                                          = zd {zdStart = zd.zdStart + 32_768 * i}
>     inspectGen (F.StartAddressOffset i)                  zd
>                                          = zd {zdStart = zd.zdStart + i}
>     inspectGen (F.EndAddressCoarseOffset i)              zd
>                                          = zd {zdEnd = zd.zdEnd + 32_768 * i}
>     inspectGen (F.EndAddressOffset i)                    zd
>                                          = zd {zdEnd = zd.zdEnd + i}
>
>     inspectGen (F.LoopStartAddressCoarseOffset i)        zd
>                                          = zd {zdStartLoop = zd.zdStartLoop + 32_768 * i}
>     inspectGen (F.LoopStartAddressOffset i)              zd
>                                          = zd {zdStartLoop = zd.zdStartLoop + i}
>     inspectGen (F.LoopEndAddressCoarseOffset i)          zd
>                                          = zd {zdEndLoop = zd.zdEndLoop + 32_768 * i}
>     inspectGen (F.LoopEndAddressOffset i)                zd
>                                          = zd {zdEndLoop = zd.zdEndLoop + i}
>
>     inspectGen _ zd                      = zd
>
> okGMRanges             :: ZoneDigest → Bool
> okGMRanges zd                            = rOk && iOk
>   where
>     infinite                             = (0, qMidiWord128 - 1)
>
>     kLim                                 = fromMaybe infinite zd.zdKeyRange
>     vLim                                 = fromMaybe infinite zd.zdVelRange
>
>     rOk                                  = okRange kLim && okRange vLim
>     okRange (j, k)                       = (0 <= j) && j <= k && k < qMidiWord128
>
>     iOk = kLim /= infinite || vLim /= infinite
>
> data PreZoneKey                          =
>   PreZoneKey {
>     pzkwFile           :: !Int
>   , pzkwInst           :: !Word
>   , pzkwBag            :: !Word
>   , pzkwSampleIndex    :: !Word}
>   deriving (Eq, Ord, Show)
>
> data AppliedLimits                       =
>   AppliedLimits {
>     rStart             :: !Word
>   , rEnd               :: !Word
>   , rLoopStart         :: !Word
>   , rLoopEnd           :: !Word}
>   deriving (Eq, Show)
> defApplied             :: AppliedLimits
> defApplied                               = AppliedLimits 0 0 0 0
>
> data Recon                               =
>   Recon {
>     rSampleMode        :: !A.SampleMode
>   , rSampleRate        :: !Double
>   , rApplied           :: AppliedLimits
>   , rRootKey           :: !AbsPitch
>   , rTuning            :: !Int
>   , rAttenuation       :: !Double
>   , rVolEnv            :: Maybe FEnvelope
>   , rPitchCorrection   :: Maybe Double
>   , rM8n               :: Modulation
>   , rEffects           :: Maybe Effects}
>   deriving Eq
> normalizeLooping       :: Recon → (Double, Double)
> normalizeLooping Recon{ .. }
>                                          = ((loopst - fullst) / denom, (loopen - fullst) / denom)
>   where
>     AppliedLimits{ .. }                        
>                                          = rApplied
>
>     (fullst, fullen)                     = (fromIntegral rStart, fromIntegral rEnd)
>     (loopst, loopen)                     = (fromIntegral rLoopStart, fromIntegral rLoopEnd)
>     denom              :: Double         = fullen - fullst
>
> data SFZone =
>   SFZone {
>     zInstIndex         :: Maybe Word
>   , zKey               :: Maybe Word
>   , zVel               :: Maybe Word
>   , zInitAtten         :: Maybe Int
>   , zCoarseTune        :: Maybe Int
>   , zFineTune          :: Maybe Int
>   , zSampleIndex       :: Maybe Word
>   , zSampleMode        :: Maybe A.SampleMode
>   , zScaleTuning       :: Maybe Int
>   , zExclusiveClass    :: Maybe Word
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
>   , zKeyToVolEnvDecay  :: Maybe Int
>
>   , zModulators        :: [Modulator]}
>   deriving (Eq, Show)
>
> defZone                :: SFZone
> defZone                                  = SFZone 
>                                            Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing
>
>                                            Nothing Nothing Nothing
>     
>                                            Nothing
> 
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing
>
>                                            []
>

reconcile zone and sample header ======================================================================================

> receiveRecon           :: SynthSwitches → PreZone → NoteOn → Recon
> receiveRecon sw pz noon
>                                          =
>   let
>     recon                                = deJust "receiveRecon" pz.pzRecon
>     z                                    = pz.pzSFZone
>   in
>     recon{rEffects = Just $ deriveEffects sw recon.rM8n z.zChorus z.zReverb z.zPan noon}
>
> resolvePreZone         :: Directives → PreZone → F.Shdr → Recon
> resolvePreZone dives pz shdr
>   | traceIf trace_RPZ False              = undefined
>   | otherwise                            = reconL
>   where
>     fName                                = "resolvePreZone"
>     trace_RPZ                            = unwords [fName, show zd, show (rApplied reconL)]
>
>     sw                                   = dives.synthSwitches
>     zd                                   = pz.pzDigest
>     z                                    = pz.pzSFZone
>     
>     m8n                                  = resolveModulation dives z
>
>     reconL                               =
>       Recon
>         (fromMaybe A.NoLoop z.zSampleMode)
>         (fromIntegral shdr.sampleRate)
>
>         (AppliedLimits
>                     ((+) shdr.start      (fromIntegral zd.zdStart))
>                     ((+) shdr.end        (fromIntegral zd.zdEnd))
>                     ((+) shdr.startLoop  (fromIntegral zd.zdStartLoop))
>                     ((+) shdr.endLoop    (fromIntegral zd.zdEndLoop)))
>         
>         (fromIntegral $ fromMaybe shdr.originalPitch z.zRootKey)
>         (fromMaybe 100 z.zScaleTuning)
>         (reconAttenuation z.zInitAtten)
>         (deriveEnvelope sw z.zDelayVolEnv z.zAttackVolEnv z.zHoldVolEnv z.zDecayVolEnv z.zSustainVolEnv Nothing)                         
>         (if sw.usePitchCorrection
>            then Just $ reconPitchCorrection shdr.pitchCorrection z.zCoarseTune z.zFineTune
>            else Nothing)
>         m8n
>         Nothing
>
>     reconPitchCorrection
>                        :: Int → Maybe Int → Maybe Int → Double
>     reconPitchCorrection sub mps mpc     = fromMaybe ((fromCents . fromIntegral) sub) (fromCents' mps mpc)
>
>     reconAttenuation   :: Maybe Int → Double
>     reconAttenuation _                   = if sw.useAttenuation
>                                              then maybe 0 fromIntegral z.zInitAtten
>                                              else 0
> resolveModulation      :: Directives → SFZone → Modulation
> resolveModulation dives z                = resolveMods
>                                              m8n
>                                              z.zModulators
>                                              (if sw.useDefModulators then defaultMods else [])
>   where
>     sw                                   = dives.synthSwitches
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
>         True
>         (-1) -- must always be replaced
>
>     resonanceType                        = ResonanceSVF
>     nModEnv                              = deriveEnvelope
>                                              dives.synthSwitches
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
>
>     deriveLFO          :: Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe LFO
>     deriveLFO del mfreq toPitch toFilterFc toVolume
>                                          =
>       if sw.useLFO && anyJust
>         then Just $ LFO (fromTimecents del)
>                         (fromAbsoluteCents $ fromMaybe 0 mfreq)
>                         (deriveModTriple toPitch toFilterFc toVolume)
>         else Nothing
>       where
>         anyJust                          = isJust toPitch || isJust toFilterFc || isJust toVolume
>
> deriveEffects          :: SynthSwitches → Modulation → Maybe Int → Maybe Int → Maybe Int → NoteOn → Effects
> deriveEffects sw m8n mChorus mReverb mPan noon
>                                          = Effects 
>                                             (dChorus / 1000) 
>                                             (dReverb / 1000) 
>                                                (dPan / 1000)
>   where
>     dChorus            :: Double         =
>       if sw.useChorus
>         then maybe 0 fromIntegral mChorus + evaluateMods ToChorus m8n.mModsMap noon
>         else 0
>     dReverb            :: Double         =
>       if sw.useReverb
>         then maybe 0 fromIntegral mReverb + evaluateMods ToReverb m8n.mModsMap noon
>         else 0
>     dPan               :: Double         =
>       if sw.usePan
>         then maybe 0 fromIntegral mPan
>         else 0

Navigation ============================================================================================================

> data ChangeEarItem                       = MakeMono deriving Eq
>
> data ChangeEar a                         =
>   ChangeEar {
>     ceSource           :: a
>   , ceChanges          :: [ChangeEarItem]} deriving Eq

The End
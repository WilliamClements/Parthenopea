> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Synthesizer
William Clements
May 14, 2023

> module Parthenopea.Repro.Synthesizer
>        ( eutSynthesize ) where
>
> import Control.Arrow
> import Control.Arrow.Operations ( ArrowCircuit(delay) )
> import qualified Data.Audio              as A
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, Clock(..) )
> import Euterpea.Music ( Dur, AbsPitch )
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Modulation
> import Parthenopea.Repro.Zone
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
  
Signal function-based synth ===========================================================================================

Euterpea provides call back mechanism for rendering. Each Midi note, fully specified, comes here to be synthesized.

> eutSynthesize          :: ∀ p . Clock p ⇒
>                           SynthSwitches
>                           → (Recon, Maybe Recon)
>                           → NoteOn
>                           → VB.Vector Double
>                           → Double
>                           → Dur
>                           → SFFileRuntime
>                           → Signal p () (Double, Double)
> eutSynthesize sw@SynthSwitches{ .. }
>               (reconL, mreconR) noon sweeps sr dur sffileRuntime
>   | traceIf trace_ES False               = undefined
>   | otherwise                            =
>   if isNothing mreconR
>     then eutSplit <<< pumpMono
>     else pumpStereo
>   where
>     fName                                = "eutSynthesize"
>     trace_ES                             =
>       unwords [fName, "numPoints", show numPoints
>                    , "freqRatio",  show freqRatio
>                    , "sr",         show sr
>                    , "deltaCalc",  show deltaCalc] 
>
>     reconR                               = fromJust mreconR
>     (m8nL, m8nR)                         = (reconL.rM8n, reconR.rM8n)
>
>     numPoints          :: Double         = fromIntegral (reconL.rApplied.rEnd - reconL.rApplied.rStart)
>     secsSampled        :: Double         = numPoints * freqRatio / sr
>     secsScored         :: Double         = 1 * fromRational dur
>     looping            :: Bool           = secsScored > secsSampled
>                                            && (reconL.rSampleMode /= A.NoLoop)
>                                            && useLoopSwitching
>     secsToPlay         :: Double         = if looping
>                                              then secsScored
>                                              else min secsSampled secsScored
>     timeFrame                            =
>       TimeFrame
>         secsSampled
>         secsScored
>         secsToPlay
>         looping
>
>     freqRatio          :: Double         =
>       case reconL.rTuning of
>       0                                  → 1
>       100                                → apToHz reconL.rRootKey / apToHz noon.noteOnKey
>       _                                  → microtoneRatio
>                                              (reconL.rRootKey - noon.noteOnKey)
>                                              (fromIntegral reconL.rTuning)
>     rateRatio          :: Double         = rate (undefined::p) / sr
>     freqFactor         :: Double         = freqRatio * rateRatio / fromMaybe 1 reconL.rPitchCorrection
>     deltaCalc          :: Double         = 1 / (numPoints * freqFactor)
>
>     pumpMono           :: Signal p () Double
>     eutSplit           :: Signal p Double (Double, Double)
>     pumpStereo         :: Signal p () (Double, Double)
>     modulateStereo, ampStereo
>                        :: Signal p (Double, Double) (Double, Double)
>     eutDriver          :: Signal p () Double
>     pumpMonoSample     :: Signal p Double Double
>     pumpStereoSample   :: Signal p Double (Double, Double)
>
>     SampleArrays{ .. }                   
>                                          = sffileRuntime.zSample
>     pumpMono                             =
>       eutDriver
>         >>> pumpMonoSample
>         >>> eutModulate         timeFrame m8nL noon
>         >>> eutEffectsMono      sw (deJust fName reconL.rEffects)
>         >>> eutAmplify          timeFrame reconL.rM8n reconL.rVolEnv sweeps noon
>
>     pumpStereo                           = 
>       eutDriver
>         >>> pumpStereoSample
>         >>> modulateStereo
>         >>> eutEffectsStereo    sw (deJust fName reconL.rEffects) (deJust fName reconR.rEffects)
>         >>> ampStereo 
>
>     eutSplit                             =
>       proc sIn → do
>         outA                             ⤙ (sIn, sIn)
>
>     eutDriver                            = if timeFrame.tfLooping
>                                              then procDriver calcLooping
>                                              else procDriver calcNotLooping
>       where
>         (lst, len)                       = normalizeLooping reconL
>
>         calcLooping, calcNotLooping
>                        :: Double → Double
>         calcLooping next                 = if next > len    then lst                      else next
>         calcNotLooping next              = if next > 1      then frac next                else next
>                                              where frac = snd . properFraction
>
>         procDriver calcPhase             = proc () → do
>           modSig                         ← eutModSignals timeFrame reconL.rM8n ToPitch     ⤙ ()
>           let delta                      =
>                 deltaCalc * evaluateModSignals "procDriver" reconL.rM8n ToPitch noon modSig
>           rec
>             let phase                    = calcPhase next
>             next           ← delay 0     ⤙ phase + delta                           
>           outA                           ⤙ phase
>
>     modulateStereo                       =
>       proc (sL, sR) → do
>         mL                               ← eutModulate timeFrame m8nL noon                 ⤙ sL
>         mR                               ← eutModulate timeFrame m8nR noon                 ⤙ sR
>         outA                                                                               ⤙ (mL, mR)
>
>     ampStereo                            =
>       proc (tL, tR) → do
>         mL                               ← eutAmplify timeFrame reconL.rM8n reconL.rVolEnv sweeps noon
>                                                                                            ⤙ tL
>         mR                               ← eutAmplify timeFrame reconR.rM8n reconR.rVolEnv sweeps noon
>                                                                                            ⤙ tR
>         outA                                                                               ⤙ (mL, mR)
>
>     pumpMonoSample                       =
>       proc pos                           → do
>         let pos'       :: Double         = fromIntegral (rEnd - rStart) * pos
>         let ix         :: Int            = truncate pos'
>         let offset     :: Double         = pos' - fromIntegral ix
>
>         let a1L                          = samplePointInterp ssData ssM24 offset (fromIntegral rStart + ix)
>         outA                             ⤙ a1L * ampL
>       where
>         AppliedLimits{ .. }
>                                          = reconL.rApplied
>         cAttenL        :: Double         =
>           fromCentibels (reconL.rAttenuation + evaluateMods ToInitAtten reconL.rM8n.mModsMap noon)
>         ampL                             = fromIntegral noon.noteOnVel / 100 / cAttenL
>
>     pumpStereoSample                     =
>       proc pos                           → do
>         let pos'       :: Double         = fromIntegral (appliedL.rEnd - appliedL.rStart) * pos
>         let ix         :: Int            = truncate pos'
>         let offset     :: Double         = pos' - fromIntegral ix
>
>         let a1L        :: Double         = samplePointInterp ssData ssM24 offset (fromIntegral appliedL.rStart + ix) 
>         let a1R        :: Double         = samplePointInterp ssData ssM24 offset (fromIntegral appliedR.rStart + ix)
>         outA                             ⤙ (a1L * ampL, a1R * ampR)
>       where
>         Recon{rAttenuation = attenL, rApplied = appliedL}
>                                          = reconL
>         Recon{rAttenuation = attenR, rApplied = appliedR}
>                                          = reconR
>         Modulation{mModsMap = mmodsL}    = m8nL
>         Modulation{mModsMap = mmodsR}    = m8nR
>         cAttenL                          = fromCentibels (attenL + evaluateMods ToInitAtten mmodsL noon)
>         cAttenR                          = fromCentibels (attenR + evaluateMods ToInitAtten mmodsR noon)
>         ampL                             = fromIntegral noon.noteOnVel / 100 / cAttenL
>         ampR                             = fromIntegral noon.noteOnVel / 100 / cAttenR

Account for custom frequency intervals = SoundFont scale tuning : 0 < x < 100 < 1200
Clearly multiple root pitches are mutually incompatible, in general, for calculating frequency ratios

For example, input of 3 = key interval, 50 = scale tuning, yields ratio of: 1.09
Multiply the root frequency by that to give output frequency

> microtoneRatio         :: AbsPitch → Int → Double
> microtoneRatio apDelta tuning            = pow 2 (fromIntegral apDelta * fromIntegral tuning / 1_200)

Resonance =============================================================================================================

> addResonance           :: ∀ p . Clock p ⇒ Modulation → NoteOn
>                           → Signal p (Double, ModSignals) Double
> addResonance m8n noon                    =
>   proc (sIn, msig)                       → do
>     let fc                               = modulateFc msig
>     pickled ← procFilter m8n.mLowpass    ⤙ (sIn, fc)
>     outA                                 ⤙ pickled
>   where
>     modulateFc     :: ModSignals → Double
>     modulateFc msig                  =
>       clip freakRange (lowpassFc m8n.mLowpass * evaluateModSignals "modulateFc" m8n ToFilterFc noon msig)

Modulation Signals ====================================================================================================

> eutModSignals          :: ∀ p . Clock p ⇒ TimeFrame → Modulation → ModDestType
>                           → Signal p () ModSignals
> eutModSignals timeFrame m8n md           =
>   proc _                                 → do
>     aL1 ← doEnvelope  timeFrame kModEnvL             ⤙ ()
>     aL2 ← doLFO       kModLfoL                       ⤙ ()
>     aL3 ← doLFO       kVibLfoL                       ⤙ ()
>     outA                                             ⤙ ModSignals aL1 aL2 aL3
>   where
>     (kModEnvL, kModLfoL, kVibLfoL)       = case md of
>       ToPitch                            → ( m8n.mModEnv, m8n.mModLfo, m8n.mVibLfo)
>       ToFilterFc                         → ( m8n.mModEnv, m8n.mModLfo, Nothing)
>       ToVolume                           → ( Nothing,     m8n.mModLfo, Nothing)
>       _                                  →
>         error $ unwords["only ToPitch, ToFilterFc, and ToVolume supported in eutModSignals, not", show md]

Modulation ============================================================================================================

> eutModulate            :: ∀ p . Clock p ⇒ TimeFrame → Modulation → NoteOn
>                           → Signal p Double Double
> eutModulate timeFrame m8n noon           =
>   proc a1L                               → do
>     modSigL                              ← eutModSignals timeFrame m8n ToFilterFc         ⤙ ()
>     a2L                                  ← addResonance m8n noon                          ⤙ (a1L, modSigL)
>     outA                                                                                  ⤙ a2L

Amplification =========================================================================================================

> eutAmplify             :: ∀ p . Clock p ⇒ TimeFrame → Modulation → Maybe FEnvelope → VB.Vector Double → NoteOn
>                           → Signal p Double Double
> eutAmplify timeFrame m8n volEnv sweeps noon
>                                          =
>   proc a1L → do
>     aSweep                               ← doSweepingEnvelope timeFrame eor                ⤙ ()
>     aenvL                                ← doEnvelope timeFrame volEnv                     ⤙ ()
>     modSigL                              ← eutModSignals timeFrame m8n ToVolume            ⤙ ()
>     let a2L                              =
>           a1L * aenvL * (aSweep / 100) * evaluateModSignals fName m8n ToVolume noon modSigL
>     outA                                 ⤙ a2L
>   where
>     fName                                = "eutAmplify"
>
>     eor                                  = if VB.null sweeps
>                                              then Left noon.noteOnVel
>                                              else Right sweeps

Effects ===============================================================================================================

> eutEffectsMono       :: ∀ p . Clock p ⇒ SynthSwitches → Effects → Signal p Double Double
> eutEffectsMono sw Effects{ .. }
>                                          =
>   proc aL → do
>     chL ← eutChorus sw.chorusRate sw.chorusDepth efChorus
>                                                          ⤙ aL
>     rbL ← eutReverb efReverb                             ⤙ aL
>
>     let mixL = (  efChorus       * chL
>                 + efReverb       * rbL
>                 + (1 - efChorus) * aL
>                 + (1 - efReverb) * aL) / 2
>
>     let pL                                               =
>           if sw.noStereoNoPan then mixL else fst $ doPan (efPan, efPan) (mixL, mixL)
>
>     pL' ←          if not sw.useDCBlock
>                      then delay 0                        ⤙ pL
>                      else dcBlock dcBlockCharacteristic  ⤙ pL
>     outA                                                 ⤙ pL'
>
> eutEffectsStereo       :: ∀ p . Clock p ⇒
>                           SynthSwitches → Effects → Effects → Signal p (Double, Double) (Double, Double)
> eutEffectsStereo sw effL effR            =
>   proc (aL, aR)                          → do
>     chL ← eutChorus sw.chorusRate sw.chorusDepth cFactorL
>                                                          ⤙ aL
>     chR ← eutChorus sw.chorusRate sw.chorusDepth cFactorR
>                                                          ⤙ aR
>
>     rbL ← eutReverb rFactorL                             ⤙ aL
>     rbR ← eutReverb rFactorR                             ⤙ aR
>
>     let mixL                             =
>                (  cFactorL       * chL
>                 + rFactorL       * rbL
>                 + (1 - cFactorL) * aL
>                 + (1 - rFactorL) * aL) / 2
>     let mixR                             =
>                (  cFactorR       * chR
>                 + rFactorR       * rbR
>                 + (1 - cFactorR) * aR
>                 + (1 - rFactorR) * aR) / 2
>
>     let (pL, pR) = doPan (pFactorL, pFactorR) (mixL, mixR)
>
>     pL' ←        if not sw.useDCBlock
>                    then delay 0                          ⤙ pL
>                    else dcBlock dcBlockCharacteristic    ⤙ pL
>     pR' ←        if not sw.useDCBlock
>                    then delay 0                          ⤙ pR
>                    else dcBlock dcBlockCharacteristic    ⤙ pR
>     outA                                                 ⤙ (pL', pR')
>   where
>     Effects{efChorus = cFactorL, efReverb = rFactorL, efPan = pFactorL} = effL
>     Effects{efChorus = cFactorR, efReverb = rFactorR, efPan = pFactorR} = effR
> 
> eutChorus              :: ∀ p . Clock p ⇒ Double → Double → Double → Signal p Double Double
> eutChorus crate cdepth cFactor         =
>   if cFactor > 0
>     then makeSF
>     else delay 0                                            
>   where
>     makeSF                               = proc sIn → do
>       z1 ← safeDelayLine1 0.023 0.017    ⤙ sIn
>       z2 ← safeDelayLine1 0.025 0.019    ⤙ sIn
>       z3 ← safeDelayLine1 0.029 0.023    ⤙ sIn
>       let sOut                           = coeff1 * z1 + coeff2 * z2 + coeff3 * z3
>       outA                               ⤙ sOut
>
>     coeff1 = 1/3
>     coeff2 = 1/3
>     coeff3 = 1/3
>
>     safeDelayLine1 maxDel del            =
>       profess
>         (maxDel >= del + cdepth)
>         (unwords [  "safeDelayLine1: maxDel"                   , show maxDel
>                   , "provided to delayLine1 lacks capacity for", show (del + cdepth)])
>         (proc sIn → do
>            cphase ← osc (tableSinesN 4096 [1]) 0
>                                          ⤙ crate
>            sOut ← delayLine1 maxDel      ⤙ (sIn, del + cdepth * cphase)
>            outA                          ⤙ sOut)
>
> eutReverb              :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> eutReverb rFactorL                       =
>   if rFactorL > epsilon
>     then eatFreeVerb $ makeFreeVerb roomSize damp width
>     else constA 0
>   where
>     roomSize                             = 0.75
>     damp                                 = 0.25
>     width                                = 1.0
>   
> doPan                  :: (Double, Double) → (Double, Double) → (Double, Double)
> doPan (azimuthL, azimuthR) (sigL, sigR)  = ((ampLL + ampRL)/2, (ampLR + ampRR)/2)
>   where
>     xL                                   = cos ((azimuthL + 0.5) * pi / 2)
>     xR                                   = cos ((azimuthR + 0.5) * pi / 2)
>     ampLL                                = sigL * xL
>     ampLR                                = sigL * (1 - xL)
>     ampRL                                = sigR * xR
>     ampRR                                = sigR * (1 - xR)
>
> dcBlockCharacteristic                    = 0.995
>
> dcBlock                :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> dcBlock characteristic                   = proc xn → do
>   rec
>     let yn             = xn - xn_l + characteristic * yn_l
>     xn_l               ← delay 0         ⤙ xn
>     yn_l               ← delay 0         ⤙ yn
>   outA ⤙ yn

The End
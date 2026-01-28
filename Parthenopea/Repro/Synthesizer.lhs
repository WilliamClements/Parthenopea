> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Synthesizer
William Clements
May 14, 2023

> module Parthenopea.Repro.Synthesizer
>        ( deriveEffects
>        , eutSynthesize
>        ) where
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
>     secsSampled                          = numPoints * freqRatio / sr
>     secsScored                           = 1 * fromRational dur
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
>       _                                  → calcMicrotoneRatio
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
>     eutModulate        :: Modulation → Signal p Double Double
>     eutDriver          :: Signal p () Double
>     eutAmplify         :: Recon → Signal p Double Double
>     pumpMonoSample     :: Signal p Double Double
>     pumpStereoSample   :: Signal p Double (Double, Double)
>
>     SampleArrays{ .. }                   
>                                          = sffileRuntime.zSample
>     pumpMono                             =
>       eutDriver
>         >>> pumpMonoSample
>         >>> eutModulate         m8nL
>         >>> eutEffectsMono      sw (deJust fName reconL.rEffects)
>         >>> eutAmplify          reconL
>
>     pumpStereo                           = 
>       eutDriver
>         >>> pumpStereoSample
>         >>> modulateStereo
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
>           modSig                         ← eutModSignals reconL.rM8n ToPitch               ⤙ ()
>           let delta                      =
>                 deltaCalc * evaluateModSignals sw "procDriver" reconL.rM8n ToPitch noon modSig
>           rec
>             let phase                    = calcPhase next
>             next           ← delay 0     ⤙ phase + delta                           
>           outA                           ⤙ phase
>
>     modulateStereo                       =
>       proc (sL, sR) → do
>         mL                               ← eutModulate m8nL                                ⤙ sL
>         mR                               ← eutModulate m8nR                                ⤙ sR
>         outA                                                                               ⤙ (mL, mR)
>
>     ampStereo                            =
>       proc (sL, sR) → do
>         (tL, tR)                         ← eutEffectsStereo sw (fromJust reconL.rEffects)
>                                                                (fromJust reconR.rEffects)  ⤙ (sL, sR)
>         mL                               ← eutAmplify reconL                               ⤙ tL
>         mR                               ← eutAmplify reconR                               ⤙ tR
>         outA                                                                               ⤙ (mL, mR)
>
>     eutModulate m8n                      =
>       proc a1L                           → do
>         modSigL                          ← eutModSignals m8n ToFilterFc                    ⤙ ()
>         a2L                              ← addResonance m8nL                               ⤙ (a1L, modSigL)
>         outA                                                                               ⤙ a2L
>
>     eutAmplify recon                     =
>       proc a1L → do
>         aSweep                           ← doSweepingEnvelope timeFrame eor                ⤙ ()
>         aenvL                            ← doEnvelope timeFrame recon.rVolEnv              ⤙ ()
>         modSigL                          ← eutModSignals recon.rM8n ToVolume               ⤙ ()
>         let a2L                          =
>               a1L * aenvL * (aSweep / 100) * evaluateModSignals sw fNameAmplify recon.rM8n ToVolume noon modSigL
>         outA                             ⤙ a2L
>       where
>         fNameAmplify                     = "eutAmplify"
>
>         eor                              = if VB.null sweeps
>                                              then Left noon.noteOnVel
>                                              else Right sweeps
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
>           fromCentibels (reconL.rAttenuation + evaluateMods sw ToInitAtten reconL.rM8n.mModsMap noon)
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
>         cAttenL                          = fromCentibels (attenL + evaluateMods sw ToInitAtten mmodsL noon)
>         cAttenR                          = fromCentibels (attenR + evaluateMods sw ToInitAtten mmodsR noon)
>         ampL                             = fromIntegral noon.noteOnVel / 100 / cAttenL
>         ampR                             = fromIntegral noon.noteOnVel / 100 / cAttenR
>
>     addResonance       :: Modulation → Signal p (Double, ModSignals) Double
>     addResonance m8n@Modulation{mLowpass}
>                                          = final
>       where
>         final                            =
>           proc (sIn, msig)               → do
>             let fc                       = modulateFc msig
>             pickled ← procFilter mLowpass ⤙ (sIn, fc)
>             outA                         ⤙ pickled
>
>         modulateFc     :: ModSignals → Double
>         modulateFc msig                  =
>           clip freakRange (lowpassFc mLowpass * evaluateModSignals sw "modulateFc" m8n ToFilterFc noon msig)

Account for custom frequency intervals -- SoundFont scale tuning : 0 < x < 100 < 1200
Clearly multiple root pitches are mutually incompatible, in general, for calculating frequency ratios

For example, input of 3 = key interval, 50 = scale tuning, yields ratio of: 1.09
Multiply the root frequency by that to give output frequency

>     calcMicrotoneRatio :: AbsPitch → Int → Double
>     calcMicrotoneRatio apDelta tuning    = pow 2 (fromIntegral apDelta * fromIntegral tuning / 1_200)

Modulation Signals ====================================================================================================

>     eutModSignals          :: Modulation → ModDestType → Signal p () ModSignals
>     eutModSignals m8n md                 =
>       proc _                             → do
>         aL1 ← doEnvelope  timeFrame kModEnvL             ⤙ ()
>         aL2 ← doLFO       kModLfoL                       ⤙ ()
>         aL3 ← doLFO       kVibLfoL                       ⤙ ()
>         outA                                             ⤙ ModSignals aL1 aL2 aL3
>       where
>         (kModEnvL, kModLfoL, kVibLfoL)   = case md of
>           ToPitch                        → ( m8n.mModEnv, m8n.mModLfo, m8n.mVibLfo)
>           ToFilterFc                     → ( m8n.mModEnv, m8n.mModLfo, Nothing)
>           ToVolume                       → ( Nothing,     m8n.mModLfo, Nothing)
>           _                              →
>             error $ unwords["only ToPitch, ToFilterFc, and ToVolume supported in eutModSignals, not", show md]

Effects ===============================================================================================================

> deriveEffects          :: SynthSwitches → Modulation → Maybe Int → Maybe Int → Maybe Int → NoteOn → Effects
> deriveEffects
>   sw@SynthSwitches{ .. } m8n mChorus mReverb mPan noon
>                                          = Effects 
>                                             (dChorus / 1000) 
>                                             (dReverb / 1000) 
>                                                (dPan / 1000)
>   where
>     dChorus            :: Double         =
>       if useChorus
>         then maybe 0 fromIntegral mChorus + evaluateMods sw ToChorus m8n.mModsMap noon
>         else 0
>     dReverb            :: Double         =
>       if useReverb
>         then maybe 0 fromIntegral mReverb + evaluateMods sw ToReverb m8n.mModsMap noon
>         else 0
>     dPan               :: Double         =
>       if usePan
>         then maybe 0 fromIntegral mPan
>         else 0
>
> eutEffectsMono       :: ∀ p . Clock p ⇒ SynthSwitches → Effects → Signal p Double Double
> eutEffectsMono
>   SynthSwitches{ .. }
>   Effects{ .. }
>                                          =
>   proc aL → do
>     chL ← eutChorus chorusRate chorusDepth efChorus      ⤙ aL
>     rbL ← eutReverb efReverb                             ⤙ aL
>
>     let mixL = (  efChorus       * chL
>                 + efReverb       * rbL
>                 + (1 - efChorus) * aL
>                 + (1 - efReverb) * aL) / 2
>
>     let pL                                               =
>           if noStereoNoPan then mixL else fst $ doPan (efPan, efPan) (mixL, mixL)
>
>     pL' ←          if not useDCBlock
>                      then delay 0                        ⤙ pL
>                      else dcBlock 0.995                  ⤙ pL
>     outA                                                 ⤙ pL'
>
> eutEffectsStereo       :: ∀ p . Clock p ⇒ SynthSwitches → Effects → Effects → Signal p (Double, Double) (Double, Double)
> eutEffectsStereo
>   SynthSwitches{ .. }
>   effL effR
>                                          =
>   proc (aL, aR) → do
>     chL ← eutChorus chorusRate chorusDepth cFactorL      ⤙ aL
>     chR ← eutChorus chorusRate chorusDepth cFactorR      ⤙ aR
>
>     rbL ← eutReverb rFactorL                             ⤙ aL
>     rbR ← eutReverb rFactorR                             ⤙ aR
>
>     let mixL = (  cFactorL       * chL
>                 + rFactorL       * rbL
>                 + (1 - cFactorL) * aL
>                 + (1 - rFactorL) * aL) / 2
>     let mixR = (  cFactorR       * chR
>                 + rFactorR       * rbR
>                 + (1 - cFactorR) * aR
>                 + (1 - rFactorR) * aR) / 2
>
>     let (pL, pR) = doPan (pFactorL, pFactorR) (mixL, mixR)
>
>     pL' ←        if not useDCBlock
>                    then delay 0                          ⤙ pL
>                    else dcBlock 0.995                    ⤙ pL
>     pR' ←        if not useDCBlock
>                    then delay 0                          ⤙ pR
>                    else dcBlock 0.995                    ⤙ pR
>     outA                                                 ⤙ (pL', pR')
>   where
>     Effects{efChorus = cFactorL, efReverb = rFactorL, efPan = pFactorL} = effL
>     Effects{efChorus = cFactorR, efReverb = rFactorR, efPan = pFactorR} = effR
> 
> eutChorus              :: ∀ p . Clock p ⇒ Double → Double → Double → Signal p Double Double
> eutChorus crate_ cdepth_ cFactor         =
>   if cFactor > 0
>     then makeSF
>     else delay 0                                            
>   where
>     crate                                = profess (crate_ == clip (0.1, 100) crate_)
>                                              "chorus rate"
>                                              crate_
>     cdepth                               = profess (cdepth_ == clip (0.0001, 1.1) cdepth_)
>                                              "chorus depth"
>                                              cdepth_
>
>     makeSF             :: Signal p Double Double
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
>     safeDelayLine1     :: Double → Double → Signal p Double Double
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
>     then makeSF
>     else constA 0
>   where
>     roomSize                             = 0.75
>     damp                                 = 0.25
>     width                                = 1.0
>
>     makeSF             :: Signal p Double Double
>     makeSF                               = eatFreeVerb $ makeFreeVerb roomSize damp width
>   
> doPan                  :: (Double, Double) → (Double, Double) → (Double, Double)
> doPan (azimuthL, azimuthR) (sigL, sigR)  = ((ampLL + ampRL)/2, (ampLR + ampRR)/2)
>   where
>     xL = cos ((azimuthL + 0.5) * pi / 2)
>     xR = cos ((azimuthR + 0.5) * pi / 2)
>     ampLL = sigL * xL
>     ampLR = sigL * (1 - xL)
>     ampRL = sigR * xR
>     ampRR = sigR * (1 - xR)
>
> dcBlock                :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> dcBlock a = proc xn → do
>   rec
>     let yn = xn - xn_l + a * yn_l
>     xn_l ← delay 0 ⤙ xn
>     yn_l ← delay 0 ⤙ yn
>   outA ⤙ yn

The End
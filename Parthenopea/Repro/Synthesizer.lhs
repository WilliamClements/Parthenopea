> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Synthesizer
William Clements
May 14, 2023

> module Parthenopea.Repro.Synthesizer
>        ( deriveEffects
>        , Effects(..)
>        , eutDriver
>        , eutSynthesize
>        , normalizingOutput
>        , noStereoNoPan
>        , Recon(..)
>        , TimeFrame(..)
>        , useAttenuation
>        , useChorus
>        , useDCBlock
>        , useLoopSwitching
>        , usePan
>        , usePitchCorrection
>        , useReverb
>        ) where
>
> import Control.Arrow
> import Control.Arrow.Operations ( ArrowCircuit(delay) )
> import qualified Data.Audio              as A
> import Data.Int ( Int8, Int16 )
> import Data.Maybe
> import qualified Data.Vector             as VB
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, Clock(..) )
> import Euterpea.Music ( AbsPitch, Dur )
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Modulation
> import Parthenopea.SoundFont.SFSpec
  
Signal function-based synth ===========================================================================================

Euterpea provides call back mechanism for rendering. Each Midi note, fully specified, comes here to be synthesized.

> eutSynthesize          :: ∀ p . Clock p ⇒
>                           (Recon, Maybe Recon)
>                           → NoteOn
>                           → Double
>                           → Dur
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p () (Double, Double)
> eutSynthesize (reconL, mreconR) noon sr dur s16 ms8
>   | traceIf trace_ES False               = undefined
>   | otherwise                            =
>   if isNothing mreconR
>     then eutSplit <<< pumpMono
>     else pumpStereo
>   where
>     fName                                = "eutSynthesize"
>     trace_ES                             = unwords [fName, show timeFrame] 
>
>     reconR                               = fromJust mreconR
>     (m8nL, m8nR)                         = (reconL.rM8n, reconR.rM8n)
>
>     numPoints          :: Double         = fromIntegral (reconL.rEnd - reconL.rStart)
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
>       _                                  → calcMicrotoneRatio reconL.rRootKey noon.noteOnKey (fromIntegral reconL.rTuning)
>     rateRatio          :: Double         = rate (undefined::p) / sr
>     freqFactor         :: Double         = freqRatio * rateRatio / fromMaybe 1 reconL.rPitchCorrection
>     delta              :: Double         = 1 / (numPoints * freqFactor)
>
>     pumpMono           :: Signal p () Double
>     pumpStereo         :: Signal p () (Double, Double)
>     modulateStereo, ampStereo
>                        :: Signal p (Double, Double) (Double, Double)
>
>     pumpMono                             =
>       eutDriver                    timeFrame reconL delta
>       >>> eutPumpMonoSample        reconL noon s16 ms8
>       >>> eutModulate              timeFrame m8nL
>       >>> eutEffectsMono           reconL
>       >>> eutAmplify               timeFrame reconL
>
>     pumpStereo                           = 
>       eutDriver                    timeFrame reconL delta
>         >>> eutPumpStereoSample    (reconL, reconR) noon s16 ms8
>         >>> modulateStereo
>         >>> ampStereo
>
>     modulateStereo                       =
>       proc (sL, sR) → do
>         mL                               ← eutModulate timeFrame m8nL                      ⤙ sL
>         mR                               ← eutModulate timeFrame m8nR                      ⤙ sR
>         outA ⤙ (mL, mR)
>
>     ampStereo                            =
>       proc (sL, sR) → do
>         (tL, tR)                         ← eutEffectsStereo (reconL, reconR)               ⤙ (sL, sR)
>         mL                               ← eutAmplify timeFrame reconL                     ⤙ tL
>         mR                               ← eutAmplify timeFrame reconR                     ⤙ tR
>         outA ⤙ (mL, mR)
>
> eutModulate            :: ∀ p . Clock p ⇒ TimeFrame → Modulation → Signal p Double Double
> eutModulate timeFrame m8nL               =
>   proc a1L                               → do
>     modSigL                              ← eutModSignals timeFrame m8nL ToFilterFc ⤙ ()
>     a2L   ← addResonance m8nL            ⤙ (a1L, modSigL)
>     outA                                 ⤙ a2L
>
> eutDriver              :: ∀ p . Clock p ⇒ TimeFrame → Recon → Double → Signal p () Double
> eutDriver timeFrame reconL idelta
>   | traceNot trace_ED False              = undefined
>   | otherwise                            = if timeFrame.tfLooping
>                                              then procDriver calcLooping
>                                              else procDriver calcNotLooping
>   where
>     fName                                = "eutDriver"
>     trace_ED                             = unwords [fName, show idelta]
>
>     calcLooping, calcNotLooping
>                        :: Double → Double
>     calcLooping next                     = if next > len    then lst           else next
>     calcNotLooping next                  = if next > 1      then frac next     else next
>
>     procDriver calcPhase                 = proc () → do
>       modSig                             ← eutModSignals timeFrame reconL.rM8n ToPitch ⤙ ()
>       let delta                          = idelta * evaluateModSignals "procDriver" reconL.rM8n ToPitch modSig
>       rec
>         let phase                        = calcPhase next
>         next           ← delay 0         ⤙ phase + delta                           
>       outA                               ⤙ phase
>
>     (lst, len)         :: (Double, Double)
>                                          = normalizeLooping reconL
>
> normalizeLooping       :: Recon → (Double, Double)
> normalizeLooping recon                    = ((loopst - fullst) / denom, (loopen - fullst) / denom)
>   where
>     (fullst, fullen)                     = (fromIntegral recon.rStart, fromIntegral recon.rEnd)
>     (loopst, loopen)                     = (fromIntegral recon.rLoopStart, fromIntegral recon.rLoopEnd)
>     denom              :: Double         = fullen - fullst
>
> eutModSignals          :: ∀ p. Clock p ⇒ TimeFrame → Modulation → ModDestType → Signal p () ModSignals
> eutModSignals timeFrame m8n md
>   | traceNot trace_EMS False             = undefined
>   | otherwise                            =
>   proc _ → do
>     aL1 ← doEnvelope  timeFrame kModEnvL ⤙ ()
>     aL2 ← doLFO       kModLfoL           ⤙ ()
>     aL3 ← doLFO       kVibLfoL           ⤙ ()
>     outA                                 ⤙ ModSignals aL1 aL2 aL3
>   where
>     fName                                = "eutModSignals"
>     trace_EMS                            = unwords [fName, show md]
>
>     (kModEnvL, kModLfoL, kVibLfoL)       = doModSigMaybes 
>
>     doModSigMaybes                       = case md of
>       ToPitch                            → ( m8n.mModEnv, m8n.mModLfo, m8n.mVibLfo)
>       ToFilterFc                         → ( m8n.mModEnv, m8n.mModLfo, Nothing)
>       ToVolume                           → ( Nothing,     m8n.mModLfo, Nothing)
>       _                                  →
>         error $ unwords["only ToPitch, ToFilterFc, and ToVolume supported in doModSigMaybes, not", show md]
>
> eutPumpMonoSample      :: ∀ p . Clock p ⇒
>                           Recon
>                           → NoteOn
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p Double Double
> eutPumpMonoSample reconL noon s16 ms8    =
>   proc pos → do
>     let pos'           :: Double         = fromIntegral (reconL.rEnd - reconL.rStart) * pos
>     let ix             :: Int            = truncate pos'
>     let offset         :: Double         = pos' - fromIntegral ix
>
>     let a1L                              = samplePointInterp s16 ms8 offset (fromIntegral reconL.rStart + ix)
>     outA                                 ⤙ a1L * ampL
>   where
>     cAttenL            :: Double         =
>       fromCentibels (reconL.rAttenuation + evaluateMods ToInitAtten reconL.rM8n.mModsMap)
>     ampL                                 = fromIntegral noon.noteOnVel / 100 / cAttenL
>
> eutPumpStereoSample    :: ∀ p . Clock p ⇒
>                           (Recon, Recon)
>                           → NoteOn
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p Double (Double, Double)
> eutPumpStereoSample (reconL, reconR) noon s16 ms8
>   | traceNot trace_EPSS False            = undefined
>   | otherwise                            =
>   proc pos → do
>     let pos'           :: Double         = fromIntegral (enL - stL) * pos
>     let ix             :: Int            = truncate pos' -- WOX should be round?
>     let offset         :: Double         = pos' - fromIntegral ix
>
>     let a1L                              = samplePointInterp s16 ms8 offset (fromIntegral stL + ix) 
>     let a1R                              = samplePointInterp s16 ms8 offset (fromIntegral stR + ix)
>     outA                                 ⤙ (a1L * ampL, a1R * ampR)
>   where
>     fName                                = "eutPumpStereoSample"
>     trace_EPSS                           = unwords [fName, show noon]
>
>     Recon{rAttenuation = attenL, rStart = stL, rEnd = enL, rM8n = m8nL}
>                                          = reconL
>     Recon{rAttenuation = attenR, rStart = stR, rM8n = m8nR}
>                                          = reconR
>     Modulation{mModsMap = mmodsL}        = m8nL
>     Modulation{mModsMap = mmodsR}        = m8nR
>     cAttenL                              = fromCentibels (attenL + evaluateMods ToInitAtten mmodsL)
>     cAttenR                              = fromCentibels (attenR + evaluateMods ToInitAtten mmodsR)
>     ampL                                 = fromIntegral noon.noteOnVel / 100 / cAttenL
>     ampR                                 = fromIntegral noon.noteOnVel / 100 / cAttenR
>
> eutAmplify             :: ∀ p . Clock p ⇒ TimeFrame → Recon → Signal p Double Double
> eutAmplify timeFrame recon               =
>   proc a1L → do
>     aSweep                               ← doVeloSweepingEnvelope timeFrame recon.rDynamics      ⤙ ()
>     aenvL                                ← doEnvelope timeFrame recon.rVolEnv                    ⤙ ()
>     modSigL                              ← eutModSignals timeFrame recon.rM8n ToVolume           ⤙ ()
>     let a2L                              =
>           a1L * aenvL * (aSweep / 100) * evaluateModSignals fName recon.rM8n ToVolume modSigL
>     outA ⤙ a2L
>   where
>     fName                                = "eutAmplify"

Effects ===============================================================================================================

> deriveEffects          :: Modulation → Maybe Int → Maybe Int → Maybe Int → Effects
> deriveEffects m8n mChorus mReverb mPan
>                                          = Effects (dChorus / 1000) (dReverb / 1000) (dPan / 1000)
>   where
>     dChorus            :: Double         =
>       if useChorus
>         then maybe 0 fromIntegral mChorus + evaluateMods ToChorus m8n.mModsMap
>         else 0
>     dReverb            :: Double         =
>       if useReverb
>         then maybe 0 fromIntegral mReverb + evaluateMods ToReverb m8n.mModsMap
>         else 0
>     dPan               :: Double         =
>       if usePan
>         then maybe 0 fromIntegral mPan
>         else 0
>
> eutEffectsMono       :: ∀ p . Clock p ⇒ Recon → Signal p Double Double
> eutEffectsMono r                                         =
>   proc aL → do
>     chL ← eutChorus chorusRate chorusDepth cho           ⤙ aL
>     rbL ← eutReverb rev                                  ⤙ aL
>
>     let mixL = (  cho       * chL
>                 + rev       * rbL
>                 + (1 - cho) * aL
>                 + (1 - rev) * aL) / 2
>
>     let pL                                               =
>           if noStereoNoPan then mixL else fst $ doPan (pan, pan) (mixL, mixL)
>
>     pL' ←          if not useDCBlock
>                    then delay 0                          ⤙ pL
>                    else dcBlock 0.995                    ⤙ pL
>     outA                                                 ⤙ pL'
>   where
>     cho                                                  = r.rEffects.efChorus
>     rev                                                  = r.rEffects.efReverb
>     pan                                                  = r.rEffects.efPan 
>
> eutEffectsStereo       :: ∀ p . Clock p ⇒ (Recon, Recon) → Signal p (Double, Double) (Double, Double)
> eutEffectsStereo (Recon{rEffects = effL}, Recon{rEffects = effR})
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
> doPan (azimuthL, azimuthR) (sinL, sinR)  = ((ampLL + ampRL)/2, (ampLR + ampRR)/2)
>   where
>     xL = cos ((azimuthL + 0.5) * pi / 2)
>     xR = cos ((azimuthR + 0.5) * pi / 2)
>     ampLL = sinL * xL
>     ampLR = sinL * (1 - xL)
>     ampRL = sinR * xR
>     ampRR = sinR * (1 - xR)
>
> dcBlock                :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> dcBlock a = proc xn → do
>   rec
>     let yn = xn - xn_l + a * yn_l
>     xn_l ← delay 0 ⤙ xn
>     yn_l ← delay 0 ⤙ yn
>   outA ⤙ yn

Utility types =========================================================================================================

> data Recon                               =
>   Recon {
>     rSampleMode        :: A.SampleMode
>   , rSampleRate        :: Double
>   , rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: AbsPitch
>   , rTuning            :: Int
>   , rDynamics          :: VB.Vector Double
>   , rAttenuation       :: Double
>   , rVolEnv            :: Maybe FEnvelope
>   , rPitchCorrection   :: Maybe Double
>   , rM8n               :: Modulation
>   , rEffects           :: Effects}
>   deriving (Eq, Show)
>
> data Effects                             =
>   Effects {
>     efChorus           :: Double
>   , efReverb           :: Double
>   , efPan              :: Double}
>   deriving (Eq, Show)

Flags for customization ===============================================================================================

> usePitchCorrection, useAttenuation, useLoopSwitching, useReverb, useChorus, usePan, useDCBlock
>                        :: Bool
> noStereoNoPan, normalizingOutput
>                        :: Bool

Turn Knobs Here =======================================================================================================

> usePitchCorrection                       = True
> useAttenuation                           = True
> useLoopSwitching                         = True
> useReverb                                = True
> useChorus                                = True
> usePan                                   = True
> useDCBlock                               = True
> noStereoNoPan                            = True
> normalizingOutput                        = True

The End
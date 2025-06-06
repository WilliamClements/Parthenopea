> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
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
>        , noStereoNoPan
>        , normalizingOutput
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
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, Clock(..) )
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Modulation
> import Parthenopea.SoundFont.SFSpec
  
Signal function-based synth ===========================================================================================

Euterpea provides call back mechanism for rendering. Each Midi note, fully specified, comes here to be synthesized.

> eutSynthesize          :: ∀ p . Clock p ⇒
>                           (Recon, Maybe Recon)
>                           → Double
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p () (Double, Double)
> eutSynthesize (reconL, mreconR) sr dur pch vol _ s16 ms8
>   | traceIf trace_eS False               = undefined
>   | otherwise                            =
>   if isNothing mreconR
>     then eutSplit <<< pumpMonoPath 
>     else pumpStereoPath
>   where
>     fName                                = "eutSynthesize"
>     trace_eS                             = unwords [fName, show timeFrame] 
>
>     noon                                 = NoteOn vol pch
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
>     pumpMonoPath       :: Signal p () Double
>     pumpStereoPath     :: Signal p () (Double, Double)
>     modulateStereo, ampStereo
>                        :: Signal p (Double, Double) (Double, Double)
>
>     pumpMonoPath                         =
>       eutDriver                    timeFrame reconL delta
>       >>> eutPumpMono              reconL noon dur s16 ms8
>       >>> eutModulate              timeFrame m8nL noon
>       >>> eutEffectsMono           reconL
>       >>> eutAmplify               timeFrame reconL noon
>
>     pumpStereoPath                       = 
>       eutDriver                    timeFrame reconL delta
>         >>> eutPumpStereo          (reconL, reconR) noon dur s16 ms8
>         >>> modulateStereo
>         >>> ampStereo
>
>     modulateStereo                       =
>       proc (sL, sR) → do
>         mL                               ← eutModulate timeFrame m8nL noon ⤙ sL
>         mR                               ← eutModulate timeFrame m8nR noon ⤙ sR
>         outA ⤙ (mL, mR)
>
>     ampStereo                            =
>       proc (sL, sR) → do
>         (tL, tR)                         ← eutEffectsStereo (reconL, reconR) ⤙ (sL, sR)
>         mL                               ← eutAmplify timeFrame reconL noon ⤙ tL
>         mR                               ← eutAmplify timeFrame reconR noon ⤙ tR
>         outA ⤙ (mL, mR)
>
> eutModulate            :: ∀ p . Clock p ⇒ TimeFrame → Modulation → NoteOn → Signal p Double Double
> eutModulate timeFrame m8nL noon =
>   proc a1L → do
>     modSigL                            ← eutModSignals timeFrame m8nL ToFilterFc ⤙ ()
>     a2L   ← addResonance noon m8nL     ⤙ (a1L, modSigL)
>     outA                               ⤙ a2L
>
> eutDriver              :: ∀ p . Clock p ⇒ TimeFrame → Recon → Double → Signal p () Double
> eutDriver timeFrame reconL@Recon{ .. } idelta
>                                          = if timeFrame.tfLooping
>                                              then procDriver calcLooping
>                                              else procDriver calcNotLooping
>   where
>     calcLooping, calcNotLooping
>                        :: Double → Double
>     calcLooping next                     = if next > len    then lst           else next
>     calcNotLooping next                  = if next > 1      then frac next     else next
>
>     procDriver calcPhase                 = proc () → do
>       modSig                             ← eutModSignals timeFrame rM8n ToPitch ⤙ ()
>       let delta                          = idelta * evaluateModSignals "procDriver" rM8n ToPitch modSig rNoteOn
>       rec
>         let phase                        = calcPhase next
>         next           ← delay 0         ⤙ phase + delta                           
>       outA                               ⤙ phase
>
>     (lst, len)         :: (Double, Double)
>                                          = normalizeLooping reconL
>
> normalizeLooping       :: Recon → (Double, Double)
> normalizeLooping Recon{ .. }             = ((loopst - fullst) / denom, (loopen - fullst) / denom)
>   where
>     (fullst, fullen)                     = (fromIntegral rStart, fromIntegral rEnd)
>     (loopst, loopen)                     = (fromIntegral rLoopStart, fromIntegral rLoopEnd)
>     denom              :: Double         = fullen - fullst
>
> eutModSignals          :: ∀ p. Clock p ⇒ TimeFrame → Modulation → ModDestType → Signal p () ModSignals
> eutModSignals timeFrame Modulation{ .. } md
>                                          =
>   proc _ → do
>     aL1 ← doEnvelope  timeFrame kModEnvL             ⤙ ()
>     aL2 ← doLFO       kModLfoL                       ⤙ ()
>     aL3 ← doLFO       kVibLfoL                       ⤙ ()
>     outA                                             ⤙ ModSignals aL1 aL2 aL3
>   where
>     (kModEnvL, kModLfoL, kVibLfoL)       = doModSigMaybes 
>
>     doModSigMaybes                       = case md of
>       ToPitch                            → ( mModEnv, mModLfo, mVibLfo)
>       ToFilterFc                         → ( mModEnv, mModLfo, Nothing)
>       ToVolume                           → ( Nothing, mModLfo, Nothing)
>       _                                  →
>         error $ unwords["only ToPitch, ToFilterFc, and ToVolume supported in doModSigMaybes, not", show md]
>
> eutPumpMono            :: ∀ p . Clock p ⇒
>                           Recon
>                           → NoteOn
>                           → Dur
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p Double Double
> eutPumpMono reconL noon@NoteOn{noteOnVel} _ s16 ms8
>                                          =
>   proc pos → do
>     let pos'           :: Double         = fromIntegral (rEnd - rStart) * pos
>     let ix             :: Int            = truncate pos'
>     let offset         :: Double         = pos' - fromIntegral ix
>
>     let a1L                              = samplePointInterp s16 ms8 offset (fromIntegral rStart + ix)
>     outA                                 ⤙ notracer "a1L" a1L * ampL
>
>   where
>     Recon{ .. }                          = reconL
>     Modulation{ .. }                     = rM8n
>     cAttenL            :: Double         =
>       fromCentibels (rAttenuation + evaluateMods ToInitAtten mModsMap noon)
>     ampL                                 = fromIntegral noteOnVel / 100 / cAttenL
>
> eutPumpStereo         :: ∀ p . Clock p ⇒
>                           (Recon, Recon)
>                           → NoteOn
>                           → Dur
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p Double (Double, Double)
> eutPumpStereo (reconL, reconR) noon@NoteOn{noteOnVel} _ s16 ms8
>                                          = 
>   proc pos → do
>     let pos'           :: Double         = fromIntegral (enL - stL) * pos
>     let ix             :: Int            = truncate pos' -- WOX should be round?
>     let offset         :: Double         = pos' - fromIntegral ix
>
>     let a1L                              = notracer "a1L" $ samplePointInterp s16 ms8 offset (fromIntegral stL + ix) 
>     let a1R                              = notracer "a1R" $ samplePointInterp s16 ms8 offset (fromIntegral stR + ix)
>     outA ⤙ (a1L * ampL, a1R * ampR)
>
>   where
>     Recon{rAttenuation = attenL, rStart = stL, rEnd = enL, rM8n = m8nL}
>                                          = reconL
>     Recon{rAttenuation = attenR, rStart = stR, rM8n = m8nR}
>                                          = reconR
>     Modulation{mModsMap = mmodsL}        = m8nL
>     Modulation{mModsMap = mmodsR}        = m8nR
>     cAttenL                              = fromCentibels (attenL + evaluateMods ToInitAtten mmodsL noon)
>     cAttenR                              = fromCentibels (attenR + evaluateMods ToInitAtten mmodsR noon)
>     ampL                                 = fromIntegral noteOnVel / 100 / cAttenL
>     ampR                                 = fromIntegral noteOnVel / 100 / cAttenR
>
> eutAmplify             :: ∀ p . Clock p ⇒ TimeFrame → Recon → NoteOn → Signal p Double Double
> eutAmplify timeFrame Recon{ .. } noon    =
>   proc a1L → do
>     aenvL                                ← doEnvelope timeFrame rVolEnv ⤙ ()
>     modSigL                              ← eutModSignals timeFrame rM8n ToVolume ⤙ ()
>     let a2L                              =
>           notracer "a1" a1L * notracer "aenv" aenvL * evaluateModSignals "eutAmplify" rM8n ToVolume modSigL noon
>     outA ⤙ a2L

Effects ===============================================================================================================

> deriveEffects          :: Modulation → NoteOn → Maybe Int → Maybe Int → Maybe Int → Effects
> deriveEffects Modulation{ .. } noon mChorus mReverb mPan
>                                          = Effects (dChorus / 1000) (dReverb / 1000) (dPan / 1000)
>   where
>     dChorus            :: Double         =
>       if useChorus
>         then maybe 0 fromIntegral mChorus + evaluateMods ToChorus mModsMap noon
>         else 0
>     dReverb            :: Double         =
>       if useReverb
>         then maybe 0 fromIntegral mReverb + evaluateMods ToReverb mModsMap noon
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
>   | traceNever trace_eE False = undefined
>   | otherwise =
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
>
>   where
>     Effects{efChorus = cFactorL, efReverb = rFactorL, efPan = pFactorL} = effL
>     Effects{efChorus = cFactorR, efReverb = rFactorR, efPan = pFactorR} = effR
>
>     trace_eE = unwords ["eutEffectsStereo", show ((cFactorL, cFactorR), (rFactorL, rFactorR))]
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
>       outA                               ⤙ chorus sIn z1 z2 z3 sOut
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
>     chorus             :: Double → Double → Double → Double → Double → Double
>     chorus tIn y1 y2 y3 tOut
>       | traceNever trace_C False         = undefined
>       | otherwise                        = tOut
>       where
>         trace_C                          = unwords ["chorus", show (tIn, y1, y2, y3, tOut)]
>
> eutReverb              :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> eutReverb rFactorL                       =
>   if rFactorL > 0
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
> doPan (azimuthL, azimuthR) (sinL, sinR)
>   | traceNever trace_DP False            = undefined
>   | otherwise                            = ((ampLL + ampRL)/2, (ampLR + ampRR)/2)
>   where
>     xL = cos ((azimuthL + 0.5) * pi / 2)
>     xR = cos ((azimuthR + 0.5) * pi / 2)
>     ampLL = sinL * xL
>     ampLR = sinL * (1 - xL)
>     ampRL = sinR * xR
>     ampRR = sinR * (1 - xR)
>
>     trace_DP = unwords ["doPan", show (ampLL, ampLR, ampRL, ampRR)]
>
> dcBlock                :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> dcBlock a = proc xn → do
>   rec
>     let yn = xn - xn_l + a * yn_l
>     xn_l ← delay 0 ⤙ xn
>     yn_l ← delay 0 ⤙ yn
>   outA ⤙ yn

Utility types =========================================================================================================

> data Recon =
>   Recon {
>     rSampleMode        :: A.SampleMode
>   , rSampleRate        :: Double
>   , rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: AbsPitch
>   , rForceKey          :: Maybe AbsPitch
>   , rForceVel          :: Maybe Volume
>   , rTuning            :: Int
>   , rNoteOn            :: NoteOn
>   , rAttenuation       :: Double
>   , rVolEnv            :: Maybe FEnvelope
>   , rPitchCorrection   :: Maybe Double
>   , rM8n               :: Modulation
>   , rEffects           :: Effects} deriving (Eq, Show)
>
> data Effects =
>   Effects {
>     efChorus           :: Double
>   , efReverb           :: Double
>   , efPan              :: Double} deriving (Eq, Show)

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
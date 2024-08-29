> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Synthesizer
William Clements
May 14, 2023

> module Synthesizer ( eutSynthesize, Recon( .. ), defS, defT, stands, scanningOutput, findOutliers, normalizingOutput
>                    , scoreBool, SampleType( .. ), qqDesires', toSampleType, isConfirmed, isConfirmed', isPossible'
>                    , Desires( .. )
>                    , deriveEnvelope, isPossible, qqDesireReStereo, usePitchCorrection, deriveEffects
>                    , useAttenuation, weighHints, weighStereo, weigh24Bit, weighResolution, weighConformance
>                    , weighFuzziness, eutDriver, Effects( .. ) ) where
>
> import qualified Codec.SoundFont         as F
> import Control.Arrow
> import Control.Arrow.ArrowP
> import Control.Arrow.Operations ( ArrowCircuit(delay) )
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Complex ( Complex(..), magnitude )
> import Data.Int ( Int8, Int16 )
> import Data.List ( maximumBy )
> import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
> import Data.Ord ( comparing )
> import Data.Word ( Word64 )
> import Discrete
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..) )
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( constA, DeltaT )
> import Modulation
> import Parthenopea
> import qualified SynToolkit              as STK
  
Signal function-based synth ===========================================================================================

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
> eutSynthesize (reconL, mreconR)
>               sr dur pch vol params s16 ms8
>   | traceNot trace_eS False              = undefined
>   | otherwise                            = sig
>   where
>     noon@NoteOn{ .. }                    = NoteOn vol pch
>
>     secsSample         :: Double         = fromIntegral (reconL.rEnd - reconL.rStart) / sr
>     secsScored         :: Double         = 1 * fromRational dur
>     looping            :: Bool           = secsScored > secsSample
>                                            && (reconL.rSampleMode /= A.NoLoop)
>                                            && useLoopSwitching
>     secsToPlay         :: Double         = if looping
>                                              then secsScored
>                                              else min secsSample secsScored
>
>     freqRatio          :: Double         =
>       if reconL.rTuning == 100
>         then apToHz reconL.rRootKey / apToHz noteOnKey
>         else 1
>     rateRatio          :: Double         = rate (undefined::p) / sr
>     freqFactor         :: Double         = freqRatio * rateRatio / fromMaybe 1 reconL.rPitchCorrection
>     delta              :: Double         = 1 / (secsSample * freqFactor * sr)
>
>     pumped             :: Signal p () (Double, Double)
>     pumped                               = 
>       eutDriver secsScored (reconL, mreconR) secsToPlay delta looping
>         >>> eutPumpSamples            (reconL, mreconR) noon dur s16 ms8
>
>     (m8nL, mm8nR)                        = (reconL.rM8n, fmap rM8n mreconR)
>
>     modulated          :: Signal p () (Double, Double)
>     modulated                            =
>       if ResonanceConvo == m8nL.mLowpass.lowpassType
>         then applyConvolutionStereo (m8nL.mLowpass, maybe m8nL.mLowpass mLowpass mm8nR) secsScored pumped
>         else eutModulate secsScored secsToPlay (m8nL, mm8nR) noon pumped
>
>     sig                                  =       modulated
>                                              >>> eutEffects                (reconL, mreconR)
>                                              >>> eutAmplify     secsScored (reconL, mreconR) noon secsToPlay 
>
>     trace_eS                             =
>       unwords [
>           "eutSynthesize",               show (dur, noon), show (secsSample, secsScored, secsToPlay, looping)] 
>
> eutModulate            :: ∀ p . Clock p ⇒
>                           Double
>                           → Double
>                           → (Modulation, Maybe Modulation)
>                           → NoteOn
>                           → Signal p () (Double, Double)
>                           → Signal p () (Double, Double)
> eutModulate secsScored secsToPlay (m8nL, mm8nR) noon sIn  =
>   if isNothing mm8nR
>     then procSame
>     else procDiff
>   where
>     procSame = proc () → do
>       (modSigL, _)                       ← eutModSignals secsScored secsToPlay (m8nL, mm8nR) ToFilterFc ⤙ ()
>       (a1L, _)                           ← sIn ⤙ ()
>       a2L   ← addResonance noon m8nL     ⤙ (a1L, modSigL)
>       outA                               ⤙ (a2L, a2L)
>     procDiff = proc () → do
>       (modSigL, modSigR)                 ← eutModSignals secsScored secsToPlay (m8nL, mm8nR) ToFilterFc ⤙ ()
>       (a1L, a1R)                         ← sIn ⤙ ()
>       a2L   ← addResonance noon m8nL     ⤙ (a1L, modSigL)
>       a2R   ← addResonance noon m8nL     ⤙ (a1R, modSigR)
>       outA                               ⤙ (a2L, a2R)
>
>
> eutDriver              :: ∀ p . Clock p ⇒
>                           Double
>                           → (Recon, Maybe Recon)
>                           → Double
>                           → Double
>                           → Bool
>                           → Signal p () Double
> eutDriver secsScored (reconL@Recon{rM8n = m8nL, rNoteOn}, mreconR) secsToPlay idelta looping
>   | traceNot trace_eD False              = undefined
>   | otherwise                            = if looping
>                                              then procDriver calcLooping
>                                              else procDriver calcNotLooping
>   where
>     calcLooping, calcNotLooping
>                        :: Double → Double
>     calcLooping next                     = if next > len    then lst           else next
>     calcNotLooping next                  = if next > 1      then frac next     else next
>
>     procDriver calcPhase                 = proc () → do
>       modSig ← eutModSignals secsScored secsToPlay (m8nL, fmap rM8n mreconR) ToPitch ⤙ ()
>       let delta                          = idelta * evaluateModSignals
>                                                       "procDriver"
>                                                       m8nL
>                                                       ToPitch
>                                                       (fst modSig)
>                                                       rNoteOn
>       rec
>         let phase                        = calcPhase next
>         next           ← delay 0         ⤙ frac (phase + delta)                           
>       outA                               ⤙ phase
>
>     (lst, len)         :: (Double, Double)
>                                          = normalizeLooping reconL
>
>     trace_eD                             = unwords ["eutDriver idelta", show idelta, "lst, len", show (lst, len)]
>
> normalizeLooping       :: Recon → (Double, Double)
> normalizeLooping Recon{ .. }             = ((loopst - fullst) / denom, (loopen - fullst) / denom)
>   where
>     (fullst, fullen)                     = (fromIntegral rStart, fromIntegral rEnd)
>     (loopst, loopen)                     = (fromIntegral rLoopStart, fromIntegral rLoopEnd)
>     denom              :: Double         = fullen - fullst
>
> eutModSignals          :: ∀ p. Clock p ⇒
>                           Double → Double
>                           → (Modulation, Maybe Modulation)
>                           → ModDestType
>                           → Signal p () (ModSignals, ModSignals)
> eutModSignals secsScored secsToPlay (m8nL, mm8nR) md
>   | traceIf trace_EMS False             = undefined
>   | otherwise                            = if isNothing mm8nR
>                                              then procSame
>                                              else procDiff
>   where
>     trace_EMS                            = unwords ["eutModSignals", show md]
>     Modulation{mModEnv = mModEnvL, mModLfo = mModLfoL, mVibLfo = mVibLfoL} = m8nL
>     Modulation{mModEnv = mModEnvR, mModLfo = mModLfoR, mVibLfo = mVibLfoR} = fromJust mm8nR
>     (kModEnvL, kModEnvR, kModLfoL, kModLfoR, kVibLfoL, kVibLfoR) = doModSigMaybes 
>     procSame                             = proc _ → do
>       aL1 ← doEnvelope  kModEnvL secsScored secsToPlay ⤙ ()
>       aL2 ← doLFO       kModLfoL                       ⤙ ()
>       aL3 ← doLFO       kVibLfoL                       ⤙ ()
>       let msL                            = ModSignals aL1 aL2 aL3
>       outA                                             ⤙ (msL, msL)
>     procDiff                             = proc _ → do
>       aL1 ← doEnvelope  kModEnvL secsScored secsToPlay ⤙ ()
>       aL2 ← doLFO       kModLfoL                       ⤙ ()
>       aL3 ← doLFO       kVibLfoL                       ⤙ ()
>       aR1 ← doEnvelope  kModEnvR secsScored secsToPlay ⤙ ()
>       aR2 ← doLFO       kModLfoR                       ⤙ ()
>       aR3 ← doLFO       kVibLfoR                       ⤙ ()
>       let msL                            = ModSignals aL1 aL2 aL3
>       let msR                            = ModSignals aR1 aR2 aR3
>       outA                                             ⤙ (msL, msR)
>
>     xModEnvR x                           = mModEnvR
>     xModLfoR x                           = mModLfoR
>     xVibLfoR x                           = mVibLfoR
>
>     doModSigMaybes                       = case md of
>       ToPitch                            → ( mModEnvL,       mm8nR >>= xModEnvR
>                                            , mModLfoL,       mm8nR >>= xModLfoR
>                                            , mVibLfoL,       mm8nR >>= xVibLfoR)
>
>       ToFilterFc                         → ( mModEnvL,       mm8nR >>= xModEnvR
>                                            , mModLfoL,       mm8nR >>= xModLfoR
>                                            , Nothing,        Nothing)
>
>       ToVolume                           → ( Nothing,        Nothing
>                                            , mModLfoL,       mm8nR >>= xModLfoR
>                                            , Nothing,        Nothing)
>       _                                  →
>         error $ unwords["only ToPitch, ToFilterFc, and ToVolume supported in doModSigMaybes, not", show md]
>
> eutPumpSamples         :: ∀ p . Clock p ⇒
>                           (Recon, Maybe Recon)
>                           → NoteOn
>                           → Dur
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p Double (Double, Double)
> eutPumpSamples (reconL, mReconR) noon@NoteOn{noteOnVel, noteOnKey} dur s16 ms8
>                                          = if isNothing mReconR
>                                              then procSame
>                                              else procDiff
>   where
>     Recon{rAttenuation = attenL, rStart = stL, rEnd = enL, rM8n = m8nL} = reconL
>     Recon{rAttenuation = attenR, rStart = stR, rEnd = enR, rM8n = m8nR} = fromJust mReconR
>     procSame                               = proc pos → do
>       let pos'           :: Double         = fromIntegral (enL - stL) * pos
>       let ix             :: Int            = truncate pos'
>       let offset         :: Double         = pos' - fromIntegral ix
>
>       let a1L                              = samplePointInterp s16 ms8 offset (fromIntegral stL + ix) * ampL
>       outA ⤙ (a1L, a1L)
>
>       where
>         graphL                           = modGraph m8nL
>         cAttenL            :: Double     = fromCentibels (attenL + evaluateMods ToInitAtten graphL noon)
>         ampL                             = fromIntegral noteOnVel / 100 / cAttenL
>
>     procDiff                             = proc pos → do
>       let pos'         :: Double         = fromIntegral (enL - stL) * pos
>       let ix           :: Int            = truncate pos'
>       let offset       :: Double         = pos' - fromIntegral ix
>
>       let (a1L, a1R)                     = (  samplePointInterp s16 ms8 offset (fromIntegral stL + ix) 
>                                             , samplePointInterp s16 ms8 offset (fromIntegral stR + ix))
>       outA ⤙ (a1L * ampL, a1R * ampR)
>
>       where
>         (graphL, graphR)                 = (modGraph m8nL, modGraph m8nR)
>         cAttenL        :: Double         = fromCentibels (attenL + evaluateMods ToInitAtten graphL noon)
>         cAttenR        :: Double         = fromCentibels (attenR + evaluateMods ToInitAtten graphR noon)
>         (ampL, ampR)                     = ( fromIntegral noteOnVel / 100 / cAttenL
>                                            , fromIntegral noteOnVel / 100 / cAttenR)
>
> eutAmplify             :: ∀ p . Clock p ⇒
>                           Double
>                           → (Recon, Maybe Recon)
>                           → NoteOn
>                           → Double
>                           → Signal p (Double, Double) (Double, Double)
> eutAmplify   secsScored
>              (reconL, mReconR)
>              noon
>              secsToPlay                  = if isNothing mReconR
>                                              then procSame
>                                              else procDiff
>   where
>     (m8nL, mm8nR)                        = (reconL.rM8n, fmap rM8n mReconR)
>
>     Recon{rVolEnv = envL}   = reconL
>     Recon{rVolEnv = envR}   = fromJust mReconR
>     procSame                             = proc (a1L, a1R)  → do
>       (modSigL, _)                       ← eutModSignals secsScored secsToPlay (m8nL, mm8nR) ToVolume ⤙ ()
>       aenvL                              ← doEnvelope envL secsScored secsToPlay ⤙ ()
>
>       let a2L                            =
>             a1L * aenvL * evaluateModSignals "eutAmplify" m8nL ToVolume modSigL noon
>       outA                               ⤙ (a2L, a2L)
>
>     procDiff                             = proc (a1L, a1R) → do
>       (modSigL, modSigR) ← eutModSignals secsScored secsToPlay (m8nL, mm8nR) ToVolume ⤙ ()
>       aenvL ← doEnvelope envL secsScored secsToPlay ⤙ ()
>       aenvR ← doEnvelope envR secsScored secsToPlay ⤙ ()
>
>       let (a2L, a2R)                     =
>            ( a1L * aenvL * evaluateModSignals "eutAmplify" m8nL ToVolume modSigL noon
>            , a1R * aenvR * evaluateModSignals "eutAmplify" (fromJust mm8nR) ToVolume modSigR noon)
>       outA                               ⤙ (a2L, a2R)

FFT ===================================================================================================================

> findOutliers           :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → IO ()
> findOutliers secs sig                    = do
>   putStrLn (findOutliersString secs sig)
>   return ()
>
> findOutliersString     :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → String
> findOutliersString secs sig              = "findOutliers " ++ show secs
>                                         ++ "..."           ++ show (abs x)
>                                         ++ " ... "         ++ show y
>                                         ++ " / "           ++ show h
>                                         ++ " = "           ++ show z
>   where
>     ss                                   = toSampleDubs (secs + 0.5) sig
>     pers               :: Double         = secs / fromIntegral (length ss)
>     ts                                   = map ((*pers) . fromIntegral) [0..(length ss - 1)]
>     timedPoints                          = zip ts ss 
>     (x, y)                               = maximumBy (comparing (abs . snd)) timedPoints
>     h                  :: Double         = fromIntegral $ length timedPoints
>     z                  :: Double         = secs * abs x / h

Envelopes =============================================================================================================

> data Segments =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show
>
> deriveEnvelope         :: Maybe Int
>                           → Maybe Int
>                           → NoteOn
>                           → (Maybe Int, Maybe Int)
>                           → (Maybe Int, Maybe Int)
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe (Maybe Int, Maybe Int)
>                           → Maybe Envelope
> deriveEnvelope mDelay mAttack NoteOn{noteOnKey} (mHold, mHoldByKey) (mDecay, mDecayByKey)
>                mSustain mRelease mTriple
>   | traceNot trace_DE False              = undefined
>   | otherwise                            = if useEnvelopes && doUse mTriple
>                                              then Just env
>                                              else Nothing
>   where
>     minDeltaT          :: Double         = fromTimecents Nothing
>     dHold              :: Double         = max minDeltaT (fromTimecents' mHold  mHoldByKey  noteOnKey)
>     dDecay             :: Double         = max minDeltaT (fromTimecents' mDecay mDecayByKey noteOnKey)
>
>     env                                  =
>       Envelope (fromTimecents mDelay) (fromTimecents mAttack)      dHold
>                dDecay                 (fromTithe mSustain)         (fromTimecents mRelease)
>                (makeModTriple mTriple)
>
>     doUse              :: Maybe (Maybe Int, Maybe Int) → Bool
>     doUse mTriple                        = case mTriple of
>                                              Nothing           → True
>                                              Just (xToPitch, xToFilterFc)
>                                                                → isJust xToPitch || isJust xToFilterFc
>
>     makeModTriple      :: Maybe (Maybe Int, Maybe Int) → ModTriple
>     makeModTriple mTriple                = case mTriple of
>                                              Nothing           → defModTriple
>                                              Just target       → uncurry deriveModTriple target Nothing
>
>     trace_DE                             =
>       if useEnvelopes
>         then unwords ["deriveEnvelope"
>                       ,  if isJust mTriple then "modEnv" else "volEnv"
>                       , "\nhold=", show mHold, show mHoldByKey, show dHold
>                       , "(not",   show (fromTimecents mHold), ")"
>                       , "\ndecay=", show mDecay, show mDecayByKey, show dDecay
>                       , " (not", show (fromTimecents mDecay), ")"
>                       , "\nmRelease=", show mRelease, "(", show (fromTimecents mRelease), ")"]
>         else unwords ["deriveEnvelope (none)"]
>
> doEnvelope             :: ∀ p . Clock p ⇒ Maybe Envelope → Double → Double → Signal p () Double
> doEnvelope menv secsScored secsToPlay    = case menv of
>                                              Nothing            → constA 1
>                                              Just env           → makeSF env
>   where
>     makeSF             :: Envelope → Signal p () Double
>     makeSF env
>       | traceNever trace_MSF False       = undefined
>       | otherwise                        = dumpSF secsScored secsToPlay sf
>       where
>         Segments{ .. }                   = computeSegments secsScored secsToPlay env
>         sf                               = envLineSeg sAmps sDeltaTs
>
>         trace_MSF                        =
>           unwords ["doEnvelope/makeSF", show (secsScored, secsToPlay), show (sAmps, sDeltaTs)]
>
>     dumpSF             :: Double → Double → Signal p () Double → Signal p () Double
>     dumpSF secsScored secsToUse sigIn
>       | traceNever trace_DSF False       = undefined
>       | otherwise                        = sigIn
>       where
>         trace_DSF                        = unwords ["dumpSF", findOutliersString secsScored sigIn]

Implement the SoundFont envelope model with specified:
  1. delay time                      0 → 0
  2. attack time                     0 → 1
  3. hold time                       1 → 1
  4. decay time                      1 → sus
  5. sustain attenuation level        ---
  6. release time                  sus → 0
          ______
         /      \
        /        \_____   5
       /               \
      /                 \
  ___/                   \
   1    2    3  4      6

Create a straight-line envelope generator with following phases:
 - delay - ~zero most of the time
   attack
   hold
   decay
   sustain
   release  

> computeSegments        :: Double → Double → Envelope → Segments
> computeSegments secsScored secsToPlay Envelope{ .. }
>   | traceNever trace_CS False            = undefined
>   | otherwise                            = Segments amps deltaTs
>   where
>     amps               :: [Double]       = [0,       0,       1,       1,     fSusLevel, fSusLevel,      0,      0]
>     deltaTs            :: [Double]       = [  fDelayT, fAttackT, fHoldT,  fDecayT, fSustainT, fReleaseT,   fPostT]
>
>     minDeltaT          :: Double         = fromTimecents Nothing
>     secsToUse          :: Double         = profess (secsToPlay > 5 * minDeltaT)
>                                                    "time too short for envelope"
>                                                    secsToPlay
>
>
>     fSusLevel          :: Double         = clip (0, 1) eSustainLevel
>
>     -- pin down onset of Release phase
>     rp                                   = secsToUse - min 0.1 (10 * minDeltaT)
>     fReleaseT          :: Double         = secsToUse - rp
>
>     fDelayT                              =
>       clip (minDeltaT, max minDeltaT rp)                                         eDelayT
>     fAttackT                             =
>       clip (minDeltaT, max minDeltaT $ rp - fDelayT)                             eAttackT
>     fHoldT                               =
>       clip (minDeltaT, max minDeltaT $ rp - (fDelayT + fAttackT))                eHoldT
>     fDecayT                              =
>       clip (minDeltaT, max minDeltaT $ rp - (fDelayT + fAttackT + fHoldT))       eDecayT
>     fSustainT                            =
>       max minDeltaT (secsToUse - (fReleaseT + fDelayT + fAttackT + fHoldT + fDecayT + minDeltaT))
>     fPostT                               = (2*minDeltaT) + secsScored - secsToUse
>
>     trace_CS                             = unwords ["computeSegments secs"    , show (secsScored, secsToUse)
>                                                   , "total time"              , show (sum deltaTs)
>                                                   , "rp"                      , show rp]

Effects ===============================================================================================================

> deriveEffects          :: Modulation → NoteOn → Maybe Int → Maybe Int → Maybe Int → Effects
> deriveEffects Modulation{ .. } noon mChorus mReverb mPan
>   | traceNot trace_DE False              = undefined
>   | otherwise                            = Effects
>                                              (dChorus / 1000)
>                                              (dReverb / 1000)
>                                              (dPan / 1000)
>   where
>     dChorus            :: Double         =
>       if useChorus
>         then maybe 0 (fromIntegral . clip (0, 1000)) mChorus + evaluateMods ToChorus modGraph noon
>         else 0
>     dReverb            :: Double         =
>       if useReverb
>         then maybe 0 (fromIntegral . clip (0, 1000)) mReverb + evaluateMods ToReverb modGraph noon
>         else 0
>     dPan               :: Double         =
>       if usePan
>         then maybe 0 (fromIntegral . clip (-500, 500)) mPan
>         else 0
>
>     trace_DE                             =
>       unwords ["deriveEffects", show (mChorus, mReverb, mPan), show (dChorus, dReverb, dPan)]
>
> eutEffects             :: ∀ p . Clock p ⇒
>                           (Recon, Maybe Recon)
>                           → Signal p (Double, Double) (Double, Double)
> eutEffects (Recon{rEffects = effL}, mreconR)
>   | traceNever trace_eE False = undefined
>   | otherwise =
>   proc (aL, aR) → do
>     chL ← eutChorus chorusRate chorusDepth cFactorL      ⤙ aL
>     chR ← eutChorus chorusRate chorusDepth cFactorR      ⤙ aR
>
>     (rbL, rbR) ← eutReverb rFactorL rFactorR             ⤙ (aL, aR)
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
>     effR                                 = maybe effL rEffects mreconR
>     Effects{efChorus = cFactorL, efReverb = rFactorL, efPan = pFactorL} = effL
>     Effects{efChorus = cFactorR, efReverb = rFactorR, efPan = pFactorR} = effR
>
>     trace_eE = unwords ["eutEffects", show ((cFactorL, cFactorR), (rFactorL, rFactorR))]
> 
> eutChorus              :: ∀ p . Clock p ⇒ Double → Double → Double → Signal p Double Double
> eutChorus crate_ cdepth_ cFactor         =
>   if cFactor > 0
>     then makeSF
>     else delay 0                                            
>   where
>     crate                                = professInRange
>                                              (0.1, 100)
>                                              crate_
>                                              "chorus rate"
>                                              crate_
>     cdepth                               = professInRange
>                                              (0.0001, 1.1)
>                                              cdepth_
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
> eutReverb              :: ∀ p . Clock p ⇒ Double → Double → Signal p (Double, Double) (Double, Double)
> eutReverb rFactorL rFactorR              =
>   if rFactorL > 0 || rFactorR > 0
>     then makeSF
>     else delay (0, 0)
>   where
>     roomSize                             = 0.75
>     damp                                 = 0.25
>     width                                = 1.0
>
>     makeSF             :: Signal p (Double, Double) (Double, Double)
>     makeSF                               = eatFreeVerb $ makeFreeVerb roomSize damp width
>   
> fvWetDryMix     = 0.2
> fvCoefficient   = 0.5
> fvFixedGain     = 0.015;
> fvScaleWet      = 3;
> fvScaleDry      = 2;
> fvScaleDamp     = 0.4;
> fvScaleRoom     = 0.28
> fvOffsetRoom    = 0.7
> fvCombDelays           :: [Word64] = [1617, 1557, 1491, 1422, 1356, 1277, 1188, 1116]
> fvAllpassDelays        :: [Word64] = [225, 556, 441, 341]
> fvPole          = 0.9
>
> windices               :: [Word]   = [0..7]
>
> makeFreeVerb           :: Double → Double → Double → STK.FreeVerb
> makeFreeVerb roomSize damp width
>   | traceIf trace_MFV False = undefined
>   | otherwise =
>   let
>     wet = fvScaleWet * fvWetDryMix
>     dry = fvScaleDry * (1 - fvWetDryMix)
>     wet' = wet / (wet + dry)
>     dry' = dry / (wet + dry)
>     wet1 = wet' * (width/2 + 0.5)
>     wet2 = wet' * (1 - width) / 2
>     initCombDelay, initAllpassDelay
>                        :: Array Word Word64
>     initCombFilter     :: Array Word STK.FilterData
>     initCombDelay    = array (0,7) $ zip windices fvCombDelays
>     initAllpassDelay = array (0,3) $ zip windices fvAllpassDelays
>     initCombFilter =   array (0,7) $ zip windices (replicate 8 $ STK.newOnePole 0.9)
>   in
>     STK.FreeVerb fvWetDryMix
>                  fvCoefficient
>                  fvFixedGain
>                  (roomSize * fvScaleRoom + fvOffsetRoom)
>                  (damp * fvScaleDamp)
>                  wet1
>                  wet2
>                  dry'
>                  width
>                  initCombDelay
>                  initCombDelay
>                  initCombFilter
>                  initCombFilter
>                  initAllpassDelay
>                  initAllpassDelay
>   where
>     trace_MFV =
>       unwords [
>           "makeFreeVerb roomSize",       show roomSize
>         , "damp",                        show damp
>         , "width",                       show width]
>   
> eatFreeVerb            :: ∀ p . Clock p ⇒ STK.FreeVerb → Signal p (Double, Double) (Double, Double)
> eatFreeVerb STK.FreeVerb
>   {   STK.iiWetDryMix      {- 0.2    -}
>     , STK.iiG              {- 0.5    -}
>     , STK.iiGain           {- 0.015  -}
>     , STK.iiRoomSize       {- 0.75   -}
>     , STK.iiDamp           {- 0.25   -}
>     , STK.iiWet1           
>     , STK.iiWet2           
>     , STK.iiDry            
>     , STK.iiWidth          {- 1.0    -}
>     , STK.iiCombDelayL     
>     , STK.iiCombDelayR      
>     , STK.iiCombLPL        
>     , STK.iiCombLPR        
>     , STK.iiAllPassDelayL  
>     , STK.iiAllPassDelayR  } =
>
>     proc (sinL, sinR) → do
>       cdL0 ← comb (iiCombDelayL ! 0) (iiCombLPL ! 0) ⤙ sinL
>       cdL1 ← comb (iiCombDelayL ! 1) (iiCombLPL ! 1) ⤙ sinL
>       cdL2 ← comb (iiCombDelayL ! 2) (iiCombLPL ! 2) ⤙ sinL
>       cdL3 ← comb (iiCombDelayL ! 3) (iiCombLPL ! 3) ⤙ sinL
>       cdL4 ← comb (iiCombDelayL ! 4) (iiCombLPL ! 4) ⤙ sinL
>       cdL5 ← comb (iiCombDelayL ! 5) (iiCombLPL ! 5) ⤙ sinL
>       cdL6 ← comb (iiCombDelayL ! 6) (iiCombLPL ! 6) ⤙ sinL
>       cdL7 ← comb (iiCombDelayL ! 7) (iiCombLPL ! 7) ⤙ sinL
>
>       cdR0 ← comb (iiCombDelayR ! 0) (iiCombLPR ! 0) ⤙ sinR
>       cdR1 ← comb (iiCombDelayR ! 1) (iiCombLPR ! 1) ⤙ sinR
>       cdR2 ← comb (iiCombDelayR ! 2) (iiCombLPR ! 2) ⤙ sinR
>       cdR3 ← comb (iiCombDelayR ! 3) (iiCombLPR ! 3) ⤙ sinR
>       cdR4 ← comb (iiCombDelayR ! 4) (iiCombLPR ! 4) ⤙ sinR
>       cdR5 ← comb (iiCombDelayR ! 5) (iiCombLPR ! 5) ⤙ sinR
>       cdR6 ← comb (iiCombDelayR ! 6) (iiCombLPR ! 6) ⤙ sinR
>       cdR7 ← comb (iiCombDelayR ! 7) (iiCombLPR ! 7) ⤙ sinR
>
>       let sumL = cdL0+cdL1+cdL2+cdL3+cdL4+cdL5+cdL6+cdL7
>       let sumR = cdR0+cdR1+cdR2+cdR3+cdR4+cdR5+cdR6+cdR7
>
>       let (fp0L, fp0R) = (sumL/8, sumR/8)
>
>       fp1L ← allpass (iiAllPassDelayL ! 0) ⤙ fp0L
>       fp2L ← allpass (iiAllPassDelayL ! 1) ⤙ fp1L
>       fp3L ← allpass (iiAllPassDelayL ! 2) ⤙ fp2L
>       fp4L ← allpass (iiAllPassDelayL ! 3) ⤙ fp3L
>             
>       fp1R ← allpass (iiAllPassDelayR ! 0) ⤙ fp0R
>       fp2R ← allpass (iiAllPassDelayR ! 1) ⤙ fp1R
>       fp3R ← allpass (iiAllPassDelayR ! 2) ⤙ fp2R
>       fp4R ← allpass (iiAllPassDelayR ! 3) ⤙ fp3R
>
>       outA ⤙ (fp4L, fp4R)
>
> comb                   :: ∀ p . Clock p ⇒ Word64 → STK.FilterData → Signal p Double Double
> comb maxDel filter
>   | traceNever trace_C False             = undefined
>   | otherwise                            =
>   proc sIn → do
>     rec
>       sOut ← delayLine secs ⤙ sIn + sOut * STK.jGain filter
>     outA ⤙ sOut
>   where
>     sr                                   = rate (undefined :: p)
>     secs               :: Double         = fromIntegral maxDel/sr
>
>     trace_C                              = unwords ["comb delay (samples)=", show maxDel, "filter=", show filter]
> 
> allpass                :: ∀ p . Clock p ⇒ Word64 → Signal p Double Double
> allpass maxDel
>   | traceNever trace_AP False            = undefined
>   | otherwise                            =
>   proc sIn → do
>     sOut ← delayLine secs ⤙ sIn
>     outA ⤙ sOut
>   where
>     sr                                   = rate (undefined :: p)
>     secs               :: Double         = fromIntegral maxDel/sr
>
>     trace_AP                             = unwords ["allpass delay (samples)=", show maxDel]
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

Charting ==============================================================================================================

> vdel     = 1.0
> vatt     = vdel + 2.0
> vhold    = vatt + 3.0
> vdec     = vhold + 1.0
> vsus     = 5.00000
> vrel     = vdec + 2.00000
>
> vals'                  :: [(Double, Double, Double, Double)]
> vals' = [ (0     , 0      , 0.3   , 0.3)
>         , (vdel  , 0      , 0.3   , 0.25)
>         , (vatt  , 7      , 0.5   , 0.5)
>         , (vhold , 7      , 0.4   , 0.75)
>         , (vdec  , 7-vsus , 0.3   , 0.10)
>         , (vrel  , 0      , 0     , 0)]

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
>   , rVolEnv            :: Maybe Envelope
>   , rPitchCorrection   :: Maybe Double
>   , rM8n               :: Modulation
>   , rEffects           :: Effects} deriving (Eq, Show)
>
> data Effects =
>   Effects {
>     efChorus           :: Double
>   , efReverb           :: Double
>   , efPan              :: Double} deriving (Eq, Show)
>
> data SampleType =
>   SampleTypeMono
>   | SampleTypeRight
>   | SampleTypeLeft
>   | SampleTypeLinked
>   | SampleTypeOggVorbis
>   | SampleTypeRomMono
>   | SampleTypeRomRight
>   | SampleTypeRomLeft
>   | SampleTypeRomLinked deriving (Eq, Show)
>
> toSampleType               :: Word → SampleType
> toSampleType n =
>   case n of
>     0x1                    → SampleTypeMono
>     0x2                    → SampleTypeRight
>     0x4                    → SampleTypeLeft
>     0x8                    → SampleTypeLinked
>     0x10                   → SampleTypeOggVorbis
>     0x8001                 → SampleTypeRomMono
>     0x8002                 → SampleTypeRomRight
>     0x8004                 → SampleTypeRomLeft
>     0x8008                 → SampleTypeRomLinked
>     _                      → error $ unwords ["bad sample type", show n]
>
> fromSampleType             :: SampleType → Word
> fromSampleType stype =
>   case stype of
>     SampleTypeMono         → 0x1
>     SampleTypeRight        → 0x2
>     SampleTypeLeft         → 0x4
>     SampleTypeLinked       → 0x8
>     SampleTypeOggVorbis    → 0x10
>     SampleTypeRomMono      → 0x8001
>     SampleTypeRomRight     → 0x8002
>     SampleTypeRomLeft      → 0x8004
>     SampleTypeRomLinked    → 0x8008

Flags for customization ===============================================================================================

> data SynthSettings =
>   SynthSettings {
>     qqUsePitchCorrection   :: Bool
>   , qqUseAttenuation       :: Bool
>   , qqUseEnvelopes         :: Bool
>   , qqUseLoopSwitching     :: Bool
>   , qqUseEffectReverb      :: Bool
>   , qqUseEffectChorus      :: Bool
>   , qqUseEffectPan         :: Bool
>   , qqUseEffectDCBlock     :: Bool
>   , qqNormalizingOutput    :: Bool
>   , qqScanningOutput       :: Bool} deriving (Eq, Show)
>
> data ScoringSettings =
>   ScoringSettings {
>     qqDesireReStereo       :: Desires
>   , qqDesireRe24Bit        :: Desires
>   , qqDesireReSplits       :: Desires
>   , qqDesireReConformance  :: Desires
>   , qqDesireReFuzzy        :: Desires
>
>   , qqWeighHints           :: Rational
>   , qqWeighStereo          :: Rational
>   , qqWeigh24Bit           :: Rational
>   , qqWeighResolution      :: Rational
>   , qqWeighConformance     :: Rational
>   , qqWeighFuzziness       :: Rational
>
>   , qqFFThresholdPossible  :: Double
>   , qqFFThresholdStands    :: Double
>   , qqFFThresholdConfirmed :: Double
>
>   , qqEnableSampleAnalysis :: Bool} deriving (Eq, Show)
>
> usePitchCorrection                       = qqUsePitchCorrection         defS
> useAttenuation                           = qqUseAttenuation             defS
> useEnvelopes                             = qqUseEnvelopes               defS
> useLoopSwitching                         = qqUseLoopSwitching           defS
> useReverb                                = qqUseEffectReverb            defS
> useChorus                                = qqUseEffectChorus            defS
> usePan                                   = qqUseEffectPan               defS
> useDCBlock                               = qqUseEffectDCBlock           defS
> normalizingOutput                        = qqNormalizingOutput          defS
> scanningOutput                           = qqScanningOutput             defS
>
> weighHints                               = qqWeighHints                 defT
> weighStereo                              = qqWeighStereo                defT
> weigh24Bit                               = qqWeigh24Bit                 defT
> weighResolution                          = qqWeighResolution            defT
> weighConformance                         = qqWeighConformance           defT
> weighFuzziness                           = qqWeighFuzziness             defT
>
> isPossible                               = qqFFThresholdPossible        defT
> stands                                   = qqFFThresholdStands          defT
> isConfirmed                              = qqFFThresholdConfirmed       defT
>
> isPossible' fuzz                         = fuzz > isPossible
> stands' fuzz                             = fuzz > stands
> isConfirmed' fuzz                        = fuzz > isConfirmed
>
> sampleAnalyisEnabled                     = qqEnableSampleAnalysis       defT
>
> data Desires =
>   DAllOff | DPreferOff | DNeutral | DPreferOn | DAllOn deriving (Eq, Show)
>
> scoreDesire            :: Desires → Rational
> scoreDesire d = case d of
>   DAllOff          → (-1)
>   DPreferOff       → (-1)
>   DNeutral         → 0
>   DPreferOn        → 1
>   DAllOn           → 1
>
> scoreBool              :: Bool → Rational
> scoreBool b = if b then 1 else (-1)
>
> qqDesires              :: [Desires]      = [qqDesireReStereo      defT
>                                           , qqDesireRe24Bit       defT
>                                           , qqDesireReSplits      defT
>                                           , qqDesireReConformance defT
>                                           , qqDesireReFuzzy       defT]
> qqDesires'             :: [Double]       = map (fromRational . scoreDesire) qqDesires

Turn Knobs Here =======================================================================================================

> defT                   :: ScoringSettings
> defT =
>   ScoringSettings {
>     qqDesireReStereo                     = DPreferOn
>   , qqDesireRe24Bit                      = DPreferOn
>   , qqDesireReSplits                     = DPreferOn
>   , qqDesireReConformance                = DPreferOn
>   , qqDesireReFuzzy                      = DPreferOn
>
>   , qqWeighHints                         = 10
>   , qqWeighStereo                        = 5/2
>   , qqWeigh24Bit                         = 0
>   , qqWeighResolution                    = 3/2
>   , qqWeighConformance                   = 3
>   , qqWeighFuzziness                     = 3
>
>   , qqFFThresholdPossible                = 0
>   , qqFFThresholdStands                  = 150
>   , qqFFThresholdConfirmed               = 250
>
>   , qqEnableSampleAnalysis               = False} 
>
> defS                   :: SynthSettings
> defS =
>   SynthSettings {
>     qqUsePitchCorrection                 = True
>   , qqUseAttenuation                     = True
>   , qqUseEnvelopes                       = True
>   , qqUseLoopSwitching                   = True
>   , qqUseEffectReverb                    = True
>   , qqUseEffectChorus                    = True
>   , qqUseEffectPan                       = True
>   , qqUseEffectDCBlock                   = True
>   , qqNormalizingOutput                  = True
>   , qqScanningOutput                     = False}

The End
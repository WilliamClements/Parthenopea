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

> module Synthesizer where
>
> import Control.Arrow
> import Control.Arrow.Operations ( ArrowCircuit(delay) )
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Int ( Int8, Int16 )
> import Data.List ( maximumBy )
> import Data.Maybe
> import Data.Ord ( comparing )
> import Data.Word ( Word64 )
> import Discrete
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..) )
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( constA )
> import Modulation
> import Parthenopea.Debug
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
> eutSynthesize (reconL, mreconR) sr dur pch vol nps s16 ms8
>   | traceNot trace_eS False              = undefined
>   | otherwise                            =
>   if isNothing mreconR
>     then if ResonanceConvo == m8nL.mLowpass.lowpassType
>            then eutSplit <<< pumpMonoConvoPath
>            else eutSplit <<< pumpMonoPath
>     else if ResonanceConvo == m8nL.mLowpass.lowpassType
>            then pumpStereoConvoPath
>            else pumpStereoPath
>   where
>     noon@NoteOn{ .. }                    = NoteOn vol pch
>     reconR                               = fromJust mreconR
>     (m8nL, m8nR)                         = (reconL.rM8n, reconR.rM8n)
>
>     numSamples         :: Double         = fromIntegral (reconL.rEnd - reconL.rStart)
>     secsSample                           = numSamples * freqRatio / sr
>     secsScored                           = 1 * fromRational dur
>     looping            :: Bool           = secsScored > secsSample
>                                            && (reconL.rSampleMode /= A.NoLoop)
>                                            && useLoopSwitching
>     secsToPlay         :: Double         = if looping
>                                              then secsScored
>                                              else min secsSample secsScored
>     freqRatio          :: Double         =
>       case reconL.rTuning of
>       0                                  → 1
>       100                                → apToHz reconL.rRootKey / apToHz noteOnKey
>       _                                  → calcMicrotoneRatio reconL.rRootKey noteOnKey (fromIntegral reconL.rTuning)
>     rateRatio          :: Double         = rate (undefined::p) / sr
>     freqFactor         :: Double         = freqRatio * rateRatio / fromMaybe 1 reconL.rPitchCorrection
>     delta              :: Double         = 1 / (numSamples * freqFactor)
>
>     pumpMonoPath, pumpMonoConvoPath
>                        :: Signal p () Double
>     pumpStereoPath, pumpStereoConvoPath
>                        :: Signal p () (Double, Double)
>     modulateStereo, ampStereo
>                        :: Signal p (Double, Double) (Double, Double)
>
>     pumpMonoConvoPath                    =
>       modulated
>       >>> eutEffectsMono           reconL
>       >>> eutAmplify               secsScored reconL noon secsToPlay
>       where
>         pumped, modulated
>                        :: Signal p () Double
>         pumped                           =
>           eutDriver                secsScored reconL secsToPlay delta looping
>           >>> eutPumpMono          reconL noon dur s16 ms8
>         modulated                            =
>           applyConvolutionMono     m8nL.mLowpass secsScored pumped
>
>     pumpMonoPath                         =
>       eutDriver                    secsScored reconL secsToPlay delta looping
>       >>> eutPumpMono              reconL noon dur s16 ms8
>       >>> eutModulate              secsScored secsToPlay m8nL noon
>       >>> eutEffectsMono           reconL
>       >>> eutAmplify               secsScored reconL noon secsToPlay
>
>     pumpStereoConvoPath                    =
>       modulated
>       >>> modulateStereo
>       >>> ampStereo
>       where
>         pumped, modulated
>                        :: Signal p () (Double, Double)
>         pumped                           =
>           eutDriver                secsScored reconL secsToPlay delta looping
>           >>> eutPumpStereo        (reconL, reconR) noon dur s16 ms8
>         modulated                        =
>           applyConvolutionStereo   (m8nL.mLowpass, m8nR.mLowpass) secsScored pumped
>
>     pumpStereoPath                       = 
>       eutDriver                    secsScored reconL secsToPlay delta looping
>         >>> eutPumpStereo          (reconL, reconR) noon dur s16 ms8
>         >>> modulateStereo
>         >>> ampStereo
>
>     modulateStereo                       =
>       proc (sL, sR) → do
>         mL                               ← eutModulate secsScored secsToPlay m8nL noon ⤙ sL
>         mR                               ← eutModulate secsScored secsToPlay m8nR noon ⤙ sR
>         outA ⤙ (mL, mR)
>
>     ampStereo                            =
>       proc (sL, sR) → do
>         (tL, tR)                         ← eutEffectsStereo (reconL, reconR) ⤙ (sL, sR)
>         mL                               ← eutAmplify secsScored reconL noon secsToPlay ⤙ tL
>         mR                               ← eutAmplify secsScored reconR noon secsToPlay ⤙ tR
>         outA ⤙ (mL, mR)
>
>     trace_eS                             =
>       unwords [
>             "eutSynthesize"
>           , show (dur, noon), show (numSamples, secsSample, secsScored, secsToPlay, looping), show nps] 
>
> eutModulate            :: ∀ p . Clock p ⇒
>                           Double
>                           → Double
>                           → Modulation
>                           → NoteOn
>                           → Signal p Double Double
> eutModulate secsScored secsToPlay m8nL noon =
>   proc a1L → do
>     modSigL                            ← eutModSignals secsScored secsToPlay m8nL ToFilterFc ⤙ ()
>     a2L   ← addResonance noon m8nL     ⤙ (a1L, modSigL)
>     outA                               ⤙ a2L
>
> eutDriver              :: ∀ p . Clock p ⇒
>                           Double
>                           → Recon
>                           → Double
>                           → Double
>                           → Bool
>                           → Signal p () Double
> eutDriver secsScored reconL@Recon{ .. } secsToPlay idelta looping
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
>       modSig                             ← eutModSignals secsScored secsToPlay rM8n ToPitch ⤙ ()
>       let delta                          = idelta * evaluateModSignals "procDriver" rM8n ToPitch modSig rNoteOn
>       rec
>         let phase                        = calcPhase next
>         next           ← delay 0         ⤙ phase + delta                           
>       outA                               ⤙ phase
>
>     (lst, len)         :: (Double, Double)
>                                          = normalizeLooping reconL
>
>     trace_eD                             = unwords ["eutDriver"
>                                                   , "secsScored",     show secsScored
>                                                   , "secsToPlay",     show secsToPlay
>                                                   , "idelta",         show idelta
>                                                   , "lst, len",       show (lst, len)]
>
> normalizeLooping       :: Recon → (Double, Double)
> normalizeLooping Recon{ .. }             = ((loopst - fullst) / denom, (loopen - fullst) / denom)
>   where
>     (fullst, fullen)                     = (fromIntegral rStart, fromIntegral rEnd)
>     (loopst, loopen)                     = (fromIntegral rLoopStart, fromIntegral rLoopEnd)
>     denom              :: Double         = fullen - fullst
>
> eutModSignals          :: ∀ p. Clock p ⇒ Double → Double → Modulation → ModDestType → Signal p () ModSignals
> eutModSignals secsScored secsToPlay m8nL md
>   | traceNot trace_EMS False             = undefined
>   | otherwise                            =
>   proc _ → do
>     aL1 ← doEnvelope  kModEnvL secsScored secsToPlay ⤙ ()
>     aL2 ← doLFO       kModLfoL                       ⤙ ()
>     aL3 ← doLFO       kVibLfoL                       ⤙ ()
>     outA                                             ⤙ ModSignals aL1 aL2 aL3
>   where
>     (kModEnvL, kModLfoL, kVibLfoL)       = doModSigMaybes 
>
>     doModSigMaybes                       = case md of
>       ToPitch                            → ( m8nL.mModEnv, m8nL.mModLfo, m8nL.mVibLfo)
>       ToFilterFc                         → ( m8nL.mModEnv, m8nL.mModLfo, Nothing)
>       ToVolume                           → ( Nothing, m8nL.mModLfo, Nothing)
>       _                                  →
>         error $ unwords["only ToPitch, ToFilterFc, and ToVolume supported in doModSigMaybes, not", show md]
>
>     trace_EMS                            = unwords ["eutModSignals", show md]
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
>     outA                                 ⤙ a1L * ampL
>
>   where
>     Recon{ .. }                          = reconL
>     cAttenL            :: Double         =
>       fromCentibels (rAttenuation + evaluateMods ToInitAtten (mmods rM8n) noon)
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
>     let a1L                              = samplePointInterp s16 ms8 offset (fromIntegral stL + ix) 
>     let a1R                              = samplePointInterp s16 ms8 offset (fromIntegral stR + ix)
>     outA ⤙ (a1L * ampL, a1R * ampR)
>
>   where
>     Recon{rAttenuation = attenL, rStart = stL, rEnd = enL, rM8n = m8nL}
>                                          = reconL
>     Recon{rAttenuation = attenR, rStart = stR, rM8n = m8nR}
>                                          = reconR
>     cAttenL                              = fromCentibels (attenL + evaluateMods ToInitAtten (mmods m8nL) noon)
>     cAttenR                              = fromCentibels (attenR + evaluateMods ToInitAtten (mmods m8nR) noon)
>     ampL                                 = fromIntegral noteOnVel / 100 / cAttenL
>     ampR                                 = fromIntegral noteOnVel / 100 / cAttenR
>
> eutAmplify             :: ∀ p . Clock p ⇒ Double → Recon → NoteOn → Double → Signal p Double Double
> eutAmplify secsScored Recon{ .. } noon secsToPlay
>                                          =
>   proc a1L → do
>     aenvL                                ← doEnvelope rVolEnv secsScored secsToPlay ⤙ ()
>     modSigL                              ← eutModSignals secsScored secsToPlay m8nL ToVolume ⤙ ()
>     let a2L                              = a1L * aenvL * evaluateModSignals "eutAmplify" m8nL ToVolume modSigL noon
>     outA ⤙ a2L
>   where
>     m8nL                                 = rM8n

FFT ===================================================================================================================

> findOutliers           :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → IO ()
> findOutliers secs sig                    = putStrLn $ findOutliersString secs sig
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
>                           → [Double]
>                           → (Maybe Int, Maybe Int)
>                           → (Maybe Int, Maybe Int)
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe (Maybe Int, Maybe Int)
>                           → Maybe Envelope
> deriveEnvelope mDelay mAttack noon _ (mHold, mHoldByKey) (mDecay, mDecayByKey)
>                mSustain mRelease mTriple
>   | traceNot trace_DE False              = undefined
>   | otherwise                            = if useEnvelopes && doUse mTriple
>                                              then Just env
>                                              else Nothing
>   where
>     minDeltaT          :: Double         = fromTimecents Nothing
>     dHold              :: Double         = max minDeltaT (fromTimecents' mHold  mHoldByKey  noon.noteOnKey)
>     dDecay             :: Double         = max minDeltaT (fromTimecents' mDecay mDecayByKey noon.noteOnKey)
>
>     env                                  =
>       Envelope (fromTimecents mDelay) (fromTimecents mAttack)                          dHold
>                dDecay                 (fromTithe mSustain (isNothing mTriple))         (fromTimecents mRelease)
>                (makeModTriple mTriple)
>
>     doUse              :: Maybe (Maybe Int, Maybe Int) → Bool
>     doUse                                =
>       \case
>         Nothing                          → True
>         Just (xToPitch, xToFilterFc)     → isJust xToPitch || isJust xToFilterFc
>
>     makeModTriple      :: Maybe (Maybe Int, Maybe Int) → ModTriple
>     makeModTriple                        =
>       \case
>         Nothing                          → defModTriple
>         Just target                      → uncurry deriveModTriple target Nothing
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
> doEnvelope menv secsScored secsToPlay    = maybe (constA 1) makeSF menv
>   where
>     makeSF             :: Envelope → Signal p () Double
>     makeSF env                           =
>       let
>         segs                             = computeSegments secsScored secsToPlay env
>       in envLineSeg segs.sAmps segs.sDeltaTs

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
> computeSegments secsScored secsToUse_ Envelope{ .. }
>   | traceNot trace_CS False              = undefined
>   | otherwise                            = Segments amps deltaTs
>   where
>     amps               :: [Double]       = [0,       0,       1,       1,     fSusLevel, fSusLevel,      0,      0]
>     deltaTs            :: [Double]       = [  fDelayT, fAttackT, fHoldT,  fDecayT, fSustainT, fReleaseT,   fPostT]
>
>     fSusLevel          :: Double         = clip (0, 1) eSustainLevel
>
>     minDeltaT          :: Double         = fromTimecents Nothing
>     lump                                 = min 0.1 (10 * minDeltaT)
>     secsToUse          :: Double         =
>       profess (secsToUse_ > 7 * minDeltaT)
>         (unwords["computeSegments", show (secsScored, secsToUse_), "..time too short for envelope"])
>         secsToUse_
>
>     reserve_                             = eDelayT + eAttackT + eHoldT + eDecayT
>     reserveOk                            = secsToUse - reserve_ > lump
>     reserve                              = if reserveOk
>                                              then reserve_
>                                              else 4 * minDeltaT
>
>     -- pin down onset of Release phase
>     lump, fReleaseT, rp  :: Double
>     fReleaseT                            = if secsToUse - (reserve + eReleaseT) > lump
>                                              then eReleaseT
>                                              else secsToUse - reserve
>     rp = secsToUse - (fReleaseT + lump)
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
>     trace_CS                             = unwords ["computeSegments", show (amps, deltaTs)]

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
>         then maybe 0 (fromIntegral . clip (0, 1000)) mChorus + evaluateMods ToChorus mmods noon
>         else 0
>     dReverb            :: Double         =
>       if useReverb
>         then maybe 0 (fromIntegral . clip (0, 1000)) mReverb + evaluateMods ToReverb mmods noon
>         else 0
>     dPan               :: Double         =
>       if usePan
>         then maybe 0 (fromIntegral . clip (-500, 500)) mPan
>         else 0
>
>     trace_DE                             =
>       unwords ["deriveEffects", show (mChorus, mReverb, mPan), show (dChorus, dReverb, dPan)]
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
> windices               :: [Word]
> windices                                 = [0..7]
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
>
>     fvWetDryMix     = 0.2
>     fvCoefficient   = 0.5
>     fvFixedGain     = 0.015;
>     fvScaleWet      = 3;
>     fvScaleDry      = 2;
>     fvScaleDamp     = 0.4;
>     fvScaleRoom     = 0.28
>     fvOffsetRoom    = 0.7
>     fvCombDelays           :: [Word64] = [1617, 1557, 1491, 1422, 1356, 1277, 1188, 1116]
>     fvAllpassDelays        :: [Word64] = [225, 556, 441, 341]
>
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
> eatFreeVerb            :: ∀ p . Clock p ⇒ STK.FreeVerb → Signal p Double Double
> eatFreeVerb STK.FreeVerb{ .. }           =
>     proc sinL → do
>       cdL0 ← comb (iiCombDelayL ! 0) (iiCombLPL ! 0) ⤙ sinL
>       cdL1 ← comb (iiCombDelayL ! 1) (iiCombLPL ! 1) ⤙ sinL
>       cdL2 ← comb (iiCombDelayL ! 2) (iiCombLPL ! 2) ⤙ sinL
>       cdL3 ← comb (iiCombDelayL ! 3) (iiCombLPL ! 3) ⤙ sinL
>       cdL4 ← comb (iiCombDelayL ! 4) (iiCombLPL ! 4) ⤙ sinL
>       cdL5 ← comb (iiCombDelayL ! 5) (iiCombLPL ! 5) ⤙ sinL
>       cdL6 ← comb (iiCombDelayL ! 6) (iiCombLPL ! 6) ⤙ sinL
>       cdL7 ← comb (iiCombDelayL ! 7) (iiCombLPL ! 7) ⤙ sinL
>
>       let sumL = cdL0+cdL1+cdL2+cdL3+cdL4+cdL5+cdL6+cdL7
>
>       let fp0L = sumL/8
>
>       fp1L ← allpass (iiAllPassDelayL ! 0) ⤙ fp0L
>       fp2L ← allpass (iiAllPassDelayL ! 1) ⤙ fp1L
>       fp3L ← allpass (iiAllPassDelayL ! 2) ⤙ fp2L
>       fp4L ← allpass (iiAllPassDelayL ! 3) ⤙ fp3L
>             
>       outA ⤙ fp4L
>
> comb                   :: ∀ p . Clock p ⇒ Word64 → STK.FilterData → Signal p Double Double
> comb maxDel stkFilter
>   | traceNever trace_C False             = undefined
>   | otherwise                            =
>   proc sIn → do
>     rec
>       sOut ← delayLine secs ⤙ sIn + sOut * STK.jGain stkFilter
>     outA ⤙ sOut
>   where
>     sr                                   = rate (undefined :: p)
>     secs               :: Double         = fromIntegral maxDel/sr
>
>     trace_C                              = unwords ["comb delay (samples)=", show maxDel, "filter=", show stkFilter]
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

> vals'                  :: [(Double, Double, Double, Double)]
> vals' = [ (0     , 0      , 0.3   , 0.3)
>         , (vdel  , 0      , 0.3   , 0.25)
>         , (vatt  , 7      , 0.5   , 0.5)
>         , (vhold , 7      , 0.4   , 0.75)
>         , (vdec  , 7-vsus , 0.3   , 0.10)
>         , (vrel  , 0      , 0     , 0)]
>   where
>     vdel     = 1.0
>     vatt     = vdel + 2.0
>     vhold    = vatt + 3.0
>     vdec     = vhold + 1.0
>     vsus     = 5.00000
>     vrel     = vdec + 2.00000

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

Flags for customization ===============================================================================================

> usePitchCorrection, useAttenuation, useEnvelopes, useLoopSwitching, useReverb, useChorus, usePan, useDCBlock
>                        :: Bool
> noStereoNoPan, normalizingOutput
>                        :: Bool

Turn Knobs Here =======================================================================================================

> usePitchCorrection                       = True
> useAttenuation                           = True
> useEnvelopes                             = True
> useLoopSwitching                         = True
> useReverb                                = True
> useChorus                                = True
> usePan                                   = True
> useDCBlock                               = True
> noStereoNoPan                            = True
> normalizingOutput                        = True

The End
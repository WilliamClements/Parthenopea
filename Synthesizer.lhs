> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module Synthesizer where
>
> import qualified Codec.SoundFont         as F
> import Control.Arrow
> import Control.Arrow.ArrowP
> import Data.Array.Unboxed ( array, Array, (!), IArray(bounds), listArray )
> import qualified Data.Audio              as A
> import Data.Complex
> import Data.Int ( Int8, Int16 )
> import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
> import Data.Word
> import Euterpea.IO.Audio.Basics
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Render
> import Euterpea.IO.Audio.Types
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import Numeric.FFT
> import Parthenopea
> import qualified SynToolkit              as STK
  
Signal function-based synth ===========================================================================================

> eutSynthesize          :: ∀ p . Clock p ⇒
>                           (Reconciled, Reconciled)
>                           → Double
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p () (Double, Double)
> eutSynthesize (reconL@Reconciled{rModulation = modsL}, reconR@Reconciled{rModulation = modsR})
>               sr dur pch vol params s16 ms8
>   | traceIf msg False                    = undefined
>   | otherwise                            = sig
>   where
>     (st, en)           :: (Word, Word)   = (rStart reconL, rEnd reconL)
>     rk                 :: AbsPitch       = fromIntegral (rRootKey reconL)
>
>     ns                 :: Double         = fromIntegral (en - st)
>     secsSample         :: Double         = ns / sr
>     secsScored         :: Double         = 2 * fromRational dur
>     freqRatio          :: Double         = apToHz rk / apToHz pch
>     rateRatio          :: Double         = rate (undefined::p) / sr
>     freqFactor         :: Double         = freqRatio * rateRatio / fromMaybe 1 (rPitchCorrection reconL)
>     looping            :: Bool           = secsScored > secsSample
>                                            && (rSampleMode reconL /= A.NoLoop)
>                                            && useLoopSwitching
>     delta              :: Double         = 1 / (secsSample * freqFactor * sr)
>
>     sig                :: Signal p () (Double, Double)
>                                          = eutIgniteModSignals modsL secsScored
>                                             >>> (if looping
>                                                    then eutDriverLooping 0 delta modsL (normalizeLooping reconL)
>                                                    else eutDriverNotLooping 0 delta modsL)
>                                             >>>  eutPumpSamples secsScored (reconL, reconR) vol dur s16 ms8
>                                             >>>  eutModulate    secsScored (reconL, reconR)
>                                             >>>  eutEffects     secsScored (reconL, reconR)
>
>     msg                                  = unwords ["eutSynthesize ", show (dur, pch, vol)]
>
> eutDriverNotLooping    :: ∀ p. Clock p ⇒ Double → Double → Modulation → Signal p ModSignals (Double, ModSignals)
> eutDriverNotLooping iphs idelta mods@Modulation{toPitchSummary}
>   | traceIf msg False = undefined
>   | otherwise =
>   proc modSig → do
>     let delta = maybe idelta (modDelta modSig) toPitchSummary
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ (phase, modSig)
>   where
>     modDelta           :: ModSignals → [Double] → Double
>     modDelta modSig fs                   = idelta / calculateModFactor modSig fs
>     msg                                  = unwords [  "eutDriverNotLooping iphs=", show iphs
>                                                     , "idelta=", show idelta
>                                                     , "\nmods=", show mods]
>
> normalizeLooping       :: Reconciled → (Double, Double)
> normalizeLooping recon@Reconciled{rStart, rEnd, rLoopStart, rLoopEnd}
>                                          = ((loopst - fullst) / denom, (loopen - fullst) / denom)
>   where
>     (fullst, fullen)   :: (Double, Double)
>                                          = (fromIntegral rStart, fromIntegral rEnd)
>     (loopst, loopen)   :: (Double, Double)
>                                          = (fromIntegral rLoopStart, fromIntegral rLoopEnd)
>     denom              :: Double         = fullen - fullst
>     
> eutDriverLooping       :: ∀ p . Clock p ⇒ Double → Double →  Modulation → (Double, Double)
>                                            → Signal p ModSignals (Double, ModSignals)
> eutDriverLooping iphs idelta mods@Modulation{toPitchSummary} (lst, len)
>   | traceIf msg False = undefined
>   | otherwise =
>   proc modSig → do
>     let delta = maybe idelta (modDelta modSig) toPitchSummary
>     rec
>       let sentinel     = if next > 1
>                          then len
>                          else 0.9999
>       let phase'       = if phase > sentinel
>                          then lst 
>                          else phase
>       next ← delay iphs ⤙ next + delta
>       phase ← delay iphs ⤙ frac (phase' + delta)
>     outA ⤙ (frac phase', modSig)
>   where
>     modDelta           :: ModSignals → [Double] → Double
>     modDelta modSig fs                   = idelta / calculateModFactor modSig fs
>     msg                                  = unwords ["eutDriverLooping iphs=", show iphs
>                                                    , "idelta=", show idelta
>                                                    , "\nmods=", show mods
>                                                    , "\nlst, len=", show (lst, len)]
>
> eutPumpSamples         :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Volume
>                           → Dur
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p (Double, ModSignals) ((Double, Double), ModSignals)
> eutPumpSamples secsScored (  reconL@Reconciled{rAttenuation = attenL}
>                            , reconR@Reconciled{rAttenuation = attenR}) vol dur s16 ms8
>   | traceIf msg False = undefined
>   | otherwise =
>   proc (pos, modSig) → do
>     let saddrL         :: Int            = fromIntegral stL + truncate (numS * pos)
>     let saddrR         :: Int            = fromIntegral stR + truncate (numS * pos)
>     let (a1L, a1R)                       = (lookupSamplePoint s16 ms8 saddrL, lookupSamplePoint s16 ms8 saddrR)
>     outA ⤙ (pump ampL a1L ampR a1R, modSig)
>
>   where
>     (stL, enL)         :: (Word, Word)   = (rStart reconL, rEnd reconL)
>     (stR, enR)         :: (Word, Word)   = (rStart reconR, rEnd reconR)
>     numS               :: Double         = fromIntegral (enL - stL)
>     (ampL, ampR)       :: (Double, Double)
>                                          = (fromIntegral vol / 100 / attenL, fromIntegral vol / 100 / attenR)
>     pump               :: Double → Double → Double → Double → (Double, Double)
>     pump ampL a1L ampR a1R
>       | traceNever msg' False            = undefined
>       | otherwise                        = (a1L * ampL, a1R * ampR)
>       where
>         msg'                             = unwords ["pump ", show (a1L, a1R)
>                                                     , "<=>", show (a1L*ampL, a1R*ampR)]
>      
>     msg = unwords ["eutPumpSamples numS = ", show numS]

Modulation ============================================================================================================

> deriveModTarget        :: Maybe Int → Maybe Int → Maybe Int → ModTarget
> deriveModTarget toPitch toFilterFc toVolume
>                                          =
>   ModTarget (maybe 0 fromIntegral toPitch)
>             (maybe 0 fromIntegral toFilterFc)
>             (maybe 0 fromIntegral toVolume)
>
> deriveLFO              :: Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe LFO
> deriveLFO del freq toPitch toFilterFc toVolume
>   | traceNever msg False                 = undefined
>   | otherwise                            = if useLFO && anyJust
>                                              then Just $ LFO (fromTimecents del)
>                                                              (maybe 8.176 fromAbsoluteCents freq)
>                                                              (deriveModTarget toPitch toFilterFc toVolume)
>                                              else Nothing
>   where
>     anyJust        :: Bool           = isJust toPitch || isJust toFilterFc || isJust toVolume
>     msg                              = unwords ["deriveLFO ", show toPitch, " ", show toFilterFc, " ", show toVolume]
>
> eutIgniteModSignals    :: ∀ p. Clock p ⇒ Modulation → Double → Signal p () ModSignals
> eutIgniteModSignals mods@Modulation{mModEnv, mModLfo, mVibLfo} secsScored
>                                          =
>   proc _ → do
>     a1 ← doEnvelope  mModEnv  secsScored ⤙ ()
>     b1 ← doLFO       mModLfo             ⤙ ()
>     c1 ← doLFO       mVibLfo             ⤙ ()
>     outA ⤙ ModSignals a1 b1 c1
>
> eutModulate            :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Signal p ((Double, Double), ModSignals) ((Double, Double), ModSignals)
> eutModulate secsScored
>             (rL@Reconciled{rVolEnv = envL, rModulation = modsL}
>            , rR@Reconciled{rVolEnv = envR, rModulation = modsR})
>                                          =
>   proc ((a1L, a1R), modSig) → do
>     aenvL ← doEnvelope envL secsScored   ⤙ ()
>     aenvR ← doEnvelope envR secsScored   ⤙ ()
>
>     a2L   ← addResonance modsL           ⤙ (a1L, modSig)
>     a2R   ← addResonance modsR           ⤙ (a1R, modSig)
>
>     outA                                 ⤙ (modulate a2L aenvL a2R aenvR, modSig)
>
>   where
>     modulate           :: Double → Double → Double → Double → (Double, Double)
>     modulate aL envL aR envR
>       | traceNever msg' False            = undefined
>       | otherwise                        = (aL * envL, aR * envR)
>       where
>         msg'                             = unwords ["modulate ", show (aL, aR)
>                                                         , "<=>", show (aL*envL, aR*envR)]
>
> calculateModFactor     :: ModSignals → [Double] → Double
> calculateModFactor ms@ModSignals{srModEnvPos, srModlfoPos, srViblfoPos} targetList
>  | traceNever msg False                  = undefined
>  | otherwise                             = fromCents (x1 + x2 + x3)
>  where
>    x1                                    = srModEnvPos * head targetList
>    x2                                    = srModlfoPos * (targetList !! 1)
>    x3                                    = srViblfoPos * (targetList !! 2)
>
>    msg                                   = unwords ["\ncalculateVibFactor=", show (x1, x2, x3)]
>
> addResonance           :: ∀ p . Clock p ⇒ Modulation → Signal p (Double, ModSignals) Double
> addResonance mods@Modulation{mLowPass, toFilterFcSummary}
>                                          = maybe delay' makeSF mLowPass
>   where
>     makeSF             :: LowPass → Signal p (Double, ModSignals) Double
>     makeSF lp@LowPass{lowPassFc, lowPassQ} 
>                                          =
>       proc (x, modSig) → do
>         let fc = maybe lowPassFc (modFc lowPassFc modSig) toFilterFcSummary
>         y ← filterLowPassBW              ⤙ (x, fc)
>         outA                             ⤙ y
>
>     modFc          :: Double → ModSignals → [Double] → Double
>     modFc fc ms fs                   = fc / calculateModFactor ms fs
>                             
>     delay'             :: Signal p (Double, ModSignals) Double
>                                          =
>       proc (x, _) -> do
>         y <- delay 0 -< x  
>         outA -< y
>
> doLFO                  :: ∀ p . Clock p ⇒ Maybe LFO → Signal p () Double
> doLFO                                    = maybe (constA 0) makeSF
>   where
>     makeSF             :: LFO → Signal p () Double
>     makeSF lfo@LFO{lfoDelay, lfoFrequency}
>                                          = 
>       proc _ → do
>         y ← triangleWave lfoFrequency    ⤙ ()
>         z ← delayLine lfoDelay           ⤙ y
>         outA                             ⤙ z  
>

FFT ===================================================================================================================

> eutAnalyzeSample       :: A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → F.Shdr
>                           → AbsPitch
>                           → [[Double]]
> eutAnalyzeSample s16 ms8 shdr ap
>   | traceAlways msgT False               = undefined
>   | otherwise                            = vFft
>   where
>     (st, en)           :: (Word, Word)   = (F.start shdr,     F.end shdr)
>     sr                 :: Double         = fromIntegral $ F.sampleRate shdr
>     rootf              :: Double         = apToHz ap
>     pc                 :: Double         = fromCents $ fromIntegral $ F.pitchCorrection shdr
>     nsI                :: Int            = fromIntegral $ min (F.sampleRate shdr) (en - st + 1)
>     nsD                :: Double         = fromIntegral nsI
>
>     (st', en')         :: (Int, Int)     = (fromIntegral st, fromIntegral st + nsI)
>     raw                :: [Double]       = map (lookupSamplePoint s16 ms8) [st'..en'- 1]
>
>     maxval             :: Double         = maximum (map abs raw)
>     vNormed            :: Array Int Double
>                                          = listArray (0, length raw - 1) (map (/maxval) raw)
>
>     fp                 :: Int            = 256
>     qf                 :: Int            = 100
>
>     fpOrig             :: Double         = sr / rootf -- WOX ? / pc
>     tFactor            :: Double         = fromIntegral fp / fpOrig
>     nsD'               :: Double         = nsD * tFactor
>     nsI'               :: Int            = truncate nsD'
>     nGo                                  = nsI' - 1
>
>     vResampled         :: Array Int Double
>                                          = listArray (0, nGo) (map (interpVal tFactor vNormed) [0..nGo])
>
>     interpVal          :: Double → Array Int Double → Int → Double
>     interpVal fact vSource ixAt          = profess
>                                              (ix0 < (snd . bounds) vSource)
>                                              "out of range (interpVal)"
>                                              (dFirst + (dSecond - dFirst) * (dixAt - fromIntegral ix0))
>       where
>         dixAt          :: Double         = fromIntegral ixAt / fact
>         ix0            :: Int            = truncate dixAt
>         dFirst         :: Double         = vSource ! ix0
>         dSecond        :: Double         = if ix0 == (snd . bounds) vSource
>                                              then dFirst
>                                              else vSource ! (ix0 + 1)
>
>         msg                              = unwords ["interpVal ", show (ixAt, dixAt, ix0)]
>
>     nSize                                = (snd . bounds) vResampled + 1
>     nChunks                              = if tFactor < 0.1 || tFactor > 100.0 || nSize < 100
>                                              then 0
>                                              else max 0 ((nSize - (nSize `mod` fp)) `div` (2*qf))
>     chunked            :: [[Complex Double]]
>                                          = doChunks nChunks vResampled 
>
>     doChunks            :: Int → Array Int Double → [[Complex Double]]
>     doChunks nToDo vect                  = profess
>                                              (nToDo * qf < (snd . bounds) vect)
>                                              "out of range (doChunks)"
>                                              (map (doChunk vect) (take nToDo [0,qf..]))
>
>     doChunk            :: Array Int Double → Int → [Complex Double]
>     doChunk vect ix                      = profess
>                                              (ix + fp < (snd . bounds) vect)
>                                              "out of range (doChunks)"
>                                              (map (\i → (vect ! i) :+ 0) [ix .. (ix + fp - 1)])
>
>     vFft               :: [[Double]]     = fmap (map magnitude . take (fp `div` 2) . fft) chunked
>
>     msgT       = unwords ["eutAnalyzeSample "
>                        , "fpOrig =", show fpOrig, " , tFactor =", show tFactor
>                        , "bounds vNormed =", show (bounds vNormed)
>                        , "length vResampled =", show (length vResampled)
>                        , " nsI, nsI' =", show nsI, " ", show nsI', "\n"]

Envelopes =============================================================================================================

> deriveEnvelope         :: Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe (Maybe Int, Maybe Int)
>                           → Maybe Envelope
> deriveEnvelope mDelay mAttack mHold mDecay mSustain mRelease mTarget
>   | traceIf msg False                   = undefined
>   | otherwise                            = if useEnvelopes && doUse mTarget
>                                              then Just env
>                                              else Nothing
>     where
>       inp                                = [mDelay, mAttack, mHold, mDecay, mSustain, mRelease]
>       env                                = Envelope (fromTimecents mDelay) (fromTimecents mAttack)      (fromTimecents mHold)
>                                                     (fromTimecents mDecay) (fromTithe mSustain)         (fromTimecents mRelease)
>                                                     (makeModTarget mTarget)
>
>       doUse            :: Maybe (Maybe Int, Maybe Int) → Bool
>       doUse mTarget                      = case mTarget of
>                                              Nothing           → True
>                                              Just target       → (isJust . fst) target || (isJust . snd) target
>
>       makeModTarget    :: Maybe (Maybe Int, Maybe Int) → ModTarget
>       makeModTarget mTarget              = case mTarget of
>                                              Nothing           → defModTarget
>                                              Just target       → uncurry deriveModTarget target Nothing
>
>       msg                                = if useEnvelopes
>                                              then unwords ["deriveEnvelope mDelay=",   show mDelay
>                                                                        , "mAttack=",   show mAttack
>                                                                        , "mHold=",     show mHold
>                                                                        , "mDecay=",    show mDecay
>                                                                        , "mSustain=",  show mSustain
>                                                                        , "mRelease=",  show mRelease
>                                                                        , "mTarget=",   show mTarget]
>                                              else unwords ["deriveEnvelope (none)"]
>
> doEnvelope             :: ∀ p . Clock p ⇒ Maybe Envelope → Double → Signal p () Double
> doEnvelope menv secsScored               = case menv of
>                                              Nothing            → constA 1
>                                              Just env           → makeSF env
>   where
>     makeSF             :: Envelope → Signal p () Double
>     makeSF env
>       | traceIf msg False                = undefined
>       | otherwise                        = envLineSeg sAmps sDeltaTs
>       where
>         segs@Segments{sAmps, sDeltaTs}   = computeSegments secsScored env
>         msg = unwords ["doEnvelope/makeSF ", show (sAmps, sDeltaTs)]

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

> computeSegments        :: Double → Envelope → Segments
> computeSegments secsScored rEnv = Segments amps deltaTs
>   where
>     susL               :: Double         = eSustainLevel rEnv
>     minA               :: Double         = minAttackTime
>     minR               :: Double         = minReleaseTime
>
>     maxOnsetRelease    :: Double         = max 0 (secsScored - minR)
>     amps               :: [Double]       = [0, 0, 1, 1, susL, susL, 0, 0]
>     -- delay time
>     del                :: Double         = eDelayT rEnv
>     del'               :: Double         = if del >= maxOnsetRelease
>                                              then maxOnsetRelease
>                                              else del
>     -- attack time
>     att                :: Double         = max minA (eAttackT rEnv)
>     att'               :: Double         = if del' + att >= maxOnsetRelease
>                                              then max 0 (maxOnsetRelease - del')
>                                              else att
>     -- hold time
>     hold               :: Double         = eHoldT rEnv
>     hold'              :: Double         = if del' + att' + hold > maxOnsetRelease
>                                              then max 0 (maxOnsetRelease - (del' + att'))
>                                              else hold
>     -- decay time
>     dec                :: Double         = eDecayT rEnv
>     dec'               :: Double         = if del' + att' + hold' + dec > maxOnsetRelease
>                                              then max 0 (maxOnsetRelease - (del' + att' + hold'))
>                                              else dec
>     -- release time
>     rel                :: Double         = eReleaseT rEnv
>     rel'               :: Double         = if del' + att' + hold' + dec' + rel > secsScored
>                                              then minR
>                                              else rel
>     -- sustain time
>     susT               :: Double         = max 0 (secsScored - (del' + att' + hold' + dec' + rel'))
>    
>     deltaTs = [del', att', hold', dec', susT, rel', minR]

Effects ===============================================================================================================

> deriveEffects          :: Maybe Int → Maybe Int → Maybe Int → Effects
> deriveEffects mChorus mReverb mPan =
>   Effects (fmap (conv (0, 1000)) mChorus)
>           (fmap (conv (0, 1000)) mReverb)
>           (fmap (conv (-500, 500)) mPan)
>   where
>     conv               :: (Int, Int) → Int → Double
>     conv range nEffect = fromIntegral (clip range nEffect) / 1000
>
> eutEffects             :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Signal p ((Double, Double), ModSignals) (Double, Double)
> eutEffects secsScored (reconL@Reconciled{rEffects = effL}, reconR@Reconciled{rEffects = effR})
>   | traceIf msg False = undefined
>   | otherwise =
>   proc ((aL, aR), modSig) → do
>     (chL, chR) ← if cFactorL <= 0 || cFactorR <= 0
>                  then delay (0,0) ⤙ (aL, aR)
>                  else eutChorus {- 15.0 0.005 -} 0.1 ⤙ (aL, aR)
>     (rbL, rbR) ← if rFactorL <= 0 || rFactorR <= 0
>                  then delay (0,0) ⤙ (aL, aR)
>                  else eutReverb 0.75 0.25 1.0 ⤙ (aL, aR)
>
>     let mixL = (cFactorL * chL
>                 + rFactorL * rbL
>                 + (1 - cFactorL) * aL
>                 + (1 - rFactorL) * aL) / 2
>     let mixR = (cFactorR * chR
>                 + rFactorR * rbR
>                 + (1 - cFactorR) * aR
>                 + (1 - rFactorR) * aR) / 2
>
>     let (pL, pR) = doPan (pFactorL, pFactorR) (mixL, mixR)
>
>     pL' ←        if not useDCBlock
>                  then delay 0       ⤙ pL
>                  else dcBlock 0.995 ⤙ pL
>     pR' ←        if not useDCBlock
>                  then delay 0       ⤙ pR
>                  else dcBlock 0.995 ⤙ pR
>     outA ⤙ (pL', pR')
>
>   where
>     cFactorL = if useChorus
>                then fromMaybe 0 (efChorus effL)
>                else 0.0
>     cFactorR = if useChorus
>                then fromMaybe 0 (efChorus effR)
>                else 0.0
>     rFactorL = if useReverb
>                then fromMaybe 0 (efReverb effL)
>                else 0.0
>     rFactorR = if useReverb
>                then fromMaybe 0 (efReverb effR)
>                else 0.0
>     pFactorL = if usePan
>                then fromMaybe 0 (efPan effL)
>                else 0.0
>     pFactorR = if usePan
>                then fromMaybe 0 (efPan effR)
>                else 0.0
>
>     msg = unwords ["eutEffects=", show effL, "=LR=", show effR, "rFactor*=", show rFactorL, " ", show rFactorR]
> 
> eutChorus              :: ∀ p . Clock p ⇒
>                           Double
>                           → Signal p (Double, Double) (Double, Double)
> eutChorus gain =
>   proc (sinL, sinR) → do
>     z1L ← delayLine 0.010 ⤙ sinL
>     z2L ← delayLine 0.020 ⤙ sinL
>     z3L ← delayLine 0.030 ⤙ sinL
>
>     z1R ← delayLine 0.010 ⤙ sinR
>     z2R ← delayLine 0.020 ⤙ sinR
>     z3R ← delayLine 0.030 ⤙ sinR
>
>     rec
>       rL ← delayLine 0.0001 ⤙ (sinL + z1L + z2L + z3L)/4 + rL * gain
>       rR ← delayLine 0.0001 ⤙ (sinR + z1R + z2R + z3R)/4 + rR * gain
>
>     outA ⤙ (rL, rR)
>
> eutReverb              :: ∀ p . Clock p ⇒
>                           Double
>                           → Double
>                           → Double
>                           → Signal p (Double, Double) (Double, Double)
> eutReverb roomSize damp width =
>   eatFreeVerb $ makeFreeVerb roomSize damp width
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
>   | traceIf msg False = undefined
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
>     msg = unwords [   "makeFreeVerb roomSize="
>                     , show roomSize
>                     , " damp="
>                     , show damp
>                     , " width="
>                     , show width]
>   
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
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     sr = rate (undefined :: p)
>   in proc sin → do
>     rec
>       r ← delayLine (fromIntegral maxDel/sr) ⤙ sin + r * STK.jGain filter
>     outA ⤙ r
>   where
>     msg = unwords ["comb delay (samples)=", show maxDel, " filter=", show filter]
> 
> allpass                :: ∀ p . Clock p ⇒ Word64 → Signal p Double Double
> allpass maxDel
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     sr = rate (undefined :: p)
>   in proc sin → do
>     r ← delayLine (fromIntegral maxDel/sr) ⤙ sin
>     outA ⤙ r
>   where
>     msg = unwords ["allpass delay (samples)=", show maxDel]
> 
> doPan                  :: (Double, Double) → (Double, Double) → (Double, Double)
> doPan (azimuthL, azimuthR) (sinL, sinR)
>   | traceNever msg False                 = undefined
>   | otherwise                            = ((ampLL + ampRL)/2, (ampLR + ampRR)/2)
>   where
>     ampLL = sinL * cos ((azimuthL + 0.5) * pi / 2)
>     ampLR = sinL - ampLL
>     ampRL = sinR * cos ((azimuthR + 0.5) * pi / 2)
>     ampRR = sinR - ampRL
>
>     msg = unwords ["doPan ", show ampLL, " ", show ampRL, " ", show ampRL, " ", show ampRR]
>
> dcBlock                :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> dcBlock a = proc xn → do
>   rec
>     let yn = xn - xn_l + a * yn_l
>     xn_l ← delay 0 ⤙ xn
>     yn_l ← delay 0 ⤙ yn
>   outA ⤙ yn

Miscellaneous =========================================================================================================

> triangleWaveTable      :: Table
> triangleWaveTable                        = tableSinesN 16384 
>                                                          [      1,  0, -0.5,  0,  0.3,   0
>                                                           , -0.25,  0,  0.2,  0, -0.167, 0
>                                                           ,  0.14,  0, -0.125]
>
> triangleWave           :: ∀ p . Clock p ⇒ Double → Signal p () Double
> triangleWave freq                           = 
>   proc _ → do
>     osc triangleWaveTable 0 ⤙ freq
>
> sawtoothTable = tableSinesN 16384 
>                   [1, 0.5, 0.3, 0.25, 0.2, 0.167, 0.14, 0.125, 0.111]
> sawtooth               :: ∀ p . Clock p ⇒  Double → Signal p () Double
> sawtooth freq                           = 
>   proc _ → do
>     osc sawtoothTable 0 ⤙ freq
>

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

> data Reconciled =
>   Reconciled {
>     rSampleMode        :: A.SampleMode
>   , rSampleRate        :: Double
>   , rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: AbsPitch
>   , rForceKey          :: Maybe AbsPitch
>   , rForceVel          :: Maybe Volume
>   , rAttenuation       :: Double
>   , rVolEnv            :: Maybe Envelope
>   , rPitchCorrection   :: Maybe Double
>   , rModulation        :: Modulation
>   , rEffects           :: Effects} deriving (Eq, Show)
>           
> data Envelope =
>   Envelope {
>     eDelayT            :: Double
>   , eAttackT           :: Double
>   , eHoldT             :: Double
>   , eDecayT            :: Double
>   , eSustainLevel      :: Double
>   , eReleaseT          :: Double
>   , eModTarget         :: ModTarget} deriving (Eq, Show)
>
> data Segments =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show
>
> data LowPass =
>   LowPass {
>     lowPassFc          :: Double
>   , lowPassQ           :: Double} deriving (Eq, Show)
>
> data ModTarget =
>   ModTarget {
>     toPitch            :: Double
>   , toFilterFc         :: Double
>   , toVolume           :: Double} deriving (Eq, Show)
>
> defModTarget                             = ModTarget 0 0 0
>
> data LFO =
>   LFO {
>     lfoDelay           :: Double
>   , lfoFrequency       :: Double
>   , lModTarget         :: ModTarget} deriving (Eq, Show)
>
> data Modulation =
>   Modulation {
>     mLowPass           :: Maybe LowPass
>   , mModEnv            :: Maybe Envelope
>   , mModLfo            :: Maybe LFO
>   , mVibLfo            :: Maybe LFO
>   , toPitchSummary     :: Maybe [Double]
>   , toFilterFcSummary  :: Maybe [Double]
>   , toVolumeSummary    :: Maybe [Double]} deriving (Eq, Show)
>
> data Effects =
>   Effects {
>     efChorus           :: Maybe Double
>   , efReverb           :: Maybe Double
>   , efPan              :: Maybe Double} deriving (Eq, Show)
>
> data ModSignals =
>   ModSignals {
>     srModEnvPos        :: Double
>   , srModlfoPos        :: Double
>   , srViblfoPos        :: Double} deriving (Show)
>
> data SampleType =
>   SampleTypeMono
>   | SampleTypeRight
>   | SampleTypeLeft
>   | SampleTypeLinked
>   | SampleTypeOggVorbis
>   | SampleTypeRom deriving (Eq, Show)
>
> toSampleType           :: Word → SampleType
> toSampleType n =
>   case n of
>     0x1                → SampleTypeMono
>     0x2                → SampleTypeRight
>     0x4                → SampleTypeLeft
>     0x8                → SampleTypeLinked
>     0x10               → SampleTypeOggVorbis
>     0x8000             → SampleTypeRom
>
> fromSampleType      :: SampleType → Word
> fromSampleType stype =
>   case stype of
>     SampleTypeMono         → 0x1
>     SampleTypeRight        → 0x2
>     SampleTypeLeft         → 0x4
>     SampleTypeLinked       → 0x8
>     SampleTypeOggVorbis    → 0x10
>     SampleTypeRom          → 0x8000

Flags for customization ===============================================================================================

> data SynthSettings =
>   SynthSettings {
>     qqUsePitchCorrection   :: Bool
>   , qqUseAttenuation       :: Bool
>   , qqUseEnvelopes         :: Bool
>   , qqMinAttackTime        :: Double
>   , qqMinReleaseTime       :: Double
>   , qqUseLoopSwitching     :: Bool
>   , qqUseLowPass           :: Bool
>   , qqUseLFO               :: Bool
>   , qqUseEffectReverb      :: Bool
>   , qqUseEffectChorus      :: Bool
>   , qqUseEffectPan         :: Bool
>   , qqUseEffectDCBlock     :: Bool
>   , qqNormalizingOutput    :: Bool} deriving (Eq, Show)
>
> data ScoringSettings =
>   ScoringSettings {
>     qqDesireReStereo       :: Desires
>   , qqDesireRe24Bit        :: Desires
>   , qqDesireReSplitCharacteristic
>                            :: Desires
>   , qqDesireReConformance  :: Desires
>   , qqDesireReFuzzy        :: Desires
>
>   , qqWeighHints           :: Int
>   , qqWeighStereo          :: Int
>   , qqWeigh24Bit           :: Int
>   , qqWeighSplits          :: Int
>   , qqWeighConformance     :: Int
>   , qqWeighFuzziness       :: Int
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
> minAttackTime                            = qqMinAttackTime              defS
> minReleaseTime                           = qqMinReleaseTime             defS
> useLoopSwitching                         = qqUseLoopSwitching           defS
> useLowPass                               = qqUseLowPass                 defS
> useLFO                                   = qqUseLFO                     defS
> useReverb                                = qqUseEffectReverb            defS
> useChorus                                = qqUseEffectChorus            defS
> usePan                                   = qqUseEffectPan               defS
> useDCBlock                               = qqUseEffectDCBlock           defS
> normalizingOutput                        = qqNormalizingOutput          defS
>
> weighHints                               = qqWeighHints                 defT
> weighStereo                              = qqWeighStereo                defT
> weigh24Bit                               = qqWeigh24Bit                 defT
> weighSplits                              = qqWeighSplits                defT
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
> scoreDesire            :: Desires → Int
> scoreDesire d = case d of
>   DAllOff          → (-1)
>   DPreferOff       → (-1)
>   DNeutral         → 0
>   DPreferOn        → 1
>   DAllOn           → 1
>
> scoreBool              :: Bool → Int
> scoreBool b = if b then 1 else (-1)
>

Turn Knobs Here =======================================================================================================

> defT                   :: ScoringSettings
> defT =
>   ScoringSettings {
>     qqDesireReStereo                     = DPreferOn
>   , qqDesireRe24Bit                      = DPreferOn
>   , qqDesireReSplitCharacteristic        = DPreferOn
>   , qqDesireReConformance                = DPreferOn
>   , qqDesireReFuzzy                      = DPreferOn
>
>   , qqWeighHints                         = 5
>   , qqWeighStereo                        = 3
>   , qqWeigh24Bit                         = 0
>   , qqWeighSplits                        = 1
>   , qqWeighConformance                   = 3
>   , qqWeighFuzziness                     = 1
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
>   , qqUseAttenuation                     = False
>   , qqUseEnvelopes                       = True
>   , qqMinAttackTime                      = 0 -- 1/64
>   , qqMinReleaseTime                     = 0 -- 1/16
>   , qqUseLoopSwitching                   = True
>   , qqUseLowPass                         = False
>   , qqUseLFO                             = True
>   , qqUseEffectReverb                    = True
>   , qqUseEffectChorus                    = True
>   , qqUseEffectPan                       = True
>   , qqUseEffectDCBlock                   = True
>   , qqNormalizingOutput                  = True}
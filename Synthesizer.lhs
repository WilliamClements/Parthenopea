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
> import Data.Complex ( Complex(..), magnitude )
> import Data.Int ( Int8, Int16 )
> import Data.List ( maximumBy )
> import qualified Data.Map                as Map
> import Data.Ord ( comparing )
> import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
> import Data.Word ( Word64 )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..) )
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import Numeric.FFT ( fft )
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
> eutSynthesize (reconL@Reconciled{rSampleMode, rStart, rEnd, rRootKey, rModulation}, reconR)
>               sr dur pch vol params s16 ms8
>   | traceIf msg False                    = undefined
>   | otherwise                            = sig
>   where
>     ns                 :: Double         = fromIntegral (rEnd - rStart)
>     secsSample         :: Double         = ns / sr
>     secsScored         :: Double         = 2 * fromRational dur
>     looping            :: Bool           = secsScored > secsSample
>                                            && (rSampleMode /= A.NoLoop)
>                                            && useLoopSwitching
>     secsToPlay         :: Double         = if looping
>                                              then secsScored
>                                              else min secsSample secsScored
>
>     freqRatio          :: Double         = apToHz (fromIntegral rRootKey) / apToHz pch
>     rateRatio          :: Double         = rate (undefined::p) / sr
>     freqFactor         :: Double         = freqRatio * rateRatio / fromMaybe 1 (rPitchCorrection reconL)
>     delta              :: Double         = 1 / (secsSample * freqFactor * sr)
>
>     sig                :: Signal p () (Double, Double)
>                                          =       eutDriver      secsScored (reconL, reconR) secsToPlay delta looping
>                                              >>> eutPumpSamples secsScored (reconL, reconR) vol dur s16 ms8
>                                              >>> eutModulate    secsScored (reconL, reconR)
>                                              >>> eutEffects     secsScored (reconL, reconR)
>                                              >>> eutAmplify     secsScored (reconL, reconR) secsToPlay
>
>     msg                                  = unwords [ "eutSynthesize ", show (dur, pch, vol)
>                                                    , "ns, sr, ns/sr= ", show (ns, sr, ns/sr)
>                                                    , "\nsample, scored, toplay = "
>                                                    , show secsSample, " , ", show secsScored, " , ", show secsToPlay]
>
> eutDriver              :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Double
>                           → Double
>                           → Bool
>                           → Signal p () (Double, (ModSignals, ModSignals))
> eutDriver secsScored (reconL@Reconciled{rModulation, rNoteOnVel, rNoteOnKeyNumber}, reconR) secsToPlay idelta looping
>   | traceIf msg False                    = undefined
>   | otherwise                            = if looping
>                                              then procDriver calcLooping
>                                              else procDriver calcNotLooping
>   where
>     calcLooping, calcNotLooping
>                        :: Double → Double
>     calcLooping next                     = if next > len    then lst           else next
>     calcNotLooping next                  = if next > 1      then frac next     else next
>
>     m@Modulation{toPitchSummary}         = rModulation
>     procDriver calcPhase                 = proc () → do
>       (modSigL, modSigR) ← eutIgniteModSignals secsScored secsToPlay (reconL, reconR)
>                                          ⤙ ()
>       let delta                          = idelta * calculateModFactor
>                                                       "eutDriver"
>                                                       m
>                                                       ToPitch
>                                                       modSigL
>                                                       (rNoteOnVel, rNoteOnKeyNumber)
>       rec
>         let phase                        = calcPhase next
>         next           ← delay 0         ⤙ frac (phase + delta)                           
>       outA                               ⤙ (phase, (modSigL, modSigR))
>
>     (lst, len)         :: (Double, Double)
>                                          = normalizeLooping reconL
>
>     msg                                  = unwords ["eutDriver idelta=", show idelta, " looping=", show looping]
>
> normalizeLooping       :: Reconciled → (Double, Double)
> normalizeLooping r@Reconciled{rStart, rEnd, rLoopStart, rLoopEnd}
>                                          = ((loopst - fullst) / denom, (loopen - fullst) / denom)
>   where
>     (fullst, fullen)   :: (Double, Double)
>                                          = (fromIntegral rStart, fromIntegral rEnd)
>     (loopst, loopen)   :: (Double, Double)
>                                          = (fromIntegral rLoopStart, fromIntegral rLoopEnd)
>     denom              :: Double         = fullen - fullst
>
> eutPumpSamples         :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Volume
>                           → Dur
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Signal p (Double, (ModSignals, ModSignals)) ((Double, Double), (ModSignals, ModSignals))
> eutPumpSamples _ (  reconL@Reconciled{rAttenuation = attenL, rStart = stL, rEnd = enL}
>                   , reconR@Reconciled{rAttenuation = attenR, rStart = stR, rEnd = enR}) vol dur s16 ms8
>   | traceIf msg False = undefined
>   | otherwise =
>   proc (pos, msig) → do
>     let saddrL         :: Int            = fromIntegral stL + truncate (numS * pos)
>     let saddrR         :: Int            = fromIntegral stR + truncate (numS * pos)
>     let (a1L, a1R)                       = (lookupSamplePoint s16 ms8 saddrL, lookupSamplePoint s16 ms8 saddrR)
>     outA ⤙ (pump (ampL, ampR) (a1L, a1R), msig)
>
>   where
>     (ampL, ampR)       :: (Double, Double)
>                                          = (fromIntegral vol / 100 / attenL, fromIntegral vol / 100 / attenR)
>     numS               :: Double         = fromIntegral (enL - stL)
>
>     pump               :: (Double, Double) → (Double, Double) → (Double, Double)
>     pump (ampL, ampR) (a1L, a1R)
>       | traceNever msg' False            = undefined
>       | otherwise                        = (a1L * ampL, a1R * ampR)
>       where
>         msg'                             = unwords ["pump ", show (a1L, a1R)
>                                                     , "<=>", show (a1L*ampL, a1R*ampR)]
>      
>     msg = unwords ["eutPumpSamples numS = ", show numS]
>
> eutAmplify             :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Double
>                           → Signal p (Double, Double) (Double, Double)
> eutAmplify   secsScored
>              (rL@Reconciled{rVolEnv = envL},  rR@Reconciled{rVolEnv = envR})
>              secsToPlay                  =
>   proc (a1L, a1R) → do
>     aenvL ← doEnvelope envL secsScored secsToPlay ⤙ ()
>     aenvR ← doEnvelope envR secsScored secsToPlay ⤙ ()
>
>     let (a2L, a2R)                       = (a1L * aenvL, a1R * aenvR)
>
>     let (a3L, a3R)                       = amplify
>                                              (a1L, a1R)
>                                              (aenvL, aenvR)
>                                              (a2L, a2R)
>
>     outA                                 ⤙ (a3L, a3R)
>
>   where
>     amplify            :: (Double, Double) → (Double, Double) → (Double, Double) → (Double, Double)
>     amplify (a1L, a1R) (aenvL, aenvR) (a3L, a3R)
>       | traceNever msg' False            = undefined
>       | otherwise                        = (a3L, a3R)
>       where
>         msg'                             = unwords ["amplify env = ",  show aenvL]
>

Modulation ============================================================================================================

> eutModulate            :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Signal p ((Double, Double), (ModSignals, ModSignals)) (Double, Double)
> eutModulate _ (rL, rR)                   =
>   proc ((a1L, a1R), (modSigL, modSigR)) → do
>     a2L   ← addResonance rL              ⤙ (a1L, modSigL)
>     a2R   ← addResonance rR              ⤙ (a1R, modSigR)
>
>     let (a3L', a3R')                     = modulate (a1L, a1R) (a2L, a2R)
>
>     outA                                 ⤙ (a3L', a3R')
>
>   where
>     modulate           :: (Double, Double) →  (Double, Double) → (Double, Double)
>     modulate (a1L, a1R) (a2L, a2R)
>       | traceNever msg' False            = undefined
>       | otherwise                        = (a2L, a2R)
>       where
>         (a3L, a3R)                       = (checkForNan a2L "mod a2L", checkForNan a2R "mod a2R" )
>
>         msg'                             = unwords ["modulate\n sin = ", show (a1L,       a1R)
>                                                   , "\nsout = ",         show (a3L,       a3R)]
>
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
>     msg                              = unwords ["deriveLFO ", show toPitch,    " "
>                                                             , show toFilterFc, " "
>                                                             , show toVolume]
>
> eutIgniteModSignals    :: ∀ p. Clock p ⇒
>                           Double
>                           → Double
>                           → (Reconciled, Reconciled)
>                           → Signal p () (ModSignals, ModSignals)
> eutIgniteModSignals secsScored secsToPlay (reconL@Reconciled{rModulation = mL}, reconR@Reconciled{rModulation = mR})
>                                          =
>   proc _ → do
>     aL1 ← doEnvelope  mModEnvL secsScored secsToPlay ⤙ ()
>     aL2 ← doLFO       mModLfoL                       ⤙ ()
>     aL3 ← doLFO       mVibLfoL                       ⤙ ()
>     aR1 ← doEnvelope  mModEnvR secsScored secsToPlay ⤙ ()
>     aR2 ← doLFO       mModLfoR                       ⤙ ()
>     aR3 ← doLFO       mVibLfoR                       ⤙ ()
>     outA ⤙ (ModSignals aL1 aL2 aL3, ModSignals aR1 aR2 aR3)
>   where
>     mL'@Modulation{mModEnv = mModEnvL, mModLfo = mModLfoL, mVibLfo = mVibLfoL} = mL
>     mR'@Modulation{mModEnv = mModEnvR, mModLfo = mModLfoR, mVibLfo = mVibLfoR} = mR
>
> calculateModFactor     :: String → Modulation → ModDestType → ModSignals → (AbsPitch, Volume) → Double
> calculateModFactor tag m@Modulation{modGraph, toPitchSummary, toFilterFcSummary, toVolumeSummary}
>                    md msig@ModSignals{srModEnvPos, srModLfoPos, srVibLfoPos} (p, v)
>  | traceNever msg False                  = undefined
>  | otherwise                             = fact
>  where
>    targetListIn        :: [Double]       = case md of
>                                              ToPitch        → toPitchSummary
>                                              ToFilterFc     → toFilterFcSummary
>                                              ToVolume       → toVolumeSummary
>                                              _              → error $ "Error in calculateModFactor " ++ show md
>    targetList                            = profess
>                                              (length targetListIn >= 3)
>                                              "bad targetList"
>                                              targetListIn
>    x1                                    = srModEnvPos * head targetList
>    x2                                    = srModLfoPos * (targetList !! 1)
>    x3                                    = srVibLfoPos * (targetList !! 2)
>    x4                                    = product $ maybe [] (map evaluateModulator) (Map.lookup md modGraph)
>    fact                                  = fromCents (x1 + x2 + x3) * x4
>
>    msg                                   = unwords ["calculateModFactor: "
>                                                     , show x1, " + "
>                                                     , show x2, " + "
>                                                     , show x3, " + "
>                                                     , show x4, " = ", show (x1+x2+x3+x4), " => ", show fact]
>
>    evaluateModulator   :: Modulator → Double
>    evaluateModulator m@Modulator{mrModId, mrModSrc, mrModAmount, mrAmountSrc}
>                                          = srcValue mrModSrc * mrModAmount * srcValue mrAmountSrc
>      where
>        srcValue            :: ModSrc → Double
>        srcValue msrc@ModSrc{msMinToMax, msBiPolar, msType}
>          | useModulators                 = case msType of
>                                              NoSource             → 1
>                                              NoteOnVelocity       → quantifyNoteOn v
>                                              NoteOnKeyNumber      → quantifyNoteOn p
>                                              LinkedModulators     → product $ maybe
>                                                                                 []
>                                                                                 (map evaluateModulator)
>                                                                                 (Map.lookup (ToLink mrModId) modGraph)
>          | otherwise                     = 0
>          where
>            quantifyNoteOn
>                        :: Int → Double
>            quantifyNoteOn n              = val''
>              where
>                val                           = fromIntegral n / 128
>                val'                          = if msMinToMax then 1 - val
>                                                              else val
>                val''                         = if msBiPolar  then val' * 2 - 1
>                                                              else val'
>
> addResonance           :: ∀ p . Clock p ⇒ Reconciled → Signal p (Double, ModSignals) Double
> addResonance recon@Reconciled{rNoteOnVel, rNoteOnKeyNumber, rModulation}
>                                          = maybe delay' makeSF mLowPass
>   where
>     m@Modulation{mLowPass, toFilterFcSummary}
>                                          = rModulation
>     makeSF             :: LowPass → Signal p (Double, ModSignals) Double
>     makeSF lp@LowPass{lowPassFc, lowPassQ}
>       | traceIf msg False                = undefined
>       | otherwise                        =
>       proc (x, msig) → do
>         let fc = lowPassFc * calculateModFactor
>                                "addResonance"
>                                m
>                                ToFilterFc
>                                msig
>                                (rNoteOnVel, rNoteOnKeyNumber)
>         y ← filterLowPassBW              ⤙ (x, fc)
>         outA                             ⤙ resonate x fc y
>       where
>         msg = unwords ["addResonance/makeSF ", show lowPassFc ]
>
>     delay'             :: Signal p (Double, ModSignals) Double
>                                          =
>       proc (x, _) → do
>         y ← delay 0                      ⤙ x  
>         outA                             ⤙ y
>
>     resonate           :: Double → Double → Double → Double
>     resonate x fc y
>       | traceNever msg' False            = undefined
>       | otherwise                        = y
>       where
>         msg'                             = unwords ["resonate\nsin  = ", show x
>                                                           , "\nfc   = ", show fc
>                                                           , "\nsout = ", show y]
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
> modVib                 :: ∀ p . Clock p ⇒ Double → Double → Signal p Double Double
> modVib rate depth =
>   proc sin → do
>     vib   ← osc sineTable 0  ⤙ rate
>     sout  ← delayLine1 0.2   ⤙ (sin,0.1+depth*vib)
>     outA ⤙ sout

FFT ===================================================================================================================

> findOutliers           :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → IO ()
> findOutliers secs sig                    = do
>   putStrLn (findOutliersString secs sig)
>   return ()
>
> findOutliersString     :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → String
> findOutliersString secs sig              = "findOutliers " ++ show secs ++ "..." ++ show (abs x) ++ " ... " ++ show y ++ " / " ++ show h ++ " = " ++ show z
>   where
>     ss                                   = toSamples (secs + 0.5) sig
>     pers               :: Double         = secs / fromIntegral (length ss)
>     ts                                   = map ((*pers) . fromIntegral) [0..(length ss - 1)]
>     timedPoints                          = zip ts ss 
>     (x, y)                               = maximumBy (comparing (abs . snd)) timedPoints
>     h                  :: Double         = fromIntegral $ length timedPoints
>     z                  :: Double         = secs * abs x / h
>
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
>   | traceIf msg False                    = undefined
>   | otherwise                            = if useEnvelopes && doUse mTarget
>                                              then Just env
>                                              else Nothing
>   where
>     inp                                  = [mDelay, mAttack, mHold, mDecay, mSustain, mRelease]
>     env                                  = Envelope (fromTimecents mDelay) (fromTimecents mAttack)      (fromTimecents mHold)
>                                                     (fromTimecents mDecay) (fromTithe mSustain)         (fromTimecents mRelease)
>                                                     (makeModTarget mTarget)
>
>     doUse            :: Maybe (Maybe Int, Maybe Int) → Bool
>     doUse mTarget                        = case mTarget of
>                                              Nothing           → True
>                                              Just target       → (isJust . fst) target || (isJust . snd) target
>
>     makeModTarget    :: Maybe (Maybe Int, Maybe Int) → ModTarget
>     makeModTarget mTarget                = case mTarget of
>                                              Nothing           → defModTarget
>                                              Just target       → uncurry deriveModTarget target Nothing
>
>     msg                                  = if useEnvelopes
>                                              then unwords ["deriveEnvelope ", if isJust mTarget
>                                                                                 then "modEnv "
>                                                                                 else "volEnv "
>                                                                        , "mDelay=  ", show mDelay
>                                                                        , "mAttack= ", show mAttack
>                                                                        , "mHold=   ", show mHold
>                                                                        , "mDecay=  ", show mDecay
>                                                                        , "mSustain=", show mSustain
>                                                                        , "mRelease=", show mRelease
>                                                                        , "mTarget= ", show mTarget]
>                                              else unwords ["deriveEnvelope (none)"]
>
> doEnvelope             :: ∀ p . Clock p ⇒ Maybe Envelope → Double → Double → Signal p () Double
> doEnvelope menv secsScored secsToPlay    = case menv of
>                                              Nothing            → constA 1
>                                              Just env           → makeSF env
>   where
>     makeSF             :: Envelope → Signal p () Double
>     makeSF env
>       | traceIf msg False                = undefined
>       | otherwise                        = dumpSF secsScored secsToPlay sf
>       where
>         sf = envLineSeg sAmps sDeltaTs
>         segs@Segments{sAmps, sDeltaTs}   = computeSegments secsScored secsToPlay env
>         msg = unwords ["doEnvelope/makeSF ", show (secsScored, secsToPlay), " ",  show (sAmps, sDeltaTs)]
>
>     dumpSF             :: Double → Double → Signal p () Double → Signal p () Double
>     dumpSF secsScored secsToUse sigIn
>       | traceNever msg' False            = undefined
>       | otherwise                        = sigIn
>       where
>         str1 = findOutliersString secsScored sigIn
>         msg' = unwords ["dumpSF ", str1]

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
> computeSegments secsScored secsToPlay rEnv@Envelope{eDelayT, eAttackT, eHoldT, eDecayT, eSustainLevel, eReleaseT}
>   | traceNever msg False                 = undefined
>   | otherwise                            = Segments amps deltaTs
>   where
>     amps               :: [Double]       = [0,       0,       1,       1,     fSusLevel, fSusLevel,      0,      0]
>     deltaTs            :: [Double]       = [  fDelayT, fAttackT, fHoldT,  fDecayT, fSustainT, fReleaseT,   fPostT]
>
>     minDeltaT          :: Double         = fromTimecents Nothing
>     secsToUse          :: Double         = profess (secsToPlay > 10 * minDeltaT)
>                                                    "time too short for envelope"
>                                                    secsToPlay
>
>     -- pin down onset of Release phase
>     rpCand1            :: Double         = max (secsToUse - eReleaseT)
>                                                (8 * minDeltaT)
>     rpCand2                              = min (eDelayT + eAttackT + eHoldT + eDecayT + minDeltaT)
>                                                (secsToUse - (8 * minDeltaT))
>     rp                                   = (rpCand1 + rpCand2) / 2
>
>     fSusLevel          :: Double         = clip (0, 1) eSustainLevel
>
>     fReleaseT          :: Double         = secsToUse - rp
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
>     msg                                  = unwords ["computeSegments secs = ", show (secsScored, secsToUse)
>                                                   , " time = ",                show (sum deltaTs)
>                                                   , " rp =",                   show (rpCand1, rpCand2, rp)]

Effects ===============================================================================================================

> deriveEffects          :: Maybe Int → Maybe Int → Maybe Int → Effects
> deriveEffects mChorus mReverb mPan       = Effects mdChorus mdReverb mdPan
>   where
>     mdChorus           :: Maybe Double   = if useChorus
>                                              then fmap (conv (0, 1000)) mChorus
>                                              else Nothing
>     mdReverb           :: Maybe Double   = if useReverb
>                                              then fmap (conv (0, 1000)) mReverb
>                                              else Nothing
>     mdPan              :: Maybe Double   = if usePan
>                                              then fmap (conv (-500, 500)) mPan
>                                              else Nothing
>
>     conv               :: (Int, Int) → Int → Double
>     conv range nEffect = fromIntegral (clip range nEffect) / 1000
>
> eutEffects             :: ∀ p . Clock p ⇒
>                           Double
>                           → (Reconciled, Reconciled)
>                           → Signal p (Double, Double) (Double, Double)
> eutEffects _ (reconL@Reconciled{rEffects = effL}, reconR@Reconciled{rEffects = effR})
>   | traceNever msg False = undefined
>   | otherwise =
>   proc (aL, aR) → do
>     chL ← eutChorus 15.0 0.005 0.1 cL ⤙ aL
>     chR ← eutChorus 15.0 0.005 0.1 cR ⤙ aR
>
>     (rbL, rbR) ← eutReverb rL ⤙ (aL, aR)
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
>     ecL@Effects{efChorus = cL, efReverb = rL, efPan = pL} = effL
>     ecR@Effects{efChorus = cR, efReverb = rR, efPan = pR} = effR
>
>     cFactorL = fromMaybe 0 cL
>     cFactorR = fromMaybe 0 cR
>     rFactorL = fromMaybe 0 rL
>     rFactorR = fromMaybe 0 rR
>     pFactorL = fromMaybe 0 pL
>     pFactorR = fromMaybe 0 pR
>
>     msg = unwords ["eutEffects=", show effL, "=LR=", show effR, "rFactor*=", show rFactorL, " ", show rFactorR]
> 
> eutChorus              :: ∀ p . Clock p ⇒ Double → Double → Double → Maybe Double → Signal p Double Double
> eutChorus rate gain depth               = maybe (delay 0) makeSF
>   where
>     gain                                 = 0.1
>
>     makeSF             :: Double → Signal p Double Double
>     makeSF rate = proc sin → do
>       lfo ← osc (tableSines 4096 [1]) 0 ⤙ rate
>       z1 ← delayLine1 0.030 ⤙ (sin, 0.010 + depth * lfo)
>       z2 ← delayLine1 0.030 ⤙ (sin, 0.020 + depth * lfo)
>       z3 ← delayLine1 0.030 ⤙ (sin, 0.030 + depth * lfo)
>       rec
>         r ← delayLine 0.0001 ⤙ (sin + z1 + z2 + z3)/4 + r * gain
>       outA ⤙ r
>
> eutReverb              :: ∀ p . Clock p ⇒ Maybe Double → Signal p (Double, Double) (Double, Double)
> eutReverb                                = maybe (delay (0, 0)) makeSF
>   where
>     roomSize                             = 0.75
>     damp                                 = 0.25
>     width                                = 1.0
>
>     makeSF             :: Double → Signal p (Double, Double) (Double, Double)
>     makeSF _                             = eatFreeVerb $ makeFreeVerb roomSize damp width
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
>   | traceNever msg False = undefined
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
>   | traceNever msg False = undefined
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
>   , rNoteOnVel         :: Volume
>   , rNoteOnKeyNumber   :: AbsPitch
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
> data ModSignals =
>   ModSignals {
>     srModEnvPos        :: Double
>   , srModLfoPos        :: Double
>   , srVibLfoPos        :: Double} deriving (Show)
>
> data ModTarget =
>   ModTarget {
>     mtPitch            :: Double
>   , mtFilterFc         :: Double
>   , mtVolume           :: Double} deriving (Eq, Show)
>
> chooseFromModTarget    :: ModDestType → ModTarget → Double
> chooseFromModTarget mdtype mt@ModTarget{mtPitch, mtFilterFc, mtVolume}
>                                          = case mdtype of
>                                              ToPitch     → mtPitch
>                                              ToFilterFc  → mtFilterFc
>                                              ToVolume    → mtVolume
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
>   , toPitchSummary     :: [Double]
>   , toFilterFcSummary  :: [Double]
>   , toVolumeSummary    :: [Double]
>   , modGraph           :: Map.Map ModDestType [Modulator]} deriving (Eq, Show)
>
> defModulation          :: Modulation
> defModulation                            = Modulation
>                                             Nothing Nothing Nothing Nothing
>                                             [] [] [] Map.empty
>
> data Modulator =
>   Modulator {
>     mrModId          :: Word
>   , mrModSrc         :: ModSrc
>   , mrModDest        :: ModDestType
>   , mrModAmount      :: Double
>   , mrAmountSrc      :: ModSrc} deriving (Eq, Show)
>    
> defModulator           :: Modulator
> defModulator                             = Modulator 0 defModSrc NoDestination 0 defModSrc
>
> data ModDestType =
>     NoDestination
>   | ToPitch
>   | ToFilterFc
>   | ToVolume
>   | ToLink Word deriving (Eq, Ord, Show)
>
> data ModSrcType =
>   NoSource | NoteOnVelocity | NoteOnKeyNumber | LinkedModulators deriving (Eq, Show)
>
> data ModSrc =
>   ModSrc {
>       msCCBit          :: Bool
>     , msMinToMax       :: Bool
>     , msBiPolar        :: Bool
>     , msType           :: ModSrcType} deriving (Eq, Show)
>
> defModSrc              :: ModSrc
> defModSrc                                = ModSrc False False False NoSource
>
> data Effects =
>   Effects {
>     efChorus           :: Maybe Double
>   , efReverb           :: Maybe Double
>   , efPan              :: Maybe Double} deriving (Eq, Show)
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
>   , qqUseModulators        :: Bool
>   , qqUseLoopSwitching     :: Bool
>   , qqUseLowPass           :: Bool
>   , qqUseLFO               :: Bool
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
> useModulators                            = qqUseModulators              defS
> useLoopSwitching                         = qqUseLoopSwitching           defS
> useLowPass                               = qqUseLowPass                 defS
> useLFO                                   = qqUseLFO                     defS
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
> qqDesires              :: [Desires]      = [qqDesireReStereo      defT
>                                           , qqDesireRe24Bit       defT
>                                           , qqDesireReSplitCharacteristic
>                                                                   defT
>                                           , qqDesireReConformance defT
>                                           , qqDesireReFuzzy       defT]
> qqDesires'             :: [Int]          = map scoreDesire        qqDesires

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
>   , qqUseModulators                      = True
>   , qqUseLoopSwitching                   = True
>   , qqUseLowPass                         = True
>   , qqUseLFO                             = True
>   , qqUseEffectReverb                    = True
>   , qqUseEffectChorus                    = True
>   , qqUseEffectPan                       = True
>   , qqUseEffectDCBlock                   = True
>   , qqNormalizingOutput                  = True
>   , qqScanningOutput                     = False}
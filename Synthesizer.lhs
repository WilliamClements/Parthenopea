> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module Synthesizer where
>
> import Control.Arrow
> import Control.Arrow.ArrowP
> import Control.SF.SF
> -- import Control.Arrow.ArrowP ( ArrowP(ArrowP), strip )
> import Data.Array.Unboxed ( array, Array, (!), IArray(bounds) )
> import qualified Data.Audio           as A
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List
> import Data.Maybe (isJust, fromJust, fromMaybe)
> import Data.Word
> import Euterpea.IO.Audio.Basics
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Render
> import Euterpea.IO.Audio.Types
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import Parthenopea
> import qualified SynToolkit           as STK
  
Signal function-based synth ===============================================================

> eutSynthesize          :: (Reconciled, Reconciled)
>                           → Double
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → AudSF () (Double, Double)
> eutSynthesize (rDataL, rDataR) sr dur pch vol params s16 ms8 =
>   let
>     ap                 :: AbsPitch            = fromIntegral (rRootKey rDataL)
>     (st, en)           :: (Word, Word)        = (rStart rDataL, rEnd rDataL)
>     ns                 :: Double              = fromIntegral (en - st + 1)
>     secsSample         :: Double              = ns / sr
>     secsScored         :: Double              = 2 * fromRational dur
>     freqRatio          :: Double              = apToHz ap / apToHz pch
>     rateRatio          :: Double              = 44100 / sr
>     freqFactor         :: Double              = freqRatio * rateRatio / rPitchCorrection rDataL
>     sig                :: AudSF () (Double, Double)
>                                               = eutDriver rDataL secsSample secsScored sr 0 freqFactor
>                                             >>> eutRelayStereo s16 ms8 secsScored (rDataL, rDataR) vol dur
>                                             >>> eutEffects secsScored (rDataL, rDataR)
>   in sig
>
> pSwitch                :: forall p col a. (Clock p, Functor col, Foldable col) ⇒
>                           col (Evt (Signal p () a))  -- Initial SF collection.
>                           → Signal p () (col (Evt (Signal p () a)))    -- Input event stream.
>                           → (col (Evt (Signal p () a))
>                                → col (Evt (Signal p () a))
>                                → col (Evt (Signal p () a)))
>                           -- A Modifying function that modifies the collection of SF
>                           --   based on the event that is occuring.
>                           → Signal p () (col a)
>                           -- The resulting collection of output values obtained from
>                           --   running all SFs in the collection.
> pSwitch col esig modsf = 
>   proc _ → do
>     evts ← esig ⤙ ()
>     rec
>       -- perhaps this can be run at a lower rate using upsample
>       sfcol ← delay col ⤙ modsf sfcol' evts  
>       let k = foldl' (\a i → fst i) 0 sfcol
>       let rs = fmap (\ns → runSF (strip (snd ns)) ()) sfcol
>       let (as, sfcol' :: col (Evt (Signal p () a)))
>            = (fmap fst rs, fmap (\r → (k, (ArrowP . snd) r)) rs)
>     outA ⤙ as
>
> eutDriverFull          :: Double → Double → AudSF () Double 
> eutDriverFull iphs delta
>   | traceAlways msg False = undefined
>   | otherwise =
>   let frac             :: RealFrac r ⇒ r → r
>       frac = snd . properFraction
>   in proc () → do
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>   where
>     msg = unwords ["eutDriverFull iphs=", show iphs, "delta=", show delta]
>
> eutDriverLooping       :: Double
>                           → Double
>                           → (Double, Double)
>                           → AudSF () Double
> eutDriverLooping iphs delta (lst, len)
>   | traceAlways msg False = undefined
>   | otherwise =
>   let frac             :: RealFrac r ⇒ r → r
>       frac = snd . properFraction
>   in proc () → do
>     rec
>       let phase = if next > len then frac lst else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>   where
>     msg = unwords ["eutDriverLooping iphs=", show iphs, "delta=", show delta, "lst, len=", show (lst, len)]
>
> eutDriver              :: Reconciled
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → AudSF () Double 
> eutDriver recon secsSample secsScored sr iphs freqFactor =
>   proc () → do
>     curPos ← sf ⤙ ()
>     let curPos' = if checkPos curPos
>                   then curPos
>                   else error "bad pos in eutDriver"
>     outA ⤙ curPos'
>
>   where
>     allsf              :: AudSF () [Double]
>                                            -- add up all samples
>                                          = pSwitch [] (evtsf secsSample) modsf
>     sf                 :: AudSF () Double
>                                          = allsf >>> arr (foldl' mix zero)
>     delta              :: Double         = 1 / (secsSample * freqFactor * sr)
>     eps                :: Double         = 0.000001
>     phase              :: Double         = 0
>     (normst,normen)    :: (Double, Double)
>                                          = normalizeLooping recon
>
>     switchPhase        :: (Double, Double) → (Double, Double)
>     switchPhase (phase, delta) = (phase + angle, delta)
>       where
>         angle'         :: Double         = snd (properFraction angle)
>         angle          :: Double         = (secsSample/sr) * delta -- TODO: duh    ... / (2 * pi)
>
>     evtsf              :: DeltaT → AudSF () [Evt (AudSF () Double)]
>     evtsf secsSample
>       | traceAlways msg False = undefined
>       | otherwise =
>       let (phase', delta')               = if useLoopPhaseCalc
>                                            then switchPhase (phase, delta)
>                                            else (normen, delta)
>       in
>       proc () → do
>         rec
>           let t' = t + 1
>           t ← delay 0 ⤙ t'
>           let evs = if t < secsSample * 44100
>                     then [(0, eutDriverFull phase delta)]
>                     else [(1, eutDriverLooping phase' delta' (normst, normen))]
>         outA ⤙ evs
>       where
>         msg = unwords ["evtsf secsSample=", show secsSample, " ", show (secsSample * 44100), " ", show (secsSample * sr)]
>
>     modsf              :: [Evt (AudSF () Double)] → [Evt (AudSF () Double)] → [Evt (AudSF () Double)]
>     modsf sfs evts =
>       let
>         evt = last evts
>         result
>           | null sfs = [evt]
>           | fst evt == fst (last sfs) = sfs
>           | otherwise = [evt]
>       in
>         result
>
>     checkPos           :: Double  → Bool
>     checkPos pos
>       | traceNever msg False = undefined
>       | otherwise = True
>       where
>         msg = unwords ["eutDriver pos=", show pos]
>
> eutRelayStereo         :: A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Double
>                           → (Reconciled, Reconciled)
>                           → Volume
>                           → Dur
>                           → AudSF Double (Double, Double)
> eutRelayStereo s16 ms8 secsScored (rDataL, rDataR) vol dur
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     (stL, enL)         :: (Word, Word)     = (rStart rDataL, rEnd rDataL)
>     (stR, enR)         :: (Word, Word)     = (rStart rDataR, rEnd rDataR)
>     numS               :: Double           = fromIntegral (enL - stL + 1)
>     amp                :: Double           = fromIntegral vol / 100
>
>   in proc pos → do
>     let saddrL         :: Int              = fromIntegral stL + truncate (numS * pos)
>     let saddrR         :: Int              = fromIntegral stR + truncate (numS * pos)
>     let ok = True
>     let (a1L, a1R) = if ok
>                      then getSampleVals (saddrL, saddrR)
>                      else error "bad addrs"
>     rec
>       aenvL ← if useEnvelopes
>               then doEnvelope secsScored (rEnvelope rDataL) ⤙ ()
>               else constA 1                                 ⤙ ()
>       aenvR ← if useEnvelopes
>               then doEnvelope secsScored (rEnvelope rDataR) ⤙ ()
>               else constA 1                                 ⤙ ()
>       a2L   ← if useLowPassFilter
>               then filterLowPass                            ⤙ (a1L,20000*aenvL)
>               else delay 0                                  ⤙ a1L
>       a2R   ← if useLowPassFilter
>               then filterLowPass                            ⤙ (a1R,20000*aenvR)
>               else delay 0                                  ⤙ a1R
>     let (zL, zR) = (a2L*amp*aenvL, a2R*amp*aenvR)
>     let ok = lookAtEveryPoint amp a2L aenvL a2R aenvR
>     let (zL', zR') = if ok
>                      then (zL, zR)
>                      else error "bad point"
>
>     outA ⤙ (zL', zR')
>
>   where
>     msg = unwords ["eutRelayStereo = ", show secsScored, " , ", show dur]
>
>     lookAtEveryPoint   :: Double → Double → Double → Double → Double → Bool
>     lookAtEveryPoint amp a2L aenvL a2R aenvR
>       | traceNever msg False = undefined
>       | otherwise = True
>       where
>         (eeL, eeR) = (a2L*amp*aenvL, a2R*amp*aenvR)
>         msg = unwords ["eeL=", show eeL, " eeR=", show eeR]
>
>     -- not 100% sure there is not a signed/unsigned problem with the 8 bit part
>     compute24          :: Int16 → Word8 → Double
>     compute24 i16 w8 = d24
>       where
>         f8to32         :: Word8 → Int32
>         f8to32 = fromIntegral
>         f16to32         :: Int16 → Int32
>         f16to32 = fromIntegral
>         d24            :: Double
>         d24 = fromIntegral (f16to32 i16 * 256 + f8to32 w8) / 8388608.0
>      
>     compute16         :: Int16 → Double
>     compute16 i16 = fromIntegral i16 / 32768.0
>
>     doEnvelope         :: (Clock p) ⇒ Double → Envelope → Signal p () Double
>     doEnvelope secsScored renv = envLineSeg (sAmps segments) (sDeltaTs segments)
>       where
>         segments = computeSegments secsScored renv
>      
>     getSampleVals      :: (Int, Int) → (Double, Double)
>     getSampleVals (saddrL, saddrR)
>       | traceIf msg' False = undefined
>       | otherwise = 
>         if isJust ms8
>         then (compute24 (s16 ! saddrL) (fromIntegral (fromJust ms8 ! saddrL))
>             , compute24 (s16 ! saddrR) (fromIntegral (fromJust ms8 ! saddrR)))
>         else (compute16 (s16 ! saddrL)
>             , compute16 (s16 ! saddrR))
>       where
>         msg' = unwords ["saddrL=", show saddrL, "saddrR=", show saddrR]
>
> normalizeLooping       :: Reconciled → (Double, Double)
> normalizeLooping recon
>   | traceAlways msg False = undefined
>   | otherwise = (normst, normen)
>   where
>     fullst             :: Double = fromIntegral $ rStart recon 
>     fullen             :: Double = fromIntegral $ rEnd recon 
>     loopst             :: Double = fromIntegral $ rLoopStart recon 
>     loopen             :: Double = fromIntegral $ rLoopEnd recon 
>     denom              :: Double = fullen - fullst + 1
>     normst             :: Double = (loopst - fullst) / denom
>     normen             :: Double = (loopen - fullst) / denom
>     msg = unwords ["normalizeLooping, recon= ", show recon
>                  , " full=",                    show fullst
>                  , " ",                         show fullen
>                  , " loop=",                    show loopst
>                  , " , ",                       show loopen
>                  , ", denom=",                  show denom]
>     
> resolvePitchCorrection :: Int → Maybe Int → Maybe Int → Double
> resolvePitchCorrection alt mps mpc = if usePitchCorrection
>                                      then 2 ** (fromIntegral cents/12/100)
>                                      else 1.0
>   where
>     cents              :: Int = if isJust mps || isJust mpc
>                                 then coarse * 100 + fine
>                                 else alt
>     coarse             :: Int = fromMaybe 0 mps
>     fine               :: Int = fromMaybe 0 mpc
>
> deriveEnvelope         :: Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Envelope
> deriveEnvelope mDelay mAttack mHold mDecay mSustain mRelease
>   | traceIf msg False = undefined
>   | otherwise =
>     let
>       iDelay =   fromMaybe (-12000) mDelay
>       iAttack =  fromMaybe (-12000) mAttack
>       iHold   =  fromMaybe (-12000) mHold
>       iDecay  =  fromMaybe (-12000) mDecay
>       iSustain = fromMaybe        0 mSustain
>       iRelease = fromMaybe (-12000) mRelease
>     in 
>       Envelope (conv iDelay) (conv iAttack)           (conv iHold)
>                (conv iDecay) (acceptSustain iSustain) (conv iRelease)
>     where
>       conv             :: Int → Double
>       conv k = 2**(fromIntegral k/1200)
>       msg = unwords ["mDelay=",   show mDelay,   "mAttack=",    show mAttack
>                     ,"mHold=",    show mHold,    "mDecay=",     show mDecay
>                     ,"mSustain=", show mSustain, "mRelease=",   show mRelease]
>
> deriveEffects          :: Maybe Int → Maybe Int → Maybe Int → Effects
> deriveEffects mChorus mReverb mPan =
>   Effects (fmap (conv (0, 1000)) mChorus)
>           (fmap (conv (0, 1000)) mReverb)
>           (fmap (conv (-500, 500)) mPan)
>   where
>     conv               :: (Int, Int) → Int → Double
>     conv (nMin, nMax) nEffect = fromIntegral nEffect''/1000
>       where
>         nEffect' = max nMin nEffect
>         nEffect'' = min nMax nEffect'
>
> acceptSustain          :: Int → Double
> acceptSustain iS = 1/raw iS
>   where
>     raw                :: Int → Double
>     raw iS = pow 10 (fromIntegral jS/200)
>       where jS
>               | iS <= 0 = 0
>               | iS >= 1000 = 1000
>               | otherwise = iS

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
> computeSegments secsScored rEnv =
>   let
>     del = eDelayT rEnv
>     att = eAttackT rEnv
>     hold = eHoldT rEnv
>     dec = eDecayT rEnv
>     sus = eSustainLevel rEnv
>     rel = eReleaseT rEnv
>     sum = del + att + hold + dec + rel
>     sustime = secsScored - sum
>     amps =   [0, 0, 1, 1, sus, sus, 0, 0]
>     deltaTs = [del, att, hold, dec, max 0 sustime, min secsScored rel, secsScored]
>   in
>     if sustime >= 0 || not useShortening
>     then Segments amps deltaTs
>     else shortenEnvelope secsScored amps deltaTs
>
> computeDeltaTs         :: Double
>                           → [Double]
>                           → [Double]
> computeDeltaTs limit xs = ys
>   where
>     (sum, ys)              = foldl' (foldFun limit) (0, []) xs
>     foldFun            :: Double
>                           → (Double, [Double])
>                           → Double
>                           → (Double, [Double])
>     foldFun limit (sum, vs) val 
>       | sum >= limit       = (val + sum, vs)
>       | val + sum > limit  = (val + sum, vs ++ [limit - sum])
>       | otherwise          = (val + sum, vs ++ [val])   
>     
> shortenEnvelope        :: Double
>                           → [Double]
>                           → [Double]
>                           → Segments
> shortenEnvelope secsScored amps deltaTs = Segments amps' deltaTs''
>   where
>     deltaTs'  = computeDeltaTs secsScored deltaTs
>     ix        = length deltaTs' - 1
>     torig     = deltaTs  !! ix
>     tnew      = deltaTs' !! ix
>     fact      = if torig == 0
>                 then 1.0
>                 else tnew / torig
>     a0        = amps !! ix
>     a1        = amps !! (ix + 1)
>     a'        = a0 + fact * (a1 - a0)    
>     amps'     = take (ix+1) amps ++ [a', 0, 0]
>     deltaTs'' = deltaTs' ++ [0.001, 0.001]
>
> eutEffects             :: Double → (Reconciled, Reconciled) → AudSF (Double, Double) (Double, Double)
> eutEffects secsScored (zL, zR)
>   | traceIf msg False = undefined
>   | otherwise =
>   proc (aL, aR) → do
>     let cFactorL = if useEffectChorus
>                    then fromMaybe 0 (efChorus effL)
>                    else 0.0
>     let cFactorR = if useEffectChorus
>                    then fromMaybe 0 (efChorus effR)
>                    else 0.0
>     let rFactorL = if useEffectReverb
>                    then fromMaybe 0 (efReverb effL)
>                    else 0.0
>     let rFactorR = if useEffectReverb
>                    then fromMaybe 0 (efReverb effR)
>                    else 0.0
>     let pFactorL = if useEffectPan
>                    then fromMaybe 0 (efPan effL)
>                    else 0.0
>     let pFactorR = if useEffectPan
>                    then fromMaybe 0 (efPan effR)
>                    else 0.0
>
>     -- TODO chL ← eutChorus 15.0 0.005 0.1 ⤙ aL
>     -- TODO chR ← eutChorus 15.0 0.005 0.1 ⤙ aR
>     let (chL, chR) = (aL, aR)
>
>     -- TODO (rbL, rbR) ← eutReverb 0.75 0.25 1.0 ⤙ (aL, aR)
>     let (rbL, rbR) = (aL, aR)
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
>     let ((cL, cR), (dL, dR))  = (doPan pFactorL mixL, doPan pFactorR mixR)
>     let (wtL, wtR)	= ((cL + dL) / 2, (cR + dR) / 2)
>
> {-
>     dcL ← dcBlock 0.99 ⤙ wtL
>     dcR ← dcBlock 0.99 ⤙ wtR
>
>     let (oaL, oaR) = if useDCBlock
>                      then (dcL, dcR)
>                      else (wtL, wtR)
> -}
>     let (oaL, oaR) = (wtL, wtR)
>
>     outA ⤙ (oaL, oaR)
>
>   where
>     (effL, effR) = (rEffects zL, rEffects zR)
>     msg = unwords ["eutEffects=", show effL, "=LR=", show effR]
> 
> eutChorus              :: Double → Double → Double → AudSF Double Double
> eutChorus rate depth gain =
>   proc sin → do
>     rec
>       lfo ← osc (tableSines 4096 [1]) 0 ⤙ rate
>       z1 ← delayLine1 0.030 ⤙ (sin, 0.010 + depth * lfo)
>       z2 ← delayLine1 0.030 ⤙ (sin, 0.020 + depth * lfo)
>       z3 ← delayLine1 0.030 ⤙ (sin, 0.030 + depth * lfo)
>       r ← delayLine 0.0001 ⤙ (sin + z1 + z2 + z3)/4 + r * gain
>     outA ⤙ r
>
> eutReverb              :: Double → Double → Double → AudSF (Double, Double) (Double, Double)
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
> makeFreeVerb roomSize damp width =
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
>
> eatFreeVerb            :: STK.FreeVerb → AudSF (Double, Double) (Double, Double)
> eatFreeVerb fv@STK.FreeVerb
>   {   STK.iiWetDryMix      = mix             {- 0.2    -}
>     , STK.iiG              = coeff           {- 0.5    -}
>     , STK.iiGain           = gain            {- 0.015  -}
>     , STK.iiRoomSize       = roomSize        {- 0.75   -}
>     , STK.iiDamp           = damp            {- 0.25   -}
>     , STK.iiWet1           = wet1
>     , STK.iiWet2           = wet2
>     , STK.iiDry            = dry
>     , STK.iiWidth          = width           {- 1.0    -}
>     , STK.iiCombDelayL     = cdL
>     , STK.iiCombDelayR     = cdR 
>     , STK.iiCombLPL        = lpL
>     , STK.iiCombLPR        = lpR
>     , STK.iiAllPassDelayL  = adL
>     , STK.iiAllPassDelayR  = adR} =
>
>     proc (sinL, sinR) → do
>       cdL0 ← comb (cdL ! 0) (lpL ! 0) ⤙ sinL
>       cdL1 ← comb (cdL ! 1) (lpL ! 1) ⤙ sinL
>       cdL2 ← comb (cdL ! 2) (lpL ! 2) ⤙ sinL
>       cdL3 ← comb (cdL ! 3) (lpL ! 3) ⤙ sinL
>       cdL4 ← comb (cdL ! 4) (lpL ! 4) ⤙ sinL
>       cdL5 ← comb (cdL ! 5) (lpL ! 5) ⤙ sinL
>       cdL6 ← comb (cdL ! 6) (lpL ! 6) ⤙ sinL
>       cdL7 ← comb (cdL ! 7) (lpL ! 7) ⤙ sinL
>
>       cdR0 ← comb (cdR ! 0) (lpR ! 0) ⤙ sinR
>       cdR1 ← comb (cdR ! 1) (lpR ! 1) ⤙ sinR
>       cdR2 ← comb (cdR ! 2) (lpR ! 2) ⤙ sinR
>       cdR3 ← comb (cdR ! 3) (lpR ! 3) ⤙ sinR
>       cdR4 ← comb (cdR ! 4) (lpR ! 4) ⤙ sinR
>       cdR5 ← comb (cdR ! 5) (lpR ! 5) ⤙ sinR
>       cdR6 ← comb (cdR ! 6) (lpR ! 6) ⤙ sinR
>       cdR7 ← comb (cdR ! 7) (lpR ! 7) ⤙ sinR
>
>       let sumL = cdL0+cdL1+cdL2+cdL3+cdL4+cdL5+cdL6+cdL7
>       let sumR = cdR0+cdR1+cdR2+cdR3+cdR4+cdR5+cdR6+cdR7
>
>       let (fp0L, fp0R) = (sumL/8, sumR/8)
>
>       fp1L ← allpass (adL ! 0) ⤙ fp0L
>       fp2L ← allpass (adL ! 1) ⤙ fp1L
>       fp3L ← allpass (adL ! 2) ⤙ fp2L
>       fp4L ← allpass (adL ! 3) ⤙ fp3L
>             
>       fp1R ← allpass (adR ! 0) ⤙ fp0R
>       fp2R ← allpass (adR ! 1) ⤙ fp1R
>       fp3R ← allpass (adR ! 2) ⤙ fp2R
>       fp4R ← allpass (adR ! 3) ⤙ fp3R
>
>       outA ⤙ (fp4L, fp4R)
>
> comb                   :: Word64 → STK.FilterData → AudSF Double Double
> comb delay filter =
>   proc sin → do
>     rec
>       r ← del ⤙ sin + r * STK.jGain filter
>     outA ⤙ r
>   where
>     del = delayLine (fromIntegral delay/1000.0)
> 
> allpass                :: Word64 → AudSF Double Double
> allpass delay =
>   proc sin → do
>     rec
>       r ← del ⤙ sin
>     outA ⤙ r
>   where
>     del = delayLine (fromIntegral delay/1000.0)
> 
> doPan                  :: Double → Double → (Double, Double)
> doPan azimuth sin = (ampL, ampR)
>   where
>     ampL = sin * cos (rad azimuth)
>     ampR = sin - ampL
>     rad                :: Double → Double
>     rad x = (x + 0.5) * pi / 2
>
> dcBlock                :: Double → AudSF Double Double
> dcBlock a = proc xn → do
>   rec
>     let yn = xn - xn_l + a * yn_l
>     xn_l ← delay 0 ⤙ xn
>     yn_l ← delay 0 ⤙ yn
>   outA ⤙ yn

Charting ==================================================================================

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

Utility types =============================================================================

> data Reconciled =
>   Reconciled {
>     rSampleMode        :: A.SampleMode
>   , rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: Word
>   , rPitchCorrection   :: Double
>   , rEnvelope          :: Envelope
>   , rEffects           :: Effects} deriving (Eq, Show)
>           
> data Envelope =
>   Envelope {
>     eDelayT            :: Double
>   , eAttackT           :: Double
>   , eHoldT             :: Double
>   , eDecayT            :: Double
>   , eSustainLevel      :: Double
>   , eReleaseT          :: Double} deriving (Eq, Show)
>
> data Segments =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show
>
> data Effects =
>   Effects {
>     efChorus           :: Maybe Double
>   , efReverb           :: Maybe Double
>   , efPan              :: Maybe Double} deriving (Eq, Show)
>
> type Evt a = (Int, a) -- 0 or 1 indicating full or looping, and the given SF
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
>     0x1                    → SampleTypeMono
>     0x2                    → SampleTypeRight
>     0x4                    → SampleTypeLeft
>     0x8                    → SampleTypeLinked
>     0x10                   → SampleTypeOggVorbis
>     0x8000                 → SampleTypeRom
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

Knobs and buttons =========================================================================

> usePitchCorrection = True
> useEnvelopes       = True
> useShortening      = True
> useLoopSwitching   = True
> useLoopPhaseCalc   = False
> useLowPassFilter   = False
> useEffectReverb    = False
> useEffectChorus    = False
> useEffectPan       = True
> useDCBlock         = False
>
> data Desires =
>   DAllOff | DPreferOff | DNeutral | DPreferOn | DAllOn deriving (Eq, Show)
>
> desireReStereo     = DPreferOn
> desireRe24Bit      = DNeutral
> desireReMaxSplits  = DPreferOn
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
> splitThreshold         :: Word           = 5
>
> weightFileHints        :: Int            = 2
> weightInstrumentHints  :: Int            = 5
>
> weightStereo           :: Int            = 2
> weight24Bit            :: Int            = 1
> weightMaxSplits        :: Int            = 1
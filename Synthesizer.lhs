> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module Synthesizer where
>
> import Control.Arrow
> import Control.Arrow.ArrowP
> import Control.SF.SF
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
> import qualified Text.FuzzyFind       as FF
 
Signal function-based synth ===========================================================================================

> eutSynthesize          :: (Reconciled, Reconciled)
>                           → Double
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → AudSF () (Double, Double)
> eutSynthesize (reconL, reconR) sr dur pch vol params s16 ms8 =
>   let
>     ap                 :: AbsPitch       = fromIntegral (rRootKey reconL)
>     (st, en)           :: (Word, Word)   = (rStart reconL, rEnd reconL)
>     ns                 :: Double         = fromIntegral (en - st + 1)
>     secsSample         :: Double         = ns / sr
>     secsScored         :: Double         = 2 * fromRational dur
>     freqRatio          :: Double         = apToHz ap / apToHz pch
>     rateRatio          :: Double         = 44100 / sr
>     freqFactor         :: Double         = freqRatio * rateRatio / rPitchCorrection reconL
>     sig                :: AudSF () (Double, Double)
>                                          = (if looping
>                                             then eutDriverLooping 0 delta (normalizeLooping reconL)
>                                             else eutDriverNotLooping 0 delta)
>                                             >>> eutRelayStereo s16 ms8 secsScored (reconL, reconR) vol dur
>                                             >>> eutEffects secsScored (reconL, reconR)
>     looping            :: Bool           = secsScored > secsSample
>                                            && (rSampleMode reconL /= A.NoLoop)
>                                            && useLoopSwitching
>     delta              :: Double         = 1 / (secsSample * freqFactor * sr)
>   in sig
>
> eutDriverNotLooping    :: Double
>                           → Double
>                           → AudSF () Double 
> eutDriverNotLooping iphs delta
>   | traceIf msg False = undefined
>   | otherwise =
>   proc () → do
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>   where
>     msg = unwords ["eutDriverNotLooping iphs=", show iphs, "delta=", show delta]
>
> eutDriverLooping       :: Double
>                           → Double
>                           → (Double, Double)
>                           → AudSF () Double 
> eutDriverLooping iphs delta (lst, len)
>   | traceIf msg False = undefined
>   | otherwise =
>   proc () → do
>     rec
>       let sentinel     = if next > 1
>                          then len
>                          else 0.999
>       let phase'       = if phase > sentinel
>                          then lst 
>                          else phase
>       next ← delay iphs ⤙ next + delta
>       phase ← delay iphs ⤙ frac (phase' + delta)
>     outA ⤙ frac phase'
>   where
>     msg = unwords ["eutDriverLooping iphs=", show iphs, "delta=", show delta
>                               , "lst, len=", show (lst, len)]
>
> eutRelayStereo         :: A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Double
>                           → (Reconciled, Reconciled)
>                           → Volume
>                           → Dur
>                           → AudSF Double (Double, Double)
> eutRelayStereo s16 ms8 secsScored (reconL, reconR) vol dur
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     (stL, enL)         :: (Word, Word)   = (rStart reconL, rEnd reconL)
>     (stR, enR)         :: (Word, Word)   = (rStart reconR, rEnd reconR)
>     numS               :: Double         = fromIntegral (enL - stL + 1)
>     amp                :: Double         = fromIntegral vol / 100 / rAttenuation reconL
>
>   in proc pos → do
>     let saddrL         :: Int            = fromIntegral stL + truncate (numS * pos)
>     let saddrR         :: Int            = fromIntegral stR + truncate (numS * pos)
>     let ok = True
>     let (a1L, a1R) = if ok
>                      then getSampleVals (saddrL, saddrR)
>                      else error "bad addrs (wink)"
>     rec
>       aenvL ← if useEnvelopes
>               then doEnvelope secsScored (rEnvelope reconL) ⤙ ()
>               else constA 1                                 ⤙ ()
>       aenvR ← if useEnvelopes
>               then doEnvelope secsScored (rEnvelope reconR) ⤙ ()
>               else constA 1                                 ⤙ ()
>       a2L   ← if useLowPassFilter
>               then filterLowPass                            ⤙ (a1L,20000*aenvL)
>               else delay 0                                  ⤙ a1L
>       a2R   ← if useLowPassFilter
>               then filterLowPass                            ⤙ (a1R,20000*aenvR)
>               else delay 0                                  ⤙ a1R
>     let (sL, sR) = (a2L*amp*aenvL, a2R*amp*aenvR)
>     let ok = lookAtEveryPoint amp a2L aenvL a2R aenvR
>     let (sL', sR') = if ok
>                      then (sL, sR)
>                      else error "bad point (wink)"
>     outA ⤙ (sL', sR')
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
>       | traceNever msg' False = undefined
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
>   | traceIf msg False = undefined
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
> resolvePitchCorrection alt mps mpc       = if usePitchCorrection
>                                              then 2 ** (fromIntegral cents/12/100)
>                                              else 1.0
>   where
>     cents              :: Int            = if isJust mps || isJust mpc
>                                              then coarse * 100 + fine
>                                              else alt
>     coarse             :: Int            = fromMaybe 0 mps
>     fine               :: Int            = fromMaybe 0 mpc
>
> fromCentibels          :: Maybe Int → Double
> fromCentibels mcents                     = 10**(fromIntegral (fromMaybe 0 mcents)/100)

Envelopes =============================================================================================================

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
>       where jS = clip iS 0 1000

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

Effects ===============================================================================================================

> eutEffects             :: Double
>                           → (Reconciled, Reconciled)
>                           → AudSF (Double, Double) (Double, Double)
> eutEffects secsScored (reconL, reconR)
>   | traceIf msg False = undefined
>   | otherwise =
>   proc (aL, aR) → do
>     (chL, chR) ← if cFactorL <= 0 || cFactorR <= 0
>                  then delay (0,0) ⤙ (aL, aR)
>                  else eutChorus 15.0 0.005 0.1 ⤙ (aL, aR)
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
>     let (cL, cR) = doPan (pFactorL, pFactorR) (mixL, mixR)
>     let (wtL, wtR) = (cL / 2, cR / 2)
>     outA ⤙ (wtL, wtR)
>
>   where
>     cFactorL = if useEffectChorus
>                then fromMaybe 0 (efChorus effL)
>                else 0.0
>     cFactorR = if useEffectChorus
>                then fromMaybe 0 (efChorus effR)
>                else 0.0
>     rFactorL = if useEffectReverb
>                then fromMaybe 0 (efReverb effL)
>                else 0.0
>     rFactorR = if useEffectReverb
>                then fromMaybe 0 (efReverb effR)
>                else 0.0
>     pFactorL = if useEffectPan
>                then fromMaybe 0 (efPan effL)
>                else 0.0
>     pFactorR = if useEffectPan
>                then fromMaybe 0 (efPan effR)
>                else 0.0
>
>     (effL, effR) = (rEffects reconL, rEffects reconR)
>
>     msg = unwords ["eutEffects=", show effL, "=LR=", show effR, "rFactor*=", show rFactorL, " ", show rFactorR]
> 
> eutChorus              :: Double
>                           → Double
>                           → Double
>                           → AudSF (Double, Double) (Double, Double)
> eutChorus rate depth gain =
>   proc (sinL, sinR) → do
>     rec
>       lfo ← osc (tableSines 4096 [1]) 0 ⤙ rate
>
>       z1L ← delayLine1 0.030 ⤙ (sinL, 0.010 + depth * lfo)
>       z2L ← delayLine1 0.030 ⤙ (sinL, 0.020 + depth * lfo)
>       z3L ← delayLine1 0.030 ⤙ (sinL, 0.030 + depth * lfo)
>
>       z1R ← delayLine1 0.030 ⤙ (sinR, 0.010 + depth * lfo)
>       z2R ← delayLine1 0.030 ⤙ (sinR, 0.020 + depth * lfo)
>       z3R ← delayLine1 0.030 ⤙ (sinR, 0.030 + depth * lfo)
>
>       rL ← delayLine 0.0001 ⤙ (sinL + z1L + z2L + z3L)/4 + rL * gain
>       rR ← delayLine 0.0001 ⤙ (sinR + z1R + z2R + z3R)/4 + rR * gain
>     outA ⤙ (rL, rR)
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
> eatFreeVerb            :: STK.FreeVerb → AudSF (Double, Double) (Double, Double)
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
> comb                   :: forall p . Clock p => Word64 → STK.FilterData → Signal p Double Double
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
> allpass                :: forall p . Clock p => Word64 → Signal p Double Double
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
> doPan (azimuth, _) (sinL, sinR) = (ampL, ampR)
>   where
>     ampL = sinL * cos ((azimuth + 0.5) * pi / 2)
>     ampR = sinR - ampL
>
> dcBlock                :: Double → AudSF Double Double
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

> data Reconciled =
>   Reconciled {
>     rSampleMode        :: A.SampleMode
>   , rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: Word
>   , rPitchCorrection   :: Double
>   , rForceKey          :: Maybe AbsPitch
>   , rForceVel          :: Maybe Volume
>   , rAttenuation       :: Double
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

Knobs and buttons =====================================================================================================

> usePitchCorrection = True
> useEnvelopes       = True
> useShortening      = True
> useLoopSwitching   = True
> useLoopPhaseCalc   = False
> useLowPassFilter   = False
> useEffectReverb    = True
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
>
> ffThreshold            :: Double         = 150
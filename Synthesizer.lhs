> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module Synthesizer where
>
> import Control.Arrow
> -- import Control.Arrow.ArrowP ( ArrowP(ArrowP), strip )
> import Data.Array.Unboxed ( Array, (!), IArray(bounds) )
> import qualified Data.Audio           as A
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List
> import Data.Maybe (isJust, fromJust, fromMaybe)
> import Data.Word
> import Euterpea.IO.Audio.Basics
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA )
> import Parthenopea

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
>     ns                 :: Double              = fromIntegral $ rEnd rDataL - rStart rDataL + 1
>     secsSample         :: Double              = ns / sr
>     secsScored         :: Double              = 2 * fromRational dur
>     freqFactor         :: Double              = if usePitchCorrection
>                                                 then freqRatio * rateRatio / rPitchCorrection rDataL
>                                                 else freqRatio * rateRatio
>     freqRatio          :: Double              = apToHz ap / apToHz pch
>     rateRatio          :: Double              = 44100 / sr
>     sig                :: AudSF () (Double, Double)
>                                               = eutDriver rDataL secsSample secsScored sr 0 freqFactor
>                                             >>> eutRelayStereo s16 ms8 secsScored (rDataL, rDataR) vol dur
>                                             >>> eutEffects (rEffects rDataL)
>   in sig
>
> eutDriverFull          :: Double
>                           → Double
>                           → AudSF () Double 
> eutDriverFull iphs delta
>   | traceIf msg False = undefined
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
>   | traceIf msg False = undefined
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
> eutDriver recon secsSample secsScored sr iphs freqFactor
>   | traceIf msg False = undefined
>   | otherwise =
>     let
>       delta            :: Double         = 1 / (secsSample * freqFactor * sr)
>       eps              :: Double         = 0.000001
>       (normst,normen)  :: (Double, Double)
>                                          = normalizeLooping recon
>     in proc () → do
>       envf   ← envLineSeg  [0, 1, 1, 0, 0] 
>                             [eps, secsSample, eps, secsScored - secsSample]    ⤙ ()
>       envl   ← envLineSeg  [0, 0, 1, 1] 
>                             [secsSample, eps, secsScored - secsSample]         ⤙ ()
>       pfull  ← eutDriverFull 0 delta                                           ⤙ ()
>       ploop  ← eutDriverLooping normst delta (normst, normen)                  ⤙ ()
>     
>       let ok = lookAtEveryPoint envf pfull envl ploop
>       let curPos = if ok
>                    then if (rSampleMode recon /= A.NoLoop) && useLoopSwitching
>                         then envf*pfull + envl*ploop
>                         else envf*pfull
>                    else error "bad point"
>     
>       outA ⤙ curPos
>   where
>     lookAtEveryPoint :: Double → Double → Double → Double → Bool
>     lookAtEveryPoint envf pfull envl ploop
>       | traceIf msg False = undefined
>       | otherwise = True
>       where
>         msg = unwords [show (envf*pfull + envl*ploop), "=", show envf, ",", show pfull,  ",", show envl, ",", show ploop]
>     msg = unwords ["eutDriver secsSample = ", show secsSample, " secsScored = ", show secsScored]
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
> resolvePitchCorrection alt mps mpc = 2 ** (fromIntegral cents/12/100)
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
>   Effects (fmap (conv (0, 100)) mChorus)
>           (fmap (conv (0, 100)) mReverb)
>           (fmap (conv (-50, 50)) mPan)
>   where
>     conv               :: (Int, Int) → Int → Double
>     conv (nMin, nMax) nEffect = fromIntegral nEffect''/100
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
>     a1        = amps !! (ix + 1 )
>     a'        = a0 + fact * (a1 - a0)    
>     amps'     = take (ix+1) amps ++ [a', 0, 0]
>     deltaTs'' = deltaTs' ++ [0.001, 0.001]
>
> eutEffects             :: Effects → AudSF (Double, Double) (Double, Double)
> eutEffects eff = 
>   proc (a, b) → do
>     let (a', b') = (doChorus (efChorus eff) a, doChorus (efChorus eff) b)
>     let (a', b') = (doReverb (efReverb eff) a, doReverb (efReverb eff) b)
>     let (a'', b'') = doPan (efPan eff) (a', b')
>     outA ⤙ (a'',b'')
> 
> doChorus               :: Maybe Double → Double → Double
> doChorus mE a = a
> doReverb               :: Maybe Double → Double → Double
> doReverb mE a = a
> doPan                  :: Maybe Double → (Double, Double) → (Double, Double)
> doPan mE (x, y) = (x, y)

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

Knobs and buttons =========================================================================

> usePitchCorrection = True
> useEnvelopes       = True
> useShortening      = True
> useLoopSwitching   = True
> useLowPassFilter   = False
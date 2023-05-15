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
> import Data.Maybe (isJust, fromJust, fromMaybe)
> import Euterpea.IO.Audio.Basics
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA )
> import FRP.UISF.UISF
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
>     secsTotal          :: Double              = fromRational dur
>
>     (fst, fen)         :: (Double, Double)    = (fromIntegral $ rStart     rDataL, fromIntegral $ rEnd rDataL)
>
>     ns'                :: Double              = fen - fst + 1
>     secs'              :: Double              = ns' / sr
>     freqFactor         :: Double              = if usePitchCorrection
>                                                 then freqRatio * rateRatio / rPitchCorrection rDataL
>                                                 else freqRatio * rateRatio
>     freqRatio          :: Double              = apToHz ap / apToHz pch
>     rateRatio          :: Double              = 44100 / sr
>     sig                :: AudSF () (Double, Double)
>                                               = eutPhaser rDataL secsSample secsTotal sr 0 freqFactor
>                                             >>> eutRelayStereo s16 ms8 secs' (rDataL, rDataR) vol dur
>   in sig
>
> eutPhaserNormal        :: Double
>                           → Double
>                           → AudSF () Double 
> eutPhaserNormal iphs delta
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
>     msg = unwords ["eutPhaserNormal iphs=", show iphs, "delta=", show delta]
>
> eutPhaserLooping       :: Double
>                           → Double
>                           → (Double, Double)
>                           → AudSF () Double 
> eutPhaserLooping iphs delta (lst, len)
>   | traceIf msg False = undefined
>   | otherwise =
>   let frac             :: RealFrac r ⇒ r → r
>       frac                           = snd . properFraction
>   in proc () → do
>     rec
>       let phase = if next > len then frac lst else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>   where
>     msg = unwords ["eutPhaseLooping iphs=", show iphs, "delta=", show delta, "lst, len=", show (lst, len)]
>
> eutPhaser              :: Reconciled
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → AudSF () Double 
> eutPhaser recon secsSample secsTotal sr iphs freqFactor
>   | traceAlways msg False = undefined
>   | otherwise =
>     let
>       delta            :: Double         = 1 / (secsSample * freqFactor * sr)
>       eps              :: Double         = 0.000001
>       (normst,normen)  :: (Double, Double)
>                                          = normalizeLooping recon
>     in proc () → do
>       envf   ← envLineSeg  [0, 1, 1, 0, 0] 
>                             [eps, secsSample, eps, 100]              ⤙ ()
>       envl   ← envLineSeg  [0, 0, 1, 1] 
>                             [secsSample, eps, 100]                   ⤙ ()
>       pfull  ← eutPhaserNormal 0 delta                               ⤙ ()
>       ploop  ← eutPhaserLooping normst delta (normst, normen)        ⤙ ()
>     
>       let ok = lookAtEveryPoint envf pfull envl ploop
>       let curPos = if ok
>                    then envf*pfull + envl*ploop
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
>     msg = unwords ["eutPhaser secsSample = ", show secsSample, " secsTotal = ", show secsTotal]
>
> eutRelayStereo         :: A.SampleData Int16
>                           → Maybe (A.SampleData Int8)
>                           → Double
>                           → (Reconciled, Reconciled)
>                           → Volume
>                           → Dur
>                           → AudSF Double (Double, Double)
> eutRelayStereo s16 ms8 stime (rDataL, rDataR) vol dur
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
>               then doEnvelope (rEnvelope rDataL) stime ⤙ ()
>               else constA 1                            ⤙ ()
>       aenvR ← if useEnvelopes
>               then doEnvelope (rEnvelope rDataR) stime ⤙ ()
>               else constA 1                            ⤙ ()
>       a2L   ← if useLowPassFilter
>               then filterLowPass         ⤙ (a1L,20000*aenvL)
>               else delay 0               ⤙ a1L
>       a2R   ← if useLowPassFilter
>               then filterLowPass         ⤙ (a1R,20000*aenvR)
>               else delay 0               ⤙ a1R
>     outA ⤙ (a2L*amp*aenvL, a2R*amp*aenvR)
>
>   where
>     msg = unwords ["eutRelayStereo=", show rDataL, "<-L...", show (rDataL == rDataR), "...R->", show rDataR]
>     compute24          :: Int16 → Int8 → Double
>     compute24 i16 i8 = d24
>       where
>         f8to32         :: Int8 → Int32
>         f8to32 = fromIntegral
>         f16to32         :: Int16 → Int32
>         f16to32 = fromIntegral
>         d24            :: Double
>         d24 = fromIntegral (f16to32 i16 * 32768 + f8to32 i8)       
>      
>     doEnvelope         :: Envelope → Double → AudSF () Double
>     doEnvelope renv secs
>       | traceIf msg False = undefined
>       | otherwise = envDAHdSR secs
>                               (eDelayT       renv)
>                               (eAttackT      renv)
>                               (eHoldT        renv)
>                               (eDecayT       renv)
>                               (eSustainLevel renv)
>                               (eReleaseT     renv)
>       where
>         msg = unwords ["Envelope=", show renv]
>      
>     getSampleVals      :: (Int, Int) → (Double, Double)
>     getSampleVals (saddrL, saddrR)
>       | traceIf msg' False = undefined
>       | otherwise = 
>         if isJust ms8
>         then (compute24 (s16 ! saddrL) (fromJust ms8 ! saddrL)
>             , compute24 (s16 ! saddrR) (fromJust ms8 ! saddrR))
>         else (fromIntegral (s16 ! saddrL)
>             , fromIntegral (s16 ! saddrR))
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
>     msg = unwords ["normalizeLooping, recon= ", show recon, " full=", show fullst, " ", show fullen, " loop=", show loopst, " ", show loopen, " denom=", show denom]
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
> acceptSustain          :: Int → Double
> acceptSustain iS = 1/raw iS
>   where
>     raw                :: Int → Double
>     raw iS = pow 10 (fromIntegral jS/200)
>       where jS
>               | iS <= 0 = 0
>               | iS >= 1000 = 1000
>               | otherwise = iS

Implements the SoundFont envelope model with:
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

Creates an envelope generator with straight-line (delayed) attack, hold, decay, release.  

> envDAHdSR              :: (Clock p) ⇒
>                            Double
>                            → Double
>                            → Double
>                            → Double
>                            → Double
>                            → Double
>                            → Double
>                            → Signal p () Double
> envDAHdSR secs del att hold dec sus release
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     sf = envLineSeg [0,0,1,1,sus,sus,0] [del, att, hold, dec, max 0 sustime, release]
>   in proc () → do
>     env ← sf ⤙ ()
>     outA ⤙ env
>   where
>     sustime = secs - (del + att + hold + dec + release)
>     msg = unwords [show sus, "=sus/ ", show secs, show (del + att + hold + dec + sustime + release), "=secs,total/", 
>                    "dahdr=", show del, show att, show hold, show dec, show release]
>
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
>     rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: Word
>   , rPitchCorrection   :: Double
>   , rEnvelope          :: Envelope} deriving (Eq, Show)
>           
> data Envelope =
>   Envelope {
>     eDelayT            :: Double
>   , eAttackT           :: Double
>   , eHoldT             :: Double
>   , eDecayT            :: Double
>   , eSustainLevel      :: Double
>   , eReleaseT          :: Double} deriving (Eq, Show)

Knobs and buttons =========================================================================

> usePitchCorrection = True
> useEnvelopes       = False
> useLowPassFilter   = False

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
> import Euterpea.Music
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA )
> import FRP.UISF.UISF
> import Parthenopea

Signal function-based synth ===============================================================

> eutPhaserNormal        :: Double
>                           → Double
>                           → AudSF () Double 
> eutPhaserNormal iphs delta =
>   let frac             :: RealFrac r ⇒ r → r
>       frac = snd . properFraction
>   in proc () → do
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> eutPhaserLooping       :: Double
>                           → Double
>                           → (Double, Double)
>                           → AudSF () Double 
> eutPhaserLooping iphs delta (st, en) =
>   let frac             :: RealFrac r ⇒ r → r
>       frac                           = snd . properFraction
>   in proc () → do
>     rec
>       let phase = if next > en then frac st else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> eutPhaser              :: Reconciled
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → Double
>                           → (Double, Double)
>                           → AudSF () Double 
> eutPhaser recon secsSample secsTotal sr iphs freqFactor (st, en)
>   | traceIf msg False = undefined
>   | otherwise =
>     let
>       delta            :: Double         = 1 / (secsSample * freqFactor * sr)
>       eps              :: Double         = 0.000001
>     in proc () → do
>       envn   ← envLineSeg  [0, 1, 1, 0, 0] 
>                             [eps, secsSample, eps, 100]            ⤙ ()
>       envl   ← envLineSeg  [0, 0, 1, 1] 
>                             [secsSample, eps, 100]                 ⤙ ()
>       pnorm  ← eutPhaserNormal 0 delta                             ⤙ ()
>       ploop  ← eutPhaserLooping st delta (normalizeLooping recon)  ⤙ ()
>     
>       let ok = lookAtEveryPoint envn pnorm envl ploop
>       let curPos = if ok
>                    then envn*pnorm + envl*ploop
>                    else error "bad point"
>     
>       outA ⤙ curPos
>   where
>     lookAtEveryPoint :: Double → Double → Double → Double → Bool
>     lookAtEveryPoint env1 pnorm env2 ploop
>       | traceIf msg False = undefined
>       | otherwise = True
>       where
>         msg = unwords ["phaser=",   show env1
>                      , " ",         show pnorm
>                      , " ",         show env2
>                      , " ",         show ploop
>                      , "... pos =", show (env1*pnorm + env2*ploop)]
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
>       | traceAlways msg False = undefined
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

> normalizeLooping       :: Reconciled → (Double, Double)
> normalizeLooping recon =
>   let
>     nst                :: Double = fromIntegral $ rStart recon 
>     nen                :: Double = fromIntegral $ rEnd recon 
>     pst                :: Double = fromIntegral $ rLoopStart recon 
>     pen                :: Double = fromIntegral $ rLoopEnd recon 
>     len                :: Double = nen - nst + 1
>     mst                :: Double = (pst - nst) / len
>     men                :: Double = (pen - nen) / len
>   in
>     (mst, men)
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

Knobs and buttons =========================================================================

> useEnvelopes       = False
> useLowPassFilter   = False

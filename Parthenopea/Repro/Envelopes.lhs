> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Envelopes
William Clements
Apr 26, 2025

> module Parthenopea.Repro.Envelopes where
>
> import Data.List
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Maybe ( isJust, isNothing, fromJust )
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg )
> import Euterpea.IO.Audio.Types ( Clock(..), Signal )
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Utility
  
volume and modulation envelopes =======================================================================================

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
   delay
   attack
   hold
   decay
   sustain
   release  

> proposeSegments        :: TimeFrame → FEnvelope → (FEnvelope, Segments)
> proposeSegments tf envRaw                = (r, segs)
>   where
>     fName                                = "proposeSegments"
>
>     r                  :: FEnvelope      =
>       let
>         extras                           =
>           EnvelopeExtras secs releaseT releaseT
>       in
>         refineEnvelope envRaw{fExtras = Just extras}
>
>     segs                                 =
>       let
>         amps                             =
>           [0,            0,             1,          1,       fSusLevel,     fSusLevel,      0,       0]
>         deltaTs                          =
>           [   r.fDelayT,    r.fAttackT,    r.fHoldT,   r.fDecayT,  r.fSustainT,    releaseT,     1]
>       in
>         Segments amps deltaTs
>
>     releaseT
>       | secs < (7 * minDeltaT)           = error $ unwords [fName, "Note too short:", show secs]
>       | secs < (7 * minDeltaT) + minUseful
>                                          = minDeltaT
>       | secs < (7 * minDeltaT + 2 * minUseful)
>                                          = minUseful / 4
>       | otherwise                        = minUseful
>           
>     fSusLevel                            = clip (0, 1) r.fSustainLevel
>     secs                                 = tf.tfSecsToPlay
>
> doSweepingEnvelope     :: ∀ p . Clock p ⇒ Double → Either Velocity (VB.Vector Double) → Signal p () Double
> doSweepingEnvelope secs                  = either (constA . fromIntegral) (doSweeps secs)
>
> doSweeps               :: ∀ p . Clock p ⇒ Double → VB.Vector Double → Signal p () Double
> doSweeps secs sweeps                     = envLineSeg segs.sAmps segs.sDeltaTs
>   where
>     segs               :: Segments       =
>       case VB.length sweeps of
>         2                                → segmentsFor2
>         4                                → segmentsFor4
>         _                                →
>           error $ unwords [show $ VB.length sweeps, "is illegal (not two or four) length for velo sweeping"]
>
>     step, midsection, leg
>                        :: Double 
>     step                                 = secs - 2 * minDeltaT
>     midsection                           = step / 8
>     leg                                  = step * 7 / 16
>
>     segmentsFor2, segmentsFor4
>                        :: Segments
>     segmentsFor2                         =
>       Segments
>         [0,      sweeps VB.! 0,    sweeps VB.! 1,         0,           0]
>         [ minDeltaT,       step,       minDeltaT,  minUseful]
>     segmentsFor4                         =
>       Segments
>         [   0,  sweeps VB.! 0,   sweeps VB.! 1,  sweeps VB.! 2,  sweeps VB.! 3,       0,            0]
>         [  minDeltaT,     leg,      midsection,         leg,            minDeltaT,    minUseful]

stepwise refinement from specified envelope parameters ================================================================

There is design intent hidden in input envelope values that are too large to make sense. Some synthesizers must 
interpret them somehow.

> feSum, feRemaining, feTarget
>                        :: FEnvelope → Double
> feSum fe                                 =
>   fe.fDelayT + fe.fAttackT + fe.fHoldT + fe.fDecayT + fe.fSustainT + ee.eeReleaseT + ee.eePostT
>   where
>     ee                                   = fromJust fe.fExtras
> feRemaining work                         = feTarget work - feSum work
> feTarget work                            = (fromJust work.fExtras).eeTargetT
>
> data FIterate                            =
>   FIterate {
>     fiFun              :: FIterate → FIterate
>   , fiEnvWork          :: FEnvelope
>   , fiDone             :: Bool}
> data FCase                               =
>   FCase {
>     fcDAH              :: Bool
>   , fcDecay            :: Bool} deriving (Eq, Ord, Show)
> evaluateCase           :: FEnvelope → FCase
> evaluateCase fe                          =
>   FCase
>     ((fe.fDelayT + fe.fAttackT + fe.fHoldT) >= (9/10) * feTarget fe)
>     (fe.fDecayT >= (7/10) * feTarget fe)
>
> refineEnvelope         :: FEnvelope → FEnvelope
> refineEnvelope fEnvIn                    = result.fiEnvWork
>   where
>     result                               = head $ dropWhile unfinished $ iterate' nextGen fiInit
>
>     fiInit                               =
>       FIterate 
>         (caseActions Map.! evaluateCase fEnvIn) 
>         fEnvIn
>         False
>     nextGen fi                           = fi.fiFun fi
>     unfinished fi                        = not fi.fiDone
>
>     caseActions        :: Map FCase (FIterate → FIterate)
>     caseActions                          =
>       Map.fromList [
>           (FCase False False, faceValue)
>         , (FCase False True, decayTooLong)
>
>         , (FCase True False, dahTooLong)
>         , (FCase True True, dahTooLong)]
>
> feCheckFinal           :: FIterate → FIterate
> feCheckFinal iterIn                      =
>   profess
>     (abs (theSum - targetT) < epsilon)
>     (unwords ["feCheckFinal failure: sum, target", show (theSum, targetT)])
>     iterIn
>   where
>     theSum                               = feSum iterIn.fiEnvWork
>     targetT                              = feTarget iterIn.fiEnvWork
>
> feFinish               :: FIterate → FEnvelope → FIterate
> feFinish iterIn workee                   =
>   feCheckFinal iterIn{fiFun = id, fiEnvWork = workee, fiDone = True}
>
> feContinue             :: FIterate → FEnvelope → (FIterate → FIterate) → FIterate
> feContinue iterIn workee fun             = iterIn{fiFun = fun, fiEnvWork = workee}
>
> faceValue, dahTooLong, decayTooLong, reduceSustain, reduceDecay, reduceHold, reduceAttack, reduceDelay
>                        :: FIterate → FIterate
>
> faceValue iterIn
>   | remaining < minDeltaT                = feContinue iterIn work reduceSustain
>   | otherwise                            = feFinish iterIn work{fSustainT = remaining + work.fSustainT}
>   where
>     work                                 = iterIn.fiEnvWork
>     remaining                            = feRemaining work
>
> dahTooLong iterIn                        = feContinue iterIn work reduceHold
>   where
>     work                                 =
>       iterIn.fiEnvWork{fDecayT = minDeltaT, fSustainT = minDeltaT}
>
> decayTooLong iterIn
>   | remaining < 0                        = feContinue iterIn work dahTooLong
>   | otherwise                            = feFinish iterIn work{fDecayT = remaining + work.fDecayT}
>   where
>     work                                 = iterIn.fiEnvWork{fDecayT = minDeltaT}
>     remaining                            = feRemaining work
>
> reduceSustain iterIn                     =
>   let
>     work                                 = iterIn.fiEnvWork{fSustainT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceDecay
>       else feFinish iterIn work{fSustainT = remaining + work.fSustainT}
>
> reduceDecay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDecayT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceHold
>       else feFinish iterIn work{fDecayT = remaining + work.fDecayT}
>
> reduceHold iterIn                        = 
>   let
>     work                                 = iterIn.fiEnvWork{fHoldT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceAttack
>       else feFinish iterIn work{fHoldT = remaining + work.fHoldT}
>
> reduceAttack iterIn                      =
>   let
>     work                                 = iterIn.fiEnvWork{fAttackT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceDelay
>       else feFinish iterIn work{fAttackT = remaining + work.fAttackT}
>
> reduceDelay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDelayT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     feFinish iterIn work{fDelayT = remaining + work.fDelayT}
>
> deriveEnvelope         :: SynthSwitches
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe (Maybe Int, Maybe Int)
>                           → Maybe FEnvelope
> deriveEnvelope sw mDelay mAttack mHold mDecay mSustain mTriple
>                                          =
>   if sw.useEnvelopes && doUse mTriple then Just env else Nothing
>   where
>     env                                  =
>       FEnvelope
>         Nothing
>         (fromTithe mSustain (isNothing mTriple))
>         (makeModTriple mTriple)
>         (fromTimecents mDelay)
>         (fromTimecents mAttack)
>         (fromTimecents mHold)
>         (fromTimecents mDecay)
>         minDeltaT
>
>     doUse              :: Maybe (Maybe Int, Maybe Int) → Bool
>     doUse                                =
>       \case
>         Nothing                          → True
>         Just (xToPitch, xToFilterFc)     → isJust xToPitch || isJust xToFilterFc
>
>     makeModTriple      :: Maybe (Maybe Int, Maybe Int) → Maybe ModTriple
>     makeModTriple                        = maybe Nothing (uncurry go)
>       where
>         go toPitch toFilterFc            = Just (deriveModTriple toPitch toFilterFc Nothing)
>
> data TimeFrame                           =
>   TimeFrame {
>     tfSecsSampled      :: Double
>   , tfSecsScored       :: Double
>   , tfSecsToPlay       :: Double
>   , tfLooping          :: Bool} deriving (Eq, Show)
> data EnvelopeExtras                      =
>   EnvelopeExtras {
>     eeTargetT          :: Double
>   , eeReleaseT         :: Double
>   , eePostT            :: Double} deriving (Eq, Show)
> data FEnvelope                           =
>   FEnvelope {
>     fExtras            :: Maybe EnvelopeExtras
>   , fSustainLevel      :: Double
>   , fModTriple         :: Maybe ModTriple
>
>   , fDelayT            :: Double
>   , fAttackT           :: Double
>   , fHoldT             :: Double
>   , fDecayT            :: Double
>   , fSustainT          :: Double}
>   deriving (Eq, Show)
> data Segments                            =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show

The End
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Envelopes
William Clements
Apr 26, 2025

> module Parthenopea.Repro.Envelopes(
>          audRate
>        , ctrRate
>        , deriveEnvelope
>        , doEnvelope
>        , doVeloSweepingEnvelope
>        , minDeltaT
>        , minUseful
>        , proposeSegments
>        , vetAsDiscreteSig
>        , vetEnvelope) where
>
> import Data.Foldable
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, isNothing, fromJust, fromMaybe )
> import qualified Data.Vector             as VB
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg )
> import Euterpea.IO.Audio.Types ( Clock(..), Signal )
> import Parthenopea.Debug
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Modulation

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
 - delay - ~zero most of the time
   attack
   hold
   decay
   sustain
   release  

> doEnvelope             :: ∀ p . Clock p ⇒ TimeFrame → Maybe FEnvelope → Signal p () Double
> doEnvelope timeFrame                     = maybe (constA 1) makeSF
>   where
>     makeSF             :: FEnvelope → Signal p () Double
>     makeSF envIn                         =
>       let
>         (envNow, segs)                   = proposeSegments timeFrame envIn
>         ok                               = vetEnvelope envNow segs
>       in
>         if ok
>           then envLineSeg segs.sAmps segs.sDeltaTs
>           else error $ unwords ["unexpected"]
>
> proposeSegments        :: TimeFrame → FEnvelope → (FEnvelope, Segments)
> proposeSegments tf envRaw                = (r, segs)
>   where
>     r                  :: FEnvelope      =
>       refineEnvelope envRaw{fTargetT = Just (tf.tfSecsScored, releaseT, releaseT)}
>
>     amps, deltaTs      :: [Double]
>     amps                                 =
>             [0,            0,             1,          1,       fSusLevel,     fSusLevel,      0,       0]
>     deltaTs                              =
>             [   r.fDelayT,    r.fAttackT,    r.fHoldT,    r.fDecayT,   r.fSustainT,   releaseT,  1]
>
>     segs                                 = Segments amps deltaTs
>
>     releaseT
>       | tf.tfSecsScored < (7 * minDeltaT)
>                                          = error $ unwords ["Note too short:", show tf.tfSecsScored]
>       | tf.tfSecsScored < (7 * minDeltaT) + minUseful
>                                          = minDeltaT
>       | tf.tfSecsScored < (7 * minDeltaT + 2 * minUseful)
>                                          = minUseful / 4
>       | otherwise                        = minUseful
>           
>     fSusLevel                            = clip (0, 1) r.fSustainLevel
>
> doVeloSweepingEnvelope :: ∀ p . Clock p ⇒ TimeFrame → VB.Vector Double → Signal p () Double
> doVeloSweepingEnvelope timeFrame vIn
>   | 0 == dLen                            = constA 128
>   | 2 == dLen                            = runEnvelope segmentsFor2
>   | 4 == dLen                            = runEnvelope segmentsFor4
>   | otherwise                            =
>       error $ unwords [show dLen, "is llegal length for velo sweeping directive"]
>   where
>     dLen                                 = VB.length vIn
>
>     stVelo0, enVelo0, stVelo1, enVelo1, step, midsection, leg
>                        :: Double 
>     stVelo0                              = vIn VB.! 0
>     enVelo0                              = vIn VB.! 1
>     stVelo1                              = vIn VB.! 2
>     enVelo1                              = vIn VB.! 3
>
>     step                                 = timeFrame.tfSecsScored - 2 * minDeltaT
>     midsection                           = step / 8
>     leg                                  = step * 7 / 16
>
>     segmentsFor2, segmentsFor4
>                        :: Segments
>     segmentsFor2                         =
>       Segments
>         [0,         stVelo0,       enVelo0,         0,           0]
>         [ minDeltaT,       step,       minDeltaT,  minUseful]
>     segmentsFor4                         =
>       Segments
>         [   0,      stVelo0,     enVelo0,     stVelo1,     enVelo1,       0,            0]
>         [  minDeltaT,     leg,      midsection,     leg,     minDeltaT,    minUseful]
>
>     runEnvelope         :: Segments → Signal p () Double
>     runEnvelope segs
>       | traceIf trace_DE False           = undefined
>       | otherwise                        = envLineSeg segs.sAmps segs.sDeltaTs
>       where
>         fName                            = "runEnvelope"
>         trace_DE                         = unwords [fName, show vIn, show segs]

stepwise refinement from specified envelope parameters ================================================================

There is design intent hidden in input envelope values that are too large to make sense. Some synthesizer must 
interpret them somehow.

> feSum, feRemaining     :: FEnvelope → Double
> feSum fe                                 =
>   fe.fDelayT + fe.fAttackT + fe.fHoldT + fe.fDecayT + fe.fSustainT + releaseT + postT
>   where
>     (_, releaseT, postT)                 = fromJust fe.fTargetT
> feRemaining work                         = targetT - feSum work
>   where
>     (targetT, _, _)                      = fromJust work.fTargetT
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
>   let
>     (targetT, _, _)                      = fromJust fe.fTargetT
>   in
>     FCase
>       ((fe.fDelayT + fe.fAttackT + fe.fHoldT) >= (9/10) * targetT)
>       (fe.fDecayT >= (7/10) * targetT)
>
> refineEnvelope         :: FEnvelope → FEnvelope
> refineEnvelope fEnvIn                    = result.fiEnvWork
>   where
>     result                               = head $ dropWhile unfinished $ iterate nextGen fiInit
>
>     unfinished fi                        = not fi.fiDone
>     nextGen fi                           = fi.fiFun fi
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
>     fiInit                               =
>       FIterate (caseActions Map.! evaluateCase fEnvIn) fEnvIn False
>
> feCheckFinal           :: FIterate → FIterate
> feCheckFinal iterIn                      =
>   profess
>     (abs (theSum - targetT) < epsilon)
>     (unwords ["feCheckFinal failure: sum, target", show (theSum, targetT)])
>     iterIn
>   where
>     theSum                               = feSum iterIn.fiEnvWork
>     (targetT, _, _)                      = fromJust iterIn.fiEnvWork.fTargetT
>
> feFinish               :: FIterate → FEnvelope → FIterate
> feFinish iterIn workee                   =
>   feCheckFinal iterIn{fiFun = undefined, fiEnvWork = workee, fiDone = True}
>
> feContinue             :: FIterate → FEnvelope → (FIterate → FIterate) → FIterate
> feContinue iterIn workee fun             = iterIn{fiFun = fun, fiEnvWork = workee}
>
> faceValue, dahTooLong, decayTooLong
>                        :: FIterate → FIterate
> reduceSustain, reduceDecay, reduceHold, reduceAttack, reduceDelay
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
> decayTooLong iterIn
>   | remaining < 0                        = feContinue iterIn work dahTooLong
>   | otherwise                            = feFinish iterIn work{fDecayT = remaining + work.fDecayT}
>   where
>     work                                 = iterIn.fiEnvWork{fDecayT = minDeltaT}
>     remaining                            = feRemaining work
>
> deriveEnvelope         :: Maybe Int
>                           → Maybe Int
>                           → NoteOn
>                           → (Maybe Int, Maybe Int)
>                           → (Maybe Int, Maybe Int)
>                           → Maybe Int
>                           → Maybe (Maybe Int, Maybe Int)
>                           → Maybe FEnvelope
> deriveEnvelope mDelay mAttack noon (mHold, mHoldByKey) (mDecay, mDecayByKey)
>                mSustain mTriple          = if useEnvelopes && doUse mTriple
>                                              then Just env
>                                              else Nothing
>   where
>     dHold              :: Double         = max minDeltaT (fromTimecents' mHold  mHoldByKey  noon.noteOnKey)
>     dDecay             :: Double         = max minDeltaT (fromTimecents' mDecay mDecayByKey noon.noteOnKey)
>
>     env                                  =
>       FEnvelope
>         Nothing
>         (fromTithe mSustain (isNothing mTriple))
>         (makeModTriple mTriple)
>         (fromTimecents mDelay)
>         (fromTimecents mAttack)
>         dHold
>         dDecay
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

Emphasis on vetting ===================================================================================================

Viability of the envelope "proposal" is checked for a few conditions not easily diagnosed from listening to produced
audio. For example, there should always be zeros at the beginning and end of every note envelope.

> maybeVetAsDiscreteSig  :: FEnvelope → Segments → Bool
> maybeVetAsDiscreteSig env segs           =
>   case rateForVetEnvelope of
>     Nothing                              → True
>     Just clockRate                       → timeSkip || isJust (vetAsDiscreteSig clockRate env segs)
>   where
>     fName                                = "maybeVetAsDiscreteSig"
>
>     (secs, _, _)                         = deJust fName env.fTargetT
>     timeSkip                             = secs /= clip (1/32, 2) secs
>
> vetAsDiscreteSig       :: Double → FEnvelope → Segments → Maybe (DiscreteSig Double)
> vetAsDiscreteSig clockRate env segs
>   | sum prologlist > epsilon             = error $ unwords [fName, "non-zero prolog", show prologlist]
>   | sum epiloglist > epsilon             = error $ unwords [fName, "non-zero epilog", show epiloglist]
>   | isNothing env.fModTriple && dipix < (kSig' `div` 5)
>                                          =
>     error $ unwords [fName, "under", show dipThresh, "at", show dipix, "of", show (kSig, kVec)]
>   | otherwise                            = Just dsig
>   where
>     fName                                = "vetAsDiscreteSig"
>
>     dsig                                 = discretizeEnvelope clockRate env segs
>     (targetT, _, _)                      = deJust fName env.fTargetT
>
>     checkSize                            = truncate $ minDeltaT * clockRate
>     dipThresh          :: Double         = 1/10
>
>     kVec, kSig, kSig'  :: Int
>     kVec                                 = VU.length dsig.dsigVec
>     kSig                                 = truncate $ clockRate * targetT
>     kSig'                                = truncate $ clockRate * min targetT 0.5
>
>     prologlist, epiloglist
>                        :: [Double]
>     prologlist                           = VU.toList $ VU.force $ VU.slice 0                  checkSize dsig.dsigVec
>     epiloglist                           = VU.toList $ VU.force $ VU.slice (kSig - checkSize) checkSize dsig.dsigVec
>
>     skipSize                             = round $ (env.fDelayT + env.fAttackT) * clockRate
>     afterAttack                          = VU.slice skipSize (kSig - skipSize) dsig.dsigVec
>     dipix                                = skipSize + fromMaybe kSig (VU.findIndex (< dipThresh) afterAttack)
>       
> vetEnvelope            :: FEnvelope → Segments → Bool
> vetEnvelope env segs
>   | badAmp || badDeltaT                  = error $ unwords [fName, "negative amp or deltaT", show segs]
>   | abs (a - b) > 0.01 || abs (b - c) > 0.01
>                                          = error $ unwords [fName, "doesn't add up", show (a, b, c)]
>   | otherwise                            = maybeVetAsDiscreteSig env segs
>   where
>     fName                                = "vetEnvelope"
>
>     (targetT, _, postT)                  = deJust fName env.fTargetT
>
>     badAmp, badDeltaT  :: Bool
>     badAmp                               = isJust $ find (< 0) segs.sAmps
>     badDeltaT                            = isJust $ find (< 0) segs.sDeltaTs
>
>     a                                    = feSum env
>     b                                    = targetT
>     c                                    = foldl' (+) (postT - 1) segs.sDeltaTs
>
> useEnvelopes           :: Bool
> useEnvelopes                             = True
>
> rateForVetEnvelope     :: Maybe Double
> rateForVetEnvelope                       = Just ctrRate

The End
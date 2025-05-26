> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
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
> proposeSegments tf envRaw
>   | traceNot trace_PS False              = undefined
>   | otherwise                            = (r, segs)
>   where
>     fName                                = "proposeSegments"
>     trace_PS                             = unwords [fName, show (tf.tfSecsScored, amps, deltaTs)]
>
>     releaseT
>       | tf.tfSecsScored < 2 * minUseful  = minDeltaT
>       | otherwise                        = minUseful
>           
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
>     fSusLevel                            = clip (0, 1) r.fSustainLevel

discern design intent governing input Generator values, then implement something ======================================

> feSum, feRemaining     :: FEnvelope → Double
> feSum FEnvelope{ .. }                    
>                                          = fDelayT + fAttackT + fHoldT + fDecayT + fSustainT + releaseT + postT
>   where
>     (_, releaseT, postT)                 = fromJust fTargetT
> feRemaining work@FEnvelope{fTargetT}     = targetT - feSum work
>   where
>     (targetT, _, _)                      = fromJust fTargetT
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
> evaluateCase FEnvelope{ .. } 
>                                          =
>   let
>     (targetT, _, _)                      = fromJust fTargetT
>   in
>     FCase
>       ((fDelayT + fAttackT + fHoldT) >= (9/10) * targetT)
>       (fDecayT >= (7/10) * targetT)
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
> feFinish               :: FIterate → FEnvelope → FIterate
> feFinish iterIn workee                   = iterIn{fiEnvWork = workee, fiDone = True}
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
>   | traceNot trace_FV False              = undefined
>   | remaining < minDeltaT                = feContinue iterIn work reduceSustain
>   | otherwise                            = feFinish iterIn work{fSustainT = max minDeltaT remaining + work.fSustainT}
>   where
>     fName                                = "faceValue"
>     trace_FV                             = unwords[fName, show (remaining, work)]
>
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
>       else feFinish iterIn work{fSustainT = remaining}
>
> reduceDecay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDecayT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceHold
>       else feFinish iterIn work{fDecayT = remaining}
>
> reduceHold iterIn                        = 
>   let
>     work                                 = iterIn.fiEnvWork{fHoldT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceAttack
>       else feFinish iterIn work{fHoldT = remaining}
>
> reduceAttack iterIn                      =
>   let
>     work                                 = iterIn.fiEnvWork{fAttackT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceDelay
>       else feFinish iterIn work{fAttackT = remaining}
>
> reduceDelay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDelayT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     feFinish iterIn work{fDelayT = max minDeltaT remaining}
>
> decayTooLong iterIn
>   | traceNot trace_DARTL False            = undefined
>   | remaining < 0                        = feContinue iterIn work dahTooLong
>   | otherwise                            = feFinish iterIn work{fDecayT = max minDeltaT remaining}
>   where
>     fName                                = "decayTooLong"
>     trace_DARTL                          = unwords[fName, show (remaining, work)]
>
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
>                mSustain mTriple
>   | traceNot trace_DE False               = undefined
>   | otherwise                            = if useEnvelopes && doUse mTriple
>                                              then Just env
>                                              else Nothing
>   where
>     fName                                = "deriveEnvelope"
>     trace_DE                             =
>       if useEnvelopes
>         then unwords [fName, if isJust mTriple then "modEnv" else "volEnv", show env]
>         else unwords [fName, "(none)"]
>
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
>     makeModTriple                        = maybe Nothing (\(mi0, mi1) → Just $ deriveModTriple mi0 mi1 Nothing) 
>
> maybeVetAsDiscreteSig  :: FEnvelope → Segments → Bool
> maybeVetAsDiscreteSig env segs           =
>   case rateForVetEnvelope of
>     Nothing                              → True
>     Just clockRate                       → isJust $ vetAsDiscreteSig clockRate env segs
>
> vetAsDiscreteSig       :: Double → FEnvelope → Segments → Maybe (DiscreteSig Double)
> vetAsDiscreteSig clockRate env segs
>   | traceNot trace_VADS False            = undefined
>   | sum prologlist > epsilon             = error $ unwords [fName, "non-zero prolog", show prologlist]
>   | sum epiloglist > epsilon             = error $ unwords [fName, "non-zero epilog", show epiloglist]
>   | isNothing env.fModTriple && dipix < (kSig `div` 5)
>                                          =
>     error $ unwords [fName, "under", show dipThresh, "at", show dipix, "of", show (kSig, kVec)]
>   | otherwise                            = Just dsig
>   where
>     fName                                = "vetAsDiscreteSig"
>     trace_VADS                           =
>       unwords [fName, show clockRate, show numSamples, show (kSig, kVec)
>              , show (prologlist, epiloglist)]
>
>     dsig@DiscreteSig{ .. }
>                                          = discretizeEnvelope clockRate env segs
>     (targetT, _, _)                      = deJust fName env.fTargetT
>
>     checkSize                            = truncate $ minDeltaT * clockRate
>     dipThresh          :: Double         = 1/10
>
>     kVec, kSig, kSig'  :: Int
>     kVec                                 = VU.length dsigVec
>     kSig                                 = truncate $ clockRate * targetT
>     kSig'                                = truncate $ clockRate * min targetT 0.5
>
>     prologlist, epiloglist
>                        :: [Double]
>     prologlist                           = VU.toList $ VU.force $ VU.slice 0                  checkSize dsigVec
>     epiloglist                           = VU.toList $ VU.force $ VU.slice (kSig - checkSize) checkSize dsigVec
>
>     numSamples                           = targetT * clockRate
>
>     skipSize                             = round $ (env.fDelayT + env.fAttackT) * clockRate
>     afterAttack                          = VU.slice skipSize (kSig - skipSize) dsigVec
>     dipix                                = skipSize + fromMaybe kSig' (VU.findIndex (< dipThresh) afterAttack)
>       
> vetEnvelope            :: FEnvelope → Segments → Bool
> vetEnvelope env segs
>   | traceNot trace_VE False              = undefined
>   | badAmp || badDeltaT                  = error $ unwords [fName, "negative amp or deltaT", show segs]
>   | abs (a - b) > 0.01                   = error $ unwords [fName, "doesn't add up #1", show (a, b, c)]
>   | abs (b - c) > 0.01                   = error $ unwords [fName, "doesn't add up #2", show (a, b, c)]
>   | otherwise                            = maybeVetAsDiscreteSig env segs
>   where
>     fName                                = "vetEnvelope"
>     trace_VE                             =
>       unwords [fName, show (a,b, c)]
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
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
>          deriveEnvelope
>        , doEnvelope
>        , minDeltaT
>        , minUseful
>        , TimeFrame(..)) where
>
> import Data.Foldable ( Foldable(foldl'), find )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, isNothing, fromJust, fromMaybe )
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg )
> import Euterpea.IO.Audio.Types ( AudSF, Clock(..), Signal)
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
>     makeSF env                           =
>       let
>         segs                             = computeSegments timeFrame env
>       in envLineSeg segs.sAmps segs.sDeltaTs
>
> computeSegments        :: TimeFrame → FEnvelope → Segments
> computeSegments tf envRaw
>   | traceIf trace_CS False               = undefined
>   | otherwise                            =
>   if vetEnvelope r segs
>     then segs
>     else error $ unwords[fName, "bad envelope"] 
>   where
>     fName                                = "computeSegments"
>     trace_CS                             = unwords [fName, show (feSum r, amps, deltaTs)]
>
>     r                                    = refineEnvelope envRaw{fTargetT = tf.tfSecsScored}
>
>     amps, deltaTs      :: [Double]
>     amps                                 =
>             [0,            0,             1,          1,       fSusLevel,     fSusLevel,      0,       0]
>     deltaTs                              =
>             [   r.fDelayT,    r.fAttackT,    r.fHoldT,    r.fDecayT,   r.fSustainT,   r.fReleaseT,  1]
>
>     segs                                 = Segments amps deltaTs
>
>     fSusLevel                            = clip (0, 1) r.fSustainLevel

discern design intent governing input Generator values, then implement something ======================================

> data FIterate                            =
>   FIterate {
>     fiFun              :: FIterate → FIterate
>   , fiEnvOrig          :: FEnvelope
>   , fiEnvWork          :: FEnvelope
>   , fiDone             :: Bool}
> data FCase                               =
>   FCase {
>     fcDAH              :: Bool
>   , fcDecay            :: Bool
>   , fcRelease          :: Bool} deriving (Eq, Ord, Show)
> evaluateCase           :: FEnvelope → FCase
> evaluateCase FEnvelope{ .. } 
>                                          =
>   FCase
>     ((fDelayT + fAttackT + fHoldT) >= (9/10) * fTargetT)
>     (fDecayT >= (7/10) * fTargetT)
>     (fReleaseT >= (7/10) * fTargetT)
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
>           (FCase False False False, faceValue)
>         , (FCase False False True, releaseTooLong)
>         , (FCase False True False, decayTooLong)
>         , (FCase False True True, decayTooLong)
>
>         , (FCase True False False, dahTooLong)
>         , (FCase True False True, dahTooLong)
>         , (FCase True True False, dahTooLong)
>         , (FCase True True True,  dahTooLong)]
>
>     fiInit                               =
>       FIterate (caseActions Map.! evaluateCase fEnvIn) fEnvIn fEnvIn False
> 
> feFinish               :: FIterate → FEnvelope → FIterate
> feFinish iterIn workee                   = iterIn{fiEnvWork = workee, fiDone = True}
>
> feContinue             :: FIterate → FEnvelope → (FIterate → FIterate) → FIterate
> feContinue iterIn workee fun             = iterIn{fiFun = fun, fiEnvWork = workee}
>
> faceValue, dahTooLong, releaseTooLong, decayTooLong
>                        :: FIterate → FIterate
> reduceRelease, reduceSustain, reduceDecay, reduceHold, reduceAttack, reduceDelay
>                        :: FIterate → FIterate
>
> faceValue iterIn
>   | traceIf trace_FV False               = undefined
>   | remaining < minDeltaT                = feContinue iterIn work reduceRelease
>   | otherwise                            = feFinish iterIn work{fSustainT = max minDeltaT remaining}
>   where
>     fName                                = "faceValue"
>     trace_FV                             = unwords[fName, show (remaining, work)]
>
>     work                                 = iterIn.fiEnvWork
>     remaining                            = work.fTargetT - feSum work
>
> dahTooLong iterIn                        = feContinue iterIn work reduceHold
>   where
>     work                                 =
>       iterIn.fiEnvWork{fDecayT = minDeltaT, fSustainT = minDeltaT, fReleaseT = minDeltaT}
>
> reduceRelease iterIn                     =
>   let
>     work                                 = iterIn.fiEnvWork{fReleaseT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceSustain
>       else feFinish iterIn work{fReleaseT = max minDeltaT remaining}
>
> reduceSustain iterIn                     =
>   let
>     work                                 = iterIn.fiEnvWork{fSustainT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceDecay
>       else feFinish iterIn work{fSustainT = max minDeltaT remaining}
>
> reduceDecay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDecayT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceHold
>       else feFinish iterIn work{fDecayT = max minDeltaT remaining}
>
> reduceHold iterIn                        = 
>   let
>     work                                 = iterIn.fiEnvWork{fHoldT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceAttack
>       else feFinish iterIn work{fHoldT = max minDeltaT remaining}
>
> reduceAttack iterIn                      =
>   let
>     work                                 = iterIn.fiEnvWork{fAttackT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceDelay
>       else feFinish iterIn work{fAttackT = max minDeltaT remaining}
>
> reduceDelay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDelayT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>   in
>     feFinish iterIn work{fDelayT = max minDeltaT remaining}
>
> releaseTooLong iterIn
>   | traceIf trace_RTL False              = undefined
>   | remaining < minDeltaT                = feContinue iterIn work reduceSustain
>   | otherwise                            = feFinish iterIn work{fSustainT = remaining - offset, fReleaseT = offset}
>   where
>     fName                                = "releaseTooLong"
>     trace_RTL                            = unwords[fName, show (remaining, work)]
>
>     work                                 = iterIn.fiEnvWork{fReleaseT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>     bigrel                               = iterIn.fiEnvOrig.fReleaseT
>
>     offset                               = if remaining - bigrel > minDeltaT
>                                              then bigrel
>                                              else minDeltaT
>
> decayTooLong iterIn
>   | traceIf trace_DARTL False            = undefined
>   | remaining < 0                        = error $ unwords [fName, "illegal envelope"]
>   | otherwise                            = feFinish iterIn work{fDecayT = decay, fReleaseT = release}
>   where
>     fName                                = "decayTooLong"
>     trace_DARTL                          = unwords[fName, show (remaining, work)]
>
>     work                                 = iterIn.fiEnvWork{fDecayT = minDeltaT, fReleaseT = minDeltaT}
>     remaining                            = work.fTargetT - feSum work
>
>     bigdecay                             = 5 * iterIn.fiEnvOrig.fDecayT
>     bigrel                               = iterIn.fiEnvOrig.fReleaseT
>     bigboth                              = bigdecay + bigrel
>
>     decay                                = remaining * bigdecay / bigboth
>     release                              = remaining * bigrel / bigboth
>
> deriveEnvelope         :: Maybe Int
>                           → Maybe Int
>                           → NoteOn
>                           → (Maybe Int, Maybe Int)
>                           → (Maybe Int, Maybe Int)
>                           → Maybe Int
>                           → Maybe Int
>                           → Maybe (Maybe Int, Maybe Int)
>                           → Maybe FEnvelope
> deriveEnvelope mDelay mAttack noon (mHold, mHoldByKey) (mDecay, mDecayByKey)
>                mSustain mRelease mTriple
>   | traceIf trace_DE False               = undefined
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
>         0
>         (fromTithe mSustain (isNothing mTriple))
>         (makeModTriple mTriple)
>         (fromTimecents mDelay)
>         (fromTimecents mAttack)
>         dHold
>         dDecay
>         minDeltaT
>         (fromTimecents mRelease)
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
> vetEnvelope            :: FEnvelope → Segments → Bool
> vetEnvelope env@FEnvelope{ .. } segs
>   | traceIf trace_GE False               = undefined
>   | isJust mnAmp                         = error $ unwords [fName, "negative amp",      show $ fromJust mnAmp]
>   | isJust mnDeltaT                      = error $ unwords [fName, "negative delta T",  show $ fromJust mnDeltaT]
>   | abs (a - b) > 0.01                   = error $ unwords [fName, "doesn't add up #1", show (a, b, c)]
>   | abs (b - c) > 0.01                   = error $ unwords [fName, "doesn't add up #2", show (a, b, c)]
>   | abs prolog > epsilon                 = error $ unwords [fName, "non-zero prolog"]
>   | abs epilog > epsilon                 = error $ unwords [fName, "non-zero epilog"]
>   | isNothing fModTriple && dipix < (k `div` 2)
>                                          = error $ unwords [fName, "under", show dipThresh, "at", show dipix, "of", show k]
>   | otherwise                            = True
>   where
>     fName                                = "vetEnvelope"
>     trace_GE                             = unwords [dsigTag, show dsigStats]
>
>     esignal            :: AudSF () Double
>     esignal                              = envLineSeg segs.sAmps segs.sDeltaTs 
>     DiscreteSig{ .. }
>                                          = fromJust $ fromContinuousSig fName fTargetT esignal
>
>     mnAmp, mnDeltaT    :: Maybe Double
>     mnAmp                                = find (< 0) segs.sAmps
>     mnDeltaT                             = find (< 0) segs.sDeltaTs
>
>     (a, b, c)                            = (feSum env, fTargetT, foldl' (+) (-1) segs.sDeltaTs)
>
>     biteSize                             = 16
>     dipThresh                            = 1/10
>
>     k                                    = VU.length dsigVec
>     prolog                               = 
>       VU.foldl (+) 0 (VU.slice 0              biteSize dsigVec) / fromIntegral biteSize
>     epilog                               =
>       VU.foldl (+) 0 (VU.slice (k - biteSize) biteSize dsigVec) / fromIntegral biteSize
>
>     slotsPerSec                          = fromIntegral k / fTargetT
>     skipSize                             = round $ (fDelayT + fAttackT) * slotsPerSec
>     afterAttack                          = VU.slice skipSize (k - skipSize) dsigVec
>     dipix                                = skipSize + fromMaybe k (VU.findIndex (< dipThresh) afterAttack)
>
> minDeltaT, minUseful   :: Double
> minDeltaT                                = fromTimecents Nothing
> minUseful                                = 1/20
>
> data Segments =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show
>
> useEnvelopes           :: Bool
> useEnvelopes                             = True

The End
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
>          computeSegments
>        , defaultEnvelope
>        , deriveEnvelope
>        , doEnvelope
>        , minDeltaT
>        , minUseful
>        , Segments(..)
>        , TimeFrame(..)
>        , vetEnvelope) where
>
> import Data.Foldable ( Foldable(foldl'), find )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, isNothing, fromJust, fromMaybe )
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg )
> import Euterpea.IO.Audio.Types ( AudRate, AudSF, Clock(..), CtrRate, CtrSF, Signal)
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
>         (_, segs)                       = computeSegments timeFrame env
>       in
>         envLineSeg segs.sAmps segs.sDeltaTs
>
> computeSegments        :: TimeFrame → FEnvelope → (FEnvelope, Segments)
> computeSegments tf envRaw
>   | traceIf trace_CS False               = undefined
>   | otherwise                            =
>   if vetEnvelope r segs
>     then (r, segs)
>     else error $ unwords [fName, "unexpected"]
>   where
>     fName                                = "computeSegments"
>     trace_CS                             = unwords [fName, show (tf.tfSecsScored, amps, deltaTs)]
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
>   | traceIf trace_FV False               = undefined
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
>       else feFinish iterIn work{fSustainT = max minDeltaT remaining}
>
> reduceDecay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDecayT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceHold
>       else feFinish iterIn work{fDecayT = max minDeltaT remaining}
>
> reduceHold iterIn                        = 
>   let
>     work                                 = iterIn.fiEnvWork{fHoldT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceAttack
>       else feFinish iterIn work{fHoldT = max minDeltaT remaining}
>
> reduceAttack iterIn                      =
>   let
>     work                                 = iterIn.fiEnvWork{fAttackT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     if remaining < minDeltaT
>       then feContinue iterIn work reduceDelay
>       else feFinish iterIn work{fAttackT = max minDeltaT remaining}
>
> reduceDelay iterIn                       =
>   let
>     work                                 = iterIn.fiEnvWork{fDelayT = minDeltaT}
>     remaining                            = feRemaining work
>   in
>     feFinish iterIn work{fDelayT = max minDeltaT remaining}
>
> decayTooLong iterIn
>   | traceIf trace_DARTL False            = undefined
>   | remaining < 0                        = error $ unwords [fName, "illegal envelope"]
>   | otherwise                            = feFinish iterIn work{fDecayT = remaining}
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
> maybeCheckDiscreteSig  :: FEnvelope → Segments → Bool
> maybeCheckDiscreteSig env segs           =
>   case rateForVetEnvelope of
>     Nothing                              → True
>     Just clockRate                       → checkDiscreteSig clockRate env segs
>
> checkDiscreteSig       :: Double → FEnvelope → Segments → Bool
> checkDiscreteSig clockRate env segs
>   | traceIf trace_CDS False              = undefined
>   | abs prolog > epsilon                 = error $ unwords [fName, "non-zero prolog"  , show prologlist]
>   | abs epilog > epsilon                 = error $ unwords [fName, "non-zero epilog"  , show epiloglist]
>   | isNothing env.fModTriple && dipix < (kSig `div` 5)
>                                          =
>     error $ unwords [fName, "under", show dipThresh, "at", show dipix, "of", show (kSig, kVec)]
>   | otherwise                            = True
>   where
>     fName                                = "checkDiscreteSig"
>     trace_CDS                            =
>       unwords [fName, show numSamples, show (prologlist, midloglist, epiloglist), show dsigStats, show (kSig, kVec)
>              , show (postList1, postList2)]
>
>     asignal            :: AudSF () Double
>     asignal                              = envLineSeg segs.sAmps segs.sDeltaTs 
>     csignal            :: CtrSF () Double
>     csignal                              = envLineSeg segs.sAmps segs.sDeltaTs 
>     
>     (targetT, _, _)                      = deJust fName env.fTargetT
>
>     DiscreteSig{ .. }
>       | abs clockRate - rate (undefined :: AudRate) < epsilon
>                                          = fromJust $ fromContinuousSig fName (targetT + minUseful) csignal
>       | abs clockRate - rate (undefined :: CtrRate) < epsilon
>                                          = fromJust $ fromContinuousSig fName (targetT + minUseful) asignal
>       | otherwise                        = error $ unwords [fName, "clockRate not supported"]
>
>     checkSize                            = truncate $ minDeltaT * clockRate / 2
>     showSize                             = 2 * checkSize
>     dipThresh          :: Double         = 1/10
>
>     kVec, kSig         :: Int
>     kVec                                 = VU.length dsigVec
>     kSig                                 = truncate $ clockRate * targetT
>
>     prologlist, midloglist, epiloglist
>                        :: [Double]
>     prologlist                           = VU.toList $ VU.slice 0                  checkSize dsigVec
>     midloglist                           = VU.toList $ VU.slice (kSig `div` 2)     showSize dsigVec
>     epiloglist                           = VU.toList $ VU.slice (kSig - checkSize) checkSize dsigVec
>
>     postList1                            = VU.toList $ VU.slice kSig               checkSize dsigVec
>     postList2                            = VU.toList $ VU.slice (kSig + checkSize) checkSize dsigVec
>
>     prolog                               = sum (map abs prologlist)
>     epilog                               = sum (map abs epiloglist)
>
>     numSamples                           = targetT * clockRate
>
>     skipSize                             = round $ (env.fDelayT + env.fAttackT) * clockRate
>     afterAttack                          = VU.slice skipSize (kSig - skipSize) dsigVec
>     dipix                                = skipSize + fromMaybe kSig (VU.findIndex (< dipThresh) afterAttack)
>       
> vetEnvelope            :: FEnvelope → Segments → Bool
> vetEnvelope env segs
>   | traceIf trace_VE False               = undefined
>   | badAmp || badDeltaT                  = error $ unwords [fName, "negative amp or deltaT", show segs]
>   | abs (a - b) > 0.01                   = error $ unwords [fName, "doesn't add up #1", show (a, b, c)]
>   | abs (b - c) > 0.01                   = error $ unwords [fName, "doesn't add up #2", show (a, b, c)]
>   | otherwise                            = maybeCheckDiscreteSig env segs
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
> minDeltaT, minUseful   :: Double
> minDeltaT                                = fromTimecents Nothing
> minUseful                                = 1/82
>
> defaultEnvelope       :: FEnvelope
> defaultEnvelope                          = 
>   FEnvelope
>     Nothing
>     1.0
>     Nothing
>     minDeltaT
>     minDeltaT
>     minDeltaT
>     minDeltaT
>     minDeltaT
>
> data Segments =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show
>
> useEnvelopes           :: Bool
> useEnvelopes                             = True
>
> rateForVetEnvelope     :: Maybe Double
> rateForVetEnvelope                 = Just $ rate (undefined :: CtrRate)

The End
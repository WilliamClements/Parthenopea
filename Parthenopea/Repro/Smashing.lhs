> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Smashing
William Clements
February 10, 2025

> module Parthenopea.Repro.Smashing
>        (  allCellsEqualTo
>         , computeSmashup
>         , fractionCovered
>         , fractionEmpty
>         , lookupCellIndex
>         , sLength
>         , Smashing(..)
>         , SmashStats(..)
>         , smashSubspaces
>         ) where
>
> import qualified Data.Bifunctor          as BF
> import Data.Ix
> import Data.List ( foldl' )
> import Data.Maybe
> import Data.Ratio ( (%) )
> import qualified Data.Vector.Unboxed     as VU
> import Parthenopea.Debug
> import Parthenopea.Repro.Modulation

Range theory ==========================================================================================================

Model rectilinear sub-space coverage; e.g. find unwanted (sub-)space overlaps. Each space (of nspaces) contains
exactly ndims (2 in the MIDI case) ranges. If dim is the value of a dimension then its overall range is implicitly
0..dim-1 -- the associated _specified_ space range carves out a subset thereof.

Say you have ndims=2 dimensions each of 64 extent. (Partially) covering overall 64x64 space are nspaces=3 "zones". 

Zone 1: 32..57 "pitch", 11..47 "velocity"
Zone 2: 21..40        , 20..21
Zone 3: 0..1          , 0..1

You see there is some overlap between Zone 1 and Zone 2.

> smashSubspaces         :: ∀ i . (Integral i, Ix i, Num i, Show i, VU.Unbox i) ⇒
>                           String → [i] → [(i, [Maybe (i, i)])] → Smashing i
> smashSubspaces tag dims spaces_
>   | traceNot trace_SS False              = undefined
>   | otherwise                            = Smashing tag dims spaces (developSmashStats svector) svector
>   where
>     spaces             :: [(i, [(i, i)])]
>     spaces                               = map (BF.second (zipWith (\dim → fromMaybe (0, dim-1)) dims)) spaces_
>
>     mag                :: Int            = fromIntegral $ product dims
>
>     svector            :: VU.Vector (i, i)
>     svector                              = foldl' sfolder (VU.replicate mag (0, 0)) spaces
>
>     sfolder            :: VU.Vector (i, i) → (i, [(i, i)]) → VU.Vector (i, i)
>     sfolder smashup (spaceId, rngs)      = VU.accum assignCell smashup (enumAssocs dims spaceId rngs)
>
>     assignCell         :: (i, i) → (i, i) → (i, i)
>     assignCell mfrom mto                 = (fst mto, snd mfrom + 1)
>
>     enumAssocs         ::  [i] → i → [(i, i)] → [(Int, (i, i))]
>     enumAssocs dimsA spaceId rngs        =
>       profess
>         (0 <= mag && mag <= 65_536 && all (uncurry validRange) (zip dimsA rngs))
>         (unwords ["enumAssocs: range violation", tag, show mag, show dimsA, show spaces])
>         (map (, (spaceId, 1)) is)
>       where
>         is             :: [Int]
>         is                               =
>           map (fromIntegral . computeCellIndex dimsA) (traverse walkRange rngs)
>
>     trace_SS                             = unwords ["smashSubspaces", show (length spaces_), show spaces_]
>
> validRange             :: ∀ i . (Integral i, Ix i) ⇒ i → (i, i) → Bool
> validRange dim (r, s)                    = 0 <= dim && r <= s && inZRange r dim && inZRange s dim
>
> validCoords            :: ∀ i . (Integral i, Ix i, VU.Unbox i) ⇒ [i] → Smashing i → Bool
> validCoords coords smashup               = and $ zipWith inZRange coords smashup.smashDims
>
> lookupCellIndex        :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒ [i] → Smashing i → (i, i)
> lookupCellIndex coords smashup           = try
>   where
>     try_                                 =
>       profess
>         (validCoords coords smashup)
>         (unwords ["lookupCellIndex", "invalid coords"])
>         (smashup.smashVec VU.! computeCellIndex smashup.smashDims coords)
>     try                                  =
>       if snd try_ > 0
>         then try_
>         else (snd $ minimum (map (measure coords) smashup.smashSpaces), 1)
>
>     measure            :: [i] → (i, [(i, i)]) → (Double, i)
>     measure coordsM space                =
>       minimum (map (distance (fst space) coordsM) (listOutPoints (snd space)))
>
>     distance           :: i → [i] → [i] → (Double, i)
>     distance bix [] []                   = (0, bix)
>     distance bix (x:xs) (y:ys)           = (var + fst (distance bix xs ys), bix)
>       where
>         delta, var     :: Double
>         delta                            = fromIntegral (x - y)
>         var                              = delta * delta
>     distance _ _ _                       =
>       error $ unwords ["distance:", "input coords args have unequal lengths"]
>
> listOutPoints          :: ∀ i . (Integral i) ⇒ [(i, i)] → [[i]]
> listOutPoints []                         = [[]]
> listOutPoints ((r, s) : ranges)          = points1 ++ points2
>   where
>     points1                              = map ([r] ++) (listOutPoints ranges)
>     points2                              = map ([s] ++) (listOutPoints ranges)
>
> computeCellIndex       :: ∀ i . (Integral i) ⇒ [i] → [i] → Int
> computeCellIndex [] []                   = 0
> computeCellIndex (_:dims) (rng:rngs)       = fromIntegral (rng * product dims) + computeCellIndex dims rngs
> computeCellIndex _ _                     =
>   error $ unwords ["computeCellIndex:", "input args dims and coords have unequal lengths"]
>
> allCellsEqualTo        :: ∀ i . (Integral i, Show i, VU.Unbox i) ⇒ Smashing i → Maybe (i, i)
> allCellsEqualTo smashup                  =
>   let
>     cand                                 = smashup.smashVec VU.! 0
>   in
>     if all (\j → cand == (smashup.smashVec VU.! j)) [0..(VU.length smashup.smashVec - 1)]
>       then Just cand
>       else Nothing
>
> data Smashing i                          =
>   Smashing {
>     smashTag            :: String
>     , smashDims         :: [i]
>     , smashSpaces       :: [(i, [(i, i)])]
>     , smashStats        :: SmashStats
>     , smashVec          :: VU.Vector (i, i)}
> instance ∀ i. (Integral i, Num i, Show i) ⇒ Show (Smashing i) where
>   show Smashing{ .. }                    =
>     unwords ["Smashing", show (smashTag, smashStats)]
> sLength                :: ∀ i. (Integral i) ⇒ Smashing i → i
> sLength smashup                        = product smashup.smashDims
> data SmashStats                        =
>   SmashStats {
>     countNothings      :: Int
>   , countSingles       :: Int
>   , countMultiples     :: Int} deriving Show
> seedSmashStats         :: SmashStats
> seedSmashStats                           = SmashStats 0 0 0
>
> developSmashStats      :: ∀ i. (Integral i, Show i, VU.Unbox i) ⇒ VU.Vector (i,i) → SmashStats
> developSmashStats                        = VU.foldl' sfolder seedSmashStats
>   where
>     sfolder            :: SmashStats → (i, i) → SmashStats
>     sfolder stats@SmashStats{ .. } (_, count)
>       | count == 0                       = stats{countNothings = countNothings + 1}
>       | count == 1                       = stats{countSingles = countSingles + 1}
>       | otherwise                        = stats{countMultiples = countMultiples + 1}
> fractionEmpty, fractionCovered
>                        :: ∀ i. (Integral i, Show i) ⇒ Smashing i → Rational
> fractionEmpty smashup                    = fromIntegral (countNothings smashup.smashStats) % fromIntegral (sLength smashup)
> fractionCovered smashup                  =
>   fromIntegral (countSingles smashup.smashStats + countMultiples smashup.smashStats) % fromIntegral (sLength smashup)
>
> inZRange               :: (Ix a, Num a) ⇒ a → a → Bool
> inZRange x y                             = inRange (0, y - 1) x 
>
> computeSmashup         :: String → [(Word, [Maybe (Word, Word)])] → Smashing Word
> computeSmashup tag                       = smashSubspaces tag dims
>   where
>     dims                                 = [fromIntegral qMidiSize128, fromIntegral qMidiSize128]
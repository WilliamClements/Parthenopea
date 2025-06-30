> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Smashing
William Clements
February 10, 2025

> module Parthenopea.Repro.Smashing where
>
> import qualified Data.Bifunctor          as BF
> import Data.Ix
> import Data.List
> import Data.Maybe
> import qualified Data.Vector.Unboxed     as VU
> import Parthenopea.Debug
  
Range theory ==========================================================================================================

"Write once, read many" characterizes a cache. A "smashing" caches values that identify the winner of a game. Later 
executed code will require those identities fast. There is no upper limit on the number of times these queries will be
issued.

When you have an Instrument and a NoteOn, the "game" pinpoints which Zone to use to play the note. The smashing for
this purposes is 128 x 128 x 2 covering all possible NoteOns, and Left/Right.

The winning Zone(s) drive the particular note synthesis. We use a flat vector for the cache and index it by the three
component values.

This source file implements a generalization of that setup. See computeInstSmashup. We must know all the "subspaces"
(range specifications) up front which we "smash" together to populate the flat vector.
          
> data Smashing i                          =
>   Smashing {
>     smashTag            :: String
>     , smashDims         :: [i]
>     , smashSpaces       :: [(i, [(i, i)])]
>     , smashStats        :: SmashStats
>     , smashVec          :: VU.Vector (i, i)}
> instance ∀ i. Show i ⇒ Show (Smashing i) where
>   show Smashing{smashTag, smashStats}    = unwords ["Smashing", show (smashTag, smashStats)]
> data SmashStats                          =
>   SmashStats {
>     countNothings      :: Int
>   , countSingles       :: Int
>   , countMultiples     :: Int} deriving (Eq, Show)
> seedSmashStats         :: SmashStats
> seedSmashStats                           = SmashStats 0 0 0
>
> seedSmashing           :: ∀ i. (Integral i, Show i, VU.Unbox i) ⇒ Smashing i
> seedSmashing                             =
>   Smashing "" [] [] seedSmashStats VU.empty
>
> developSmashStats      :: ∀ i. (Integral i, Show i, VU.Unbox i) ⇒ VU.Vector (i,i) → SmashStats
> developSmashStats                        = VU.foldl' sfolder seedSmashStats
>   where
>     sfolder            :: SmashStats → (i, i) → SmashStats
>     sfolder stats@SmashStats{ .. } (_, count)
>       | count == 0                       = stats{countNothings = countNothings + 1}
>       | count == 1                       = stats{countSingles = countSingles + 1}
>       | otherwise                        = stats{countMultiples = countMultiples + 1}

Each space (of nspaces) contains exactly ndims (3 in the Midi case) ranges. If dim is the value of a dimension then
the associated range is implicitly 0..dim-1 -- the specifications each carve out a subset thereof.

Say you have ndims=2 dimensions each of 64 extent. (Partially) covering overall 64x64 space are nspaces=3 "zones". 

Zone 1: 32..57 "pitch", 11..47 "velocity"
Zone 2: 21..40        , 20..21
Zone 3: 0..1          , 0..1

You see there is some overlap between Zone 1 and Zone 2.

> smashSubspaces         :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒
>                           String → [i] → [(i, [Maybe (i, i)])] → Smashing i
> smashSubspaces tag dims spaces_          =
>   Smashing tag dims spaces (developSmashStats svector) svector
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
>     sfolder vec (spaceId, rngs)          =
>       let
>         enumAssocs     :: [(Int, (i, i))]
>         enumAssocs
>           | traceNot trace_EA False      = undefined
>           | otherwise                    =
>             profess
>               (0 <= mag && mag <= 32_768 && all (uncurry validRange) (zip dims rngs))
>               (unwords ["enumAssocs: range violation", tag, show mag, show dims, show spaces])
>               (map (, (spaceId, 1)) is)
>           where
>             fName                        = "enumAssocs"
>             trace_EA                     = unwords [fName, tag, show spaces]
>
>             is         :: [Int]
>             is                           =
>               map (computeCellIndex dims) (traverse walkRange rngs)
>       in
>          VU.accum smashCell vec enumAssocs

Smashing smashings ====================================================================================================

We smash smashings together in the Midi case to merge multiple Instruments into one. The function smashCell acts as a
zipper to carry out the you-know-what.

> smashSmashings         :: ∀ i . (Integral i, Show i, VU.Unbox i) ⇒
>                           Smashing i → Smashing i → Smashing i
> smashSmashings s1 s2                     =
>   Smashing
>     (s1.smashTag ++ s2.smashTag)
>     dims
>     (s1.smashSpaces ++ s2.smashSpaces)
>     (developSmashStats svector)
>     svector
>   where
>     fName                                = "smashSmashings"
>
>     svector                              = VU.zipWith smashCell s1.smashVec s2.smashVec
>
>     dims                                 =
>       profess
>         (s1.smashDims == s2.smashDims)
>         (unwords [fName, "dims mismatch?!?"])
>         s1.smashDims
>
> smashCell              :: ∀ i . (Integral i) ⇒ (i, i) → (i, i) → (i, i)
> smashCell (spaceId1, cnt1) (spaceId2, cnt2)
>   | cnt2 == 0                            = (spaceId1, cnt1)
>   | otherwise                            = (spaceId2, cnt1 + cnt2)
>
> walkRange              :: Integral n ⇒ (n, n) → [n]
> walkRange (x, y)                         = if x > y || y < 0 then [] else [x..y]
>
> validRange             :: ∀ i . (Integral i, Ix i) ⇒ i → (i, i) → Bool
> validRange dim (r, s)                    = 0 <= dim && r <= s && inZRange r dim && inZRange s dim
>
> validCoords            :: ∀ i . (Integral i, Ix i, VU.Unbox i) ⇒ [i] → Smashing i → Bool
> validCoords coords smashup               = and $ zipWith inZRange coords smashup.smashDims
>
> lookupCellIndex        :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒ [i] → Smashing i → (i, i)
> lookupCellIndex coords smashup           = cell
>   where
>     cell_                                =
>       profess
>         (validCoords coords smashup)
>         (unwords ["lookupCellIndex", "invalid coords"])
>         (smashup.smashVec VU.! computeCellIndex smashup.smashDims coords)
>     cell                                 =
>       if snd cell_ > 0
>         then cell_
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
> computeCellIndex (_:dims) (rng:rngs)     = fromIntegral (rng * product dims) + computeCellIndex dims rngs
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
> inZRange               :: (Ix a, Num a) ⇒ a → a → Bool
> inZRange x y                             = inRange (0, y - 1) x 

The End
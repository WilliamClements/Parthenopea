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
> import Data.IntMap.Strict (IntMap)
> import qualified Data.IntMap.Strict as IntMap
> import Data.Ix
> import Data.Maybe
> import qualified Data.Vector.Unboxed     as VU
> import Parthenopea.Debug
> import Parthenopea.SoundFont.Utility

Range theory ==========================================================================================================

"Write once, read many" characterizes a cache. A "smashing" caches values that identify the winner of a game. Later 
executed code will require those identities fast. There is no upper limit on the number of times these queries will be
issued.

When you have an Instrument and a NoteOn, the "game" pinpoints which Zone to use to play the note. The smashing for
this purpose is 128 x 128 x 2 covering all possible NoteOns, and Left/Right (channels). We use a flat vector for the
cache and custom-index it by the three coordinates.

The winning Zone(s) drive the note synthesis. 

This source file implements a generalization of above definition. See computeInstSmashup. We must know all the
"subspaces" (range specifications) up front which we "smash" together to populate the flat vector.
          
> data Smashing i                          =
>   Smashing {
>     smashTag            :: String
>     , smashDims         :: [i]
>     , smashSpaces       :: IntMap [(i, i)]
>     , smashStats        :: SmashStats
>     , smashVec          :: VU.Vector (i, i)}
> instance ∀ i . Show i ⇒ Show (Smashing i) where
>   show Smashing{smashTag, smashStats}    = unwords ["Smashing", show (smashTag, smashStats)]
> data SmashStats                          =
>   SmashStats {
>     countNothings      :: Int
>   , countSingles       :: Int
>   , countMultiples     :: Int}
>   deriving (Eq, Show)
> seedSmashStats         :: SmashStats
> seedSmashStats                           = SmashStats 0 0 0
>
> developSmashStats      :: ∀ i. (Integral i, Show i, VU.Unbox i) ⇒
>                           VU.Vector (i,i) → SmashStats
> developSmashStats                        = VU.foldl' sfolder seedSmashStats
>   where
>     sfolder            :: SmashStats → (i, i) → SmashStats
>     sfolder stats (_, count)
>       | count == 0                       = stats{countNothings  = stats.countNothings + 1}
>       | count == 1                       = stats{countSingles   = stats.countSingles + 1}
>       | otherwise                        = stats{countMultiples = stats.countMultiples + 1}

Each space (of nspaces) contains exactly ndims (3 in the Midi case) ranges. If dim is the value of a dimension then
the associated range is implicitly 0..dim-1

Say you have ndims=2 dimensions each of 64 extent. (Partially) covering overall 64x64 space are nspaces=3 "zones". 

Zone 1: 32..57 "pitch", 11..47 "velocity"
Zone 2: 21..40        , 20..21
Zone 3: 0..1          , 0..1

You see there is some overlap between Zone 1 and Zone 2.

> smashSubspaces         :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒
>                           String → [i] → IntMap [Maybe (i, i)] → Smashing i
> smashSubspaces tag dims spaces_          =
>   Smashing 
>     tag 
>     dims
>     spaces
>     (developSmashStats svector)
>     svector
>   where
>     spaces                               = IntMap.map (zipWith expand dims) spaces_
>                                              where expand dim = fromMaybe (0, dim-1)
>     mag                :: Int            = fromIntegral $ product dims
>     svector                              = IntMap.foldlWithKey sfolder (VU.replicate mag (0, 0)) spaces
>     sfolder            :: VU.Vector (i, i) → Int → [(i, i)] → VU.Vector (i, i)
>     sfolder vec spaceId rngs          =
>       let
>         enumAssocs     :: [(Int, (i, i))]
>         enumAssocs                       =
>           profess
>             (0 <= mag && mag <= 32_768 && all (uncurry validRange) (zip dims rngs))
>             (unwords [fName, "range violation", tag, show mag, show dims, show spaces])
>             (map (, (fromIntegral spaceId, 1)) is)
>           where
>             fName                        = "enumAssocs"
>
>             is         :: [Int]
>             is                           =
>               map (computeCellIndex dims) (traverse walkRange rngs)
>
>         walkRange      :: (i, i) → [i]
>         walkRange (x, y)                 = if x > y || y < 0 then [] else [x..y]
>       in
>         VU.accum smashCell vec enumAssocs

Smashing smashings ====================================================================================================

We smash smashings together in the Midi case when we merge multiple Instruments into one. The function smashCell acts
as a zipper to carry out the you-know-what (smashing, stupid!)

> smashSmashings         :: ∀ i . (Integral i, Show i, VU.Unbox i) ⇒ Smashing i → Smashing i → Smashing i
> smashSmashings s1 s2
>   | traceNot trace_SS False              = undefined
>   | otherwise                            =
>   Smashing
>     fName
>     dims
>     (s1.smashSpaces `IntMap.union` s2.smashSpaces)
>     (developSmashStats svector)
>     svector
>   where
>     fName                                = "smashSmashings"
>     trace_SS                             = unwords [fName, show s1, show s2]
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
> validRange             :: ∀ i . (Integral i, Ix i) ⇒ i → (i, i) → Bool
> validRange dim (r, s)                    = 0 <= dim && r <= s && inZRange r dim && inZRange s dim
>
> validCoords            :: ∀ i . (Integral i, Ix i, VU.Unbox i) ⇒ [i] → Smashing i → Bool
> validCoords coords Smashing{ .. }
>                                          = and $ zipWith inZRange coords smashDims

Navigation ============================================================================================================

> getLeafCells           :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒ [i] → Smashing i → VU.Vector (i, i)
> getLeafCells coords smashup@Smashing{ .. }
>   | traceNot trace_GLC False             = undefined
>   | not (validCoords coords smashup)     = error $ unwords [fName, "invalid coords"]
>   | otherwise                            = VU.slice cellix leafDim smashVec
>   where
>     fName                                = "getLeafCells"
>     trace_GLC                            = unwords [fName, show coords, show leafDim, show cellix]
>
>     leafDim                              = (fromIntegral . last) smashDims
>     cellix                               = computeCellIndex smashDims (init coords ++ [0])
>
> lookupCell             :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒ [i] → Smashing i → (i, i)
> lookupCell coords smashup@Smashing{ .. }
>                                          = cell
>   where
>     fName                                = "lookupCell"
>
>     cell
>       | not (validCoords coords smashup) = error $ unwords [fName, "invalid coords"]
>       | null measured                    = error $ unwords [fName, "smashup has no subspaces"]
>       | snd cell_ > 0                    = cell_
>       | otherwise                        = (snd $ minimum measured, 1)
>       where
>         measured                         = IntMap.mapWithKey (measure coords) smashSpaces
>         cell_                            = smashVec VU.! computeCellIndex smashDims coords
>
>     measure            :: [i] → Int → [(i, i)] → (Double, i)   
>     measure coordsM spaceId space        =
>       let
>         listOut                          = listOutPoints space
>       in
>         profess
>           (not $ null listOut)
>           (unwords [fName, "minimum"])
>           (minimum (map (distance (fromIntegral spaceId) coordsM) listOut))
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
> listOutPoints ((r, s) : ranges)          =
>   let
>     listOut                              = listOutPoints ranges
>   in
>     map ([r] ++) listOut ++ map ([s] ++) listOut
>
> computeCellIndex       :: ∀ i . (Integral i) ⇒ [i] → [i] → Int
> computeCellIndex [] []                   = 0
> computeCellIndex (_:dims) (rng:rngs)     = fromIntegral (rng * product dims) + computeCellIndex dims rngs
> computeCellIndex _ _                     = error $ unwords [fName, "input dims and coords have unequal lengths"]
>   where
>     fName                                = "computeCellIndex"
>
> allCellsEqualTo        :: ∀ i . (Integral i, VU.Unbox i) ⇒ Smashing i → Maybe (i, i)
> allCellsEqualTo smashup                  =
>   let
>     cand               :: (i, i)         = smashup.smashVec VU.! 0
>     allix              :: [Int]          = [0..(VU.length smashup.smashVec - 1)]
>     match j                              = cand == (smashup.smashVec VU.! j)
>   in
>     if all match allix
>       then Just cand
>       else Nothing

The End
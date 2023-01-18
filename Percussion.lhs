{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

Playground Examples with Percussion and Randomness
William Clements
Last modified: 27-September-2022

> module Aleatory where
> import Euterpea
> import Euterpea.Music
> import System.Random
> import Debug.Trace
> import Data.List (unfoldr)
> import Control.Monad (replicateM)
> import Control.Monad.ST (runST)
> import Control.Monad.State
> import Fanfare
> import Euterpea.IO.MIDI.MEvent

> piece =
>    removeZeros
>    $ tempo (2/1)
>    $ transpose 5
>    $ keysig C Mixolydian
>    -- $ times 2
>    $ instrument Percussion
>    $ guts
>
>    where
>
>       guts = line [rest hn, c 3 qn, c 4 qn, a 3 qn, a 4 qn]

Oceans ============================================================================

> oceans, seas, water, lake :: Music Pitch    
> oceans =  line $ map nRandom2Note $ normalizedDoubles $ mkStdGen 259
> seas =    line $ map nRandom2Perc $ normalizedDoubles $ mkStdGen 747
> water =   (rest en :+: oceans) :=: seas
>
> normalizedDoubles :: StdGen -> [Double]
> normalizedDoubles g =
>    let (x, g') = uniformR (0,1) g
>    in x : normalizedDoubles g'
>
> nRandom2Note :: Double -> Music Pitch
> nRandom2Note r =
>    note qn $ pitch $ round $ normalized2ranged r pitchRange
>    where
>       pitchRange = (24, 92)
>
>   -- calibrate [0,1] to [lo,up]
> normalized2ranged :: Double -> (Double, Double) -> Double
> normalized2ranged r (lo, up)= lo + r * (up-lo)
>
> nRandom2Perc :: Double -> Music Pitch
> nRandom2Perc r =
>    perc (toEnum $ round $ normalized2ranged r pitchRange) qn
>    where
>       pitchRange = (0, 46)
>
>
>
> ingestRC :: [RandomContext Pitch] -> [Music Pitch]
> ingestRC rcs = map nRandomContext2Music rcs
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -fno-warn-orphans #-}

Playground Examples with Percussion and Randomness
William Clements
Last modified: 27-September-2022

> module Percussion where
>
> import Data.List (unfoldr)
> import Debug.Trace
> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import System.Random

> piece =
>    removeZeros
>    $ tempo 2
>    $ transpose 5
>    $ keysig C Mixolydian
>    $ instrument Percussion guts
>
>    where
>
>       guts = line [rest hn, c 3 qn, c 4 qn, a 3 qn, a 4 qn]

Oceans ============================================================================

> oceans, seas, water :: Music Pitch    
> oceans =  line $ map nRandom2Note $ normalizedDoubles $ mkStdGen 259
> seas =    line $ map nRandom2Perc $ normalizedDoubles $ mkStdGen 747
> water =   (rest en :+: oceans) :=: seas
>
> normalizedDoubles :: StdGen → [Double]
> normalizedDoubles g =
>    let (x, g') = randomR (0,1) g
>    in x : normalizedDoubles g'
>
> nRandom2Note :: Double → Music Pitch
> nRandom2Note r =
>    note qn $ pitch $ round $ normalized2ranged r pitchRange
>    where
>       pitchRange = (24, 92)
>
>   -- calibrate [0,1] to [lo,up]
> normalized2ranged :: Double → (Double, Double) → Double
> normalized2ranged r (lo, up)= lo + r * (up-lo)
>
> nRandom2Perc :: Double → Music Pitch
> nRandom2Perc r =
>    perc (toEnum $ round $ normalized2ranged r pitchRange) qn
>    where
>       pitchRange = (0, 46)
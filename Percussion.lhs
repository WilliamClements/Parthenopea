> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE UnicodeSyntax #-}

Percussion
William Clements
Last modified: 23-Jan-2023

> module Percussion where
>
> import Debug.Trace
> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import System.Random

perc helpers ==========================================================================================================

> percm :: PercussionSound → [Dur] → Music Pitch
> percm p ds = line (map (perc p) ds)
>
> percBDqn =  perc BassDrum1     qn
> percOHHqn = perc OpenHiHat     qn
> percCHHqn = perc ClosedHiHat   qn
> percCCqn  = perc CrashCymbal1  qn
> percRCqn  = perc RideCymbal1   qn
> percSDqn  = perc AcousticSnare qn
> percLTqn =  perc LowTom        qn
> percHTqn =  perc HighTom       qn
>
> percBDen =  perc BassDrum1     en
> percOHHen = perc OpenHiHat     en
> percCHHen = perc ClosedHiHat   en
> percCCen  = perc CrashCymbal1  en
> percRCen  = perc RideCymbal1   en
> percSDen  = perc AcousticSnare en
> percLTen =  perc LowTom        en
> percHTen =  perc HighTom       en
>
> percBDsn =  perc BassDrum1     sn
> percOHHsn = perc OpenHiHat     sn
> percCHHsn = perc ClosedHiHat   sn
> percCCsn  = perc CrashCymbal1  sn
> percRCsn  = perc RideCymbal1   sn
> percSDsn  = perc AcousticSnare sn
> percLTsn =  perc LowTom        sn
> percHTsn =  perc HighTom       sn

Piece =================================================================================================================

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

Oceans ================================================================================================================

> oceans, seas, water    :: Music Pitch    
> oceans                                   = line $ map nRandom2Note $ normalizedDoubles $ mkStdGen 259
> seas                                     = line $ map nRandom2Perc $ normalizedDoubles $ mkStdGen 747
> water                                    = (rest en :+: oceans) :=: seas
>
> normalizedDoubles      :: StdGen → [Double]
> normalizedDoubles g                      = x : normalizedDoubles g'
>    where
>      (x, g') = randomR (0,1) g
>
> nRandom2Note           :: Double → Music Pitch
> nRandom2Note r                           = note qn $ pitch $ round $ normalized2ranged r (24, 92)
>
>   -- calibrate [0,1] to [lo,up]
> normalized2ranged      :: Double → (Double, Double) → Double
> normalized2ranged r (lo, up)             = lo + r * (up-lo)
>
> nRandom2Perc           :: Double → Music Pitch
> nRandom2Perc r                           = perc (toEnum $ round $ normalized2ranged r (0, 46)) qn

The End
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE UnicodeSyntax #-}

Percussion
William Clements
23-Jan-2023

> module Parthenopea.Music.Percussion where
>
> import Euterpea.Music
> import Parthenopea.Siren
> import System.Random

perc helpers ==========================================================================================================

> percm :: PercussionSound → [Dur] → Music Pitch
> percm p durs = line (map (perc p) durs)
>
> percBDqn, percOHHqn, percCHHqn, percCCqn, percRCqn, percSDqn, percLTqn, percHTqn
>                        :: Music Pitch
> percBDqn =  perc BassDrum1     qn
> percOHHqn = perc OpenHiHat     qn
> percCHHqn = perc ClosedHiHat   qn
> percCCqn  = perc CrashCymbal1  qn
> percRCqn  = perc RideCymbal1   qn
> percSDqn  = perc AcousticSnare qn
> percLTqn =  perc LowTom        qn
> percHTqn =  perc HighTom       qn
>
> percBDen, percOHHen, percCHHen, percCCen, percRCen, percSDen, percLTen, percHTen
>                        :: Music Pitch
> percBDen =  perc BassDrum1     en
> percOHHen = perc OpenHiHat     en
> percCHHen = perc ClosedHiHat   en
> percCCen  = perc CrashCymbal1  en
> percRCen  = perc RideCymbal1   en
> percSDen  = perc AcousticSnare en
> percLTen =  perc LowTom        en
> percHTen =  perc HighTom       en
>
> percBDsn, percOHHsn, percCHHsn, percCCsn, percRCsn, percSDsn, percLTsn, percHTsn
>                        :: Music Pitch
> percBDsn =  perc BassDrum1     sn
> percOHHsn = perc OpenHiHat     sn
> percCHHsn = perc ClosedHiHat   sn
> percCCsn  = perc CrashCymbal1  sn
> percRCsn  = perc RideCymbal1   sn
> percSDsn  = perc AcousticSnare sn
> percLTsn =  perc LowTom        sn
> percHTsn =  perc HighTom       sn

play water ============================================================================================================

> oceans, seas, water    :: Music Pitch    
> oceans                                   = line $ map nRandom2Note $ randomNorms $ mkStdGen 259
> seas                                     = line $ map nRandom2Perc $ randomNorms $ mkStdGen 747
> water                                    = (rest en :+: oceans) :=: seas
>
> nRandom2Note           :: Double → Music Pitch
> nRandom2Note r                           = note qn $ pitch $ round $ denorm r (24, 92)
>
> nRandom2Perc           :: Double → Music Pitch
> nRandom2Perc r                           = perc (toEnum $ round $ denorm r (0, 46)) qn

The End
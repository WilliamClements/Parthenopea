Simple examples with Euterpea's Music data structures
William Clements
Last modified: 27-September-2022

> module Simple where
> import Euterpea
> import System.Random
> import Debug.Trace

> bb12Together :: Music Pitch
> bb12Together = tempo 1.75 (instrument Woodblock (drumsPart 79))
>                           :=: (instrument FretlessBass (bassPart (-12)))

> drumsPart :: AbsPitch -> Music Pitch 
> drumsPart x = if stdvol < thresh then doRest dTime else doNote dTime (pitch x)

> bassPart :: AbsPitch -> Music Pitch
> bassPart x = if stdvol < thresh then doRest dTime else doNote dTime (pitch x)

> doRest :: Dur -> Music a
> doRest d =
>    trace ("doRest d=" ++ show d)
>    Prim (Rest d)

> doNote :: Dur -> Pitch -> Music Pitch
> doNote d p =
>    trace ("doNote d=" ++ show d ++ " p=" ++ show p)
>    Prim (Note d p)

> stdvol = 800
> thresh = 20
> dTime = wn
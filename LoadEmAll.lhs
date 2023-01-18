> module LoadEmAll where
>
> import Aleatory
> import Baking
> import Bell
> import Cecil
> import Covers
> import Euterpea
> import EuterpeaExamples
> import Fanfare
> import Interlude
> import MidiWidgets
> import MoreMusic
> import MUI
> import Parthenopea
> import SelfSimilar
> import Signals
> import WindEffect

LoadEmAll =========================================================================

To force inclusion of everything I might want to reference in ghci

> wrapStuff :: Int -> Rational
> wrapStuff n =
>   let toss, rN :: Rational
>       toss = dur theFanfare
>       rN = fromIntegral n
>   in toss - rN


> {-# LANGUAGE UnicodeSyntax #-}
>
> module App.Rosters (
>          ajingles
>          , allJingles
>          , bjingles
>          , cjingles
>          , djingles
>          , ejingles
>          , njingles
>          , zjingles
>          , selectedJingleList) where
>
> import Tunes.Cecil
> import Tunes.Covers
> import qualified Data.Map                as Map
> import Tunes.Fanfare
> import Tunes.Opus131 ( opus131 )
> import Parthenopea.Music.Baking
> import Parthenopea.Music.Siren
> import Parthenopea.SoundFont.Directives
> import Tunes.SunPyg
>
> ajingles, bjingles, cjingles, djingles, ejingles, allJingles
>                        :: [Song]
> njingles, zjingles     :: Directives → [Song]
> ajingles =
>    [ Song    "theFanfare"      (theFanfare False)          Map.empty
>    , Song    "alice"           alice                       Map.empty
>    , Song    "bob"             (bob 4)                     Map.empty
>    , Song    "copper"          (copper 2)                  Map.empty
>    , Song    "gold"            gold                        Map.empty
>    , Song    "silver"          silver                      Map.empty
>    , Song    "deyDumpDum"      (deyDumpDum False)          Map.empty]
> bjingles =
>    [ Song    "getCITM"         getCITM                     Map.empty
>    , Song    "slot"            (slot 4)                    Map.empty
>    , Song    "whelpNarp"       whelpNarp                   Map.empty
>    , Song    "bill"            (bill 4)                    Map.empty
>    , Song    "roger"           roger                       Map.empty]
> cjingles =
>    [ Song    "cecil"           cecil                       Map.empty
>    , Song    "abby"            abby                        Map.empty
>    , Song    "wj"              wj                          Map.empty
>    , Song    "shelby"          shelby                      Map.empty
>    , Song    "weHateHer"       weHateHer                   Map.empty]
> djingles =
>    [ Song    "waypostpurple"   waypostpurple               Map.empty
>    , Song    "snake"           snake                       Map.empty
>    , Song    "pendingtonArnt"  (pendingtonArnt 2)          Map.empty
>    , Song    "ssailor"         ssailor                     Map.empty]
> ejingles =
>    [ Song    "kit"             kit                         Map.empty
>    , Song    "pit"             pit                         Map.empty
>    , Song    "dit"             dit                         Map.empty
>    , Song    "rattan"          rattan                      Map.empty]
> njingles dives                           =
>    [ Song    "opus131"         (opus131 dives)             Map.empty
>    , Song    "bake"            (bakedJingle dives 93)      Map.empty]
> zjingles dives                           =
>    [ Song    "deathlessHorsie" (deathlessHorsie dives)     Map.empty
>    , Song    "basicLick"       (basicLick dives)           Map.empty
>    , Song    "sunPyg"          (sunPyg dives)              Map.empty
>    , Song    "packardGoose"    (packardGoose dives)        Map.empty]
>
> allJingles                               = ajingles ++ bjingles ++ cjingles ++ djingles
>                                         ++ ejingles
>
> selectedJingleList     :: Directives → [Song]
> selectedJingleList _                     = ajingles
>   -- [ Song    "sunPyg"      (sunPyg dives)              Map.empty]
>   -- [ Song    "bake"        (const $ bakedJingle 1003)  Map.empty]
>   -- [ Song    "iboogie"     (iboogie)                   Map.empty]
>   -- [ Song    "theFanfare"  (theFanfare False)          Map.empty]
>   -- [ Song    "whelpNarp"   whelpNarp                   Map.empty]
>   -- [ Song    "pit"         pit                         Map.empty]
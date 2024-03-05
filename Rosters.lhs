> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Rosters support =======================================================================================================

> module Main where
>
> import Baking
> import Cecil
> import Control.Monad ( foldM )
> import Covers
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Debug.Trace ( traceIO, traceM )
> import Euterpea.IO.MIDI ( play )
> import Euterpea.Music
> import Fanfare
> import Parthenopea
> import SoundFont
> import SunPyg
> import System.Environment ( getArgs )
>
> main                   :: IO ()
> main = do
>   args ← getArgs
>   let playAll          = (length args == 1) && ("all" == head args)
>   _ ← if playAll
>         then doEverything combineAll
>         else doEverything sj -- modulationTest003-- cjingles -- pitchSamples 80
>   return ()

organize exposed music ================================================================================================

> combineAll = ajingles ++ bjingles ++ cjingles ++ djingles ++ ejingles ++ zjingles
>
> ajingles, bjingles
>  , cjingles, djingles, ejingles
>  , zjingles            :: [(String, DynMap → Music (Pitch, [NoteAttribute]))]
>
> ajingles =
>    [ ("theFanfare"     , theFanfare False)
>    , ("slot"           , slot 4)
>    , ("alice"          , alice)
>    , ("bob"            , bob 4)
>    , ("copper"         , copper 2)
>    , ("gold"           , gold)
>    , ("silver"         , silver)
>    , ("deyDumpDum"     , deyDumpDum)]
> bjingles =
>    [ ("getCITM"        , getCITM)
>    , ("bake"           , shimSong $ bakedJingle 9345)
>    , ("bill"           , bill 4)
>    , ("roger"          , roger)]
> cjingles =
>    [ ("cecil"          , cecil)
>    , ("abby"           , abby)
>    , ("wj"             , wj)
>    , ("shelby"         , shelby)
>    , ("weHateHer"      , weHateHer)]
> djingles =
>    [ ("waypostpurple"  , waypostpurple)
>    , ("whelpNarp"      , whelpNarp)
>    , ("snake"          , snake)
>    , ("pendingtonArnt" , pendingtonArnt 2)
>    , ("ssailor"        , ssailor)]
> ejingles =
>    [ ("kit"            , kit)
>    , ("pit"            , pit)
>    , ("dit"            , dit)
>    , ("rattan"         , rattan)]
> zjingles =
>    [ ("deathlessHorsie", deathlessHorsie)
>    , ("basicLick"      , basicLick)
>    , ("sunPyg"         , sunPyg)
>    , ("packardGoose"   , packardGoose)
>    , ("yahozna"        , shimSong $ aggrandize yahozna)]
>
> sj = 
>    -- [("peewee" , shimSong $ aggrandize littlePendingtonArnt)]
>       [ ("deyDumpDum"  , deyDumpDum)]
>    -- [ ("sunPyg"      , sunPyg)]
>    -- [ ("weHateHer"   , weHateHer)]
>    -- [ ("basicLick"   , aggrandize basicLick)]
>    -- [ ("baked"       , shimSong $ bakedJingle 47209)]
>    -- [ ("packardGoose", packardGoose)]
>    -- [ ("roger"       , roger)]
>    -- [ ( "deathlessHorsie"         , deathlessHorsie)]
>    -- [ ("littleSailor"   , shimSong $ aggrandize littleSailor)]
>    -- [ ("cutbake"     , cut 4.5 $ bakedJingle 9123)]

a few playthings ... get it? ==========================================================================================

> playJingle             :: () → (String, Music (Pitch, [NoteAttribute])) → IO ()
> playJingle _ (s, m) =
>    do
>       traceM ( show s ++ " " ++ show (durS (dur m)) ++ " seconds" )
>       playDM Nothing m
>       
> playJingles            :: [(String, Music (Pitch, [NoteAttribute]))] → IO ()
> playJingles jingles =
>    foldM playJingle () (cycle jingles)
>
> playSnippet            :: () → Int → IO ()
> playSnippet () i =
>    let inst :: InstrumentName
>        inst = toEnum (i `mod` fromEnum Gunshot)
>    in do
>       traceIO ("InstrumentName = " ++ show inst)
>       play $ instrument inst pSnippet02
>
> playSnippets           :: IO ()
> playSnippets = 
>    foldM playSnippet () [0..]
> gUnit :: Music Pitch
> gUnit = addDur qn [f 4, a 4, b 4, a 4, f 4, a 4, b 4, a 4
>                  , e 4, a 4, b 4, a 4, e 4, a 4, b 4, a 4]
>
> gUnitAtVolume          :: Volume → Music (Pitch, Volume)
> gUnitAtVolume vol = addVolume vol gUnit
>
> nylon :: Music (Pitch, Volume)
> nylon =
>   removeZeros
>   $ tempo 1
>   $ transpose 0
>   $ keysig A Major
>   $ instrument Glockenspiel
>     (line [gUnitAtVolume  40, rest hn, gUnitAtVolume  60, rest hn, gUnitAtVolume 80, rest hn
>          , gUnitAtVolume 100, rest hn, gUnitAtVolume 120])

The End
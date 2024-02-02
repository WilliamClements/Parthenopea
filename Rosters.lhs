> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
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
>    , ("alice"          , shimSong $ aggrandize alice)
>    , ("bob"            , bob 4)
>    , ("copper"         , shimSong $ aggrandize (copper 2))
>    , ("gold"           , shimSong $ aggrandize gold)
>    , ("silver"         , shimSong $ aggrandize silver)]
> bjingles =
>    [ ("getCITM"        , getCITM)
>    , ("bake"           , shimSong $ bakedJingle 9345)
>    , ("bill"           , bill 4)
>    , ("roger"          , roger)]
> cjingles =
>    [ ("cecil"          , shimSong $ aggrandize cecil)
>    , ("abby"           , shimSong $ aggrandize abby)
>    , ("wj"             , shimSong $ aggrandize wj)
>    , ("shelby"         , shimSong $ aggrandize shelby)
>    , ("weHateHer"      , shimSong $ aggrandize weHateHer)]
> djingles =
>    [ ("waypostpurple"  , waypostpurple)
>    , ("whelpNarp"      , whelpNarp)
>    , ("snake"          , shimSong $ aggrandize snake)
>    , ("pendingtonArnt" , pendingtonArnt 2)
>    , ("ssailor"        , ssailor)]
> ejingles =
>    [ ("kit"            , kit)
>    , ("pit"            , pit)
>    , ("dit"            , dit)
>    , ("rattan"         , shimSong $ aggrandize rattan)]
> zjingles =
>    [ ("deathlessHorsie", deathlessHorsie)
>    , ("basicLick"      , basicLick)
>    , ("sunPyg"         , sunPyg)
>    , ("yahozna"        , shimSong $ aggrandize yahozna)]
>
> sj =
>    [("pendingtonArnt" , pendingtonArnt 2)]
>
>    -- [ ("sunPyg"      , sunPyg)]
>    -- [ ("slot"        , aggrandize (slot 1))]
>    -- [ ("basicLick"   , aggrandize basicLick)]
>    -- [ ("baked"       , bakedJingle 54349)]
>    -- [ ("ssailor"     , aggrandize ssailor)]
>    -- [ ("cut4roger"   , cut 4 $ aggrandize roger)]
>    -- [ ("bob"         , aggrandize (bob 2))]
>    -- [ ("silver"         , shimSong $ aggrandize silver)]
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
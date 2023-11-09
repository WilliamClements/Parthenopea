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
> import Debug.Trace ( traceIO, traceM )
> import Euterpea.IO.MIDI ( play )
> import Euterpea.Music
> import Fanfare
> import Modulation ( modulationTest003 )
> import Parthenopea ( aggrandize, playDM, addDur, durS, pSnippet02 )
> -- import RosterDef
> import SoundFont
> import SunPyg
> import System.Environment ( getArgs )  
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
>  , cjingles, djingles
>  , zjingles            :: [(String, Music (Pitch, [NoteAttribute]))]
>
> ajingles =
>    [ ("theFanfare"     , aggrandize theFanfare)
>    , ("slot"           , aggrandize (slot 4))
>    , ("alice"          , aggrandize alice)
>    , ("bob"            , aggrandize (bob 4))
>    , ("copper"         , aggrandize (copper 2))
>    , ("gold"           , aggrandize gold)
>    , ("silver"         , aggrandize silver)]
> bjingles =
>    [ ("getCITM"        , aggrandize getCITM)
>    , ("bake"           , bakedJingle 9345)
>    , ("bill"           , aggrandize (bill 4))
>    , ("roger"          , aggrandize roger)]
> cjingles =
>    [ ("cecil"          , aggrandize cecil)
>    , ("abby"           , aggrandize abby)
>    , ("wj"             , aggrandize wj)
>    , ("shelby"         , aggrandize shelby)
>    , ("weHateHer"      , aggrandize weHateHer)]
> djingles =
>    [ ("waypostpurple"  , aggrandize waypostpurple)
>    , ("whelpNarp"      , aggrandize whelpNarp)
>    , ("snake"          , aggrandize snake)
>    , ("pendingtonArnt" , aggrandize (pendingtonArnt 2))
>    , ("ssailor"        , aggrandize ssailor)]
> ejingles =
>    [ ("kit"            , aggrandize kit)
>    , ("pit"            , aggrandize pit)
>    , ("dit"            , aggrandize dit)
>    , ("rattan"         , aggrandize rattan)]
> zjingles =
>    [ ("deathlessHorsie", aggrandize deathlessHorsie)
>    , ("basicLick"      , aggrandize basicLick)
>    , ("sunPyg"         , aggrandize sunPyg)
>    , ("yahozna"        , aggrandize yahozna)]
> sj =
>    [ ("wj"             , aggrandize wj) 
>    -- [ ("sunPyg"      , aggrandize sunPyg)]
>    -- [ ("littleAbby"     , aggrandize littleAbby)]
>    -- [ ("cut4roger"   , cut 4 $ aggrandize roger)]
>    -- [ ("roger"       , aggrandize roger)]
>    -- [ ("copper'"     , aggrandize copper')]
>    -- [ ("bake"        , {- cut 1.5 -} bakedJingle 98203)]
>    ]


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
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Rosters
William Clements
May 4, 2023

> module Rosters where
>
> import Baking
> import Cecil
> import Control.Monad ( foldM, join )
> import Covers
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Debug.Trace ( traceIO, traceM )
> import Discrete
> import DiscreteTest
> import Euterpea.IO.MIDI ( play )
> import Euterpea.Music
> import Fanfare
> import ModulationTest
> import Parthenopea
> import ParthenopeaTest
> import SoundFont
> import SunPyg
> import SynthesizerTest
> import System.Environment ( getArgs )

Rosters support =======================================================================================================

> rmain                   :: IO ()
> rmain = do
>   args ← getArgs
>   let playAll          = (length args == 1) && ("all" == head args)
>   let doProf           = (length args == 1) && ("prof" == head args)
>
>   _ ← if playAll
>         then do
>           resultA                        ← runTestsQuietly modulationTests     
>           resultB                        ← runTestsQuietly synthesizerTests
>           let resultC                    = True -- runTestsQuietly discreteTests
>           resultD                        ← runTestsQuietly parthTests
>           let all                        =
>                 profess
>                   (resultA && resultB && resultC && resultD)
>                   (unwords ["one or more unit tests failed"])
>                   True
>           putStrLn "Unit tests completed successfully"
>           doEverything combineAll
>         else if doProf
>           then profileSF2s
>           else doEverything sj -- modulationTest003-- cjingles -- pitchSamples 80
>   return ()
>
> playWithWav            :: IO ()
> playWithWav                              = do
>   wav ← importWav' "BatCave.wav"
>   print wav

organize exposed music ================================================================================================

> combineAll = ajingles ++ bjingles ++ cjingles ++ djingles ++ ejingles ++ zjingles
>
> ajingles, bjingles
>  , cjingles, djingles, ejingles
>  , zjingles            :: [(String, DynMap → Music (Pitch, [NoteAttribute]))]
>
> ajingles =
>    [ ("theFanfare"     , theFanfare False)
>    , ("alice"          , alice)
>    , ("bob"            , bob 4)
>    , ("copper"         , copper 2)
>    , ("gold"           , gold)
>    , ("silver"         , silver)
>    , ("deyDumpDum"     , deyDumpDum)]
> bjingles =
>    [ ("getCITM"        , getCITM)
>    , ("slot"           , slot 4)
>    , ("whelpNarp"      , whelpNarp)
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
>    -- [ ("testslot"    , shimSong $ aggrandize testslot)]
>    -- [ ("littleDH"    , shimSong $ aggrandize littleDH)]
>    -- [ ("dh"          , deathlessHorsie)]
>    -- [ ("bill1"       , bill 1)]
>    -- [ ("pa"          , pendingtonArnt 1)]
>       [ ("gold"        , gold)]
>    -- [ ("deyDumpDum"  , deyDumpDum)]
>    -- [ ("baked"       , shimSong $ bakedJingle 23320)]
>    -- [ ("slot_1"      , slot 1)]
>    -- [ ("theFanfare"  , theFanfare)]
>    -- [ ("pit"         , pit)]
>    -- [ ("littleSailor", shimSong $ aggrandize littleSailor)]

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
> shredAll               :: IO (Map Kind Shred)
> shredAll                                 = do
>   allSongs                               ← mapM shredSong combineAll
>   return $ foldr (Map.unionWith combineShreds) Map.empty allSongs
>
> doShredAll             :: IO ()
> doShredAll                               = do
>   allShreds                              ← shredAll
>   mapM_ (\(x, y) → print (x, shCount y)) (Map.assocs allShreds)
>
> playSnippet            :: () → Int → IO ()
> playSnippet () i =
>   let inst :: InstrumentName
>       inst = toEnum (i `mod` fromEnum Gunshot)
>   in do
>     traceIO ("InstrumentName = " ++ show inst)
>     play $ instrument inst pSnippet02
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
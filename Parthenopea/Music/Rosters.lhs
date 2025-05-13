> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Rosters
William Clements
May 4, 2023

> module Parthenopea.Music.Rosters where
>
> import Control.Monad ( foldM )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Debug.Trace ( traceIO, traceM )
> import Euterpea.IO.MIDI ( play )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Baking
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.ModulationTest ( modulationTests )
> import Parthenopea.Repro.SmashingTest ( smashingTests )
> import Parthenopea.Repro.SynthesizerTest ( synthesizerTests )
> import Parthenopea.SoundFont.Boot ( equipInstruments )
> import Parthenopea.SoundFont.BootTest ( bootTests )
> import Parthenopea.SoundFont.Runtime ( bootNRender )
> import Parthenopea.Tunes.Cecil
> import Parthenopea.Tunes.Covers
> import Parthenopea.Tunes.Fanfare
> import Parthenopea.Tunes.SunPyg
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
>           resultSmashing              ← runTestsQuietly smashingTests
>           resultBoot                     ← runTestsQuietly bootTests
>           resultModulation               ← runTestsQuietly modulationTests     
>           resultSynthesizer              ← runTestsQuietly synthesizerTests
>           let resultDiscrete             = True -- runTestsQuietly discreteTests
>           putStrLn $ unwords [show
>                 (profess
>                   (and [resultSmashing, resultBoot, resultModulation, resultSynthesizer, resultDiscrete])
>                   (unwords ["one or more unit tests failed"])
>                   True)]
>           putStrLn "Unit tests completed successfully"
>           bootNRender combineAll
>         else if doProf
>           then listInstruments
>           else bootNRender sj
>   return ()
>
> playWithWav            :: IO ()
> playWithWav                              = do
>   wav ← importWav' "BatCave.wav"
>   print wav

organize exposed music ================================================================================================

> combineAll             :: [(String, DynMap → Music (Pitch, [NoteAttribute]))]
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
>    , ("deyDumpDum"     , deyDumpDum False)]
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
> sj                     :: [(String, Map InstrumentName InstrumentName → Music (Pitch, [NoteAttribute]))]
> sj = ejingles
>    -- [ ("gold"        , gold)]
>    -- [ ("slot"        , slot 1)]
>    -- [ ("alice"       , alice)]
>    -- [ ("pa"          , pendingtonArnt 1)]
>    -- [ ("deyDumpDum"  , deyDumpDum False)]
>    -- [ ("baked"       , shimSong $ bakedJingle 42310)]
>    -- [ ("bob_1"       , bob 1)]
>    -- [ ("theFanfare"  , theFanfare False)]
>    -- [ ("kit"         , kit)]
>    -- [ ("wj"          , wj)]

a few playthings ... get it? ==========================================================================================

> durS                   :: Rational → Double
> durS r                                   = 2 * fromRational r
> 
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
> shredAll               :: IO (Map GMKind Shred)
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
>
> listInstruments        :: IO ()
> listInstruments                          = do
>   mbundle                                ← equipInstruments allKinds
>   if isJust mbundle
>     then do
>       let (runt, _, _)                   = fromJust mbundle
>       print runt
>     else do
>       return ()

The End
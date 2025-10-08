> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

RangesReport
William Clements
October 5, 2025

> module Parthenopea.SoundFont.RangesReport ( runUnitTests, writeRangesReport ) where
>
> import Data.Array.Unboxed ( inRange )
> import Data.Either
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Euterpea.IO.MIDI.MEvent ( MEvent(ePitch) )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.PassageTest ( passageTests )
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.EnvelopesTest ( envelopesTests )
> import Parthenopea.Repro.ModulationTest ( modulationTests )
> import Parthenopea.Repro.SmashingTest ( smashingTests )
> import Parthenopea.Repro.SynthesizerTest ( synthesizerTests )
> import Parthenopea.SoundFont.BootTest ( bootTests )
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility

unit tests ============================================================================================================

> runUnitTests           :: IO ()
> runUnitTests                             = do
>   resultBoot                             ← runTestsQuietly bootTests
>   resultEnvelopes                        ← runTestsQuietly envelopesTests
>   resultModulation                       ← runTestsQuietly modulationTests     
>   resultSmashing                         ← runTestsQuietly smashingTests
>   resultSynthesizer                      ← runTestsQuietly synthesizerTests
>   resultsPassage                         ← runTestsQuietly passageTests
>   let resultDiscrete                     = True -- runTestsQuietly discreteTests
>   putStrLn $ unwords [show
>      (profess
>        (and [resultSmashing, resultBoot, resultModulation, resultSynthesizer
>            , resultsPassage, resultEnvelopes, resultDiscrete])
>        (unwords ["one or more unit tests failed"])
>        True)]
>   putStrLn "Unit tests completed successfully"

check all the incoming music for instrument range violations ==========================================================

> writeRangesReport      :: [Song] → Map GMKind Shred → IO ()
> writeRangesReport songs ding             = do
>   let rollup                             =
>         Song "rollup" (const (foldr ((:+:) . uncap . songMusic) (rest 0) songs)) ding
>   let esAll                              = concatMap doSong songs
>   let esPrefix                           =
>         [ToFieldL "GMKind" 20
>        , ToFieldL "(range)" 22
>        , ToFieldR "lowest" 8, Blanks 3
>        , ToFieldL "*status" 8
>        , ToFieldR "highest" 8, Blanks 3
>        , ToFieldL "*status" 8
>        , ToFieldL "note count" 15
>        , ToFieldL "alternative" 20, EndOfLine]
>   let esSuffix                          = if 1 < length songs
>                                             then doSong rollup
>                                             else []
>   writeFileBySections reportRangesName [esPrefix, esAll, esSuffix]
>
>   where
>     uncap              :: (DynMap → Music1) → Music1
>     uncap m                              = m Map.empty
>
>     doSong             :: Song → [Emission]
>     doSong song                          =
>       [EndOfLine
>      , ToFieldL song.songName 60
>      , ToFieldL (songTimeAndNoteCount song) 60
>      , EndOfLine, EndOfLine] ++ doMusic song.songShredding
>     doMusic            :: Map GMKind Shred → [Emission]
>     doMusic ding'                        = concatMap (uncurry doGMKind) (Map.assocs ding')
>     doGMKind           :: GMKind → Shred → [Emission]
>     doGMKind gmkind shred                = either doInstrument doPercussion gmkind
>       where
>         lo                               = shred.shLowNote.ePitch
>         hi                               = shred.shHighNote.ePitch
>
>         this                             = fromLeft (error "writeRangesReport") gmkind
>         alt                              = findBetterInstrument this (lo, hi)
>         strAlt                           = if isLeft gmkind && (alt /= this) 
>                                              then show alt
>                                              else ""  
>         doInstrument       :: InstrumentName → [Emission]
>         doInstrument kind                =
>           [emitShowL kind 20
>          , emitShowL (instrumentPitchRange kind) 22
>          , emitShowR (pitch lo) 8, Blanks 3
>          , ToFieldL (indicator lo) 8
>          , emitShowR (pitch hi) 8, Blanks 3
>          , ToFieldL (indicator hi) 8
>          , emitShowR shred.shCount 10, Blanks 5
>          , ToFieldL strAlt 20, EndOfLine]
>           
>         doPercussion       :: PercussionSound → [Emission]
>         doPercussion kind                =
>           [emitShowL kind 20
>          , emitShowL (fromEnum kind + 35) 22
>          , Blanks 44
>          , emitShowL shred.shCount 15, EndOfLine]
>
>         mrange                           =
>           case gmkind of
>             Left iname                   → instrumentAbsPitchRange iname
>             _                            → Nothing
>         indicator p                      = if isNothing mrange || inRange (deJust "range" mrange) p
>                                              then "*in"
>                                              else "*out!!"

The End
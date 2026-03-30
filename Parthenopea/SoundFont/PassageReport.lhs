> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

PassageReport
William Clements
October 5, 2025

> module Parthenopea.SoundFont.PassageReport (
>        summarizeOnePassage
>        , writePassageReport ) where
>
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import Data.Either
> import Data.Map.Strict ( Map )
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Debug.Trace ( traceIO )
> import Euterpea.IO.MIDI.MEvent ( MEvent(..) )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Passage
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility

summarize incoming passage ============================================================================================

> summarizeOnePassage     :: VB.Vector MekNote → [Emission]
> summarizeOnePassage meksIn               = concatMap summarize meksIn
>   where
>     summarize           :: MekNote → [Emission]
>     summarize mek                        = [Unblocked (show (mek.mSelfIndex, mek.mParams, mek.mPrimitive)), EndOfLine]
>
> writePassageReport      :: Directives → [Song] → Map GMKind Shred → IO ()
> writePassageReport dives songs ding      = do
>   CM.when diagnosticsEnabled             (traceIO $ unwords [fName, "--", show $ length songs, "songs"])
>   let rollup                             =
>         Song "rollup" (foldr ((:+:) . songMusic) (rest 0) songs) ding
>   let esAll                              = concatMap doSong songs
>   let esPrefix                           =
>         [ToFieldL "GMKind" 22
>        , ToFieldL "(working range)" 22
>        , ToFieldL "lowest" 8, Blanks 3
>        , ToFieldL "*status" 8
>        , ToFieldR "highest" 8, Blanks 3
>        , ToFieldL "*status" 8
>        , ToFieldL "note count" 15
>        , ToFieldL "alternative" 20, EndOfLine]
>   let esRollup                          = if 1 < length songs
>                                             then doSong rollup
>                                             else []
>   writeReportBySections dives reportRangesName [esPrefix, esAll, esRollup]
>   where
>     fName                                = "writeRangesReport"
>
>     doSong             :: Song → [Emission]
>     doSong song                          =
>       [EndOfLine
>      , ToFieldL song.songName 60
>      , ToFieldL (songTimeAndNoteCount song) 60
>      , EndOfLine, EndOfLine] ++ doMusic song.shreds
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
>           [emitShowL kind 22
>          , emitShowL (instrumentPitchRange kind) 22
>          , emitShowL (pitch lo) 8, Blanks 3
>          , ToFieldL (indicator lo) 8
>          , emitShowL (pitch hi) 8, Blanks 3
>          , ToFieldL (indicator hi) 8
>          , emitShowL shred.shCount 10, Blanks 5
>          , ToFieldL strAlt 20, EndOfLine]
>           
>         doPercussion       :: PercussionSound → [Emission]
>         doPercussion kind                =
>           [emitShowL kind 22
>          , emitShowL (fromEnum kind + 35) 22
>          , Blanks 38
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
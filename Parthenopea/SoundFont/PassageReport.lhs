> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

PassageReport
William Clements
October 5, 2025

> module Parthenopea.SoundFont.PassageReport (
>        summarizeOnePassage
>        , writePassageReport ) where
>
> import Control.Lens hiding ( element, strict )
> import qualified Control.Monad           as CM
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Debug.Trace ( traceIO )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Passage
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.SFSpec

summarize incoming passage ============================================================================================

> summarizeOnePassage     :: VB.Vector MekNote → [Emission]
> summarizeOnePassage                      = concatMap summarize
>   where
>     summarize           :: MekNote → [Emission]
>     summarize mek                        = [Unblocked (show (mek.mSelfIndex, mek.mParams, mek.mPrimitive)), EndOfLine]
>
> writePassageReport      :: Directives → [Song] → IO ()
> writePassageReport dives songs           = do
>   CM.when diagnosticsEnabled             (traceIO $ unwords [fName, "--", show $ length songs, "songs"])
>   let esSongs                            = concatMap doSong songs
>   let esPrefix                           =
>         [ToFieldL "fake" 22
>        , ToFieldL "fake" 22
>        , ToFieldL "fake" 8, Blanks 3
>        , ToFieldL "fake" 8
>        , ToFieldR "fake" 8, Blanks 3
>        , ToFieldL "fake" 8
>        , ToFieldL "fake" 15
>        , ToFieldL "alternative" 20, EndOfLine]
>   CM.unless (null esSongs)               (writeReportBySections dives reportPassageName [esPrefix, esSongs])
>   where
>     fName                                = "writePassageReport"
>
>     doSong             :: Song → [Emission]
>     doSong song                          =
>       let
>         esSong                           = goPassages emitter song.songMusic
>       in
>         if null esSong
>           then []
>           else 
>             [ EndOfLine
>             , ToFieldL song.songName 60
>             , EndOfLine, EndOfLine] ++ esSong
>
>     emitter            :: PContext → NoteAttributeDigest → (Dur, Pitch) → [Emission]
>     emitter pc nad (d, p)                = es
>       where
>         p'                               = xpo p
>         es                               =
>           if PTypePassage == (pc ^. pcType)
>             then case nad.jazzParams of
>               Nothing                    → []
>               Just ps                    →
>                     [ToFieldL (fromJust nad.jazzName) 30
>                   , emitShowL d 14
>                   , emitShowL p 10
>                   , if p == p'
>                       then Blanks 10
>                       else emitShowL p' 10
>                   , Unblocked (show ps)
>                   , EndOfLine]
>             else []
>         xpo            :: Pitch → Pitch
>         xpo pIn                          = if 0 == pc ^. pcXpo
>                                              then pIn
>                                              else (pitch . (+ pc ^. pcXpo) . absPitch) pIn

The End
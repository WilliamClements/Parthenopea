> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

PassageReport
William Clements
October 5, 2025

> module Parthenopea.SoundFont.PassageReport (
>          summarizeOnePassage
>        , writePassageReport ) where
>
> import Control.Lens hiding ( element, strict )
> import qualified Control.Monad           as CM
> import qualified Data.Vector.Strict      as VB
> import Debug.Trace ( traceIO )
> import Euterpea.Music
> import Parthenopea.Debug ( diagnosticsEnabled )
> import Parthenopea.Music.Passage
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility

summarize incoming passage ============================================================================================

> summarizeOnePassage     :: VB.Vector MekNote → [Emission]
> summarizeOnePassage                      = concatMap summarize
>   where
>     summarize           :: MekNote → [Emission]
>     summarize mek                        =
>       [Unblocked (show (mek.mSelfIndex, mek.mParams, mek.mPrimitive)), EndOfLine]
>
> writePassageReport      :: Directives → [Song] → IO ()
> writePassageReport dives songs           = do
>   CM.when diagnosticsEnabled             (traceIO fName)
>   CM.unless (null esSongs)               (writeReportBySections dives reportPassageName [velos, esSongs])
>   where
>     fName                                = "writePassageReport"
>
>     esSongs                              = concatMap reportSong songs
>     velos                                = concatMap describe [PPP .. FFF] ++ [EndOfLine]
>       where
>         describe       :: StdLoudness → [Emission]            
>         describe loud                    =
>           [  Blanks 9
>            , ToFieldL (show loud) 10
>            , ToFieldL " = " 4
>            , emitShowL (stdVelocity loud) 10
>            , EndOfLine]
>
>     reportSong         :: Song → [Emission]
>     reportSong song                      =
>       let
>         esSong                           = doSongPassages doPassageNotes song.songMusic
>       in
>         if null esSong
>           then []
>           else 
>             [ EndOfLine
>             , ToFieldL song.songName 60
>             , EndOfLine, EndOfLine] ++ esLegend ++ [EndOfLine] ++ esSong
>
>     doPassageNotes     :: VB.Vector PassageNote → [Emission]
>     doPassageNotes vpn                   = if null vpn
>                                              then []
>                                              else concat (VB.map doPassageNote vpn) ++ [EndOfLine]
>
>     doPassageNote      :: PassageNote → [Emission]
>     doPassageNote (PassageNote pc nad d pWritten)
>                                          = esFields ++ [EndOfLine]
>       where
>         pConcert                         = if 0 == pc ^. pcXpo
>                                              then pWritten
>                                              else (pitch . (+ pc ^. pcXpo) . absPitch) pWritten
>
>         esFields                         =
>           emitFields 
>             [  (5, Nothing)
>              , (30, nad ^. nadName)
>              , (25, fmap show (pc ^. pcInst))
>              , (14, Just (show d))
>              , (10, Just (show pConcert))
>              , (12, if pWritten == pConcert then Nothing else Just (show pWritten))
>              , (30, fmap (show . roundVectorBy 100) (nad ^. nadParams))
>              , (10, fmap show (nad ^. nadVolume))]
>
>     esLegend                             =
>         [Blanks                5
>        , ToFieldL "name"       30
>        , ToFieldL "instrument" 25
>        , ToFieldL "duration"   14
>        , ToFieldL "concert"    10
>        , ToFieldL "written"    12
>        , ToFieldL "params"     30
>        , ToFieldL "volume"     10]

The End
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
June 16, 2025

> module Parthenopea.SoundFont.Command ( pCommand, batchProcessor ) where
>
> import qualified Codec.Midi              as M
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Either ( lefts, rights )
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe ( fromMaybe )
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF )
> import Euterpea.IO.MIDI ( fromMidi )
> import Euterpea.Music
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Boot ( surveyInstruments )
> import Parthenopea.SoundFont.RangesReport
> import Parthenopea.SoundFont.ScanReport
> import Parthenopea.SoundFont.Runtime
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.TournamentReport
> import Parthenopea.SoundFont.Utility
> import qualified System.FilePattern.Directory
>                                          as FP

Implement PCommand ====================================================================================================

> pCommand               :: IO ()
> pCommand                                 = do
>   batchProcessor defDirectives []
>
> batchProcessor         :: Directives → [Song] → IO ()
> batchProcessor dives isongs              = do
>   timeThen                               ← getZonedTime
>   putStr $ reapEmissions $ openingRemarks timeThen
>
>   mids                                   ← FP.getDirectoryFiles "." ["*.mid", "*.midi"]
>   sf2s                                   ← FP.getDirectoryFiles "." ["*.sf2"]
>   putStr $ reapEmissions [msg mids sf2s, EndOfLine]
>   CM.unless (okDirectives dives) (error "garbage in Directives")
>   proceed dives isongs mids sf2s
>
>   timeNow                                ← getZonedTime
>   putStr $ reapEmissions $ closingRemarks timeNow timeThen
>
>   return ()
>   where
>     msg                :: [FilePath] → [FilePath] → Emission
>     msg ms ss
>       | null ms && null isongs && null ss
>                                          = Unblocked "no *.mid or *.sf2 files found: nothing to do"
>       | null ss                          = Unblocked "no *.sf2 files found: proceeding to survey mids"
>       | null ms && null isongs           = Unblocked "no *.mid files found: proceeding to survey sf2s"
>       | otherwise                        = Unblocked "proceeding to render mids by sf2s"
>
> proceed                :: Directives → [Song] → [FilePath] → [FilePath] → IO ()
> proceed dives isongs mids sf2s           = do
>   msongs                                 ← mapM convertFromMidi mids
>   let songs_                             = isongs ++ msongs
>   songs                                  ← mapM captureSong songs_
>
>   CM.when (20 < length songs && not (null sf2s)) runUnitTests
>   let ding                               = Map.unionsWith combineShreds (map songShredding songs)
>   CM.when (dForRanges > 0) (writeRangesReport dives songs ding)
>
>   rost                                   ← qualifyKinds ding songs
>
>   CM.unless (null sf2s)
>             (putStr $ reapEmissions [  EndOfLine
>                                      , Unblocked $ unwords ["surveySoundFonts"], EndOfLine])
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFilesBoot                         = VB.fromList extraction
>   if VB.null vFilesBoot
>     then return ()
>     else do
>       (runt, choices)                     ← commandLogic dives rost vFilesBoot
>       -- here's the heart of the coconut
>       mapM_ (renderSong runt choices) songs
>   where
>     ReportVerbosity{ .. }               
>                                          = dives.dReportVerbosity
>
> commandLogic           :: Directives → ([InstrumentName], [PercussionSound])
>                           → VB.Vector SFFileBoot
>                           → IO (SFRuntime, ( Map InstrumentName (Bool, Maybe PerGMKey, [Emission])
>                                            , Map PercussionSound (Bool, Maybe PerGMKey, [Emission])))
> commandLogic dives rost vFilesBoot       = do
>   (cache, matches, rd)                   ← surveyInstruments dives rost vFilesBoot
>   CM.when (dForScan > 0)                 (writeScanReport dives dForScan vFilesBoot rd)
>   (zI, zP)                               ← establishWinners dives rost vFilesBoot cache matches
>   runt                                   ← prepareRuntime dives rost vFilesBoot cache (zI, zP)
>   return (runt, (zI, zP))
>   where
>     ReportVerbosity{ .. }
>                                          = dives.dReportVerbosity
>
> renderSong             :: SFRuntime
>                           → ( Map InstrumentName (Bool, Maybe PerGMKey, [Emission])
>                             , Map PercussionSound (Bool, Maybe PerGMKey, [Emission]))
>                           → Song
>                           → IO ()
> renderSong runt choices (Song name music ding)
>                                          =
>   do
>     let switches                         = runt.zDirectives.synthSwitches
>
>     tsStart                              ← getZonedTime
>     putStr $ reapEmissions [Unblocked $ unwords ["renderSong", name], EndOfLine]
>
>     let dynMap                           = makeDynMap ding
>     CM.unless (null dynMap)              
>               (putStr $ reapEmissions [Unblocked $ unwords ["dynMap", show dynMap], EndOfLine])
>     let ks                               = Map.keys ding
>     let (is, ps)                         = (map (\i → fromMaybe i (Map.lookup i dynMap)) (lefts ks), rights ks)
>     let (esI, esP)                       = printChoices choices is ps
>     let ex                               = concatMap snd esI ++ concatMap snd esP
>     putStr $ reapEmissions ex
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let (durS, s)                    = renderSF (music dynMap) runt.zInstrumentMap
>         if switches.normalizingOutput
>           then outFileNorm               (name ++ ".wav") durS s
>           else outFile                   (name ++ ".wav") durS s
>         tsFinish                         ← getZonedTime
>         putStr $ reapEmissions $ emitSongTime durS tsStart tsFinish
>       else
>         putStr $ reapEmissions [Unblocked "skipping..."]
>     return ()
> 
> convertFromMidi        :: FilePath → IO Song
> convertFromMidi path                     = do
>   midi_                                  ← M.importFile path
>   let midi                               = case midi_ of
>                                              Left err → error err
>                                              Right m  → m
>   return $ Song (removeExtension path) (const $ fromMidi midi) Map.empty
>   where
>     removeExtension fp                   =
>       let
>         fpRev                            = reverse fp
>         fpRev'                           = dropWhile (/= '.') fpRev
>         fpRev''                          = drop 1 fpRev'
>       in
>         reverse fpRev''
>
> qualifyKinds           :: Map GMKind Shred → [Song] → IO ([InstrumentName], [PercussionSound])
> qualifyKinds ding songs                  = do
>   let isandps                            = Map.keys ding
>   return $ if null songs then allKinds else (lefts isandps, rights isandps)
>
> openSoundFontFile      :: Int → FilePath → IO SFFileBoot
> openSoundFontFile wFile filename         = do
>   putStr $ reapEmissions [Unblocked (unwords [show wFile, filename]), EndOfLine]
>   result                                 ← F.importFile filename
>   case result of
>     Left s                               →
>       error $ unwords ["openSoundFontFile", "decoding error", s, show filename]
>     Right soundFont                      → do
>       let pdata                          = F.pdta soundFont
>       let sdata                          = F.sdta soundFont
>       let boota                          =
>             FileArrays
>               (F.insts pdata) (F.ibags pdata)
>               (F.igens pdata) (F.imods pdata)
>               (F.shdrs pdata)
>       let samplea                        = SampleArrays (F.smpl  sdata) (F.sm24  sdata)
>       let sffileBoot                     = SFFileBoot wFile filename boota samplea
>       return sffileBoot

The End
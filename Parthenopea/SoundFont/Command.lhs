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
> import Data.Time.Clock ( diffUTCTime, getCurrentTime )
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF )
> import Euterpea.IO.MIDI ( fromMidi )
> import Euterpea.Music
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Synthesizer ( normalizingOutput )
> import Parthenopea.SoundFont.Boot ( surveyInstruments )
> import Parthenopea.SoundFont.RangesReport
> import Parthenopea.SoundFont.ScanReport
> import Parthenopea.SoundFont.Runtime
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.TournamentReport
> import qualified System.FilePattern.Directory
>                                          as FP

Implement PCommand ====================================================================================================

> pCommand               :: IO ()
> pCommand                                 = do
>   batchProcessor defDirectives []
>
> batchProcessor         :: Directives → [Song] → IO ()
> batchProcessor dives isongs              = do
>   mids                                   ← FP.getDirectoryFiles "." ["*.mid", "*.midi"]
>   sf2s                                   ← FP.getDirectoryFiles "." ["*.sf2"]
>   putStrLn (msg mids sf2s)
>   CM.unless ( okDirectives dives) (error "garbage in Directives")
>   proceed dives isongs mids sf2s
>   where
>     msg                :: [FilePath] → [FilePath] → String
>     msg ms ss
>       | null ms && null isongs && null ss
>                                          = "no *.mid or *.sf2 files found: nothing to do"
>       | null ss                          = "no *.sf2 files found: proceeding to survey mids"
>       | null ms && null isongs           = "no *.mid files found: proceeding to survey sf2s"
>       | otherwise                        = "proceeding to render mids by sf2s"
>
> proceed                :: Directives → [Song] → [FilePath] → [FilePath] → IO ()
> proceed dives isongs mids sf2s           = do
>   msongs                                 ← mapM convertFromMidi mids
>   let songs_                             = isongs ++ msongs
>   songs                                  ← mapM captureSong songs_
>
>   CM.when (20 < length songs && not (null sf2s)) runUnitTests
>   let ding                               = Map.unionsWith combineShreds (map songShredding songs)
>   CM.when (dForRanges > 0) (writeRangesReport songs ding)
>
>   rost                                   ← qualifyKinds ding songs
>
>   CM.unless (null sf2s) (do putStrLn ""; putStrLn $ unwords ["surveySoundFonts"])
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFilesBoot                         = VB.fromList extraction
>   if VB.null vFilesBoot
>     then return ()
>     else do
>       runt                               ← commandLogic dives rost vFilesBoot
>
>       -- here's the heart of the coconut
>       mapM_ (renderSong runt) songs
>   where
>     ReportVerbosity{ .. }               
>                                          = dives.dReportVerbosity
>
> renderSong             :: SFRuntime → Song → IO ()
> renderSong runt (Song name music ding)   =
>   do
>     timeNow                              ← getCurrentTime
>     putStrLn $ unwords ["renderSong", name, show timeNow, "->"]
>
>     let dynMap                           = makeDynMap ding
>     CM.unless (null dynMap)              (putStrLn $ unwords ["dynMap", show dynMap])
>     let ks                               = Map.keys ding
>     let (is, ps)                         = (map (\i → fromMaybe i (Map.lookup i dynMap)) (lefts ks), rights ks)
>     let (esI, esP)                       = printChoices runt is ps
>     let ex                               = concatMap snd esI ++ concatMap snd esP
>     putStr $ reapEmissions ex
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let (durS, s)                    = renderSF (music dynMap) runt.zInstrumentMap
>         if normalizingOutput
>           then outFileNorm               (name ++ ".wav") durS s
>           else outFile                   (name ++ ".wav") durS s
>
>         tsFinished                       ← getCurrentTime
>         putStrLn $ unwords ["<-", name, show (diffUTCTime tsFinished timeNow)]
>       else
>         putStrLn "skipping..."
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
>     removeExtension    :: FilePath → FilePath
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
>   putStrLn (unwords [show wFile, filename])
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
>
> commandLogic           :: Directives → ([InstrumentName], [PercussionSound]) → VB.Vector SFFileBoot → IO SFRuntime
> commandLogic dives rost vFilesBoot       = do
>   (cache, matches, rd)                   ← surveyInstruments dives rost vFilesBoot
>   CM.when (dForScan > 0)                 (writeScanReport dForScan vFilesBoot rd)
>   (zI, zP)                               ← establishWinners dives rost vFilesBoot cache matches
>   prepareRuntime dives rost vFilesBoot cache (zI, zP)
>   where
>     ReportVerbosity{ .. }
>                                          = dives.dReportVerbosity

The End
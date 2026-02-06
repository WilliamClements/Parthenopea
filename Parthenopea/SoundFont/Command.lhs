> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
June 16, 2025

> module Parthenopea.SoundFont.Command
>        (  pCommand
>         , batchProcessor
>         ) where
>
> import qualified Codec.Midi              as M
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Either
> import qualified Data.IntMap.Strict as IntMap
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF )
> import Euterpea.IO.MIDI ( fromMidi )
> import Euterpea.Music ( InstrumentName, PercussionSound )
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Boot ( surveyInstruments )
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.RangesReport
> import Parthenopea.SoundFont.ScanReport ( writeScanReport )
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
>   timeThen                               ← getZonedTime
>   putStr $ reapEmissions $ openingRemarks timeThen
>   CM.unless (okDirectives dives)         (error $ unwords[fName, "garbage in Directives", show dives])
>
>   mids                                   ← FP.getDirectoryFiles "." ["*.mid", "*.midi"]
>   sf2s                                   ← FP.getDirectoryFiles "." ["*.sf2"]
>   putStr $ reapEmissions [msg mids sf2s, EndOfLine]
>
>   proceed dives isongs mids sf2s
>
>   timeNow                                ← getZonedTime
>   putStr $ reapEmissions $ closingRemarks timeNow timeThen
>   where
>     fName                                = "batchProcessor"
>
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
>   songs                                  ← mapM captureSong (isongs ++ msongs)
>
>   CM.when (20 < length songs && not (null sf2s)) runUnitTests
>
>   let ding                               = Map.unionsWith combineShreds (map shreds songs)
>   CM.when (dForRanges > 0) (writeRangesReport dives songs ding)
>
>   rost                                   ← identifyRoster ding songs
>
>   CM.unless (null sf2s)
>             (putStr $ reapEmissions [  EndOfLine
>                                      , Unblocked $ unwords ["surveySoundFonts"], EndOfLine])
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFilesBoot                         = VB.fromList extraction
>   if VB.null vFilesBoot
>     then return ()
>     else do
>       putStrLn "extracted!"
>       (runt, choices)                    ← commandLogic dives rost vFilesBoot
>       -- here's the heart of the coconut
>       mapM_ (renderSong runt choices) songs
>   where
>     captureSong (Song name music _)      = do
>       ding                               ← shredMusic music
>       return $ Song name music ding
>     identifyRoster ding songs            = return $ if null songs then allKinds else partitionEithers $ Map.keys ding
>     ReportVerbosity{ .. }               
>                                          = dives.dReportVerbosity
>
> commandLogic           :: Directives → ([InstrumentName], [PercussionSound])
>                           → VB.Vector SFFileBoot
>                           → IO (SFRuntime, (Map InstrumentName GMChoices, Map PercussionSound GMChoices))
> commandLogic dives rost vFilesBoot_      = do
>   putStrLn "commandLogic..."
>   (vFilesBoot, survey)                   ← surveyInstruments dives rost vFilesBoot_
>
>   let rd                                 = survey.sDispositions
>   let cache                              = survey.sPerInstruments
>   let matches                            = survey.sMatches
>
>   CM.when (dForScan > 0)                 (writeScanReport dives dForScan vFilesBoot rd)
>   (wI, wP)                               ← decideWinners dives rost vFilesBoot cache matches
>   CM.when (dForTournament > 0)           (writeTournamentReport dives vFilesBoot (wI, wP))
>   (zI, zP)                               ← establishWinners rost (wI, wP)
>   runt                                   ← prepareRuntime dives rost vFilesBoot cache (zI, zP)
>   return (runt, (zI, zP))
>   where
>     ReportVerbosity{ .. }
>                                          = dives.dReportVerbosity
>
> renderSong             :: SFRuntime
>                           → (Map InstrumentName GMChoices, Map PercussionSound GMChoices)
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
>     let ks                               = Map.keys ding
>     let (is, ps)                         = (lefts ks, rights ks)
>     let (esI, esP)                       = printChoices choices is ps
>     putStr $ reapEmissions (concatMap snd esI ++ concatMap snd esP)
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let (durS, s)                    = renderSF music runt.zInstrumentMap
>         if switches.normalizingOutput
>           then outFileNorm               (name ++ ".wav") durS s
>           else outFile                   (name ++ ".wav") durS s
>         tsFinish                         ← getZonedTime
>         putStr $ reapEmissions $ emitSongTime durS tsStart tsFinish
>       else
>         putStr $ reapEmissions skipSong
>     return ()
> 
> convertFromMidi        :: FilePath → IO Song
> convertFromMidi path                     = do
>   midi_                                  ← M.importFile path
>   let midi                               = case midi_ of
>                                              Left err → error err
>                                              Right m  → m
>   return $ Song (removeExtension path) (fromMidi midi) Map.empty
>   where
>     removeExtension fp                   =
>       let
>         fpRev                            = reverse fp
>         fpRev'                           = dropWhile (/= '.') fpRev
>         fpRev''                          = drop 1 fpRev'
>       in
>         reverse fpRev''
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
>       let sffileBoot                     = SFFileBoot wFile filename boota IntMap.empty samplea
>       return sffileBoot

The End
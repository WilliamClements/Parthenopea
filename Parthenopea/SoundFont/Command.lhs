> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
June 16, 2025

> module Parthenopea.SoundFont.Command (
>           pCommand
>         , batchProcessor ) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import qualified Data.Bifunctor          as BF
> import Data.Either
> import qualified Data.IntMap.Strict      as IntMap
> import Data.List
> import qualified Data.Map.Strict         as Map
> import Data.Ord ( Down(Down) )
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.Music ( InstrumentName, PercussionSound )
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Render2 ( renderSF2 )
> import Parthenopea.SoundFont.Boot
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.PassageReport
> import Parthenopea.SoundFont.RangesReport
> import Parthenopea.SoundFont.ScanReport ( writeScanReport )
> import Parthenopea.SoundFont.Runtime
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.TournamentReport ( printChoices, writeTournamentReport )
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
>   CM.unless (okDirectives dives)         (error $ unwords[fName, "garbage in Directives", show dives])
>
>   mids                                   ← FP.getDirectoryFilesIgnoreSlow "." ["*.mid", "*.midi"] []
>   sf2s                                   ← FP.getDirectoryFilesIgnoreSlow "." ["*.sf2"] []
>   putStr $ reapEmissions [shareIntention mids sf2s, EndOfLine]
>
>   proceed dives isongs mids sf2s
>
>   timeNow                                ← getZonedTime
>   putStr $ reapEmissions $ closingRemarks timeNow timeThen
>   where
>     fName                                = "batchProcessor"
>
>     shareIntention     :: [FilePath] → [FilePath] → Emission
>     shareIntention ms ss
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
>   let nSongs                             = length songs
>
>   CM.when (20 < nSongs && not (null sf2s)) runUnitTests
>
>   let ding                               = Map.unionsWith combineShreds (map shreds songs)
>   CM.when (dForPassage > 0 && 0 < nSongs) (writePassageReport dives songs)
>   CM.when (dForRanges > 0 && 0 < nSongs) (writeRangesReport dives songs ding)
>
>   rost                                   ← identifyRoster ding songs
>
>   CM.unless (null sf2s)
>             (putStr $ reapEmissions [Unblocked $ unwords ["surveySoundFonts"], EndOfLine])
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFilesBoot                         = VB.fromList extraction
>   if VB.null vFilesBoot
>     then return ()
>     else do
>       putStrLn "extracted!"
>       runt                               ← buildRuntime dives rost vFilesBoot
>       -- here's the heart of the coconut
>       mapM_ (renderSong runt) songs
>   where
>     captureSong (Song name music _)      = do
>       ding                               ← shredMusic music
>       return $ Song name music ding
>     identifyRoster ding songs            = return $ if null songs
>                                                       then allKinds
>                                                       else partitionEithers (Map.keys ding)
>     ReportVerbosity{ .. }               
>                                          = dives.dReportVerbosity
>
> buildRuntime           :: Directives → ([InstrumentName], [PercussionSound]) → VB.Vector SFFileBoot → IO SFRuntime
> buildRuntime dives rost vFilesBoot_      = do
>   putStrLn fName
>   (vFilesBoot, sy)                       ← surveyInstruments dives rost vFilesBoot_
>
>   let rd                                 = sy.sDispositions
>   let insts                              = sy.sPerInstruments
>   let matches                            = sy.sMatches
>
>   (candI, candP)                         ← proposeCandidates dives rost (VB.map zPreZonesBoot vFilesBoot)
>                                                            insts matches
>   let (sortedI, sortedP)                 = BF.bimap
>                                              (Map.map (sortOn (Down . pScore . pArtifactGrade)))
>                                              (Map.map (sortOn (Down . pScore . pArtifactGrade)))
>                                              (candI, candP)
>
>   CM.when (dForTournament > 0)           (writeTournamentReport dives vFilesBoot (sortedI, sortedP))
>   ((wonI, wonP), rd')                    ← establishWinners rost (sortedI, sortedP) rd
>   CM.when (dForScan > 0)                 (writeScanReport dives dForScan vFilesBoot rd')
>   runt                                   ← prepareRuntime dives rost vFilesBoot insts (wonI, wonP)
>   return runt{zWinners = (wonI, wonP)}
>   where
>     fName                                = "buildRuntime"
>
>     ReportVerbosity{ .. }
>                                          = dives.dReportVerbosity
>
> renderSong             :: SFRuntime → Song → IO ()
> renderSong (SFRuntime dives _ choices _ imap) (Song name music shreds)
>                                          = do
>   let sw                                 = dives.synthSwitches
>
>   tsStart                                ← getZonedTime
>   putStr $ reapEmissions [Unblocked $ unwords ["renderSong", name], EndOfLine]
>
>   let ks                                 = Map.keys shreds
>   let (is, ps)                           = (lefts ks, rights ks)
>   let (esI, esP)                         = printChoices choices is ps
>   putStr $ reapEmissions (concatMap snd esI ++ concatMap snd esP)
>     -- render song only if all OK
>   if all fst esI && all fst esP
>     then do
>       let (durS, s)                      = renderSF2 music imap
>       if sw.normalizingOutput
>         then outFileNorm                 (name ++ ".wav") durS s
>         else outFile                     (name ++ ".wav") durS s
>       tsFinish                           ← getZonedTime
>       putStr $ reapEmissions $ emitSongTime durS tsStart tsFinish
>     else
>       putStr $ reapEmissions skipSong
>   return ()
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
>       return $ SFFileBoot 
>                  wFile 
>                  filename 
>                  boota 
>                  IntMap.empty 
>                  samplea

The End
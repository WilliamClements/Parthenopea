> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE OverloadedRecordDot #-}
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
> import Data.Array.Unboxed ( listArray )
> import Data.Either ( lefts, rights )
> import qualified Data.Map                as Map
> import Data.Maybe ( fromMaybe )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, InstrMap )
> import Euterpea.IO.Audio.Types ( Stereo, Clock )
> import Euterpea.IO.MIDI ( fromMidi )
> import Euterpea.Music
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Synthesizer ( normalizingOutput )
> import Parthenopea.SoundFont.Boot ( surveyInstruments )
> import Parthenopea.SoundFont.Runtime
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import qualified System.FilePattern.Directory
>                                          as FP

Implement PCommand ====================================================================================================

> pCommand               :: IO ()
> pCommand                                 = do
>   batchProcessor []
>
> batchProcessor         :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> batchProcessor isongs                    = do
>   mids                                   ← FP.getDirectoryFiles "." ["*.mid", "*.midi"]
>   sf2s                                   ← FP.getDirectoryFiles "." ["*.sf2"]
>   putStrLn (msg mids sf2s)
>   proceed isongs mids sf2s
>   where
>     msg                :: [FilePath] → [FilePath] → String
>     msg ms ss
>       | null ms && null isongs && null ss
>                                          = "no *.mid or *.sf2 files found: nothing to do"
>       | null ss                          = "no *.sf2 files found: proceeding to survey mids"
>       | null ms && null isongs           = "no *.mid files found: proceeding to survey sf2s"
>       | otherwise                        = "proceeding to render mids by sf2s"
>
> proceed                :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → [FilePath] → [FilePath] → IO ()
> proceed isongs mids sf2s                 = do
>   msongs                                 ← mapM convertFromMidi mids
>   let songs                              = isongs ++ msongs
>
>   CM.when (20 < length songs)            runUnitTests
>
>   sffilesp                               ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFile                              = listArray (0, fromIntegral (length sf2s - 1)) sffilesp
>   rost                                   ← qualifyKinds songs
>
>   if null vFile
>     then return ()
>     else do
>       (prerunt, matches, rd)             ← surveyInstruments vFile rost
>
>       writeScanReport prerunt rd
>       (wI, wP)                           ← decideWinners prerunt rost matches 
>       writeTournamentReport prerunt wI wP
>
>       let winners                        = WinningRecord (Map.map head wI) (Map.map head wP)
>       let runt                           = prerunt{zWinningRecord = winners}
>       imap                               ← prepareInstruments runt
>
>       -- here's the heart of the coconut
>       mapM_ (renderSong runt imap) songs
>
> renderSong             :: ∀ p . Clock p ⇒
>                           SFRuntime
>                           → InstrMap (Stereo p)
>                           → (String, DynMap → Music (Pitch, [NoteAttribute]))
>                           → IO ()
> renderSong runt imap (name, song)           =
>   do
>     putStrLn ("renderSong " ++ name)
>     ding                                 ← shredMusic (song Map.empty)
>     let dynMap                           = makeDynMap ding
>     CM.unless (null dynMap)              (putStrLn $ unwords ["dynMap", show dynMap])
>     let ks                               = Map.keys ding.shRanges
>     let (is, ps)                         = (map (\i → fromMaybe i (Map.lookup i dynMap)) (lefts ks), rights ks)
>     let (esI, esP)                       = printChoices runt is ding.shMsgs ps
>     let ex                               = [Unblocked name, EndOfLine] ++ concatMap snd esI ++ concatMap snd esP
>     putStr (reapEmissions ex)
>     -- render song only if all OK
>     if all fst esI && all fst esP
>       then do
>         let path                         = name ++ ".wav"
>         putStr path
>         let (durS,s)                     = renderSF (song dynMap) imap
>         putStrLn (unwords ["-> outFile*", path, show durS])
>         if normalizingOutput
>           then outFileNorm path durS s
>           else outFile     path durS s
>         putStrLn (unwords ["<- outFile*", path, show durS])
>       else
>         putStrLn "skipping..."
>     return ()
> 
> convertFromMidi        :: FilePath → IO (String, DynMap → Music (Pitch, [NoteAttribute]))
> convertFromMidi path                     = do
>   midi_                                  ← M.importFile path
>   let midi                               = case midi_ of
>                                              Left err → error err
>                                              Right m  → m
>   return (removeExtension path, const $ fromMidi midi)
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
> qualifyKinds           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))]
>                           → IO ([InstrumentName], [PercussionSound])
> qualifyKinds songs                       = do
>   mks                                    ← shredSongs songs
>   let isandps                            = Map.keys mks
>   return $ if null songs then allKinds else (lefts isandps, rights isandps)
>
> openSoundFontFile      :: Word → FilePath → IO SFFile
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
>       let sffile                         = SFFile wFile filename boota samplea
>       return sffile

The End
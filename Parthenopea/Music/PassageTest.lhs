> {-# LANGUAGE UnicodeSyntax #-}

PassageTest
William Clements
August 15, 2025

> module Parthenopea.Music.PassageTest ( passageTests ) where
>
> import qualified Data.Vector.Strict      as VB
> import Euterpea.Music
> import Parthenopea.Music.Passage
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.PassageReport (summarizeOnePassage)
>
> passageTests           :: [IO Bool]
> passageTests = [twoNotesSameVelocity
>               , twoNotesDiffVelocity
>               , threeNotesWithRests
>               , fourMarksWork
>               , fourInflectionsWork ]
>
> aMarkings, bMarkings, cMarkings, dMarkings
>                        :: VB.Vector Marking
> aMarkings                                = expandMarkings [Mark SF, Mark SF]
> bMarkings                                = expandMarkings [Inflect PP, Inflect FF]
> cMarkings                                = expandMarkings [Mark SF, Inflect FF, Mark P]
> dMarkings                                = expandMarkings [Mark PPP, Mark P, Mark P, Mark FF]
> eMarkings                                = expandMarkings [Inflect PPP, Inflect P, Inflect P, Inflect FF]
>
> aSnippet, bSnippet, cSnippet
>                        :: Music Pitch
> aSnippet                                 = line [a 4 qn, b 4 qn]
> bSnippet                                 = line [c 3 hn, rest qn, g 3 qn, rest hn, b 3 qn]
> cSnippet                                 = line [c 4 hn, rest qn, d 4 qn, rest hn, b 3 qn, rest sn, a 3 sn]
>
> doEnrich               :: VB.Vector Marking → Music Pitch → VB.Vector MekNote
> doEnrich                                 = enrichPassage defDirectives defBandPart
>                                              
> twoNotesSameVelocity   :: IO Bool
> twoNotesSameVelocity                     = do
>   putStrLn "twoNotesSameVelocity"
>   let meks                               = doEnrich aMarkings aSnippet
>   let summary          :: [Emission]     = summarizeOnePassage meks
>   putStrLn $ reapEmissions summary
>   return True
>
> twoNotesDiffVelocity   :: IO Bool
> twoNotesDiffVelocity                     = do
>   putStrLn "twoNotesDiffVelocity"
>   let meks                               = doEnrich bMarkings aSnippet
>   let summary          :: [Emission]     = summarizeOnePassage meks
>   putStrLn $ reapEmissions summary
>   return True
>
> threeNotesWithRests    :: IO Bool
> threeNotesWithRests                      = do
>   putStrLn "threeNotesWithRests"
>   let meks                               = doEnrich cMarkings bSnippet
>   let summary          :: [Emission]     = summarizeOnePassage meks
>   putStrLn $ reapEmissions summary
>   return True
>
> fourMarksWork          :: IO Bool
> fourMarksWork                            = do
>   putStrLn "fourMarksWork"
>   let meks                               = doEnrich dMarkings cSnippet
>   let summary          :: [Emission]     = summarizeOnePassage meks
>   putStrLn $ reapEmissions summary
>   return True
>
> fourInflectionsWork    :: IO Bool
> fourInflectionsWork                      = do
>   putStrLn "fourInflectionsWork"
>   let meks                               = doEnrich eMarkings cSnippet
>   let summary          :: [Emission]     = summarizeOnePassage meks
>   putStrLn $ reapEmissions summary
>   return True

The End 
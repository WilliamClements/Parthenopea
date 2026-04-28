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
> import Parthenopea.Repro.Chart
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.PassageReport (summarizeOnePassage)
>
> useInflectionsInTests  :: Bool           = True
>
> passageTests           :: [IO Bool]
> twoMarkedNotes, twoInflectedNotes, threeNotesWithRests, fourMarksWork, fourInflectionsWork, tripletsWork
>                        :: IO Bool
> passageTests = [twoMarkedNotes
>               , twoInflectedNotes
>               , threeNotesWithRests
>               , fourMarksWork
>               , fourInflectionsWork
>               , tripletsWork ]
>
> aMarkings, bMarkings, cMarkings, dMarkings, eMarkings, fMarkings
>                        :: VB.Vector Marking
> aMarkings                                = expandMarkings [Mark PP, Mark FF]
> bMarkings                                = expandMarkings [Inflect PP, Inflect FF]
> cMarkings                                = expandMarkings [Mark SF, Inflect FF, Mark P]
> dMarkings                                = expandMarkings [Mark PPP, Mark P, Mark P, Mark FF]
> eMarkings                                = expandMarkings [Inflect PPP, Inflect PP, Inflect P, Inflect FF]
> fMarkings                                = expandMarkings [Inflect SF, SpanN 2, Inflect FF, Inflect P, Inflect FF]
>
> aSnippet, bSnippet, cSnippet, dSnippet
>                        :: Music Pitch
> aSnippet                                 = line [a 4 qn, b 4 qn]
> bSnippet                                 = line [c 3 hn, rest qn, g 3 qn, rest hn, b 3 qn]
> cSnippet                                 = line [c 4 hn, rest qn, d 4 qn, rest hn, b 3 qn, rest sn, a 3 sn]
> dSnippet                                 = line [d 5 qn, d 5 en, e 5 en, t32 [d 5 en, cs 5 en, b 4 en]]
>
> doPassageTest          :: String → VB.Vector Marking → Music Pitch → IO Bool
> doPassageTest tName ms music             = do
>   putStrLn tName
>
>   let meks                               = enrichPassage defDirectives defBandPart ms music
>
>   let summs                              = reapEmissions (summarizeOnePassage meks)
>   chartPoints tName (chartPassage meks)
>   putStrLn summs
>   return True
> 
> twoMarkedNotes                           = doPassageTest "twoMarkedNotes"            aMarkings      aSnippet
> twoInflectedNotes                        = doPassageTest "twoInflectedNotes"         bMarkings      aSnippet
> threeNotesWithRests                      = doPassageTest "threeNotesWithRests"       cMarkings      bSnippet
> fourMarksWork                            = doPassageTest "fourMarksWork"             dMarkings      cSnippet
> fourInflectionsWork                      = doPassageTest "fourInflectionsWork"       eMarkings      cSnippet
> tripletsWork                             = doPassageTest "tripletsWork"              fMarkings      dSnippet

The End 
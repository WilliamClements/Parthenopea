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
> passageTests = [singleIsSingle
>               , doubleIsDouble
>               , fourReMarksWork
>               , fourMarksWork ]
>
> aPrims, cPrims
>                        :: VB.Vector (Primitive Pitch)
> aPrims                                   = VB.fromList [Note wn (Af, 3), Rest hn, Note wn (Bf, 3)]
> cPrims                                   = VB.fromList [Note qn (Gs, 4), Note hn (Bs, 4), Note hn (Cs, 5), Note dhn (A, 4)]
>
> aMarkings, bMarkings, cMarkings, dMarkings
>                        :: VB.Vector Marking
> aMarkings                                = expandMarkings [Inflect PP, Inflect FF]
> bMarkings                                = expandMarkings [Mark PP, Rest1, Mark FF]
> cMarkings                                = expandMarkings [Mark PPP, Mark P, Mark P, Mark FF]
> dMarkings                                = expandMarkings [Inflect PPP, Inflect P, Inflect P, Inflect FF]
>
> singleIsSingle         :: IO Bool
> singleIsSingle                           = do
>   let meks                               = enrichPassage
>                                              defDirectives
>                                              defBandPart
>                                              aMarkings
>                                              -- WOX (line [rest 0, rest 0])
>                                              -- WOX (line [a 4 qn, b 4 qn])
>                                              (line [a 4 qn, b 4 qn])
>   let summary          :: [Emission]     = summarizeOnePassage meks
>   putStrLn $ reapEmissions summary
>   return True
>
> doubleIsDouble         :: IO Bool
> doubleIsDouble                           = do
>   putStrLn "dummy 2"
>   return True
>
> fourReMarksWork        :: IO Bool
> fourReMarksWork                           = do
>   putStrLn "dummy 3"
>   return True
>
> fourMarksWork         :: IO Bool
> fourMarksWork                            = do
>   putStrLn "dummy 4"
>   return True

The End 
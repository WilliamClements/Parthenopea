> {-# LANGUAGE UnicodeSyntax #-}

PassageTest
William Clements
August 15, 2025

> module Parthenopea.Music.PassageTest ( passageTests ) where
>
> import qualified Data.Vector.Strict      as VB
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Passage
>
> passageTests           :: [IO Bool]
> passageTests                             = []
> {-
> passageTests                             = [singleIsSingle
>                                           , doubleIsDouble
>                                           , fourReMarksWork
>                                           , fourMarksWork ]
> -}
>
> aPrims, cPrims
>                        :: VB.Vector (Primitive Pitch)
> aPrims                                   = VB.fromList [Note wn (Af, 3), Rest hn, Note wn (Bf, 3)]
> cPrims                                   = VB.fromList [Note qn (Gs, 4), Note hn (Bs, 4), Note hn (Cs, 5), Note dhn (A, 4)]
>
> aMarkings, bMarkings, cMarkings, dMarkings
>                        :: [Marking]
> aMarkings                                = [Inflect PP, Rest1, Inflect FF]
> bMarkings                                = [Mark PP, Rest1, Mark FF]
> cMarkings                                = [Mark PPP, Mark P, Mark P, Mark FF]
> dMarkings                                = [Inflect PPP, Inflect P, Inflect P, Inflect FF]
>
> testMeks               :: VB.Vector (Primitive Pitch) → [Marking] → VB.Vector MekNote
> testMeks prims markings                  =
>   VB.zipWith4 makeMekNote (VB.fromList [0..3]) prims (expandMarkings markings) VB.empty
>
> singleIsSingle         :: IO Bool
> singleIsSingle                           = do
>   return
>     $ aEqual
>         (formNodeGroups $ testMeks aPrims aMarkings)
>         (VB.fromList [(0,2)], VB.fromList [VB.fromList[0, 2]])
>
> doubleIsDouble         :: IO Bool
> doubleIsDouble                           = do
>   return
>     $ aEqual
>         (formNodeGroups $ testMeks aPrims bMarkings)
>         (VB.fromList [(0,2)], VB.fromList [VB.fromList[0], VB.fromList[2]])
>
> fourReMarksWork        :: IO Bool
> fourReMarksWork                           = do
>   return
>     $ aEqual
>         (formNodeGroups $ testMeks cPrims cMarkings)
>         (VB.fromList [(0,1),(1,2),(2,3)], VB.fromList [  VB.fromList [0]
>                                                        , VB.fromList [1]
>                                                        , VB.fromList [2]
>                                                        , VB.fromList [3]])
>
> fourMarksWork         :: IO Bool
> fourMarksWork                            = do
>   return
>     $ aEqual
>         (formNodeGroups $ testMeks cPrims dMarkings)
>         (VB.fromList [(0,1),(1,2),(2,3)], VB.fromList [ VB.fromList [0, 1, 2, 3]])

The End 
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

PassageTest
William Clements
August 15, 2025

> module Parthenopea.Music.PassageTest ( passageTests ) where
>
> import qualified Data.Vector             as VB
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Passage
>
> passageTests           :: [IO Bool]
> passageTests                             = [singleIsSingle
>                                           , doubleIsDouble
>                                           , fourReMarksWork
>                                           , fourMarksWork ]
>
> aPrims, cPrims
>                        :: VB.Vector (Primitive Pitch)
> aPrims                                   = VB.fromList [Note wn (Af, 3), Rest hn, Note wn (Bf, 3)]
> cPrims                                   = VB.fromList [Note qn (Gs, 4), Note hn (Bs, 4), Note hn (Cs, 5), Note dhn (A, 4)]
>
> aMarkings, bMarkings, cMarkings, dMarkings
>                        :: [Marking]
> aMarkings                                = [Mark PP, Rest1, Mark FF]
> bMarkings                                = [ReMark PP, Rest1, ReMark FF]
> cMarkings                                = [ReMark PPP, ReMark P, ReMark P, ReMark FF]
> dMarkings                                = [Mark PPP, Mark P, Mark P, Mark FF]
>
> testMeks               :: VB.Vector (Primitive Pitch) → [Marking] → VB.Vector MekNote
> testMeks prims markings                  =
>   VB.zipWith3 makeMekNote (VB.fromList [0..3]) prims (expandMarkings markings)
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
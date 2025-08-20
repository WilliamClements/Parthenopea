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
>                                           , doubleIsDouble ]
>
> aPrims, bPrims         :: VB.Vector (Primitive Pitch)
> aPrims                                   = VB.fromList [Note wn (Af, 3), Rest hn, Note wn (Bf, 3)]
> bPrims                                   = VB.fromList [Note wn (Af, 3), Rest hn, Note wn (Bf, 3)]
>
> aMarkings, bMarkings   :: VB.Vector Marking
> aMarkings                                = VB.fromList [Mark PP, Rest1, Mark FF]
> bMarkings                                = VB.fromList [ReMark PP, Rest1, ReMark FF]
>
> testMeks               :: VB.Vector (Primitive Pitch) → VB.Vector Marking → VB.Vector MekNote
> testMeks prims markings                  =
>   VB.zipWith3 makeMekNote (VB.fromList [0..3]) prims (VB.fromList (expandMarkings (VB.toList markings)))
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

The End 
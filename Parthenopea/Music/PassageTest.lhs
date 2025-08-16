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
> passageTests                             = [singleIsSingle {-
>                                           , closedMarkingsLeaveNoNothings -} ]
>
> testMeks               :: VB.Vector MekNote
> testMeks                                 = VB.fromList $ zipWith3 makeMekNote [0..] prims (expandMarkings markings)
>   where
>     prims                                = [Note wn (Af, 3), Rest hn, Note wn (Bf, 3)]
>     markings                             = [Mark PP, Rest1, Mark FF]
>
> singleIsSingle         :: IO Bool
> singleIsSingle                           = do
>   return
>     $ aEqual
>         (map mMarking (VB.toList testMeks))
>         [Mark PP,Rest1,Mark FF]
>
> {-
> closedMarkingsLeaveNoNothings
>                        :: IO Bool
> closedMarkingsLeaveNoNothings            = do
>   return True
>   mapM_ output testMarkingses
>   return $ all allGood testMarkingses
>   where
>     allGood markings                     = vnodesHaveNoNothings $ compileMarkings testMeks markings
>     output vn                            = print $ compileMarkings testMeks vn -}

The End 
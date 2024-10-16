> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE UnicodeSyntax #-}

ParthenopeaTest
William Clements
October 11, 2024

> module ParthenopeaTest where
>
> import Data.Maybe
> import Parthenopea

Testing ===============================================================================================================

> runParthTests                            = runTests parthTests
>
> parthTests             :: [IO Bool]      = [canClaimEntireSpaceWithImpunity
>                                         ,   lastSpaceWins
>                                         ,   overlapsCountedCorrectly
>                                         ,   nonOverlapsCountedCorrectly
>                                         ,   handle3dSpaces
>                                         ,   handle4dSpaces]
>
> canClaimEntireSpaceWithImpunity
>   , lastSpaceWins
>   , overlapsCountedCorrectly
>   , nonOverlapsCountedCorrectly
>   , handle3dSpaces
>   , handle4dSpaces
>                        :: IO Bool
>
> s2x2, s3x2             :: [Int]
> s2x2                                     = [2, 2]
> s3x2                                     = [3, 2]
>
> filln                  :: Int → Maybe (Int, Int)
> filln n                                  = Just (0, n - 1)
>
> pt01, pt02, pt03, pt04, pt05
>                        :: Smashing Int
>
> pt01                                     =
>   smashSubspaces
>     "pt01"
>     s3x2
>     [(101, [filln 3, filln 2])]
>
> canClaimEntireSpaceWithImpunity          = do
>   return $ aEqual True (allCellsEqualTo pt01 (101, 1))
>
> pt02                                     =
>   smashSubspaces
>     "pt02"
>     s3x2
>     [(201, [filln 3, filln 2]), (202, [filln 3, filln 2])]
>
> lastSpaceWins                            = do
>   return $ aEqual True (allCellsEqualTo pt02 (202, 2))
>
> pt03                                     =
>   smashSubspaces
>     "pt03"
>     s3x2
>     [(301, [Just (1, 2), Just (0, 0)]), (302, [Just (1, 1), Just (0, 1)])]
>
> overlapsCountedCorrectly                 = do
>   let aat                                =
>         lookupCellIndex
>           [1, 0]
>           pt03
>   print aat
>   return $ aEqual (snd aat) 2
>
> nonOverlapsCountedCorrectly              = do
>   let aat                                =
>         lookupCellIndex
>           [1, 1]
>           pt03
>   print aat
>   return $ aEqual (snd aat) 1
>
> pt04                                     =
>   smashSubspaces
>     "pt04"
>     [4, 3, 4]
>     [   (401, [Just (0, 0), Just (1, 1), Just (1, 2)])
>       , (402, [Just (1, 3), Just (2, 2), Just (3, 3)])]
>
> handle3dSpaces                           = do
>   let aat                                =
>         lookupCellIndex
>           [0, 1, 1]
>           pt04
>   print aat
>   return $ aEqual (fst aat) 401 
>
> pt05                                     =
>   smashSubspaces
>     "pt05"
>     [2, 2, 3, 3]
>     [   (501, [Just (0, 0), Just (1, 1), Just (1, 2), Just (2, 2)])
>       , (502, [Just (1, 1), Just (0, 1), Just (1, 1), Just (0, 1)])]
>
> handle4dSpaces                           = do
>   let aat                                =
>         lookupCellIndex
>           [1, 0, 1, 0]
>           pt05
>   print aat
>   return $ aEqual (fst aat) 502

The End
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
>                                         ,   firstSpaceWins
>                                         ,   overlapsCountedCorrectly
>                                         ,   nonOverlapsCountedCorrectly
>                                         ,   handle3dSpaces
>                                         ,   handle4dSpaces]
>
> canClaimEntireSpaceWithImpunity
>   , firstSpaceWins
>   , overlapsCountedCorrectly
>   , nonOverlapsCountedCorrectly
>   , handle3dSpaces
>   , handle4dSpaces
>                        :: IO Bool
>
> s2x2                   :: [Int]
> s2x2                                     = [2, 2]
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
>     s2x2
>     [[filln 2, filln 2]]
>
> canClaimEntireSpaceWithImpunity          = do
>   let result                             =
>         lookupCellIndex
>           [0, 0]
>           pt01
>   return $ aEqual True (isJust result)
>
> pt02                                     =
>   smashSubspaces
>     "pt02"
>     s2x2
>     [[filln 2, filln 2], [filln 2, filln 2]]
>
> firstSpaceWins                           = do
>   let aat                                =
>         lookupCellIndex
>           [1, 0]
>           pt02
>   let spacenum                           =
>         case aat of
>           Nothing                        → error "expected non-Nothing"
>           Just (sn, _)                   → sn
>   return $ aEqual 0 spacenum
>
> pt03                                     =
>   smashSubspaces
>     "pt03"
>     s2x2
>     [[Just (0, 1), Just (0, 0)], [Just (0, 1), Just (0, 1)]]
>
> overlapsCountedCorrectly                 = do
>   print pt03
>   let aat                                =
>         lookupCellIndex
>           [1, 0]
>           pt03
>   let count                              =
>         case aat of
>           Nothing                        → Nothing
>           Just (_, cnt)                  → Just cnt
>   return $ aEqual (Just 2) count
>
> nonOverlapsCountedCorrectly              = do
>   let aat                                =
>         lookupCellIndex
>           [0, 1]
>           pt03
>   let count                              =
>         case aat of
>           Nothing                        → Nothing
>           Just (_, cnt)                  → Just cnt
>   return $ aEqual (Just 1) count
>
> pt04                                     =
>   smashSubspaces
>     "pt04"
>     [4, 4, 4]
>     [   [Just (0, 0), Just (1, 1), Just (1, 2)]
>       , [Just (1, 3), Just (2, 3), Just (3, 3)]]
>
> handle3dSpaces                           = do
>   let aat                                =
>         lookupCellIndex
>           [0, 0, 1]
>           pt04
>   let spacenum                           =
>         case aat of
>           Nothing                        → Nothing
>           Just (sn, _)                   → Just sn
>   return $ aEqual Nothing spacenum
>
> pt05                                     =
>   smashSubspaces
>     "pt05"
>     [3, 3, 3, 3]
>     [   [Just (0, 0), Just (1, 1), Just (1, 2), Just (2, 2)]
>       , [Just (1, 2), Just (0, 1), Just (1, 1), Just (0, 1)]]
>
> handle4dSpaces                           = do
>   let aat                                =
>         lookupCellIndex
>           [0, 0, 1, 0]
>           pt05
>   let spacenum                           =
>         case aat of
>           Nothing                        → Nothing
>           Just (sn, _)                   → Just sn
>   return $ aEqual Nothing spacenum

The End
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE BangPatterns #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

ParthenopeaTest
William Clements
October 11, 2024

> module ParthenopeaTest where
>
> import Control.Exception
> import Data.Array.Unboxed
> import Data.Either
> import Data.List ( foldl', sort, nub, sortOn, singleton )
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord
> import qualified Data.Vector.Unboxed     as VU
> import qualified Data.Vector             as VB
> import Modulation
> import ModulationTest
> import Parthenopea

Testing ===============================================================================================================

> runParthTests                            = runTests parthTests
>
> parthTests             :: [IO Bool]      = [canClaimEntireSpaceWithImpunity
>                                         ,   firstSpaceWins
>                                         ,   overlapsCountedCorrectly
>                                         ,   nonOverlapsCountedCorrectly
>                                         ,   handle3dSpaces]
>
> canClaimEntireSpaceWithImpunity
>   , firstSpaceWins
>   , overlapsCountedCorrectly
>   , nonOverlapsCountedCorrectly
>   , handle3dSpaces
>                        :: IO Bool
>
> s2x2                   :: [Int]
> s2x2                                     = [2, 2]
>
> fillit                 :: Maybe (Int, Int)
> fillit                                   = Just (0, 1)
>
> pt01, pt02, pt03, pt04 :: VB.Vector (Maybe (Int, Int))
> pt01                                     =
>   smashSubspaces
>     s2x2
>     (singleton [fillit, fillit])
>
> canClaimEntireSpaceWithImpunity          = do
>   let result                             =
>         lookupCellIndex
>           s2x2 
>           [0, 0]
>           pt01
>   return $ aEqual True (isJust result)
>
> pt02                                     =
>   smashSubspaces
>     s2x2
>     [[fillit, fillit], [fillit, fillit]]
>
> firstSpaceWins                           = do
>   let aat                                =
>         lookupCellIndex
>           s2x2
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
>     s2x2
>     [[Just (0, 1), Just (0, 0)], [Just (0, 1), Just (0, 1)]]
>
> overlapsCountedCorrectly                 = do
>   print pt03
>   let aat                                =
>         lookupCellIndex
>           s2x2
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
>           s2x2
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
>     [4, 4, 4]
>     [   [Just (0, 0), Just (1, 1), Just (1, 2)]
>       , [Just (1, 3), Just (2, 3), Just (3, 3)]]
>
> handle3dSpaces                           = do
>   let aat                                =
>         lookupCellIndex [4, 4, 4] [0, 0, 1] pt04
>   let spacenum                           =
>         case aat of
>           Nothing                        → Nothing
>           Just (sn, _)                   → Just sn
>   return $ aEqual Nothing spacenum

The End
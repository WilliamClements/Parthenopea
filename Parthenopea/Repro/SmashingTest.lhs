> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

SmashingTest
William Clements
October 11, 2024

> module Parthenopea.Repro.SmashingTest where
>
> import Data.List ( singleton )
> import Parthenopea.Debug ( aEqual, runTests )
> import Parthenopea.Repro.Smashing
  
Smashing-related tests ================================================================================================

Includes a four-dimensional example.

> runSmashingTests          :: IO ()
> runSmashingTests                         = runTests smashingTests
>
> smashingTests             :: [IO Bool]
> smashingTests                            = [canClaimEntireSpaceWithImpunity
>                                          ,  canClaimEntireSpaceByDefault
>                                          ,  lastSpaceWins
>                                          ,  nothingNothings
>                                          ,  theWholeIsEqualToTheSumOfItsParts
>                                          ,  spotCheckOverlapCounts
>                                          ,  spotCheck3dSpaceCell
>                                          ,  spotCheck4dSpaceCell
>                                          ,  canCountPartialCover
>                                          ,  canCountMultiples
>                                          ,  workOutNearbyVerticalUncovered
>                                          ,  workOutNearbyHorizontalUncovered
>                                          ,  leftAndRightAddUpToUnity]
>
> filln                  :: Int → Maybe (Int, Int)
> filln n                                  = Just (0, n - 1)
>
> canClaimEntireSpaceWithImpunity, canClaimEntireSpaceByDefault, lastSpaceWins, nothingNothings
>                        :: IO Bool
> theWholeIsEqualToTheSumOfItsParts, spotCheckOverlapCounts, spotCheck3dSpaceCell, spotCheck4dSpaceCell
>                        :: IO Bool
> canCountPartialCover, canCountMultiples, workOutNearbyVerticalUncovered, workOutNearbyHorizontalUncovered
>                        :: IO Bool
> leftAndRightAddUpToUnity
>                        :: IO Bool
>
> canClaimEntireSpaceWithImpunity          = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [3, 2] [(101, [filln 3, filln 2])]
>   return $ aEqual (allCellsEqualTo smashup) (Just (101, 1))
>
> canClaimEntireSpaceByDefault             = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [3, 2] [(101, [Nothing, Nothing])]
>   return $ aEqual (allCellsEqualTo smashup) (Just (101, 1))
>
> lastSpaceWins                            = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [3, 2] [(201, [filln 3, filln 2])
>                                        , (202, [filln 3, filln 2])]
>   return $ aEqual (allCellsEqualTo smashup) (Just (202, 2))
>
> nothingNothings                          = do
>   let smashup          :: Smashing Int   = smashSubspaces "smashup" [3,2] []
>   return $ aEqual smashup.smashStats.countNothings 6
>
> theWholeIsEqualToTheSumOfItsParts        = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [4, 8] [(101, [Nothing, Just (0, 5)])
>                                        , (102, [Nothing, Just (6, 7)])]
>   return $ aEqual smashup.smashStats.countSingles 32
>
> spotCheckOverlapCounts                   = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [3, 2] [(301, [Just (1, 2), Just (0, 0)])
>                                        , (302, [Just (1, 1), Just (0, 1)])]
>   let aat1                               = lookupCellIndex [1, 0] smashup
>   let aat2                               = lookupCellIndex [1, 1] smashup
>   print (aat1, aat2)
>   return $ aEqual (snd aat1) 2 && aEqual (snd aat2) 1
>
> spotCheck3dSpaceCell                     = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [4, 3, 4] [ (401, [Just (0, 0), Just (1, 1), Just (1, 2)])
>                                            , (402, [Just (1, 3), Just (2, 2), Just (3, 3)])]
>   let aat                                = lookupCellIndex [0, 1, 1] smashup
>   print aat
>   return $ aEqual (fst aat) 401 
>
> spotCheck4dSpaceCell                     = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [2, 2, 3, 3] [ (501, [Just (0, 0), Just (1, 1), Just (1, 2), Just (2, 2)])
>                                               , (502, [Just (1, 1), Just (0, 1), Just (1, 1), Just (0, 1)])]
>   let aat                                = lookupCellIndex [1, 0, 1, 0] smashup
>   print aat
>   return $ aEqual (fst aat) 502
>
> canCountPartialCover                     = do
>   -- 0 1 1
>   -- 0 1 1
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [3, 2] [(101, [Just (1, 2), Nothing])]
>   return $ aEqual smashup.smashStats.countSingles 4
>
> canCountMultiples                        = do
>   -- 0 1 1   +   0 0 0    ==   0 1 1
>   -- 0 1 1   +   1 1 1    ==   1 2 2
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [3, 2] [(101, [Just (1, 2), Nothing])
>                                        , (102, [Nothing, Just (1,1)])]
>   return $ aEqual smashup.smashStats.countMultiples 2
>
> workOutNearbyVerticalUncovered           = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [4, 8]  [(101, [Nothing, Just (2, 4)])
>                                         , (102, [Nothing, Just (5, 6)])]
>   let southern                           = [1, 3]
>   let northern                           = [1, 7]
>   let aat                                = lookupCellIndex southern smashup
>   let bat                                = lookupCellIndex northern smashup
>   return $ aEqual (aat, bat) ((101, 1), (102, 1))
>
> workOutNearbyHorizontalUncovered         = do
>   let smashup          :: Smashing Int   =
>         smashSubspaces "smashup" [8, 4]  [(101, [Just (2, 3), Nothing])
>                                         , (102, [Just (6, 7), Nothing])]
>   let western                            = [3, 1]
>   let eastern                            = [7, 1]
>   let aat                                = lookupCellIndex western smashup
>   let bat                                = lookupCellIndex eastern smashup
>   return $ aEqual (aat, bat) ((101, 1), (102, 1))
>
> leftAndRightAddUpToUnity                 = do
>   let dims                               = [2, 4, 7]
>
>   let space1                             = singleton (101, [Just (0, 0), Nothing, Just (0, 6)])
>   let space2                             = singleton (102, [Just (1, 1), Nothing, Just (0, 6)])
>   let smashup1         :: Smashing Int   = smashSubspaces "smashup1" dims space1
>   let smashup2         :: Smashing Int   = smashSubspaces "smashup2" dims space2
>   let smashup          :: Smashing Int   = smashSubspaces "smashup"  dims (space1 ++ space2)
>
>   let count1                             = product (tail dims)
>   let count2                             = product (tail dims)
>   let count                              = product dims
>   let checkStats1                        = SmashStats count1 count1   0
>   let checkStats2                        = SmashStats count2 count2   0
>   let checkStats                         = SmashStats 0      count    0
>
>   let check1                             = aEqual smashup1.smashStats checkStats1
>   let check2                             = aEqual smashup2.smashStats checkStats2
>   let check                              = aEqual smashup.smashStats  checkStats
>   let checks                             = check1 && check2 && check
>
>   let coords1                            = [0, 2, 5]
>   let coords2                            = [1, 1, 3]
>   let spot1                              = aEqual (lookupCellIndex coords1 smashup) (101, 1)
>   let spot2                              = aEqual (lookupCellIndex coords2 smashup) (102, 1)
>   let spots                              = spot1 && spot2
>
>   return $ checks && spots

The End
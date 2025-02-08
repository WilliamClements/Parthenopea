> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

BootTest
William Clements
January 7, 2025

> module BootTest where
>
> import SoundFont
> import Parthenopea.Debug (runTests)
  
Testing ===============================================================================================================

> runBootTests           :: IO ()
> runBootTests                             = runTests bootTests
>
> nonFatalScanIsNotFatal, fatalScanIsFatal, rescuedScanIsNotFatal, noNewsIsGoodNews1, noNewsIsGoodNews2
>                        :: IO Bool
> correctlyJudgesDispoFatal, correctlyJudgesDispoNonFatal, orderIndependence1, orderIndependence2
>                        :: IO Bool
> bootTests              :: [IO Bool]
> bootTests                                = [  nonFatalScanIsNotFatal
>                                             , fatalScanIsFatal
>                                             , rescuedScanIsNotFatal
>                                             , noNewsIsGoodNews1
>                                             , noNewsIsGoodNews2
>                                             , correctlyJudgesDispoFatal
>                                             , correctlyJudgesDispoNonFatal
>                                             , orderIndependence1
>                                             , orderIndependence2 ]
>
> nonFatalScanIsNotFatal                   = do
>   let ss                                 = accepted (PerGMKey 0 0 Nothing) Ok
>   return $ not $ dead ss
>
> fatalScanIsFatal                         = do
>   let ss                                 = violated (PerGMKey 0 0 Nothing) NoZones
>   return $ dead ss
>
> rescuedScanIsNotFatal                    = do
>   let fName                              = "rescuedScanIsNotFatal"
>   let viol                               = Scan Violated NoZones fName "test"
>   let resc                               = Scan Rescued  NoZones fName "test"
>   let ss                                 = [viol, resc]
>   return $ not $ dead ss
>
> noNewsIsGoodNews1                        = do
>   let pergm                              = PerGMKey 0 0 Nothing
>   return $ not $ deadrd pergm virginrd
>
> noNewsIsGoodNews2                        = do
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [] virginrd
>   return $ not $ deadrd pergm rd
>
> correctlyJudgesDispoFatal               = do
>   let fName                              = "fatalrdCorrectlyJudgesFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Violated NoZones fName "test"] virginrd
>   return $ deadrd pergm rd
>
> correctlyJudgesDispoNonFatal            = do
>   let fName                              = "fatalrdCorrectlyJudgesNonFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Accepted Ok fName "test"] virginrd
>   return $ not $ deadrd pergm rd
>
> orderIndependence1                       = do
>   let fName                              = "orderIndependence1"
>   let order1                             = [Scan Accepted Ok      fName "test",  Scan Violated NoZones fName "test"]
>   let order2                             = [Scan Violated NoZones fName "test",  Scan Accepted Ok fName "test"]
>   return $ dead order1 && dead order2
>
> orderIndependence2                       = do
>   let fName                              = "orderIndependence2"
>   let order1                             = [Scan Violated NoZones fName "test",  Scan Rescued NoZones fName "test"]
>   let order2                             = [Scan Rescued NoZones fName "test",   Scan Violated NoZones fName "test"]
>   return $ not (dead order1) && not (dead order2)

The End
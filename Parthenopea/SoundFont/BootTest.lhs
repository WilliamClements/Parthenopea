> {-# LANGUAGE UnicodeSyntax #-}

BootTest
William Clements
January 7, 2025

> module Parthenopea.SoundFont.BootTest where
>
> import Parthenopea.SoundFont.SFSpec

Boot-related tests ====================================================================================================

> nonFatalScanIsNotFatal, fatalScanIsFatal, rescuedScanIsNotFatal, noNewsIsGoodNews1, noNewsIsGoodNews2
>                        :: IO Bool
> correctlyJudgesDispoFatal, correctlyJudgesDispoNonFatal, orderIndependence1, orderIndependence2
>                        :: IO Bool
>
> bootTests              :: [IO Bool]
> bootTests                                = [  nonFatalScanIsNotFatal
>                                             , fatalScanIsFatal
>                                             , rescuedScanIsNotFatal
>                                             , noNewsIsGoodNews1
>                                             , noNewsIsGoodNews2
>                                             , correctlyJudgesDispoFatal
>                                             , correctlyJudgesDispoNonFatal
>                                             , orderIndependence1
>                                             , orderIndependence2]
>
> nonFatalScanIsNotFatal                   = do
>   let ss                                 = [Scan Accepted Ok "nonFatalScanIsNotFatal" noClue]
>   return $ not $ dead ss
>
> fatalScanIsFatal                         = do
>   let ss                                 = [Scan Violated NoZones "fatalScanIsFatal" noClue]
>   return $ dead ss
>
> rescuedScanIsNotFatal                    = do
>   let fName                              = "rescuedScanIsNotFatal"
>   let viol                               = Scan Violated NoZones fName noClue
>   let resc                               = Scan Rescued  NoZones fName noClue
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
>   let fName                              = "correctlyJudgesDispoFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Violated NoZones fName noClue] virginrd
>   return $ deadrd pergm rd
>
> correctlyJudgesDispoNonFatal            = do
>   let fName                              = "correctlyJudgesDispoNonFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Accepted Ok fName noClue] virginrd
>   return $ not $ deadrd pergm rd
>
> orderIndependence1                       = do
>   let fName                              = "orderIndependence1"
>   let order1                             = [Scan Accepted Ok      fName noClue,  Scan Violated NoZones fName noClue]
>   let order2                             = [Scan Violated NoZones fName noClue,  Scan Accepted Ok fName noClue]
>   return $ dead order1 && dead order2
>
> orderIndependence2                       = do
>   let fName                              = "orderIndependence2"
>   let order1                             = [Scan Violated NoZones fName noClue,  Scan Rescued NoZones fName noClue]
>   let order2                             = [Scan Rescued NoZones fName noClue,   Scan Violated NoZones fName noClue]
>   return $ not (dead order1) && not (dead order2)

The End
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

SoundFontTest
William Clements
January 7, 2025

> module SoundFontTest where
>
> import Data.Maybe
> import Parthenopea
> import SoundFont
  
Testing ===============================================================================================================

> runBootTests                             = runTests bootTests
>
> bootTests              :: [IO Bool]      = [  nonFatalScanIsNotFatal
>                                             , fatalScanIsFatal
>                                             , rescuedScanIsNotFatal
>                                             , noNewsIsGoodNews1
>                                             , noNewsIsGoodNews2
>                                             , fatalrdCorrectlyJudgesFatal
>                                             , fatalrdCorrectlyJudgesNonFatal
>                                             , orderIndependence1
>                                             , orderIndependence2 ]
>
> nonFatalScanIsNotFatal                   = do
>   let fName                              = "nonFatalScanIsNotFatal"
>   let ss                                 = accept Ok fName "test"
>   return $ not (fatalss ss)
>
> fatalScanIsFatal                         = do
>   let fName                              = "fatalScanIsFatal"
>   let ss                                 = violate NoZones fName "test"
>   return $ fatalss ss
>
> rescuedScanIsNotFatal                    = do
>   let fName                              = "rescuedScanIsNotFatal"
>   let viol                               = Scan Violation NoZones fName "test"
>   let resc                               = Scan Rescued   NoZones fName "test"
>   let ss                                 = [viol, resc]
>   return $ not (fatalss ss)
>
> noNewsIsGoodNews1                        = do
>   let fName                              = "noNewsIsGoodNews1"
>   let pergm                              = PerGMKey 0 0 Nothing
>   return $ not (fatalrd virginrd pergm)
>
> noNewsIsGoodNews2                        = do
>   let fName                              = "noNewsIsGoodNews2"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [] virginrd
>   return $ not (fatalrd rd pergm)
>
> fatalrdCorrectlyJudgesFatal              = do
>   let fName                              = "fatalrdCorrectlyJudgesFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Violation NoZones fName "test"] virginrd
>   return $ fatalrd rd pergm
>
> fatalrdCorrectlyJudgesNonFatal           = do
>   let fName                              = "fatalrdCorrectlyJudgesNonFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Accepted Ok fName "test"] virginrd
>   return $ not (fatalrd rd pergm)
>
> orderIndependence1                       = do
>   let fName                              = "orderIndependence1"
>   let order1                             = [Scan Accepted Ok fName "test",       Scan Violation NoZones fName "test"]
>   let order2                             = [Scan Violation NoZones fName "test", Scan Accepted Ok fName "test"]
>   return $ fatalss order1 && fatalss order2
>
> orderIndependence2                       = do
>   let fName                              = "orderIndependence2"
>   let order1                             = [Scan Violation NoZones fName "test", Scan Rescued NoZones fName "test"]
>   let order2                             = [Scan Rescued NoZones fName "test",   Scan Violation NoZones fName "test"]
>   return $ not (fatalss order1) && not (fatalss order2)

The End
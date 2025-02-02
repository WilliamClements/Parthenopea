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
> import Parthenopea (runTests)
  
Testing ===============================================================================================================

> runBootTests           :: IO ()
> runBootTests                             = runTests bootTests
>
> nonFatalScanIsNotFatal, fatalScanIsFatal, rescuedScanIsNotFatal, noNewsIsGoodNews1, noNewsIsGoodNews2
>                        :: IO Bool
> fatalrdCorrectlyJudgesFatal, fatalrdCorrectlyJudgesNonFatal, orderIndependence1, orderIndependence2
>                        :: IO Bool
> bootTests              :: [IO Bool]
> bootTests                                = [  nonFatalScanIsNotFatal
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
>   let sa                                 = ScanAlts (PerGMKey 0 0 Nothing) fName []
>   let ss                                 = accepted sa Ok "test"
>   return $ not (cancels [Accepted, NoChange] ss)
>
> fatalScanIsFatal                         = do
>   let fName                              = "fatalScanIsFatal"
>   let sa                                 = ScanAlts (PerGMKey 0 0 Nothing) fName []
>   let ss                                 = violated sa NoZones "test"
>   return $ cancels [Accepted, NoChange] ss
>
> rescuedScanIsNotFatal                    = do
>   let fName                              = "rescuedScanIsNotFatal"
>   let sa                                 = ScanAlts (PerGMKey 0 0 Nothing) fName []
>   let viol                               = Scan Violated NoZones sa.saFromName "test"
>   let resc                               = Scan Rescued  NoZones sa.saFromName "test"
>   let ss                                 = [viol, resc]
>   return $ not (cancels [Accepted, NoChange] ss)
>
> noNewsIsGoodNews1                        = do
>   let pergm                              = PerGMKey 0 0 Nothing
>   return $ not (fatalrd [Accepted, NoChange] virginrd pergm)
>
> noNewsIsGoodNews2                        = do
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [] virginrd
>   return $ not (fatalrd [Accepted, NoChange] rd pergm)
>
> fatalrdCorrectlyJudgesFatal              = do
>   let fName                              = "fatalrdCorrectlyJudgesFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Violated NoZones fName "test"] virginrd
>   return $ fatalrd [Accepted, NoChange] rd pergm
>
> fatalrdCorrectlyJudgesNonFatal           = do
>   let fName                              = "fatalrdCorrectlyJudgesNonFatal"
>   let pergm                              = PerGMKey 0 0 Nothing
>   let rd                                 = dispose pergm [Scan Accepted Ok fName "test"] virginrd
>   return $ not (fatalrd [Accepted, NoChange] rd pergm)
>
> orderIndependence1                       = do
>   let fName                              = "orderIndependence1"
>   let order1                             = [Scan Accepted Ok fName "test",       Scan Violated NoZones fName "test"]
>   let order2                             = [Scan Violated NoZones fName "test",  Scan Accepted Ok fName "test"]
>   return $ cancels [Accepted, NoChange] order1 && cancels [Accepted, NoChange] order2
>
> orderIndependence2                       = do
>   let fName                              = "orderIndependence2"
>   let order1                             = [Scan Violated NoZones fName "test",  Scan Rescued NoZones fName "test"]
>   let order2                             = [Scan Rescued NoZones fName "test",   Scan Violated NoZones fName "test"]
>   return $ not (cancels [Accepted, NoChange] order1) && not (cancels [Accepted, NoChange] order2)

The End
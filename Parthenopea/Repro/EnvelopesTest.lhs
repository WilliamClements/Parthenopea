> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

EnvelopesTest
William Clements
June 22, 2024

> module Parthenopea.Repro.EnvelopesTest where
>
> import Parthenopea.Debug
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Modulation
>
> ctrRateWorksForDefaultEnvelope
>  , audRateWorksForDefaultEnvelope
>  , ctrRateWorksForOddEnvelope
>  , audRateWorksForOddEnvelope
>                        :: IO Bool
>
> envelopesTests        :: [IO Bool]
> envelopesTests                           = [ ctrRateWorksForDefaultEnvelope
>                                            , audRateWorksForDefaultEnvelope
>                                            , ctrRateWorksForOddEnvelope
>                                            , audRateWorksForOddEnvelope]
>
> runEnvelopesTests     :: IO ()
> runEnvelopesTests                        = runTests envelopesTests
>
> ctrRateWorksForDefaultEnvelope           = do
>   print r
>   print segs
>   chartDiscreteSig nPoints dsig "ctrRateWorks"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de1Envelope
>     dsig                                 = deJust "ctrRateWorksForDefaultEnvelope" $ vetAsDiscreteSig ctrRate r segs
>     nPoints            :: Int            = round $ ctrRate * defTimeFrame.tfSecsScored
>
> audRateWorksForDefaultEnvelope           = do
>   print r
>   print segs
>   chartDiscreteSig nPoints dsig "audRateWorks"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de1Envelope
>     dsig                                 = deJust "audRateWorksForDefaultEnvelope" $ vetAsDiscreteSig audRate r segs
>     nPoints            :: Int            = round $ audRate * defTimeFrame.tfSecsScored
>
> ctrRateWorksForOddEnvelope           = do
>   print r
>   print segs
>   print dsig
>   chartDiscreteSig nPoints dsig "ctrOdd"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de2Envelope
>     dsig                                 = deJust "ctrRateWorksForOddEnvelope" $ vetAsDiscreteSig ctrRate r segs
>     nPoints            :: Int            = round $ ctrRate * defTimeFrame.tfSecsScored
>
> audRateWorksForOddEnvelope           = do
>   print r
>   print segs
>   print dsig
>   chartDiscreteSig nPoints dsig "audOdd"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de2Envelope
>     dsig                                 = deJust "audRateWorksForOddEnvelope" $ vetAsDiscreteSig audRate r segs
>     nPoints            :: Int            = round $ audRate * defTimeFrame.tfSecsScored
>
> defTimeFrame :: TimeFrame
> defTimeFrame                             =
>   TimeFrame 1 (1/2) (1/2) False
>
> de1Envelope, de2Envelope
>                        :: FEnvelope
> de1Envelope                              = 
>   FEnvelope
>     Nothing
>     1.0
>     Nothing
>     minDeltaT
>     (2*minUseful)
>     (2*minUseful)
>     (2*minUseful)
>     (2*minUseful)
> de2Envelope                              = 
>   FEnvelope
>     Nothing
>     0.5
>     Nothing
>     minDeltaT
>     minUseful
>     0.2
>     10.0
>     0.2

The End
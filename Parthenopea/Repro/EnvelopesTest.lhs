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
> ctrRateWorksForde1Envelope
>   , audRateWorksForde1Envelope
>   , ctrRateWorksForde2Envelope
>   , audRateWorksForde2Envelope
>   , ctrRateWorksForde3Envelope
>   , audRateWorksForde3Envelope
>                        :: IO Bool
>
> envelopesTests        :: [IO Bool]
> envelopesTests                           = [ ctrRateWorksForde1Envelope
>                                            , audRateWorksForde1Envelope
>                                            , ctrRateWorksForde2Envelope
>                                            , audRateWorksForde2Envelope
>                                            , ctrRateWorksForde3Envelope
>                                            , audRateWorksForde3Envelope]
>
> runEnvelopesTests     :: IO ()
> runEnvelopesTests                        = runTests envelopesTests
>
> ctrRateWorksForde1Envelope               = do
>   print r
>   print segs
>   chartDiscreteSig nPoints dsig "ctrRateWorks"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de1Envelope
>     dsig                                 = deJust "ctrRateWorksForDefaultEnvelope" $ vetAsDiscreteSig ctrRate r segs
>     nPoints            :: Int            = round $ ctrRate * defTimeFrame.tfSecsScored
>
> audRateWorksForde1Envelope               = do
>   print r
>   print segs
>   chartDiscreteSig nPoints dsig "audRateWorks"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de1Envelope
>     dsig                                 = deJust "audRateWorksForDefaultEnvelope" $ vetAsDiscreteSig audRate r segs
>     nPoints            :: Int            = round $ audRate * defTimeFrame.tfSecsScored
>
> ctrRateWorksForde2Envelope               = do
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
> audRateWorksForde2Envelope               = do
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
> ctrRateWorksForde3Envelope               = do
>   print r
>   print segs
>   print dsig
>   chartDiscreteSig nPoints dsig "ctrde3"
>   return True 
>   where
>     (r, segs)                            = proposeSegments de3TimeFrame de3Envelope
>     dsig                                 = deJust "ctrRateWorksForOddEnvelope" $ vetAsDiscreteSig audRate r segs
>     nPoints            :: Int            = round $ ctrRate * defTimeFrame.tfSecsScored
>
> audRateWorksForde3Envelope               = do
>   print r
>   print segs
>   print dsig
>   chartDiscreteSig nPoints dsig "audde3"
>   return True 
>   where
>     (r, segs)                            = proposeSegments de3TimeFrame de3Envelope
>     dsig                                 = deJust "audRateWorksForOddEnvelope" $ vetAsDiscreteSig audRate r segs
>     nPoints            :: Int            = round $ audRate * defTimeFrame.tfSecsScored
>
> defTimeFrame :: TimeFrame
> defTimeFrame                             =
>   TimeFrame 1 (1/2) (1/2) False
>
> de1Envelope, de2Envelope, de3Envelope
>                        :: FEnvelope
> de3TimeFrame           :: TimeFrame
>
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
> de3Envelope                              =
>   FEnvelope
>     Nothing
>     0.09
>     Nothing
>     minDeltaT
>     minDeltaT
>     minDeltaT
>     0.8
>     minDeltaT
> de3TimeFrame                             =
>   TimeFrame
>     1.2
>     4.5
>     4.5
>     True

The End
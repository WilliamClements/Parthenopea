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
>                        :: IO Bool
>
> envelopesTests        :: [IO Bool]
> envelopesTests                           = [ ctrRateWorksForDefaultEnvelope
>                                            , audRateWorksForDefaultEnvelope]
>
> runEnvelopesTests     :: IO ()
> runEnvelopesTests                        = runTests envelopesTests
>
> ctrRateWorksForDefaultEnvelope           = do
>   chartDiscreteSig dsig "ctrRateWorks"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de1Envelope
>     dsig                                 = deJust "ctrRateWorksForDefaultEnvelope" $ vetAsDiscreteSig ctrRate r segs
>
> audRateWorksForDefaultEnvelope           = do
>   chartDiscreteSig dsig "audRateWorks"
>   return True 
>   where
>     (r, segs)                            = proposeSegments defTimeFrame de1Envelope
>     dsig                                 = deJust "audRateWorksForDefaultEnvelope" $ vetAsDiscreteSig audRate r segs
>
> defTimeFrame :: TimeFrame
> defTimeFrame                             =
>   TimeFrame 1 (1/2) (1/2) False
>
> de1Envelope            :: FEnvelope
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

The End
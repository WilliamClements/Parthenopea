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
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.Types ( AudRate, CtrRate, rate)
> import Parthenopea.Debug
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Modulation
>
> audRateWorksForDefaultEnvelope
>   , ctrRateWorksForDefaultEnvelope
>                        :: IO Bool
>
> envelopesTests        :: [IO Bool]
> envelopesTests                           = [ audRateWorksForDefaultEnvelope
>                                            , ctrRateWorksForDefaultEnvelope]
>
> runEnvelopesTests     :: IO ()
> runEnvelopesTests                        = runTests envelopesTests
>
> audRateWorksForDefaultEnvelope           = do
>   -- chartDiscreteSig VU.empty "mommy"
>   return True 
>   where
>     (r, segs)                            = computeSegments defTimeFrame de1Envelope
>     dsig                                 = vetEnvelope r segs
>
> ctrRateWorksForDefaultEnvelope           = do
>   -- chartDiscreteSig dsig "poppy"
>   return True 
>   where
>     (r, segs)                            = computeSegments defTimeFrame de1Envelope
>     dsig                                 = vetEnvelope r segs
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
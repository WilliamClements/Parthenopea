> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

EnvelopesTest
William Clements
June 22, 2024

> module Parthenopea.Repro.EnvelopesTest ( envelopesTests ) where
>
> import Data.Foldable
> import Parthenopea.Debug
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Modulation

Envelopes-related tests ===============================================================================================

> ctrRateWorksForde1Envelope
>   , audRateWorksForde1Envelope
>   , ctrRateWorksForde2Envelope
>   , audRateWorksForde2Envelope
>   , ctrRateWorksForLongNoteEnvelope
>   , audRateWorksForShortNoteEnvelope
>   , ctrRateWorksForShortNoteEnvelope
>                        :: IO Bool
>
> envelopesTests        :: [IO Bool]
> envelopesTests                           = [ ctrRateWorksForde1Envelope
>                                            , audRateWorksForde1Envelope
>                                            , ctrRateWorksForde2Envelope
>                                            , audRateWorksForde2Envelope
>                                            , ctrRateWorksForLongNoteEnvelope
>                                            , audRateWorksForShortNoteEnvelope
>                                            , ctrRateWorksForShortNoteEnvelope]
>
> munch                 :: String → TimeFrame → FEnvelope → Double → IO Bool
> munch tag tf fe clockRate                = do
>   print segs
>   print secs
>   case mdsig of
>     Nothing                              → return False
>     Just dsig                            →
>       do
>         chartDiscreteSig clockRate nPoints dsig tag
>         return True
>   where
>     (r, segs)                            = proposeSegments tf fe
>     ee                                   = deJust "munch" r.fExtras
>     secs                                 = foldl' (+) (ee.eePostT - 1) segs.sDeltaTs
>     
>     mdsig                                = vetAsDiscreteSig clockRate r segs
>     nPoints            :: Int            = round $ clockRate * (tf.tfSecsScored + 0.5)
>
> ctrRateWorksForde1Envelope               = munch "ctrde1" defTimeFrame de1Envelope ctrRate
>
> audRateWorksForde1Envelope               = munch "audde1" defTimeFrame de1Envelope audRate
>
> ctrRateWorksForde2Envelope               = munch "ctrde2" defTimeFrame de2Envelope ctrRate
>
> audRateWorksForde2Envelope               = munch "audde2" defTimeFrame de2Envelope audRate
>
> ctrRateWorksForLongNoteEnvelope          = munch "ctrLongNote" eLongNoteTimeFrame eLongNoteEnvelope ctrRate
>
> audRateWorksForShortNoteEnvelope         = munch "audShortNote" eShortNoteTimeFrame eShortNoteEnvelope audRate
>
> ctrRateWorksForShortNoteEnvelope         = munch "ctrShortNote" eShortNoteTimeFrame eShortNoteEnvelope ctrRate
>
> defTimeFrame :: TimeFrame
> defTimeFrame                             =
>   TimeFrame 1 (1/2) (1/2) False
>
> de1Envelope, de2Envelope, eLongNoteEnvelope, eShortNoteEnvelope
>                        :: FEnvelope
> eLongNoteTimeFrame, eShortNoteTimeFrame
>                        :: TimeFrame
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
> eLongNoteEnvelope                        =
>   FEnvelope
>     Nothing
>     0.09
>     Nothing
>     minDeltaT
>     minDeltaT
>     minDeltaT
>     0.8
>     minDeltaT
> eLongNoteTimeFrame                       =
>   TimeFrame
>     1.2
>     4.5
>     4.5
>     True
> eShortNoteEnvelope                       =
>   FEnvelope
>     Nothing
>     1
>     Nothing
>     minDeltaT
>     0.05
>     minDeltaT
>     minDeltaT
>     minDeltaT
> eShortNoteTimeFrame                      =
>   TimeFrame
>     1.2
>     0.02
>     0.02
>     False

The End
EnvelopesTest
William Clements
June 22, 2024

> module Parthenopea.Repro.EnvelopesTest ( envelopesTests ) where
>
> import Parthenopea.Repro.Envelopes
> import Parthenopea.SoundFont.Utility

Envelopes-related tests ===============================================================================================

> testDe1Envelope
>   , testDe2Envelope
>   , testLongNoteEnvelope
>   , testShortNoteEnvelope
>                        :: IO Bool
>
> envelopesTests         :: [IO Bool]
> envelopesTests                           = [ testDe1Envelope
>                                            , testDe2Envelope
>                                            , testLongNoteEnvelope
>                                            , testShortNoteEnvelope]
>
> testDe1Envelope                          = chartEnvelope "slwde1" defTimeFrame de1Envelope slwRate
>
> testDe2Envelope                          = chartEnvelope "slwde2" defTimeFrame de2Envelope slwRate
>
> testLongNoteEnvelope                     = chartEnvelope "ctrLongNote" eLongNoteTimeFrame eLongNoteEnvelope ctrRate
>
> testShortNoteEnvelope                    = chartEnvelope "slwShortNote" eShortNoteTimeFrame eShortNoteEnvelope slwRate
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
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

Utility
William Clements
October 8, 2025

> module Parthenopea.SoundFont.Utility where
>
> import Euterpea.IO.MIDI.GeneralMidi ( )
> import Euterpea.Music ( AbsPitch, InstrumentName, PercussionSound, Volume )
> import Parthenopea.Debug
>
> type GMKind                              = Either InstrumentName PercussionSound
> type KeyNumber                           = AbsPitch
> type Velocity                            = Volume
>
> qMidiSize128           :: Word
> qMidiSize128                             = 128
>
> accommodate            :: Ord n ⇒ (n, n) → n → (n, n)
> accommodate (xmin, xmax) newx            = (min xmin newx, max xmax newx)
>
> clip                   :: Ord n ⇒ (n, n) → n → n
> clip (lower, upper) val                  = min upper (max lower val)
>
> roundBy                :: Double → Double → Double
> roundBy p10 x                            = fromIntegral rnd / p10
>   where
>     rnd                :: Int
>     rnd                                  = round (p10 * x)
>
> data NoteOn                              =
>   NoteOn {
>     noteOnVel          :: Velocity
>   , noteOnKey          :: KeyNumber} deriving (Eq, Ord, Show)
> carefulNoteOn          :: Velocity → AbsPitch → NoteOn
> carefulNoteOn volIn pchIn                = NoteOn volOut pchOut
>   where
>     fName                                = "carefulNoteOn"
>
>     volOut                               = safeClip volIn
>     pchOut                               = safeClip pchIn
>
>     safeClip x                           =
>       profess (x == clip (0, 127) x) (unwords [fName, "out of bounds", show (volIn, pchIn)]) x
> noonAsCoords           :: NoteOn → ([Word], [Word])
> noonAsCoords noon                        =
>   (  [fromIntegral noon.noteOnKey, fromIntegral noon.noteOnVel, 0]
>    , [fromIntegral noon.noteOnKey, fromIntegral noon.noteOnVel, 1])

The End
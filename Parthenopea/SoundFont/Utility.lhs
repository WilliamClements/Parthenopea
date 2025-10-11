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
> import Data.Maybe
> import Data.Time
> import Data.Time.Clock.POSIX
> import Euterpea.IO.MIDI.GeneralMidi ( )
> import Euterpea.Music ( AbsPitch, InstrumentName, PercussionSound, Volume )
>
> type GMKind                              = Either InstrumentName PercussionSound
> type KeyNumber                           = AbsPitch
> type Velocity                            = Volume
>
> qMidiSize128           :: Word
> qMidiSize128                             = 128
>
> profess                :: Bool → String → a → a
> profess assertion msg something          = if not assertion
>                                              then error (unwords ["Failed assertion --", msg])
>                                              else something
>
> deJust                 :: ∀ a. String → Maybe a → a
> deJust tag item                          = profess (isJust item) (unwords["expected Just for", tag]) (fromJust item)
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

-- Function to format NominalDiffTime into HH:MM:SS

> formatDiffTime         :: ZonedTime → ZonedTime → String
> formatDiffTime ltNow ltThen              =
>   let
>     utThen                               = zonedTimeToUTC ltThen
>     utNow                                = zonedTimeToUTC ltNow
>     utDiff                               = diffUTCTime utNow utThen
>   in
>     formatNominalDiffTime utDiff
>   where
>     formatNominalDiffTime diff           = formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime diff)

The End
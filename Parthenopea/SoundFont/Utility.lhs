> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

Utility
William Clements
October 8, 2025

> module Parthenopea.SoundFont.Utility where
>
> import Data.Array.Unboxed
> import Data.Foldable
> import Data.Graph (Graph)
> import Data.Maybe
> import Data.Ratio ( approxRational )
> import Data.Time
> import Data.Time.Clock.POSIX
> import Euterpea.Music ( AbsPitch, Dur, InstrumentName, PercussionSound, Volume )
>
> type GMKind                              = Either InstrumentName PercussionSound
> type KeyNumber                           = AbsPitch
> type Velocity                            = Volume
> type Node                                = Int
>
> qMidiWord128           :: Word
> qMidiWord128                             = 128
> qMidiInt128            :: Int
> qMidiInt128                              = 128
> qMidiDouble128         :: Double
> qMidiDouble128                           = 128

error wrappers ========================================================================================================

> profess                :: Bool → String → a → a
> profess assertion msg something          = if not assertion
>                                              then error (unwords ["Failed assertion --", msg])
>                                              else something
>
> deJust                 :: ∀ a. String → Maybe a → a
> deJust tag item                          = profess (isJust item) (unwords["expected Just for", tag]) (fromJust item)

ranges/numerics =======================================================================================================

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
> inARange               :: (Ord a) ⇒ (a, a) → a → Bool
> inARange (m, n) v                        = m <= v && v <= n
>
> inZRange               :: (Ix a, Num a) ⇒ a → a → Bool
> inZRange x y                             = inRange (0, y - 1) x 

note-on abstraction ===================================================================================================

> data NoteOn                              =
>   NoteOn {
>     noteOnVel          :: Velocity
>   , noteOnKey          :: KeyNumber} deriving (Eq, Ord, Show)
>
> carefulNoteOn      :: Bool → Velocity → KeyNumber → NoteOn
> carefulNoteOn hack volIn pchIn           = NoteOn (safeMidiValue volIn) (safeMidiValue pchIn)
>   where
>     fName                                = "carefulNoteOn"
>
>     safeMidiValue      :: Int → Int
>     safeMidiValue x                      =
>       let
>         x'                               = clip (0, qMidiInt128 - 1) x
>       in
>         profess
>           (hack || x == x')
>           (unwords [fName, "wild", show (volIn, pchIn)])
>           x'
>
> noonAsCoords           :: NoteOn → ([Word], [Word])
> noonAsCoords noon                        =
>   (  [fromIntegral noon.noteOnKey, fromIntegral noon.noteOnVel, 0]
>    , [fromIntegral noon.noteOnKey, fromIntegral noon.noteOnVel, 1])

graph Theory ==========================================================================================================

> makeGraph              :: [(Node, [Node])] → Graph
> makeGraph list                           = 
>   let
>     highestL                             = if null list
>                                              then 0
>                                              else maximum (map fst list)
>     highestR                             = foldl' (\x y → max x (maximum y)) 0 (map snd list)
>     highest                              = max highestL highestR
>     orphans                              = filter (\x → isNothing (lookup x list)) [0..highest]
>     extra                                = map (,[]) orphans
>   in array (0, highest) (list ++ extra)

time ==================================================================================================================

> ratEps                 :: Double
> ratEps                                   = 0.000_1
>
> approx                 :: Double → Dur
> approx durA                              = approxRational durA ratEps

-- Function to format elapsed time between two zoned times

> diffZonedTime          :: ZonedTime → ZonedTime → Double
> diffZonedTime tLater tEarlier            = 
>   let
>     (tStart, tEnd)                       = (zonedTimeToUTC tEarlier, zonedTimeToUTC tLater)
>   in
>     realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime tEnd tStart
>
> formatDiffTime         :: ZonedTime → ZonedTime → String
> formatDiffTime tsNow tsThen              =
>   let
>     (tStart, tEnd)                       = (zonedTimeToUTC tsThen, zonedTimeToUTC tsNow)
>     utcDiff                              = diffUTCTime tEnd tStart
>   in
>     formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime utcDiff)

-- Function to format elapsed time from raw seconds

> formatSeconds         :: Rational → String
> formatSeconds tsDiff                     =
>   let
>     utcDiff                              = secondsToNominalDiffTime $ fromRational tsDiff
>   in
>     formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime utcDiff)

The End
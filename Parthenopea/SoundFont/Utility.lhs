> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

Utility
William Clements
October 8, 2025

> module Parthenopea.SoundFont.Utility where
>
> import Control.Arrow
> import Data.Array.Unboxed
> import Data.Complex
> import Data.Graph (Graph)
> import Data.Maybe
> import Data.Ratio ( approxRational )
> import Data.Time
> import Data.Time.Clock.POSIX
> import Euterpea.IO.Audio.Types ( AudRate, Clock(..), CtrRate )
> import Euterpea.IO.MIDI.GeneralMidi ( )
> import Euterpea.Music
>
> type GMKind                              = Either InstrumentName PercussionSound
> type KeyNumber                           = AbsPitch
> type Velocity                            = Volume
> type Node                                = Int
>
> allKinds               :: ([InstrumentName], [PercussionSound])
> allKinds                                 =
>   (  map toEnum [fromEnum AcousticGrandPiano .. fromEnum Gunshot]
>    , map toEnum [fromEnum AcousticBassDrum .. fromEnum OpenTriangle])
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
>
> ctrRate, audRate       :: Double
> ctrRate                                  = rate (undefined :: CtrRate)
> audRate                                  = rate (undefined :: AudRate)

-- Function to format elapsed time from raw seconds

> formatSeconds         :: Rational → String
> formatSeconds tsDiff                     =
>   let
>     utcDiff                              = secondsToNominalDiffTime $ fromRational tsDiff
>   in
>     formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime utcDiff)
>
> data ModTriple                           = ModTriple !Double !Double !Double
> defModTriple           :: ModTriple
> defModTriple                             = ModTriple 0 0 0
>
> deriveModTriple        :: Maybe Int → Maybe Int → Maybe Int → ModTriple
> deriveModTriple toPitch toFilterFc toVolume
>                                          =
>   ModTriple
>     (maybe 0 fromIntegral toPitch)
>     (maybe 0 fromIntegral toFilterFc)
>     (maybe 0 fromIntegral toVolume)
>
> data TimeFrame                           =
>   TimeFrame {
>     tfSecsSampled      :: Double
>   , tfSecsScored       :: Double
>   , tfSecsToPlay       :: Double
>   , tfLooping          :: Bool} deriving (Eq, Show)
> data EnvelopeExtras                      =
>   EnvelopeExtras {
>     eeTargetT          :: Double
>   , eeReleaseT         :: Double
>   , eePostT            :: Double} deriving (Eq, Show)
> data FEnvelope                           =
>   FEnvelope {
>     fExtras            :: Maybe EnvelopeExtras
>   , fSustainLevel      :: Double
>   , fModTriple         :: Maybe ModTriple
>
>   , fDelayT            :: Double
>   , fAttackT           :: Double
>   , fHoldT             :: Double
>   , fDecayT            :: Double
>   , fSustainT          :: Double}
> data Segments                            =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show

Returns the frequency ratio

> fromCents              :: Double → Double
> fromCents cents                          = pow 2 (cents/12/100)
>
> fromCents'             :: Maybe Int → Maybe Int → Maybe Double
> fromCents' mcoarse mfine
>   | isNothing mcoarse && isNothing mfine = Nothing
>   | otherwise                            = Just $ fromCents $ coarse * 100 + fine
>   where
>     coarse = maybe 0 fromIntegral mcoarse
>     fine   = maybe 0 fromIntegral mfine

Returns the frequency

> fromAbsoluteCents      :: Int → Double
> fromAbsoluteCents acents                 = 8.176 * fromCents (fromIntegral acents)
>
> toAbsoluteCents        :: Double → Int
> toAbsoluteCents freq                     = round $ 100 * 12 * logBase 2 (freq / 8.176)

Returns the elapsed time in seconds

> fromTimecents          :: Maybe Int → Double
> fromTimecents mtimecents                 = pow 2 (maybe (- 12_000) fromIntegral mtimecents / 1_200)
>
> fromTimecents'         :: Maybe Int → Maybe Int → KeyNumber → Double
> fromTimecents' mtimecents mfact key      = pow 2 (base + inc)
>   where
>     base               :: Double         =
>       maybe (-12_000) fromIntegral mtimecents / 1_200
>     inc                :: Double         =
>       maybe 0 fromIntegral mfact * fromIntegral (60 - key) / qMidiDouble128 / 1_200
>
> toTimecents            :: Double → Int
> toTimecents secs                         = round $ logBase 2 secs * 1_200
>
> minDeltaT, minUseful   :: Double
> minDeltaT                                = fromTimecents Nothing
> minUseful                                = 1/82

Returns the amplitude ratio

> fromCentibels          :: Double → Double
> fromCentibels centibels                  = pow 10 (centibels/1000)
>
> toCentibels            :: Double → Double
> toCentibels ratio                        = logBase 10 (ratio * 1000)

Returns the amplitude ratio (based on input 10ths of a percent) 

> fromTithe              :: Maybe Int → Bool → Double
> fromTithe iS isVol                       =
>   if isVol
>     then 1 / fromCentibels jS
>     else (1000 - jS) / 1000
>   where
>     jS                 :: Double         = maybe 0 fromIntegral iS
>
> theE, epsilon, upsilon :: Double
> theE                                     = 2.718_281_828_459_045_235_360_287_471_352_7
> epsilon                                  = 1e-8               -- a generous little epsilon
> upsilon                                  = 1e10               -- a scrawny  big    upsilon
>    
> theE' :: Complex Double
> theE' = theE :+ 0
>
> theJ :: Complex Double
> theJ = 0 :+ 1
>  
> constA                 :: Arrow a ⇒ c → a b c
> constA                                   = arr . const
   
Raises 'a' to the power 'b' using logarithms.

> pow                    :: Floating a ⇒ a → a → a
> pow x y                                  = exp (log x * y)

The End
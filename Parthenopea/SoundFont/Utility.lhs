> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

Utility
William Clements
October 8, 2025

> module Parthenopea.SoundFont.Utility where
>
> import Control.Arrow
> import Control.Arrow.ArrowP
> import Control.Arrow.Operations
> import Data.Array.Unboxed
> import Data.Complex
> import Data.Graph (Graph)
> import Data.IntMap.Strict ( IntMap )
> import qualified Data.IntMap.Strict      as IntMap
> import Data.Maybe
> import Data.Ratio ( approxRational )
> import Data.Time
> import Data.Time.Clock.POSIX
> import qualified Data.Vector.Strict      as VB
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Types ( AudRate, Clock(..), CtrRate, SigFun, Signal )
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
> roundVectorBy          :: Double → VB.Vector Double → VB.Vector Double
> roundVectorBy p10                        = VB.map (roundBy p10)
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
>           (unwords [fName, "wild", show (volIn, pchIn), show (x, x')])
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
>     formatTime defaultTimeLocale "%H:%M:%S%Q" (posixSecondsToUTCTime utcDiff)
>
> data SlwRate
> instance Clock SlwRate where
>   rate _                                 = 441
> type SlwSF a b                           = SigFun SlwRate a b
>
> slwRate, ctrRate, audRate
>                        :: Double
> slwRate                                  = rate (undefined :: SlwRate)
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
>   deriving (Eq, Show)
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
>   deriving (Eq, Show)
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
> toTimecents            :: Double → Int
> toTimecents secs                         = round $ logBase 2 secs * 1_200
>
> minDeltaT, minUseful   :: Double
> minDeltaT                                = fromTimecents Nothing -- 9.765625e-4
> minUseful                                = 1/82

Returns the amplitude ratio

> fromCentibels, toCentibels
>                        :: Double → Double
> fromCentibels centibels                  = pow 10 (centibels/1000)
> toCentibels ratio                        = logBase 10 (ratio * 1000)

Returns the amplitude ratio (based on input 10ths of a percent) 

> fromTithe              :: Maybe Int → Bool → Double
> fromTithe iS isVol                       =
>   if isVol
>     then 1 / fromCentibels jS
>     else (1000 - jS) / 1000
>   where
>     jS                 :: Double         = maybe 0 fromIntegral iS

An adaptor to make CtlSF into AudSF, communicating an IntMap

> upsample2              :: ∀ a b d p1 p2 . (ArrowChoice a, ArrowCircuit a, Clock p1, Clock p2) ⇒
>                           ArrowP a p1 b (IntMap d) → ArrowP a p2 b (IntMap d)
> upsample2 f                              =
>   let
>     r                                    =
>       if outRate < inRate 
>         then error "Cannot upsample a signal of higher rate to lower rate" 
>         else outRate / inRate
>     inRate  = rate (undefined :: p1)
>     outRate = rate (undefined :: p2)
>   in    
>     proc x                               → do
>       rec
>         cc ← delay 0                     ⤙ if cc >= r-1 then 0 else cc+1
>         y ← if cc == 0
>               then ArrowP (strip f)
>                                          ⤙ x 
>               else delay IntMap.empty    ⤙ y
>       outA                               ⤙ y

A delay line that doesn't use unsafePerformIO. To do: implement the delay line variants

> vuDelayLine            :: ∀ p . Clock p ⇒ Double → Signal p Double Double
> vuDelayLine maxdel                       =
>     let sr                               = rate (undefined :: p)
>         sz                               = truncate (sr * maxdel)
>     in proc x → do
>       rec
>         let ix' = if ix == sz-1 then 0 else ix+1
>         ix ← delay 0 ⤙ ix'
>         buf ← delay (VU.replicate sz 0) ⤙ buf'
>         let next = buf VU.! ix
>         let buf' = VU.update buf (VU.singleton (ix, x))
>       outA ⤙ next

Raises 'a' to the power 'b' using logarithms

> pow                    :: Floating a ⇒ a → a → a
> pow x y                                  = exp (log x * y)

Need this definition

> constA                 :: Arrow a ⇒ c → a b c
> constA                                   = arr . const

Constants

> theE, epsilon, upsilon :: Double
> theE                                     = 2.718_281_828_459_045_235_360_287_471_352_7 -- just kiddin'
> epsilon                                  = 1e-8                                        -- a generous little epsilon
> upsilon                                  = 1e10                                        -- a scrawny  big    upsilon
>    
> theE', theJ            :: Complex Double
> theE' = theE :+ 0
> theJ = 0 :+ 1

The End
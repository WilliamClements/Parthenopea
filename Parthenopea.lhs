> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeFamilies #-} 
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UnicodeSyntax #-}

Parthenopea
William Clements
December 12, 2022

> module Parthenopea where
>
> import qualified Codec.Midi              as M
> import Control.Arrow.ArrowP
> import Control.DeepSeq (NFData)
> import Control.SF.SF
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Char
> import Data.Complex
> import Data.Either
> import Data.Graph (Graph)
> import qualified Data.Graph              as Graph
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List hiding (transpose)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( fromJust, isJust, isNothing, mapMaybe, fromMaybe, listToMaybe )
> import Data.MemoTrie
> import Data.Ord
> import Data.Ratio ( approxRational, (%) )
> import qualified Data.Vector.Unboxed     as VU
> import qualified Data.Vector             as VB
> import Data.Word
> import Debug.Trace ( trace )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg, Table, tableSinesN, osc )
> import Euterpea.IO.Audio.Types
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.MidiIO (unsafeOutputID, unsafeInputID, OutputDeviceID, InputDeviceID)
> import Euterpea.IO.MIDI.Play
> import Euterpea.IO.MIDI.ToMidi2 (writeMidi2)
> import Euterpea.Music
> import GHC.Generics (Generic) 
> import HSoM.Performance ( metro, Context (cDur) )
> import SettingsDefs
> import System.Random ( Random(randomR), StdGen )
  
Tracing ===============================================================================================================

> traceIf, traceNow, traceAlways, traceNever, traceNot
>                        :: String → a → a
> traceIf str expr                         = if diagnosticsEnabled then trace str expr else expr
> traceNow                                 = trace
> traceAlways                              = trace
> traceNever str expr                      = expr
> traceNot str expr                        = expr
>
> tracer                 :: Show a ⇒ String → a → a
> tracer str x                             =
>   if True
>     then traceNow (unwords [str ++ "=", show x]) x
>     else x
>
> notracer               :: Show a ⇒ String → a → a
> notracer _ x                             = x

Utilities =============================================================================================================

> hzToAp                 :: Double → AbsPitch
> hzToAp freak                             =
>   round $ fromIntegral (absPitch (A,4)) + 12 * (logBase 2 freak - logBase 2 440)
>
> theE, epsilon, upsilon :: Double
> theE                                     = 2.718_281_828_459_045_235_360_287_471_352_7
> epsilon                                  = 1e-8               -- a generous little epsilon
> upsilon                                  = 1e10               -- a scrawny  big    upsilon
>
> qMidiSize128           :: Int            = 128
> qMidiSizeSpace         :: Int            = qMidiSize128 * qMidiSize128
>
> qOffsetWeights         :: [Int]          = [1, 32_768]
> freakRange             :: (Double, Double)
>                                          = (20, 20_000)
>
> impM                   :: FilePath → IO ()
> impM fp                                  = do
>   mu ← importMidi fp
>   play mu
>   -- listInstruments $ aggrandize mu
>   return ()
>
> importMidi             :: FilePath → IO (Music (Pitch, Volume))
> importMidi fp = do
>   x ← M.importFile fp
>   case x of
>     Left z             → error z
>     Right m2           → analyzeMidi m2
>
> analyzeMidi            :: M.Midi → IO (Music (Pitch, Volume))
> analyzeMidi midi                         = do
>   print midi
>   return $ rest 0
>
> addDur                 :: Dur → [Dur → Music a] → Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)
>
> grace                  :: Int → Music Pitch → Music Pitch
> grace n (Prim (Note d p))                = note (est * d) (trans n p) :+: note ((1 - est) * d) p
>   where
>     est                :: Rational
>     est
>       | d < 1/8                          = 1/2
>       | d > (2*wn)                       = 1/(8*d)
>       | otherwise                        = 1/8
> grace n _                                = 
>   error "Can only add a grace note to a note."
>
> t32                    :: [Music a] → Music a
> t32 notes                                = tempo (3/2) (foldr (:+:) (rest 0) notes)
>
> dim                    :: Rational → Music a → Music a
> dim amt                                  = phrase [Dyn (Diminuendo amt)]
>
> capture                :: Music (Pitch, [NoteAttribute]) → IO()
> capture                                  = writeMidi2 "Parthenopea.mid"
>
> fractionOf             :: Int → Double → Int
> fractionOf x d                           = min 127 $ round $ d * fromIntegral x
>
> slur                   :: Rational → Music a → Music a
> slur rate                                = Modify (Phrase [Art (Slurred rate)])
>
> durS                   :: Rational → Double
> durS r                                   = 2 * fromRational r
> 
> ratEps                 :: Double
> ratEps                                   = 0.000_1
>
> approx                 :: Double → Dur
> approx dur                               = approxRational dur ratEps
>
> rawPitches                               = [0..127]
>
> allPitches =
>    foldr ((:+:) . notize) (rest 0) rawPitches
>    where
>       notize aP                          = note qn (pitch aP)
>
> -- note chordFromArray has the same function body as chord itself
> chordFromArray         :: Array Int (Music (Pitch, [NoteAttribute])) → Music (Pitch, [NoteAttribute])
> chordFromArray                           = foldr (:=:) (rest 0)

This alternate playback function will enable channel manipulation to allow
more than 16 instruments to be used in the source music.

> playDM                 :: (NFData a, ToMusic1 a) ⇒ Maybe Int → Music a → IO ()
> playDM mi                                =
>   playC defParams
>     { strict=False
>     , chanPolicy = dynamicCP 16 9
>     , devID                              = case mi of
>                                              Nothing → Nothing
>                                              Just i → Just $ unsafeOutputID i
>     , perfAlg= map (\e → e{eDur = max 0 (eDur e - 0.000_001)}) . perform}

This alternate playback function will merge overlapping notes, 
which makes for a cleaner sound on some synthesizers:

> playX        :: (NFData a, ToMusic1 a) ⇒ Music a → IO ()
> playX = playC defParams{perfAlg = eventMerge . perform} where 
>     eventMerge :: Performance → Performance
>     eventMerge (e1:e2:es) = 
>         let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
>         in  if ePitch e1 == ePitch e2 then eventMerge (e1':es) 
>             else e1 : eventMerge (e2:es)
>     eventMerge e = e

"triad" ===============================================================================================================

> triad                  :: PitchClass → Mode → Pitch → Dur → Music Pitch
> triad key mode base dur                  = chord [n1, n2, n3]
>   where
>     rkP                                  = absPitch (key, snd base - 1)
>     bP                                   = absPitch base
>     ocd                                  = (bP - rkP) `div` 12
>     kP                                   = rkP + (12 * ocd)
>     apD                                  = bP - kP           -- guaranteed to be nonnegative
>
>     is                 :: [AbsPitch]     = offsets2intervals mode2offsets
>
>     n1, n2, n3         :: Music Pitch
>     n1                                   = note dur $ pitch bP
>     n2                                   = note dur $ pitch (bP + head          is)
>     n3                                   = note dur $ pitch (bP + (head . tail) is)
>
>     major                                = [0, 4, 7, 12, 16]
>     minor                                = [0, 3, 7, 12, 15]
>     dim                                  = [0, 3, 6, 12, 15]
>     sus4                                 = [0, 5, 7, 12, 17]
>     sus2                                 = [0, 2, 7, 12, 14]
>     aug                                  = [0, 4, 8, 12, 16]
>     chrome                               = [0, 1, 2,  3,  4]
>
>     mode2offsets       :: [AbsPitch]
>     mode2offsets
>       | Major                == mode     = major
>       | Minor                == mode     = minor
>       | Ionian               == mode     = major
>       | Dorian               == mode     = minor
>       | Phrygian             == mode     = minor
>       | Lydian               == mode     = major
>       | Mixolydian           == mode     = major
>       | Aeolian              == mode     = minor
>       | Locrian              == mode     = dim
>       | CustomMode "Sus4"    == mode     = sus4
>       | CustomMode "Sus2"    == mode     = sus2
>       | CustomMode "Dim"     == mode     = dim
>       | CustomMode "Aug"     == mode     = aug
>       | CustomMode "Chrome"  == mode     = chrome
>       | otherwise                        = error "Requested Mode not supported"
>
>     offsets2intervals  :: [AbsPitch] → [AbsPitch]
>     offsets2intervals os
>       | apD == a                         = (b - a) : [c - a]
>       | apD == b                         = (c - b) : [d - b]
>       | apD == c                         = (d - c) : [e - c]
>       | otherwise                        = error "Malformed Triad"
>       where
>         a                                = head os
>         b                                = os!!1
>         c                                = os!!2
>         d                                = os!!3
>         e                                = os!!4

"mode2Templ8" =========================================================================================================

> mode2Templ8            :: Mode → [AbsPitch]
> mode2Templ8 mode                         = templ8
>   where
>     fName                                = "mode2templ8"
>
>     base                                 = [0,2,4,5,7,9,11]
>     templ8
>       | Ionian                   == mode = shiftMode 0 base
>       | Dorian                   == mode = shiftMode 1 base
>       | Phrygian                 == mode = shiftMode 2 base
>       | Mixolydian               == mode = shiftMode 3 base
>       | Lydian                   == mode = shiftMode 4 base
>       | Aeolian                  == mode = shiftMode 5 base
>       | Locrian                  == mode = shiftMode 6 base
>
>       | Major                    == mode = shiftMode 0 base
>       | Minor                    == mode = [0,2,3,5,7,9,11]
>
>       | chromatic                == mode = [0,1,2,3,4,5,6,7,8,9,10,11]
>       | diminished               == mode = [0,2,3,5,6,8,9,11]
>       | augmented                == mode = [0,2,4,6,8,10]
>       | otherwise                        = error $ unwords [fName, show mode, "not supported yet"]
>
> shiftMode k t                            =
>   let
>     n                                    = length t
>     t'                                   = t ++ t
>     v                                    = 12 - t' !! k
>   in
>     take n $ map (\x → (v + x) `mod` 12) (drop k t')
>         
> chromatic                                = CustomMode "Chrome"
> diminished                               = CustomMode "Dim"
> augmented                                = CustomMode "Aug"
>
> findInMode               :: Pitch → PitchClass → Mode → Maybe Int
> findInMode p pc mode                     = elemIndex (abs (q2 - q1)) (mode2Templ8 mode)
>   where
>     q1                                   = fromEnum (fst p)
>     q2                                   = fromEnum pc

"ascendFrom/descendFrom" ==============================================================================================

> squeezeAPSequence      :: [AbsPitch] → Dur → Music Pitch
> squeezeAPSequence gliss dur
>   | skipGlissandi                         = rest dur
>   | length gliss < 6                      = error $ unwords [fName, "not enough notes"]
>   | dur < 1 / 8                           = error $ unwords [fName, "not enough duration"]
>   | otherwise                             = chord [rest dur, dim 1 $ slur 2 $ line notes]
>   where
>     fName                                = "squeezeAPSequence"
>
>     reach              :: Rational       = fromIntegral $ length gliss
>     notes              :: [Music Pitch]  = [note (dur * 9 / (reach * 10)) (pitch x) | x ← gliss]
>
> extendModeToInfinity   :: Bool → AbsPitch → [AbsPitch] → [(AbsPitch, Int)]
> extendModeToInfinity desc start templ8
>   | traceIf trace_EMTI False             = undefined
>   | otherwise                            = iterate' doNext (start, 0)
>   where
>     fName                                = "extendModeToInfinity"
>
>     n                                    = length templ8
>     doNext             :: (AbsPitch, Int) → (AbsPitch, Int)
>     doNext (ap, index)                   = (ap + delta, index')
>       where  
>         (index', offset)
>           | desc                         = if index == 0 then (n - 1, -12) else (index - 1, 0)
>           | index + 1 == n               = (0, 12)
>           | otherwise                    = (index + 1, 0)
>  
>         delta                            = templ8 !! index' - templ8 !! index  + offset
>
>     trace_EMTI                           = unwords [fName, show start, show templ8]
>
> descendFrom            :: BandPart → Pitch → PitchClass → Mode → Dur → Music Pitch
> descendFrom bp p pc mode                 = squeezeAPSequence (takeWhile (>= bottom) limited)
>   where
>     top                                  = absPitch p
>     bottom                               = (absPitch . fst) (relativeRange bp)
>     shift                                = deJust "first note (top) not in Mode" (findInMode p pc mode)
>     extended                             = extendModeToInfinity True top $ shiftMode shift $ mode2Templ8 mode
>     limited                              = take 28 (map fst extended)
>
> ascendFrom             :: BandPart → Pitch → PitchClass → Mode → Dur → Music Pitch
> ascendFrom bp p pc mode                  = squeezeAPSequence (takeWhile (<= top) limited)
>   where
>     top                                  = (absPitch . snd) (relativeRange bp)
>     bottom                               = absPitch p
>     shift                                = deJust "first note (bottom) not in Mode" (findInMode p pc mode)
>     extended                             = extendModeToInfinity False bottom $ shiftMode shift $ mode2Templ8 mode
>     limited                              = take 28 (map fst extended)

ranges ================================================================================================================

>   -- calibrate (0,1) to (lo,up) e.g. (24,92)
> denorm                 :: Double → (Double, Double) → Double
> denorm r (lo, up)                        = lo + r * (up-lo)
>
> randomNorms            :: StdGen → [Double]
> randomNorms g                            = x : randomNorms g'
>    where
>      (x, g') = randomR (0,1) g
>
> randomNormSets         :: Int → StdGen → [[Double]]
> randomNormSets n g                       = takeWhile (not . null) $ unfoldr (Just . splitAt n) (randomNorms g)
>
> instrumentLimit :: Double
> instrumentLimit = fromIntegral $ fromEnum Gunshot

re instrumentRange content, see:
   https://www.orchestralibrary.com/reftables/rang.html
also
   https://philharmonia.co.uk/resources/instruments/
   https://omeka-s.grinnell.edu/s/MusicalInstruments

> instrumentRange        :: InstrumentName → Maybe (AbsPitch, AbsPitch)
> instrumentRange inst                     = mrange >>= Just . BF.bimap absPitch absPitch
>   where
>     mrange                               = instrumentRange_ inst
>
> instrumentRange_       :: InstrumentName → Maybe (Pitch, Pitch)
> instrumentRange_ inst =
>    case inst of
>       Piccolo                   → Just (( D, 5), ( C, 8)) -- C piccolo
>       Flute                     → Just (( C, 4), ( D, 7))
>       -- AltoFlute
>       Oboe                      → Just ((Bf, 3), ( A, 6))
>       -- Oboe d'amore
>       EnglishHorn               → Just (( E, 2), ( C, 6))
>       -- BassOboe
>       Clarinet                  → Just (( D, 3), ( B, 6)) -- Bb clarinet
>       -- BassetHorn
>       -- BassClarinet
>       Bassoon                   → Just ((Bf, 1), (Ef, 5))
>       -- ContraBassoon
>       SopranoSax                → Just ((Af, 3), ( F, 6)) -- Bb soprano
>       AltoSax                   → Just (( D, 3), ( B, 6)) -- Eb alto
>       TenorSax                  → Just ((Af, 2), ( F, 5)) -- Bb tenor
>       BaritoneSax               → Just (( D, 2), ( B, 5)) -- Eb baritone
>       -- BassSax
>
>       FrenchHorn                → Just (( B, 1), ( F, 5)) -- F horn
>       -- WagnerTuben
>       Trumpet                   → Just (( E, 3), ( C, 6)) -- Bb trumpet
>       -- PiccoloTrumpet
>       -- ExoticTrumpet
>       -- AltoTrombone
>       Trombone                  → Just (( E, 2), ( F, 5))
>       -- BassTrombone
>       -- ContraBassTrombone
>       Tuba                      → Just (( D, 1), ( F, 4))
>       -- Euphonium
>
>       Timpani                   → Just (( D, 2), ( C, 4))
>       Xylophone                 → Just (( F, 4), ( C, 8))
>       Marimba                   → Just (( C, 2), ( C, 7))
>       Glockenspiel              → Just (( G, 5), ( C, 8))
>       Vibraphone                → Just (( F, 3), ( F, 6))
>       MusicBox                  → Just (( C, 4), ( E, 7)) -- made up out of thin air
>       Percussion                → Nothing -- was Just wideOpen
>       -- Chimes
>       AcousticGuitarNylon       → Just (( E, 2), ( B, 6))
>       AcousticGuitarSteel       → Just (( E, 2), ( B, 6))
>       ElectricGuitarJazz        → Just (( E, 2), ( B, 6))
>       ElectricGuitarClean       → Just (( E, 2), ( B, 6))
>       ElectricGuitarMuted       → Just (( E, 2), ( B, 6))
>       OverdrivenGuitar          → Just (( E, 2), ( B, 6))
>       DistortionGuitar          → Just (( E, 2), ( B, 6))
>       OrchestralHarp            → Just (( C, 1), (Fs, 7))
>       AcousticBass              → Just (( E, 1), ( C, 8))
>       ElectricBassFingered      → Just (( E, 1), ( C, 8))
>       ElectricBassPicked        → Just (( E, 1), ( C, 8))
>       Sitar                     → Just (( C, 2), ( E, 5))
>       FretlessBass              → Just (( E, 1), ( G, 4))
>       SlapBass1                 → Just (( E, 1), ( C, 8))
>       SlapBass2                 → Just (( E, 1), ( C, 8))
>       SynthBass1                → Just (( E, 1), ( C, 8))
>       SynthBass2                → Just (( E, 1), ( C, 8))
>
>       AcousticGrandPiano        → Just (( A, 0), ( C, 8))
>       BrightAcousticPiano       → Just (( A, 0), ( C, 8))
>       ElectricGrandPiano        → Just (( A, 0), ( C, 8))
>       HonkyTonkPiano            → Just (( A, 0), ( C, 8))
>       RhodesPiano               → Just (( A, 0), ( C, 8))
>       ChorusedPiano             → Just (( A, 0), ( C, 8))
>       Celesta                   → Just (( C, 4), ( C, 8))
>       Harpsichord               → Just (( F, 1), ( F, 6))
>       -- Harmonium
>       ChurchOrgan               → Just (( C, 2), ( C, 7))
>       ReedOrgan                 → Just (( C, 2), ( C, 7))
>       HammondOrgan              → Just (( C, 2), ( C, 7))
>
>       Violin                    → Just (( G, 3), ( A, 7))
>       Viola                     → Just (( C, 3), ( E, 6))
>       Cello                     → Just (( C, 2), ( C, 6))
>       Contrabass                → Just (( C, 1), (Cs, 2))
>       StringEnsemble1           → Just
>                                    $ unionRanges
>                                    $ map (fromJust . instrumentRange_)
>                                          (Violin:Viola:[Cello])
>
>       Banjo                     → Just (( C, 3), ( E, 5))
>       Harmonica                 → Just (( G, 3), ( F, 7))
>
>       ChoirAahs                 → Just (( C, 3), ( C, 6))
>
>       _                         → Nothing
>
> nonPitchedInstrument   :: InstrumentName → Bool
> nonPitchedInstrument kind                =
>   case kind of
>     Agogo                                → True
>     Applause                             → True
>     BirdTweet                            → True
>     BreathNoise                          → True
>     Gunshot                              → True
>     Helicopter                           → True
>     Percussion                           → True
>     ReverseCymbal                        → True
>     Seashore                             → True
>     SynthDrum                            → True
>     TaikoDrum                            → True
>     TelephoneRing                        → True
>     Timpani                              → True
>     Woodblock                            → True
>     _                                    → False
>
> findBetterInstrument   :: InstrumentName → (AbsPitch, AbsPitch) → InstrumentName
> findBetterInstrument than (playedLo, playedHi)
>   | traceNever trace_FBI False           = undefined
>   | otherwise                            =
>     if null rangedInsts
>       then than
>       else snd $ minimumBy (comparing fst) rangedInsts
>   where
>     rangedInsts        :: [(Int, InstrumentName)]
>     rangedInsts                          = mapMaybe judgeScore (fst allKinds)
>
>     judgeScore         :: InstrumentName → Maybe (Int, InstrumentName)
>     judgeScore cand                      = (\c → if nonPitchedInstrument c then Nothing else Just c) cand
>                                            >>= instrumentRange >>= uncurry (fitsIn cand)
>     
>     fitsIn             :: InstrumentName → AbsPitch → AbsPitch → Maybe (Int, InstrumentName)
>     fitsIn cand rangeLo rangeHi
>       | traceNever trace_FI False        = undefined
>       | otherwise                        =
>         if inRange range playedLo && inRange range playedHi 
>           then Just (snd range - fst range, cand)
>           else Nothing
>       where
>         range                            = (rangeLo, rangeHi)
>         trace_FI                         = unwords ["fitsIn", show range, show playedLo, show playedHi]
>
>     trace_FBI                            = unwords ["findBetterInstrument", show than, show rangedInsts]
>
> selectRanged           :: [InstrumentName] → Array Int (InstrumentName, (AbsPitch, AbsPitch))
> selectRanged is                          = listArray (1, length qual) qual
>   where
>     qual                                 = mapMaybe (\i → instrumentRange i >>= Just . (i,)) is
>
> denormVector           :: Double → Array Int b → b
> denormVector norm vect                   = profess
>                                              (norm >= 0 && norm <= 1)
>                                              (unwords ["denormVector:", "bad input norm"])
>                                              (vect ! (lo + floor (denorm norm frange)))
>   where
>     (lo, hi)                             = bounds vect
>     frange                               = (0, fromIntegral (hi - lo + 1) - 0.000_001)
>
> wideOpen               :: (AbsPitch, AbsPitch) = (0, 127)

instrument range checking =============================================================================================

> union2Ranges           :: (Ord a, Ord b) ⇒ (a, b) → (a, b) → (a, b)
> union2Ranges r1 r2                       = unionRanges (r1:[r2])
> unionRanges []                           = error "empty range list"
> unionRanges (r:rs)                       = ( minimum (map fst (r:rs))
>                                            , maximum (map snd (r:rs)) )
> intersect2Ranges r1 r2                   = intersectRanges (r1:[r2])
> intersectRanges (r:rs)                   =
>   case uncurry compare inverted of
>     LT → Just inverted
>     _  → Nothing
>   where
>     inverted                             = ( maximum (map fst (r:rs))
>                                            , minimum (map snd (r:rs)) )
> intersectRanges _                        = error "empty range list"
>
> data BandPart =
>   BandPart {
>     bpInstrument       :: InstrumentName
>   , bpTranspose        :: AbsPitch
>   , bpVelocity         :: Velocity} deriving Show
> relativeRange          :: BandPart → (Pitch, Pitch)
> relativeRange bp                         =
>   let
>     recenter           :: AbsPitch → Pitch
>     recenter                             = trans (-bp.bpTranspose) . pitch
>   in
>     BF.bimap recenter recenter (fromJust (instrumentRange bp.bpInstrument))
>
> type DynMap                              = Map InstrumentName InstrumentName
>
> makePitched            :: InstrumentName → AbsPitch → AbsPitch → Velocity → BandPart
> makePitched iname apGlobal apLocal       = BandPart iname (apGlobal + apLocal)
>
> makeNonPitched         :: Velocity → BandPart
> makeNonPitched                           = BandPart Percussion 0
>
> replace                :: BandPart → DynMap → BandPart
> replace bp@BandPart{ .. } dynMap
>   | traceNever trace_R False             = undefined
>   | otherwise                            = bp{bpInstrument = ninst}
>   where
>     minst                                = Map.lookup bpInstrument dynMap
>     ninst                                =
>       if not (nonPitchedInstrument bpInstrument) && isJust minst
>         then fromJust minst
>         else bpInstrument
>
>     trace_R                              = unwords ["replace", show dynMap, show minst, show ninst]
>
> makeDynMap             :: Shredding → DynMap
> makeDynMap ding                          =
>   if replacePerCent > 50
>     then foldl' maker Map.empty (Map.assocs ding.shRanges)
>     else Map.empty
>   where
>     maker              :: DynMap → (GMKind, Shred) → DynMap
>     maker i2i (kind, Shred{ .. })        =
>       let
>         inameL, inameR :: InstrumentName
>         inameL                           = fromLeft (error "makeDynMap problem: no instrument") kind
>         inameR                           = findBetterInstrument inameL (ePitch shLowNote, ePitch shHighNote)
>       in
>         if isLeft kind && inameL /= inameR
>           then Map.insert inameL inameR i2i
>           else i2i
>
> bandPart               :: BandPart → Music Pitch → Music (Pitch, [NoteAttribute])
> bandPart bp                              = mMap bChanger . instrument bp.bpInstrument . transpose bp.bpTranspose
>   where
>     bChanger           :: Pitch → (Pitch, [NoteAttribute])
>     bChanger p                           = (p, [Volume bp.bpVelocity])
>
> orchestraPart          :: BandPart → Music (Pitch, [NoteAttribute]) → Music (Pitch, [NoteAttribute])
> orchestraPart bp                         = mMap oChanger . instrument bp.bpInstrument . transpose bp.bpTranspose
>   where
>     oChanger           :: (Pitch, [NoteAttribute]) → (Pitch, [NoteAttribute])
>     oChanger (p, nas)                    = (p, map nasFun nas)
>
>     nasFun             :: NoteAttribute → NoteAttribute
>     nasFun (Volume _)                    = Volume bp.bpVelocity
>     nasFun na                            = na 
>
> bendNote               :: BandPart → PitchClass → Octave → Dur → AbsPitch → Music (Pitch, [NoteAttribute])
> bendNote bp pc o d bend                  = note d ((pc, o), [Volume bp.bpVelocity, Params [fromIntegral bend]])
>
> addVolume'    :: BandPart → Music Pitch → Music (Pitch,[NoteAttribute])
> addVolume' bp                            = mMap (, [Volume bp.bpVelocity])
>
> psoundToPitch :: PercussionSound → Pitch
> psoundToPitch psound                     = pitch (fromEnum psound + 35)

examine song for instrument and percussion usage ======================================================================

> type GMKind                              = Either InstrumentName PercussionSound
> 
> data Shred =
>   Shred {
>       shLowNote        :: MEvent
>     , shHighNote       :: MEvent
>     , shCount          :: Int} deriving (Show, Eq, Ord)
>
> data Shredding =
>   Shredding {
>       shRanges         :: Map GMKind Shred
>     , shMsgs           :: [(InstrumentName, [String])]} deriving (Show, Eq, Ord)
> defShredding                             = Shredding Map.empty []
>
> getGMKind              :: MEvent → GMKind
> getGMKind MEvent{eInst, ePitch}          =
>   case eInst of
>     Percussion                           → Right $ toEnum (ePitch - 35)
>     _                                    → Left eInst
>
> shimSong                :: Music (Pitch, [NoteAttribute]) → DynMap → Music (Pitch, [NoteAttribute])
> shimSong m a                             = m
>
> shredMusic              :: ToMusic1 a ⇒ Music a → IO Shredding
> shredMusic m                             =
>   return $ critiqueMusic $ foldl' shFolder defShredding $ fst (musicToMEvents defaultContext (toMusic1 m))
>
> shredSongs              :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO (Map GMKind Shred)
> shredSongs songs                         = do
>   shredses                               ← mapM shredSong songs
>   return $ foldr (Map.unionWith combineShreds) Map.empty shredses
>
> allKinds               :: ([InstrumentName], [PercussionSound])
> allKinds                                 =
>   (  map toEnum [fromEnum AcousticGrandPiano .. fromEnum Gunshot]
>    , map toEnum [fromEnum AcousticBassDrum .. fromEnum OpenTriangle])
>
> qualifyKinds           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))]
>                           → IO ([InstrumentName], [PercussionSound])
> qualifyKinds songs                       = do
>   mks                                    ← shredSongs songs
>   let isandps                            = Map.keys mks
>   return (lefts isandps, rights isandps)
>
> shredSong              :: (String, DynMap → Music (Pitch, [NoteAttribute])) → IO (Map GMKind Shred)
> shredSong (_, song)                      = do -- return $ shredMusic $ song Map.empty
>   let asMusic                            = song Map.empty
>   ding                                   ← shredMusic asMusic
>   return $ shRanges ding
>
> critiqueMusic          :: Shredding → Shredding
> critiqueMusic Shredding{ .. }            = Shredding shRanges (concatMap critiqueShred (Map.assocs shRanges))
>
> combineShreds          :: Shred → Shred → Shred
> combineShreds s1 s2                      =
>   s1 {  shLowNote                        =
>           if ePitch s1.shLowNote < ePitch s2.shLowNote
>             then s1.shLowNote
>             else s2.shLowNote
>       , shHighNote                       =
>           if ePitch s1.shHighNote > ePitch s2.shHighNote
>             then s1.shHighNote
>             else s2.shHighNote
>       , shCount                          =
>           s1.shCount + s2.shCount}
>
> critiqueShred          :: (GMKind, Shred) → [(InstrumentName, [String])]
> critiqueShred (kind, Shred{ .. })        =
>   let
>     (instr, range)                       =
>       case kind of
>         Left iname                       → (iname, fromMaybe wideOpen (instrumentRange iname))
>         _                                → (Percussion, wideOpen)
>   in critiqueNote instr range shLowNote ++ critiqueNote instr range shHighNote
> 
> critiqueNote           :: InstrumentName → (AbsPitch, AbsPitch) → MEvent → [(InstrumentName, [String])]
> critiqueNote name range mev              =
>   let
>     p                                    = mev.ePitch
>   in
>     if p == clip range p
>       then []
>       else singleton (name, singleton $ unwords ["...", show p, "out of range", show range])
>
> shFolder               :: Shredding → MEvent → Shredding
> shFolder ding mev                        =
>   let
>     kind               :: GMKind         = getGMKind mev
>     mshred             :: Maybe Shred    = Map.lookup kind ding.shRanges
>   in
>     case mshred of
>       Nothing                            → Shredding (Map.insert kind (Shred mev mev 1) ding.shRanges) ding.shMsgs
>       Just shred                         → Shredding (Map.insert kind (upd shred)       ding.shRanges) ding.shMsgs
>   where
>     upd shred                            =
>       Shred
>         (if ePitch mev < ePitch shred.shLowNote  then mev else shred.shLowNote)
>         (if ePitch mev > ePitch shred.shHighNote then mev else shred.shHighNote)
>         (shred.shCount + 1)
>
> shredJingles           :: [(String, DynMap → Music (Pitch, [NoteAttribute]))] → IO ()
> shredJingles js                          = do
>   putStrLn "showing incidence, static range, onset time of low and high.\nNote: ! denotes out of bounds\n"
>   mapM_ (uncurry shredJingle) js
>
> shredJingle            :: String → (DynMap → Music (Pitch, [NoteAttribute])) → IO ()
> shredJingle name m                       = do
>   putStrLn name
>   putStrLn ""
>   printShreds =<< shredMusic (m Map.empty)
>
> printShreds            :: Shredding → IO ()
> printShreds Shredding{shRanges}          = 
>   mapM_ (uncurry printShred) (Map.assocs shRanges)
>   
> printShred             :: GMKind → Shred → IO ()
> printShred kind Shred{ .. }              = do
>   putStrLn showGivenRange
>   putStrLn showLowHighNotes
>
>   where
>   mrange                                 =
>     case kind of
>       Left iname                         → instrumentRange iname
>       _                                  → Nothing
>   showGivenRange                         = showKind ++ "(" ++ show shCount ++ ")" ++ " = " ++ showAvail
>     where
>       showAvail                          = maybe "" (\r → show (fst r) ++ " .. " ++ show (snd r)) mrange
>   showKind                               =
>     case kind of
>       Left iname                         → show iname
>       Right psound                       → show psound
>   showLowHighNotes                       =
>     case kind of
>       Left _                             → showShred shLowNote ++ "\n" ++ showShred shHighNote ++ "\n" 
>       _                                  → showShred shLowNote ++ "\n" 
>   showShred mev                          =
>     case kind of
>       Left _                             → show (fromRational mev.eTime)
>                                            ++ show (pitch mev.ePitch)
>                                            ++ showOutOfRangeIndicator mev.ePitch
>       _                                  → show (fromRational mev.eTime)
>   showOutOfRangeIndicator p              = if isNothing mrange || inRange (deJust "range" mrange) p
>                                              then "."
>                                              else "!"
>
> pitchToPerc            :: AbsPitch → Maybe PercussionSound
> pitchToPerc ap                           =
>   let
>     ad                                   = ap - 35
>   in
>     if ad >= fromEnum AcousticBassDrum && ad <= fromEnum OpenTriangle
>       then Just (toEnum ad)
>       else Nothing

tournament among instruments in various soundfont files ===============================================================

> hist         :: (Ix a, Integral b) ⇒ (a,a) → [a] → Array a b
> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i ← is, inRange bnds i]
>
> defBins      :: Int
> defBins = 32
>
> collectMEvents         :: ToMusic1 a ⇒ Music a → Performance
> collectMEvents m                         = fst (musicToMEvents defaultContext (toMusic1 m))
>
> defaultContext         :: MContext
> defaultContext                           =
>   MContext
>     {  mcTime = 0
>      , mcInst = AcousticGrandPiano
>      , mcDur = metro 120 qn
>      , mcVol = 100}
>
> extractTimes           :: Performance → [Double]
> extractTimes                             = map (fromRational . eTime)
>
> percussionLimit        :: Double
> percussionLimit                          = fromIntegral $ fromEnum OpenTriangle

snippets to be used with "lake" =======================================================================================

> pSnippet01, pSnippet02, defSnippet
>                        :: Music Pitch
> pSnippet01                               = tempo (3/2) (line [ e 4 qn, e 4 en, e 4 qn, e 4 en])
>
> pSnippet02                               = line [c 4 en, e 4 qn, bf 3 en, d 4 qn, f 4 en]
>
> defSnippet                               = pSnippet01
>
> stdMels                :: Music Pitch → [Music Pitch]
> stdMels melInput                         = [mel0, mel1, mel2, mel3]
>   where 
>     mel0                                 = melInput
>     mel1                                 = retro melInput
>     mel2                                 = invert melInput
>     mel3                                 = (invert . retro) melInput

music converter =======================================================================================================

> aggrandize             :: Music (Pitch, Volume) → Music (Pitch, [NoteAttribute])
> aggrandize (Prim (Note d (p, v)))        = Prim (Note d (p, [Volume v]))
> aggrandize (Prim (Rest d))               = Prim (Rest d)
> aggrandize (m1 :+: m2)                   = aggrandize m1 :+: aggrandize m2
> aggrandize (m1 :=: m2)                   = aggrandize m1 :=: aggrandize m2
> aggrandize (Modify c m)                  = Modify c (aggrandize m)
>
> linten                 :: Music (Pitch, [NoteAttribute]) → [String]
> linten                                   = mFold f (++) (++) h
>   where
>     f (Note _ (_, na))                   = concatMap g na
>     f _                                  = []
>     g (Volume v)                         = [show v | v < 0]
>     g _                                  = []
>     h (Phrase pa) ss                     = ss ++ concatMap j pa
>     h _ ss                               = ss
>     j (Dyn dyn)                          = [show dyn]
>     j _                                  = []

-----------------------------------------------------------------------------------------------------------------------

> data KernelSpec                          =
>   KernelSpec {
>     ksFc               :: Int
>   , ksQ                :: Int
>   , ksSr               :: Int
>   , ksFast             :: Bool
>   , ksLen              :: Int} deriving (Eq, Generic, Ord, Show)
>
> defKernelSpec bFast                      = KernelSpec 13_500 0 44_100 bFast 300
>
> instance HasTrie KernelSpec where
>   newtype (KernelSpec :->: b)            = KernelSpecTrie { unKernelSpecTrie :: Reg KernelSpec :->: b } 
>   trie                                   = trieGeneric KernelSpecTrie 
>   untrie                                 = untrieGeneric unKernelSpecTrie
>   enumerate                              = enumerateGeneric unKernelSpecTrie
>
> eutSplit               :: ∀ p . Clock p ⇒ Signal p Double (Double, Double)
> eutSplit                                 =
>   proc sIn → do
>     outA                                 ⤙ (sIn, sIn)

Signals of interest ===================================================================================================

> sawtoothTable      :: Table              = tableSinesN 16_384 
>                                                          [      1, 0.5  , 0.3
>                                                            , 0.25, 0.2  , 0.167
>                                                            , 0.14, 0.125, 0.111]
>
> triangleWaveTable      :: Table          = tableSinesN 16_384 
>                                                          [      1,  0, -0.5,  0,  0.3,   0
>                                                           , -0.25,  0,  0.2,  0, -0.167, 0
>                                                           ,  0.14,  0, -0.125]
>

Sampling ==============================================================================================================

> toSamples              :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → [a]
> toSamples secs sig                       = take numSamples $ unfold $ strip sig
>   where
>     sr                                   = rate     (undefined :: p)
>     numChannels                          = numChans (undefined :: a)
>     numSamples                           = truncate (secs * sr)
>
> toFftSamples           :: ∀ a p. (AudioSample a, VU.Unbox a, Clock p) ⇒ Int → Signal p () a → VU.Vector a
> toFftSamples numSamples sig              = VU.fromList $ take numSamples $ unfold $ strip sig
>
> toSampleDubs           :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → [Double]
> toSampleDubs secs sig                    = take numDubs $ concatMap collapse $ unfold $ strip sig
>   where
>     sr                                   = rate     (undefined :: p)
>     numChannels                          = numChans (undefined :: a)
>     numDubs                              = truncate (secs * sr) * numChannels
>
> maxSample              :: ∀ p. (Clock p) ⇒ Double → Signal p () Double → Double
> maxSample dur sf                         = maximum $ map abs (toSamples dur sf)

Control Functions

The use of following functions requires that their input is normalized between 0 and 1
(And you can count on the output being likewise normalized!)

> controlLinear          :: Double → Double
> controlLinear d                          = d
>
> quarterCircleTable     :: Array Int Double
>                                            -- TODO: use Table
>                                          = array (0, qTableSize - 1) [(x, calc x) | x ← [0..(qTableSize - 1)]]
>   where
>     calc               :: Int → Double
>     calc i                               = 1 - sqrt (1 - c*c)
>       where
>         c              :: Double         = fromIntegral i / tableSize
>
> qTableSize             :: Int            = 1024
> tableSize              :: Double         = fromIntegral qTableSize
>
> controlConcave d
>   | d >= 1                               = 1
>   | otherwise                            = quarterCircleTable ! truncate (d * tableSize)
>
> controlConvex d
>   | (1 - d) >= 1                         = 1
>   | otherwise                            = 1 - (quarterCircleTable ! truncate ((1 - d) * tableSize))
>
> controlSwitch d                          = if d < 0.5
>                                              then 0
>                                              else 1
>
>
> class Coeff a where
>   azero                :: a
>   acomplex             :: a → Complex Double
>   aamp                 :: a → Double
>   ascale               :: Double → a → a
>   aadd                 :: a → a → a
>   amul                 :: a → a → a
>   asqrt                :: a → a
>
> instance Coeff Double where
>   azero                :: Double
>   azero                                  = 0
>   acomplex             :: Double → Complex Double
>   acomplex                               = (:+ 0)
>   aamp                 :: Double → Double
>   aamp                                   = abs
>   ascale               :: Double → Double → Double
>   ascale                                 = amul
>   aadd                 :: Double → Double → Double
>   aadd d e                               = d + e
>   amul                 :: Double → Double → Double
>   amul d e                               = d * e
>   asqrt                :: Double → Double
>   asqrt                                  = sqrt
>
> instance Coeff (Complex Double) where
>   azero                :: Complex Double
>   azero                                  = 0
>   acomplex             :: Complex Double → Complex Double
>   acomplex                               = id
>   aamp                 :: Complex Double → Double
>   aamp                                   = magnitude
>   ascale               :: Double → Complex Double → Complex Double
>   ascale s                               = amul (s :+ 0)
>   aadd                 :: Complex Double → Complex Double → Complex Double
>   aadd d e                               = d + e
>   amul                 :: Complex Double → Complex Double → Complex Double
>   amul d e                               = d * e
>   asqrt                :: Complex Double → Complex Double
>   asqrt                                  = sqrt
>
> data SlwRate
> instance Clock SlwRate where
>   rate _                                 = 4.41
> type SlwSF a b                           = SigFun SlwRate a b
>
> type Node = Int
>
> aEqual                 :: (Eq a, Show a) ⇒ a → a → Bool
> aEqual a b
>   | a /= b                               = error (show a ++ " and " ++ show b ++ " had to be equal!?")
>   | otherwise                            = True
>
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
>
> -- | Calculates all the nodes that are part of cycles in a graph.
> cyclicNodes :: Graph → [Node]
> cyclicNodes graph                        = (map fst . filter isCyclicAssoc . assocs) graph
>   where
>     isCyclicAssoc                        = uncurry (reachableFromAny graph)
>
> -- | In the specified graph, can the specified node be reached, starting out
> -- from any of the specified vertices?
> reachableFromAny :: Graph → Node → [Node] → Bool
> reachableFromAny graph node =
>   elem node . concatMap (Graph.reachable graph)

Conversion functions and general helpers ==============================================================================

> checkForNan            :: Double → String → Double
> checkForNan y msg                        =
>   profess
>     (not $ isNaN y || isInfinite y || isDenormalized y || abs y > 200_000)
>     (msg ++ " bad Double = " ++ show y)
>     y
>
> goodChar               :: Char → Bool
> goodChar c                               = isAscii c && not (isControl c)
>
> goodName               :: String → Bool
> goodName                                 = all goodChar
>
> fixName                :: String → String
> fixName                                  = map (\c → if goodChar c then c else '_')
>
> profess                :: Bool → String → a → a
> profess assertion msg something          = if not assertion
>                                              then error ("Failed assertion -- " ++ msg)
>                                              else something
>
> professInRange         :: (Eq a, Ord a, Show a) ⇒ (a, a) → a → String → a → a
> professInRange range val role            = profess
>                                              (val == clip range val)
>                                              (unwords ["out of", role, "range", show range, show val])
>
> deJust                 :: ∀ a. String → Maybe a → a
> deJust tag item                          = profess (isJust item) (unwords["expected Just for", tag]) (fromJust item)
>
> roundBy                :: Double → Double → Double
> roundBy p10 x                            = fromIntegral (round (p10 * x)) / p10
>
> sumOfWeightedInts      :: ∀ a. Num a ⇒ [Maybe a] → [a] → a
> sumOfWeightedInts xs ws                  = sum $ zipWith (\w x → w * fromMaybe 0 x) ws xs
>       
> addIntToWord           :: Word → Int → Word
> addIntToWord w i                         = fromIntegral sum
>   where
>     iw                 :: Int            = fromIntegral w
>     sum                :: Int            = iw + i
>
> integralize            :: ∀ a b. (Integral a, Num b) ⇒ Maybe (a, a) → Maybe (b, b)
> integralize mww                          =
>   case mww of
>     Nothing                              → Nothing
>     Just (w1, w2)                        → if w1 > w2 then Nothing else Just (fromIntegral w1, fromIntegral w2)
>
> wrap                   :: (Ord n, Num n) ⇒ n → n → n
> wrap val bound                           = if val > bound then wrap val (val-bound) else val
>
> accommodate            :: Ord n ⇒ (n, n) → n → (n, n)
> accommodate (xmin, xmax) newx            = (min xmin newx, max xmax newx)
>
> clip                   :: Ord n ⇒ (n, n) → n → n
> clip (lower, upper) val                  = min upper (max lower val)
>
> deriveRange            :: Integral n ⇒ n → n → [n]
> walkRange              :: Integral n ⇒ (n, n) → [n]
> deriveRange x y                          = if x >= y || y <= 0 then [] else [x..(y-1)]
> walkRange (x, y)                         = if x > y || y < 0 then [] else [x..y]
>
> almostEqual            :: Double → Double → Bool
> almostEqual 0 0                          = True
> almostEqual a b                          = epsilon > abs ((a - b) / (a + b))

Account for microtones specified by SoundFont scale tuning : 0 < x < 100 < 1200
Note result is incorrect overall when involves multiple root pitches

> calcMicrotoneRatio     :: AbsPitch → AbsPitch → Double → Double
> calcMicrotoneRatio rootp p x             = step ** fromIntegral (rootp - p)
>   where
>     step               :: Double         = 2 ** (x / 1_200)

Range theory ==========================================================================================================

Model rectilinear sub-space coverage; e.g. find unwanted (sub-)space overlaps. Each space (of nspaces) contains
exactly ndims (2 in the MIDI case) ranges. If dim is the value of a dimension then its overall range is implicitly
0..dim-1 -- the associated _specified_ space range carves out a subset thereof.

Say you have ndims=2 dimensions each of 64 extent. (Partially) covering overall 64x64 space are nspaces=3 "zones". 

Zone 1: 32..57 "pitch", 11..47 "velocity"
Zone 2: 21..40        , 20..21
Zone 3: 0..1          , 0..1

You see there is some overlap between Zone 1 and Zone 2.

> smashSubspaces         :: ∀ i . (Integral i, Ix i, Num i, Show i, VU.Unbox i) ⇒
>                           String → [i] → [(i, [Maybe (i, i)])] → Smashing i
> smashSubspaces tag dims spaces_
>   | traceIf trace_SS False               = undefined
>   | otherwise                            = Smashing tag dims spaces (developSmashStats svector) svector
>   where
>     spaces             :: [(i, [(i, i)])]
>     spaces                               = map (BF.second (zipWith (\dim → fromMaybe (0, dim-1)) dims)) spaces_
>
>     ndims              :: i              = genericLength dims
>     nspaces            :: i              = genericLength spaces
>
>     mag                :: Int            = fromIntegral $ product dims
>
>     svector            :: VU.Vector (i, i)
>     svector                              = foldl' sfolder (VU.replicate mag (0, 0)) spaces
>
>     sfolder            :: VU.Vector (i, i) → (i, [(i, i)]) → VU.Vector (i, i)
>     sfolder smashup (spaceId, rngs)      = VU.accum assignCell smashup (enumAssocs dims spaceId rngs)
>
>     assignCell         :: (i, i) → (i, i) → (i, i)
>     assignCell mfrom mto                 = (fst mto, snd mfrom + 1)
>
>     enumAssocs         ::  [i] → i → [(i, i)] → [(Int, (i, i))]
>     enumAssocs dims spaceId rngs         =
>       profess
>         (0 <= mag && mag <= 65_536 && all (uncurry validRange) (zip dims rngs))
>         (unwords ["enumAssocs: range violation", tag, show mag, show dims, show spaces])
>         (map (, (spaceId, 1)) indices)
>       where
>         indices        :: [Int]
>         indices                          =
>           map (fromIntegral . computeCellIndex dims) (traverse walkRange rngs)
>
>     trace_SS                             = unwords ["smashSubspaces", show (length spaces_), show spaces_]
>
> validRange             :: ∀ i . (Integral i, Ix i) ⇒ i → (i, i) → Bool
> validRange dim (r, s)                    = 0 <= dim && r <= s && inZRange r dim && inZRange s dim
>
> validCoords            :: ∀ i . (Integral i, Ix i, VU.Unbox i) ⇒ [i] → Smashing i → Bool
> validCoords coords smashup               = and $ zipWith inZRange coords smashup.smashDims
>
> lookupCellIndex        :: ∀ i . (Integral i, Ix i, Show i, VU.Unbox i) ⇒ [i] → Smashing i → (i, i)
> lookupCellIndex coords smashup           = try
>   where
>     try_                                 =
>       profess
>         (validCoords coords smashup)
>         (unwords ["lookupCellIndex", "invalid coords"])
>         (smashup.smashVec VU.! computeCellIndex smashup.smashDims coords)
>     try                                  =
>       if snd try_ > 0
>         then try_
>         else (snd $ minimum (map (measure coords) smashup.smashSpaces), 1)
>
>     measure            :: [i] → (i, [(i, i)]) → (Double, i)
>     measure coords space                 =
>       minimum (map (distance (fst space) coords) (listOutPoints (snd space)))
>
>     distance           :: i → [i] → [i] → (Double, i)
>     distance bix [] []                   = (0, bix)
>     distance bix (x:xs) (y:ys)           = (var + fst (distance bix xs ys), bix)
>       where
>         delta, var     :: Double
>         delta                            = fromIntegral (x - y)
>         var                              = delta * delta
>     distance _ _ _                       =
>       error $ unwords ["distance:", "input coords args have unequal lengths"]
>
> listOutPoints          :: ∀ i . (Integral i) ⇒ [(i, i)] → [[i]]
> listOutPoints []                         = [[]]
> listOutPoints ((r, s) : ranges)          = points1 ++ points2
>   where
>     points1                              = map ([r] ++) (listOutPoints ranges)
>     points2                              = map ([s] ++) (listOutPoints ranges)
>
> computeCellIndex       :: ∀ i . (Integral i) ⇒ [i] → [i] → Int
> computeCellIndex [] []                   = 0
> computeCellIndex (_:as) (b:bs)           = fromIntegral (b * product as) + computeCellIndex as bs
> computeCellIndex _ _                     =
>   error $ unwords ["computeCellIndex:", "input args dims and coords have unequal lengths"]
>
> allCellsEqualTo        :: ∀ i . (Integral i, Show i, VU.Unbox i) ⇒ Smashing i → Maybe (i, i)
> allCellsEqualTo smashup                  =
>   let
>     cand                                 = smashup.smashVec VU.! 0
>   in
>     if all (\j → cand == (smashup.smashVec VU.! j)) [0..(VU.length smashup.smashVec - 1)]
>       then Just cand
>       else Nothing
>
> data Smashing i                          =
>   Smashing {
>     smashTag            :: String
>     , smashDims         :: [i]
>     , smashSpaces       :: [(i, [(i, i)])]
>     , smashStats        :: SmashStats
>     , smashVec          :: VU.Vector (i, i)}
> instance ∀ i. (Integral i, Num i, Show i) ⇒ Show (Smashing i) where
>   show                 :: Smashing i → String
>   show Smashing{ .. }                    =
>     unwords ["Smashing", show (smashTag, smashStats)]
> sLength smashup                        = product smashup.smashDims
> data SmashStats                        =
>   SmashStats {
>     countNothings      :: Int
>   , countSingles       :: Int
>   , countMultiples     :: Int} deriving Show

> seedSmashStats                           = SmashStats 0 0 0
> developSmashStats      :: ∀ i. (Integral i, Show i, VU.Unbox i) ⇒ VU.Vector (i,i) → SmashStats
> developSmashStats                        = VU.foldl' sfolder seedSmashStats
>   where
>     sfolder            :: SmashStats → (i, i) → SmashStats
>     sfolder stats@SmashStats{ .. } (_, count)
>       | count == 0                       = stats{countNothings = countNothings + 1}
>       | count == 1                       = stats{countSingles = countSingles + 1}
>       | otherwise                        = stats{countMultiples = countMultiples + 1}
> fractionEmpty, fractionCovered
>                        :: ∀ i. (Integral i, Show i) ⇒ Smashing i → Rational
> fractionEmpty smashup                    = fromIntegral (countNothings smashup.smashStats) % fromIntegral (sLength smashup)
> fractionCovered smashup                  =
>   fromIntegral (countSingles smashup.smashStats + countMultiples smashup.smashStats) % fromIntegral (sLength smashup)
>
> inZRange d x                             = inRange (0, x - 1) d 
          
Raises 'a' to the power 'b' using logarithms.

> pow                    :: Floating a ⇒ a → a → a
> pow a b                                  = exp (log a * b)

Returns the fractional part of 'x'.

> frac                   :: RealFrac r ⇒ r → r
> frac                                     = snd . properFraction

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportScanName         :: FilePath       = "ScanReport'.log"
> reportTournamentName   :: FilePath       = "TournamentReport'.log"

Returns the amplitude ratio

> fromCentibels          :: Double → Double
> fromCentibels centibels                  = pow 10 (centibels/1000)
>
> toCentibels            :: Double → Double
> toCentibels ratio                        = logBase 10 (ratio * 1000)

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
>       maybe 0 fromIntegral mfact * fromIntegral (60 - key) / fromIntegral qMidiSize128 / 1_200
>
> toTimecents            :: Double → Int
> toTimecents secs                         = round $ logBase 2 secs * 1_200

Returns the amplitude ratio (based on input 10ths of a percent) 

> fromTithe              :: Maybe Int → Bool → Double
> fromTithe iS isVol                       =
>   if isVol
>     then 1 / fromCentibels jS
>     else (1000 - jS) / 1000
>   where
>     jS                 :: Double         = maybe 0 (fromIntegral . clip (0, 1000)) iS

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

Test runner

> runTests               :: [IO Bool] → IO ()
> runTests tests                           = do
>   results                                ← sequence tests
>   let nSuccesses                         = foldl' (\n t → n + if t then 1 else 0) 0 results
>   putStrLn $ unwords ["results =", show results]
>   putStrLn $ unwords ["  ", show nSuccesses, "/", show $ length results]
>
> runTestsQuietly        :: [IO Bool] → IO Bool
> runTestsQuietly tests                    = do
>   results                                ← sequence tests
>   let nSuccesses                         = foldl' (\n t → n + if t then 1 else 0) 0 results
>   return (nSuccesses == length results)

Mapping is used in SoundFont modulator

> data Mapping =
>   Mapping {
>     msContinuity     :: Continuity
>   , msBiPolar        :: Bool  
>   , msMax2Min        :: Bool
>   , msCCBit          :: Bool} deriving (Eq, Ord, Show)
>
> data Continuity =
>     Linear
>   | Concave
>   | Convex
>   | Switch deriving (Eq, Ord, Show, Enum)
>
> defMapping                               = Mapping Linear False False False
> allMappings                              = [Mapping cont bipolar max2min False
>                                                   | cont                  ← [Linear, Concave, Convex, Switch]
>                                                        , bipolar          ← [False, True]
>                                                              , max2min    ← [False, True]]                                          

Returns sample point as (normalized) Double

> sample24               :: Int16 → Word8 → Double
> sample24 i16 w8                          = fromIntegral (f16to32 i16 * 256 + f8to32 w8) / 8_388_608.0
>   where
>     f8to32             :: Word8 → Int32  = fromIntegral
>     f16to32            :: Int16 → Int32  = fromIntegral
>      
> sample16               :: Int16 → Double
> sample16 i16                             = fromIntegral i16 / 32_768.0
>
> samplePoint            :: A.SampleData Int16 → Maybe (A.SampleData Int8) → Int → Double
> samplePoint s16 ms8 ix                   = sample24 (s16 ! ix) (fromIntegral (maybe 0 (! ix) ms8))
>
> samplePointInterp      :: A.SampleData Int16 → Maybe (A.SampleData Int8) → Double → Int → Double
> samplePointInterp s16 ms8 offset ix      = s0 + offset * (s1 - s0)
>   where
>     (s0, s1)           :: (Double, Double)
>                                          = (  samplePoint s16 ms8 ix
>                                             , samplePoint s16 ms8 (ix + 1))

sampleUp returns power of 2 greater than OR EQUAL TO the input value (result at least 2**14)
sampleDown returns power of 2 less than OR EQUAL TO the input value (input enforced <= 2**31)
breakUp returns a list of integers approximating divisions of a floating point range

> sampleUp               :: Int → Int
> sampleUp i                               =
>   if i <= 0
>     then error "out of range for sampleUp"
>     else max 16_384 (head $ dropWhile (< i) (iterate' (* 2) 1))
>
> sampleDown             :: Int → Int
> sampleDown i                             =
>   if i <= 0 || i > 2_147_483_648
>     then error "out of range for sampleDown"
>     else head $ dropWhile (> i) (iterate' (`div` 2) 2_147_483_648)
>
> breakUp :: (Double, Double) → Double → Int → [Int]
> breakUp (xmin, xmax) base nDivs =
>   let
>     (ymin, ymax) =
>       if base == 0
>         then (xmin, xmax)
>         else (logBase base xmin, logBase base xmax)
>     delta = (ymax - ymin) / fromIntegral nDivs
>     oper =
>       if base == 0
>         then id
>         else pow base
>   in
>     map (round . oper . (+ ymin) . (* delta) . fromIntegral) ([0..nDivs] :: [Int])
>    
> theE' :: Complex Double
> theE' = theE :+ 0
>
> theJ :: Complex Double
> theJ = 0 :+ 1
>
> zshow                  :: ∀ a . a → String
> zshow a                                  = "list"

Emission capability ===================================================================================================

> data Emission                            = 
>   ToFieldL String Int
>   | ToFieldR String Int
>   | Unblocked String
>   | Blanks Int
>   | Empty 
>   | EndOfLine deriving Show
>
> makeString             :: Emission → String
> makeString e                             =
>   case e of
>     ToFieldL str sz    → if len > sz then error $ unwords ["overflowL", show sz, show len, show str]
>                                      else fillFieldL sz str
>       where
>         len                              = length str
>     ToFieldR str sz    → if len > sz then error $ unwords ["overflowR", show sz, show len, show str]
>                                      else fillFieldR sz str
>       where
>         len                              = length str
>     Unblocked str      → str
>     Blanks sz          → replicate sz ' '
>     Empty              → ""
>     EndOfLine          → "\n"
>
> emitLine               :: [Emission] → [Emission]
> emitLine es                              = singleton literate ++ es ++ singleton EndOfLine
>
> literate               :: Emission       = ToFieldL ">" 2
>
> commaOrNot             :: Int → Emission
> commaOrNot nth                           =
>   if nth == 0
>     then ToFieldL ""  2
>     else ToFieldL "," 2
>
> parens                 :: [Emission] → [Emission]
> parens es                                = [Unblocked "("] ++ es ++ [Unblocked ")"]
>
> bracks                 :: [Emission] → [Emission]
> bracks es                                = [Unblocked "["] ++ es ++ [Unblocked "]"]
>
> comma                  :: Emission       = Unblocked ", "
>
> emitComment            :: [Emission] → [Emission]
> emitComment es                           = [EndOfLine] ++ es ++ [EndOfLine, EndOfLine]
>
> emitNextComment        :: [Emission] → [Emission]
> emitNextComment es                       = es ++ [EndOfLine, EndOfLine]
>
> emitShowL              :: (Show a) ⇒ a → Int → Emission
> emitShowL a                              = ToFieldL (show a)
>
> emitShowR              :: (Show a) ⇒ a → Int → Emission
> emitShowR a                              = ToFieldR (show a)
>
> emitDefault            :: (Show a) ⇒ a → Emission
> emitDefault a                            = Unblocked (show a)
>
> gmId                   :: (Show a) ⇒ a → Emission
> gmId i                                   = emitShowL i 22
>
> reapEmissions          :: [Emission] → String
> reapEmissions                            = concatMap makeString
>
> fillFieldL             :: Int → String → String
> fillFieldL fieldSz str                   = str ++ safeReplicate (length str) fieldSz ' '
>
> fillFieldR             :: Int → String → String
> fillFieldR fieldSz str                   = safeReplicate (length str) fieldSz ' ' ++ str
>
> safeReplicate          :: Int → Int → Char → String
> safeReplicate sz maxSz                   = replicate (maxSz - sz)
>
> emitTaggedLine         :: String → String → String → [Emission]
> emitTaggedLine prolog item epilog        = emitLine [Unblocked prolog, Unblocked item, Unblocked epilog]
>
> emitPragmaLine lang                      = emitTaggedLine "{-# LANGUAGE " lang " #-}"
> emitModuleLine mod                       = emitTaggedLine "module " mod " where"
> emitImportLine imp                       = emitTaggedLine "import " imp ""
>
> writeFileBySections    :: FilePath → [[Emission]] → IO ()
> writeFileBySections fp eSections         = do
>   mapM_ (appendFile fp . reapEmissions) eSections
>
> type Velocity                            = Volume
> type KeyNumber                           = AbsPitch

Configurable parameters ===============================================================================================

> doRender                                 = controlSettingsQqDoRender                   defC
> diagnosticsEnabled                       = controlSettingsQqDiagnosticsEnabled         defC 
> reportScan                               = controlSettingsQqReportScan                 defC 
> reportTourney                            = controlSettingsQqReportTourney              defC 
> skipGlissandi                            = controlSettingsQqSkipGlissandi              defC
> replacePerCent                           = controlSettingsQqReplacePerCent             defC

Edit the following ====================================================================================================

> defC                   :: ControlSettings
> defC                                     = ControlSettings True True True True False 0

The End
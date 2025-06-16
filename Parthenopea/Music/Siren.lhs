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
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UnicodeSyntax #-}

Siren
William Clements
December 12, 2022

> module Parthenopea.Music.Siren where
>
> import qualified Codec.Midi              as M
> import Control.Arrow.ArrowP
> import Control.DeepSeq (NFData)
> import Control.SF.SF
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Either
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List hiding (transpose)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord
> import Data.Ratio ( approxRational )
> import qualified Data.Vector.Unboxed     as VU
> import Data.Word
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Types
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.MidiIO ( unsafeOutputID )
> import Euterpea.IO.MIDI.Play
> import Euterpea.IO.MIDI.ToMidi2 ( writeMidi2 )
> import Euterpea.Music
> import Parthenopea.Debug
> import System.Random ( Random(randomR), StdGen )

> type Velocity                            = Volume
> type KeyNumber                           = AbsPitch
>
> allKinds               :: ([InstrumentName], [PercussionSound])
> allKinds                                 =
>   (  map toEnum [fromEnum AcousticGrandPiano .. fromEnum Gunshot]
>    , map toEnum [fromEnum AcousticBassDrum .. fromEnum OpenTriangle])
>
> percPitchRange         :: (AbsPitch, AbsPitch)
> percPitchRange                           = (fromEnum AcousticBassDrum + 35, fromEnum OpenTriangle + 35)
>
> type GMKind                              = Either InstrumentName PercussionSound

Utilities =============================================================================================================

> hzToAp                 :: Double → AbsPitch
> hzToAp freak                             =
>   round $ fromIntegral (absPitch (A,4)) + 12 * (logBase 2 freak - logBase 2 440)
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
> addDur durA ns  =  let fun n = n durA
>                    in line (map fun ns)
>
> grace                  :: Int → Music Pitch → Music Pitch
> grace n (Prim (Note durG p))             = note (est * durG) (trans n p) :+: note ((1 - est) * durG) p
>   where
>     est                :: Rational
>     est
>       | durG < 1/8                       = 1/2
>       | durG > (2*wn)                    = 1/(8*durG)
>       | otherwise                        = 1/8
> grace _ _                                = 
>   error "Can only add a grace note to a note."
>
> t32                    :: [Music a] → Music a
> t32 notes                                = tempo (3/2) (foldr (:+:) (rest 0) notes)
>
> dim                    :: Rational → Music a → Music a
> dim amt                                  = phrase [Dyn (Diminuendo amt)]
>
> captureMidi            :: Music (Pitch, [NoteAttribute]) → IO()
> captureMidi                              = writeMidi2 "Siren.mid"
>
> fractionOf             :: Int → Double → Int
> fractionOf x doub                        = min 127 $ round $ doub * fromIntegral x
>
> slur                   :: Rational → Music a → Music a
> slur rateS                               = Modify (Phrase [Art (Slurred rateS)])
>
> ratEps                 :: Double
> ratEps                                   = 0.000_1
>
> approx                 :: Double → Dur
> approx durA                              = approxRational durA ratEps
>
> rawPitches             :: [AbsPitch]
> rawPitches                               = [0..127]
>
> allPitches             :: Music Pitch
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
>     , perfAlg= map (\mev → mev{eDur = max 0 (eDur mev - 0.000_001)}) . perform}

This alternate playback function will merge overlapping notes, 
which makes for a cleaner sound on some synthesizers:

> playX        :: (NFData a, ToMusic1 a) ⇒ Music a → IO ()
> playX = playC defParams{perfAlg = eventMerge . perform} where 
>     eventMerge :: Performance → Performance
>     eventMerge (e1:e2:mevs) = 
>         let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
>         in  if ePitch e1 == ePitch e2 then eventMerge (e1':mevs) 
>             else e1 : eventMerge (e2:mevs)
>     eventMerge mev = mev

"triad" ===============================================================================================================

> triad                  :: PitchClass → Mode → Pitch → Dur → Music Pitch
> triad key mode base durT                 = chord [n1, n2, n3]
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
>     n1                                   = note durT $ pitch bP
>     n2                                   = note durT $ pitch (bP + head          is)
>     n3                                   = note durT $ pitch (bP + (head . tail) is)
>
>     major                                = [0, 4, 7, 12, 16]
>     minor                                = [0, 3, 7, 12, 15]
>     dimn                                 = [0, 3, 6, 12, 15]
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
>       | Locrian              == mode     = dimn
>       | CustomMode "Sus4"    == mode     = sus4
>       | CustomMode "Sus2"    == mode     = sus2
>       | CustomMode "Dim"     == mode     = dimn
>       | CustomMode "Aug"     == mode     = aug
>       | CustomMode "Chrome"  == mode     = chrome
>       | otherwise                        = error "Requested Mode not supported"
>
>     offsets2intervals  :: [AbsPitch] → [AbsPitch]
>     offsets2intervals os
>       | apD == aI                        = (bI - aI) : [cI - aI]
>       | apD == bI                        = (cI - bI) : [dI - bI]
>       | apD == cI                        = (dI - cI) : [eI - cI]
>       | otherwise                        = error "Malformed Triad"
>       where
>         aI                               = head os
>         bI                               = os!!1
>         cI                               = os!!2
>         dI                               = os!!3
>         eI                               = os!!4

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
> shiftMode              :: Integral a ⇒ Int → [a] → [a]
> shiftMode k t                            =
>   let
>     n                                    = length t
>     t'                                   = t ++ t
>     v                                    = 12 - t' !! k
>   in
>     take n $ map (\x → (v + x) `mod` 12) (drop k t')
>
> chromatic, diminished, augmented
>                        :: Mode         
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
> squeezeAPSequence gliss durS
>   | skipGlissandi                         = rest durS
>   | length gliss < 6                      = error $ unwords [fName, "not enough notes"]
>   | durS < 1 / 8                          = error $ unwords [fName, "not enough duration"]
>   | otherwise                             = chord [rest durS, dim 1 $ slur 2 $ line notes]
>   where
>     fName                                = "squeezeAPSequence"
>
>     reach              :: Rational       = fromIntegral $ length gliss
>     notes              :: [Music Pitch]  = [note (durS * 9 / (reach * 10)) (pitch x) | x ← gliss]
>
> extendModeToInfinity   :: Bool → AbsPitch → [AbsPitch] → [(AbsPitch, Int)]
> extendModeToInfinity desc start templ8   = iterate' doNext (start, 0)
>   where
>     doNext             :: (AbsPitch, Int) → (AbsPitch, Int)
>     doNext (ap, ix)                      =
>       let  
>         (ix', offs)
>           | desc                         = if ix == 0 then (nT - 1, -12) else (ix - 1, 0)
>           | ix + 1 == nT                 = (0, 12)
>           | otherwise                    = (ix + 1, 0)
>  
>         nT                               = length templ8
>         delta                            = templ8 !! ix' - templ8 !! ix  + offs
>       in
>         (ap + delta, ix')
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

>   -- calibrate (0,1) to a desired range e.g. (24,92)
> denorm                 :: Double → (Double, Double) → Double
> denorm r (lo, up)                        = lo + r * (up-lo)
>
> randomNorms            :: StdGen → [Double]
> randomNorms genR                         = x : randomNorms genR'
>    where
>      (x, genR')                          = randomR (0,1) genR
>
> randomNormSets         :: Int → StdGen → [[Double]]
> randomNormSets n genR                    = takeWhile (not . null) $ unfoldr (Just . splitAt n) (randomNorms genR)
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
>     judgeScore cand                      = (\j → if nonPitchedInstrument j then Nothing else Just j) cand
>                                            >>= instrumentRange >>= uncurry (fitsIn cand)
>     
>     fitsIn             :: InstrumentName → AbsPitch → AbsPitch → Maybe (Int, InstrumentName)
>     fitsIn cand rangeLo rangeHi
>       | traceNever trace_FI False        = undefined
>       | otherwise                        =
>         if inRange rng playedLo && inRange rng playedHi 
>           then Just (snd rng - fst rng, cand)
>           else Nothing
>       where
>         rng                              = (rangeLo, rangeHi)
>         trace_FI                         = unwords ["fitsIn", show rng, show playedLo, show playedHi]
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
> wideOpen               :: (AbsPitch, AbsPitch)
> wideOpen                                       = (0, 127)

instrument range checking =============================================================================================

> union2Ranges           :: (Ord a, Ord b) ⇒ (a, b) → (a, b) → (a, b)
> union2Ranges r1 r2                       = unionRanges (r1:[r2])
> unionRanges            :: (Ord a, Ord b) ⇒ [(a, b)] → (a, b)
> unionRanges []                           = error "empty range list"
> unionRanges (r:rs)                       = ( minimum (map fst (r:rs))
>                                            , maximum (map snd (r:rs)) )
> intersect2Ranges       :: Ord b ⇒ (b, b) → (b, b) → Maybe (b, b)
> intersect2Ranges r1 r2                   = intersectRanges (r1:[r2])
> intersectRanges        :: Ord b ⇒ [(b, b)] → Maybe (b, b)
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
> bendNote bp pc o durB bend               = note durB ((pc, o), [Volume bp.bpVelocity, Params [fromIntegral bend]])
>
> addVolume'    :: BandPart → Music Pitch → Music (Pitch,[NoteAttribute])
> addVolume' bp                            = mMap (, [Volume bp.bpVelocity])
>
> psoundToPitch :: PercussionSound → Pitch
> psoundToPitch psound                     = pitch (fromEnum psound + 35)

examine song for instrument and percussion usage ======================================================================

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
> defShredding           :: Shredding
> defShredding                             = Shredding Map.empty []
>
> getGMKind              :: MEvent → GMKind
> getGMKind MEvent{eInst, ePitch}          =
>   case eInst of
>     Percussion                           → Right $ toEnum (ePitch - 35)
>     _                                    → Left eInst
>
> shimSong                :: Music (Pitch, [NoteAttribute]) → DynMap → Music (Pitch, [NoteAttribute])
> shimSong m _                             = m
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
> critiqueMusic Shredding{shRanges}        =
>   Shredding shRanges (concatMap critiqueShred (Map.assocs shRanges))
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
>     (instr, rng)                       =
>       case kind of
>         Left iname                       → (iname, fromMaybe wideOpen (instrumentRange iname))
>         _                                → (Percussion, wideOpen)
>   in critiqueNote instr rng shLowNote ++ critiqueNote instr rng shHighNote
> 
> critiqueNote           :: InstrumentName → (AbsPitch, AbsPitch) → MEvent → [(InstrumentName, [String])]
> critiqueNote name rng mev              =
>   let
>     p                                    = mev.ePitch
>   in
>     if inRange rng p
>       then []
>       else singleton (name, singleton $ unwords ["...", show p, "out of range", show rng])
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
>       Left _                             → show v
>                                            ++ show (pitch mev.ePitch)
>                                            ++ showOutOfRangeIndicator mev.ePitch
>       _                                  → show v
>     where
>       v                :: Double
>       v                                  = fromRational mev.eTime
>   showOutOfRangeIndicator p              = if isNothing mrange || inRange (deJust "range" mrange) p
>                                              then "."
>                                              else "!"

music-related utilities ===============================================================================================
          some code _copied_ from HSoM and Euterpea - if you feel strongly, I'll do it the right way with exports

> defaultContext         :: MContext
> defaultContext                           =
>   MContext
>     {  mcTime = 0
>      , mcInst = AcousticGrandPiano
>      , mcDur = metro 120 qn
>      , mcVol = 100}
>
> metro                  :: Int → Dur → DurT
> metro setting durM                       = 60 / (fromIntegral setting * durM)
>
> trill                  :: Int → Dur → Music Pitch → Music Pitch
> trill i sDur (Prim (Note tDur p))        =
>    if sDur >= tDur  then note tDur p
>                     else  note sDur p :+: 
>                           trill  (negate i) sDur 
>                                  (note (tDur-sDur) (trans i p))
> trill i dDur (Modify (Tempo r) m)        = tempo r (trill i (dDur*r) m)
> trill i dDur (Modify cont m)             = Modify cont (trill i dDur m)
> trill _ _ _                              = 
>       error "trill: input must be a single note."
>
> trill'                 :: Int → Dur → Music Pitch → Music Pitch
> trill' i sDur m                          = trill (negate i) sDur (transpose i m)
>
> trilln                 :: Int → Int → Music Pitch → Music Pitch
> trilln i nTimes m                        = trill i (dur m / fromIntegral nTimes) m
>
> trilln'                :: Int → Int → Music Pitch → Music Pitch
> trilln' i nTimes m                       = trilln (negate i) nTimes (transpose i m)
>
> roll                   :: Dur → Music Pitch → Music Pitch
> rolln                  :: Int → Music Pitch → Music Pitch
>
> roll                                     = trill  0
> rolln                                    = trilln 0
>
> extractTimes           :: Performance → [Double]
> extractTimes                             = map (fromRational . eTime)
>
> percussionLimit        :: Double
> percussionLimit                          = fromIntegral $ fromEnum OpenTriangle
>
> pinnedKR               :: [PercussionSound] → (AbsPitch, AbsPitch) → Maybe (AbsPitch, AbsPitch)
> pinnedKR pss (p1, p2)                    = if qualifies then Just (p1, p2) else Nothing                   
>   where
>     qualifies                            = (p2 < p1 + 2) && any available [p1 .. p2]
>     available          :: AbsPitch → Bool
>     available ap                         = maybe False (`elem` pss) (pitchToPerc ap)
>
> pitchToPerc            :: AbsPitch → Maybe PercussionSound
> pitchToPerc ap                           =
>   let
>     ad                                   = ap - 35
>   in
>     if ad >= fromEnum AcousticBassDrum && ad <= fromEnum OpenTriangle
>       then Just (toEnum ad)
>       else Nothing

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
> aggrandize (Prim (Note durN (p, v)))     = Prim (Note durN (p, [Volume v]))
> aggrandize (Prim (Rest durN))            = Prim (Rest durN)
> aggrandize (m1 :+: m2)                   = aggrandize m1 :+: aggrandize m2
> aggrandize (m1 :=: m2)                   = aggrandize m1 :=: aggrandize m2
> aggrandize (Modify ctrl m)               = Modify ctrl (aggrandize m)

-----------------------------------------------------------------------------------------------------------------------

> eutSplit               :: ∀ p . Clock p ⇒ Signal p Double (Double, Double)
> eutSplit                                 =
>   proc sIn → do
>     outA                                 ⤙ (sIn, sIn)

Sampling ==============================================================================================================

> toSamples              :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → [a]
> toSamples secs sig                       = take numSamples $ unfold $ strip sig
>   where
>     sr                                   = rate     (undefined :: p)
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
> maxSample durS sf                        = maximum $ map abs (toSamples durS sf)
>
> data SlwRate
> instance Clock SlwRate where
>   rate _                                 = 4.41
> type SlwSF a b                           = SigFun SlwRate a b

Conversion functions and general helpers ==============================================================================

> roundBy                :: Double → Double → Double
> roundBy p10 x                            = fromIntegral rnd / p10
>   where
>     rnd                :: Int
>     rnd                                  = round (p10 * x)

Returns sample point as (normalized) Double

> sample24               :: Int16 → Word8 → Double
> sample24 i16 w8                          = fromIntegral (f16to32 i16 * 256 + f8to32 w8) / 8_388_608.0 -- 1_048_576
>   where
>     f8to32             :: Word8 → Int32  = fromIntegral
>     f16to32            :: Int16 → Int32  = fromIntegral
>      
> samplePoint            :: A.SampleData Int16 → Maybe (A.SampleData Int8) → Int → Double
> samplePoint s16 ms8 ix                   = sample24 (s16 ! ix) (fromIntegral (maybe 0 (! ix) ms8))
>
> samplePointInterp      :: A.SampleData Int16 → Maybe (A.SampleData Int8) → Double → Int → Double
> samplePointInterp s16 ms8 offs ix      = s0 + offs * (s1 - s0)
>   where
>     (s0, s1)           :: (Double, Double)
>                                          = (  samplePoint s16 ms8 ix
>                                             , samplePoint s16 ms8 (ix + 1))
>
> qMidiSize128           :: Int
> qMidiSize128                             = 128
> qMidiSizeSpace         :: Int
> qMidiSizeSpace                           = qMidiSize128 * qMidiSize128

Configurable parameters ===============================================================================================

> doRender, skipGlissandi
>                        :: Bool
> replacePerCent         :: Rational

Edit the following ====================================================================================================

> doRender                                 = True
> skipGlissandi                            = False
> replacePerCent                           = 0

The End
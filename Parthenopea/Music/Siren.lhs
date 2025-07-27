> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Siren
William Clements
December 12, 2022

> module Parthenopea.Music.Siren where
>
> import Control.Arrow.ArrowP
> import Control.DeepSeq ( NFData )
> import Control.SF.SF ( unfold )
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Either
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List hiding (transpose)
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Ord ( comparing )
> import Data.Ratio ( approxRational )
> import qualified Data.Vector.Unboxed     as VU
> import Data.Word ( Word8 )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Types
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.MidiIO ( unsafeOutputID )
> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import Parthenopea.Debug
> import System.Random ( Random(randomR), StdGen )
>  
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
> approximate            :: Velocity → StdLoudness → Double
> approximate v loud                       =
>   let
>     v', prox           :: Double
>     v'                                   = fromIntegral v
>     prox                                 = 
>       case loud of
>           PPP  → v' * 40;    PP → v' * 50;   P    → v' * 60
>           MP   → v' * 70;    SF → v' * 80;   MF   → v' * 90
>           NF   → v' * 100;   FF → v' * 110;  FFF  → v' * 120
>   in
>     prox / 128
>     
> t32                    :: [Music a] → Music a
> t32 notes                                = tempo (3/2) (foldr (:+:) (rest 0) notes)
>
> data PassageAttribute                    =
>   Levels StdLoudness StdLoudness
>   | Bends Double Double deriving (Show, Eq, Ord)
>
> hashNote               :: AbsPitch → Dur → String
> hashNote hAp hD                          = unwords [show hAp, "_-_", show hD]
>
> passage               :: BandPart → [PassageAttribute] → Music Pitch → Music1
> passage bp pas ma                        = infuse ma
>   where
>     fName_                               = "passage"
>
>     v                 :: Velocity
>     v                                    = bp.bpHomeVelocity
>
>     ma'               :: Music1
>     ma'                                  = toMusic1 ma
>
>     evs               :: [MEvent]
>     allDur_           :: Dur
>     allDur            :: Double
>     (evs, allDur_)                       = musicToMEvents (bandPartContext bp) ma'
>     allDur                               = fromRational allDur_
>
>     eMap              :: Map String MEvent
>     eMap                                 = foldl' eFolder Map.empty evs
>       where
>         eFolder eFold mev                = Map.insert
>                                              (hashNote mev.ePitch mev.eDur) 
>                                              mev
>                                              eFold

    Make a function to compute instantaneous Velocity given an elapsed time

>     velocityPerTime    :: Double → Double
>     velocityPerTime tIn                  = tIn * changeRate + fst overall
>
>     overall            :: (Double, Double)
>     overall                              =
>       case head pas of
>         Levels stL enL                   → (approximate v stL, approximate v enL)
>         _                                → error $ unwords [fName_, "unsupported PassageAttribute"]
>
>     changeRate         :: Double
>     changeRate                           = (snd overall - fst overall) / allDur
>
>     infuse             :: Music Pitch → Music1
>     infuse
>       | traceNow trace_I False           = undefined
>       | otherwise                        = mFold pFun (:+:) (:=:) gFun
>       where
>         fName                            = "infuse"
>         trace_I                          = unwords [fName, show (length evs), "= #evs", show changeRate]
>
>         pFun           :: Primitive Pitch → Music1
>         pFun (Note durI pitchI)          = note durI (pitchI, na (absPitch pitchI) durI)
>         pFun (Rest durI)                 = rest durI
>
>         na             :: AbsPitch → Dur → [NoteAttribute]
>         na kAp kDur                      =
>           let
>             key        :: String
>             key                          = hashNote kAp kDur
>             mev        :: Maybe MEvent
>             mev                          = tracer "lookup" $ Map.lookup key eMap
>           in
>             case mev of
>               Nothing                    → []
>               Just ev                    → [Dynamics fName, Params (velos ev)]
>           where
>             velos      :: MEvent → [Double]
>             velos ev                     =
>               let
>                 onset, delta
>                        :: Double
>                 onset                    = fromRational ev.eTime
>                 delta                    = fromRational ev.eDur
>               in
>                 [velocityPerTime onset, velocityPerTime (onset + delta)]
>            
>         gFun           :: Control → Music1 → Music1
>         gFun cont m1                      = Modify cont (toMusic1 m1)
>
> dim                    :: Rational → Music a → Music a
> dim amt                                  = phrase [Dyn (Diminuendo amt)]
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
> chordFromArray         :: Array Int Music1 → Music1
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

> instrumentAbsPitchRange
>                        :: InstrumentName → Maybe (AbsPitch, AbsPitch)
> instrumentAbsPitchRange inst             = mrange >>= Just . BF.bimap absPitch absPitch
>   where
>     mrange                               = instrumentPitchRange inst
>
> instrumentPitchRange   :: InstrumentName → Maybe (Pitch, Pitch)
> instrumentPitchRange inst                =
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
>                                    $ map (fromJust . instrumentPitchRange)
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
>                                          =
>   if null rangedInsts
>     then than
>     else snd $ minimumBy (comparing fst) rangedInsts
>   where
>     rangedInsts        :: [(Int, InstrumentName)]
>     rangedInsts                          = mapMaybe judgeScore (fst allKinds)
>
>     judgeScore         :: InstrumentName → Maybe (Int, InstrumentName)
>     judgeScore cand                      = (\j → if nonPitchedInstrument j then Nothing else Just j) cand
>                                            >>= instrumentAbsPitchRange >>= uncurry (fitsIn cand)
>     
>     fitsIn             :: InstrumentName → AbsPitch → AbsPitch → Maybe (Int, InstrumentName)
>     fitsIn cand rangeLo rangeHi          =
>       let
>         rng                              = (rangeLo, rangeHi)
>       in
>         if inRange rng playedLo && inRange rng playedHi 
>           then Just (snd rng - fst rng, cand)
>           else Nothing
>
> selectRanged           :: [InstrumentName] → Array Int (InstrumentName, (AbsPitch, AbsPitch))
> selectRanged is                          = listArray (1, length qual) qual
>   where
>     qual                                 = mapMaybe (\i → instrumentAbsPitchRange i >>= Just . (i,)) is
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

> unionRanges            :: (Ord a, Ord b) ⇒ [(a, b)] → (a, b)
> unionRanges []                           = error "empty range list"
> unionRanges (r:rs)                       = ( minimum (map fst (r:rs))
>                                            , maximum (map snd (r:rs)) )
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
>   , bpHomeVelocity     :: Velocity} deriving Show
> bandPartContext        :: BandPart → MContext
> bandPartContext bp = MContext {mcTime = 0, mcInst = bp.bpInstrument, mcDur = metro 120 qn, mcVol=127}
> relativeRange          :: BandPart → (Pitch, Pitch)
> relativeRange bp                         =
>   let
>     recenter           :: AbsPitch → Pitch
>     recenter                             = trans (-bp.bpTranspose) . pitch
>   in
>     BF.bimap recenter recenter (fromJust (instrumentAbsPitchRange bp.bpInstrument))
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
> replace bp dynMap                        = bp{bpInstrument = ninst}
>   where
>     minst                                = Map.lookup bp.bpInstrument dynMap
>     ninst                                =
>       if not (nonPitchedInstrument bp.bpInstrument) && isJust minst
>         then fromJust minst
>         else bp.bpInstrument
>
> makeDynMap             :: Map GMKind Shred → DynMap
> makeDynMap ding                          =
>   if replacePerCent > 50
>     then foldl' maker Map.empty (Map.assocs ding)
>     else Map.empty
>   where
>     maker              :: DynMap → (GMKind, Shred) → DynMap
>     maker i2i (kind, shred)              =
>       let
>         inameL, inameR :: InstrumentName
>         inameL                           = fromLeft (error "makeDynMap problem: no instrument") kind
>         inameR                           =
>           findBetterInstrument inameL (ePitch shred.shLowNote, ePitch shred.shHighNote)
>       in
>         if isLeft kind && inameL /= inameR
>           then Map.insert inameL inameR i2i
>           else i2i
>
> bandPart               :: BandPart → Music Pitch → Music1
> bandPart bp                              = mMap bChanger . instrument bp.bpInstrument . transpose bp.bpTranspose
>   where
>     bChanger           :: Pitch → (Pitch, [NoteAttribute])
>     bChanger p                           = (p, [Volume bp.bpHomeVelocity])
>
> orchestraPart          :: BandPart → Music1 → Music1
> orchestraPart bp                         = mMap oChanger . instrument bp.bpInstrument . transpose bp.bpTranspose
>   where
>     oChanger           :: (Pitch, [NoteAttribute]) → (Pitch, [NoteAttribute])
>     oChanger (p, nas)                    = (p, map nasFun nas)
>
>     nasFun             :: NoteAttribute → NoteAttribute
>     nasFun (Volume _)                    = Volume bp.bpHomeVelocity
>     nasFun na                            = na 
>
> bendNote               :: BandPart → PitchClass → Octave → Dur → AbsPitch → Music1
> bendNote bp pc o durB bend               =
>   note durB ((pc, o), [Volume bp.bpHomeVelocity, Params [fromIntegral bend]])
>
> addVolume'    :: BandPart → Music Pitch → Music1
> addVolume' bp                            = mMap (, [Volume bp.bpHomeVelocity])
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
> data Song                                =
>   Song {
>     songName           :: String
>   , songMusic          :: DynMap → Music1
>   , songShredding      :: Map GMKind Shred}
> songTimeAndNoteCount   :: Song → String
> songTimeAndNoteCount song                =
>   let
>     sec                :: Double         = (fromRational . dur) (song.songMusic Map.empty)
>     sec'               :: Int            = round sec
>     notes              :: Int            = Map.foldr ((+) . shCount) 0 song.songShredding
>   in
>     unwords [show sec', "sec,", show notes, "notes"]
>
> captureSong            :: Song → IO Song
> captureSong (Song name music _)          = do
>   ding                                   ← shredMusic $ music Map.empty
>   return $ Song name music ding
>
> getGMKind              :: MEvent → GMKind
> getGMKind MEvent{eInst, ePitch}          =
>   case eInst of
>     Percussion                           → Right $ toEnum (ePitch - 35)
>     _                                    → Left eInst
>
> shredMusic              :: ToMusic1 a ⇒ Music a → IO (Map GMKind Shred)
> shredMusic m                             =
>   return $ foldl' shFolder Map.empty $ fst (musicToMEvents defaultContext (toMusic1 m))
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
> critiqueShred (kind, shred)              =
>   let
>     (instr, rng)                         =
>       case kind of
>         Left iname                       → (iname, fromMaybe wideOpen (instrumentAbsPitchRange iname))
>         _                                → (Percussion, wideOpen)
>   in critiqueNote instr rng shred.shLowNote ++ critiqueNote instr rng shred.shHighNote
> 
> critiqueNote           :: InstrumentName → (AbsPitch, AbsPitch) → MEvent → [(InstrumentName, [String])]
> critiqueNote name rng mev                =
>   let
>     p                                    = mev.ePitch
>   in
>     if inRange rng p
>       then []
>       else singleton (name, singleton $ unwords ["...", show p, "out of range", show rng])
>
> shFolder               :: Map GMKind Shred → MEvent → Map GMKind Shred
> shFolder ding mev                        =
>   let
>     kind               :: GMKind         = getGMKind mev
>     mshred             :: Maybe Shred    = Map.lookup kind ding
>   in
>     case mshred of
>       Nothing                            → Map.insert kind (Shred mev mev 1) ding
>       Just shred                         → Map.insert kind (upd shred)       ding
>   where
>     upd shred                            =
>       Shred
>         (if ePitch mev < ePitch shred.shLowNote  then mev else shred.shLowNote)
>         (if ePitch mev > ePitch shred.shHighNote then mev else shred.shHighNote)
>         (shred.shCount + 1)
>
> printShreds            :: (Map GMKind Shred) → IO ()
> printShreds ding                         = 
>   mapM_ (uncurry printShred) (Map.assocs ding)
>   
> printShred             :: GMKind → Shred → IO ()
> printShred kind shred                    = do
>   putStrLn showGivenRange
>   putStrLn showLowHighNotes
>
>   where
>   mrange                                 =
>     case kind of
>       Left iname                         → instrumentAbsPitchRange iname
>       _                                  → Nothing
>   showGivenRange                         = showKind ++ "(" ++ show shred.shCount ++ ")" ++ " = " ++ showAvail
>     where
>       showAvail                          = maybe "" (\r → show (fst r) ++ " .. " ++ show (snd r)) mrange
>   showKind                               =
>     case kind of
>       Left iname                         → show iname
>       Right psound                       → show psound
>   showLowHighNotes                       =
>     case kind of
>       Left _                             → showShred shred.shLowNote ++ "\n" ++ showShred shred.shHighNote ++ "\n" 
>       _                                  → showShred shred.shLowNote ++ "\n" 
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
>
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
> qMidiSize128           :: Word
> qMidiSize128                             = 128

Configurable parameters ===============================================================================================

> skipGlissandi          :: Bool
> replacePerCent         :: Rational

Edit the following ====================================================================================================

> skipGlissandi                            = False
> replacePerCent                           = 0

The End
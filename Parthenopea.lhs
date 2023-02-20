> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

Parthenopea
William Clements
December 12, 2022

> module Parthenopea where

> import Control.DeepSeq (NFData)
> import Data.Array
> import Data.Maybe ( fromJust, isJust )
> import Data.Ratio (approxRational)
> import Debug.Trace ( trace )
> import Euterpea.IO.Audio.IO
> import Euterpea.IO.Audio.Render
> import Euterpea.IO.MIDI.ExportMidiFile
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.Play
> import Euterpea.IO.MIDI.ToMidi2 (writeMidi2)
> import Euterpea.Music
> import HSoM.Performance ( metro )
> import System.Random ( Random(randomR), StdGen )

Utilities =========================================================================

> diagnosticsEnabled = False
> traceIf :: String → a → a
> traceIf str expr = if diagnosticsEnabled
>                    then trace str expr
>                    else expr
> traceAlways :: String → a → a
> traceAlways = trace
> traceNever :: String → a → a
> traceNever str expr = expr
>   
> addDur       :: Dur → [Dur → Music a] → Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)
>
> dim :: Rational → Music a → Music a
> dim amt = phrase [Dyn (Diminuendo amt)]
>
> capture :: Music (Pitch, [NoteAttribute]) → IO()
> capture = writeMidi2 "Parthenopea.mid"
>
> fractionOf :: Int → Double → Int
> fractionOf x d = min 127 $ round $ d * fromIntegral x
>
> {-
> takeItToTheWave :: FilePath → Music (Pitch, Volume) → IO()
> takeItToTheWave fp m = outFile fp secs sig
>   where
>     (secs, sig) = renderSF m []
> -}
>
> slur :: Rational → Music a → Music a
> slur rate = Modify (Phrase [Art (Slurred rate)])
>
> durS :: Rational → Double
> durS r = 2 * fromRational r
> 
> rationalEpsilon :: Double
> rationalEpsilon = 0.0001
> approx :: Double → Dur
> approx dur = approxRational dur rationalEpsilon
>
> rawPitches = [0..127]
>
> allPitches =
>    foldr ((:+:) . notize) (rest 0) rawPitches
>    where
>       notize aP = note qn $ pitch aP
>
> -- note chordFromArray has the same function body as chord itself
> chordFromArray :: Array Int (Music (Pitch, [NoteAttribute]))
>                   → Music (Pitch, [NoteAttribute])
> chordFromArray = foldr (:=:) (rest 0)

This alternate playback function will enable channel manipulation to allow
more than 16 instruments to be used in the source music.

> playDM :: (NFData a, ToMusic1 a) ⇒ Music a → IO ()
> playDM = playC defParams{chanPolicy = dynamicCP 16 9, strict=False}

This alternate playback function will merge overlapping notes, 
which makes for a cleaner sound on some synthesizers:

> playX :: (NFData a, ToMusic1 a) ⇒ Music a → IO ()
> playX = playC defParams{perfAlg = eventMerge . perform} where 
>     eventMerge :: Performance → Performance
>     eventMerge (e1:e2:es) = 
>         let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
>         in  if ePitch e1 == ePitch e2 then eventMerge (e1':es) 
>             else e1 : eventMerge (e2:es)
>     eventMerge e = e

  "triads"

> triad :: PitchClass → Mode → Pitch → Dur → Music Pitch
> triad key mode base dur = chord [ n1, n2, n3] where
>   rkP = absPitch (key, snd base - 1)
>   bP  = absPitch base
>   ocd = (bP - rkP) `div` 12
>   kP  = rkP + (12*ocd)
>   apD = bP - kP           -- guaranteed to be nonnegative
>   is  = formExact apD mode
>   n1, n2, n3 :: Music Pitch
>   n1 = note dur $ pitch bP
>   n2 = note dur $ pitch (bP + head is)
>   n3 = note dur $ pitch (bP + is!!1)
>   formExact :: AbsPitch → Mode → [AbsPitch]
>   formExact apDelta mode = offsets2intervals apDelta $ mode2offsets mode
>     where
>       major = [0, 4, 7, 12, 16]
>       minor = [0, 3, 7, 12, 15]
>       dim =   [0, 3, 6, 12, 15]
>       sus4 =  [0, 5, 7, 12, 17]
>       sus2 =  [0, 2, 7, 12, 14]
>       aug =   [0, 4, 8, 12, 16]
>       mode2offsets :: Mode → [AbsPitch]
>       mode2offsets mode
>         | Major             == mode = major
>         | Minor             == mode = minor
>         | Ionian            == mode = major
>         | Dorian            == mode = minor
>         | Phrygian          == mode = minor
>         | Lydian            == mode = major
>         | Mixolydian        == mode = major
>         | Aeolian           == mode = minor
>         | Locrian           == mode = dim
>         | CustomMode "Sus4" == mode = sus4
>         | CustomMode "Sus2" == mode = sus2
>         | CustomMode "Dim"  == mode = dim
>         | CustomMode "Aug"  == mode = aug
>         | otherwise          = error "Requested Mode not supported"
>       offsets2intervals :: AbsPitch → [AbsPitch] → [AbsPitch]
>       offsets2intervals apDelta os
>         | apDelta == (os!!0) = ((os!!1) - (os!!0)) : [(os!!2) - (os!!0)]
>         | apDelta == (os!!1) = ((os!!2) - (os!!1)) : [(os!!3) - (os!!1)]
>         | apDelta == (os!!2) = ((os!!3) - (os!!2)) : [(os!!4) - (os!!2)]
>         | otherwise          = error "Malformed Triad"
>
> data Lake = Lake { 
>                   lInst    :: InstrumentName, 
>                   lWch     :: Music Pitch, 
>                   lPch     :: AbsPitch,
>                   lVol     :: Volume,
>                   lKey     :: (PitchClass, Mode),
>                   lVel     :: Double}
>      deriving Show
>
>   -- calibrate (0,1) to (lo,up) e.g. (24,92)
> denorm :: Double → (Double, Double) → Double
> denorm r (lo, up) = lo + r * (up-lo)
>
> mkLake :: [Music Pitch] → [Double] → Lake
> mkLake mels rs =
>   let cnt :: Double
>       cnt = fromIntegral (length mels) - 0.000001
>   in Lake {  
>       lInst  = toEnum $ round   $ denorm (rs!!0) (0,instrumentLimit)
>       , lWch =  mels !! floor    (denorm (rs!!1) (0,cnt))
>       , lPch =  round           $ denorm (rs!!2) (24,92)
>       , lVol =  round           $ denorm (rs!!3) (80,120)
>       , lKey =  (toEnum $ round $ denorm (rs!!4) (0,12), Major)
>       , lVel =                    denorm (rs!!5) (1,3)}
>
> sextuplets :: StdGen → [[Double]]
> sextuplets g =
>    let (z1, g') =      randomR (0,1) g
>        (z2, g'') =     randomR (0,1) g'
>        (z3, g''') =    randomR (0,1) g''
>        (z4, g'''') =   randomR (0,1) g'''
>        (z5, g''''') =  randomR (0,1) g''''
>        (z6, g'''''') = randomR (0,1) g'''''
>        sextuplet = z1:z2:z3:z4:z5:[z6]
>    in sextuplet : sextuplets g''''''
>
> instrumentLimit :: Double
> instrumentLimit = fromIntegral $ fromEnum Gunshot

re instrumentRange content, see:
   https://www.orchestralibrary.com/reftables/rang.html
also
   https://philharmonia.co.uk/resources/instruments/
   https://omeka-s.grinnell.edu/s/MusicalInstruments/item/631

> instrumentRange :: InstrumentName → Maybe (Pitch, Pitch)
> instrumentRange inst =
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
>       Percussion                → Just wideOpen
>       -- Chimes
>       AcousticGuitarNylon       → Just (( E, 2), ( E, 5))
>       AcousticGuitarSteel       → Just (( E, 2), ( E, 5))
>       ElectricGuitarJazz        → Just (( E, 2), ( E, 5))
>       ElectricGuitarClean       → Just (( E, 2), ( E, 5))
>       ElectricGuitarMuted       → Just (( E, 2), ( E, 5))
>       OverdrivenGuitar          → Just (( E, 2), ( E, 5))
>       DistortionGuitar          → Just (( E, 2), ( E, 5))
>       OrchestralHarp            → Just (( C, 1), (Fs, 7))
>       AcousticBass              → Just (( E, 1), ( C, 8))
>       ElectricBassFingered      → Just (( E, 1), ( C, 8))
>       ElectricBassPicked        → Just (( E, 1), ( C, 8))
>       Sitar                     → Just (( C, 2), ( E, 5))
>       -- FretlessBass
>       SlapBass1                 → Just (( E, 1), ( C, 8))
>       SlapBass2                 → Just (( E, 1), ( C, 8))
>       -- SynthBass1
>       -- SynthBass2
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
>
>       Violin                    → Just (( G, 3), ( A, 7))
>       Viola                     → Just (( C, 3), ( E, 6))
>       Cello                     → Just (( C, 2), ( C, 6))
>       Contrabass                → Just (( E, 1), ( G, 4)) -- alias Doublebass
>       StringEnsemble1           → Just
>                                    $ unionRanges
>                                    $ map (fromJust . instrumentRange)
>                                          (Violin:Viola:[Cello])
>
>       Banjo                     → Just (( C, 3), ( C, 6))
>       _                         → Nothing
>
> rangedInstruments :: Array Int (InstrumentName, (Pitch, Pitch))
> rangedInstruments = selectInstruments
>                     (isJust . instrumentRange)
>                     (take 128 (enumFrom AcousticGrandPiano))
>
> selectInstruments :: (InstrumentName → Bool)
>                      → [InstrumentName]
>                      → Array Int (InstrumentName, (Pitch, Pitch))
> selectInstruments pred is =
>   let
>     justs = [(i, fromJust $ instrumentRange i) | i ← is, pred i]
>     inits = zip [1..] justs
>   in 
>     array (1, length inits) inits
>
> denormInstrument :: Double
>                     → Array Int (InstrumentName, (Pitch, Pitch))
>                     → (InstrumentName, (Pitch, Pitch))
> denormInstrument norm insts
>   | norm < 0 || norm > 1 = error "Must pass normalized range to selectInstrument"
>   | otherwise = foundIt
>       where
>         ifirst  = 1
>         ilast   = length insts
>         iselect = floor $ denorm norm (fromIntegral ifirst, fromIntegral ilast)
>         foundIt = insts ! iselect
>
> wideOpen :: (Pitch, Pitch)
> wideOpen = (pitch 0, pitch 127)
>
> union2Ranges :: (Pitch, Pitch) → (Pitch, Pitch) → (Pitch, Pitch)
> union2Ranges r1 r2 = unionRanges (r1:[r2])
>
> unionRanges :: [(Pitch, Pitch)] → (Pitch, Pitch)
> unionRanges (r:rs) = ( minimum (map fst (r:rs))
>                      , maximum (map snd (r:rs)) )
> unionRanges _ = error "empty range list"
>
> intersect2Ranges :: (Pitch, Pitch) → (Pitch, Pitch) → Maybe (Pitch, Pitch)
> intersect2Ranges r1 r2 = intersectRanges (r1:[r2])
>
> intersectRanges :: [(Pitch, Pitch)] → Maybe (Pitch, Pitch)
> intersectRanges (r:rs) =
>   case uncurry compare inverted of
>     LT → Just inverted
>     _ → Nothing
>   where
>     inverted = ( maximum (map fst (r:rs))
>                , minimum (map snd (r:rs)) )
> intersectRanges _ = error "empty range list"
>
> withinRange :: (Pitch, Pitch) → AbsPitch → Pitch → Bool
> withinRange range t p =
>    let aP = absPitch p + t
>        minP = absPitch $ fst range
>        maxP = absPitch $ snd range
>    in minP <= aP && maxP >= aP
>
> type OorCase = (InstrumentName, AbsPitch, Pitch)
>
> listOutOfRangeCases :: Music (Pitch, [NoteAttribute]) → [OorCase]
> listOutOfRangeCases m =
>    let listOor :: InstrumentName
>                   → AbsPitch
>                   → Music (Pitch, [NoteAttribute])
>                   → [OorCase]
>        listOor inst t (Prim (Note _ (p, a))) =
>           case instrumentRange inst of
>              Nothing    → [(inst, t, p)]
>              Just range → ([(inst, t, p) | not (withinRange range t p)])
>        listOor inst t (m1 :+: m2) =
>           listOor inst t m1 ++ listOor inst t m2
>        listOor inst t (m1 :=: m2) =
>           listOor inst t m1 ++ listOor inst t m2
>        listOor inst t (Modify (Instrument iname) m)  = listOor iname t m
>        listOor inst t (Modify (Transpose newt) m)    = listOor inst (t+newt) m
>        listOor inst t (Modify c m)                   = listOor inst t m
>        listOor _ _ _                                 = []
>
>    in listOor AcousticGrandPiano 0 m
>
> scoreOnsets :: Int → [Double] → Array Int Int
> scoreOnsets nBins ts
>   | traceAlways msg False = undefined
>   | otherwise = hist (0, nBins - 1) is
>   where
>      safemax arr
>        | null arr = 0
>        | otherwise = maximum ts
>      safemin arr
>        | null arr = 0
>        | otherwise = minimum ts
>   
>      hi, lo, fact :: Double
>      hi = safemax ts + 0.0000001
>      lo = safemin ts - 0.0000001
>      fact = fBins / (hi - lo)
>      cv :: Double → Int
>      cv d = floor $ (d - lo) * fact
>      fBins :: Double
>      fBins = fromIntegral nBins
>
>      is :: [Int]
>      is = map cv ts
>
>      msg = unwords [ "scoreOnsets lo, hi, fact="
>                    , show (safemin ts)
>                    , ":", show (safemax ts)
>                    , ":", show fact
>                    , " ts="
>                    , show (length ts)
>                    , " is="
>                    , show (length is)]
>
> hist :: (Ix a, Integral b) ⇒ (a,a) → [a] → Array a b
> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i ← is, inRange bnds i]
>
> defBins :: Int
> defBins = 32
>
> scoreMusic :: Int → Array Int (Music (Pitch, [NoteAttribute])) → Array Int Int
> scoreMusic nBins m
>   | traceIf msg False = undefined
>   | otherwise = score
>   where
>     score :: Array Int Int
>     score = 
>       scoreOnsets nBins
>       $ extractTimes
>       $ collectMEvents
>       $ chordFromArray m
>     msg = unwords [ "bins=", show $ elems score]
>
> collectMEvents :: ToMusic1 a ⇒ Music a → Performance
> collectMEvents m = fst (musicToMEvents defaultContext (toMusic1 m))
>
> defaultContext :: MContext
> defaultContext = MContext
>           {mcTime = 0
>            , mcInst = AcousticGrandPiano
>            , mcDur = metro 120 qn
>            , mcVol = 100}
>
> extractTimes :: Performance → [Double]
> extractTimes = map (fromRational . eTime)
>
> percussionLimit :: Double
> percussionLimit = fromIntegral $ fromEnum OpenTriangle

These snippets seem good to be used with "lake"

> pSnippet01, pSnippet02, defSnippet :: Music Pitch
> pSnippet01 =
>   tempo (3/2) (line [ e 4 qn, e 4 en, e 4 qn, e 4 en])
>
> pSnippet02 = line [c 4 en, e 4 qn, bf 3 en, d 4 qn, f 4 en]
>
> defSnippet = pSnippet01
>
> stdMels :: Music Pitch -> [Music Pitch]
> stdMels melInput = [mel0, mel1, mel2, mel3]
>   where 
>     mel0 = melInput
>     mel1 = retro melInput
>     mel2 = invert melInput
>     mel3 = (invert.retro) melInput
>
> -- music converter
> aggrandize :: Music (Pitch, Volume) → Music (Pitch, [NoteAttribute])
> aggrandize (Prim (Note d (p, v))) =
>        Prim (Note d (p, [Volume v]))
> aggrandize (Prim (Rest d)) =
>        Prim (Rest d)
> aggrandize (m1 :+: m2)            =
>        aggrandize m1 :+: aggrandize m2
> aggrandize (m1 :=: m2)            =
>        aggrandize m1 :=: aggrandize m2
> aggrandize (Modify c m)  = Modify c (aggrandize m)
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

Parthenopea
William Clements
December 12, 2022

> module Parthenopea where
>
> import Control.DeepSeq (NFData)
> import Data.Array.Unboxed
> import Data.Int ( Int16, Int32 )
> import Data.List
> import qualified Data.Map as Map
> import Data.Maybe ( fromJust, isJust, isNothing, mapMaybe, fromMaybe )
> import Data.Ratio ( approxRational )
> import Debug.Trace ( trace )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg )
> import Euterpea.IO.Audio.IO
> import Euterpea.IO.Audio.Render
> import Euterpea.IO.Audio.Types
> import Euterpea.IO.MIDI.ExportMidiFile
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.MidiIO (unsafeOutputID, unsafeInputID, OutputDeviceID, InputDeviceID)
> import Euterpea.IO.MIDI.Play
> import Euterpea.IO.MIDI.ToMidi2 (writeMidi2)
> import Euterpea.Music
> import HSoM.Performance ( metro, Context (cDur) )
> import System.Random ( Random(randomR), StdGen )
> import qualified Text.FuzzyFind as FF
  
Utilities =============================================================================================================

> diagnosticsEnabled = False
> traceIf      :: String → a → a
> traceIf str expr = if diagnosticsEnabled
>                    then trace str expr
>                    else expr
> traceAlways  :: String → a → a
> traceAlways = trace
> traceNever   :: String → a → a
> traceNever str expr = expr
>
> addDur       :: Dur → [Dur → Music a] → Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)
>
> grace                  :: Int → Music Pitch → Music Pitch
> grace n (Prim (Note d p)) = note (est * d) (trans n p) :+: note ((1 - est) * d) p
>   where
>     est                :: Rational
>     est = case compare d (1/8) of
>                 LT → 1/2
>                 _  → 1/8
> grace n  _                  = 
>           error "Can only add a grace note to a note."
>
> t32 :: [Music a] → Music a
> t32 notes = tempo (3/2) (foldr (:+:) (rest 0) notes)
>
> dim          :: Rational → Music a → Music a
> dim amt = phrase [Dyn (Diminuendo amt)]
>
> capture      :: Music (Pitch, [NoteAttribute]) → IO()
> capture = writeMidi2 "Parthenopea.mid"
>
> fractionOf   :: Int → Double → Int
> fractionOf x d = min 127 $ round $ d * fromIntegral x
>
> slur         :: Rational → Music a → Music a
> slur rate = Modify (Phrase [Art (Slurred rate)])
>
> durS         :: Rational → Double
> durS r = 2 * fromRational r
> 
> ratEps       :: Double
> ratEps = 0.0001
>
> approx       :: Double → Dur
> approx dur = approxRational dur ratEps
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

> playDM       :: (NFData a, ToMusic1 a) ⇒ Maybe Int → Music a → IO ()
> playDM mi = playC defParams
>                 { strict=False
>                 , chanPolicy = dynamicCP 16 9
>                 , devID=case mi of
>                         Nothing → Nothing
>                         Just i → Just $ unsafeOutputID i
>                 , perfAlg= map (\e → e{eDur = max 0 (eDur e - 0.000001)}) . perform}

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

  "triad"

> triad        :: PitchClass → Mode → Pitch → Dur → Music Pitch
> triad key mode base dur = chord [n1, n2, n3] where
>   rkP = absPitch (key, snd base - 1)
>   bP  = absPitch base
>   ocd = (bP - rkP) `div` 12
>   kP  = rkP + (12*ocd)
>   apD = bP - kP           -- guaranteed to be nonnegative
>   is  = formExact apD mode
>   n1, n2, n3 :: Music Pitch
>   n1 = note dur $ pitch bP
>   n2 = note dur $ pitch (bP + head        is)
>   n3 = note dur $ pitch (bP + (head.tail) is)
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

  "ceilingPowerOfTwo"

> ceilingPowerOfTwo :: Int → Int
> ceilingPowerOfTwo num
>   | num < 1  = error "ceilingPowerOfTwo requires input >= 1"
>   | otherwise = ceiling $ logBase 2.0 (fromIntegral num)
>   

  "ascent/descent"

> data FigureType = Ascent | Descent | Cluster
>
> glissando              :: [AbsPitch] → Dur → Music Pitch
> glissando gliss dur =
>   dim (3/2)
>   $ slur 2
>   $ line
>   $ notes ++ [rest (dur - fromIntegral reach * step)]
>   where
>     notes              :: [Music Pitch]
>     notes = [note step (pitch x) | x ← gliss]
>     step = sfn
>     reach = length gliss
>
> descent2               :: AbsPitch → AbsPitch → Dur → Music Pitch
> descent2 xLo xHi dur
>   | traceAlways msg False = undefined
>   | xHi < xLo = error "not low enough range for descent"
>   | otherwise = glissando [xHi..xLo] dur
>   where
>     msg = unwords ["descent high lo" ++ show xHi ++ " " ++ show xLo]
>
> ascent2                :: AbsPitch → AbsPitch → Dur → Music Pitch
> ascent2 xLo xHi dur
>   | traceAlways msg False = undefined
>   | xHi < xLo = error "not high enough range for ascent"
>   | otherwise = glissando [xLo..xHi] dur
>   where
>     msg = unwords ["ascent lo high" ++ show xLo ++ " " ++ show xHi]
>
> descent                :: InstrumentName → Pitch → Dur → Music Pitch
> descent iname p = descent2 (absPitch (fst (fromJust (instrumentRange iname)))) (absPitch p) 
>
> ascent                 :: InstrumentName → Pitch → Dur → Music Pitch
> ascent iname p =  ascent2 (absPitch p) (absPitch (snd (fromJust (instrumentRange iname))))
>
>   -- calibrate (0,1) to (lo,up) e.g. (24,92)
> denorm       :: Double → (Double, Double) → Double
> denorm r (lo, up) = lo + r * (up-lo)
>
> sextuplets   :: StdGen → [[Double]]
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
>       AcousticGuitarNylon       → Just (( E, 2), ( B, 5))
>       AcousticGuitarSteel       → Just (( E, 2), ( B, 5))
>       ElectricGuitarJazz        → Just (( E, 2), ( B, 5))
>       ElectricGuitarClean       → Just (( E, 2), ( B, 5))
>       ElectricGuitarMuted       → Just (( E, 2), ( B, 5))
>       OverdrivenGuitar          → Just (( E, 2), ( B, 5))
>       DistortionGuitar          → Just (( E, 2), ( B, 5))
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
>       Banjo                     → Just (( C, 3), ( E, 5))
>
>       ChoirAahs                 → Just (( E, 3), ( C, 6))
>
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

instrument range checking =============================================================================================

> union2Ranges :: (Pitch, Pitch) → (Pitch, Pitch) → (Pitch, Pitch)
> union2Ranges r1 r2 = unionRanges (r1:[r2])
>
> unionRanges  :: [(Pitch, Pitch)] → (Pitch, Pitch)
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
>     _  → Nothing
>   where
>     inverted = ( maximum (map fst (r:rs))
>                , minimum (map snd (r:rs)) )
> intersectRanges _ = error "empty range list"
>
> withinRange  :: (Pitch, Pitch) → AbsPitch → Pitch → Bool
> withinRange range t p =
>    let aP = absPitch p + t
>        minP = absPitch $ fst range
>        maxP = absPitch $ snd range
>    in minP <= aP && maxP >= aP
>

examine song for instrument and percussion usage ======================================================================

> data OorCase =
>   OorCase {  oInstrument  :: InstrumentName
>            , oTranspose   :: AbsPitch
>            , oPitch       :: Pitch}    deriving (Show, Eq, Ord)
>
> type InstDB = Map.Map InstrumentName Int32
> type OorDB = Map.Map OorCase Int32
>
> initCase               :: OorCase
> initCase = 
>   OorCase {  oInstrument = AcousticGrandPiano
>            , oTranspose = 0
>            , oPitch = (C, 4)}
>
> listInstruments        :: Music (Pitch, [NoteAttribute]) → IO ()
> listInstruments m = do
>   let (db1, db2) = listI m initCase (Map.empty, Map.empty)
>   putStrLn ("\n\n instruments used in that music: " ++ show (Map.keys db1))
>   if null db2
>     then putStrLn "\n\n no out of range cases"
>     else do
>            putStrLn "\n\n out of range cases:"
>            print (Map.keys db2)
>   
> listI                  :: Music (Pitch, [NoteAttribute])
>                           → OorCase
>                           → (InstDB, OorDB)
>                           → (InstDB, OorDB)
> listI (Prim (Note _ (p, a))) ooc (db1, db2)
>       = case instrumentRange (oInstrument ooc) of
>              Nothing    → reportBad
>              Just range → if withinRange range (oTranspose ooc) p
>                           then reportGood
>                           else reportBad
>       where
>         reportBad  = (Map.insert (oInstrument ooc) 0 db1, Map.insert ooc{oPitch = p} 0 db2)
>         reportGood = (Map.insert (oInstrument ooc) 0 db1, db2)
>
> listI (Prim (Rest _)) ooc (db1, db2) = (db1, db2)
>
> listI (m1 :+: m2) ooc (db1, db2) = (Map.union m1db1 m2db1, Map.union m1db2 m2db2)
>   where
>     (m1db1, m1db2) = listI m1 ooc (db1, db2)
>     (m2db1, m2db2) = listI m2 ooc (db1, db2)
>     
> listI (m1 :=: m2) ooc (db1, db2) = (Map.union m1db1 m2db1, Map.union m1db2 m2db2)
>   where
>     (m1db1, m1db2) = listI m1 ooc (db1, db2)
>     (m2db1, m2db2) = listI m2 ooc (db1, db2)
>
> listI (Modify (Instrument iname) m) ooc (db1, db2)
>   = listI m ooc{oInstrument=iname} (db1, db2)
>
> listI (Modify (Transpose newT) m) ooc (db1, db2)
>   = listI m ooc{oTranspose=oTranspose ooc + newT} (db1, db2)
>
> listI (Modify _ m) ooc (db1, db2)
>   = listI m ooc (db1, db2)
>
> listPercussion        :: Music (Pitch, [NoteAttribute]) → IO ()
> listPercussion m = do
>   let db1 = listP m initCase Map.empty
>   putStrLn ("\n\n percussion used in that music: " ++ show (Map.keys db1))
>   
> listP                  :: Music (Pitch, [NoteAttribute])
>                           → OorCase
>                           → Map.Map PercussionSound Int32
>                           → Map.Map PercussionSound Int32
> listP (Prim (Note _ (p, a))) ooc db1
>       = case oInstrument ooc of
>              Percussion    → Map.insert (toEnum (absPitch p - 35)) 0 db1
>              _ → db1
>
> listP (Prim (Rest _)) ooc db1 = db1
>
> listP (m1 :+: m2) ooc db1 = db1'
>   where
>     m1db1 = listP m1 ooc db1
>     m2db1 = listP m2 ooc db1
>     db1' = Map.union m1db1 m2db1
>     
> listP (m1 :=: m2) ooc db1 = db1'
>   where
>     m1db1 = listP m1 ooc db1
>     m2db1 = listP m2 ooc db1
>     db1' = Map.union m1db1 m2db1
>
> listP (Modify (Instrument iname) m) ooc db1
>   = listP m ooc{oInstrument=iname} db1
>
> listP (Modify (Transpose newT) m) ooc db1
>   = listP m ooc{oTranspose=oTranspose ooc + newT} db1
>
> listP (Modify _ m) ooc db1
>   = listP m ooc db1
>

apply fuzzyfind to mining instruments + percussion ====================================================================

> class GMPlayable a where
>   getFFKeys            :: a → Maybe [String]
>   getList              :: [a]
>
> instance GMPlayable InstrumentName where
>   getFFKeys = instrumentFFKeys
>   getList = map toEnum [fromEnum AcousticGrandPiano .. fromEnum Gunshot]
>
> instance GMPlayable PercussionSound where
>   getFFKeys = percussionFFKeys
>   getList = map toEnum [fromEnum AcousticBassDrum .. fromEnum OpenTriangle]
>
> instrumentFFKeys :: InstrumentName → Maybe [String]
> instrumentFFKeys inst =
>    case inst of
>       AcousticGrandPiano        → Just            ["piano", "grand", "acoustic"]
>       BrightAcousticPiano       → Just            ["piano", "bright", "acoustic"]
>       ElectricGrandPiano        → Just            ["piano", "electric", "grand"]
>       HonkyTonkPiano            → Just            ["piano", "honkytonk"]
>       RhodesPiano               → Just            ["piano", "rhodes"]
>       ChorusedPiano             → Just            ["piano", "chorused"]
>       Harpsichord               → Just $ singleton "harpsichord"
>       Clavinet                  → Just $ singleton "clavinet"
>       Celesta                   → Just $ singleton "celesta"
>       Glockenspiel              → Just $ singleton "glockenspiel"
>       MusicBox                  → Just $ singleton "musicbox"
>       Vibraphone                → Just $ singleton "vibraphone"
>       Marimba                   → Just $ singleton "marimba"
>       Xylophone                 → Just $ singleton "xylophone"
>       TubularBells              → Just            ["bells", "tubular"]
>       Dulcimer                  → Just $ singleton "dulcimer"
>       HammondOrgan              → Just            ["organ", "hammond"]
>       PercussiveOrgan           → Just            ["organ", "percussive"]
>       RockOrgan                 → Just            ["organ", "rock"] 
>       ChurchOrgan               → Just            ["organ", "church"]
>       ReedOrgan                 → Just            ["organ", "reed"]
>       Accordion                 → Just $ singleton "accordion"
>       Harmonica                 → Just $ singleton "harmonica"
>       TangoAccordion            → Just            ["accordion", "tango"]
>       AcousticGuitarNylon       → Just            ["guitar", "acoustic", "nylon"]
>       AcousticGuitarSteel       → Just            ["guitar", "acoustic", "steel"]
>       ElectricGuitarJazz        → Just            ["guitar", "electric", "jazz"]
>       ElectricGuitarClean       → Just            ["guitar", "electric", "clean"]
>       ElectricGuitarMuted       → Just            ["guitar", "electric", "muted"]
>       OverdrivenGuitar          → Just            ["guitar", "electric", "overdrive"]
>       DistortionGuitar          → Just            ["guitar", "fuzz", "distortion"]
>       GuitarHarmonics           → Just            ["guitar", "harmonics"]
>       AcousticBass              → Just            ["bass", "acoustic"]
>       ElectricBassFingered      → Just            ["bass", "electric", "finger"]
>       ElectricBassPicked        → Just            ["bass", "electric", "pick"]
>       FretlessBass              → Just            ["bass", "fretless"] 
>       SlapBass1                 → Just            ["bass", "slap", "1"]
>       SlapBass2                 → Just            ["bass", "slap", "2"]
>       SynthBass1                → Just            ["bass", "synth", "1"]
>       SynthBass2                → Just            ["bass", "synth", "2"]
>       Violin                    → Just $ singleton "violin"
>       Viola                     → Just $ singleton "viola"
>       Cello                     → Just $ singleton "cello"
>       Contrabass                → Just $ singleton "contrabass"
>       TremoloStrings            → Just            ["strings", "tremolo"]
>       PizzicatoStrings          → Just            ["strings", "pizzicato"]
>       OrchestralHarp            → Just            ["harp", "orchestra"]
>       Timpani                   → Just $ singleton "timpani"
>       StringEnsemble1           → Just            ["ensemble", "string", "1"]
>       StringEnsemble2           → Just            ["ensemble", "string", "2"]
>       SynthStrings1             → Just            ["strings", "synth", "1"]
>       SynthStrings2             → Just            ["strings", "synth", "2"]
>       ChoirAahs                 → Just            ["choir", "aahs"]
>       VoiceOohs                 → Just            ["voice", "oohs"]
>       SynthVoice                → Just            ["synth", "voice"]
>       OrchestraHit              → Just            ["orchestra", "hit"]
>       Trumpet                   → Just $ singleton "trumpet"
>       Trombone                  → Just $ singleton "trombone"
>       Tuba                      → Just $ singleton "tuba"
>       MutedTrumpet              → Just            ["trumpet", "muted"]
>       FrenchHorn                → Just            ["horn", "french"]
>       BrassSection              → Just            ["brass", "section"]
>       SynthBrass1               → Just            ["brass", "synth", "1"]
>       SynthBrass2               → Just            ["brass", "synth", "2"]
>       SopranoSax                → Just            ["sax" , "soprano"]
>       AltoSax                   → Just            ["sax" , "alto"]
>       TenorSax                  → Just            ["sax" , "tenor"]
>       BaritoneSax               → Just            ["sax" , "baritone"]
>       Oboe                      → Just $ singleton "oboe"
>       Bassoon                   → Just $ singleton "bassoon"
>       EnglishHorn               → Just            ["horn", "english"]
>       Clarinet                  → Just $ singleton "clarinet"
>       Piccolo                   → Just $ singleton "piccolo"
>       Flute                     → Just $ singleton "flute"
>       Recorder                  → Just $ singleton "recorder"
>       PanFlute                  → Just $ singleton "panflute"
>       BlownBottle               → Just            ["bottle", "blown"]
>       Shakuhachi                → Just $ singleton "shakuhachi"
>       Whistle                   → Just $ singleton "whistle"
>       Ocarina                   → Just $ singleton "ocarina"
>       Lead1Square               → Just $ singleton "lead1square"
>       Lead2Sawtooth             → Just $ singleton "lead2sawtooth"
>       Lead3Calliope             → Just $ singleton "lead3calliope"
>       Lead4Chiff                → Just $ singleton "lead4chiff"
>       Lead5Charang              → Just $ singleton "lead5charang"
>       Lead6Voice                → Just $ singleton "lead6voice"
>       Lead7Fifths               → Just $ singleton "lead7fifths"
>       Lead8BassLead             → Just $ singleton "lead8basslead"
>       Pad1NewAge                → Just $ singleton "pad1newage"
>       Pad2Warm                  → Just $ singleton "pad2warm"
>       Pad3Polysynth             → Just $ singleton "pad3polysynth"
>       Pad4Choir                 → Just $ singleton "pad4choir"
>       Pad5Bowed                 → Just $ singleton "pad5bowed"
>       Pad6Metallic              → Just $ singleton "pad6metallic"
>       Pad7Halo                  → Just $ singleton "pad7halo"
>       Pad8Sweep                 → Just $ singleton "pad8sweep"
>       FX1Train                  → Just $ singleton "fx1train"
>       FX2Soundtrack             → Just $ singleton "fx2soundtrack"
>       FX3Crystal                → Just $ singleton "fx3crystal"
>       FX4Atmosphere             → Just $ singleton "fx4atmosphere"
>       FX5Brightness             → Just $ singleton "fx5brightness"
>       FX6Goblins                → Just $ singleton "fx6goblins"
>       FX7Echoes                 → Just $ singleton "fx7echoes"
>       FX8SciFi                  → Just $ singleton "fx8scifi"
>       Sitar                     → Just $ singleton "sitar"
>       Banjo                     → Just $ singleton "banjo"
>       Shamisen                  → Just $ singleton "shamisen"
>       Koto                      → Just $ singleton "koto"
>       Kalimba                   → Just $ singleton "kalimba"
>       Bagpipe                   → Just $ singleton "bagpipe"
>       Fiddle                    → Just $ singleton "fiddle"
>       Shanai                    → Just $ singleton "shanai"
>       TinkleBell                → Just            ["bell", "tinkle"]
>       Agogo                     → Just $ singleton "agogo"
>       SteelDrums                → Just            ["drums", "steel"]
>       Woodblock                 → Just $ singleton "woodblock"
>       TaikoDrum                 → Just            ["drum", "taiko"]
>       MelodicDrum               → Just            ["drum", "melodic"]
>       SynthDrum                 → Just            ["drum", "synth"]
>       ReverseCymbal             → Just            ["cymbal", "reverse"]
>       GuitarFretNoise           → Just            ["guitar", "fret", "noise"]
>       BreathNoise               → Just            ["breath", "noise"]
>       Seashore                  → Just $ singleton "seashore"
>       BirdTweet                 → Just            ["bird", "tweet"]
>       TelephoneRing             → Just            ["ring", "telephone"]
>       Helicopter                → Just $ singleton "helicopter"
>       Applause                  → Just $ singleton "applause"
>       Gunshot                   → Just $ singleton "gunshot"
>       _                         → Nothing
>
> percussionFFKeys :: PercussionSound → Maybe [String]
> percussionFFKeys perc =
>    case perc of
>       AcousticBassDrum          → Just            ["drum", "bass", "acoustic"]
>       BassDrum1                 → Just            ["drum", "bass", "kick"]
>       SideStick                 → Just            ["side", "stick"]
>       AcousticSnare             → Just            ["snare", "drum", "acoustic"]
>       HandClap                  → Just            ["clap", "hand"]
>       ElectricSnare             → Just            ["electric", "snare", "drum"]
>       LowFloorTom               → Just            ["tom", "low", "floor"]
>       ClosedHiHat               → Just            ["hihat", "closed"]
>       HighFloorTom              → Just            ["tom", "high", "floor"]
>       PedalHiHat                → Just            ["hihat", "pedal"]
>       LowTom                    → Just            ["tom", "low"]
>       OpenHiHat                 → Just            ["hihat", "open"]
>       LowMidTom                 → Just            ["tom", "mid", "low"]
>       HiMidTom                  → Just            ["tom", "mid", "high"]
>       CrashCymbal1              → Just            ["crash", "cymbal", "1"]
>       HighTom                   → Just            ["tom", "high"]
>       RideCymbal1               → Just            ["cymbal", "ride", "1"]
>       ChineseCymbal             → Just            ["cymbal", "chinese"]
>       RideBell                  → Just            ["bell", "ride"]
>       Tambourine                → Just            ["tambourine"]
>       SplashCymbal              → Just            ["cymbal", "splash"]
>       Cowbell                   → Just            ["cowbell"]
>       CrashCymbal2              → Just            ["crash", "cymbal", "2"]
>       Vibraslap                 → Just            ["vibraslap"]
>       RideCymbal2               → Just            ["cymbal", "ride", "2"]
>       HiBongo                   → Just            ["bongo", "high"]
>       LowBongo                  → Just            ["bongo", "low"]
>       MuteHiConga               → Just            ["conga", "mute", "high"]
>       OpenHiConga               → Just            ["conga", "open", "high"]
>       LowConga                  → Just            ["conga", "low"]
>       HighTimbale               → Just            ["timbale", "high"]
>       LowTimbale                → Just            ["timbale", "low"]
>       HighAgogo                 → Just            ["agogo", "high"]
>       LowAgogo                  → Just            ["agogo", "low"]
>       Cabasa                    → Just            ["cabasa"]
>       Maracas                   → Just            ["maracas"]
>       ShortWhistle              → Just            ["whistle", "short"]
>       LongWhistle               → Just            ["whistle", "long"]
>       ShortGuiro                → Just            ["guiro", "short"]
>       LongGuiro                 → Just            ["guiro", "long"]
>       Claves                    → Just            ["claves"]
>       HiWoodBlock               → Just            ["woodblock", "high"]
>       LowWoodBlock              → Just            ["woodblock", "low"]
>       MuteCuica                 → Just            ["cuica", "mute"]
>       OpenCuica                 → Just            ["cuica", "open"]
>       MuteTriangle              → Just            ["triangle", "mute"]
>       OpenTriangle              → Just            ["triangle", "open"]
>
> quickFFTest            :: String → [String] → [Maybe FF.Alignment]
> quickFFTest inp = map (`FF.bestMatch` inp)
>
> evalPick               :: forall a. (Ord a, Show a) ⇒ Maybe (a, Double) → Double
> evalPick Nothing = 0
> evalPick (Just (playable, x)) = x
>
> findMatchingA          :: forall a. (Ord a, Show a, GMPlayable a) ⇒
>                           String
>                           → Maybe (a, Double)
> findMatchingA inp
>   | traceIf msg False                    = undefined
>   | otherwise                            = chooseWinner asScored
>   where
>     asList              :: [a]           = getList
>     
>     asLooks = mapMaybe (eval1 getFFKeys) asList
>
>     eval1              :: forall a. (Ord a, Show a) ⇒ (a → Maybe [String]) → a → Maybe (a, [String])
>     eval1 lookupKeys item                = mr
>       where
>         mffk           :: Maybe [String] = lookupKeys item
>         mr             :: Maybe (a, [String])
>         mr
>          | isNothing mffk                = Nothing
>          | otherwise                     = Just (item, fromJust mffk) 
>
>     asScored = mapMaybe eval2 asLooks
>
>     eval2              :: forall a. (Ord a, Show a) ⇒ (a, [String]) → Maybe (a, Double)
>     eval2 (iname, keys)                  = ms
>       where
>         lFactor        :: Double         = sqrt $ fromIntegral $ length keys
>         weights        :: [Double]       = [1.5 / lFactor
>                                           , 1.4 / lFactor
>                                           , 1.3 / lFactor
>                                           , 1.2 / lFactor
>                                           , 1.1 / lFactor]
>         withWeights    :: [(String, Double)]
>                                          = zip keys weights
>         pieces         :: [Double]       = map eval3 withWeights
>         tot            :: Double         = sum pieces
>         ms             :: Maybe (a, Double)
>           | tot <= 0                     = Nothing
>           | otherwise                    = Just (iname, tot)
>
>     eval3              :: (String, Double) → Double
>     eval3 (key, weight)
>       | traceNever msg False = undefined
>       | otherwise                            = piece
>       where
>         malign = FF.bestMatch key inp
>         piece  = maybe 0 ((* weight) . fromIntegral . FF.score) malign
>         msg = unwords ["eval3", show key, show weight, " and piece=", show piece]
>
>     chooseWinner       :: [(a, Double)] → Maybe (a, Double)
>     chooseWinner scores
>       | null scores                     = Nothing
>       | otherwise                       = Just winner 
>       where
>         winner         :: (a, Double)   = maximumBy eval4 scores
>
>     eval4              :: (a, Double) → (a, Double) → Ordering
>     eval4 (_, s1) (_, s2)                = compare s1 s2
>
>     msg = unwords ["findMatchingA", show inp] -- , "\n", show result]
>
> findMatchingAExcludingB:: forall a b. (Ord a, Show a, GMPlayable a, Ord b, Show b, GMPlayable b) ⇒
>                           Maybe (a, Double) → Maybe (b, Double) → Double → Maybe (a, Double)
> findMatchingAExcludingB ma mb thresh =
>   let
>     ascore = evalPick ma
>     bscore = evalPick mb
>   in
>     if ascore > thresh && ascore >= bscore
>     then ma
>     else Nothing
>     
> findMatchingInstrument :: String → Double → Maybe (InstrumentName, Double)
> findMatchingInstrument inp thresh =
>   let
>     ma                 :: Maybe (InstrumentName, Double)
>                                          = findMatchingA inp
>     mb                 :: Maybe (PercussionSound, Double)
>                                          = findMatchingA inp
>   in
>     findMatchingAExcludingB ma mb thresh
>
> findMatchingPercussion :: String → Double → Maybe (PercussionSound, Double)
> findMatchingPercussion inp thresh =
>   let
>     ma                 :: Maybe (PercussionSound, Double)
>                                          = findMatchingA inp
>     mb                 :: Maybe (InstrumentName, Double)
>                                          = findMatchingA inp
>   in
>     findMatchingAExcludingB ma mb thresh
>
> wink                  :: forall a. (GMPlayable a) ⇒ String → Maybe (a, Double) → Maybe (a, String)
> wink inp                                 = maybe Nothing (wink' inp)
>
> wink'                 :: forall a. (GMPlayable a) ⇒ String → (a, Double) → Maybe (a, String)
> wink' inp (iname, _)                     = Just (iname, inp)
>
>
> findMatchingInstrument' :: String → Double → Maybe (InstrumentName, String)
> findMatchingInstrument' inp thresh = wink inp ma
>   where
>     ma                 :: Maybe (InstrumentName, Double)
>                                          = findMatchingInstrument inp thresh
>
> findMatchingPercussion' :: String → Double → Maybe (PercussionSound, String)
> findMatchingPercussion' inp thresh = wink inp ma
>   where
>     ma                 :: Maybe (PercussionSound, Double)
>                                          = findMatchingPercussion inp thresh
>

tournament among instruments in various soundfont files ===============================================================

> scoreOnsets  :: Int → [Double] → Array Int Int
> scoreOnsets nBins ts
>   | traceAlways msg False = undefined
>   | otherwise = hist (0, nBins - 1) is
>   where
>      safemax, safemin :: [Double] → Double
>      safemax ts
>        | null ts = 0
>        | otherwise = maximum ts
>      safemin ts
>        | null ts = 0
>        | otherwise = minimum ts
>   
>      hi, lo, fact :: Double
>      hi = safemax ts + 0.0000001
>      lo = safemin ts - 0.0000001
>      fact = fromIntegral nBins / (hi - lo)
>
>      is :: [Int]
>      is = map (\d → floor $ (d - lo) * fact) ts
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
> hist         :: (Ix a, Integral b) ⇒ (a,a) → [a] → Array a b
> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i ← is, inRange bnds i]
>
> defBins      :: Int
> defBins = 32
>
> scoreMusic   :: Int → Array Int (Music (Pitch, [NoteAttribute])) → Array Int Int
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

snippets to be used with "lake" =======================================================================================

> pSnippet01, pSnippet02, defSnippet :: Music Pitch
> pSnippet01 = tempo (3/2) (line [ e 4 qn, e 4 en, e 4 qn, e 4 en])
>
> pSnippet02 = line [c 4 en, e 4 qn, bf 3 en, d 4 qn, f 4 en]
>
> defSnippet = pSnippet01
>
> stdMels      :: Music Pitch → [Music Pitch]
> stdMels melInput = [mel0, mel1, mel2, mel3]
>   where 
>     mel0 = melInput
>     mel1 = retro melInput
>     mel2 = invert melInput
>     mel3 = (invert.retro) melInput

music converter =======================================================================================================

> aggrandize             :: Music (Pitch, Volume) → Music (Pitch, [NoteAttribute])
> aggrandize (Prim (Note d (p, v))) =
>        Prim (Note d (p, [Volume v]))
> aggrandize (Prim (Rest d)) =
>        Prim (Rest d)
> aggrandize (m1 :+: m2)            =
>        aggrandize m1 :+: aggrandize m2
> aggrandize (m1 :=: m2)            =
>        aggrandize m1 :=: aggrandize m2
> aggrandize (Modify c m)  = Modify c (aggrandize m)

Wave ==================================================================================================================

> class AudioSample a ⇒ WaveAudioSample a where
>   retrieve :: UArray Int Int32 → Int → a
>
> instance WaveAudioSample Double where
>   retrieve :: UArray Int Int32 → Int → Double
>   retrieve sData idx = fromIntegral (sData ! idx)
>
> instance WaveAudioSample (Double,Double) where
>   retrieve :: UArray Int Int32 → Int → (Double, Double)
>   retrieve sData idx = (fromIntegral (sData ! idx), fromIntegral (sData ! (idx + 1)))
>
> data SlwRate
> instance Clock SlwRate where
>   rate _ = 4.41
> type SlwSF a b  = SigFun SlwRate a b

Histogram ==============================================================================================================

> numIN = (fromEnum Gunshot - fromEnum AcousticGrandPiano) + 1
> numPS = (fromEnum OpenTriangle - fromEnum AcousticBassDrum) + 1
>
> type Histogram = Array Int Int
>
> zeroHistogram           :: Int → Histogram
> zeroHistogram n = listArray (0, n-1) (replicate n 0)
>
> zeroHistogramI          :: Histogram
> zeroHistogramI = zeroHistogram numIN
>
> zeroHistogramP          :: Histogram
> zeroHistogramP = zeroHistogram numPS
>
> combineHistograms      :: Histogram → Histogram → Histogram
> combineHistograms h1 h2 = accumArray (+) 0 (e, f) (assocs h1 ++ assocs h2)
>   where
>     (a, b) = bounds h1
>     (c, d) = bounds h2
>     n = 1 + max b d
>     (e, f)
>       | a /= 0 || c /= 0 = error "Histograms must be zero-based"
>       | b /= d           = error "Histograms being combined must have same dimension"
>       | otherwise        = (0, n - 1)

Conversion functions and general helpers ==============================================================================

> wrap :: (Ord n, Num n) ⇒ n → n → n
> wrap val bound = if val > bound then wrap val (val-bound) else val

> clip :: Ord n ⇒ n → n → n → n
> clip val lower upper 
>     | val <= lower = lower
>     | val >= upper = upper
>     | otherwise    = val

Raises 'a' to the power 'b' using logarithms.

> pow :: Floating a ⇒ a → a → a
> pow a b = exp (log a * b)

Returns the fractional part of 'x'.

> frac :: RealFrac r ⇒ r → r
> frac = snd . properFraction

Takes care of characters that need quoting like '"'

> quoteCodeString        :: String → String
> quoteCodeString                          = concatMap quote
>   where
>     quote              :: Char → String
>     quote c = case c of
>                 '\"'   → "\\\""
>                 _      → [c]

> fromCentibels          :: Maybe Int → Double
> fromCentibels mcents                     = 10**(fromIntegral (fromMaybe 0 mcents)/100)


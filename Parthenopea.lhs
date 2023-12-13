> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

Parthenopea
William Clements
December 12, 2022

> module Parthenopea where
>
> import Codec.Midi(exportFile, importFile)
> import Control.Arrow.ArrowP
> import Control.DeepSeq (NFData)
> import Control.SF.SF
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Graph (Graph)
> import qualified Data.Graph              as Graph
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( fromJust, isJust, isNothing, mapMaybe, fromMaybe )
> import Data.Ord
> import Data.Ratio ( approxRational )
> import qualified Data.Set as Set
> import Data.Word
> import Debug.Trace ( trace )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg, Table, tableSinesN )
> import Euterpea.IO.Audio.IO
> import Euterpea.IO.Audio.Render
> import Euterpea.IO.Audio.Types
> import Euterpea.IO.MIDI.ExportMidiFile
> import Euterpea.IO.MIDI.FromMidi2
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.MidiIO (unsafeOutputID, unsafeInputID, OutputDeviceID, InputDeviceID)
> import Euterpea.IO.MIDI.Play
> import Euterpea.IO.MIDI.ToMidi2 (writeMidi2)
> import Euterpea.Music
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay) )
> import HSoM.Performance ( metro, Context (cDur) )
> import System.Random ( Random(randomR), StdGen )
> import qualified Text.FuzzyFind          as FF
  
Utilities =============================================================================================================

> traceIf      :: String → a → a
> traceIf str expr = if diagnosticsEnabled
>                    then trace str expr
>                    else expr
> traceNow  :: String → a → a
> traceNow = trace
> traceAlways  :: String → a → a
> traceAlways = trace
> traceNever   :: String → a → a
> traceNever str expr = expr
> traceNot   :: String → a → a
> traceNot str expr = expr
>
> impM                   :: FilePath → IO ()
> impM fp                                  = do
>   mu <- importMidi fp
>   play mu
>   -- listInstruments $ aggrandize mu
>   return ()
>
> importMidi             :: FilePath → IO (Music (Pitch, Volume))
> importMidi fp = do
>   x <- importFile fp
>   let y = case x of
>             Left z -> error z
>             Right m2 -> fromMidi2 m2
>   return y
>
> hasDuplicates :: (Ord a) ⇒ [a] → Bool
> hasDuplicates list = length list /= length set
>   where
>     set = Set.fromList list
>
> addDur       :: Dur → [Dur → Music a] → Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)
>
> grace                  :: Int → Music Pitch → Music Pitch
> grace n (Prim (Note d p)) = note (est * d) (trans n p) :+: note ((1 - est) * d) p
>   where
>     est                :: Rational
>     est
>       | d < 1/8                          = 1/2
>       | d > (2*wn)                       = 1/(8*d)
>       | otherwise                        = 1/8
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

"triad" ===============================================================================================================

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

"ascent/descent" ======================================================================================================

> glissando'              :: [AbsPitch] → Dur → Music Pitch
> glissando' gliss dur
>   | traceIf msg False                    = undefined
>   | otherwise                            = music
>   where
>     music = 
>       dim 1
>       $ slur 2
>       $ line
>         notes 
>     notes              :: [Music Pitch]  = [note (dur * 9 / (reach * 10)) (pitch x) | x ← gliss]
>     reach              :: Rational       = fromIntegral $ length gliss
>     msg                                  = unwords ["glissando' " ++ show music]
>
> glissando              :: Bool → (AbsPitch, AbsPitch) → Dur → Music Pitch
> glissando _ _ 0                        = rest 0
> glissando desc (xLo, xHi) dur
>   | traceIf msg False                    = undefined
>   | xHi < xLo + 6                        = error "glissando: not enough range"
>   | dur < 1 / 8                          = error "glissando: not enough duration"
>   | otherwise                            = if skipGlissandi
>                                              then
>                                                rest dur
>                                              else
>                                                glissando' (take 28 nList) dur
>   where
>     nList                                = if desc
>                                              then reverse [xLo..xHi]
>                                              else [xLo..xHi]
>     msg                                  = unwords ["glissando " ++ show nList]
>
> descent                :: AbsPitch → InstrumentName → Pitch → Dur → Music Pitch
> descent xpo iname p dur                  =
>   chord [  rest dur
>          , glissando True (absPitch bottom, absPitch p) dur]
>   where
>     bottom = trans (-xpo) $ fst (fromJust (instrumentRange iname))
>
> ascent                 :: AbsPitch → InstrumentName → Pitch → Dur → Music Pitch
> ascent xpo iname p dur                   =
>   chord [  rest dur
>          , glissando False (absPitch p, absPitch top) dur]
>   where
>     top = trans (-xpo) $ snd (fromJust (instrumentRange iname))

ranges ================================================================================================================

>   -- calibrate (0,1) to (lo,up) e.g. (24,92)
> denorm                 :: Double → (Double, Double) → Double
> denorm r (lo, up)                        = lo + r * (up-lo)
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
>       MusicBox                  → Just (( C, 4), ( E, 7)) -- made up out of thin air
>       Percussion                → Just wideOpen
>       -- Chimes
>       AcousticGuitarNylon       → Just (( E, 2), ( B, 5))
>       AcousticGuitarSteel       → Just (( E, 2), ( B, 5))
>       ElectricGuitarJazz        → Just (( E, 2), ( B, 5))
>       ElectricGuitarClean       → Just (( E, 2), ( B, 6))
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
>       Contrabass                → Just (( E, 1), ( G, 4)) -- alias Doublebass
>       StringEnsemble1           → Just
>                                    $ unionRanges
>                                    $ map (fromJust . instrumentRange)
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
>     Agogo                                  → True
>     Applause                               → True
>     BirdTweet                              → True
>     BreathNoise                            → True
>     Gunshot                                → True
>     Helicopter                             → True
>     ReverseCymbal                          → True
>     Seashore                               → True
>     SynthDrum                              → True
>     TaikoDrum                              → True
>     TelephoneRing                          → True
>     Timpani                                → True
>     Woodblock                              → True
>     _                                      → False
>
> nonPitchedInstruments  :: [InstrumentName]
> nonPitchedInstruments                    = filter nonPitchedInstrument getList
>
> selectRanged           :: [InstrumentName] → Array Int (InstrumentName, (Pitch, Pitch))
> selectRanged is                          = listArray (1, length qual) qual
>   where
>     qual                                 = mapMaybe (\i → instrumentRange i >>= Just . (i,)) is
>
> denormInstrument :: Double
>                     → Array Int (InstrumentName, (Pitch, Pitch))
>                     → (InstrumentName, (Pitch, Pitch))
> denormInstrument norm insts              = profess
>                                              (norm >= 0 && norm <= 1)
>                                              "denormInstrument"
>                                              (insts ! iselect)
>   where
>     ifirst                               = 1
>     ilast                                = length insts
>     iselect                              = floor $ denorm norm (fromIntegral ifirst, fromIntegral ilast)
>
> wideOpen               :: (Pitch, Pitch) = (pitch 0, pitch 127)
>
> type PercussionPair                      = [PercussionSound]
>
> getPairs               :: [PercussionPair]
> getPairs                                 = [ [AcousticBassDrum, BassDrum1], [LowMidTom, HiMidTom]]

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
> type InstDB = Map InstrumentName Int32
> type OorDB =  Map OorCase        Int32
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
>                           → Map PercussionSound Int32
>                           → Map PercussionSound Int32
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
> pitchToPerc            :: AbsPitch → Maybe PercussionSound
> pitchToPerc ap                           =
>   let
>     ad                                   = ap - 35
>   in
>     if ad >= fromEnum AcousticBassDrum && ad <= fromEnum OpenTriangle
>     then Just (toEnum ad)
>     else Nothing

apply fuzzyfind to mining instruments + percussion ====================================================================

> class GMPlayable a where
>   getProFFKeys         :: a → Maybe [String]
>   getConFFKeys         :: a → Maybe [String]
>   getList              :: [a]
>   getProMatches        :: FFMatches → [(a, (String, Fuzz))]
>   getConMatches        :: FFMatches → [(a, (String, Fuzz))]
>
> instance GMPlayable InstrumentName where
>   getProFFKeys                           = instrumentProFFKeys
>   getConFFKeys                           = instrumentConFFKeys
>   getList                                = map toEnum [fromEnum AcousticGrandPiano .. fromEnum Gunshot]
>   getProMatches                          = instAs
>   getConMatches                          = instBs
>
> instance GMPlayable PercussionSound where
>   getProFFKeys                           = percussionProFFKeys
>   getConFFKeys                           = percussionConFFKeys
>   getList                                = map toEnum [fromEnum AcousticBassDrum .. fromEnum OpenTriangle]
>   getProMatches                          = percAs
>   getConMatches                          = percBs
>
> type Fuzz = Double
>
> instrumentConFFKeys    :: InstrumentName → Maybe [String]
> instrumentConFFKeys inst                 =
>    case inst of
>       AcousticGrandPiano        → Just            ["upright", "bright", "mellow", "elec"]
>       Trumpet                   → Just $ singleton "mute"
>       _                         → Nothing
>
> instrumentProFFKeys    :: InstrumentName → Maybe [String]
> instrumentProFFKeys inst                 =
>    case inst of
>       AcousticGrandPiano        → Just            ["piano", "grand"]
>       BrightAcousticPiano       → Just            ["piano", "bright", "brite"]
>       ElectricGrandPiano        → Just            ["piano", "elec"]
>       HonkyTonkPiano            → Just            ["piano", "honkytonk"]
>       RhodesPiano               → Just            ["piano", "rhodes"]
>       ChorusedPiano             → Just            ["piano", "chorused"]
>       Harpsichord               → Just            ["harpsi", "harpsichord"]
>       Clavinet                  → Just $ singleton "clavinet"
>       Celesta                   → Just $ singleton "celesta"
>       Glockenspiel              → Just $ singleton "glockenspiel"
>       MusicBox                  → Just $ singleton "musicbox"
>       Vibraphone                → Just            ["vibra", "phone"]
>       Marimba                   → Just $ singleton "marimba"
>       Xylophone                 → Just $ singleton "xylophone"
>       TubularBells              → Just            ["bells", "tubular"]
>       Dulcimer                  → Just $ singleton "dulcimer"
>       HammondOrgan              → Just            ["org", "hammond"]
>       PercussiveOrgan           → Just            ["org", "percuss"]
>       RockOrgan                 → Just            ["org", "rock"] 
>       ChurchOrgan               → Just            ["org", "church"]
>       ReedOrgan                 → Just            ["org", "reed"]
>       Accordion                 → Just $ singleton "accordion"
>       Harmonica                 → Just $ singleton "harmonica"
>       TangoAccordion            → Just            ["accordion", "tango"]
>       AcousticGuitarNylon       → Just            ["guit", "nylon"]
>       AcousticGuitarSteel       → Just            ["guit", "steel"]
>       ElectricGuitarJazz        → Just            ["guit", "jazz"]
>       ElectricGuitarClean       → Just            ["guit", "clean"]
>       ElectricGuitarMuted       → Just            ["guit", "mute"]
>       OverdrivenGuitar          → Just            ["guit", "overdrive"]
>       DistortionGuitar          → Just            ["guit", "fuzz", "distort"]
>       GuitarHarmonics           → Just            ["guit", "harmonics"]
>       AcousticBass              → Just            ["bass", "acou"]
>       ElectricBassFingered      → Just            ["bass", "elec", "finger"]
>       ElectricBassPicked        → Just            ["bass", "elec", "pick"]
>       FretlessBass              → Just            ["bass", "fretless"] 
>       SlapBass1                 → Just            ["bass", "slap", "1"]
>       SlapBass2                 → Just            ["bass", "slap", "2"]
>       SynthBass1                → Just            ["bass", "synth", "1"]
>       SynthBass2                → Just            ["bass", "synth", "2"]
>       Violin                    → Just $ singleton "violin"
>       Viola                     → Just $ singleton "viola"
>       Cello                     → Just $ singleton "cello"
>       Contrabass                → Just $ singleton "contrabass"
>       TremoloStrings            → Just            ["string", "tremolo"]
>       PizzicatoStrings          → Just            ["string", "pizzicato"]
>       OrchestralHarp            → Just            ["harp", "orchest"]
>       Timpani                   → Just            ["timpani", "timp"]
>       StringEnsemble1           → Just            ["ensemble", "string", "1"]
>       StringEnsemble2           → Just            ["ensemble", "string", "2"]
>       SynthStrings1             → Just            ["string", "synth", "1"]
>       SynthStrings2             → Just            ["string", "synth", "2"]
>       ChoirAahs                 → Just            ["choir", "aahs", "chorus"]
>       VoiceOohs                 → Just            ["voice", "oohs", "chorus"]
>       SynthVoice                → Just            ["voice", "synth"]
>       OrchestraHit              → Just            ["orchest", "hit"]
>       Trumpet                   → Just            ["trumpet", "trump"]
>       Trombone                  → Just $ singleton "trombone"
>       Tuba                      → Just $ singleton "tuba"
>       MutedTrumpet              → Just            ["trumpet", "mute"]
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
>       GuitarFretNoise           → Just            ["guit", "fret", "noise"]
>       BreathNoise               → Just            ["breath", "noise"]
>       Seashore                  → Just $ singleton "seashore"
>       BirdTweet                 → Just            ["bird", "tweet"]
>       TelephoneRing             → Just            ["ring", "telephone"]
>       Helicopter                → Just $ singleton "helicopter"
>       Applause                  → Just $ singleton "applause"
>       Gunshot                   → Just $ singleton "gunshot"
>       _                         → Nothing
>
> percussionConFFKeys    :: PercussionSound → Maybe [String]
> percussionConFFKeys perc                 = Nothing
>
> percussionProFFKeys    :: PercussionSound → Maybe [String]
> percussionProFFKeys perc =
>    case perc of
>       AcousticBassDrum          → Just            ["drum", "acou", "bass"]
>       BassDrum1                 → Just            ["kick", "drum"]
>       SideStick                 → Just            ["side", "stick"]
>       AcousticSnare             → Just            ["snare", "drum", "acou"]
>       HandClap                  → Just            ["clap", "hand"]
>       ElectricSnare             → Just            ["snare", "elec", "drum"]
>       LowFloorTom               → Just            ["tom", "floor", "low"]
>       ClosedHiHat               → Just            ["hihat", "close"]
>       HighFloorTom              → Just            ["tom", "high", "floor"]
>       PedalHiHat                → Just            ["hihat", "pedal"]
>       LowTom                    → Just            ["tom", "low"]
>       OpenHiHat                 → Just            ["hihat", "open"]
>       LowMidTom                 → Just            ["tom", "mid", "low"]
>       HiMidTom                  → Just            ["tom", "high", "mid"]
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
>       HiBongo                   → Just            ["bongo", "hi"]
>       LowBongo                  → Just            ["bongo", "low"]
>       MuteHiConga               → Just            ["conga", "mute", "hi"]
>       OpenHiConga               → Just            ["conga", "open", "hi"]
>       LowConga                  → Just            ["conga", "low"]
>       HighTimbale               → Just            ["timbale", "hi"]
>       LowTimbale                → Just            ["timbale", "low"]
>       HighAgogo                 → Just            ["agogo", "hi"]
>       LowAgogo                  → Just            ["agogo", "low"]
>       Cabasa                    → Just            ["cabasa"]
>       Maracas                   → Just            ["maracas"]
>       ShortWhistle              → Just            ["whistle", "short"]
>       LongWhistle               → Just            ["whistle", "long"]
>       ShortGuiro                → Just            ["guiro", "short"]
>       LongGuiro                 → Just            ["guiro", "long"]
>       Claves                    → Just            ["claves"]
>       HiWoodBlock               → Just            ["woodblock", "hi"]
>       LowWoodBlock              → Just            ["woodblock", "low"]
>       MuteCuica                 → Just            ["cuica", "mute"]
>       OpenCuica                 → Just            ["cuica", "open"]
>       MuteTriangle              → Just            ["triangle", "mute"]
>       OpenTriangle              → Just            ["triangle", "open"]
>
> adhocFuzz              :: String → [String] → [Maybe FF.Alignment]
> adhocFuzz inp                            = map (`FF.bestMatch` inp)
>

handle "matching as" cache misses =====================================================================================

> computeMatchingAs      :: ∀ a. (GMPlayable a) ⇒ String → Bool → [(a, (String, Fuzz))]
> computeMatchingAs inp pro                = sortOn (Down . snd) asScored
>   where
>     -- weed out candidates with no fuzzy keys
>     asLooks            :: [(a, [String])]
>                                          = mapMaybe eval1 getList
>
>     eval1              :: a → Maybe (a, [String])
>     eval1 kind                           = Just . (kind,) =<< (if pro then getProFFKeys else getConFFKeys) kind
>
>     -- weed out candidates with no fuzzy key matches
>     asScored           :: [(a, (String, Fuzz))]
>                                          = mapMaybe (evalAgainstKeys inp) asLooks
>
> evalAgainstKeys        :: ∀ a. (GMPlayable a) ⇒ String → (a, [String]) → Maybe (a, (String, Double))
> evalAgainstKeys inp (kind, keys)         = if tot <= 0 then Nothing else Just (kind, (inp, tot))
>   where
>     lFactor        :: Double             = sqrt $ fromIntegral $ length keys
>     weights        :: [Double]           = [1.6 / lFactor
>                                           , 1.4 / lFactor
>                                           , 1.25 / lFactor
>                                           , 1.17 / lFactor
>                                           , 1.14 / lFactor]
>     tot            :: Double             = sum $ zipWith evalAgainstOne keys weights
>
>     evalAgainstOne     :: String → Double → Double
>     evalAgainstOne key weight            = maybe 0 ((* weight) . fromIntegral . FF.score) (FF.bestMatch key inp)
>

use "matching as" cache ===============================================================================================

> evalAgainstKind        :: ∀ a. (GMPlayable a, Eq a) ⇒ a → FFMatches → Fuzz
> evalAgainstKind kind ffs                 = maybe 0 snd (lookup kind (getProMatches ffs))
>                                            - maybe 0 snd (lookup kind (getConMatches ffs))
>   
> bestQualifying         :: ∀ a. (GMPlayable a) ⇒ [(a, (String, Fuzz))] → Double → Maybe (a, (String, Fuzz))
> bestQualifying as thresh
>   | null as                              = Nothing
>   | (snd . snd . head) as < thresh       = Nothing
>   | otherwise                            = Just (head as)
>
> data FFMatches =
>   FFMatches {
>     instAs             :: [(InstrumentName, (String, Fuzz))]
>   , percAs             :: [(PercussionSound, (String, Fuzz))]
>   , instBs             :: [(InstrumentName, (String, Fuzz))]
>   , percBs             :: [(PercussionSound, (String, Fuzz))]} deriving Show
>
> computeFFMatches       :: String → FFMatches
> computeFFMatches inp                     = FFMatches ias pas ibs pbs
>   where
>     ias = computeMatchingAs inp True
>     pas = computeMatchingAs inp True
>     ibs = computeMatchingAs inp False
>     pbs = computeMatchingAs inp False

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

Sampling ==============================================================================================================

> type SampleAnalysis    = Double
>
> toSamples              :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → [Double]
> toSamples dur sf
>   | traceIf msg False                    = undefined
>   | otherwise                            = take numSamples $ concatMap collapse $ unfold $ strip sf
>   where
>     sr                                   = rate     (undefined :: p)
>     numChannels                          = numChans (undefined :: a)
>     numSamples                           = truncate (dur * sr) * numChannels
>
>     msg = unwords ["toSamples   dur= " ++ show dur
>                             ++ " sr= " ++ show sr
>                             ++ " ch= " ++ show numChannels
>                             ++ " ns= " ++ show numSamples]
>
> maxSample              :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → Double
> maxSample dur sf                         = maximum (map abs (toSamples dur sf))
>
> class AudioSample a ⇒ WaveAudioSample a where
>   retrieve             :: UArray Int Int32 → Int → a
>
> instance WaveAudioSample Double where
>   retrieve             :: UArray Int Int32 → Int → Double
>   retrieve sData idx                     = fromIntegral (sData ! idx)
>
> instance WaveAudioSample (Double,Double) where
>   retrieve             :: UArray Int Int32 → Int → (Double, Double)
>   retrieve sData idx                     = (fromIntegral (sData ! idx), fromIntegral (sData ! (idx + 1)))
>
> data SlwRate
> instance Clock SlwRate where
>   rate _ = 4.41
> type SlwSF a b  = SigFun SlwRate a b
>
> sumUpFft               :: [[Double]] → Double
> sumUpFft xs                              =
>   let
>     (t, l)                               = foldl' (summer (qqNumTakeFftChunks defC)) (0,0) xs
>   in t/l
>
> summer                 :: Int → (Double, Double) → [Double] → (Double, Double)
> summer n (t, l) xs                   = (t', l')
>   where
>     n' = min n (length xs)
>     t' = t + sum (take n' xs)
>     l' = l + fromIntegral n'
>
> showFft                :: Int → [[Double]] → String
> showFft nChunks vFft                     = concatMap (showChunk vFft) [0..]
>   where
>     showChunk          :: [[Double]] → Int → String
>     showChunk vFft ix                    = show ix ++ ">" ++ show (vFft !! ix) ++ "\n"
>
> delay'                 :: ∀ p . Clock p ⇒ Signal p (Double, Double) Double
> delay'                                   =
>     proc (x, _) → do
>       y ← delay 0                        ⤙ x  
>       outA                               ⤙ y
>
> type Node = Int
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
> cyclicNodes graph =
>   map fst . filter isCyclicAssoc . assocs $ graph
>   where
>    isCyclicAssoc = uncurry $ reachableFromAny graph
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
>     (not $ isNaN y || isInfinite y || isDenormalized y || abs y > 200000)
>     (msg ++ " bad Double = " ++ show y)
>                                              y
>
> profess                :: Bool → String → a → a
> profess assertion msg something          = if not assertion
>                                              then error ("Failed assertion -- " ++ msg)
>                                              else something
>
> professIsJust          :: ∀ a. Maybe a → String → a
> professIsJust item msg                   = profess (isJust item) msg (fromJust item)
>
> professIsJust'         :: ∀ a. Maybe a → a
> professIsJust' item                      = professIsJust item "expected Just"
>
> sumOfMaybeInts         :: [Maybe Int] → Int
> sumOfMaybeInts                           = foldr ((+).fromMaybe 0) 0
>       
> addIntToWord           :: Word → Int → Word
> addIntToWord w i                         = fromIntegral sum
>   where
>     iw                 :: Int            = fromIntegral w
>     sum                :: Int            = iw + i
>
> data Continuity =
>     Linear
>   | Concave
>   | Convex
>   | Switch deriving (Eq, Ord, Show, Enum)
>
> wrap :: (Ord n, Num n) ⇒ n → n → n
> wrap val bound = if val > bound then wrap val (val-bound) else val
>
> safeAt                 :: Show a ⇒ Array Word a → Word → a
> safeAt v i                               = profess
>                                              (i >= imin && i <= imax)
>                                              ("safeAt " ++ show i)
>                                              (v ! i)
>   where
>     (imin, imax)                         = bounds v
>
> clip :: Ord n ⇒ (n, n) → n → n
> clip (lower, upper) val
>     | val <= lower = lower
>     | val >= upper = upper
>     | otherwise    = val
>
> safeRange :: Integral n ⇒ n → n → [n]
> safeRange x y = if x > y || y == 0 then [] else [x..(y-1)]

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
>
> unquoteCodeString      :: String → String
> unquoteCodeString                        = concatMap unquote
>   where
>     unquote            :: Char → String
>     unquote c = case c of
>                 '\\'   → ""
>                 _      → [c]

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportName             :: FilePath
> reportName                               = "TournamentReport'.lhs"

Returns the amplitude ratio

> fromCentibels          :: Maybe Int → Double
> fromCentibels mcentibels                 = fromCentibels' $ fromIntegral (fromMaybe 0 mcentibels)

> fromCentibels'         :: Double → Double
> fromCentibels' centibels                 = pow 10 (centibels/1000)

Returns the elapsed time in seconds

> fromTimecents          :: Maybe Int → Double
> fromTimecents mtimecents                 = pow 2 (fromIntegral (fromMaybe (-12000) mtimecents)/1200)

> fromTimecents'         :: Maybe Int → Maybe Int → KeyNumber → Double
> fromTimecents' mtimecents mfact key      = pow 2 (base + inc)
>   where
>     base               :: Double         = fromIntegral (fromMaybe (-12000) mtimecents) / 1200
>     inc                :: Double         = fromIntegral (fromMaybe 0 mfact) * fromIntegral (60 - key) / 128 / 1200

Returns the attenuation (based on input 10ths of a percent)

> fromTithe              :: Maybe Int → Double
> fromTithe iS                             = 1 / pow 10 (jS/200)
>   where
>     jS                 :: Double         = maybe 0 (clip (0, 1000) . fromIntegral) iS

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
> data Mapping =
>   Mapping {
>     msContinuity     :: Continuity
>   , msBiPolar        :: Bool  
>   , msMax2Min        :: Bool
>   , msCCBit          :: Bool} deriving (Eq, Ord, Show)
>
> defMapping                               = Mapping Linear False False False
> allMappings                              = [Mapping cont polar dir False
>                                                   | cont   ← [Linear, Concave, Convex, Switch]
>                                                        , polar  ← [False, True]
>                                                              , dir    ← [False, True]]                                          

Returns sample point as (normalized) Double

> sample24               :: Int16 → Word8 → Double
> sample24 i16 w8                          = fromIntegral (f16to32 i16 * 256 + f8to32 w8) / 8388608.0
>   where
>     f8to32             :: Word8 → Int32  = fromIntegral
>     f16to32            :: Int16 → Int32  = fromIntegral
>      
> sample16               :: Int16 → Double
> sample16 i16                             = fromIntegral i16 / 32768.0
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

Emission capability ===================================================================================================

> data Emission =
>   ToFieldL String Int
>   | ToFieldR String Int
>   | Unblocked String
>   | Blanks Int
>   | Empty 
>   | EndOfLine deriving Show
>
> makeString             :: Emission → String
> makeString e                             = case e of
>                                            ToFieldL str sz         → fillFieldL str sz
>                                            ToFieldR str sz         → fillFieldR str sz
>                                            Unblocked str           → str
>                                            Blanks sz               → replicate sz ' '
>                                            Empty                   → ""
>                                            EndOfLine               → "\n"
>
> emitLine               :: [Emission] → [Emission]
> emitLine es                              = [literate] ++ es ++ [EndOfLine]
>
> literate               :: Emission       = ToFieldL ">" 2
>
> commaOrNot             :: Int → Emission
> commaOrNot nth                           = if nth == 0
>                                              then ToFieldL ""  2
>                                              else ToFieldL "," 2
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
> emitShowL              :: ∀ a. (Show a) ⇒ a → Int → Emission
> emitShowL a                              = ToFieldL (show a)
>
> emitShowR              :: ∀ a. (Show a) ⇒ a → Int → Emission
> emitShowR a                              = ToFieldR (show a)
>
> emitDefault            :: ∀ a. (Show a) ⇒ a → Emission
> emitDefault a                            = Unblocked (show a)
>
> gmId                   :: ∀ a. (Show a) ⇒ a → Emission
> gmId i                                   = emitShowL i 22
>
> gmName                 :: String → Emission
> gmName str                               = emitShowL str 22
>
> reapEmissions          :: [Emission] → String
> reapEmissions es                         = concat $ foldl' reapItem [""] es
>
> reapItem               :: [String] → Emission → [String]
> reapItem outP e                          = init outP ++ [last outP ++ makeString e]
>
> fillFieldL             :: String → Int → String
> fillFieldL str fieldSz                   = str ++ safeReplicate (length str) fieldSz ' '
>
> fillFieldR             :: String → Int → String
> fillFieldR str fieldSz                   = safeReplicate (length str) fieldSz ' ' ++ str
>
> safeReplicate          :: Int → Int → Char → String
> safeReplicate sz maxSz                   = profess
>                                              (sz <= maxSz)
>                                              ("safeReplicate " ++ show sz ++ " " ++ show maxSz) 
>                                              (replicate (maxSz - sz))
>
> emitTaggedLine         :: String → String → String → [Emission]
> emitTaggedLine prolog item epilog        = emitLine [Unblocked prolog, Unblocked item, Unblocked epilog]
>
> emitPragmaLine         ::  String → [Emission]
> emitPragmaLine lang                      = emitTaggedLine "{-# LANGUAGE " lang " #-}"
>
> emitModuleLine         ::  String → [Emission]
> emitModuleLine mod                       = emitTaggedLine "module " mod " where"
>
> emitImportLine         ::  String → [Emission]
> emitImportLine imp                       = emitTaggedLine "import " imp ""
>
> writeFileBySections    :: FilePath → [[Emission]] → IO ()
> writeFileBySections fp eSections = do
>   let zSections        = map reapEmissions eSections
>   appendFile fp (concat zSections)
>
> type Velocity                            = Volume
> type KeyNumber                           = AbsPitch

Configurable parameters ===============================================================================================

> diagnosticsEnabled                       = qqDiagnosticsEnabled         defC 
> skipReporting                            = qqSkipReporting              defC 
> skipGlissandi                            = qqSkipGlissandi              defC
>
> data ControlSettings =
>   ControlSettings {
>     qqDiagnosticsEnabled                 :: Bool
>   , qqSkipReporting                      :: Bool
>   , qqSkipGlissandi                      :: Bool
>   , qqDumpFftChunks                      :: Bool
>   , qqNumTakeFftChunks                   :: Int} deriving (Eq, Show)
>
> defC                   :: ControlSettings
> defC =
>   ControlSettings {
>     qqDiagnosticsEnabled                 = False
>   , qqSkipReporting                      = False
>   , qqSkipGlissandi                      = False
>   , qqDumpFftChunks                      = False
>   , qqNumTakeFftChunks                   = 3}

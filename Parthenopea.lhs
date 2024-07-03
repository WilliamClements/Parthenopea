> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
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
> import Data.Complex
> import Data.Either
> import Data.Graph (Graph)
> import qualified Data.Graph              as Graph
> import Data.Int ( Int8, Int16, Int32 )
> import Data.IntSet (IntSet)
> import qualified Data.IntSet             as IntSet
> import Data.List ( iterate', singleton, foldl', sortOn, minimumBy )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( fromJust, isJust, isNothing, mapMaybe, fromMaybe )
> import Data.MemoTrie
> import Data.Ord
> import Data.Ratio ( approxRational )
> import Data.Set (Set)
> import qualified Data.Set                as Set
> import Data.Word
> import Debug.Trace ( trace )
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg, Table, tableSinesN, osc )
> import Euterpea.IO.Audio.IO
> import Euterpea.IO.Audio.Types
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.MidiIO (unsafeOutputID, unsafeInputID, OutputDeviceID, InputDeviceID)
> import Euterpea.IO.MIDI.Play
> import Euterpea.IO.MIDI.ToMidi2 (writeMidi2)
> import Euterpea.Music
> import GHC.Generics (Generic) 
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
> hzToAp                 :: Double -> AbsPitch
> hzToAp freak                             =
>   round $ fromIntegral (absPitch (A,4)) + 12 * (logBase 2 freak - logBase 2 440)
>
> theE                   :: Double
> theE                                     = 2.718_281_828_459_045_235_360_287_471_352_7
>
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
> ratEps = 0.000_1
>
> approx       :: Double → Dur
> approx dur = approxRational dur ratEps
>
> rawPitches = [0..127]
>
> allPitches =
>    foldr ((:+:) . notize) (rest 0) rawPitches
>    where
>       notize aP = note qn (pitch aP)
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
>                 , perfAlg= map (\e → e{eDur = max 0 (eDur e - 0.000_001)}) . perform}

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
>   rkP                                    = absPitch (key, snd base - 1)
>   bP                                     = absPitch base
>   ocd                                    = (bP - rkP) `div` 12
>   kP                                     = rkP + (12*ocd)
>   apD                                    = bP - kP           -- guaranteed to be nonnegative
>   is                   :: [AbsPitch]     = formExact apD mode
>   n1, n2, n3           :: Music Pitch
>   n1                                     = note dur $ pitch bP
>   n2                                     = note dur $ pitch (bP + head        is)
>   n3                                     = note dur $ pitch (bP + (head.tail) is)
>
>   formExact :: AbsPitch → Mode → [AbsPitch]
>   formExact apDelta mode                 = offsets2intervals apDelta $ mode2offsets mode
>     where
>       major                              = [0, 4, 7, 12, 16]
>       minor                              = [0, 3, 7, 12, 15]
>       dim                                = [0, 3, 6, 12, 15]
>       sus4                               = [0, 5, 7, 12, 17]
>       sus2                               = [0, 2, 7, 12, 14]
>       aug                                = [0, 4, 8, 12, 16]
>
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
>
>       offsets2intervals :: AbsPitch → [AbsPitch] → [AbsPitch]
>       offsets2intervals apDelta os
>         | apDelta == a                   = (b - a) : [c - a]
>         | apDelta == b                   = (c - b) : [d - b]
>         | apDelta == c                   = (d - c) : [e - c]
>         | otherwise          = error "Malformed Triad"
>         where
>           a                              = head os
>           b                              = os!!1
>           c                              = os!!2
>           d                              = os!!3
>           e                              = os!!4

"ascent/descent" ======================================================================================================

> glissando'              :: [AbsPitch] → Dur → Music Pitch
> glissando' gliss dur                     = dim 1 $ slur 2 $ line notes
>   where
>     reach              :: Rational       = fromIntegral $ length gliss
>     notes              :: [Music Pitch]  = [note (dur * 9 / (reach * 10)) (pitch x) | x ← gliss]
>
> glissando              :: Bool → (AbsPitch, AbsPitch) → Dur → Music Pitch
> glissando _ _ 0                          = rest 0
> glissando desc (xLo, xHi) dur
>   | traceIf trace_G False                = undefined
>   | skipGlissandi                        = rest dur
>   | xHi < xLo + 6                        = error (unwords ["glissando:"
>                                                          , show (pitch xLo, pitch xHi)
>                                                          , "is not enough range"])
>   | dur < 1 / 8                          = error (unwords ["glissando:"
>                                                          , show dur
>                                                          , "is not enough duration"])
>   | otherwise                            = glissando' (take 28 pitches) dur
>   where
>     pitches                              = if desc
>                                              then reverse [xLo..xHi]
>                                              else [xLo..xHi]
>     trace_G                              = unwords ["glissando", show pitches]
>
> descent                :: BandPart → Pitch → Dur → Music Pitch
> descent BandPart{ .. } p dur
>   | traceNow trace_D False               = undefined
>   | otherwise                            = chord [rest dur, glissando True (absPitch bottom, absPitch p) dur]
>   where
>     bottom                               = trans (-bpTranspose) $ fst (fromJust (instrumentRange bpInstrument))
>
>     trace_D                              = unwords ["descent ", show p]
>
> ascent                 :: BandPart → Pitch → Dur → Music Pitch
> ascent BandPart{ .. } p dur
>   | traceNow trace_A False               = undefined
>   | otherwise                            = chord [rest dur, glissando False (absPitch p, absPitch top) dur]
>   where
>     top                                  = trans (-bpTranspose) $ snd (fromJust (instrumentRange bpInstrument))
>
>     trace_A                              = unwords ["ascent", show p]

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
>       Percussion                → Nothing -- was Just wideOpen
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
>     rangedInsts                          = mapMaybe judgeScore getList
>
>     judgeScore         :: InstrumentName → Maybe (Int, InstrumentName)
>     judgeScore cand                      = (\c → if nonPitchedInstrument c then Nothing else Just c) cand
>                                            >>= instrumentRange >>= uncurry (fitsIn cand)
>     
>     fitsIn             :: InstrumentName → Pitch → Pitch → Maybe (Int, InstrumentName)
>     fitsIn cand rangeLo rangeHi
>       | traceNever trace_FI False        = undefined
>       | otherwise                        =
>         if inRange range playedLo && inRange range playedHi 
>           then Just (snd range - fst range, cand)
>           else Nothing
>       where
>         range                            = (absPitch rangeLo, absPitch rangeHi)
>         trace_FI                         = unwords ["fitsIn", show range, show playedLo, show playedHi]
>
>     trace_FBI                            = unwords ["findBetterInstrument", show than, show rangedInsts]
>
> nonPitchedInstruments  :: [InstrumentName]
> nonPitchedInstruments                    = filter nonPitchedInstrument getList
>
> selectRanged           :: [InstrumentName] → Array Int (InstrumentName, (Pitch, Pitch))
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
> wideOpen               :: (Pitch, Pitch) = (pitch 0, pitch 127)
>
> type PercussionPair                      = [PercussionSound]
>
> getPairs               :: [PercussionPair]
> getPairs                                 = [ [AcousticBassDrum, BassDrum1], [LowMidTom, HiMidTom]]

instrument range checking =============================================================================================

> union2Ranges :: (Ord a, Ord b) ⇒ (a, b) → (a, b) → (a, b)
> union2Ranges r1 r2 = unionRanges (r1:[r2])
> unionRanges [] = error "empty range list"
> unionRanges (r:rs) = ( minimum (map fst (r:rs))
>                      , maximum (map snd (r:rs)) )
> intersect2Ranges r1 r2 = intersectRanges (r1:[r2])
> intersectRanges (r:rs) =
>   case uncurry compare inverted of
>     LT → Just inverted
>     _  → Nothing
>   where
>     inverted = ( maximum (map fst (r:rs))
>                , minimum (map snd (r:rs)) )
> intersectRanges _ = error "empty range list"
>
> data BandPart =
>   BandPart {
>     bpInstrument       :: InstrumentName
>   , bpTranspose        :: AbsPitch
>   , bpVelocity         :: Velocity}
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
> makeDynMap Shredding{ .. }               =
>   if replacePerCent > 50
>     then foldl' maker Map.empty (Map.assocs shRanges)
>     else Map.empty
>   where
>     maker              :: DynMap → (Kind, Shred) → DynMap
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
> bandPart_              :: (InstrumentName, Velocity) → Music Pitch → Music (Pitch, Volume)
> bandPart_ (inst, vel) m                  = mMap (, vel) (instrument inst m)
> bandPart               :: BandPart → Music Pitch → Music (Pitch, Volume)
> bandPart BandPart{ .. } m                = mMap (, bpVelocity) (instrument bpInstrument (transpose bpTranspose m))

examine song for instrument and percussion usage ======================================================================

> type Kind                                = Either InstrumentName PercussionSound
> type AgainstKindResult                   = Double
> 
> data Shred =
>   Shred {
>       shLowNote        :: MEvent
>     , shHighNote       :: MEvent
>     , shCount          :: Int} deriving (Show, Eq, Ord)
>
> data Shredding =
>   Shredding {
>       shRanges           :: Map Kind Shred
>     , shMsgs             :: [(InstrumentName, [String])]} deriving (Show, Eq, Ord)
> defShredding                             = Shredding Map.empty []
>
> getKind                :: MEvent → Kind
> getKind MEvent{eInst, ePitch}            =
>   case eInst of
>     Percussion                           → Right $ toEnum (ePitch - 35)
>     _                                    → Left eInst
>
> shimSong                :: Music (Pitch, [NoteAttribute]) → DynMap → Music (Pitch, [NoteAttribute])
> shimSong m a                             = m
>
> shredMusic              :: ToMusic1 a ⇒ Music a → IO Shredding
> shredMusic m                             =
>   return $ critiqueMusic $ foldl' shredder defShredding $ fst (musicToMEvents defaultContext (toMusic1 m))
>
> critiqueMusic          :: Shredding → Shredding
> critiqueMusic Shredding{ .. }            = Shredding shRanges (concatMap critiqueShred (Map.assocs shRanges))
>
> critiqueShred          :: (Kind, Shred) → [(InstrumentName, [String])]
> critiqueShred (kind, Shred{ .. })        =
>   let
>     (instr, range)                       =
>       case kind of
>         Left iName                       → (iName, fromMaybe wideOpen (instrumentRange iName))
>         _                                → (Percussion, wideOpen)
>   in critiqueNote instr range shLowNote ++ critiqueNote instr range shHighNote
> 
> critiqueNote           :: InstrumentName → (Pitch, Pitch) → MEvent → [(InstrumentName, [String])]
> critiqueNote name range MEvent{ .. }       =
>   let
>     p                                      = pitch ePitch
>   in
>     if p == clipPitch range p
>       then []
>       else singleton (name, singleton $ unwords ["...", show p, "out of range", show range])
>
> shredder               :: Shredding → MEvent → Shredding
> shredder Shredding{ .. } mev             =
>   let
>     kind               :: Kind           = getKind mev
>     mshred             :: Maybe Shred    = Map.lookup kind shRanges
>   in
>     case mshred of
>       Nothing                            → Shredding
>                                              (Map.insert kind (Shred mev mev 1) shRanges)
>                                              shMsgs
>       Just shred                         → Shredding
>                                              (Map.insert kind (upd shred)       shRanges)
>                                              shMsgs
>   where
>     upd Shred{ .. }                      = Shred
>                                              (if ePitch mev < ePitch shLowNote
>                                                 then mev
>                                                 else shLowNote)
>                                              (if ePitch mev > ePitch shHighNote
>                                                 then mev
>                                                 else shHighNote)
>                                              (shCount + 1)
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
> printShred             :: Kind → Shred → IO ()
> printShred kind Shred{ .. }              = do
>   putStrLn showGivenRange
>   putStrLn showLowHighNotes
>
>   where
>
>   mrange               :: Maybe (Pitch, Pitch)
>                                          =
>     case kind of
>       Left iname                         → instrumentRange iname
>       _                                  → Nothing
>   range                                  = BF.bimap absPitch absPitch (fromJust mrange)
>
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
>   showShred MEvent{ .. }                 =
>     case kind of
>       Left _                             → show (fromRational eTime)
>                                            ++ show (pitch ePitch)
>                                            ++ showOutOfRangeIndicator ePitch
>       _                                  → show (fromRational eTime)
>   showOutOfRangeIndicator p              = if isNothing mrange || inRange range p
>                                              then "."
>                                              else "!"
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
>   | traceAlways trace_SO False           = undefined
>   | otherwise                            = hist (0, nBins - 1) is
>   where
>      safemax, safemin :: [Double] → Double
>      safemax ts
>        | null ts                         = 0
>        | otherwise                       = maximum ts
>      safemin ts
>        | null ts                         = 0
>        | otherwise                       = minimum ts
>   
>      hi, lo, fact      :: Double
>      hi                                  = safemax ts + 0.000_000_1
>      lo                                  = safemin ts - 0.000_000_1
>      fact                                = fromIntegral nBins / (hi - lo)
>
>      is                :: [Int]
>      is                                  = map (\d → floor $ (d - lo) * fact) ts
>
>      trace_SO =
>        unwords [ "scoreOnsets lo, hi, fact="
>                , show (safemin ts)
>                , ":", show (safemax ts)
>                , ":", show fact
>                , "ts="
>                , show (length ts)
>                , "is="
>                , show (length is)]
>
> hist         :: (Ix a, Integral b) ⇒ (a,a) → [a] → Array a b
> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i ← is, inRange bnds i]
>
> defBins      :: Int
> defBins = 32
>
> scoreMusic             :: Int → Array Int (Music (Pitch, [NoteAttribute])) → Array Int Int
> scoreMusic nBins m
>   | traceIf trace_SM False               = undefined
>   | otherwise                            = score
>   where
>     score              :: Array Int Int
>     score                                = 
>       scoreOnsets nBins
>       $ extractTimes
>       $ collectMEvents
>       $ chordFromArray m
>
>     trace_SM                             = unwords [ "bins", show $ elems score]
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
>     mel3                                 = (invert.retro) melInput

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

-----------------------------------------------------------------------------

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

Sampling ==============================================================================================================

> type SampleAnalysis    = Double
>
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
> toSamples              :: ∀ a p. (AudioSample a, Clock p) ⇒ Double → Signal p () a → [a]
> toSamples secs sig                       = take numSamples $ unfold $ strip sig
>   where
>     sr                                   = rate     (undefined :: p)
>     numChannels                          = numChans (undefined :: a)
>     numSamples                           = truncate (secs * sr)
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
(And you can count on the output being normalized!)

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
> instance AudioSample ((Double, Double), (ModSignals, ModSignals)) where
>    zero = ((0,0),(defModSignals, defModSignals))
>    mix ((a,b),(msL,msR)) ((c,d),(_,_)) = ((a+c,b+d), (msL, msR))
>    collapse ((a,b),(_,_)) = [a,b]
>    numChans _ = 2
>
> instance AudioSample (Complex Double) where
>    zero = 0
>    mix x y = x + y
>    collapse x = [realPart x]
>    numChans _ = 1
>
> class AudioSample a ⇒ WaveAudioSample a where
>   azero                :: a
>   acomplex             :: a → Complex Double
>   aamp                 :: a → Double
>   ascale               :: Double → a → a
>   aadd                 :: a → a → a
>   amul                 :: a → a → a
>   asqrt                :: a → a
>
> instance WaveAudioSample Double where
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
> instance WaveAudioSample (Double,Double) where
>   azero                :: (Double, Double)
>   azero                                  = (0, 0)
>   acomplex             :: (Double, Double) → Complex Double
>   acomplex                               = error "no acomplex for stereo type"
>   aamp                 :: (Double, Double) → Double
>   aamp (d, e)                            = sqrt (d * d + e * e)
>   ascale               :: Double → (Double, Double) → (Double, Double)
>   ascale d (e, f)                        = amul (d, d) (e, f)
>   aadd                 :: (Double, Double) → (Double, Double) → (Double, Double)
>   aadd (d, e) (f, g)                     = (d + f, e + g)
>   amul                 :: (Double, Double) → (Double, Double) → (Double, Double)
>   amul (d, e) (f, g)                     = (d * f, e * g)
>   asqrt                :: (Double, Double) → (Double, Double)
>   asqrt (d, e)                           = (sqrt d, sqrt e)
>
> instance WaveAudioSample (Complex Double) where
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
> data ModSignals                          =
>   ModSignals {
>     xModEnvPos         :: Double
>   , xModLfoPos         :: Double
>   , xVibLfoPos         :: Double} deriving (Show)
>
> defModSignals                            = ModSignals 0 0 0
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
> cyclicNodes graph =
>   (map fst . filter isCyclicAssoc . assocs) graph
>   where
>     isCyclicAssoc = uncurry (reachableFromAny graph)
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
>                                              y
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
> professIsJust          :: ∀ a. Maybe a → String → a
> professIsJust item msg                   = profess (isJust item) msg (fromJust item)
>
> professIsJust'         :: ∀ a. Maybe a → a
> professIsJust' item                      = professIsJust item "expected Just"
>
> sumOfMaybeInts         :: [Maybe Int] → Int
> sumOfMaybeInts                           = sum . map (fromMaybe 0)
>       
> addIntToWord           :: Word → Int → Word
> addIntToWord w i                         = fromIntegral sum
>   where
>     iw                 :: Int            = fromIntegral w
>     sum                :: Int            = iw + i
>
> wrap                   :: (Ord n, Num n) ⇒ n → n → n
> wrap val bound                           = if val > bound then wrap val (val-bound) else val
>
> clip                   :: Ord n ⇒ (n, n) → n → n
> clip (lower, upper) val                  = min upper (max lower val)
>
> clipPitch              :: (Pitch, Pitch) → Pitch → Pitch
> clipPitch (lower, upper) val             = pitch $ clip (lower', upper') val'
>   where
>     upper'                               = absPitch upper
>     lower'                               = absPitch lower
>     val'                                 = absPitch val
>
> makeRange              :: Integral n ⇒ n → n → [n]
> makeRange x y                            = if x > y || y <= 0 then [] else [x..(y-1)]
>
> almostEqual            :: Double → Double → Bool
> almostEqual 0 0                          = True
> almostEqual a b                          = 1e-8 > abs ((a - b) / (a + b))

Raises 'a' to the power 'b' using logarithms.

> pow                    :: Floating a ⇒ a → a → a
> pow a b                                  = exp (log a * b)

Returns the fractional part of 'x'.

> frac                   :: RealFrac r ⇒ r → r
> frac                                     = snd . properFraction

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

forms an IntSet based on an arbitrary list and corresponding input function to Int

> formIntSet             :: ∀ a . [a] → (a → Int) → IntSet
> formIntSet as fun                        = IntSet.fromList $ map fun as

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportName             :: FilePath       = "TournamentReport'.lhs"

Returns the amplitude ratio

> fromMaybeCentibels     :: Maybe Int → Double
> fromMaybeCentibels mcentibels            = fromCentibels $ fromIntegral (fromMaybe 0 mcentibels)

> fromCentibels          :: Double → Double
> fromCentibels centibels                  = pow 10 (centibels/1000)
>
> toCentibels            :: Double → Double
> toCentibels ratio                        = logBase 10 ratio * 1000

Returns amplitude, computed relative to a fixed one

> relativeAmp            :: Double → Double → Double → Double → Double
> relativeAmp freakFrom ampFrom freakTo rate
>                                          =
>   ampFrom * pow (freakFrom / freakTo) (rate / logBase 10 2)

Returns the elapsed time in seconds

> fromTimecents          :: Maybe Int → Double
> fromTimecents mtimecents                 = pow 2 (fromIntegral (fromMaybe (-12_000) mtimecents)/1_200)

> fromTimecents'         :: Maybe Int → Maybe Int → KeyNumber → Double
> fromTimecents' mtimecents mfact key      = pow 2 (base + inc)
>   where
>     base               :: Double         = fromIntegral (fromMaybe (-12_000) mtimecents) / 1_200
>     inc                :: Double         = fromIntegral (fromMaybe 0 mfact) * fromIntegral (60 - key) / 128 / 1_200

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
> allMappings                              = [Mapping cont polar dir False
>                                                   | cont   ← [Linear, Concave, Convex, Switch]
>                                                        , polar  ← [False, True]
>                                                              , dir    ← [False, True]]                                          

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

returns the lowest power of 2 greater than OR EQUAL TO the input value

> sampleUp               :: Int → Int
> sampleUp i                               =
>   if i <= 0
>     then error "out of range for sampleUp"
>     else head $ dropWhile (< i) (iterate' (* 2) 1)
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

r is the resonance radius, w0 is the angle of the poles and b0 is the gain factor

> data CoeffsM2N2                          =
>   CoeffsM2N2 {
>     m2n2_b0            :: Double
>   , m2n2_b1            :: Double
>   , m2n2_b2            :: Double
>   , m2n2_a1            :: Double
>   , m2n2_a2            :: Double} deriving (Eq, Show)
>
> extractCoefficients    :: Complex Double → (Double, Double)
> extractCoefficients porz                 = (k1, k2)
>   where
>     mag                                  = magnitude porz
>     ph                                   = phase porz
>
>     k1                                   = -2 * mag * cos ph
>     k2                                   = mag * mag
>
> indeedReplaceRadius                      = False
>
> pickZerosAndPoles      :: Double → Double → ([Complex Double], [Complex Double])
> pickZerosAndPoles initFc normQ           = (zeros, poles)
>   where
>     zeros, poles       :: [Complex Double]
>     zeros                                = [cis pi, cis pi]
>     poles                                = [p, conjugate p]
>     -- two identical zeros
>     -- two poles that are complex conjugates
>     p                                    =
>       mkPolar
>         (if indeedReplaceRadius
>            then 1 - normQ * sin (pi * initFc)
>            else normQ)
>         (2 * pi * initFc)
>     
> buildSystemM2N2        :: ([Complex Double], [Complex Double]) → CoeffsM2N2
> buildSystemM2N2 (zeros, poles)
>   | traceNot trace_BSM2N2 False          = undefined
>   | otherwise                            =
>   let
>     (z0, p0)                             =
>       profess
>         (length zeros == 2 && length poles == 2)
>         "only 2x2 systems are supportedi in ResonanceTwoPoles"
>         (head zeros, head poles)
>     (b1, b2)                         = extractCoefficients z0
>     (a1, a2)                         = extractCoefficients p0
>     b0                               = (1 + a1 + a2) / 4
>   in
>     CoeffsM2N2 b0 b1 b2 a1 a2
>   where
>     trace_BSM2N2                         = unwords ["buildSystemM2N2\n", show zeros, "\n", show poles]

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
>     ToFieldL str sz    → fillFieldL str sz
>     ToFieldR str sz    → fillFieldR str sz
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
>                                              (unwords ["safeReplicate", show (sz, maxSz)])
>                                              (replicate (maxSz - sz))
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
>   let zSections                          = map reapEmissions eSections
>   appendFile fp (concat zSections)
>
> type Velocity                            = Volume
> type KeyNumber                           = AbsPitch

Tracing ===============================================================================================================

> tracer                 :: Show a ⇒ String → a → a
> tracer str x                             =
>   if True
>     then traceNow (unwords [str, show x]) x
>     else x
>
> notracer               :: Show a ⇒ String → a → a
> notracer _ x                             = x

Configurable parameters ===============================================================================================

> diagnosticsEnabled                       = qqDiagnosticsEnabled         defC 
> skipReporting                            = qqSkipReporting              defC 
> skipGlissandi                            = qqSkipGlissandi              defC
> replacePerCent                           = qqReplacePerCent             defC
> usingPlayCache                           = qqUsingPlayCache             defC
>
> data ControlSettings =
>   ControlSettings {
>     qqDiagnosticsEnabled                 :: Bool
>   , qqSkipReporting                      :: Bool
>   , qqSkipGlissandi                      :: Bool
>   , qqReplacePerCent                     :: Double
>   , qqUsingPlayCache                     :: Bool
>   , qqDumpFftChunks                      :: Bool
>   , qqNumTakeFftChunks                   :: Int} deriving (Eq, Show)

Edit the following ====================================================================================================

> defC                   :: ControlSettings
> defC =
>   ControlSettings {
>     qqDiagnosticsEnabled                 = True
>   , qqSkipReporting                      = True
>   , qqSkipGlissandi                      = False
>   , qqReplacePerCent                     = 0
>   , qqUsingPlayCache                     = False
>   , qqDumpFftChunks                      = False
>   , qqNumTakeFftChunks                   = 3}

The End
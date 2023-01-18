Parthenopea
William Clements
December 12, 2022

> module Parthenopea where

> import Control.DeepSeq (NFData)
> import Data.Array
> import Debug.Trace
> import Euterpea
> import Euterpea.IO.MIDI.ToMidi2 (writeMidi2)
> import System.Random

Utilities =========================================================================

> diagnosticsEnabled = False
> traceIf :: String -> a -> a
> traceIf str expr = if diagnosticsEnabled
>                    then trace str expr
>                    else expr
> traceAlways :: String -> a -> a
> traceAlways str expr = trace str expr
> traceNever :: String -> a -> a
> traceNever str expr = expr
>   
> addDur       :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)
>
> dim :: Rational -> Music a -> Music a
> dim amt = phrase [Dyn (Diminuendo amt)]
>
> capture :: Music (Pitch, Volume) -> IO()
> capture m = writeMidi2 "Parthenopea.mid" m
>
> slur :: Rational -> Music a -> Music a
> slur rate = Modify (Phrase [Art (Slurred rate)])
>
> durS :: Rational -> Double
> durS r = 2 * fromRational r
> 
> rationalEpsilon :: Double
> rationalEpsilon = 0.0001
>
> rawPitches = [0..127]
>
> allPitches =
>    foldr (:+:) (rest 0) (map notize rawPitches)
>    where
>       notize aP = note qn $ pitch aP
>
> -- note chordFromArray has the same function body as chord itself
> chordFromArray :: Array Int (Music (Pitch, Volume))
>                   -> Music (Pitch, Volume)
> chordFromArray = foldr (:=:) (rest 0)
>
> snippet :: Music Pitch
> snippet = line [c 4 en, e 4 qn, bf 3 en, d 4 qn, f 4 en]

This alternate playback function will enable channel manipulation to allow
more than 16 instruments to be used in the source music.

> playDM :: (NFData a, ToMusic1 a) => Music a -> IO ()
> playDM = playC defParams{chanPolicy = dynamicCP 16 9, strict=True}

This alternate playback function will merge overlapping notes, 
which makes for a cleaner sound on some synthesizers:

> playX :: (NFData a, ToMusic1 a) => Music a -> IO ()
> playX = playC defParams{perfAlg = eventMerge . perform} where 
>     eventMerge :: Performance -> Performance
>     eventMerge (e1:e2:es) = 
>         let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
>         in  if ePitch e1 == ePitch e2 then eventMerge (e1':es) 
>             else e1 : eventMerge (e2:es)
>     eventMerge e = e

  "triads"

> triad :: PitchClass -> Mode -> Pitch -> Dur -> Music Pitch
> triad key mode base dur = chord [ n1, n2, n3] where
>   rkP = absPitch (key, (snd base)-1)
>   bP  = absPitch base
>   ocd = (bP - rkP) `div` 12
>   kP  = rkP + (12*ocd)
>   apD = bP - kP           -- guaranteed to be nonnegative
>   is  = formExact apD mode
>   n1, n2, n3 :: Music Pitch
>   n1 = note dur $ pitch bP
>   n2 = note dur $ pitch (bP + is!!0)
>   n3 = note dur $ pitch (bP + is!!1)
>   formExact :: AbsPitch -> Mode -> [AbsPitch]
>   formExact apDelta mode = offsets2intervals apDelta $ mode2offsets mode
>     where
>       major = [0, 4, 7, 12, 16]
>       minor = [0, 3, 7, 12, 15]
>       dim =   [0, 3, 6, 12, 15]
>       sus4 =  [0, 5, 7, 12, 17]
>       sus2 =  [0, 2, 7, 12, 14]
>       aug =   [0, 4, 8, 12, 16]
>       mode2offsets :: Mode -> [AbsPitch]
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
>       offsets2intervals :: AbsPitch -> [AbsPitch] -> [AbsPitch]
>       offsets2intervals apDelta os
>         | apDelta == (os!!0) = ((os!!1) - (os!!0)) : [((os!!2) - (os!!0))]
>         | apDelta == (os!!1) = ((os!!2) - (os!!1)) : [((os!!3) - (os!!1))]
>         | apDelta == (os!!2) = ((os!!3) - (os!!2)) : [((os!!4) - (os!!2))]
>         | otherwise          = error "Malformed Triad"
>
> data Lake = Lake { 
>                   cInst    :: InstrumentName, 
>                   cWch     :: Int, 
>                   cPch     :: AbsPitch,
>                   cVol     :: Volume,
>                   cKey     :: (PitchClass, Mode),
>                   cVel     :: Double}
>      deriving Show
>
>   -- calibrate (0,1) to (lo,up) e.g. (24,92)
> denorm :: Double -> (Double, Double) -> Double
> denorm r (lo, up) = lo + r * (up-lo)
>
> mkLake :: [Double] -> Lake
> mkLake rs =
>    Lake {  
>       cInst  = toEnum $ round   $ denorm (rs!!0) (0,instrumentLimit)
>       , cWch =  floor           $ denorm (rs!!1) (0,3.99999)
>       , cPch =  round           $ denorm (rs!!2) (24,92)
>       , cVol =  round           $ denorm (rs!!3) (80,120)
>       , cKey =  (toEnum $ round $ denorm (rs!!4) (0,12), Major)
>       , cVel =                    denorm (rs!!5) (1,3)}
>
> sextuplets :: StdGen -> [[Double]]
> sextuplets g =
>    let (z1, g') =      uniformR (0,1) g
>        (z2, g'') =     uniformR (0,1) g'
>        (z3, g''') =    uniformR (0,1) g''
>        (z4, g'''') =   uniformR (0,1) g'''
>        (z5, g''''') =  uniformR (0,1) g''''
>        (z6, g'''''') = uniformR (0,1) g'''''
>        sextuplet = z1:z2:z3:z4:z5:[z6]
>    in sextuplet : sextuplets g''''''
>
> instrumentLimit :: Double
> instrumentLimit = fromIntegral $ fromEnum Gunshot

re instrumentRange content, see:
   https://www.orchestralibrary.com/reftables/rang.html
also
   https://philharmonia.co.uk/resources/instruments/

> instrumentRange :: InstrumentName -> Maybe (Pitch, Pitch)
> instrumentRange inst =
>    case inst of
>       Piccolo                   -> Just (( D, 5), ( C, 8)) -- C piccolo
>       Flute                     -> Just (( C, 4), ( D, 7))
>       -- AltoFlute
>       Oboe                      -> Just ((Bf, 3), ( A, 6))
>       -- Oboe d'amore
>       EnglishHorn               -> Just (( E, 2), ( C, 6))
>       -- BassOboe
>       Clarinet                  -> Just (( D, 3), ( B, 6)) -- Bb clarinet
>       -- BassetHorn
>       -- BassClarinet
>       Bassoon                   -> Just ((Bf, 1), (Ef, 5))
>       -- ContraBassoon
>       SopranoSax                -> Just ((Af, 3), ( F, 6)) -- Bb soprano
>       AltoSax                   -> Just (( D, 3), ( B, 6)) -- Eb alto
>       TenorSax                  -> Just ((Af, 2), ( F, 5)) -- Bb tenor
>       BaritoneSax               -> Just (( D, 2), ( B, 5)) -- Eb baritone
>       -- BassSax
>
>       FrenchHorn                -> Just (( B, 1), ( F, 5)) -- F horn
>       -- WagnerTuben
>       Trumpet                   -> Just (( E, 3), ( C, 6)) -- Bb trumpet
>       -- PiccoloTrumpet
>       -- ExoticTrumpet
>       -- AltoTrombone
>       Trombone                  -> Just (( E, 2), ( F, 5))
>       -- BassTrombone
>       -- ContraBassTrombone
>       Tuba                      -> Just (( D, 1), ( F, 4))
>       -- Euphonium
>
>       Timpani                   -> Just (( D, 2), ( C, 4))
>       Xylophone                 -> Just (( F, 4), ( C, 8))
>       Marimba                   -> Just (( C, 2), ( C, 7))
>       Glockenspiel              -> Just (( G, 5), ( C, 8))
>       Vibraphone                -> Just (( F, 3), ( F, 6))
>       Percussion                -> Just wideOpen
>       -- Chimes
>       AcousticGuitarNylon       -> Just (( E, 2), ( E, 5))
>       AcousticGuitarSteel       -> Just (( E, 2), ( E, 5))
>       ElectricGuitarJazz        -> Just (( E, 2), ( E, 5))
>       ElectricGuitarClean       -> Just (( E, 2), ( E, 5))
>       ElectricGuitarMuted       -> Just (( E, 2), ( E, 5))
>       OverdrivenGuitar          -> Just (( E, 2), ( E, 5))
>       DistortionGuitar          -> Just (( E, 2), ( E, 5))
>       OrchestralHarp            -> Just (( C, 1), (Fs, 7))
>       AcousticBass              -> Just (( E, 1), ( C, 8))
>       ElectricBassFingered      -> Just (( E, 1), ( C, 8))
>       ElectricBassPicked        -> Just (( E, 1), ( C, 8))
>       -- FretlessBass
>       SlapBass1                 -> Just (( E, 1), ( C, 8))
>       SlapBass2                 -> Just (( E, 1), ( C, 8))
>       -- SynthBass1
>       -- SynthBass2
>
>       AcousticGrandPiano        -> Just (( A, 0), ( C, 8))
>       BrightAcousticPiano       -> Just (( A, 0), ( C, 8))
>       ElectricGrandPiano        -> Just (( A, 0), ( C, 8))
>       HonkyTonkPiano            -> Just (( A, 0), ( C, 8))
>       RhodesPiano               -> Just (( A, 0), ( C, 8))
>       ChorusedPiano             -> Just (( A, 0), ( C, 8))
>       Celesta                   -> Just (( C, 4), ( C, 8))
>       Harpsichord               -> Just (( F, 1), ( F, 6))
>       -- Harmonium
>       ChurchOrgan               -> Just (( C, 2), ( C, 7))
>
>       Violin                    -> Just (( G, 3), ( A, 7))
>       Viola                     -> Just (( C, 3), ( E, 6))
>       Cello                     -> Just (( C, 2), ( C, 6))
>       Contrabass                -> Just (( E, 1), ( G, 4)) -- alias Doublebass
>
>       Banjo                     -> Just (( C, 3), ( C, 6))
>       otherwise                 -> Nothing
>
> wideOpen :: (Pitch, Pitch)
> wideOpen = (pitch 0, pitch 127)
>
> withinRange :: (Pitch, Pitch) -> AbsPitch -> Pitch -> Bool
> withinRange range t p =
>    let aP = absPitch p + t
>        minP = absPitch $ fst range
>        maxP = absPitch $ snd range
>    in minP <= aP && maxP >= aP
>
> type OorCase = (InstrumentName, AbsPitch, Pitch)
>
> listOutOfRangeCases :: Music (Pitch, Volume) -> [OorCase]
> listOutOfRangeCases m =
>    let listOor :: InstrumentName
>                   -> AbsPitch
>                   -> Music (Pitch, Volume)
>                   -> [OorCase]
>        listOor inst t (Prim (Note _ p)) =
>           case instrumentRange inst of
>              Nothing    -> [(inst, t, fst p)]
>              Just range -> if withinRange range t (fst p)
>                            then []
>                            else [(inst, t, fst p)]              
>        listOor inst t (m1 :+: m2) =
>           concat [listOor inst t m1, listOor inst t m2]
>        listOor inst t (m1 :=: m2) =
>           concat [listOor inst t m1, listOor inst t m2]
>        listOor inst t (Modify (Instrument iname) m)  = listOor iname t m
>        listOor inst t (Modify (Transpose newt) m)    = listOor inst (t+newt) m
>        listOor inst t (Modify c m)                   = listOor inst t m
>        listOor _ _ _                                 = []
>
>    in listOor AcousticGrandPiano 0 m
>
> makeHistogram :: Int -> [Double] -> Array Int Int
> makeHistogram nDivs ts
>   | traceAlways msg False = undefined
>   | otherwise = hist (0, nDivs - 1) is
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
>      fact = fDivs / (hi - lo)
>      cv :: Double -> Int
>      cv d = floor $ (d - lo) * fact
>      fDivs :: Double
>      fDivs = fromIntegral nDivs
>
>      is :: [Int]
>      is = map cv ts
>
>      msg = unwords [ "makeHistogram lo, hi, fact="
>                    , show (safemin ts)
>                    , ":", show (safemax ts)
>                    , ":", show fact
>                    , " ts="
>                    , show (length ts)
>                    , " is="
>                    , show (length is)]
>
> hist            :: (Ix a, Integral b) => (a,a) -> [a] -> Array a b
> hist bnds is    =  accumArray (+) 0 bnds [(i, 1) | i <- is, inRange bnds i]
>
> defDivs :: Int
> defDivs = 32
>
> percussionLimit :: Double
> percussionLimit = fromIntegral $ fromEnum OpenTriangle

These snippets seem good to be used with "lake"

> pSnippet01 =
>   tempo (3/2) (line [ e 4 qn, e 4 en, e 4 qn, e 4 en])

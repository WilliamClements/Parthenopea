Baking
William Clements
December 16, 2022

> module Baking where

> import Data.Array
> import Data.List (sortBy)
> import Data.Ratio (approxRational)
> import Euterpea
> import FRP.UISF.UISF
> import MoreMusic
> import Parthenopea
> import Signals
> import System.Random

Bake ==============================================================================

>
> type Baking = (BakingMetrics, Array Int (Music (Pitch, Volume)))
>

The progress of the algorithm is expressed in above pair.

> -- from a list of contexts, assemble sections, and produce a
> -- music channel array (with length numChannels)
> consumeBakes :: [Bake] -> Array Int (Music (Pitch, Volume))
> consumeBakes bakes = if checkJobOk (bm, ms)
>                      then ms
>                      else error "consumeBakes: bad job"
>   where (bm, ms) = buildChannels bakes
>
> measureBakes :: [Bake] -> Array Int Int
> measureBakes bakes = if checkMeasureOk $ sHistogram bm
>                      then sHistogram bm
>                      else error "measureBakes: bad job"
>    where (bm, _) = buildChannels bakes
>
> sampleBakes :: [Bake] -> Array Int Int
> sampleBakes bakes = makeHistogram bakingDivs
>                     $ map extractOnset bakes
>    where
>       extractOnset :: Bake -> Double
>       extractOnset bake@Bake 
>                    { bOnset = bo } = bo
>
> bake :: Music (Pitch, Volume)
> bake = bake2 358016
>
> bake2 :: Int -> Music (Pitch, Volume)
> bake2 seed =
>    removeZeros
>    $ instrument Percussion
>    $ chordFromArray
>    $ consumeBakes
>    $ zipWith (\a b -> b{bIx = a}) [0..]
>    $ sortBy sortByOnset
>    $ map sex2Bake
>    $ take (round numSections)
>    $ sextuplets
>    $ mkStdGen seed
>
> bake3 :: Int -> Array Int Int
> bake3 seed =
>    measureBakes
>    $ zipWith (\a b -> b{bIx = a}) [0..]
>    $ sortBy sortByOnset
>    $ map sex2Bake
>    $ take (round numSections)
>    $ sextuplets
>    $ mkStdGen seed
>
> bake4 :: Int -> Array Int Int
> bake4 seed =
>    sampleBakes
>    $ map sex2Bake
>    $ take (round numSections)
>    $ sextuplets
>    $ mkStdGen seed
>
> buildChannels :: [Bake] -> Baking
> buildChannels bakes =
>    let
>       -- "Contexts" -urns- are in increasing onset time order. There is
>       -- an algorithm to mitigate "collisions", via skipping of
>       -- excessively "simultaneous" Bakes. Therefore, the -inns- list
>       -- is arranged in increasing end time order.
>       build :: [Bake] -> [Bake] -> Baking -> Baking
>       build [] [] baking             = baking
>       build (urn:urns) [] baking     = tryUrn urn urns [] baking
>       build [] (inn:inns) baking     = retireInn inn [] inns baking
>       build (urn:urns) (inn:inns) baking
>          | computeEnd (bOnset inn) (bDur inn) < (bOnset urn) =
>               retireInn inn (urn:urns) inns baking
>          | otherwise =
>               tryUrn urn urns (inn:inns) baking
>
>       retireInn, tryUrn, acceptUrn, skipUrn
>          :: Bake -> [Bake] -> [Bake] -> Baking -> Baking
>
>       retireInn _ urns inns baking = build urns inns baking
>
>       tryUrn urn urns inns baking
>          | (skipThreshold >= length inns) = acceptUrn urn urns inns baking
>          | otherwise                      = skipUrn   urn urns inns baking
>
>       acceptUrn urn urns inns baking =
>          let
>              inns' = sortBy sortByEnd (urn:inns)
>              baking' = acceptSection urn baking
>          in if checkUrnOk urn
>             then build urns inns' baking'
>             else error "acceptUrn: bad job"
>                 
>       skipUrn urn urns inns baking = build urns inns baking'
>          where baking' = skipSection urn baking
>               
>       skipSection :: Bake -> Baking -> Baking
>       skipSection urn@Bake 
>                    { bIx    = ix
>                    , bOnset = os} (bm, ms) = (bm', ms)
>          where bm' = skipMetrics bm os
>
>       acceptSection :: Bake -> Baking -> Baking
>       acceptSection urn@Bake 
>                      { bIx    = ix
>                      , bOnset = os
>                      , bDur   = du
>                      , bSnd   = sound
>                      , bVol   = vol
>                      , bVel   = vel} (bm, ms) = (bm', ms')
>          where
>
>          chan = ix `mod` numChannels
>
>          durSoFar :: Double
>          newDur   :: Double
>          ss       :: SectionSpec
>          newm     :: Music (Pitch, Volume)
>
>          -- algorithm gets an N squared component here
>          durSoFar = fromRational $ dur (ms!chan)
>          ss = calibrateSection durSoFar urn
>          newm = generateSection sound vol ss
>          ms' = appendFill ms chan newm
>          newDur = fromRational $ dur (ms'!chan)
>
>          -- statskeeping
>          bm' = acceptMetrics bm durSoFar newDur ss os
>                   
>       appendFill :: Array Int (Music (Pitch, Volume))
>                     -> Int
>                     -> Music (Pitch, Volume)
>                     -> Array Int (Music (Pitch, Volume))
>       appendFill ms chan newm = ms // [(chan,ms!chan :+: newm)]
>
>       calibrateSection :: Double -> Bake -> SectionSpec
>       calibrateSection
>          durSoFar
>          urn@Bake
>             {bIx     = ix
>             , bOnset = os
>             , bDur   = du
>             , bVel   = vel} =
>             let
>                 restDur, restTempo, fillDur, fillTempo  :: Double
>
>                 -- use rest to pass the time until our onset
>                 restDur = (os - durSoFar)
>                 restTempo = 1.0
>
>                 -- meat part of the section
>                 fillDur = du
>                 fillTempo = vel
>             in ((restDur, restTempo), (fillDur, fillTempo))
>
>       generateSection :: PercussionSound
>                          -> Volume
>                          -> SectionSpec
>                          -> Music (Pitch, Octave)
>       generateSection sound vol ss = theRest :+: theFill
>
>          where
>
>          restDur, restTempo, fillDur, fillTempo  :: Double
>          fillBeats :: Int                 
>
>          ((restDur, restTempo), (fillDur, fillTempo)) = ss
>          fillBeats = round (fillDur * fillTempo)
>
>          theRest, theFill :: Music (Pitch, Octave)
>
>          theRest =
>             rest
>             $ approx restDur
>          theFill =
>             addVolume vol
>             $ roll (approx fillDur)
>             $ perc sound 
>             $ approx (1.0/fillTempo)
>
>          approx :: Double -> Dur
>          approx dur = approxRational dur rationalEpsilon
>
>    in build
>       bakes
>       []
>       (newBakingMetrics,
>       array (0, numChannels - 1)
>             [(x, rest 0) | x <- [0..(numChannels - 1)]])
>
> sortByOnset :: Bake -> Bake -> Ordering
> sortByOnset lhs@Bake{bOnset = t1} rhs@Bake{bOnset = t2} =
>    compare t1 t2
>
> sortByEnd :: Bake -> Bake -> Ordering
> sortByEnd lhs@Bake{bOnset = t1, bDur = d1} rhs@Bake{bOnset = t2, bDur = d2} =
>    compare (computeEnd t1 d1) (computeEnd t2 d2)
>
> checkMeasureOk :: Array Int Int -> Bool
> checkMeasureOk arr
>    | traceIf msg False = undefined
>    | otherwise = True
>    where
>       msg = unwords ["\n\n checkMeasureOk arr =", show arr]
>
> checkJobOk :: Baking -> Bool
> checkJobOk (bm@BakingMetrics
>             { sBaked        = bakd
>             , sSkipped      = skpd
>             , sAccumBaked   = abkd
>             , sAccumSkipped = asps} , ms)
>    | traceIf msg False = undefined
>    | otherwise = True
>    where
>       msg = unwords
>              [ "checkJobOk: bm ="
>                , show bm
>                , "\n avg skipped start="
>                , show $ safeAverage asps skpd
>                , "avg baked start="
>                , show $ safeAverage abkd bakd 
>                , "durs="
>                , foldShowDur ms]
>       safeAverage :: Double -> Int -> Double
>       safeAverage d n
>          | n==0 = 0.0
>          | otherwise = d / (fromIntegral n)
>       foldShowDur :: Array Int (Music (Pitch, Volume)) -> String
>       foldShowDur ms = foldr foldFun "" ms
>          where
>             foldFun :: Music (Pitch, Volume) -> String -> String
>             foldFun m s = " " ++ (show d1) ++ s
>                where
>                   d1 :: Double
>                   d1 = fromRational (dur m)
>
> checkUrnOk :: Bake -> Bool
> checkUrnOk urn
>    | traceIf msg False = undefined
>    | otherwise = True
>    where
>       msg = unwords [ "checkUrnOk: urn = ", show urn ]
>
> acceptMetrics :: BakingMetrics
>                  -> Double
>                  -> Double
>                  -> SectionSpec
>                  -> Double
>                  -> BakingMetrics
> acceptMetrics bm@BakingMetrics
>                { sBaked      = bakd
>                , sRestDur    = rdur
>                , sFillDur    = fdur
>                , sAccumBaked = abkd
>                , sHistogram  = chst
>                , sDurs       = durs} durSoFar newDur ss os
>    | traceIf msg False = undefined
>    | otherwise =
>    let restDur, restTempo, fillDur, fillTempo  :: Double
>        ((restDur, restTempo), (fillDur, fillTempo)) = ss
>        baked'   = bakd           + 1
>        restDur' = rdur           + restDur
>        fillDur' = fdur           + fillDur
>        accum'   = abkd           + os
>        chst'    = chst           // quantize chst os fillDur
>        durs'    = durSoFar       : durs
>    in bm { sBaked      = baked'
>          , sRestDur    = restDur'
>          , sFillDur    = fillDur'
>          , sAccumBaked = accum'
>          , sHistogram  = chst'
>          , sDurs       = durs'}
>    where msg = unwords
>                 [ "acceptMetrics "
>                   , show durSoFar
>                   , " <- durSoFar, os -> "
>                   , show os 
>                   , "\n ss = "
>                   , show ss
>                   , "\n newDur="
>                   , show newDur] -- "\n durs="
>                                  -- , show durs
>
>          quantize :: Array Int Int -> Double -> Double -> [(Int,  Int)]
>          quantize chst os du = [(x, (chst!x) + 1) | x <- [fk..gk]]
>             where
>                fk,gk :: Int
>                fk = histSelect os
>                gk = histSelect $ (computeEnd os du) - 0.000001
>
>          histSelect :: Double -> Int
>          histSelect os = floor $ os * (fromIntegral bakingDivs) / songLength
>
> skipMetrics :: BakingMetrics -> Double -> BakingMetrics
> skipMetrics bm accum = 
>    let skipped'   = sSkipped bm      + 1
>        accum'     = sAccumSkipped bm + accum
>    in bm { sSkipped      = skipped'
>          , sAccumSkipped = accum'}
>
> sex2Bake :: [Double] -> Bake
> sex2Bake rs
>    | (length rs >= 6) = 
>       Bake {
>          bIx =                     0
>          , bWch =  floor           $ denorm (rs!!0) (0,4.99999)
>          , bOnset =                  denorm (rs!!1) (0,songLength)
>          , bDur =                    denorm (rs!!2) (0.5,1.3)
>          , bSnd =  toEnum $ round  $ denorm (rs!!3) (0,percussionLimit)
>          , bVol =  round           $ denorm (rs!!4) (50,110)
>          , bVel =                    denorm (rs!!5) (2,5)}
>    | otherwise = error "insufficiently sized list for sex2Bake" 
>
> computeEnd :: Double -> Double -> Double
> computeEnd onset dur = 
>    min songLength (onset + dur)
>
> data Bake = Bake {bIx      :: Int, 
>                   bWch     :: Int, 
>                   bOnset   :: Double, 
>                   bDur     :: Double,
>                   bSnd     :: PercussionSound,
>                   bVol     :: Volume,
>                   bVel     :: Double}
>    deriving (Show, Eq, Ord)
>
> type TimeSpec = (Double, Double)
> type SectionSpec = (TimeSpec, TimeSpec)
>
> data BakingMetrics =
>   BakingMetrics {sSkipped         :: Int
>                  , sBaked         :: Int
>                  , sRestDur       :: Double
>                  , sFillDur       :: Double
>                  , sAccumBaked    :: Double
>                  , sAccumSkipped  :: Double
>                  , sHistogram     :: Array Int Int
>                  , sDurs          :: [Double]}
>      deriving (Show, Eq, Ord)
>
> newBakingMetrics = BakingMetrics
>                    {sSkipped   = 0
>                     , sBaked   = 0
>                     , sRestDur = 0
>                     , sFillDur = 0
>                     , sAccumBaked = 0
>                     , sAccumSkipped = 0
>                     , sHistogram =
>                          array (0, eb) [(x, 0) | x <- [0..eb]]
>                     , sDurs = []}
>   where eb = bakingDivs - 1
>
> songLength, numSections :: Double
> songLength = 24.0      -- 48 seconds
> numSections = 250.0
>
> numChannels, skipThreshold, bakingDivs :: Int
> numChannels = 7
> skipThreshold = 16
> bakingDivs = 32

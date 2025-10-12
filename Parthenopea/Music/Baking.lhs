> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Baking
William Clements
December 16, 2022

> module Parthenopea.Music.Baking (
>                  bakedJingle
>                , measureBakes
>                , sampleBakes
>                , bake4Measuring
>                , bake4Sampling
>                , bakeOps
>                , checkMeasureOk) where
>
> import Data.Array.Unboxed
> import Data.List ( sortOn )
> import Debug.Trace
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.SoundFont.Scoring ( scoreOnsets )
> import Parthenopea.SoundFont.SFSpec ( allKinds )
> import Parthenopea.SoundFont.Utility
> import System.Random ( mkStdGen )
  
Bake ==================================================================================================================

> type Baking = (BakingMetrics, Array Int Music1)

The progress of the algorithm is expressed in above pair.

> -- from a list of contexts, assemble sections, and produce a
> -- music "channel" array (with size numChannels)
> consumeBakes           :: [Bake] → Array Int Music1
> consumeBakes bakes                       = profess
>                                              (checkJobOk bm)
>                                              "consumeBakes checkJobOk"
>                                              ms
>   where
>     (bm, ms)                             = buildChannels bakes
>
> measureBakes           :: [Bake] → Array Int Int
> measureBakes bakes                       = profess
>                                              (checkJobOk bm && checkMeasureOk bm)
>                                              "measureBakes checkJobOk"
>                                              bm.sHistogram
>   where
>     (bm, _)                              = buildChannels bakes
>
> sampleBakes            :: [Bake] → Array Int Int
> sampleBakes bakes                        = scoreOnsets bakingBins (map bOnset bakes)
>
> bakedJingle            :: Int → Music1
> bakedJingle seed                         = removeZeros
>                                            $ instrument Percussion
>                                            $ chordFromArray
>                                            $ bake4Consumption seed
>
> bake4Consumption       :: Int → Array Int Music1
> bake4Consumption seed                    = consumeBakes
>                                            $ zipWith (\x y → y{bIx = x}) [0..]
>                                            $ sortOn bOnset
>                                            $ bakeUtility seed
>
> bake4Measuring         :: Int → Array Int Int
> bake4Measuring seed                      = measureBakes
>                                            $ zipWith (\x y → y{bIx = x}) [0..]
>                                            $ sortOn bOnset
>                                            $ bakeUtility seed
>
> bake4Sampling          :: Int → Array Int Int
> bake4Sampling seed                       = sampleBakes
>                                            $ bakeUtility seed
>
> bakeOps                :: IO ()
> bakeOps                                  = do
>   let vm                                 = bake4Consumption 100
>   let vi                                 = bake4Measuring 100
>   traceIO $ show vm
>   traceIO $ show vi
>   return ()
>
> bakeUtility            :: Int → [Bake]
> bakeUtility seed                         = map rs2Bake
>                                            $ take (round numSections)
>                                            $ randomNormSets 7
>                                            $ mkStdGen seed
>
> buildChannels          :: [Bake] → Baking
> buildChannels bakes                      = build bakes [] (newBakingMetrics, newMusic)
>   where
>     -- "Bakes" -urns- are in increasing onset time order. There is
>     -- an algorithm to mitigate "collisions" by skipping
>     -- excessively "simultaneous" Bakes. Therefore, the -inns- list
>     -- is arranged in increasing end time order.
>     build              :: [Bake] → [Bake] → Baking → Baking
>     build [] [] baking                   = baking
>     build (urn:urns) [] baking           = tryUrn urn urns [] baking
>     build [] (inn:inns) baking           = retireInn inn [] inns baking
>     build (urn:urns) (inn:inns) baking
>       | computeEnd inn.bOnset 1.0 < urn.bOnset
>                                          = retireInn inn (urn:urns) inns baking
>       | otherwise                        = tryUrn urn urns (inn:inns) baking
>
>     retireInn, tryUrn, acceptUrn, skipUrn
>                        -- target  urns     inns      old      new
>                        :: Bake → [Bake] → [Bake] → Baking → Baking
>
>     retireInn _                          = build
>
>     tryUrn urn urns inns baking
>       | skipThreshold >= length inns     = acceptUrn urn urns inns baking
>       | otherwise                        = skipUrn   urn urns inns baking
>
>     acceptUrn urn urns inns baking       =
>       profess
>         (checkUrnOk urn)
>         (unwords [fName, "checkUrnOk"])
>         (build urns (sortOn bOnset (urn:inns)) (acceptSection urn baking))
>       where
>         fName                            = "acceptUrn"
>                 
>     skipUrn urn urns inns baking         = build urns inns (skipSection urn baking)
>               
>     skipSection        :: Bake → Baking → Baking
>     skipSection urn (bm, ms)             = (skipMetrics bm (bOnset urn), ms)
>
>     acceptSection      :: Bake → Baking → Baking
>     acceptSection urn (bm, ms)
>                                          = (bm', ms')
>       where
>         chan                             = urn.bIx `mod` numChannels
>         inst                             = fst urn.bWch
>         rng                              = snd urn.bWch
>
>         durSoFar       :: Double
>         ss             :: SectionSpec
>         newm           :: Music1
>
>         -- algorithm gets an N squared component here
>         durSoFar                         = fromRational $ dur (ms ! chan)
>         ss                               = calibrateSection durSoFar urn
>         newm                             =
>           generateSection urn.bSnd urn.bVol ss inst rng urn.bXpose urn.bSweep
>         ms'                              = appendSection ms chan newm
>
>         -- statskeeping
>         bm'                              = acceptMetrics bm ss urn.bOnset
>                   
>     appendSection      :: Array Int Music1
>                           → Int
>                           → Music1
>                           → Array Int Music1
>     appendSection ms chan newm           = ms // [(chan, ms!chan :+: newm)]
>
>     calibrateSection   :: Double → Bake → SectionSpec
>     calibrateSection durSoFar urn        = ((restDur, restTempo), (fillDur, fillTempo))
>       where
>         restDur, restTempo, fillDur, fillTempo
>                        :: Double
>
>         -- use rest to pass the time until our onset
>         restDur                          = urn.bOnset - durSoFar
>         restTempo                        = 1.0
>
>         -- meat part of the section
>         fillDur                          = 1.0
>         fillTempo                        = urn.bTempo
>
>     generateSection    :: PercussionSound
>                           → Volume
>                           → SectionSpec
>                           → InstrumentName
>                           → (AbsPitch, AbsPitch)
>                           → Double
>                           → Double
>                           → Music1
>     generateSection sound vol ss inst rng dXp sweep
>                                          = theRest :+: (thePercFill :=: theInstFill)
>       where
>         restDur, fillDur, fillTempo
>                        :: Double
>         fillBeats      :: Int                 
>
>         ((restDur, _), (fillDur, fillTempo))
>                                          = ss
>         fillBeats                        = round (fillDur * fillTempo * 3.0 / 4.0)
>
>         theRest, thePercFill, theInstFill
>                        :: Music1
>         theRest                          = rest $ approx restDur
>         thePercFill                      =
>           makePercFill sound (approx (0.5/fillTempo)) (fractionOf vol 1.25) (approx fillDur)
>         theInstFill                      =
>           instrument inst $ makeInstFill fillBeats rng dXp sweep (fractionOf vol 0.60)
>
>         fractionOf     :: Int → Double → Int
>         fractionOf x doub                = min 127 $ round $ doub * fromIntegral x
>
>
> makePercFill           :: PercussionSound → Rational → Volume → Dur → Music1
> makePercFill sound beats vol durP        = bandPart (makeNonPitched vol) m
>   where
>     m                                    =
>       if skipGlissandi
>         then rest durP
>         else roll durP $ perc sound beats
>
> makeInstFill           :: Int → (AbsPitch, AbsPitch) → Double → Double → Volume → Music1
> makeInstFill beats (lo, hi) dXp bend vol
>                                          = note (qn * toRational beats) (p, [Volume vol, Params [bend]])
>   where
>     p                  :: Pitch          = pitch $ lo + round (dXp * fromIntegral (hi - lo))
>
> checkMeasureOk         :: BakingMetrics → Bool
> checkMeasureOk bm
>   | traceNot trace_CMO False             = undefined
>   | otherwise                            = True
>   where
>     trace_CMO                            = unwords ["\ncheckMeasureOk", show bm.sHistogram]
>
> checkJobOk             :: BakingMetrics → Bool
> checkJobOk bm
>   | traceNot trace_CBO False             = undefined
>   | otherwise                            = True
>   where
>     trace_CBO                            =
>       unwords
>         [ "checkJobOk",                  show $ safeAverage bm.sAccumSkipped bm.sSkipped
>         , "avg baked start",             show $ safeAverage bm.sAccumBaked bm.sBaked]
>
>     safeAverage        :: Double → Int → Double
>     safeAverage dd n
>       | n == 0                           = 0.0
>       | otherwise                        = dd / fromIntegral n
>
> checkUrnOk             :: Bake → Bool
> checkUrnOk urn
>   | traceNot trace_CUO False             = undefined
>   | otherwise                            = True
>   where
>     trace_CUO                            = unwords ["checkUrnOk", show urn]
>
> checkZListOk           :: [(Int,  Int)] → Bool
> checkZListOk zlist
>   | traceNot trace_CZLO False            = undefined
>   | otherwise                            = True
>   where
>     trace_CZLO                           = unwords ["checkZListOK", show zlist]
>
> acceptMetrics          :: BakingMetrics → SectionSpec → Double → BakingMetrics
> acceptMetrics bm ss os                   = bm { sBaked      = baked'
>                                               , sRestDur    = restDur'
>                                               , sFillDur    = fillDur'
>                                               , sAccumBaked = accum'
>                                               , sHistogram  = hst'}
>   where
>     restDur, fillDur   :: Double
>     ((restDur, _), (fillDur, _))         = ss
>     baked'                               = bm.sBaked        + 1
>     restDur'                             = bm.sRestDur      + restDur
>     fillDur'                             = bm.sFillDur      + fillDur
>     accum'                               = bm.sAccumBaked   + os
>     zlist                                = quantize bm.sHistogram os fillDur
>     hst'                                 = if not (checkZListOk zlist)
>                                              then error "Bad ZList"
>                                              else bm.sHistogram // zlist
>
>     quantize           :: Array Int Int → Double → Double → [(Int,  Int)]
>     quantize hst o du                    = [(x, (hst!x) + 1) | x ← [fk..gk]]
>       where
>         fk, gk         :: Int
>         fk                               = histSelect o
>         gk                               = histSelect $ computeEnd o du - 0.000001
>
>     histSelect         :: Double → Int
>     histSelect o                         = floor $ o * fromIntegral bakingBins / songLength
>
> skipMetrics            :: BakingMetrics → Double → BakingMetrics
> skipMetrics bm accm                      = bm { sSkipped      = bm.sSkipped      + 1
>                                               , sAccumSkipped = bm.sAccumSkipped + accm}
>
> rs2Bake                :: [Double] → Bake
> rs2Bake rs
>   | length rs >= 7                       = Bake {
>                                              bIx       = 0
>                                            , bWch      = denormVector     (head rs)     vChoiceI
>                                            , bOnset    = denorm           (rs!!1)       (0, songLength)
>                                            , bXpose    = denorm           (rs!!2)       (0.2, 0.8)
>                                            , bSnd      = denormVector     (rs!!3)       vChoiceP
>                                            , bVol      = round $ denorm   (rs!!4)       (60, 100)
>                                            , bTempo    = denorm           (rs!!5)       (2, 5)
>                                            , bSweep    = denorm           (rs!!6)       (-6, 6)}
>   | otherwise                            = error $ unwords ["rs2Bake:", "insufficiently sized randoms list", show $ length rs]
>     where
>       vChoiceI         :: Array Int (InstrumentName, (AbsPitch, AbsPitch))
>       vChoiceI                           = selectRanged choices
>       vChoiceP         :: Array Int PercussionSound
>       vChoiceP                           = listArray (0, length curatedP - 1) curatedP
>
>       curatedP                           = filter (not . flip elem blacklist) (snd allKinds)
>
>       choices = [  Piccolo,         Oboe,      AltoSax,             Trumpet
>                  , Trombone,        Marimba,   Vibraphone,          ElectricGuitarJazz
>                  , OrchestralHarp,  SlapBass1, RhodesPiano,         Harpsichord
>                  , HammondOrgan,    Violin,    Contrabass]
>       blacklist = [MuteCuica, OpenCuica, LongWhistle, ShortWhistle]
>
> computeEnd             :: Double → Double → Double
> computeEnd onset durE                    = min songLength (onset + durE)
  
Definitions ===========================================================================================================

> data Bake                                =
>   Bake {
>       bIx              :: Int
>     , bWch             :: (InstrumentName, (AbsPitch, AbsPitch))
>     , bOnset           :: Double
>     , bXpose           :: Double
>     , bSnd             :: PercussionSound
>     , bVol             :: Volume
>     , bTempo           :: Double
>     , bSweep           :: Double} deriving (Show, Eq, Ord)
>
> type TimeSpec                            = (Double, Double)
> type SectionSpec                         = (TimeSpec, TimeSpec)
>
> data BakingMetrics                       =
>   BakingMetrics {
>       sSkipped         :: Int
>     , sBaked           :: Int
>     , sRestDur         :: Double
>     , sFillDur         :: Double
>     , sAccumBaked      :: Double
>     , sAccumSkipped    :: Double
>     , sHistogram       :: Array Int Int} deriving (Show, Eq, Ord)
>
> newBakingMetrics       :: BakingMetrics
> newBakingMetrics                         = BakingMetrics 0 0 0 0 0 0 (array (0, eb) [(x, 0) | x ← [0..eb]])
>   where
>     eb                                   = bakingBins - 1
> newMusic               :: Array Int Music1
> newMusic                                 = array (0, numChannels - 1) [(x, rest 0) | x ← [0..(numChannels - 1)]]
>
> songLength, numSections
>                        :: Double
> songLength                               = 32.0      -- 64 seconds
> numSections                              = 512.0
>
> numChannels, skipThreshold, bakingBins
>                        :: Int
> numChannels                              = 7
> skipThreshold                            = 16
> bakingBins                               = 64

The End
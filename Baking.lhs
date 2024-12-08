> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Baking
William Clements
December 16, 2022

> module Baking (bakedJingle) where
>
> import Data.Array.Unboxed
> import Data.List ( sortOn )
> import Data.Maybe ( isJust )
> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import HSoM.Examples.MoreMusic ( roll )
> import Parthenopea
> import Scoring
> import System.Random ( mkStdGen )
  
Bake ==================================================================================================================

> type Baking = (BakingMetrics, Array Int (Music (Pitch, [NoteAttribute])))

The progress of the algorithm is expressed in above pair.

> -- from a list of contexts, assemble sections, and produce a
> -- music "channel" array (with size numChannels)
> consumeBakes           :: [Bake] → Array Int (Music (Pitch, [NoteAttribute]))
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
>                                              sHistogram
>   where
>     (bm@BakingMetrics{ .. }, _)          = buildChannels bakes
>
> sampleBakes            :: [Bake] → Array Int Int
> sampleBakes bakes                        = scoreOnsets bakingBins (map bOnset bakes)
>
> bakedJingle            :: Int → Music (Pitch, [NoteAttribute])
> bakedJingle seed                         = removeZeros
>                                            $ instrument Percussion
>                                            $ chordFromArray
>                                            $ bake4Consumption seed
>
> bake4Consumption       :: Int → Array Int (Music (Pitch, [NoteAttribute]))
> bake4Consumption seed                    = consumeBakes
>                                            $ zipWith (\a b → b{bIx = a}) [0..]
>                                            $ sortOn bOnset
>                                            $ bakeUtility seed
>
> bake4Measuring         :: Int → Array Int Int
> bake4Measuring seed                      = measureBakes
>                                            $ zipWith (\a b → b{bIx = a}) [0..]
>                                            $ sortOn bOnset
>                                            $ bakeUtility seed
>
> bake4Sampling          :: Int → Array Int Int
> bake4Sampling seed                       = sampleBakes
>                                            $ bakeUtility seed
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
>     acceptSection urn@Bake{ .. } (bm, ms)
>                                          = (bm', ms')
>       where
>         chan                             = bIx `mod` numChannels
>         inst                             = fst bWch
>         range                            = snd bWch
>
>         durSoFar       :: Double
>         newDur         :: Double
>         ss             :: SectionSpec
>         newm           :: Music (Pitch, [NoteAttribute])
>
>         -- algorithm gets an N squared component here
>         durSoFar                         = fromRational $ dur (ms ! chan)
>         ss                               = calibrateSection durSoFar urn
>         newm                             =
>           generateSection bSnd bVol ss inst range bXpose bSweep
>         ms'                              = appendSection ms chan newm
>         newDur                           = fromRational $ dur (ms' ! chan)
>
>         -- statskeeping
>         bm'                              = acceptMetrics bm durSoFar newDur ss bOnset
>                   
>     appendSection      :: Array Int (Music (Pitch, [NoteAttribute]))
>                           → Int
>                           → Music (Pitch, [NoteAttribute])
>                           → Array Int (Music (Pitch, [NoteAttribute]))
>     appendSection ms chan newm           = ms // [(chan, ms!chan :+: newm)]
>
>     calibrateSection   :: Double → Bake → SectionSpec
>     calibrateSection durSoFar urn@Bake{ .. }
>                                          = ((restDur, restTempo), (fillDur, fillTempo))
>       where
>         restDur, restTempo, fillDur, fillTempo
>                        :: Double
>
>         -- use rest to pass the time until our onset
>         restDur                          = bOnset - durSoFar
>         restTempo                        = 1.0
>
>         -- meat part of the section
>         fillDur                          = 1.0
>         fillTempo                        = bTempo
>
>     generateSection    :: PercussionSound
>                           → Volume
>                           → SectionSpec
>                           → InstrumentName
>                           → (AbsPitch, AbsPitch)
>                           → Double
>                           → Double
>                           → Music (Pitch, [NoteAttribute])
>     generateSection sound vol ss inst range dXp sweep
>                                          = theRest :+: (thePercFill :=: theInstFill)
>       where
>         restDur, restTempo, fillDur, fillTempo
>                        :: Double
>         fillBeats      :: Int                 
>
>         ((restDur, restTempo), (fillDur, fillTempo))
>                                          = ss
>         fillBeats                        = round (fillDur * fillTempo * 3.0 / 4.0)
>
>         theRest, thePercFill, theInstFill
>                        :: Music (Pitch, [NoteAttribute])
>         theRest                          = rest $ approx restDur
>         thePercFill                      =
>           makePercFill sound (approx (0.5/fillTempo)) (fractionOf vol 1.25) (approx fillDur)
>         theInstFill                      =
>           instrument inst $ makeInstFill fillBeats range dXp sweep (fractionOf vol 0.60) 
>         ratDur         :: Rational       = approx fillDur
>
> makePercFill           :: PercussionSound → Rational → Volume → Dur → Music (Pitch, [NoteAttribute])
> makePercFill sound beats vol dur         = bandPart (makeNonPitched vol) m
>   where
>     m                                    =
>       if skipGlissandi
>         then rest dur
>         else roll dur $ perc sound beats
>
> makeInstFill           :: Int → (AbsPitch, AbsPitch) → Double → Double → Volume → Music (Pitch, [NoteAttribute])
> makeInstFill beats (lo, hi) dXp bend vol
>                                          = note (qn * toRational beats) (p, [Volume vol, Params [bend]])
>   where
>     p                  :: Pitch          = pitch $ lo + round (dXp * fromIntegral (hi - lo))
>
> checkMeasureOk         :: BakingMetrics → Bool
> checkMeasureOk BakingMetrics{ .. }
>   | traceNot trace_CMO False             = undefined
>   | otherwise                            = True
>   where
>     trace_CMO                            = unwords ["\ncheckMeasureOk", show sHistogram]
>
> checkJobOk             :: BakingMetrics → Bool
> checkJobOk BakingMetrics{ .. }
>   | traceNot trace_CBO False             = undefined
>   | otherwise                            = True
>   where
>     trace_CBO                            =
>       unwords
>         [ "checkJobOk",                  show $ safeAverage sAccumSkipped sSkipped
>         , "avg baked start",             show $ safeAverage sAccumBaked sBaked]
>
>     safeAverage        :: Double → Int → Double
>     safeAverage d n
>       | n == 0                           = 0.0
>       | otherwise                        = d / fromIntegral n
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
> acceptMetrics          :: BakingMetrics → Double → Double → SectionSpec → Double → BakingMetrics
> acceptMetrics bm@BakingMetrics{ .. } durSoFar newDur ss os
>   | traceNever trace_AM False            = undefined
>   | otherwise                            = bm { sBaked      = baked'
>                                               , sRestDur    = restDur'
>                                               , sFillDur    = fillDur'
>                                               , sAccumBaked = accum'
>                                               , sHistogram  = hst'}
>   where
>     restDur, fillDur   :: Double
>     ((restDur, _), (fillDur, _))         = ss
>     baked'                               = sBaked           + 1
>     restDur'                             = sRestDur         + restDur
>     fillDur'                             = sFillDur         + fillDur
>     accum'                               = sAccumBaked      + os
>     zlist                                = quantize sHistogram os fillDur
>     hst'                                 = if not (checkZListOk zlist)
>                                              then error "Bad ZList"
>                                              else sHistogram // zlist
>
>     trace_AM                             =                   
>       unwords ["acceptMetrics", show durSoFar, "← durSoFar, os →", show os
>              , "\nss", show ss, "\nnewDur", show newDur]
>
>     quantize           :: Array Int Int → Double → Double → [(Int,  Int)]
>     quantize hst os du                   = [(x, (hst!x) + 1) | x ← [fk..gk]]
>       where
>         fk, gk         :: Int
>         fk                               = histSelect os
>         gk                               = histSelect $ computeEnd os du - 0.000001
>
>     histSelect         :: Double → Int
>     histSelect os                        = floor $ os * fromIntegral bakingBins / songLength
>
> skipMetrics            :: BakingMetrics → Double → BakingMetrics
> skipMetrics bm@BakingMetrics{ .. } accum = bm { sSkipped      = sSkipped      + 1
>                                               , sAccumSkipped = sAccumSkipped + accum}
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
> computeEnd onset dur                     = min songLength (onset + dur)
  
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
> newBakingMetrics                         = BakingMetrics 0 0 0 0 0 0 (array (0, eb) [(x, 0) | x ← [0..eb]])
>   where
>     eb                                   = bakingBins - 1
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
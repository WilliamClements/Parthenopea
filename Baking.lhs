> {-# HLINT ignore "Eta reduce" #-}
> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

Baking
William Clements
December 16, 2022

> module Baking where

> import Data.Array ( Array, (!), (//), array, listArray, bounds )
> import Data.List ( sortOn )
> import Data.Maybe ( isJust )
> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import HSoM.Examples.MoreMusic ( roll )
> import Parthenopea
> import System.Random ( mkStdGen )
  
Bake ==================================================================================================================

>
> type Baking = (BakingMetrics, Array Int (Music (Pitch, Volume)))
>

The progress of the algorithm is expressed in above pair.

> -- from a list of contexts, assemble sections, and produce a
> -- music "channel" array (with size numChannels)
> consumeBakes           :: [Bake] → Array Int (Music (Pitch, [NoteAttribute]))
> consumeBakes bakes                       = profess
>                                              (checkJobOk (bm, ms))
>                                              "consumeBakes checkJobOk"
>                                              (aggrandize <$> ms)
>   where
>     (bm, ms)                             = buildChannels bakes
>
> measureBakes           :: [Bake] → Array Int Int
> measureBakes bakes                       = profess
>                                              (checkJobOk (bm, ms) && checkMeasureOk (bm, ms))
>                                              "measureBakes checkJobOk"
>                                              (sHistogram bm)
>   where
>     (bm, ms)                             = buildChannels bakes
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
> bakeUtility seed                         = map sex2Bake
>                                            $ take (round numSections)
>                                            $ sextuplets
>                                            $ mkStdGen seed
>
> buildChannels          :: [Bake] → Baking
> buildChannels bakes                      = build bakes [] (newBakingMetrics, newMusic)
>   where
>     -- "Contexts" -urns- are in increasing onset time order. There is
>     -- an algorithm to mitigate "collisions", via skipping of
>     -- excessively "simultaneous" Bakes. Therefore, the -inns- list
>     -- is arranged in increasing end time order.
>     build              :: [Bake] → [Bake] → Baking → Baking
>     build [] [] baking                   = baking
>     build (urn:urns) [] baking           = tryUrn urn urns [] baking
>     build [] (inn:inns) baking           = retireInn inn [] inns baking
>     build (urn:urns) (inn:inns) baking
>       | computeEnd (bOnset inn) 1.0 < bOnset urn
>                                          = retireInn inn (urn:urns) inns baking
>       | otherwise                        = tryUrn urn urns (inn:inns) baking
>
>     retireInn, tryUrn, acceptUrn, skipUrn
>                        -- target  urns     inns      old      new
>                        :: Bake → [Bake] → [Bake] → Baking → Baking
>
>     retireInn _ urns inns baking         = build urns inns baking
>
>     tryUrn urn urns inns baking
>       | skipThreshold >= length inns     = acceptUrn urn urns inns baking
>       | otherwise                        = skipUrn   urn urns inns baking
>
>     acceptUrn urn urns inns baking       = profess
>                                              (checkUrnOk urn)
>                                              "acceptUrn checkUrnOk"
>                                              (build urns inns' baking')
>       where
>         inns'                            = sortOn bOnset (urn:inns)
>         baking'                          = acceptSection urn baking
>                 
>     skipUrn urn urns inns baking         = build urns inns (skipSection urn baking)
>               
>     skipSection        :: Bake → Baking → Baking
>     skipSection urn (bm, ms)             = (skipMetrics bm (bOnset urn), ms)
>
>     acceptSection      :: Bake → Baking → Baking
>     acceptSection urn@Bake{bIx, bWch, bOnset, bXpose, bSnd, bVol} (bm, ms)
>                                          = (bm', ms')
>       where
>         chan                             = bIx `mod` numChannels
>         inst                             = fst bWch
>         range                            = snd bWch
>
>         durSoFar       :: Double
>         newDur         :: Double
>         ss             :: SectionSpec
>         newm           :: Music (Pitch, Volume)
>
>         -- algorithm gets an N squared component here
>         durSoFar                         = fromRational $ dur (ms!chan)
>         ss                               = calibrateSection durSoFar urn
>         newm                             = generateSection bSnd bVol ss inst range bXpose
>         ms'                              = appendSection ms chan newm
>         newDur                           = fromRational $ dur (ms'!chan)
>
>         -- statskeeping
>         bm'                              = acceptMetrics bm durSoFar newDur ss bOnset
>                   
>     appendSection      :: Array Int (Music (Pitch, Volume))
>                           → Int
>                           → Music (Pitch, Volume)
>                           → Array Int (Music (Pitch, Volume))
>     appendSection ms chan newm           = ms // [(chan, ms!chan :+: newm)]
>
>     calibrateSection   :: Double → Bake → SectionSpec
>     calibrateSection durSoFar urn@Bake{bOnset, bTempo}
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
>     generateSection
>                        :: PercussionSound
>                           → Volume
>                           → SectionSpec
>                           → InstrumentName
>                           → (Pitch, Pitch)
>                           → Double
>                           → Music (Pitch, Volume)
>     generateSection sound vol ss inst range dXp
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
>                        :: Music (Pitch, Volume)
>         theRest                        = rest $ approx restDur
>         thePercFill                    = addVolume (fractionOf vol 1.25)
>                                          $ roll (approx fillDur)
>                                          $ perc sound 
>                                          $ approx (0.5/fillTempo)
>         theInstFill                    = addVolume (fractionOf vol 0.60)
>                                          $ instrument inst
>                                          $ makeInstFill fillBeats range dXp
>
> makeInstFill           :: Int → (Pitch, Pitch) → Double → Music Pitch
> makeInstFill nBeats (lo, hi) dXp         = note dur p
>   where
>     dur                :: Rational
>     p                  :: Pitch
>     dLo, dHi           :: Double
>
>     dur                                  = qn * toRational nBeats
>
>     dLo                                  = fromIntegral (absPitch lo)
>     dHi                                  = fromIntegral (absPitch hi)
>     p                                    = pitch $ round $ dLo + (dXp * (dHi - dLo))
>
> checkMeasureOk :: Baking → Bool
> checkMeasureOk (bm, ms)
>   | traceAlways msg False                = undefined
>   | otherwise                            = True
>   where
>     msg = unwords ["\n checkMeasureOk bins =", show (sHistogram bm)]
>
> checkJobOk             :: Baking → Bool
> checkJobOk (bm@BakingMetrics{sBaked, sSkipped, sAccumBaked, sAccumSkipped}, ms)
>    | traceIf msg False                   = undefined
>    | otherwise                           = True
>    where
>      msg = unwords
>             [ "checkJobOk: bm =",        show bm
>               , "\n avg skipped start=", show $ safeAverage sAccumSkipped sSkipped
>               , "avg baked start=",      show $ safeAverage sAccumBaked sBaked 
>               , "durs=",                 foldShowDur ms]
>
>      safeAverage      :: Double → Int → Double
>      safeAverage d n
>         | n==0                           = 0.0
>         | otherwise                      = d / fromIntegral n
>
>      foldShowDur       :: Array Int (Music (Pitch, Volume)) → String
>      foldShowDur                         = foldr foldFun ""
>        where
>          foldFun       :: Music (Pitch, Volume) → String → String
>          foldFun m s                     = " " ++ show d1 ++ s
>            where
>              d1        :: Double
>              d1                          = fromRational (dur m)
>
> checkUrnOk :: Bake → Bool
> checkUrnOk urn
>    | traceIf msg False = undefined
>    | otherwise = True
>    where
>       msg = unwords [ "checkUrnOk: urn = ", show urn ]
> checkZListOk :: [(Int,  Int)] → Bool
> checkZListOk zlist
>    | traceAlways msg False = undefined
>    | otherwise = True
>    where
>       msg = unwords [ "checkZListOK: zlist = ", show zlist ]
>
> acceptMetrics          :: BakingMetrics → Double → Double → SectionSpec → Double → BakingMetrics
> acceptMetrics bm@BakingMetrics{sBaked, sRestDur, sFillDur, sAccumBaked, sHistogram} durSoFar newDur ss os
>    | traceNever msg False                = undefined
>    | otherwise                           = bm { sBaked      = baked'
>                                               , sRestDur    = restDur'
>                                               , sFillDur    = fillDur'
>                                               , sAccumBaked = accum'
>                                               , sHistogram  = hst'}
>    where
>      restDur, fillDur  :: Double
>      ((restDur, _), (fillDur, _))        = ss
>      baked'                              = sBaked           + 1
>      restDur'                            = sRestDur         + restDur
>      fillDur'                            = sFillDur         + fillDur
>      accum'                              = sAccumBaked      + os
>      zlist                               = quantize sHistogram os fillDur
>      hst'                                = if not (checkZListOk zlist)
>                                              then error "Bad ZList"
>                                              else sHistogram // zlist
>
>      msg                                 = unwords [  "acceptMetrics ", show durSoFar, " ← durSoFar, os → ", show os
>                                                     , "\n ss = ", show ss
>                                                     , "\n newDur=", show newDur]
>
>      quantize          :: Array Int Int → Double → Double → [(Int,  Int)]
>      quantize hst os du                  = [(x, (hst!x) + 1) | x ← [fk..gk]]
>        where
>          fk, gk        :: Int
>          fk                              = histSelect os
>          gk                              = histSelect $ computeEnd os du - 0.000001
>
>      histSelect        :: Double → Int
>      histSelect os                       = floor $ os * fromIntegral bakingBins / songLength
>
> skipMetrics            :: BakingMetrics → Double → BakingMetrics
> skipMetrics bm accum                     = bm { sSkipped      = skipped'
>                                               , sAccumSkipped = accum'}
>   where
>     skipped'                             = sSkipped bm      + 1
>     accum'                               = sAccumSkipped bm + accum
>
> sex2Bake               :: [Double] → Bake
> sex2Bake rs
>   | length rs >= 6                       = Bake {
>                                              bIx       = 0
>                                            , bWch      = denormInstrument (rs!!0) vChoiceI
>                                            , bOnset    = denorm (rs!!1) (0,songLength)
>                                            , bXpose    = denorm (rs!!2) (0.2,0.8)
>                                            , bSnd      = (vChoiceP !)
>                                                          $ truncate
>                                                          $ denorm (rs!!3) (0.0, fromIntegral (length vChoiceP) - 1)
>                                            , bVol      = round
>                                                          $ denorm (rs!!4) (60,100)
>                                            , bTempo    = denorm (rs!!5) (2,5)}
>   | otherwise                            = error "insufficiently sized list for sex2Bake" 
>     where
>       vChoiceI         :: Array Int (InstrumentName, (Pitch, Pitch))
>       vChoiceI                           = selectRanged choices
>       vChoiceP         :: Array Int PercussionSound
>       vChoiceP                           = listArray (0, length curatedP - 1) curatedP
>
>       curatedP                           = filter (not . flip elem blacklist) getList
>
>       choices = [  Piccolo,         Oboe,      AltoSax,             Trumpet
>                  , Trombone,        Marimba,   Vibraphone,          ElectricGuitarJazz
>                  , OrchestralHarp,  SlapBass1, BrightAcousticPiano, Harpsichord
>                  , ChurchOrgan,     Violin,    Contrabass]
>       blacklist = [MuteCuica, OpenCuica, LongWhistle, ShortWhistle]
>
> computeEnd             :: Double → Double → Double
> computeEnd onset dur                     = min songLength (onset + dur)
  
Definitions ===========================================================================================================

> data Bake =
>   Bake {
>       bIx              :: Int
>     , bWch             :: (InstrumentName, (Pitch, Pitch))
>     , bOnset           :: Double
>     , bXpose           :: Double
>     , bSnd             :: PercussionSound
>     , bVol             :: Volume
>     , bTempo           :: Double} deriving (Show, Eq, Ord)
>
> type TimeSpec                            = (Double, Double)
> type SectionSpec                         = (TimeSpec, TimeSpec)
>
> data BakingMetrics =
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

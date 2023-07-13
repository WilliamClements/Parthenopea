> {-# HLINT ignore "Eta reduce" #-}
> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

Baking
William Clements
December 16, 2022

> module Baking where

> import Data.Array ( Array, (!), (//), array )
> import Data.List ( sortBy, sortOn )
> import Data.Ord (comparing)
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
> consumeBakes :: [Bake] → Array Int (Music (Pitch, [NoteAttribute]))
> consumeBakes bakes = if checkJobOk (bm, ms)
>                      then aggrandize <$> ms
>                      else error "consumeBakes: bad job (wink)"
>   where (bm, ms) = buildChannels bakes
>
> measureBakes :: [Bake] → Array Int Int
> measureBakes bakes = if checkJobOk (bm, ms) && checkMeasureOk (bm, ms)
>                      then sHistogram bm
>                      else error "measureBakes: bad job (wink)"
>    where (bm, ms) = buildChannels bakes
>
> sampleBakes :: [Bake] → Array Int Int
> sampleBakes bakes = scoreOnsets bakingBins
>                     $ map extractOnset bakes
>    where
>       extractOnset :: Bake → Double
>       extractOnset bake@Bake 
>                    { bOnset = bo } = bo
>
> bakedJingle :: Int → Music (Pitch, [NoteAttribute])
> bakedJingle seed =
>    removeZeros
>    $ instrument Percussion
>    $ chordFromArray
>    $ bake4Consumption seed
>
> bake4Consumption :: Int → Array Int (Music (Pitch, [NoteAttribute]))
> bake4Consumption seed =
>    consumeBakes
>    $ zipWith (\a b → b{bIx = a}) [0..]
>    $ sortOn bOnset
>    $ map sex2Bake
>    $ take (round numSections)
>    $ sextuplets
>    $ mkStdGen seed
>
> bake4Measuring :: Int → Array Int Int
> bake4Measuring seed =
>    measureBakes
>    $ zipWith (\a b → b{bIx = a}) [0..]
>    $ sortOn bOnset
>    $ map sex2Bake
>    $ take (round numSections)
>    $ sextuplets
>    $ mkStdGen seed
>
> bake4Sampling :: Int → Array Int Int
> bake4Sampling seed =
>    sampleBakes
>    $ map sex2Bake
>    $ take (round numSections)
>    $ sextuplets
>    $ mkStdGen seed
>
> buildChannels :: [Bake] → Baking
> buildChannels bakes =
>    let
>       -- "Contexts" -urns- are in increasing onset time order. There is
>       -- an algorithm to mitigate "collisions", via skipping of
>       -- excessively "simultaneous" Bakes. Therefore, the -inns- list
>       -- is arranged in increasing end time order.
>       build :: [Bake] → [Bake] → Baking → Baking
>       build [] [] baking             = baking
>       build (urn:urns) [] baking     = tryUrn urn urns [] baking
>       build [] (inn:inns) baking     = retireInn inn [] inns baking
>       build (urn:urns) (inn:inns) baking
>          | computeEnd (bOnset inn) 1.0 < bOnset urn =
>               retireInn inn (urn:urns) inns baking
>          | otherwise =
>               tryUrn urn urns (inn:inns) baking
>
>       retireInn, tryUrn, acceptUrn, skipUrn
>       --   target   urns     inns      old      new
>          :: Bake → [Bake] → [Bake] → Baking → Baking
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
>             else error "acceptUrn: bad job (wink)"
>                 
>       skipUrn urn urns inns baking = build urns inns baking'
>          where baking' = skipSection urn baking
>               
>       skipSection :: Bake → Baking → Baking
>       skipSection urn (bm, ms) = (bm', ms)
>          where bm' = skipMetrics bm $ bOnset urn
>
>       acceptSection :: Bake → Baking → Baking
>       acceptSection urn@Bake 
>                      { bIx    = ix
>                      , bWch   = wch
>                      , bOnset = os
>                      , bXpose = xp
>                      , bSnd   = sound
>                      , bVol   = vol} (bm, ms) = (bm', ms')
>          where
>
>          chan  = ix `mod` numChannels
>          inst  = fst wch
>          range = snd wch
>
>          durSoFar :: Double
>          newDur   :: Double
>          ss       :: SectionSpec
>          newm     :: Music (Pitch, Volume)
>
>          -- algorithm gets an N squared component here
>          durSoFar = fromRational $ dur (ms!chan)
>          ss = calibrateSection durSoFar urn
>          newm = generateSection sound vol ss inst range xp
>          ms' = appendSection ms chan newm
>          newDur = fromRational $ dur (ms'!chan)
>
>          -- statskeeping
>          bm' = acceptMetrics bm durSoFar newDur ss os
>                   
>       appendSection :: Array Int (Music (Pitch, Volume))
>                        → Int
>                        → Music (Pitch, Volume)
>                        → Array Int (Music (Pitch, Volume))
>       appendSection ms chan newm = ms // [(chan, ms!chan :+: newm)]
>
>       calibrateSection :: Double → Bake → SectionSpec
>       calibrateSection
>          durSoFar
>          urn@Bake
>             {bIx     = ix
>             , bOnset = os
>             , bXpose = xp
>             , bVel   = vel} =
>             let
>                 restDur, restTempo, fillDur, fillTempo  :: Double
>
>                 -- use rest to pass the time until our onset
>                 restDur = (os - durSoFar)
>                 restTempo = 1.0
>
>                 -- meat part of the section
>                 fillDur = 1.0
>                 fillTempo = vel
>             in ((restDur, restTempo), (fillDur, fillTempo))
>
>       generateSection :: PercussionSound
>                          → Volume
>                          → SectionSpec
>                          → InstrumentName
>                          → (Pitch, Pitch)
>                          → Double
>                          → Music (Pitch, Volume)
>       generateSection sound vol ss inst range dXp = newM
>
>          where
>
>          newM = theRest :+: (thePercFill :=: theInstFill)
>
>          restDur, restTempo, fillDur, fillTempo  :: Double
>          fillBeats :: Int                 
>
>          ((restDur, restTempo), (fillDur, fillTempo)) = ss
>          fillBeats = round (fillDur * fillTempo * 3.0 / 4.0)
>
>          theRest, thePercFill, theInstFill :: Music (Pitch, Volume)
>
>          theRest =
>             rest
>             $ approx restDur
>          thePercFill =
>             addVolume (fractionOf vol 1.25)
>             $ roll (approx fillDur)
>             $ perc sound 
>             $ approx (0.5/fillTempo)
>          theInstFill =
>             addVolume (fractionOf vol 0.60)
>             $ instrument inst
>             $ makeInstFill fillBeats range dXp
>            
>    in build
>       bakes
>       []
>       (newBakingMetrics,
>       array (0, numChannels - 1)
>             [(x, rest 0) | x ← [0..(numChannels - 1)]])
>
> makeInstFill :: Int → (Pitch, Pitch) → Double → Music Pitch
> makeInstFill nBeats (lo, hi) dXp = note dur p
>   where
>     dur :: Rational
>     p :: Pitch
>     dLo, dHi :: Double
>
>     dur = qn * toRational nBeats
>
>     dLo = fromIntegral (absPitch lo)
>     dHi = fromIntegral (absPitch hi)
>     p = pitch $ round $ dLo + (dXp * (dHi - dLo))
>
> sortByOnset :: Bake → Bake → Ordering
> sortByOnset lhs@Bake{bOnset = t1} rhs@Bake{bOnset = t2} =
>   compare t1 t2
>
> sortByEnd :: Bake → Bake → Ordering
> sortByEnd lhs@Bake{bOnset = t1} rhs@Bake{bOnset = t2} =
>   compare (computeEnd t1 1.0) (computeEnd t2 1.0)
>
> checkMeasureOk :: Baking → Bool
> checkMeasureOk (bm, ms)
>   | traceAlways msg False = undefined
>   | otherwise = True
>   where
>     bins = sHistogram bm
>     msg = unwords ["\n checkMeasureOk bins =", show bins]
> checkJobOk :: Baking → Bool
> checkJobOk (bm@BakingMetrics
>             { sBaked        = nB
>             , sSkipped      = skpd
>             , sAccumBaked   = aB
>             , sAccumSkipped = asps} , ms)
>    | traceIf msg False = undefined
>    | otherwise = True
>    where
>       msg = unwords
>              [ "checkJobOk: bm =",        show bm
>                , "\n avg skipped start=", show $ safeAverage asps skpd
>                , "avg baked start=",      show $ safeAverage aB nB 
>                , "durs=",                 foldShowDur ms]
>       safeAverage :: Double → Int → Double
>       safeAverage d n
>          | n==0 = 0.0
>          | otherwise = d / fromIntegral n
>       foldShowDur :: Array Int (Music (Pitch, Volume)) → String
>       foldShowDur = foldr foldFun ""
>          where
>             foldFun :: Music (Pitch, Volume) → String → String
>             foldFun m s = " " ++ show d1 ++ s
>                where
>                   d1 :: Double
>                   d1 = fromRational (dur m)
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
> acceptMetrics :: BakingMetrics → Double → Double → SectionSpec → Double → BakingMetrics
> acceptMetrics bm@BakingMetrics
>          { sBaked      = nB
>          , sRestDur    = rD
>          , sFillDur    = fD
>          , sAccumBaked = aB
>          , sHistogram  = hst} durSoFar newDur ss os
>    | traceNever msg False = undefined
>    | otherwise =
>    let     restDur, fillDur  :: Double
>            ((restDur, _), (fillDur, _)) = ss
>            baked'   = nB           + 1
>            restDur' = rD           + restDur
>            fillDur' = fD           + fillDur
>            accum'   = aB           + os
>            zlist    = quantize hst os fillDur
>            hst'    = if not (checkZListOk zlist) then error "Bad ZList"
>                                                   else hst // zlist
>    in bm { sBaked      = baked'
>          , sRestDur    = restDur'
>          , sFillDur    = fillDur'
>          , sAccumBaked = accum'
>          , sHistogram  = hst'}
>    where
>      msg = unwords
>                 [ "acceptMetrics ", show durSoFar, " ← durSoFar, os → ", show os
>                 ,       "\n ss = ", show ss,
>                       "\n newDur=", show newDur]
>      quantize :: Array Int Int → Double → Double → [(Int,  Int)]
>      quantize hst os du = [(x, (hst!x) + 1) | x ← [fk..gk]]
>        where
>          fk,gk :: Int
>          fk = histSelect os
>          gk = histSelect $ computeEnd os du - 0.000001
>      histSelect :: Double → Int
>      histSelect os = floor $ os * fromIntegral bakingBins / songLength
>
> skipMetrics :: BakingMetrics → Double → BakingMetrics
> skipMetrics bm accum = 
>    let     skipped'      = sSkipped bm      + 1
>            accum'        = sAccumSkipped bm + accum
>    in bm { sSkipped      = skipped'
>          , sAccumSkipped = accum'}
>
> sex2Bake :: [Double] → Bake
> sex2Bake rs
>    | length rs >= 6 = 
>       Bake {
>          bIx =                            0
>          , bWch =          denormInstrument (rs!!0) selected
>          , bOnset =                  denorm (rs!!1) (0,songLength)
>          , bXpose =                  denorm (rs!!2) (0.2,0.8)
>          , bSnd =  toEnum $ round  $ denorm (rs!!3) (0,percussionLimit)
>          , bVol =  round           $ denorm (rs!!4) (60,100)
>          , bVel =                    denorm (rs!!5) (2,5)}
>    | otherwise = error "insufficiently sized list for sex2Bake" 
>       where
>         choices :: [InstrumentName]
>         choices = [Piccolo,         Oboe,      AltoSax,             Trumpet
>                  , Trombone,        Marimba,   Vibraphone,          DistortionGuitar
>                  , OrchestralHarp,  SlapBass1, BrightAcousticPiano, Harpsichord
>                  , ChurchOrgan,     Violin,    Contrabass]
>         isChoice :: InstrumentName → Bool
>         isChoice x = x `elem` choices
>         selected :: Array Int (InstrumentName, (Pitch, Pitch))
>         selected = selectInstruments isChoice choices
>
> computeEnd :: Double → Double → Double
> computeEnd onset dur = 
>    min songLength (onset + dur)
>
  
Definitions ===========================================================================================================

> data Bake = Bake { bIx             :: Int
>                  , bWch            :: (InstrumentName, (Pitch, Pitch))
>                  , bOnset          :: Double
>                  , bXpose          :: Double
>                  , bSnd            :: PercussionSound
>                  , bVol            :: Volume
>                  , bVel            :: Double}
>    deriving (Show, Eq, Ord)
>
> type TimeSpec =      (Double, Double)
> type SectionSpec = (TimeSpec, TimeSpec)
>
> data BakingMetrics =
>   BakingMetrics {  sSkipped       :: Int
>                  , sBaked         :: Int
>                  , sRestDur       :: Double
>                  , sFillDur       :: Double
>                  , sAccumBaked    :: Double
>                  , sAccumSkipped  :: Double
>                  , sHistogram     :: Array Int Int}
>      deriving (Show, Eq, Ord)
>
> newBakingMetrics =
>   BakingMetrics 0 0 0 0 0 0 (array (0, eb) [(x, 0) | x ← [0..eb]])
>     where eb = bakingBins - 1
> bakingBins = 64
>
> songLength, numSections :: Double
> songLength = 32.0      -- 64 seconds
> numSections = 512.0
>
> numChannels, skipThreshold, bakingBins :: Int
> numChannels = 7
> skipThreshold = 16

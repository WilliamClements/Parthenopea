> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Discrete
William Clements
June 17, 2024

> module Discrete where
>
> import Control.Arrow
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Bits
> import Data.Complex
> import Data.Graph (Graph)
> import qualified Data.Graph              as Graph
> import Data.List ( foldl', iterate', find )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, mapMaybe )
> import Data.MemoTrie
> import Debug.Trace ( traceIO, trace )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..), collapse )
> import Euterpea.Music ( Volume, AbsPitch, Dur, absPitch, PitchClass(..) )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import HSoM.Examples.Additive ( sineTable, sfTest1 )
> import Numeric.FFT ( fft, ifft )
> import Parthenopea
  
Discrete approach =====================================================================================================

> createFrFun            :: Double → Double → Double → (Double → Double)
> createFrFun cutoff bulge bwCent
>   | traceNow trace_CFF False             = undefined
>   | otherwise                            =
>       profess (bwWidth >= 0 && cutoff >= bwWidth)
>               (unwords ["createFrFun: bad bwWidth (or) cutoff", show bwWidth, show cutoff])
>               (\freq → mapx freq * tracer "ynorm" ynorm)
>   where
>     trace_CFF                            =
>       unwords ["createFrFun", show (cutoff, bulge, bwCent, bwWidth, ynorm)
>                             , " = (cutoff, bulge, bwCent, bwWidth, ynorm)"]
>
>     bwWidth                              = professInRange
>                                              (0, 100)
>                                              bwCent
>                                              "bwCent"
>                                              (if bulge > 0
>                                                then bwCent / 100
>                                                else 0)
>     ynorm                                = professInRange
>                                              (0, 1)
>                                              bwWidth
>                                              "bwWidth"
>                                              1 / (1 + bulge)
>
>     xA                                   = cutoff - bwWidth
>     xB                                   = cutoff
>     xC                                   = cutoff + bwWidth
>
>     mapx               :: Double → Double
>     mapx x
>       | x < 0                            = error "mapx"
>       | x < xA                           = 1
>       | x < xB                           = startUp    ((x - xA) / (xB - xA))
>       | x < xC                           = finishDown ((x - xB) / (xC - xB))
>       | otherwise                        = taperOff    (x - xC)
>
>     startUp c                            = 1 + controlConvex c
>     finishDown c                         = 1 + controlConvex (1 - c)
>     taperOff d                           = max 0 (1 - rolloff d)
>
>     rolloff            :: Double → Double
>     rolloff c                            = c * c / 1_048_576 -- WOX what should it be?
>
> squeezeIR = 1 
>
> computeIR          :: KernelSpec → Maybe (Array Int Double)
> computeIR KernelSpec{ .. }
>   | traceNow trace_CIR False              = undefined
>   | otherwise                             =
>   if null esOut
>     then Nothing
>     else Just $ listArray (0, impulseSize - 1) (map (* amp) esOut)
>   where
>     trace_CIR                            = unwords ["computeIR", show (ksFc, ksQ, ksSr), show delta, show amp]
>
>     -- factor for choosing frequencies to be interested in
>     delta              :: Double         = fromIntegral ksSr / squeezeIR * fromIntegral impulseSize
>
>     esInComplex        :: [Complex Double]
>     esInReal, esOut    :: [Double]
>     esInReal                             =
>       map (fr . (* delta) . fromIntegral) ([0..impulseSize - 1]::[Int])
>     esInComplex                          =
>       map (:+ 0) esInReal
>     esOut                                =
>       if useFastFourier
>         then esInReal
>         else map realPart (ifft esInComplex)
>
>     amp                                  = 1 / maximum esOut
>     
>     fr                 :: (Double → Double)
>                                          = createFrFun dInitFc dNormQ bulgeBandWidth
>     dInitFc                              = fromIntegral ksFc
>     dNormQ                               = fromIntegral ksQ / 960
>
> memoizedComputeIR = memo computeIR
>
> applyConvolutionMono   :: ∀ p . Clock p ⇒ Lowpass → Double → Signal p () Double → Signal p () Double                 
> applyConvolutionMono lowP secsToPlay sIn
>   | traceNot trace_AC False              = undefined
>   | otherwise                            = toContinuousSig result 1
>   where
>     oper1, oper2       :: DiscreteSig Int Double
>     oper1                                = toDiscreteSig "mono operand" secsToPlay sIn
>     oper2                                = 
>       maybe oper1 (fromRawVector "impulse response mono") (memoizedComputeIR lowP.lowpassKs)
>     result                               = convolveDiscreteSigs oper1 oper2
>
>     trace_AC                             = unwords ["applyConvolutionMono", show lowP]
>
> applyConvolutionStereo :: ∀ a p . Clock p ⇒
>                           (Lowpass, Lowpass)
>                           → Double
>                           → Signal p () (Double, Double)
>                           → Signal p () (Double, Double)
> applyConvolutionStereo (lowpL, lowpR) secsToPlay sIn
>   | traceNot trace_AC False              = undefined
>   | otherwise                            = toContinuousSig result 1
>   where
>     pairs                                = toSamples secsToPlay sIn
>     kN                                   = length pairs
>
>     oper1a                               = fromRawVector "stereo left" $ listArray (0, kN - 1) (map fst pairs)
>     oper1b                               = fromRawVector "stereo right" $ listArray (0, kN - 1) (map snd pairs)
>     oper2a                               = maybe (error "memoLeft")  (fromRawVector "impulse response left") (memoizedComputeIR lowpL.lowpassKs)
>     oper2b                               = maybe (error "memoRight") (fromRawVector "impulse response right") (memoizedComputeIR lowpR.lowpassKs)
>
>     resulta                              = convolveDiscreteSigs oper1a oper2a
>     resultb                              = convolveDiscreteSigs oper1b oper2b
>
>     pairs'                               = zip (elems $ dsigVec resulta) (elems $ dsigVec resultb)
>     kN'                                  = length pairs'
>
>     result                               = fromRawVector "convolved stereo" $ listArray (0, kN' - 1) pairs'
>
>     trace_AC                             = unwords ["applyConvolutionStereo", show lowpL]
>
> toDiscreteSig          :: ∀ a i p. (WaveAudioSample a, Ix i, Integral i, Clock p) ⇒
>                           String → Double → Signal p () a → DiscreteSig i a
> toDiscreteSig tag dur sf                 = DiscreteSig tag stats vec
>   where
>     dlist              :: [a]            = toSamples dur sf
>     bs                 :: (i, i)         = (0, fromIntegral (length dlist) - 1)
>     vec                                  = listArray bs dlist
>     stats                                = measureDiscreteSig vec
>
> fromRawVector          :: ∀ a i. (WaveAudioSample a, Ix i, Integral i) ⇒
>                           String → Array i a → DiscreteSig i a
> fromRawVector tag vec                    = DiscreteSig tag (measureDiscreteSig vec) vec
>
> normalize, noNormalize :: ∀ a i. (WaveAudioSample a, Ix i) ⇒
>                           Array i a → Array i a
> normalize vec                            = listArray (bounds vec) (map (ascale (1 / maxImpulse vec)) (elems vec))
> noNormalize vec                          = vec
> 
> maxImpulse             :: ∀ a i. (WaveAudioSample a, Ix i) ⇒ Array i a → Double
> maxImpulse vec                           = maximum $ map abs (concatMap collapse (elems vec))
>
> multiplyDiscreteSigs   :: ∀ b i. (Ix i, Integral i, Show i, Ord b, Num b, WaveAudioSample b, Show b) ⇒
>                           DiscreteSig i b → DiscreteSig i b → DiscreteSig i b
> multiplyDiscreteSigs dsig1@DiscreteSig{dsigTag = tag1, dsigVec = vec1, dsigStats = stats1}
>                      dsig2@DiscreteSig{dsigTag = tag2, dsigVec = vec2, dsigStats = stats2} =
>   profess
>     (len1 == len2)
>     (unwords ["lengths", show len1, "and", show len2, "illegally different for multiplyDiscreteSigs"])
>     (fromRawVector
>        (unwords ["product of (", tag1, ") and (", tag2, ")"])
>        (listArray (0, len1 - 1) (zipWith (*) (elems vec1) (elems vec2))))
>   where
>     DiscreteStats{ dsigLength = len1 }   = stats1
>     DiscreteStats{ dsigLength = len2 }   = stats2
>    
> convolveDiscreteSigs   :: ∀ b i. (Ix i, Integral i, Show i, Ord b, Num b, WaveAudioSample b, Show b) ⇒
>                           DiscreteSig i b → DiscreteSig i b → DiscreteSig i b
> convolveDiscreteSigs dsig1 dsig2
>   | traceNow trace_CA False              = undefined
>   | otherwise                            =
>   profess
>     (ok x1 && ok x2 && ok x3)
>     (unwords ["convolveDiscreteSigs-- problem with 1,2, or 3:", show (ok x1, ok x2, ok x3)])
>     dsig3
>   where
>     (x1, x2)                             = (dsigVec dsig1, dsigVec dsig2)
>     (m1, m2)                             = (snd (bounds x1), snd (bounds x2))
>     m3                                   = m1 + m2 + 1
>     x3                 :: Array i b      =
>       listArray (0,m3)
>         [ sum [ x1 ! k * x2 ! (n-k) | k ← [max 0 (n-m2)..min n m1] ] | n ← [0..m3] ]
>     dsig3              :: DiscreteSig i b
>     dsig3                                = fromRawVector "convolved" x3
>
>     ok vec                               =
>       fst (bounds vec) == 0 && snd (bounds vec) > 0 && all ((< 1_000) . abs) vec
>
>     trace_CA                             =
>       unwords ["convolveDiscreteSigs\n", show dsig1
>              , "\n X \n", show dsig2
>              , "\n = \n", show dsig3]
>
> fftDiscreteSignal         :: ∀ b i. (Ix i, Integral i, Show i) ⇒
>                              DiscreteSig i Double → DiscreteSig i Double
> fftDiscreteSignal DiscreteSig{ .. }      =
>   fromRawVector
>     (unwords ["fft of", dsigTag])
>     (listArray (0, dsigLength - 1) (map realPart (fft $ map (:+ 0) (elems dsigVec))))
>   where
>     DiscreteStats { .. }                 = dsigStats
>
> measureDiscreteSig     :: ∀ a i. (WaveAudioSample a, Ix i, Integral i) ⇒ Array i a → DiscreteStats i a
> measureDiscreteSig vec                   = stats''
>   where
>     len                :: Double         =
>       profess
>         (not $ null vec)
>         (unwords ["discrete signal cannot be empty"])
>         (fromIntegral $ snd (bounds vec) + 1)
>     stats'@DiscreteStats{ .. }           = foldl' sfolder (DiscreteStats 0 azero azero azero) (elems vec)
>     stats''                              = stats' {statsSquare = asqrt $ ascale (1/len) statsSquare}
>
>     sfolder            :: DiscreteStats i a → a → DiscreteStats i a
>     sfolder stats@DiscreteStats{ .. } d  =
>       stats {
>              dsigLength                  = dsigLength + 1
>              , statsDCOffset             = aadd statsDCOffset d
>              , statsSquare               = aadd statsSquare (amul d d)
>              , statsMaxAmp               = amax statsMaxAmp (aabs d)}
>
> toContinuousSig        :: ∀ a i p. (WaveAudioSample a, Show a, Ix i, Integral i, Show i, Clock p) ⇒
>                           DiscreteSig i a → Double → Signal p () a
> toContinuousSig dsig@DiscreteSig{ .. } amp
>   | traceNow trace_MS False              = undefined
>   | otherwise                            =
>     proc ()                              → do
>       rec
>         ii' ← delay 0                    ⤙ ii
>         let ii                           = ii' + 1
>       outA                               ⤙ ascale amp (dsigVec ! (ii' `mod` dsigLength))
>   where
>     trace_MS                             = unwords ["toContinuousSig", show dsig]
>
>     DiscreteStats{ .. }                  = dsigStats

Type declarations =====================================================================================================

> data ResonanceType                       =
>   ResonanceNone
>   | ResonanceConvo 
>   | ResonanceLowpass
>   | ResonanceBandpass
>   | ResonanceSVF1
>   | ResonanceSVF2
>   | ResonanceOnePole
>   | ResonanceTwoPoles deriving (Eq, Bounded, Enum, Show)
>
> data Lowpass                             =
>   Lowpass {
>     lowpassType        :: ResonanceType
>   , lowpassKs          :: KernelSpec
>   , lowpassFc          :: Double
>   , lowpassQ           :: Double} deriving (Eq, Show)
>
> data DiscreteStats i a                   =
>   DiscreteStats {
>     dsigLength            :: i
>     , statsDCOffset       :: a
>     , statsSquare         :: a
>     , statsMaxAmp         :: a} deriving Show
>
> data DiscreteSig i a                     =
>   DiscreteSig {
>     dsigTag               :: String
>     , dsigStats           :: DiscreteStats i a
>     , dsigVec             :: Array i a}
>
> instance (Show i, Show a) ⇒ Show (DiscreteSig i a) where
>   show                    :: DiscreteSig i a → String
>   show DiscreteSig{ .. }                 = unwords [show dsigTag, show dsigStats]
>
> data DiscreteSettings =
>   DiscreteSettings {
>     qqBulgeBandwidth   :: Double
>   , qqImpulseSize      :: Int
>   , qqUseFastFourier   :: Bool} deriving Show
>
> defD                   :: DiscreteSettings
> defD =
>   DiscreteSettings {
>     qqBulgeBandwidth                     = 10
>   , qqImpulseSize                        = 2_048
>   , qqUseFastFourier                     = False}
>
> bulgeBandWidth                           = qqBulgeBandwidth             defD
> impulseSize                              = qqImpulseSize                defD
> useFastFourier                           = qqUseFastFourier             defD

The End
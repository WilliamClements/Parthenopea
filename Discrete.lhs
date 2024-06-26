> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE InstanceSigs #-}
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
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..), collapse )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import Numeric.FFT ( fft, ifft )
> import Parthenopea
  
Discrete approach =====================================================================================================

> computeIR          :: KernelSpec → Maybe (Array Int Double)
> computeIR ks@KernelSpec{ .. }
>   | traceNow trace_CIR False              = undefined
>   | otherwise                             =
>   profess
>     (ksLen >= 0)
>     (unwords ["computeIR bad ksLen"])
>     (if null esOut
>        then Nothing
>        else Just $ listArray (0, esOutLen - 1) esOut)
>   where
>     trace_CIR                            =
>       unwords ["computeIR", show ks, "delta=", show delta]
>
>     -- used below for enumerating interesting frequencies
>     delta              :: Double         = fromIntegral ksSr / fromIntegral ksLen
>
>     esIn, esOut        :: [Double]
>     esIn                                 = map (getFreaky ks . (* delta) . fromIntegral) ([0..ksLen - 1]::[Int])
>     esOut                                =
>       if useFastFourier
>         then esIn
>         else fftDoubles esIn "toTimeDomain"
>     esOutLen                             = length esOut
>
> memoizedComputeIR = memo computeIR
>
> applyConvolutionMono   :: ∀ p . Clock p ⇒ Lowpass → Double → Signal p () Double → Signal p () Double                 
> applyConvolutionMono lowP secsToPlay sIn
>   | traceNot trace_AC False              = undefined
>   | otherwise                            = sig 
>   where
>     dsigIn             :: Maybe (DiscreteSig Int Double)
>     dsigIn                               = toDiscreteSig "input mono" secsToPlay sIn
>
>     sig                                  = maybe sIn doIt dsigIn
>
>     doIt               :: DiscreteSig Int Double → Signal p () Double
>     doIt dsigOut                         =
>       if useFastFourier
>         then toContinuousSig (fastConvolveFR dsigOut lowP) secsToPlay
>         else toContinuousSig (slowConvolveIR dsigOut lowP) secsToPlay 
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
>     dsigInL                              = fromRawVector "input left"  $ listArray (0, kN - 1) (map fst pairs)
>     dsigInR                              = fromRawVector "input right" $ listArray (0, kN - 1) (map snd pairs)
>
>     resultL                              =
>       if useFastFourier
>         then fastConvolveFR dsigInL lowpL
>         else slowConvolveIR     dsigInL lowpL
>     resultR                              =
>       if useFastFourier
>         then fastConvolveFR dsigInR lowpR
>         else slowConvolveIR     dsigInR lowpR
>
>     pairs'                               = zip (elems $ dsigVec resultL) (elems $ dsigVec resultR)
>     kN'                                  = length pairs'
>
>     result                               = fromRawVector "convolved stereo" $ listArray (0, kN' - 1) pairs'
>
>     trace_AC                             = unwords ["applyConvolutionStereo", show lowpL]
>
> toDiscreteSig          :: ∀ a i p. (WaveAudioSample a, Ix i, Integral i, Show i, Clock p) ⇒
>                           String → Double → Signal p () a → Maybe (DiscreteSig i a)
> toDiscreteSig tag dur sf
>   | traceNot trace_TDS False             = undefined
>   | otherwise                            = 
>   if not (null dlist)
>     then Just $ DiscreteSig tag stats vec
>     else Nothing
>   where
>     dlist              :: [a]            = toSamples dur sf
>     dlistLen                             = fromIntegral $ length dlist
>     bs                 :: (i, i)         = (0, dlistLen - 1)
>     vec                                  = listArray bs dlist
>     stats                                = measureDiscreteSig vec
>
>     trace_TDS                            = unwords ["toDiscreteSig", show dlistLen]
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
>                      dsig2@DiscreteSig{dsigTag = tag2, dsigVec = vec2, dsigStats = stats2}
>                                          =
>   profess
>     (len1 == len2)
>     (unwords ["lengths", show len1, "and", show len2, "illegally different for multiplyDiscreteSigs", tag1, tag2])
>     (fromRawVector
>        (unwords ["product of (", tag1, ") and (", tag2, ")"])
>        (listArray (0, zlen - 1) zipped))
>   where
>     DiscreteStats{ dsigLength = len1 }   = stats1
>     DiscreteStats{ dsigLength = len2 }   = stats2
>     zipped                               = zipWith (*) (elems vec1) (elems vec2)
>     zlen                                 = fromIntegral $ length zipped
>    
> slowConvolveIR         :: DiscreteSig Int Double → Lowpass → DiscreteSig Int Double
> slowConvolveIR dsigIn Lowpass{ .. }
>   | traceNot trace_CIR False             = undefined
>   | otherwise                            =
>   profess
>     (ok x1 && ok x2 && ok x3)
>     (unwords ["slowConvolveIR-- problem with 1,2, or 3:", show (ok x1, ok x2, ok x3)])
>     dsigOut
>   where
>     dsigIR                               =
>       maybe (error "slowConvolveIR") (fromRawVector "IR") (memoizedComputeIR lowpassKs)
>     (x1, x2)                             = (dsigVec dsigIn, dsigVec dsigIR)
>     (m1, m2)                             = (snd (bounds x1), snd (bounds x2))
>     m3                                   = m1 + m2 + 1
>     x3                                   =
>       listArray (0,m3)
>         [ sum [ x1 ! k * x2 ! (n-k) | k ← [max 0 (n-m2)..min n m1] ] | n ← [0..m3] ]
>     dsigOut                              = fromRawVector "slow-convolved" x3
>
>     ok vec                               =
>       fst (bounds vec) == 0 && snd (bounds vec) > 0 && all ((< 1_000) . abs) vec
>
>     trace_CIR                            =
>       unwords ["slowConvolveIR\n", show dsigIn
>              , "\n X \n", show dsigIR
>              , "\n = \n", show dsigOut]
>
> fastConvolveFR         :: DiscreteSig Int Double → Lowpass → DiscreteSig Int Double
> fastConvolveFR dsigIn Lowpass{ .. }
>   | traceNot trace_FCIR False            = undefined
>   | otherwise                            =
>     dsigOut
>   where
>     dsigIn', dsigFR    :: DiscreteSig Int Double
>     dsigIn'                              = fftDiscreteSig dsigIn "toFrequencyDomain"
>     mvec               :: Maybe (Array Int Double)
>     mvec                                 = memoizedComputeIR lowpassKs{ksLen = dsigIn'.dsigStats.dsigLength}
>     vecOut             :: Array Int Double
>     vecOut                               = fromMaybe dsigIn'.dsigVec mvec
>     dsigFR                               = fromRawVector "FR!" vecOut
>     dsigOut                              =
>       if null vecOut
>         then dsigIn
>         else fftDiscreteSig (multiplyDiscreteSigs dsigIn' dsigFR) "toTimeDomain"
>
>     trace_FCIR                             =
>       unwords ["fastConvolveFR\n", show dsigIn
>                       , "\n X \n", show dsigFR
>                       , "\n = \n", show dsigOut]
>
> measureDiscreteSig     :: ∀ a i. (WaveAudioSample a, Ix i, Integral i) ⇒ Array i a → DiscreteStats i a
> measureDiscreteSig vec                   = stats''
>   where
>     len                :: Double         =
>       profess
>         (not (null vec))
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
>   | traceNot trace_MS False              = undefined
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
>
> fftDiscreteSig         :: ∀ i. (Ix i, Integral i, Show i) ⇒
>                           DiscreteSig i Double → String → DiscreteSig i Double
> fftDiscreteSig DiscreteSig{ .. } which = fromRawVector (unwords [which, "(", dsigTag, ")"]) raw 
>   where
>     DiscreteStats{ .. }                  = dsigStats
>     dsOut              :: [Double]       = fftDoubles (elems dsigVec) which
>     dsOutLen                             = fromIntegral $ length dsOut
>     raw                :: Array i Double = listArray (0, dsOutLen - 1) dsOut
>
> fftDoubles             :: [Double] → String → [Double]
> fftDoubles dsIn which                    = map realPart (fftfun (map (:+ 0) dsIn))
>   where
>     fftfun             :: [Complex Double] → [Complex Double]
>     fftfun                               =
>       if "toFrequencyDomain" == which
>         then fft
>         else ifft
>
> startUp                              = controlConvex
> finishDown c                         = controlConvex (1 - c)

Frequency Response ====================================================================================================

x-axis: frequency in cycles/sec
y-axis: amplitude ratio normalized to 0..1

but first, the y-axis is modeled as from 0 to one PLUS the Q-induced bulge ratio
(i.e. these ys, in final step, are multiplied by 1 / (1 + bulge) to normalize them)

because the Q "bulge" magnitude is defined as (0..960) centibels ABOVE DC-gain, and we here treat the DC-gain as one

second, we want to "gain" -120 centibels/octave from the "cutoff"

> getFreaky              :: KernelSpec → Double → Double
> getFreaky ks@KernelSpec{ .. }            = freakyResponse ks shapes
>   where
>     stretch                              = if q == 0 then 0 else fc / bulgeDiv
>     width                                = fc - (stretch / 2)
>     height                               = 1
>
>     shapes             :: [ResponseShape]
>     shapes                               =
>       [  Block width height
>        , Bulge stretch height
>        , Decline dropoffRate height]
>
>     fc, q              :: Double
>     fc                                   = fromAbsoluteCents ksFc
>     q                                    = fromIntegral ksQ
>     
> freakyResponse         :: KernelSpec → [ResponseShape] → Double → Double
> freakyResponse KernelSpec{ .. } shapes xIn
>                                          = ((* yn) . fryXform . modelXform . frxXform) xIn
>   where
>     FrSummary{ .. }                      = foldl' doShape (FrSummary [] 0) shapes
>     FrItem{ .. }                         =
>       professIsJust
>         (find inVogue frsItems)
>         (unwords ["could not find an item??!? -- bad boundaries"])
>
>     fc, sr, bp, yn     :: Double
>     fc                                   = fromAbsoluteCents ksFc
>     sr                                   = fromIntegral ksSr
>     bp                                   = fromCentibels $ fromIntegral ksQ
>     yn                                   = 1 / (1 + bp)
>     
>     inVogue            :: FrItem → Bool
>     inVogue FrItem{ .. }                 = xIn == clip friRange xIn
>
>     updateSummary      :: FrSummary → Double → [FrItem] → FrSummary
>     updateSummary prev@FrSummary{ .. } newD newIs
>                                          =
>       prev{frsDisplacement = newD, frsItems = frsItems ++ newIs}
>
>     doShape            :: FrSummary → ResponseShape → FrSummary
>     doShape prev@FrSummary{ .. } (Block width height)
>                                          = updateSummary prev newD [newI]
>       where
>         newD           :: Double         = frsDisplacement + width
>         newI           :: FrItem         = FrItem (frsDisplacement, newD) sr id (const height) id
>         
>     doShape prev@FrSummary{ .. } (Bulge width height)
>                                          = updateSummary prev newD2 iList
>       where
>         newD1, newD2   :: Double
>         newD1                            = frsDisplacement + width / 2
>         newD2                            = frsDisplacement + width
>
>         iList          :: [FrItem]       = if width <= 0 then [] else [newI1, newI2]
>
>         newI1, newI2   :: FrItem
>         newI1                            =
>           FrItem
>             (frsDisplacement, newD1)
>             sr
>             (ddNorm2 frsDisplacement newD1)
>             startUp
>             (ddXform1d bp height)
>         newI2                            =
>           FrItem
>             (newD1, newD2)
>             sr
>             (ddNorm2 newD1 newD2)
>             finishDown
>             (ddXform1d bp height)
>         
>     doShape prev@FrSummary{ .. } (Decline dropoff height)
>                                          = updateSummary prev sr [newI]
>       where
>         newI                             =
>           FrItem
>             (frsDisplacement, sr)
>             sr
>             (ddDown3 frsDisplacement dropoff height)
>             (max 0)
>             id -- (alternatively, could be "ddXform1d bp height" with corresponding change above)
>
>     ddXform1d          :: Double → Double → (Double → Double)
>     ddXform1d scale offset xFrom
>       | traceNot trace_DDX False         = undefined
>       | otherwise                        = yTo
>       where
>         yTo                              = scale * xFrom + offset
>         trace_DDX                        =
>           unwords ["ddXform1d", show (scale, offset, xFrom, yTo)]
>
>     ddNorm2            :: Double → Double → (Double → Double)
>     ddNorm2 dLeft dRight xFrom
>       | traceNot trace_DDN False         = undefined
>       | otherwise                        = ratio
>       where
>         ratio                            = (xFrom - dLeft) / (dRight - dLeft)
>         trace_DDN                        =
>           unwords ["ddNorm2", show (dLeft, dRight, xFrom, ratio)]
>
>     ddDown3            :: Double → Double → Double → (Double → Double)
>     ddDown3 fLeft rate height xFrom
>       | traceNot trace_DDD False         = undefined
>       | otherwise                        = newMag
>       where
>         rate'                            = rate * xFrom / (2 * fLeft)   -- centibels per octave
>                                                                         -- maybe fLeft and xFrom should be squared?
>         newMag                           = relativeAmp fLeft height xFrom rate'
>         trace_DDD                        =
>           unwords ["ddDown3\n", show (fLeft, rate, height, xFrom, newMag)]

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
>   , lowpassKs          :: KernelSpec} deriving (Eq, Show)
>
> lowpassFc              :: Lowpass → Double
> lowpassFc lp                             = fromAbsoluteCents (ksFc $ lowpassKs lp)
>
> lowpassQ               :: Lowpass → Double
> lowpassQ lp                              = fromIntegral (ksQ $ lowpassKs lp)
>
> data DiscreteStats i a                   =
>   DiscreteStats {
>     dsigLength         :: i
>     , statsDCOffset    :: a
>     , statsSquare      :: a
>     , statsMaxAmp      :: a} deriving Show
>
> data DiscreteSig i a                     =
>   DiscreteSig {
>     dsigTag            :: String
>     , dsigStats        :: DiscreteStats i a
>     , dsigVec          :: Array i a}
>
> instance (Show i, Show a) ⇒ Show (DiscreteSig i a) where
>   show                    :: DiscreteSig i a → String
>   show DiscreteSig{ .. }                 = unwords [show dsigTag, show dsigStats]
> 
> data DiscreteSettings =
>   DiscreteSettings {
>     qqBulgeDiv         :: Double
>   , qqDropoffRate      :: Double
>   , qqImpulseSize      :: Int
>   , qqUseFastFourier   :: Bool} deriving Show
>
> data ResponseShape =
>     Block Double Double                  -- width height
>   | Bulge Double Double                  -- width height
>   | Decline Double Double                -- dropoff height
>     deriving Show
>
> data FrItem =
>   FrItem {
>     friRange           :: (Double, Double)
>   , friSampleRate      :: Double
>
>   , frxXform           :: Double → Double
>   , modelXform         :: Double → Double
>   , fryXform           :: Double → Double}
>
> instance Show FrItem where
>   show (FrItem range _ _ _ _)    = unwords [show range]
>
> data FrSummary =
>   FrSummary {
>     frsItems           :: [FrItem]
>   , frsDisplacement    :: Double} deriving Show
>
> type ObjectData                          = Double
> defObjectData          :: ObjectData     = 0
>
> bulgeDiv                                 = qqBulgeDiv                   defD
> dropoffRate                              = qqDropoffRate                defD
> impulseSize                              = qqImpulseSize                defD
> useFastFourier                           = qqUseFastFourier             defD
>
> defD                   :: DiscreteSettings
> defD =
>   DiscreteSettings {
>     qqBulgeDiv                           = 20                           -- bulge bandwidth is frequency / div
>   , qqDropoffRate                        = 120                          -- centibels per octave
>   , qqImpulseSize                        = 2_048                        -- small "default" size
>   , qqUseFastFourier                     = True}

The End
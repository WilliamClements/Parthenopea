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

> createFrFun            :: Double → Double → Double → (Double → Double)
> createFrFun cutoff bulge bwCent
>   | traceNow trace_CFF False             = undefined
>   | otherwise                            =
>       profess (bwWidth >= 0 && cutoff >= bwWidth)
>               (unwords ["createFrFun: bad bwWidth (or) cutoff", show bwWidth, show cutoff])
>               (\freq → mapx freq * ynorm)
>   where
>     trace_CFF                            =
>       unwords ["createFrFun", show (cutoff, bulge, bwCent, bwWidth, ynorm)
>                             , " = (cutoff, bulge, bwCent, bwWidth, ynorm)"]
>
>     bwWidth                              =
>       professInRange (0, 100) bwCent "bwCent" (if bulge > 0 then bwCent / 100 else 0)
>     ynorm                                =
>       professInRange (0, 1)  bwWidth "bwWidth" 1 / (1 + bulge)
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
>     taperOff d                           = max 0 (1 - rolloff d)
>
>     rolloff            :: Double → Double
>     rolloff c                            = c * c / 1_048_576 -- WOX what should formula be?
>
> computeIR          :: KernelSpec → Int → Maybe (Array Int Double)
> computeIR KernelSpec{ .. } len
>   | traceNow trace_CIR False              = undefined
>   | otherwise                             =
>   if null esOut
>     then Nothing
>     else Just $ listArray (0, esOutLen - 1) (map (* amp) esOut)
>   where
>     trace_CIR                            =
>       unwords ["computeIR", show (ksFc, ksQ, ksSr)
>                           , "len=", show len, "delta=", show delta, "amp=", show amp]
>
>     -- factor for choosing frequencies to be interested in
>     delta              :: Double         = fromIntegral ksSr / fromIntegral len
>
>     esIn, esOut        :: [Double]
>     esIn                                 = map (fr . (* delta) . fromIntegral) ([0..len - 1]::[Int])
>       
>     esOut                                =
>       if useFastFourier
>         then esIn
>         else fftDoubles esIn "toTimeDomain"
>     esOutLen                             = length esOut
>
>     amp                                  = 1 -- WOX dividebyzero   1 / maximum esOut
>     
>     fr                 :: (Double → Double)
>                                          = createFrFun dInitFc dNormQ bulgeBandWidth
>     dInitFc                              = fromIntegral ksFc
>     dNormQ                               = fromIntegral ksQ / 960
>
> memoizedComputeIR = memo2 computeIR
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
>         else toContinuousSig (convolveIR     dsigOut lowP) secsToPlay 
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
>         else convolveIR     dsigInL lowpL
>     resultR                              =
>       if useFastFourier
>         then fastConvolveFR dsigInR lowpR
>         else convolveIR     dsigInR lowpR
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
>   | traceNow trace_TDS False             = undefined
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
>     (unwords ["lengths", show len1, "and", show len2, "illegally different for multiplyDiscreteSigs"])
>     (fromRawVector
>        (unwords ["product of (", tag1, ") and (", tag2, ")"])
>        (listArray (0, zlen - 1) zipped))
>   where
>     DiscreteStats{ dsigLength = len1 }   = stats1
>     DiscreteStats{ dsigLength = len2 }   = stats2
>     zipped                               = zipWith (*) (elems vec1) (elems vec2)
>     zlen                                 = fromIntegral $ length zipped
>    
> convolveIR             :: DiscreteSig Int Double → Lowpass → DiscreteSig Int Double
> convolveIR dsigIn Lowpass{ .. }
>   | traceNot trace_CIR False             = undefined
>   | otherwise                            =
>   profess
>     (ok x1 && ok x2 && ok x3)
>     (unwords ["convolveIR-- problem with 1,2, or 3:", show (ok x1, ok x2, ok x3)])
>     dsigOut
>   where
>     dsigIR                               =
>       maybe (error "convolveIR") (fromRawVector "IR") (memoizedComputeIR lowpassKs impulseSize)
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
>       unwords ["convolveIR\n", show dsigIn
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
>     mvec                                 = memoizedComputeIR lowpassKs dsigIn'.dsigStats.dsigLength
>     vecOut             :: Array Int Double
>     vecOut                               = fromMaybe dsigIn'.dsigVec mvec
>     dsigFR                               = fromRawVector "FR" vecOut
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
>         (not (null vec) || True) -- WOX! Hack Alert!!
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
> startUp c                            = 1 + controlConvex c
> finishDown c                         = 1 + controlConvex (1 - c)

Frequency Response ====================================================================================================

x-axis: frequency in cycles/sec
y-axis: amplitude ratio normalized to 0..1

but first, the y-axis is modeled as from 0 to one PLUS the Q-induced bulge ratio
(i.e. these ys, in final step, are multiplied by 1 / (1 + bulge) to normalize them)

because the Q "bulge" magnitude is defined as (0..960) centibels ABOVE DC-gain, and we treat the DC-gain as one

second, we want to drop off (gain) -120 centibels/octave from the "cutoff"

> testFreaks             :: IO Double
> testFreaks                               = do
>   let fun                                = getFreaky (KernelSpec 10_000 0 44_100) 300
>   let xs               :: [Double]       = [fromIntegral x | x ← [0..44_100]]
>   let ys                                 = map fun xs
>   print ys
>   return 0
>
> getFreaky              :: KernelSpec → Int → Double → Double
> getFreaky ks@KernelSpec{ .. } len        = freakyResponse ks len shapes
>   where
>     stretch                              = if q == 0 then 0 else fc / 100
>     width1                               = fc - stretch / 2
>     width2                               = stretch
>     height                               = 1
>     rate                                 = -0.5 -- WOX
>
>     shapes             :: [ResponseShape]
>     shapes                               =
>       [  Block width1 height
>        , Bulge width2 height stretch
>        , Decline rate height] -- WOX
>
>     fc, q, sr, yn      :: Double
>     fc                                   = fromAbsoluteCents ksFc
>     q                                    = fromIntegral ksQ
>     sr                                   = fromIntegral ksSr   
>     yn                                   = 1 / (1 + fromCentibels q)
>     
> freakyResponse         :: KernelSpec → Int → [ResponseShape] → Double → Double
> freakyResponse KernelSpec{ .. } len shapes xIn
>   | traceNot trace_FR False              = undefined
>   | otherwise                            = ((* yn) . fryXform . modelXform . frxXform) xIn
>   where
>     fs@FrSummary{ .. }                   = foldl' doShape (FrSummary [] 0) shapes
>     FrItem{ .. }                         = fromJust $ find inVogue frsItems
>
>     fc, q, sr, yn      :: Double
>     fc                                   = fromAbsoluteCents ksFc
>     q                                    = fromIntegral ksQ
>     sr                                   = fromIntegral ksSr   
>     yn                                   = 1 / (1 + fromCentibels q)
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
>         newI           :: FrItem         =
>           FrItem
>             (Block width height)  
>             (frsDisplacement, newD)
>             id
>             (const height)
>             id
>         
>     doShape prev@FrSummary{ .. } (Bulge width height stretch)
>                                          = updateSummary prev newD2 iList
>       where
>         newD1, newD2   :: Double
>         newD1                            = frsDisplacement + width / 2
>         newD2                            = frsDisplacement + width
>
>         iList          :: [FrItem]
>         iList                            =
>           if width <= 0 || stretch <= 0 || stretch >= width
>             then []
>             else [newI1, newI2]
>         newI1, newI2   :: FrItem
>         newI1                            =
>           FrItem
>             (Bulge (width / 2) height stretch)
>             (frsDisplacement, newD1)
>             ((/ (newD1 - frsDisplacement)) . ddSubtract frsDisplacement)
>             startUp
>             id
>         newI2                            =
>           FrItem
>             (Bulge width height stretch)
>             (newD1, newD2)
>             ((/ (newD2 - newD1)) . ddSubtract newD1)
>             finishDown
>             id
>         
>     doShape prev@FrSummary{ .. } (Decline height rate)
>                                          = updateSummary prev sr [newI]
>       where
>         newI                             =
>           FrItem
>             (Decline height rate)
>             (frsDisplacement, sr)
>             (ddSubtract frsDisplacement)
>             (ddTaperOff rate height)
>             (+ height)
>
>     ddXform1d          :: Double → Double → (Double → Double)
>     ddXform1d scale offset xFrom         = scale * xFrom + offset
>
>     ddSubtract         :: Double → (Double → Double)
>     ddSubtract offset                    = ddXform1d 1 (-offset)
>
>     ddTaperOff         :: Double → Double → (Double → Double)
>     ddTaperOff height slope xFrom        = max 0 (ddXform1d slope height xFrom)
>
>     trace_FR                             = unwords ["freakyResponse", show fs, "\ndoing", show friShape]
>
> -- for testing only
> defShapes              :: [ResponseShape]
> defShapes                                = [Block 2_000 1, Bulge 50 1 5, Decline 1 77]

Type declarations ============================================================================ =========================

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
>     qqBulgeBandwidth   :: Double
>   , qqImpulseSize      :: Int
>   , qqUseFastFourier   :: Bool} deriving Show
>
> data ResponseShape =
>     Block Double Double                  -- width height
>   | Bulge Double Double Double           -- width height stretch
>   | Decline Double Double                -- rate height
>     deriving Show
>
> data FrItem =
>   FrItem {
>     friShape           :: ResponseShape
>   , friRange           :: (Double, Double)
>
>   , frxXform           :: Double → Double
>   , modelXform         :: Double → Double
>   , fryXform           :: Double → Double}
>
> instance Show FrItem where
>   show (FrItem shape range _ _ _) = unwords ["<", show shape, ">", show range]
>
> data FrSummary =
>   FrSummary {
>     frsItems           :: [FrItem]
>   , frsDisplacement    :: Double} deriving Show
>
> bulgeBandWidth                           = qqBulgeBandwidth             defD
> impulseSize                              = qqImpulseSize                defD
> useFastFourier                           = qqUseFastFourier             defD
>
> defD                   :: DiscreteSettings
> defD =
>   DiscreteSettings {
>     qqBulgeBandwidth                     = 10
>   , qqImpulseSize                        = 2_048
>   , qqUseFastFourier                     = True}

The End
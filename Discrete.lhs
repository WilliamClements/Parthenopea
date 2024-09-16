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
> import qualified Codec.Wav               as W
> import Control.Arrow
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Complex
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List ( foldl', iterate', find )
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, mapMaybe )
> import Data.MemoTrie
> import qualified Data.Vector.Unboxed     as VU
> import Debug.Trace ( traceIO, trace )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..), collapse )
> import FRP.UISF.AuxFunctions ( delay )
> import Numeric.FFT ( fft, ifft )
> import Parthenopea
  
Discrete approach =====================================================================================================

> computeFR          :: KernelSpec → DiscreteSig (Complex Double)
> computeFR ks@KernelSpec{ .. }
>   | traceNow trace_CFR False             = undefined
>   | otherwise                            =
>   profess
>     ((ksLen > 0) && not (null ys'))
>     (unwords ["computeFR bad input"])
>     (fromRawVector tag' vec')
>   where
>     trace_CFR                            = unwords ["computeFR", show ks, show shapes]
>
>     kd                                   = calcKernelData ks
>     shapes                               = makeShapes ResponseNormal kd
>
>     -- function's domain corresponds to 0..ksLen-1 
>     -- there is coverage on whole target buffer
>     -- but internally, freaks above nyq will go as negative
>     ys, ys'            :: [Complex Double]
>     ys                                   = map (getFreaky kd shapes . fromIntegral) ([0..ksLen - 1]::[Int])
>
>     -- capture the Frequency Response; store the magnitudes (amplification/attenuation per freak)
>     -- slow path then runs it through fft to create Impulse Response
>     ys'                                 = if ksFast then ys    else toTimeDomain ys
>     tag'                                = if ksFast then "FR!" else "IR!"
>     vec'                                = VU.fromList ys'
>
> memoizedComputeFR = memo computeFR
>
> applyConvolutionMono   :: ∀ p . Clock p ⇒ Lowpass → Double → Signal p () Double → Signal p () Double                 
> applyConvolutionMono lowP secsToPlay sIn
>   | traceIf trace_AC False               = undefined
>   | otherwise                            = sig 
>   where
>     dsigIn             :: Maybe (DiscreteSig Double)
>     dsigIn                               = fromContinuousSig "input mono" secsToPlay sIn
>
>     sig                                  = maybe sIn doIt dsigIn
>
>     doIt               :: DiscreteSig Double → Signal p () Double
>     doIt dsig                            =
>       if lowP.lowpassKs.ksFast
>         then toContinuousSig (fastConvolveFR dsig lowP)
>         else toContinuousSig (slowConvolveIR dsig lowP) 
>
>     trace_AC                             = unwords ["applyConvolutionMono", show lowP]
>
> applyConvolutionStereo :: ∀ p . Clock p ⇒
>                           (Lowpass, Lowpass)
>                           → Double
>                           → Signal p () (Double, Double)
>                           → Signal p () (Double, Double)
> applyConvolutionStereo (lowpL, lowpR) secsToPlay sIn
>   | traceNot trace_AC False              = undefined
>   | otherwise                            = toContinuousSig' resultL resultR
>   where
>     baseLen            :: Int            = truncate (secsToPlay * rate (undefined :: p))
>     fftLen                               = sampleUp baseLen
>     
>     pairs                                = toFftSamples fftLen sIn
>
>     dsigInL                              = fromRawVector "input left"  $ VU.map fst pairs
>     dsigInR                              = fromRawVector "input right" $ VU.map snd pairs
>
>     (resultL, resultR)
>       | disableConvo                     = (dsigInL, dsigInR)
>       | lowpL.lowpassKs.ksFast           = (fastConvolveFR dsigInL lowpL, fastConvolveFR dsigInR lowpR)
>       | otherwise                        = (slowConvolveIR dsigInL lowpL, slowConvolveIR dsigInR lowpR)
>
>     trace_AC                             =
>       unwords ["applyConvolutionStereo", show baseLen, show fftLen, "\ndsigInL:", show dsigInL, "\ndsigInR:", show dsigInR]
>
> fromContinuousSig'     :: ∀ p. Clock p ⇒
>                           String → Double → Signal p () (Double, Double)
>                           → Maybe (DiscreteSig Double, DiscreteSig Double)
> fromContinuousSig' tag dur sf            = 
>   if not (null dlist)
>     then Just (fromRawVector tag (VU.fromList (map fst dlist)), fromRawVector tag (VU.fromList (map snd dlist)))
>     else Nothing
>   where
>     dlist              :: [(Double, Double)]
>     dlist                                = toSamples dur sf
>
> fromContinuousSig      :: ∀ p a. (Clock p) ⇒
>                           String → Double → Signal p () Double → Maybe (DiscreteSig Double)
> fromContinuousSig tag dur sf             = 
>   if not (null dlist)
>     then Just $ fromRawVector tag (VU.fromList dlist)
>     else Nothing
>   where
>     dlist              :: [Double]
>     dlist                                = toSamples dur sf
>
> toContinuousSig         :: ∀ p a. (Clock p, Coeff a, VU.Unbox a) ⇒ DiscreteSig a → Signal p () a
> toContinuousSig dsL                    =
>   proc ()                              → do
>     rec
>       ii' ← delay 0                    ⤙ ii
>       let ii                           = ii' + 1
>     outA                               ⤙ dsL.dsigVec VU.! (ii' `mod` dsL.dsigStats.dsigLength)
>
> toContinuousSig'        :: ∀ p a. (Clock p, Coeff a, VU.Unbox a) ⇒ DiscreteSig a → DiscreteSig a → Signal p () (a, a)
> toContinuousSig' dsL dsR                 =
>   proc ()                              → do
>     rec
>       ii' ← delay 0                    ⤙ ii
>       let ii                           = ii' + 1
>     outA                               ⤙ (  dsL.dsigVec VU.! (ii' `mod` dsL.dsigStats.dsigLength)
>                                            , dsR.dsigVec VU.! (ii' `mod` dsR.dsigStats.dsigLength))
>
> fromRawVector          :: (Coeff a, VU.Unbox a) ⇒ String → VU.Vector a → DiscreteSig a
> fromRawVector tag vec                    = DiscreteSig tag (measureDiscreteSig vec) vec
>
> measureDiscreteSig     :: (Coeff a, VU.Unbox a) ⇒ VU.Vector a → DiscreteStats a
> measureDiscreteSig vec                   = finished
>   where
>     len                :: Double         =
>       profess
>         (not $ VU.null vec)
>         (unwords ["measureDiscreteSig: discrete signal cannot be empty"])
>         (fromIntegral $ VU.length vec )
>
>     folded                               = VU.foldl' sfolder defDiscreteStats vec
>     finished                             = folded {stVariance = ascale (1/(len-1)) folded.stVariance}
>
>     sfolder            ::  ∀ a. (Coeff a) ⇒ DiscreteStats a → a → DiscreteStats a
>     sfolder stats@DiscreteStats{ .. } d  =
>       DiscreteStats
>         (dsigLength + 1)
>         (stNumZeros + if aamp d < epsilon then 1 else 0)
>         (stNZ16th   + if aamp d < epsilon && dsigLength < 512 then 1 else 0)
>         (aadd stDCOffset         d)
>         (aadd stVariance         (amul d d))
>         (max  stMaxAmp           (aamp d))
>
> measureFrequencyResponse
>                        :: forall a. (Coeff a, VU.Unbox a) ⇒ VU.Vector a → FrequencyResponseStats
> measureFrequencyResponse                 = VU.foldl' sfolder defFrequencyResponseStats
>   where
>     sfolder            ::  FrequencyResponseStats → a → FrequencyResponseStats
>     sfolder FrequencyResponseStats{ .. } d
>                                          =
>       FrequencyResponseStats
>         (accommodate stRealExtent (realPart (acomplex d)))
>         (accommodate stImagExtent (imagPart (acomplex d)))
>
> slowConvolveIR         :: DiscreteSig Double → Lowpass → DiscreteSig Double
> slowConvolveIR dsigIn Lowpass{ .. }
>   | traceNow trace_SCIR False            = undefined
>   | otherwise                            =
>   profess
>     (ok x1 && ok x2 && ok x3 && sane dsigOut)
>     (unwords ["slowConvolveIR-- problem with 1,2, or 3"])
>     dsigOut
>   where
>     cdsigIn            :: DiscreteSig (Complex Double)
>     cdsigIn                              =
>       fromRawVector ("widen " ++ dsigTag dsigIn) $ VU.map (:+ 0) (dsigVec dsigIn)
>
>     cdsigIR            :: DiscreteSig (Complex Double)
>     cdsigIR                              =
>       memoizedComputeFR lowpassKs{ksLen = VU.length (dsigVec cdsigIn), ksSr = 44_100}
>
>     x1, x2, x3         :: VU.Vector (Complex Double)
>     x1                                   = dsigVec cdsigIn
>     x2                                   = dsigVec cdsigIR
>
>     m1                                   = VU.length x1 - 1
>     m2                                   = VU.length x2 - 1
>     m3                                   = m1 + m2 + 1
>
>     x3                                   =
>       VU.fromList [ sum [ x1 VU.! k * x2 VU.! (n-k) | k ← [max 0 (n-m2)..min n m1] ] | n ← [0..m3] ]
>     dsigOut                              = fromRawVector "slowConvolveIR" (VU.map realPart x3)
>
>     ok vec                               = VU.length vec > 0
>
>     trace_SCIR                           =
>       unwords ["slowConvolveIR\n", show dsigIn
>              , "\n X \n", show cdsigIR
>              , "\n = \n", show dsigOut]
>
> fastConvolveFR         :: DiscreteSig Double → Lowpass → DiscreteSig Double
> fastConvolveFR dsigIn Lowpass{ .. }
>   | traceNot trace_FCFR False            = undefined
>   | otherwise                            =
>   profess
>     (sane dsigOut')
>     (unwords ["fastConvolveFR-- insane result"])
>     dsigOut'
>   where
>     dsigIn'                              = if correctDCOffset
>                                              then subtractDCOffset dsigIn
>                                              else dsigIn
>     dsigOut'                             = if correctDCOffset
>                                              then subtractDCOffset dsigOut
>                                              else dsigOut
>     dsigOut                              =
>       fromRawVector
>         (unwords["toTime.product(", fst tags, "&", snd tags, ")"])
>         (if reverseSignal
>           then VU.reverse (VU.fromList (map realPart result))
>           else VU.fromList (map realPart result))
>
>     cdubsIn            :: [Complex Double]
>     cdubsIn                              = toFrequencyDomain $ VU.toList $ dsigVec dsigIn'
>     len1               :: Int            = length cdubsIn
>
>     cdsigIn            :: DiscreteSig (Complex Double)
>     cdsigIn                              =
>       fromRawVector ("toFreak " ++ dsigTag dsigIn') (VU.fromList cdubsIn)
>
>     cdsigFR            :: DiscreteSig (Complex Double)
>     cdsigFR                              = memoizedComputeFR lowpassKs{ksLen = length cdubsIn, ksSr = 44_100}
>
>     tags                                 = (dsigTag cdsigIn, dsigTag cdsigFR)
>     vecs                                 = (dsigVec cdsigIn, dsigVec cdsigFR)
>     lens                                 = BF.bimap VU.length VU.length vecs
>     lensOK                               = uncurry (==) lens
>
>     product            :: VU.Vector (Complex Double)
>     product                              =
>       profess
>         lensOK
>         (unwords ["multiplicand lengths incompatible", show lens])
>         uncurry (VU.zipWith (*)) vecs
>
>     result             :: [Complex Double]
>     result                               =
>       if disableMultiply
>         then toTimeDomain cdubsIn
>         else toTimeDomain $ VU.toList product
>
>     trace_FCFR                           =
>       unwords ["fastConvolveFR\n", show cdsigIn
>                                  , show cdsigFR
>                                  , show dsigOut]
>
> toFrequencyDomain      :: forall a. Coeff a ⇒ [a] → [Complex Double]
> toFrequencyDomain                        = doFft fft
>
> toTimeDomain           :: forall a. Coeff a ⇒ [a] → [Complex Double]
> toTimeDomain                             = doFft ifft
>
> doFft                  :: forall a. Coeff a ⇒
>                           ([Complex Double] → [Complex Double]) → [a] → [Complex Double]
> doFft fftFun as                          = fftFun cds
>   where
>     inLen                                = length as
>     newLen                               = if chopSignal
>                                              then sampleDown inLen
>                                              else sampleUp inLen
>     cds                                  = map acomplex as ++ replicate (newLen - inLen) 0
>
> interpVal              :: Double → Array Int Double → Int → Double
> interpVal fact vSource ixAt          =
>   profess
>     (ix0 < (snd . bounds) vSource)
>     (unwords ["out of range (interpVal)"])
>     (dFirst + (dSecond - dFirst) * (dixAt - fromIntegral ix0))
>   where
>     dixAt              :: Double         = fromIntegral ixAt / fact
>     ix0                :: Int            = truncate dixAt
>     dFirst             :: Double         = vSource ! ix0
>     dSecond            :: Double         = if ix0 == (snd . bounds) vSource
>                                              then dFirst
>                                              else vSource ! (ix0 + 1)

Frequency Response ====================================================================================================

x-axis: frequency in cycles/sec 0..nyq
y-axis: amplitude ratio normalized to 0..1

but first, the y-axis is modeled as from 0 to one PLUS the Q-induced bulge ratio
(i.e. these ys, in final step, are multiplied by 1 / (1 + bulge) to normalize them)

because the Q "bulge" magnitude is defined as (0..960) centibels ABOVE DC-gain, and we here treat the DC-gain as one

second, we want to "gain" -120 centibels/octave from the "cutoff"

> getFreaky              :: KernelData → [ResponseShape] → Double → Complex Double
> getFreaky kd shapes xIn                         =
>   profess
>     (xIn >= 0)
>     (unwords ["failure to contain negative frequencies"])
>     (freakyResponse kd shapes xIn)
>
> makeShapes             :: ResponseStrategy → KernelData → [ResponseShape]
> makeShapes rs KernelData{ .. }           =
>   if ResponseAllPass == rs
>     then [Block]
>     else [Block, Bulge, Decline]

Each of the shape types has a driver to "get down to business"
Each driver specifies an xform composed of functions from Double to Double

        rough concept
        =============
        factor out negative freaks by "copying" magnitude (not phase) from corresponding (careful!) positive freak
        map x values (0 <= x <= nyquist) into a "model space"
        map within the model space
        map into the y-axis (but before multiplying by ynorm)

> freakyResponse         :: KernelData → [ResponseShape] → Double → Complex Double
> freakyResponse KernelData{ .. } shapes xIn_
>                                          = mkPolar mag phase
>   where
>     phase                                = if xIn_ <= kdNyq
>                                              then 3*pi/2
>                                              else pi/2
>     xIn                                  = if xIn_ <= kdNyq
>                                              then xIn_
>                                              else kdNyq - xIn_
>
>     fritems                              = foldl' doShape [] shapes
>     fritems'                             = dropWhile past fritems
>     FrItem{ .. }                         =
>       profess
>         (not $ null fritems')
>         (unwords ["xIn", show xIn, "out of range (FrItem)", show fritems])
>         (head fritems')
>
>     mag'                                 = notracer "mag" mag
>     mag                                  =
>       profess
>         (xIn <= kdNyq)
>         (unwords ["xIn", show xIn, "out of range (mag)", show fritems])
>         (notracer "mag" $ ((* ynorm) . friCompute) xIn)
>
>     ynorm, height              :: Double
>     ynorm                                = 1 / (1 + kdEQ)
>     height                               = 1
>     
>     past               :: FrItem → Bool
>     past FrItem{ .. }                    = xIn > friTrans
>
>     doShape            :: [FrItem] → ResponseShape → [FrItem]
>     doShape fritems Block                = fritems ++ [newI]
>       where
>         newD           :: Double         = kdLeftOfBulge
>         newI           :: FrItem         = FrItem newD (const height)
>         
>     doShape fritems Bulge                = fritems ++ iList
>       where
>         newD                             = kdRightOfBulge
>         iList          :: [FrItem]       = if kdStretch == 0 then [] else [newI1, newI2]
>
>         newI1, newI2   :: FrItem
>         newI1                            =
>           FrItem
>             kdFc
>             (ddLinear2 kdEQ height . startUp . ddNorm2 kdLeftOfBulge kdFc)
>         newI2                            =
>           FrItem
>             kdRightOfBulge
>             (ddLinear2 kdEQ height . finishDown . ddNorm2 kdFc kdRightOfBulge)
>         
>     doShape fritems Decline              = fritems ++ [newI]
>       where
>         newD                             = kdNyq
>         newI                             =
>           FrItem
>             newD
>             xformDecline
>         xformDecline   :: Double → Double
>         xformDecline                     =
>             notracer "fromCentibels"     . fromCentibels
>           . notracer "ddLinear2"         . ddLinear2 (-dropoffRate) (toCentibels height)
>           . notracer "delta"             . flip (-) (logBase 2 kdFc)
>           . notracer "logBase 2"         . logBase 2
>           . notracer "xIn"
>          
> ddNorm2                :: Double → Double → (Double → Double)
> ddNorm2 dLeft dRight xIn                 = (xIn - dLeft) / (dRight - dLeft)
>
> ddLinear2              :: Double → Double → (Double → Double)
> ddLinear2 m b xIn                        = b + m * xIn
>
> startUp                                  = controlConvex
> finishDown c                             = controlConvex (1 - c)

WAV ===================================================================================================================

> importWav'              :: FilePath → IO (DiscreteSig Double)
> importWav' fp = do
>   x ← W.importFile fp
>   case x of
>     Left z             → error z
>     Right w            → return $ discretizeWav w
>
> discretizeWav             :: A.Audio Int32 → DiscreteSig Double
> discretizeWav wav                        =
>   let
>     bookie = elems $ A.sampleData wav
>
>     kookie             :: VU.Vector Double
>     kookie                               = VU.fromList (map ((/ 4_294_967_296) . fromIntegral) bookie)
>   in
>     fromRawVector "fromWAV" kookie

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
>   } deriving (Eq, Show)
> lowpassFc, lowpassQ    :: Lowpass → Double
> lowpassFc lp                             = fromAbsoluteCents lp.lowpassKs.ksFc -- (ksFc $ lowpassKs lp)
> lowpassQ lp                              = fromIntegral      (ksQ  $ lowpassKs lp)
>
> data KernelData                          =
>   KernelData {
>     kdLen              :: Double         -- from ksLen
>   , kdFc               :: Double         -- filter cutoff (frequency)
>   , kdEQ               :: Double         -- effective Q (centibels)
>   , kdNyq              :: Double         -- half-length of the FR (or IR)
>   , kdStretch          :: Double         -- bandwidth of center frequency (range)
>   , kdSpread           :: Int            -- bandwidth of show-able range
>   , kdLeftOfBulge      :: Double
>   , kdRightOfBulge     :: Double
>   } deriving Show
> calcKernelData         :: KernelSpec → KernelData
> calcKernelData KernelSpec{ .. }          =
>   KernelData
>     (fromIntegral ksLen)
>     fc
>     effectiveQ
>     (fromIntegral ksLen / 2)
>     stretch
>     (round $ stretch * 2)
>     (fc - stretch / 2)
>     (fc + stretch / 2)
>   where
>     fc, effectiveQ, stretch, fudge4
>                        :: Double
>     fudge4                               = 4
>     fc                                   = fromAbsoluteCents ksFc
>     effectiveQ                           = fromCentibels (fromIntegral ksQ) / fudge4
>     stretch                              = if ksQ == 0 then 0 else fc / bulgeDiv
>
> data DiscreteStats a                     =
>   DiscreteStats {
>     dsigLength         :: Int
>     , stNumZeros       :: Int
>     , stNZ16th         :: Int
>     , stDCOffset       :: a
>     , stVariance       :: a
>     , stMaxAmp         :: Double} deriving Show
> defDiscreteStats       :: (Coeff a, VU.Unbox a) ⇒ DiscreteStats a
> defDiscreteStats                         = DiscreteStats 0 0 0 azero azero 0
> data FrequencyResponseStats              =
>   FrequencyResponseStats {
>       stRealExtent     :: (Double, Double)
>     , stImagExtent     :: (Double, Double)} deriving Show
> defFrequencyResponseStats
>                        :: FrequencyResponseStats
> defFrequencyResponseStats                =
>   FrequencyResponseStats (upsilon, -upsilon) (upsilon, -upsilon)
> data DiscreteSig a                       =
>   DiscreteSig {
>     dsigTag            :: String
>     , dsigStats        :: DiscreteStats a
>     , dsigVec          :: VU.Vector a}
> instance forall a. (Show a, Coeff a, VU.Unbox a) ⇒ Show (DiscreteSig a) where
>   show                 :: DiscreteSig a → String
>   show DiscreteSig{ .. }                 =
>     unwords ["DiscreteSig", show (dsigTag, dsigStats, measureFrequencyResponse dsigVec)]
> sane                   :: (Coeff a, VU.Unbox a) ⇒ DiscreteSig a → Bool
> sane dsig                                =
>   profess
>     (maxAmp < upsilon)
>     (unwords ["insanely large amplitude", show maxAmp])
>     True
>   where
>     maxAmp                               = dsig.dsigStats.stMaxAmp
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
>         "only 2x2 systems are supported in ResonanceTwoPoles"
>         (head zeros, head poles)
>     (b1, b2)                         = extractCoefficients z0
>     (a1, a2)                         = extractCoefficients p0
>     b0                               = (1 + a1 + a2) / 4
>   in
>     CoeffsM2N2 b0 b1 b2 a1 a2
>   where
>     trace_BSM2N2                         = unwords ["buildSystemM2N2\n", show zeros, "\n", show poles]
>
> subtractDCOffset       :: DiscreteSig Double → DiscreteSig Double
> subtractDCOffset dIn                     =
>   fromRawVector (dsigTag dIn) (VU.map (\x → x - dIn.dsigStats.stDCOffset) dIn.dsigVec)
>
> data ResponseShape                       =
>     Block
>   | Bulge
>   | Decline
>     deriving (Eq, Show)
>
> data ResponseStrategy                    =
>     ResponseNormal
>   | ResponseAllPass
>     deriving (Eq, Show)
>
> data FrItem                              =
>   FrItem {
>     friTrans           :: Double
>   , friCompute         :: Double → Double}
>
> instance Show FrItem where
>   show (FrItem startPoint _)             = unwords ["FrItem", show startPoint]
>
> data DiscreteSettings =
>   DiscreteSettings {
>     qqBulgeDiv         :: Double
>   , qqDropoffRate      :: Double
>   , qqReverseSignal    :: Bool
>   , qqDisableConvo     :: Bool
>   , qqDisableMultiply  :: Bool
>   , qqUseFastFourier   :: Bool
>   , qqCorrectDCOffset  :: Bool
>   , qqChopSignal       :: Bool} deriving Show
>
> bulgeDiv                                 = qqBulgeDiv                   defD
> -- what fraction of cutoff freak affected by nonzero Q
> dropoffRate                              = qqDropoffRate                defD
> -- how many centibels per octave to reduce magnitude after cutoff freq
> reverseSignal                            = qqReverseSignal              defD
> -- whether to reverse result to move the zeros to the end
> disableConvo                             = qqDisableConvo               defD
> -- if response type is convo, no modulation at all
> disableMultiply                          = qqDisableMultiply            defD
> -- if response type is convo, do domain conversions, but not convolution
> useFastFourier                           = qqUseFastFourier             defD
> -- False to employ convolution in time domain
> correctDCOffset                          = qqCorrectDCOffset            defD
> -- experiment with neutralizing DC offset of a discrete signal
> chopSignal                               = qqChopSignal                 defD
> -- experiment with truncating, rather than padding, buffer, to power of two size
>
> defD                   :: DiscreteSettings
> defD =
>   DiscreteSettings {
>     qqBulgeDiv                           = 20                           -- bulge bandwidth is cutoff freak / div
>   , qqDropoffRate                        = 240                           -- centibels per octave
>   , qqReverseSignal                      = False
>   , qqDisableConvo                       = False
>   , qqDisableMultiply                    = False
>   , qqUseFastFourier                     = True
>   , qqCorrectDCOffset                    = False
>   , qqChopSignal                         = False}

The End
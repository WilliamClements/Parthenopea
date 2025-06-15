> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Discrete
William Clements
June 17, 2024

> module Parthenopea.Repro.Discrete where
>
> import qualified Codec.Wav               as W
> import Control.Arrow.Operations
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Complex
> import Data.Int ( Int32 )
> import Data.List ( foldl' )
> import Data.MemoTrie ( memo )
> import qualified Data.Vector.Unboxed     as VU
> import Diagrams.Prelude hiding ( fc )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.BasicSigFuns ( envLineSeg )
> import Euterpea.IO.Audio.Types ( AudSF, AudioSample(..), Clock(..), CtrSF, Signal)
> import Numeric.FFT ( fft, ifft )
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Chart ( Section(Section), chartPoints )
> import Parthenopea.Repro.Modulation
  
Discrete approach =====================================================================================================

> computeFR          :: KernelSpec → DiscreteSig (Complex Double)
> computeFR ks                             =
>   profess
>     ((ks.ksLen > 0) && not (null ys'))
>     (unwords [fName, "bad input", show ks.ksLen, show ys'])
>     (fromRawVector tag' vec')
>   where
>     fName                                = "computeFR"
>
>     kd                                   = calcKernelData ks
>     shapes                               = makeShapes ResponseNormal
>
>     -- function's domain corresponds to 0..ksLen-1 
>     -- there is coverage on whole target buffer
>     -- but internally, freaks above nyq will go as negative
>     ys, ys'            :: [Complex Double]
>     ys                                   = map (getFreaky kd shapes . fromIntegral) ([0..ks.ksLen - 1]::[Int])
>
>     -- capture the Frequency Response; store the magnitudes (amplification/attenuation per freak)
>     -- slow path then runs it through fft to create Impulse Response
>     ys'                                 = if ks.ksFast then ys    else toTimeDomain ys
>     tag'                                = if ks.ksFast then "FR!" else "IR!"
>     vec'                                = VU.fromList ys'
>
> memoizedComputeFR      :: KernelSpec → DiscreteSig (Complex Double)
> memoizedComputeFR = memo computeFR
>
> applyConvolutionMono   :: ∀ p . Clock p ⇒ Lowpass → Double → Signal p () Double → Signal p () Double                 
> applyConvolutionMono lowP secsToPlay sIn
>   | traceNot trace_AC False              = undefined
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
>   | traceNot trace_ACS False             = undefined
>   | otherwise                            = toContinuousSig' resultL resultR
>   where
>     baseLen, fftLen    :: Int
>     baseLen                              = truncate (secsToPlay * rate (undefined :: p))
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
>     trace_ACS                            =
>       unwords ["applyConvolutionStereo", show baseLen, show fftLen
>              , "\ndsigInL:", show dsigInL
>              , "\ndsigInR:", show dsigInR]
>
> fromContinuousSig      :: ∀ p a. (Clock p, Coeff a, VU.Unbox a, AudioSample a) ⇒
>                           String → Double → Signal p () a → Maybe (DiscreteSig a)
> fromContinuousSig tag dur sf
>   | traceNot trace_FCS False             = undefined
>   | otherwise                            =
>     if not (null dlist)
>       then Just $ fromRawVector tag (VU.fromList dlist)
>       else Nothing
>   where
>     fName                                = "fromContinuousSig"
>     trace_FCS                            = unwords [fName, show $ rate (undefined::p), show $ length dlist]
>
>     dlist              :: [a]
>     dlist                                = toSamples dur sf
>
> toContinuousSig         :: ∀ p a. (Clock p, Coeff a, VU.Unbox a) ⇒ DiscreteSig a → Signal p () a
> toContinuousSig dsL                      =
>   proc ()                                → do
>     rec
>       ii' ← delay 0                      ⤙ ii
>       let ii                             = ii' + 1
>     outA                                 ⤙ dsL.dsigVec VU.! (ii' `mod` dsL.dsigStats.dsigLength)
>
> toContinuousSig'        :: ∀ p a. (Clock p, Coeff a, VU.Unbox a) ⇒ DiscreteSig a → DiscreteSig a → Signal p () (a, a)
> toContinuousSig' dsL dsR                 =
>   proc ()                                → do
>     rec
>       ii' ← delay 0                      ⤙ ii
>       let ii                             = ii' + 1
>     outA                                 ⤙ (  dsL.dsigVec VU.! (ii' `mod` dsL.dsigStats.dsigLength)
>                                              , dsR.dsigVec VU.! (ii' `mod` dsR.dsigStats.dsigLength))
>
> fromRawVector          :: (Coeff a, VU.Unbox a) ⇒ String → VU.Vector a → DiscreteSig a
> fromRawVector tag vec                    = DiscreteSig tag (measureDiscreteSig vec) vec
>
> discretizeEnvelope     :: Double → FEnvelope → Segments → DiscreteSig Double
> discretizeEnvelope clockRate env segs
>   | traceNot trace_DE False              = undefined
>   | otherwise                            =
>   if howMany < 5 && howNegative > -0.2
>     then dsig
>     else error $ unwords [fName, "too many negative values", show (howMany, howNegative)]
>   where
>     fName                                = "discretizeEnvelope"
>     trace_DE                             = unwords [fName, show (howMany, howNegative), show dsig]
>
>     dsig@DiscreteSig{ .. }
>       | abs (clockRate - ctrRate) < epsilon
>                                          = deJust fName $ fromContinuousSig fName (targetT + minUseful) csignal
>       | abs (clockRate - audRate) < epsilon
>                                          = deJust fName $ fromContinuousSig fName (targetT + minUseful) asignal
>       | otherwise                        = error $ unwords [fName, show clockRate, "clockRate not supported"]
>
>     (targetT, _, _)                      = deJust fName env.fTargetT
>
>     nindices                             = VU.findIndices (< 0) dsigVec
>     nvalues                              = VU.map (dsigVec VU.!) nindices
>     howMany                              = VU.length nindices
>     howNegative                          = VU.sum nvalues
>
>     csignal            :: CtrSF () Double
>     csignal                              =
>       proc () → do
>         ctr ← envLineSeg segs.sAmps segs.sDeltaTs ⤙ ()
>         outA ⤙ ctr
>
>     asignal            :: AudSF () Double
>     asignal                              =
>       proc () -> do
>         aud ← envLineSeg segs.sAmps segs.sDeltaTs ⤙ ()
>         outA ⤙ aud
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
>     sfolded                              = VU.foldl' sfolder defDiscreteStats vec
>     finished                             = sfolded {stVariance = ascale (1/(len-1)) sfolded.stVariance}
>
>     sfolder            ::  ∀ a. (Coeff a) ⇒ DiscreteStats a → a → DiscreteStats a
>     sfolder DiscreteStats{ .. } d  =
>       DiscreteStats
>         (dsigLength + 1)
>         (stNumZeros + if aamp d < epsilon then 1 else 0)
>         (stNZ16th + if aamp d < epsilon && dsigLength < 512 then 1 else 0)
>         (aadd stDCOffset d)
>         (aadd stVariance (amul d d))
>         (max stMaxAmp (aamp d))
>
> measureFrequencyResponse
>                        :: ∀ a. (Coeff a, VU.Unbox a) ⇒ VU.Vector a → FrequencyResponseStats
> measureFrequencyResponse                 = VU.foldl' sfolder defFrequencyResponseStats
>   where
>     sfolder            :: FrequencyResponseStats → a → FrequencyResponseStats
>     sfolder FrequencyResponseStats{ .. } d
>                                          =
>       FrequencyResponseStats
>         (accommodate stRealExtent (realPart (acomplex d)))
>         (accommodate stImagExtent (imagPart (acomplex d)))
>
> slowConvolveIR         :: DiscreteSig Double → Lowpass → DiscreteSig Double
> slowConvolveIR dsigIn Lowpass{ .. }
>   | traceNot trace_SCIR False            = undefined
>   | otherwise                            =
>   profess
>     (ok x1 && ok x2 && ok x3 && sane dsigOut)
>     (unwords [fName, "- problem with 1,2, or 3"])
>     dsigOut
>   where
>     fName                                = "slowConvolveIR"
>
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
>     dsigOut                              = fromRawVector fName (VU.map realPart x3)
>
>     ok vec                               = VU.length vec > 0
>
>     trace_SCIR                           =
>       unwords [fName, "\n", show dsigIn
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
>     fName                                = "fastConvolveFR"
>
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
>     counts                               = BF.bimap VU.length VU.length vecs
>
>     vprod              :: VU.Vector (Complex Double)
>     vprod                                =
>       profess
>         (uncurry (==) counts)
>         (unwords ["multiplicand lengths incompatible", show counts])
>         uncurry (VU.zipWith (*)) vecs
>
>     result             :: [Complex Double]
>     result                               =
>       if disableMultiply
>         then toTimeDomain cdubsIn
>         else toTimeDomain $ VU.toList vprod
>
>     trace_FCFR                           =
>       unwords [fName, "\n", show cdsigIn
>                           , show cdsigFR
>                           , show dsigOut]
>
> toFrequencyDomain      :: ∀ a. Coeff a ⇒ [a] → [Complex Double]
> toFrequencyDomain                        = doFft fft
>
> toTimeDomain           :: ∀ a. Coeff a ⇒ [a] → [Complex Double]
> toTimeDomain                             = doFft ifft
>
> doFft                  :: ∀ a. Coeff a ⇒ ([Complex Double] → [Complex Double]) → [a] → [Complex Double]
> doFft fftFun as                          = fftFun cds
>   where
>     inLen                                = length as
>     newLen                               = if chopSignal
>                                              then sampleDown inLen
>                                              else sampleUp inLen
>     cds                                  = map acomplex as ++ replicate (newLen - inLen) 0
>
> interpVal              :: Double → Array Int Double → Int → Double
> interpVal fact vSource ixIn          =
>   profess
>     (ix0 < (snd . bounds) vSource)
>     (unwords ["out of range (interpVal)"])
>     (dFirst + (dSecond - dFirst) * (dixAt - fromIntegral ix0))
>   where
>     dixAt              :: Double         = fromIntegral ixIn / fact
>     ix0                :: Int            = truncate dixAt
>     dFirst             :: Double         = vSource ! ix0
>     dSecond            :: Double         = if ix0 == (snd . bounds) vSource
>                                              then dFirst
>                                              else vSource ! (ix0 + 1)

Frequency Response ====================================================================================================

x-axis: frequency in cycles/sec 0..nyq
y-axis: amplitude ratio normalized to 0..1

but first, the y-axis is modeled as from 0 to one PLUS the Q-induced bulge ratio
(therefore these ys, in final step, are multiplied by 1 / (1 + bulge) to normalize them)

because the Q "bulge" magnitude is defined as (0..960) centibels ABOVE DC-gain, and we here treat the DC-gain as one

second, we want to "gain" -120 centibels/octave from the "cutoff"

> getFreaky              :: KernelData → [ResponseShape] → Double → Complex Double
> getFreaky kd shapes xIn                         =
>   profess
>     (xIn >= 0)
>     (unwords ["failure to contain negative frequencies"])
>     (freakyResponse kd shapes xIn)
>
> makeShapes             :: ResponseStrategy → [ResponseShape]
> makeShapes rs                            =
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
>                                          = mkPolar mag ph
>   where
>     mag                                  =
>       profess
>         (xIn <= kdNyq)
>         (unwords ["xIn", show xIn, "out of range (mag)", show fritems])
>         ((* ynorm) . \x → friCompute x xIn) fritem
>     (ph, xIn)                            = if xIn_ <= kdNyq
>                                              then (3*pi/2, xIn_)
>                                              else (pi/2, kdNyq - xIn_)
>
>     fritem                               =
>       if null fritems'
>         then head fritems
>         else head fritems'
>     fritems'                             = dropWhile ((xIn <) . friTrans) fritems
>     fritems                              = foldl' doShape [] shapes
>
>     ynorm, h                   :: Double
>     ynorm                                = 1 / (1 + kdEQ)
>     h                                    = 1
>     
>     doShape            :: [FrItem] → ResponseShape → [FrItem]
>     doShape oldI Block                   = oldI ++ [newI]
>       where
>         newD           :: Double         = kdLeftOfBulge
>         newI           :: FrItem         = FrItem newD (const h)
>         
>     doShape oldI Bulge                   = oldI ++ iList
>       where
>         iList          :: [FrItem]       = if kdStretch == 0 then [] else [newI1, newI2]
>
>         newI1, newI2   :: FrItem
>         newI1                            =
>           FrItem
>             kdFc
>             (ddLinear2 kdEQ h . startUp . ddNorm2 kdLeftOfBulge kdFc)
>         newI2                            =
>           FrItem
>             kdRightOfBulge
>             (ddLinear2 kdEQ h . finishDown . ddNorm2 kdFc kdRightOfBulge)
>         
>     doShape oldI Decline                 = oldI ++ [newI]
>       where
>         newD                             = kdNyq
>         newI                             =
>           FrItem
>             newD
>             (fromCentibels
>              . ddLinear2 (-dropoffRate) (toCentibels h)
>              . flip (-) (logBase 2 kdFc)
>              . logBase 2)
>          
> ddNorm2                :: Double → Double → (Double → Double)
> ddNorm2 dLeft dRight xIn                 = (xIn - dLeft) / (dRight - dLeft)
>
> ddLinear2              :: Double → Double → (Double → Double)
> ddLinear2 m b xIn                        = b + m * xIn
>
> startUp                :: Double → Double
> startUp                                  = controlConvex
> finishDown             :: Double → Double
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

> data KernelData                          =
>   KernelData {
>     kdLen              :: Double         -- from ksLen
>   , kdFc               :: Double         -- filter cutoff (frequency)
>   , kdEQ               :: Double         -- effective Q (centibels)
>   , kdNyq              :: Double         -- half-length of the FR (or IR)
>   , kdStretch          :: Double         -- bandwidth of center frequency (range)
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
>     kStretch
>     (fc - kStretch / 2)
>     (fc + kStretch / 2)
>   where
>     fc, effectiveQ, kStretch, fudge4
>                        :: Double
>     fudge4                               = 4
>     fc                                   = fromAbsoluteCents ksFc
>     effectiveQ                           = fromCentibels (fromIntegral ksQ) / fudge4
>     kStretch                             = if ksQ == 0 then 0 else fc / bulgeDiv
>
> data DiscreteStats a                     =
>   DiscreteStats {
>     dsigLength         :: Int
>     , stNumZeros       :: Int
>     , stNZ16th         :: Int
>     , stDCOffset       :: a
>     , stVariance       :: a
>     , stMaxAmp         :: Double} deriving Show
> defDiscreteStats       :: (Coeff a) ⇒ DiscreteStats a
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
> instance ∀ a. (Show a, Coeff a, VU.Unbox a) ⇒ Show (DiscreteSig a) where
>   show                 :: DiscreteSig a → String
>   show DiscreteSig{ .. }                 =
>     unwords ["DiscreteSig", show (dsigTag, dsigStats, measureFrequencyResponse dsigVec)]
> sane                   :: (Coeff a) ⇒ DiscreteSig a → Bool
> sane dsig                                =
>   profess
>     (stMaxAmp < upsilon)
>     (unwords ["insanely large amplitude", show stMaxAmp])
>     True
>   where
>     DiscreteStats{ .. }                  
>                                          = dsig.dsigStats
> chartDiscreteSig       :: Double → Int → DiscreteSig Double → String → IO ()
> chartDiscreteSig clockRate nPoints dsig tag
>                                          = chartPoints tag [sec]
>   where
>     sec                                  = Section (opaque blue) (zip xs ys)
>     xs, ys             :: [Double]
>     xs                                   = map ((/ clockRate) . fromIntegral) [0::Int ..]
>     ys                                   = take nPoints (VU.toList dsig.dsigVec)
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
> bulgeDiv, dropoffRate  :: Double
> reverseSignal, disableConvo, disableMultiply, correctDCOffset, chopSignal
>                        :: Bool
>
> -- what fraction of cutoff freak affected by nonzero Q (cutoff freak / div)
> bulgeDiv                                 = 20
> dropoffRate                              = 240
> -- how many centibels per octave to reduce magnitude after cutoff freq
> reverseSignal                            = True
> -- whether to reverse result to move the zeros to the end
> disableConvo                             = False
> -- if response type is convo, no modulation at all
> disableMultiply                          = False
> -- if response type is convo, do domain conversions, but not convolution
> correctDCOffset                          = False
> -- experiment with neutralizing DC offset of a discrete signal
> chopSignal                               = False
> -- experiment with truncating, rather than padding, buffer, to power of two size

The End
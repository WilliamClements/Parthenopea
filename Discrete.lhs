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
> import HSoM.Examples.FFT
> import Numeric.FFT ( fft, ifft )
> import Parthenopea
  
Discrete approach =====================================================================================================

> computeFR          :: KernelSpec → DiscreteSig (Complex Double)
> computeFR ks@KernelSpec{ .. }
>   | traceIf trace_CIR False               = undefined
>   | otherwise                             =
>   profess
>     ((ksLen > 0) && not (null frs'))
>     (unwords ["computeFR bad input"])
>     (fromRawVector tag' (VU.fromList frs'))
>   where
>     trace_CIR                            =
>       unwords ["computeFR", show ksLen, "delta", show delta, show $ length frs, show $ length frs']
>
>     -- set fudge3 to populate FR : 1 for full range, 2 for up to nyquist only
>     fudge3             :: Double         = 1
>
>     -- calculate the bin width + list all the target freaks
>     delta              :: Double         = fromIntegral ksSr / (fudge3 * fromIntegral ksLen)
>     freaks             :: [Double]       = map ((* delta) . fromIntegral) ([0..ksLen - 1]::[Int])
>
>     -- capture the Frequency Response; stores the magnitudes (so sound of those frequencies 
>     -- are amplified/attenuated) into a list -- slow path runs it first through fft
>     frs, frs'          :: [Complex Double]
>     frs                                 = map ((:+ 0) . getFreaky ks) freaks
>     frs'                                = if ksFast then frs   else toTimeDomain frs
>     tag'                                = if ksFast then "FR!" else "IR!"
>
> memoizedComputeIR = memo computeFR
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
>   | traceIf trace_AC False               = undefined
>   | otherwise                            = toContinuousSig pairs'
>   where
>     pairs                                = toSamples secsToPlay sIn
>     kN                                   = length pairs
>
>     dsigInL                              = fromRawVector "input left"  $ VU.fromList (map fst pairs)
>     dsigInR                              = fromRawVector "input right" $ VU.fromList (map snd pairs)
>
>     (resultL, resultR)                   =
>       if lowpL.lowpassKs.ksFast
>         then (fastConvolveFR dsigInL lowpL, fastConvolveFR dsigInR lowpR)
>         else (slowConvolveIR dsigInL lowpL, slowConvolveIR dsigInR lowpR)
>
>     pairs'                               =
>       fromRawVector
>       "convolved"
>       (VU.zip (VU.slice 0 kN $ dsigVec resultL)
>               (VU.slice 0 kN $ dsigVec resultR))
>     kN'                                  = VU.length (dsigVec pairs')
>
>     trace_AC                             =
>       unwords ["applyConvolutionStereo", show kN, show kN', "\n", show resultL, "\n", show resultR]
>
> fromContinuousSig      :: ∀ p a. (Clock p, WaveAudioSample a, VU.Unbox a) ⇒
>                           String → Double → Signal p () a → Maybe (DiscreteSig a)
> fromContinuousSig tag dur sf             = 
>   if not (null dlist)
>     then Just $ fromRawVector tag vec
>     else Nothing
>   where
>     dlist              :: [a]            = toSamples dur sf
>     vec                                  = VU.fromList dlist
>
> toContinuousSig        :: ∀ p a. (Clock p, WaveAudioSample a, VU.Unbox a, Show a) ⇒
>                           DiscreteSig a → Signal p () a
> toContinuousSig DiscreteSig{ .. }
>   | traceIf trace_MS False               = undefined
>   | otherwise                            =
>     proc ()                              → do
>       rec
>         ii' ← delay 0                    ⤙ ii
>         let ii                           = ii' + 1
>       outA                               ⤙ (VU.!) dsigVec (ii' `mod` dsigLength)
>   where
>     DiscreteStats{ .. }                  = dsigStats
>     trace_MS                             = unwords ["toContinuousSig", show dsigTag, show dsigStats]
>
> fromRawVector          :: (WaveAudioSample a, VU.Unbox a) ⇒ String → VU.Vector a → DiscreteSig a
> fromRawVector tag vec                    = DiscreteSig tag (measureDiscreteSig vec) vec
>
> measureDiscreteSig     :: (WaveAudioSample a, VU.Unbox a) ⇒ VU.Vector a → DiscreteStats a
> measureDiscreteSig vec                   = finished
>   where
>     len                :: Double         =
>       profess
>         (not $ VU.null vec)
>         (unwords ["measureDiscreteSig: discrete signal cannot be empty"])
>         (fromIntegral $ VU.length vec )
>
>     folded                               = VU.foldl' sfolder defDiscreteStats vec
>     finished                             = folded {stSquare = asqrt $ ascale (1/len) folded.stSquare}
>
>     sfolder            ::  ∀ a. (WaveAudioSample a) ⇒ DiscreteStats a → a → DiscreteStats a
>     sfolder stats@DiscreteStats{ .. } d  =
>       stats {
>              dsigLength                  = dsigLength + 1
>              , stDCOffset                = aadd stDCOffset    d
>              , stSquare                  = aadd stSquare      (amul d d)
>              , stMaxAmp                  = max  stMaxAmp      (aamp d)}
>
> slowConvolveIR         :: DiscreteSig Double → Lowpass → DiscreteSig Double
> slowConvolveIR dsigIn Lowpass{ .. }
>   | traceNot trace_CIR False             = undefined
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
>     cdsigIR                              = memoizedComputeIR lowpassKs
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
>     dsigOut                              = fromRawVector "slowConvolveIR" (VU.map finish x3)
>
>     ok vec                               = VU.length vec > 0
>
>     trace_CIR                            =
>       unwords ["slowConvolveIR\n", show dsigIn
>              , "\n X \n", show dsigIn
>              , "\n = \n", show dsigOut]
>
> fastConvolveFR         :: DiscreteSig Double → Lowpass → DiscreteSig Double
> fastConvolveFR dsigIn Lowpass{ .. }
>   | traceIf trace_FCIR False             = undefined
>   | otherwise                            =
>   profess
>     (sane dsigOut)
>     (unwords ["fastConvolveFR-- insane result"])
>     dsigOut
>   where
>     dsigOut                              = fromRawVector
>                                              (unwords["product", fst tags, "X", snd tags])
>                                              (VU.fromList (map finish product))
>
>     cdubsIn            :: [Complex Double]
>     cdubsIn                              = toFrequencyDomain $ VU.toList $ dsigVec dsigIn
>
>     cdsigIn            :: DiscreteSig (Complex Double)
>     cdsigIn                              =
>       fromRawVector ("toFreak " ++ dsigTag dsigIn) (VU.fromList cdubsIn)
>
>     cdsigFR            :: DiscreteSig (Complex Double)
>     cdsigFR                              = memoizedComputeIR lowpassKs{ksLen = length cdubsIn}
>
>     tags                                 = (dsigTag cdsigIn, dsigTag cdsigFR)
>     vecs                                 = (dsigVec cdsigIn, dsigVec cdsigFR)
>
>     product            :: [Complex Double]
>     product                             = toTimeDomain (VU.toList $ uncurry (VU.zipWith (*)) vecs)
>
>     trace_FCIR                          = unwords ["fastConvolveFR\n", show cdsigIn, "\n", show cdsigFR]
>
> finishWithMagnitudes                     = False
> finish                                   = if finishWithMagnitudes
>                                              then magnitude
>                                              else realPart
>
> toFrequencyDomain      :: forall a. WaveAudioSample a ⇒ [a] → [Complex Double]
> toFrequencyDomain                        = doFft fft
> toTimeDomain           :: forall a. WaveAudioSample a ⇒ [a] → [Complex Double]
> toTimeDomain                             = doFft ifft
>
> doFft                  :: forall a. WaveAudioSample a ⇒
>                           ([Complex Double] → [Complex Double]) → [a] → [Complex Double]
> doFft fftFun as                          = fftFun cds
>   where
>     inLen                                = length as
>     newLen                               = sampleUp inLen
>     cds                                  = map acomplex as ++ replicate (newLen - inLen) 0

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
>     KernelData{ .. }                     = calcKernelData ks
>
>     width                                = kdFc - (kdStretch / 2)
>     height                               = 1
>
>     shapes             :: [ResponseShape]
>     shapes                               =
>       [  Block width height
>        , Bulge kdStretch height
>        , Decline dropoffRate height]
>     
> freakyResponse         :: KernelSpec → [ResponseShape] → Double → Double
> freakyResponse ks@KernelSpec{ .. } shapes xIn
>   | traceNot trace_FR False              = undefined
>   | otherwise                            = ((* ynorm) . fryXform . modelXform . frxXform) xIn
>   where
>     KernelData{ .. }                     = calcKernelData ks
>
>     FrSummary{ .. }                      = foldl' doShape (FrSummary [] 0) shapes
>     FrItem{ .. }                         =
>       professIsJust
>         (find inVogue frsItems)
>         (unwords ["could not find an item??!? -- bad boundaries"])
>
>     fudge1, fudge2     :: Double
>     fudge1                               = 8
>     ynorm              :: Double
>     ynorm                                = fudge1 / (1 + kdEQ) -- WOX instead of 1
>     
>     inVogue            :: FrItem → Bool
>     inVogue FrItem{ .. }                 = xIn == clip friRange xIn
>
>     upd                :: FrSummary → Double → [FrItem] → FrSummary
>     upd prev@FrSummary{ .. } newD newIs  = prev{frsDisplacement = newD, frsItems = frsItems ++ newIs}
>
>     doShape            :: FrSummary → ResponseShape → FrSummary
>     doShape prev@FrSummary{ .. } (Block width height)
>                                          = upd prev newD [newI]
>       where
>         newD           :: Double         = frsDisplacement + width
>         newI           :: FrItem         = FrItem (frsDisplacement, newD) kdSr id (const height) id
>         
>     doShape prev@FrSummary{ .. } (Bulge width height)
>                                          = upd prev newD2 iList
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
>             kdSr
>             (ddNorm2 frsDisplacement newD1)
>             startUp
>             (ddXform1d kdEQ height)
>         newI2                            =
>           FrItem
>             (newD1, newD2)
>             kdSr
>             (ddNorm2 newD1 newD2)
>             finishDown
>             (ddXform1d kdEQ height)
>         
>     doShape prev@FrSummary{ .. } (Decline dropoff height)
>                                          = upd prev kdSr [newI]
>       where
>         newI                             =
>           FrItem
>             (frsDisplacement, kdSr)
>             kdSr
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
>     fudge2                               = 100 -- WOX instead of 2 (that represented one octave)
>
>     ddDown3            :: Double → Double → Double → (Double → Double)
>     ddDown3 fLeft rate height xFrom
>       | traceNot trace_DDD False         = undefined
>       | otherwise                        = relativeAmp fLeft height xFrom rate'
>       where
>         rate'                            = rate * xFrom / (fudge2 * fLeft)   -- adjusted centibels per octave
>                                                                              -- maybe fLeft and xFrom squared?
>         trace_DDD                        =
>           unwords ["ddDown3", show (fLeft, rate, height, xFrom)]
>
>     startUp                              = controlConvex
>     finishDown c                         = controlConvex (1 - c)
>
>     trace_FR                             = unwords ["freakyResponse", show ynorm]

WAV ===================================================================================================================

> importWav'              :: FilePath → IO (DiscreteSig Double)
> importWav' fp = do
>   x ← W.importFile fp
>   case x of
>     Left z             → error z
>     Right w            → return $ discretizeWav w
>
> discretizeWav             :: A.Audio Int32 → DiscreteSig Double
> discretizeWav wav                           =
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
> lowpassFc, lowpassQ    :: Lowpass -> Double
> lowpassFc lp                             = fromAbsoluteCents lp.lowpassKs.ksFc -- (ksFc $ lowpassKs lp)
> lowpassQ lp                              = fromIntegral      (ksQ  $ lowpassKs lp)
>
> data KernelData                          =
>   KernelData {
>     kdFc               :: Double         -- filter cutoff (frequency)
>   , kdEQ               :: Double         -- effective Q ()
>   , kdSr               :: Double         -- sample rate (frequency)
>   , kdLen              :: Double         -- length of the FR (or IR)
>   , kdStretch          :: Double         -- bandwidth of center frequency (range)
>   , kdSpread           :: Int            -- bandwidth of show-able range
>   } deriving Show
> calcKernelData         :: KernelSpec → KernelData
> calcKernelData KernelSpec{ .. }          =
>   KernelData
>     fc
>     effectiveQ
>     (fromIntegral ksSr)
>     (fromIntegral ksLen)
>     stretch
>     (round $ stretch * 2)
>   where
>     fc, effectiveQ, stretch, fudge4
>                        :: Double
>     fudge4                               = 8
>     fc                                   = fromAbsoluteCents ksFc
>     effectiveQ                           = fromCentibels (fromIntegral ksQ) / fudge4
>     stretch                              = if ksQ == 0 then 0 else fc / bulgeDiv
>
> data DiscreteStats a                     =
>   DiscreteStats {
>     dsigLength         :: Int
>     , stDCOffset       :: a
>     , stSquare         :: a
>     , stMaxAmp         :: Double} deriving Show
> defDiscreteStats       :: (WaveAudioSample a, VU.Unbox a) ⇒ DiscreteStats a
> defDiscreteStats                         = DiscreteStats 0 azero azero azero
> data DiscreteSig a                       =
>   DiscreteSig {
>     dsigTag            :: String
>     , dsigStats        :: DiscreteStats a
>     , dsigVec          :: VU.Vector a}
> instance (Show a) ⇒ Show (DiscreteSig a) where
>   show                 :: DiscreteSig a → String
>   show DiscreteSig{ .. }                 = unwords [show dsigTag, show dsigStats]
> sane                   :: (WaveAudioSample a, VU.Unbox a) ⇒ DiscreteSig a → Bool
> sane dsig                                =
>   profess
>     (maxAmp < 1_000_000)
>     (unwords ["insanely large amplitude", show maxAmp])
>     True
>   where
>     maxAmp                               = dsig.dsigStats.stMaxAmp
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
>   , qqDropoffRate                        = 60                           -- centibels per octave
>   , qqImpulseSize                        = 2_048                        -- small "default" size
>   , qqUseFastFourier                     = True}

The End
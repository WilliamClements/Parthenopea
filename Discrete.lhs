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
> import Debug.Trace ( traceIO, trace )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..), collapse )
> import FRP.UISF.AuxFunctions ( delay )
> import HSoM.Examples.FFT
> import Numeric.FFT ( fft, ifft )
> import Parthenopea
  
Discrete approach =====================================================================================================

> computeFR          :: KernelSpec → (String, [Complex Double])
> computeFR ks@KernelSpec{ .. }
>   | traceNow trace_CIR False              = undefined
>   | otherwise                             =
>   profess
>     (ksLen >= 0)
>     (unwords ["computeFR bad ksLen"])
>     (if ksFast then ("FR!", esIn) else ("IR!", ifft esIn))
>   where
>     trace_CIR                            =
>       unwords ["computeFR", show ksLen, "delta", show delta]
>
>     -- set fudge3 to populate FR : 1 for full range, 2 for up to nyquist only
>     fudge3             :: Double         = 2
>
>     delta              :: Double         = fromIntegral ksSr / (fudge3 * fromIntegral ksLen)
>
>     freaks             :: [Double]
>     freaks                               = map ((* delta) . fromIntegral) ([0..ksLen - 1]::[Int])
>
>     esIn               :: [Complex Double]
>     esIn                                 = map ((:+ 0) . getFreaky ks) freaks
>
> memoizedComputeIR = memo computeFR
>
> applyConvolutionMono   :: ∀ p . Clock p ⇒ Lowpass → Double → Signal p () Double → Signal p () Double                 
> applyConvolutionMono lowP secsToPlay sIn
>   | traceNot trace_AC False              = undefined
>   | otherwise                            = sig 
>   where
>     dsigIn             :: Maybe (DiscreteSig Double)
>     dsigIn                               = toDiscreteSig "input mono" secsToPlay sIn
>
>     sig                                  = maybe sIn doIt dsigIn
>
>     doIt               :: DiscreteSig Double → Signal p () Double
>     doIt dsig                            =
>       if useFastFourier
>         then toContinuousSigMono (fastConvolveFR dsig lowP) secsToPlay
>         else toContinuousSigMono (slowConvolveIR dsig lowP) secsToPlay 
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
>   | otherwise                            = toContinuousSigStereo resultL resultR 1
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
>         else slowConvolveIR dsigInL lowpL
>     resultR                              =
>       if useFastFourier
>         then fastConvolveFR dsigInR lowpR
>         else slowConvolveIR dsigInR lowpR
>
>     pairs'                               = zip (elems $ dsigVec resultL) (elems $ dsigVec resultR)
>     kN'                                  = length pairs'
>
>     trace_AC                             = unwords ["applyConvolutionStereo", show lowpL]
>
> toDiscreteSig          :: ∀ p. (Clock p) ⇒
>                           String → Double → Signal p () Double → Maybe (DiscreteSig Double)
> toDiscreteSig tag dur sf
>   | traceNot trace_TDS False             = undefined
>   | otherwise                            = 
>   if not (null dlist)
>     then Just $ DiscreteSig tag stats vec
>     else Nothing
>   where
>     dlist              :: [Double]       = toSamples dur sf
>     dlistLen                             = fromIntegral $ length dlist
>     bs                                   = (0, dlistLen - 1)
>     vec                                  = listArray bs dlist
>     stats                                = measureDiscreteSig vec
>
>     trace_TDS                            = unwords ["toDiscreteSig", show stats]
>
> fromRawVector          :: String → UArray Int Double → DiscreteSig Double
> fromRawVector tag vec                    = DiscreteSig tag (measureDiscreteSig vec) vec
>
> normalize, noNormalize :: UArray Int Double → UArray Int Double
> normalize vec                            = listArray (bounds vec) (map (ascale (1 / maxImpulse vec)) (elems vec))
> noNormalize vec                          = vec
> 
> maxImpulse             :: UArray Int Double → Double
> maxImpulse vec                           = maximum $ map abs (concatMap collapse (elems vec))
>
> skipMultiply = False
>
> opera                                    = if skipMultiply
>                                              then const
>                                              else (*)
>    
> slowConvolveIR         :: DiscreteSig Double → Lowpass → DiscreteSig Double
> slowConvolveIR dsigIn@DiscreteSig{ .. } Lowpass{ .. }
>   | traceNot trace_CIR False             = undefined
>   | otherwise                            =
>   profess
>     (ok x1 && ok x2 && ok x3 && sane dsigOut)
>     (unwords ["slowConvolveIR-- problem with 1,2, or 3"])
>     dsigOut
>   where
>     cdubsIn                              = map (:+ 0) (elems dsigVec)
>     m1                                   = length cdubsIn - 1
>
>     history            :: String
>     cdubsIR            :: [Complex Double]
>     (history, cdubsIR)                   = memoizedComputeIR lowpassKs
>     m2                                   = length cdubsIR - 1
>     m3                                   = m1 + m2 + 1    
>
>     x1, x2, x3                           :: Array Int (Complex Double)
>     x1                 = listArray (0, m1) cdubsIn
>     x2                                   = listArray (0, m2) cdubsIR
>     x3_                                  =
>       [ sum [ x1 ! k * x2 ! (n-k) | k ← [max 0 (n-m2)..min n m1] ] | n ← [0..m3] ]
>     x3                                   = listArray (0, m3) x3_
>     dsigOut                              = fromRawVector "slowConvolveIR" (listArray (0, m3) (map finish x3_))
>
>     ok vec                               =
>       fst (bounds vec) == 0 && snd (bounds vec) > 0
>
>     trace_CIR                            =
>       unwords ["slowConvolveIR\n", show dsigIn
>              , "\n X \n", show dsigIn
>              , "\n = \n", show dsigOut]
>
> disableFft                               = False
>
> fastConvolveFR         :: DiscreteSig Double → Lowpass → DiscreteSig Double
> fastConvolveFR dsigIn Lowpass{ .. }
>   | traceNow trace_FCIR False            = undefined
>   | otherwise                            =
>   if disableFft
>     then dsigIn
>     else dsigOut'
>   where
>     dsigOut'                             =
>       profess
>         (sane dsigOut)
>         (unwords ["fastConvolveFR-- insane result"])
>         dsigOut
>     dsigOut                              =
>       fromRawVector "FR product" (listArray (0, productLen - 1) (map finish product))
>
>     cdubsIn, product
>                        :: [Complex Double]
>     cdubsIn                              = fftDiscreteSig dsigIn "toFrequencyDomain"
>
>     history            :: String
>     esOut              :: [Complex Double]
>     (history, esOut)                     = memoizedComputeIR lowpassKs{ksLen = length cdubsIn}
>
>     product                              = ifft $ zipWith (*) cdubsIn esOut
>     productLen                           = length product
>
>     trace_FCIR                           =
>       unwords ["fastConvolveFR\n", show dsigIn
>                       , "\n = \n", show dsigOut]
>
> measureDiscreteSig     :: UArray Int Double → DiscreteStats Double
> measureDiscreteSig vec                   = stats''
>   where
>     len                :: Double         =
>       profess
>         (not (null $ assocs vec))
>         (unwords ["discrete signal cannot be empty"])
>         (fromIntegral $ snd (bounds vec) + 1)
>     stats'@DiscreteStats{ .. }           = foldl' sfolder (DiscreteStats 0 azero azero azero) (elems vec)
>     stats''                              = stats' {statsSquare = asqrt $ ascale (1/len) statsSquare}
>
>     sfolder            :: DiscreteStats Double → Double → DiscreteStats Double
>     sfolder stats@DiscreteStats{ .. } d  =
>       stats {
>              dsigLength                  = dsigLength + 1
>              , statsDCOffset             = aadd statsDCOffset d
>              , statsSquare               = aadd statsSquare (amul d d)
>              , statsMaxAmp               = amax statsMaxAmp (aabs d)}
>
> toContinuousSigMono    :: ∀ p. (Clock p) ⇒
>                           DiscreteSig Double → Double → Signal p () Double
> toContinuousSigMono dsig@DiscreteSig{ .. } amp
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
> toContinuousSigStereo  :: ∀ p. (Clock p) ⇒
>                           DiscreteSig Double → DiscreteSig Double → Double → Signal p () (Double, Double)
> toContinuousSigStereo dsig1@DiscreteSig{dsigVec = vec1} dsig2@DiscreteSig{dsigVec = vec2} amp
>   | traceNot trace_MS False              = undefined
>   | otherwise                            =
>     proc ()                              → do
>       rec
>         ii' ← delay 0                    ⤙ ii
>         let ii                           = ii' + 1
>       outA                               ⤙ ( ascale amp (vec1 ! (ii' `mod` len1))
>                                              ,ascale amp (vec2 ! (ii' `mod` len2)))
>   where
>     len1                                 = dsig1.dsigStats.dsigLength
>     len2                                 = dsig2.dsigStats.dsigLength
>     trace_MS                             = unwords ["toContinuousSig", show dsig1, show dsig2]
>
> fftDiscreteSig         :: DiscreteSig Double → String → [Complex Double]
> fftDiscreteSig DiscreteSig{ .. }         = fftDoubles (elems dsigVec)
>
> produceMagnitudes = True
> finish                                   = if produceMagnitudes
>                                              then magnitude
>                                              else realPart
>
> fftDoubles             :: [Double] → String → [Complex Double]
> fftDoubles dsIn which                    = fftfun (map (:+ 0) (dsIn ++ zeros))
>   where
>     fftfun             :: [Complex Double] → [Complex Double]
>     fftfun                               =
>       if "toFrequencyDomain" == which
>         then fft
>         else ifft
>
>     dsInLen                              = length dsIn
>     newLen                               = sampleUp dsInLen
>     zeros                                =
>       if dsInLen == newLen
>         then []
>         else replicate (newLen - dsInLen) 0.0

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
>     bookieLen = length bookie
>
>     kookie :: UArray Int Double
>     kookie = listArray (0, bookieLen - 1) (map ((/ 4_294_967_296) . fromIntegral) bookie)
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
>     , statsDCOffset    :: a
>     , statsSquare      :: a
>     , statsMaxAmp      :: a} deriving Show
> data DiscreteSig a                       =
>   DiscreteSig {
>     dsigTag            :: String
>     , dsigStats        :: DiscreteStats a
>     , dsigVec          :: UArray Int a}
> instance (Show a) ⇒ Show (DiscreteSig a) where
>   show                 :: DiscreteSig a → String
>   show DiscreteSig{ .. }                 = unwords [show dsigTag, show dsigStats]
> sane                   :: DiscreteSig Double → Bool
> sane dsig                                =
>   profess
>     (maxAmp < 1_000_000)
>     (unwords ["insane amplitude", show maxAmp])
>     True
>   where
>     maxAmp                               = dsig.dsigStats.statsMaxAmp
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
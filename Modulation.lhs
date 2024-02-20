> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module Modulation where
>
> import Data.Array
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Bits
> import Data.Graph (Graph)
> import qualified Data.Graph              as Graph
> import Data.List ( foldl', iterate', find )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, mapMaybe )
> import Debug.Trace ( traceIO )
> import Euterpea ( (<<<), (>>>) )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..) )
> import Euterpea.Music ( Volume, AbsPitch, Dur, absPitch, PitchClass(..) )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import HSoM.Examples.Additive ( sineTable, sfTest1 )
> import Parthenopea
  
Modulator management ==================================================================================================

> resolveMods            :: Modulation → [Modulator] → [Modulator] → Modulation
> resolveMods m8n m8rs dm8rs               = m8n{modGraph = compileMods checked}
>   where
>     sifted                               = siftMods (dm8rs ++ m8rs)
>     checked                              = profess
>                                              (not $ hasCycles sifted)
>                                              "cycles in modulator graph"
>                                              sifted
>         
> hasCycles              :: [Modulator] → Bool
> hasCycles m8rs                           = not $ null $ cyclicNodes $ makeGraph edgeList
>   where
>     nodeList           :: [(ModDestType, [Modulator])]
>                                          = filter (isJust . outGoing . fst) (Map.assocs (compileMods m8rs))
>     edgeList           :: [(Node, [Node])]
>                                          = map
>                                              (BF.bimap nodeFrom (map (fromIntegral . mrModId)))
>                                              nodeList
>
>     nodeFrom           :: ModDestType → Node
>     nodeFrom mdt                         = maybe
>                                              (error $ unwords ["only ToLink forms a ModDestType"
>                                                              , "to be turned into a Node"])
>                                              fromIntegral
>                                              (outGoing mdt)
>
> outGoing               :: ModDestType → Maybe Word
> outGoing mdt                             = case mdt of
>                                              ToLink mId       → Just mId
>                                              _                → Nothing
>
> data Sifting                             =
>   Sifting {
>       ssCounter        :: Int
>     , ssCurrent        :: [Modulator]
>     , ssPrevious       :: [Modulator]} deriving Show 
>
> eliminateDanglingMods  :: Sifting → Sifting
> eliminateDanglingMods Sifting{ .. }      = Sifting (ssCounter + 1) newTry ssCurrent
>   where
>     -- let's examine result of the previous generation
>     -- use it to produce next generation, dropping nodes that:
>     -- 1. expect linked sources but have none
>     --     or
>     -- 2. are superseded 
>     newTry                               = profess
>                                              (ssCounter <= 10)
>                                              "maximum of 10 tries exceeded..."
>                                              filter shouldStay ssCurrent
>
>     shouldStay m8r                         = linkageInOk && linkageOutOk && not superceded
>       where
>         linkageInOk                      =
>           FromLinked /= m8rSource || maybe False (not . null) (Map.lookup (ToLink m8rId) (compileMods ssCurrent))
>         linkageOutOk                     =
>           maybe True present (outGoing m8rDest)
>         superceded                       =
>           maybe False noMatch (Map.lookup (fst (supercedeKey m8r)) (foldl' supercfr Map.empty ssCurrent))
>         
>         m8rId                            = mrModId m8r
>         m8rSource                        = msSource (mrModSrc m8r)
>         m8rDest                          = mrModDest m8r
>
>         noMatch current                  = m8rId /= current
>         present modId                    = (isJust . find (\x → mrModId x == modId)) ssCurrent
>
>     supercfr accum m8r                   = Map.insert newK newW accum
>       where
>         (newK, newW)                     = supercedeKey m8r
>
> siftMods               :: [Modulator] → [Modulator]
> siftMods m8rs                            = ssCurrent 
>   where
>     seed                                 = Sifting 0 m8rs []
>     generations                          = iterate' eliminateDanglingMods seed
>     Sifting{ .. }                        = head $ dropWhile unfinished generations
>
>     unfinished         :: Sifting → Bool
>     unfinished Sifting{ .. }             = ssCurrent /= ssPrevious
>
> compileMods            :: [Modulator] → Map ModDestType [Modulator]
> compileMods                              = foldl' nodeFolder Map.empty
>   where
>     nodeFolder accum m8r@Modulator{ .. } =
>       let
>         soFar                            = fromMaybe [] (Map.lookup mrModDest accum)
>       in
>         Map.insert mrModDest (m8r : soFar) accum
>
> unpackModSrc           :: Word → Maybe ModSrc
> unpackModSrc wIn
>   | traceNever trace_UMS False           = undefined
>   | otherwise                            = mmapping
>                                              >>= addMapping 
>                                              >>= addSource
>   where
>     mmapping                             = Just defMapping
>                                              >>= addContinuity
>                                              >>= addBiPolar
>                                              >>= addMax2Min
>                                              >>= addCCBit
>     cont               :: Int            = fromIntegral   (wIn `shift` (-10))    `mod` 64
>     bipolar            :: Int            = fromIntegral   (wIn `shift` (-9))     `mod` 2
>     max2Min            :: Int            = fromIntegral   (wIn `shift` (-8))     `mod` 2
>     ccBit              :: Int            = fromIntegral   (wIn `shift` (-7))     `mod` 2
>     src                :: Int            = fromIntegral   wIn                    `mod` 128
>
>     addContinuity from                   = if cont < 16    then Just from{msContinuity = toEnum cont}
>                                                            else Nothing
>     addBiPolar from                      = if bipolar /= 0 then Just from{msBiPolar = True}
>                                                            else Just from{msBiPolar = False}
>     addMax2Min from                      = if max2Min /= 0 then Just from{msMax2Min = True}
>                                                            else Just from{msMax2Min = False}
>     addCCBit from                        = if ccBit /= 0   then Nothing
>                                                            else Just from{msCCBit = False}
>     addMapping m                         = Just defModSrc{msMapping = m}
>     addSource from                       = result
>       where
>         result = case src of
>                    0      → Just from{msSource = FromNoController}
>                    2      → Just from{msSource = FromNoteOnVel}
>                    3      → Just from{msSource = FromNoteOnKey}
>                    127    → Just from{msSource = FromLinked}
>                    _      → Nothing
>
>     trace_UMS                            = unwords [show cont, show bipolar, show max2Min, show ccBit, show src]
>
> addSrc                 :: ModSrc → Modulator → Maybe Modulator
> addSrc modSrc from                       = Just from{mrModSrc = modSrc}
>
> addDest                :: Word → Modulator → Maybe Modulator
> addDest wIn from
>   | (wIn .&. 0x8000) /= 0                = Just from{mrModDest = ToLink $ wIn .&. 0x7fff}
>   | otherwise                            = case wIn of
>                                              8       → Just from{mrModDest = ToFilterFc}
>                                              15      → Just from{mrModDest = ToChorus}
>                                              16      → Just from{mrModDest = ToReverb}
>                                              48      → Just from{mrModDest = ToInitAtten}
>                                              _ → Nothing
>
> addAmount              :: Double → Modulator → Maybe Modulator
> addAmount dIn from                       = if dIn == 0
>                                              then Nothing
>                                              else Just (from{mrModAmount = dIn})
>
> addAmtSrc              :: Maybe Modulator → ModSrc → Maybe Modulator
> addAmtSrc mm8r amtSrc@ModSrc{msSource}   = mm8r >>=     (\x → case msSource of
>                                                               FromLinked       → Nothing
>                                                               _                → Just x{mrAmountSrc = amtSrc})
> addAmtSrc'              :: ModSrc → Modulator → Maybe Modulator
> addAmtSrc' modSrc@ModSrc{msSource} m8r   = Just m8r >>= (\x → case msSource of
>                                                               FromLinked       → Nothing
>                                                               _                → Just x{mrAmountSrc = modSrc})
>
> defaultMods            :: [Modulator]    = if useDefaultMods
>                                              then [ makeDefaultMod 0 ms0 48 960     defModSrc
>                                                   , makeDefaultMod 1 ms1  8 (-2400) ms2 ] ++ specialDefaultMods
>                                              else []
>  -- all three of these are negative unipolar, continuity varying                                                            
>   where                                                 --    cont    bipolar  neg  CC
>     ms0                                  = ModSrc   (Mapping Concave   False  True False) FromNoteOnVel
>     ms1                                  = ModSrc   (Mapping Linear    False  True False) FromNoteOnVel
>  -- some implementations do not include the amount source below in the second default modulator
>     ms2                                  = ModSrc   (Mapping Switch    False  True False) FromNoteOnVel
>
>     makeDefaultMod     :: Word → ModSrc → Word → Double → ModSrc → Modulator
>     makeDefaultMod mId ms igen amt amtSrc
>                                          = professIsJust
>                                              (Just defModulator{mrModId = mId}
>                                              >>= addSrc ms
>                                              >>= addDest igen
>                                              >>= addAmount amt
>                                              >>= addAmtSrc' amtSrc)
>                                            "makeDefaultMod"
>
>     ms3                                  = ModSrc   (Mapping Linear False False False) FromNoController
>
>     specialDefaultMods :: [Modulator]
>     specialDefaultMods                   = modChorus ++ modReverb
>       where
>         modChorus                        =
>           if chorusAllPercent > 0
>             then [makeDefaultMod 10 ms3 15 (chorusAllPercent*10) defModSrc]
>             else []
>         modReverb                        =
>           if reverbAllPercent > 0
>             then [makeDefaultMod 11 ms3 16 (reverbAllPercent*10) defModSrc]
>             else []
>
> evaluateMods           :: ModDestType → Map ModDestType [Modulator] → NoteOn → Double
> evaluateMods md graph noon@NoteOn{ .. }  = sum $ maybe [] (map evaluateMod) (Map.lookup md graph)
>   where
>     evaluateMod        :: Modulator → Double
>     evaluateMod Modulator{ .. }
>       | traceNever trace_EM False        = undefined
>       | otherwise                        = getValue mrModSrc * mrModAmount * getValue mrAmountSrc
>       where
>         getValue ModSrc{ .. }
>           | useModulators                =
>               case msSource of
>                 FromNoController         → 1
>                 FromNoteOnVel            → evaluateNoteOn noteOnVel msMapping
>                 FromNoteOnKey            → evaluateNoteOn noteOnKey msMapping
>                 FromLinked               → evaluateMods (ToLink mrModId) graph noon
>           | otherwise                    = 0
>
>         trace_EM                         =
>           unwords["evaluateMod"
>                 , show md
>                 , show (getValue mrModSrc)
>                 , show mrModAmount
>                 , show (getValue mrAmountSrc)]
>
> evaluateNoteOn         :: Int → Mapping → Double
> evaluateNoteOn n ping                    = controlDenormal ping (fromIntegral n / 128) (0, 1)
>
> evaluateModSignals     :: String → Modulation → ModDestType → ModSignals → NoteOn → Double
> evaluateModSignals tag Modulation{ .. } md ModSignals{ .. } noon
>  | traceNever trace_CMF False            = undefined
>  | otherwise                             = fromCents (xmodEnv + xmodLfo + xvibLfo + xmods)
>  where
>    ModCoefficients{ .. }                 =
>      case md of
>        ToPitch         → toPitchCo
>        ToFilterFc      → toFilterFcCo
>        ToVolume        → toVolumeCo
>        _               → error $ unwords ["Error in caller of evaluateModSignals"
>                                         , "(only ToPitch, ToFilterFc, and ToVolume supported)"
>                                         , show tag, show md]
>
>    xmodEnv             :: Double         = xModEnvPos * xModEnvCo
>    xmodLfo                               = xModLfoPos * xModLfoCo
>    xvibLfo                               = xVibLfoPos * xVibLfoCo
>    xmods                                 = evaluateMods md modGraph noon
>
>    trace_CMF                             = unwords ["evaluateModSignals: "
>                                                     , show tag,    " + "
>                                                     , show xmodEnv, " + "
>                                                     , show xmodLfo, " + "
>                                                     , show xvibLfo, " + "
>                                                     , show xmods,   " = "
>                                                     , show (xmodEnv+xmodLfo+xvibLfo+xmods), " => "
>                                                     , show (fromCents (xmodEnv+xmodLfo+xvibLfo+xmods))]
>
> addResonance           :: ∀ p . Clock p ⇒ NoteOn → Modulation → Signal p (Double, ModSignals) Double
> addResonance noon m8n@Modulation{ .. }   = maybe delay' makeSF mLowPass
>   where
>     LowPass{ .. }                        = fromJust mLowPass
>
>     makeSF             :: LowPass → Signal p (Double, ModSignals) Double
>     makeSF _                             
>       | traceNever trace_MSF False       = undefined
>       | otherwise                        =
>           case useResonanceType of
>             ResonanceNone                → error $ unwords ["addResonance/makeSF"
>                                                           , "should not reach sf if ResonanceNone"]
>             ResonanceLowpass             → procButter
>             ResonanceBandpass            → procBandpass
>             ResonanceSVF                 → procSVF
>       where
>         trace_MSF                        = unwords ["addResonance/makeSF (fc, q)"
>                                                   , show (lowPassFc, show lowPassQ)]
>
>     modulateFc         :: ModSignals → Double
>     modulateFc msig                      =
>       clip (20, 20000) (lowPassFc * evaluateModSignals
>                                       "modulateFc"
>                                       m8n
>                                       ToFilterFc
>                                       msig
>                                       noon)
>
>     procButter         :: Signal p (Double, ModSignals) Double
>     procButter                           = 
>       proc (x, msig) → do
>         let fc = modulateFc msig
>         y ← filterLowPassBW              ⤙ (x, fc)
>         let y' = resonate x fc y
>         outA                             ⤙ y'
>
>     lowpassWeight                         = 0.50
>     bandpassWeight                        = 0.75
>
>     procBandpass       :: Signal p (Double, ModSignals) Double
>     procBandpass                         = 
>       proc (x, msig) → do
>         let fc = modulateFc msig
>         y1 ← filterLowPassBW             ⤙ (x, fc)
>         y2 ← filterBandPass 2            ⤙ (x, fc, bpQ)
>         let y' = resonate x fc (y1*lpW + y2*bpW)
>         outA                             ⤙ y'
>       where
>         bpQ = lowPassQ / 3
>         lpW = lowpassWeight
>         bpW = bandpassWeight
>
>     procSVF            :: Signal p (Double, ModSignals) Double
>     procSVF                              =
>       proc (x, msig) → do
>         let fc = modulateFc msig
>         y ← filterSVF 0 {- lowPassQ  -}  ⤙ (x, fc)
>         let y' = resonate x fc y
>         outA                             ⤙ y'
>
>     delay'             :: Signal p (Double, ModSignals) Double
>                                          =
>       proc (x, _) → do
>         y ← delay 0                      ⤙ x  
>         outA                             ⤙ y
>
>     resonate           :: Double → Double → Double → Double
>     resonate x fc y
>       | traceNever trace_R False         = undefined
>       | otherwise                        = y'
>       where
>         y'                               = checkForNan y "resonate y"
>         trace_R                          =
>           unwords [
>               "resonate\nsIn"    , show (checkForNan  x "resonate x")
>             , "\nfc"             , show (checkForNan fc "resonate fc")
>             , "\nsOut"           , show y']
>
> deriveModTriple        :: Maybe Int → Maybe Int → Maybe Int → ModTriple
> deriveModTriple toPitch toFilterFc toVolume
>                                          =
>   ModTriple
>     (maybe 0 fromIntegral toPitch)
>     (maybe 0 fromIntegral toFilterFc)
>     (maybe 0 fromIntegral toVolume)
>
> deriveLFO              :: Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe LFO
> deriveLFO del mfreq toPitch toFilterFc toVolume
>   | traceNever trace_DLFO False          = undefined
>   | otherwise                            =
>       if useLFO && anyJust
>         then Just $ LFO (fromTimecents del)
>                         (fromAbsoluteCents $ maybe 0 (clip (-16000, 4500)) mfreq)
>                         (deriveModTriple toPitch toFilterFc toVolume)
>         else Nothing
>   where
>     anyJust            :: Bool           = isJust toPitch || isJust toFilterFc || isJust toVolume
>     trace_DLFO                           = unwords ["deriveLFO", show (toPitch, toFilterFc, toVolume)]
>
> doLFO                  :: ∀ p . Clock p ⇒ Maybe LFO → Signal p () Double
> doLFO                                    = maybe (constA 0) makeSF
>   where
>     makeSF             :: LFO → Signal p () Double
>     makeSF LFO{ .. }                     = 
>       proc _ → do
>         y ← triangleWave lfoFrequency    ⤙ ()
>         z ← delayLine lfoDelay           ⤙ y
>         outA                             ⤙ oscillate z
>
>     oscillate          :: Double → Double
>     oscillate sIn
>       | traceNever trace_O False         = undefined
>       | otherwise                        = sOut
>       where
>         sOut                             = sIn
>         trace_O                          = unwords ["oscillate", show sIn]
>
> triangleWave           :: ∀ p . Clock p ⇒ Double → Signal p () Double
> triangleWave freq                        = 
>   proc _ → do
>     osc triangleWaveTable 0              ⤙ freq
>
> modVib                 :: ∀ p . Clock p ⇒ Double → Double → Signal p Double Double
> modVib rate depth                        =
>   proc sIn → do
>     vib   ← osc sineTable 0              ⤙ rate
>     sOut  ← delayLine1 0.2               ⤙ (sIn,0.1+depth*vib)
>     outA                                 ⤙ sOut
>
> echo                   :: ∀ p . Clock p ⇒ Signal p  Double Double
> echo                                     =
>   proc sIn → do
>     rec
>       fb   ← delayLine 0.04            ⤙ sIn + 0.7*fb   -- 0.7*fb
>       fb'  ← delayLine 0.06            ⤙ sIn + 0.7*fb'  -- 0.7*fb
>       fb'' ← delayLine 0.1             ⤙ sIn + 0.7*fb'' -- 0.7*fb
>     outA ⤙ (fb + fb' + fb'')                           -- fb/3
>
> triangleWaveTable      :: Table          = tableSinesN 16384 
>                                                          [      1,  0, -0.5,  0,  0.3,   0
>                                                           , -0.25,  0,  0.2,  0, -0.167, 0
>                                                           ,  0.14,  0, -0.125]

Miscellaneous =========================================================================================================

> sawtoothTable = tableSinesN 16384 
>                   [1, 0.5, 0.3, 0.25, 0.2, 0.167, 0.14, 0.125, 0.111]
> sawtooth               :: ∀ p . Clock p ⇒  Double → Signal p () Double
> sawtooth freq                           = 
>   proc _ → do
>     osc sawtoothTable 0 ⤙ freq

twave partial (-1)^i * n^-2 * sin (2*pi f0 n t)

i = harmonic label 0..N-1
k = harmonic mode number = (2 * i) + 1
f0 = fundamental frequency, say 3 Hz

when i is zero, k is 1, 2*pi*f0*k is 18.8
when i is one,  k is 3, 2*pi*f0*k is 56.5
when i is two,  k is 5, 2*pi*f0*k is 94.2

> listem                 :: IO Double
> listem = return $ sum (map getTerm [0..500])
>
> getTerm                :: Int → Double
> getTerm i                                = sign    *     (1 / (kF*kF)) * sin (2*pi * f0 * jF )
>   where
>     j, k               :: Int
>     j = i
>     k = 2 * i + 1
>
>     jF, kF             :: Double
>     jF = fromIntegral j
>     kF = fromIntegral k
>
>     sign = (-1) ^ i
>     f0 = 5

see source https://karmafx.net/docs/karmafx_digitalfilters.pdf for the Notch case

> filterSVF              :: ∀ p . Clock p ⇒ Double → Signal p (Double,Double) Double
> filterSVF initQ
>   | traceNow trace_FSVF False            = undefined
>   | otherwise                            =
>   let
>     sr                                   = rate (undefined :: p)
>   in
>     proc (sig, fc) → do
>       let f1                             = 2 * sin (pi * fc / sr)
>       rec
>         let yL'                          = f1 * yB + yL
>         let yH'                          = sig - yL' - initQ * yB
>         let yB'                          = f1 * yH' + yB
>
>         yL ← delay 0                     ⤙ yL'
>         yB ← delay 0                     ⤙ yB'
>         yH ← delay 0                     ⤙ yH'
>       outA                               ⤙ yL'
>   where
>     trace_FSVF                           = unwords ["filterSVF initQ = ", show initQ, show (fromCentibels' initQ)]

Controller Curves =====================================================================================================

> qTableSize             :: Int            = 1024
> tableSize              :: Double         = fromIntegral qTableSize
> qMidiSize128           :: Int            = 128
> qStepSize                                = qTableSize `div` qMidiSize128
>
> quarterCircleTable     :: Array Int Double
>                                            -- TODO: use Table
>                                          = array (0, qTableSize - 1) [(x, calc x) | x ← [0..(qTableSize - 1)]]
>   where
>     calc               :: Int → Double
>     calc i                               = 1 - sqrt (1 - x*x)
>       where 
>         x              :: Double         = fromIntegral i / tableSize

The use of these functions requires that their input is normalized between 0 and 1
(And the output is normalized, too)

> funLinear              :: Double → Double
> funLinear d                              = d
>
> funConcave d
>   | d >= 1                               = 1
>   | otherwise                            = quarterCircleTable ! truncate (d * tableSize)
>
> funConvex d
>   | (1 - d) >= 1                         = 1
>   | otherwise                            = 1 - (quarterCircleTable ! truncate ((1 - d) * tableSize))
>
> funSwitch d                              = if d < 0.5
>                                              then 0
>                                              else 1
>
> controlDenormal        :: Mapping → Double → (Double, Double) → Double
> controlDenormal ping@Mapping{msBiPolar} dIn (lo, hi)
>                                          = if msBiPolar
>                                              then controlBiPolar ping dNorm
>                                              else controlUniPolar ping dNorm
>   where
>     scale                                = profess (lo < hi)
>                                              "inverted range in controlDenormal"
>                                              (hi - lo)
>     dNorm                                = (dIn - lo) / scale
>
> controlUniPolar        :: Mapping → Double → Double
> controlUniPolar Mapping{ .. } dIn        = fun xStart
>   where
>     fun                                  = case msContinuity of
>                                              Linear     → funLinear
>                                              Concave    → funConcave
>                                              Convex     → funConvex
>                                              Switch     → funSwitch
>
>     xStart             :: Double         = if msMax2Min
>                                              then 1 - dIn
>                                              else dIn
>
> controlBiPolar         :: Mapping → Double → Double
> controlBiPolar ping@Mapping{ .. } dIn    = dOut
>   where
>     -- bipolar concave/convex:
>     --   if positive, swap the left half to the opposite
>     --   if negative, swap the right one
>
>     isCurve            :: Bool           = msContinuity == Concave || msContinuity == Convex
>
>     swapCont           :: Continuity → Continuity
>     swapCont cIn                         = case cIn of
>                                              Concave → Convex
>                                              Convex  → Concave
>
>     (left, right)      :: (Continuity, Continuity)
>                                          = if isCurve
>                                              then if msMax2Min
>                                                     then (msContinuity, swapCont msContinuity)
>                                                     else (swapCont msContinuity, msContinuity)
>                                              else (msContinuity, msContinuity)
>     pingL                                = ping{msBiPolar = False, msContinuity = left}
>     pingR                                = ping{msBiPolar = False, msContinuity = right}
>     (addL, addR)                         = if msMax2Min
>                                              then (0, -1)
>                                              else (-1, 0)
>
>     dOut
>       | msContinuity == Switch           = (controlUniPolar ping dIn * 2) - 1
>       | dIn < 0.5                        = controlDenormal pingL dIn (0.0, 0.5) + addL
>       | otherwise                        = controlDenormal pingR dIn (0.5, 1.0) + addR

Type declarations =====================================================================================================

> data NoteOn =
>   NoteOn {
>     noteOnVel          :: Velocity
>   , noteOnKey          :: KeyNumber} deriving (Eq, Ord, Show)
>
> data LowPass =
>   LowPass {
>     lowPassFc          :: Double
>   , lowPassQ           :: Double} deriving (Eq, Show)
>
> data ModSignals =
>   ModSignals {
>     xModEnvPos         :: Double
>   , xModLfoPos         :: Double
>   , xVibLfoPos         :: Double} deriving (Show)
>
> data ModCoefficients =
>   ModCoefficients {
>     xModEnvCo          :: Double
>   , xModLfoCo          :: Double
>   , xVibLfoCo          :: Double} deriving (Eq, Show)
>
> defModCoefficients                       = ModCoefficients 0 0 0
>
> data ModTriple =
>   ModTriple {
>     coPitch            :: Double
>   , coFilterFc         :: Double
>   , coVolume           :: Double} deriving (Eq, Show)
>
> coAccess               :: ModDestType → ModTriple → Double
> coAccess md mt@ModTriple{coPitch, coFilterFc, coVolume}
>                                          = case md of
>                                              ToPitch     → coPitch
>                                              ToFilterFc  → coFilterFc
>                                              ToVolume    → coVolume
>                                              _           → error ("ModTriple only deals with ToPitch"
>                                                               ++ ", ToFilterFc, and ToVolume")
> defModTriple                             = ModTriple 0 0 0
>
> data ResonanceType =
>   ResonanceNone 
>   | ResonanceLowpass
>   | ResonanceBandpass
>   | ResonanceSVF deriving (Eq, Show)
>
> data LFO =
>   LFO {
>     lfoDelay           :: Double
>   , lfoFrequency       :: Double
>   , lfoModTriple       :: ModTriple} deriving (Eq, Show)
>
> data Modulation =
>   Modulation {
>     mLowPass           :: Maybe LowPass
>   , mModEnv            :: Maybe Envelope
>   , mModLfo            :: Maybe LFO
>   , mVibLfo            :: Maybe LFO
>   , toPitchCo          :: ModCoefficients
>   , toFilterFcCo       :: ModCoefficients
>   , toVolumeCo         :: ModCoefficients
>   , modGraph           :: Map ModDestType [Modulator]} deriving (Eq, Show)
>
> defModulation                            = Modulation
>                                              Nothing Nothing Nothing Nothing
>                                              defModCoefficients defModCoefficients defModCoefficients
>                                              Map.empty
>
> data Modulator =
>   Modulator {
>     mrModId            :: Word
>   , mrModSrc           :: ModSrc
>   , mrModDest          :: ModDestType
>   , mrModAmount        :: Double
>   , mrAmountSrc        :: ModSrc} deriving (Eq, Show)
>    
> defModulator                             = Modulator 0 defModSrc NoDestination 0 defModSrc
>
> data ModKey =
>   ModKey {
>     krSrc              :: ModSrc
>   , krDest             :: ModDestType
>   , krAmtSrc           :: ModSrc} deriving (Eq, Ord, Show)
>
> supercedeKey          :: Modulator → (ModKey, Word)
> supercedeKey Modulator{ .. }             = (ModKey mrModSrc mrModDest mrAmountSrc, mrModId)
>
> data ModDestType =
>     NoDestination
>   | ToPitch
>   | ToFilterFc
>   | ToVolume
>   | ToInitAtten
>   | ToChorus
>   | ToReverb
>   | ToLink Word deriving (Eq, Ord, Show)
>
> data ModSrcSource =
>     FromNoController
>   | FromNoteOnVel
>   | FromNoteOnKey
>   | FromLinked deriving (Eq, Ord, Show)
>
> data ModSrc =
>   ModSrc {
>     msMapping          :: Mapping
>   , msSource           :: ModSrcSource} deriving (Eq, Ord, Show)
>
> defModSrc                                = ModSrc defMapping FromNoController
>
> data Envelope =
>   Envelope {
>     eDelayT            :: Double
>   , eAttackT           :: Double
>   , eHoldT             :: Double
>   , eDecayT            :: Double
>   , eSustainLevel      :: Double
>   , eReleaseT          :: Double
>   , eModTriple         :: ModTriple} deriving (Eq, Show)
>
> data ModulationSettings =
>   ModulationSettings {
>     qqUseModulators    :: Bool
>   , qqChorusAllPerCent :: Double
>   , qqReverbAllPerCent :: Double
>   , qqUseDefaultMods   :: Bool
>   , qqUseResonanceType :: ResonanceType
>   , qqUseLFO           :: Bool} deriving Show
>
> useModulators                            = qqUseModulators              defM
> chorusAllPercent                         = qqChorusAllPerCent           defM
> reverbAllPercent                         = qqReverbAllPerCent           defM
> useDefaultMods                           = qqUseDefaultMods             defM
> useResonanceType                         = qqUseResonanceType           defM
> useLFO                                   = qqUseLFO                     defM
>
> defM                   :: ModulationSettings
> defM =
>   ModulationSettings {
>     qqUseModulators                      = True
>   , qqChorusAllPerCent                   = 0
>   , qqReverbAllPerCent                   = 0
>   , qqUseDefaultMods                     = True
>   , qqUseResonanceType                   = ResonanceBandpass -- ResonanceLowpass -- ResonanceNone
>   , qqUseLFO                             = True}

The End
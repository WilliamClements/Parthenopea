> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module Modulation where
>
> import Control.Arrow
> import Data.Array
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
> import Debug.Trace ( traceIO, trace )
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
>     sifted                               = renumberMods $ siftMods $ groomMods (dm8rs ++ m8rs)
>     checked                              = profess
>                                              (freeOfCycles sifted)
>                                              "cycles in modulator graph"
>                                              sifted
>         
> groomMods              :: [Modulator] → [Modulator]
> groomMods m8rs                           = Map.elems uniqued
>   where
>     uniqued                              = foldl' ufolder Map.empty m8rs
>     ufolder accum m8r@Modulator{ .. }    = Map.insert (ModKey mrModSrc mrModDest mrAmountSrc) m8r accum
>
> freeOfCycles           :: [Modulator] → Bool
> freeOfCycles m8rs                        = null $ cyclicNodes $ makeGraph edgeList
>   where
>     nodeList           :: [(ModDestType, [Modulator])]
>                                          = filter (isJust . outGoing . fst) (Map.assocs (compileMods m8rs))
>     edgeList           :: [(Node, [Node])]
>                                          = map (BF.bimap nodeFrom (map (fromIntegral . mrModId))) nodeList
>
>     nodeFrom           :: ModDestType → Node
>     nodeFrom mdt                         =
>       maybe
>         (error $ unwords ["only ToLink forms a ModDestType to be turned into a Node"])
>         fromIntegral
>         (outGoing mdt)
>
> outGoing               :: ModDestType → Maybe Word
> outGoing mdt                             =
>   case mdt of
>     ToLink mId         → Just mId
>     _                  → Nothing
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
>     -- 2. consist of an outbound link (by Id) to non-existent mod
>     --     or
>     -- 3. are superseded 
>     newTry                               = profess
>                                              (ssCounter <= 10)
>                                              "maximum of 10 tries exceeded..."
>                                              filter shouldStay ssCurrent
>
>     byModDestType                        = compileMods ssCurrent
>
>     shouldStay m8r                       = linkageInOk && linkageOutOk
>       where
>         linkageInOk                      =
>           FromLinked /= m8rSource || maybe False (not . null) (Map.lookup (ToLink m8rId) byModDestType)
>         linkageOutOk                     =
>           maybe True (\w -> (isJust . find (\m → mrModId m == w)) ssCurrent) (outGoing m8rDest)
>         
>         m8rId                            = mrModId m8r
>         m8rSource                        = msSource (mrModSrc m8r)
>         m8rDest                          = mrModDest m8r
>
> siftMods               :: [Modulator] → [Modulator]
> siftMods m8rs                            = ssCurrent 
>   where
>     generations                          = iterate' eliminateDanglingMods (Sifting 0 m8rs [])
>     Sifting{ .. }                        = head $ dropWhile unfinished generations
>     unfinished Sifting{ .. }             = ssCurrent /= ssPrevious
>
> compileMods            :: [Modulator] → Map ModDestType [Modulator]
> compileMods                              = foldl' mfolder Map.empty
>   where
>     mfolder accum m8r@Modulator{ .. }    =
>       let
>         soFar                            = fromMaybe [] (Map.lookup mrModDest accum)
>       in
>         Map.insert mrModDest (m8r : soFar) accum
>
> renumberMods           :: [Modulator] → [Modulator]
> renumberMods m8rs                        = map renumber m8rs
>   where
>     subs               :: [(Word, Word)]
>     subs                                 = zipWith (\i m → (mrModId m, i)) [0..] m8rs
>     renumber           :: Modulator → Modulator
>     renumber m8r@Modulator{ .. }         =
>       let
>         upd mid                          = fromJust $ lookup mid subs
>       in
>         m8r{  mrModId                    = upd mrModId
>             , mrModDest                  = (\case (ToLink m)         → (ToLink $ upd m);
>                                                   o                  → o) mrModDest}
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
>  | traceNever trace_EMS False            = undefined
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
>    trace_EMS                             = unwords ["evaluateModSignals: "
>                                                     , show tag,    " + "
>                                                     , show xmodEnv, " + "
>                                                     , show xmodLfo, " + "
>                                                     , show xvibLfo, " + "
>                                                     , show xmods,   " = "
>                                                     , show (xmodEnv+xmodLfo+xvibLfo+xmods), " => "
>                                                     , show (fromCents (xmodEnv+xmodLfo+xvibLfo+xmods))]
>
> delay'                 :: ∀ p . Clock p ⇒ Signal p (Double, ModSignals) Double
> delay'                                   =
>     proc (x, _) → do
>       y ← delay 0                        ⤙ x  
>       outA                               ⤙ y
>
> cascadeCount           :: ResonanceType → Maybe Int
> cascadeCount resonType                   =
>   case resonType of
>     ResonanceNone                        → Nothing
>     ResonanceConvo                       → Nothing
>     _                                    → Just cascadeConfig
>
> addResonance           :: Clock p ⇒ NoteOn → Modulation → Signal p (Double, ModSignals) Double
> addResonance noon m8n@Modulation{ .. }   =
>   case cascadeCount lowpassType of
>     Nothing            → delay'
>     Just count         →
>       case count of
>         0              → final
>         1              → stage >>> final
>         2              → stage >>> stage >>> final
>         3              → stage >>> stage >>> stage >>> final
>         4              → stage >>> stage >>> stage >>> stage >>> final
>         _              → error $ unwords [show count, "cascades are too many, not supported"]
>   where
>     Lowpass{ .. }                        = mLowpass
>
>     stage                                =
>         proc (sIn, msig)                 → do
>           let fc                         = modulateFc msig
>           pickled      ← procFilter m8n  ⤙ (sIn, fc)
>           let sOut                       = resonate sIn fc pickled
>           outA                           ⤙ (sOut, msig)
>
>     final
>       | traceNow trace_MSF False         = undefined
>       | otherwise                        =
>         proc (sIn, msig)                 → do
>           let fc                         = modulateFc msig
>           pickled      ← procFilter m8n  ⤙ (sIn, fc)
>           let sOut                       = resonate sIn fc pickled
>           outA                           ⤙ sOut
>     trace_MSF                            = unwords ["addResonance (fc, q)"
>                                                    , show (lowpassFc, lowpassQ)]
>
>     modulateFc         :: ModSignals → Double
>     modulateFc msig                      =
>       clip (20, 20000)
>            (lowpassFc * evaluateModSignals
>                           "modulateFc"
>                           m8n
>                           ToFilterFc
>                           msig
>                           noon)
>
>     resonate           :: Double → Double → Double → Double
>     resonate x fc y
>       | traceNot trace_R False           = undefined
>       | otherwise                        = y
>       where
>         y'                               = checkForNan y "resonate y"
>         trace_R                          =
>           unwords [
>               "resonate\nsIn"    , show (checkForNan  x "resonate x")
>             , "\nfc"             , show (checkForNan fc "resonate fc")
>             , "\nsOut"           , show y']
>
> procFilter             :: ∀ p . Clock p ⇒ Modulation → Signal p (Double, Double) Double
> procFilter m8n@Modulation{ .. }          =
>   case lowpassType mLowpass of
>     ResonanceNone                        → error $ unwords ["procFilter"
>                                                           , "should not reach makeSF if ResonanceNone"]
>     ResonanceLowpass                     → procLowpass m8n
>     ResonanceBandpass                    → procBandpass m8n
>     ResonanceSVF1                        → procSVF1 m8n
>     ResonanceSVF2                        → procSVF2 m8n
>     ResonanceOnePole                     → procOnePole m8n
>     ResonanceTwoPoles                    → procTwoPoles m8n
>
> procLowpass            :: ∀ p . Clock p ⇒ Modulation → Signal p (Double, Double) Double
> procLowpass _                            =
>   proc (x, fc) → do
>     y ← filterLowPassBW                  ⤙ (x, fc)
>     outA                                 ⤙ notracer "lp" y
>
> procBandpass           :: ∀ p . Clock p ⇒ Modulation → Signal p (Double, Double) Double
> procBandpass Modulation{ .. }            =
>   proc (x, fc) → do
>     y1 ← filterLowPassBW                 ⤙ (x, fc)
>     y2 ← filterBandPass 2                ⤙ (x, fc, lowpassQ / 3)
>     let y'                               = traceBandpass y1 y2
>     outA                                 ⤙ notracer "bp" y'
>   where
>     Lowpass{ .. }                        = mLowpass
>     lowpassWeight                        = 0.50
>     bandpassWeight                       = 0.75
>
>     traceBandpass      :: Double → Double → Double
>     traceBandpass y1 y2
>       | traceNever trace_BP False        = undefined
>       | otherwise                        = y'
>       where
>         y'                               = y1*lowpassWeight + y2*bandpassWeight
>         trace_BP                         = unwords ["traceBandPass", show y1, show y2]
>

see source https://karmafx.net/docs/karmafx_digitalfilters.pdf for the Notch case

> indeedAverageInput                       = True
> extraDampFactor                          = 1
>
> procSVF1               :: ∀ p . Clock p ⇒ Modulation → Signal p (Double,Double) Double
> procSVF1 Modulation{..}                  =
>   proc (x, fc) → do
>     let f1                               = 2 * sin (theta fc)
>     rec
>       let yL                             = f1 * yB + yL'
>       let xuse                           = maybeAverageInput x x'
>       let yH                             = xuse - yL' - damp * yB'
>       let yB                             = f1 * yH + yB'
>
>       x' ← delay 0                       ⤙ x
>       yL' ← delay 0                      ⤙ yL
>       yB' ← delay 0                      ⤙ yB
>     outA                                 ⤙ yL
>   where
>     Lowpass{ .. }                        = mLowpass
>
>     damp                                 = extraDampFactor / fromCentibels' lowpassQ
>     theta c                              = pi * c / rate (undefined :: p)
>
>     maybeAverageInput c c'               =
>       if indeedAverageInput
>         then (c + c') / 2
>         else c
>
> procSVF2               :: ∀ p . Clock p ⇒ Modulation → Signal p (Double,Double) Double
> procSVF2 Modulation{..}                  =
>   proc (x, fc) → do
>     let f1                               = 2 * sin (theta fc)
>     rec
>       let yL                             = f1 * yB' + yL'
>       let xuse                           = maybeAverageInput x x'
>       let yH                             = xuse - yL - damp * yB'
>       let yB                             = f1 * yH + yB'
>
>       x' ← delay 0                       ⤙ x
>       yL' ← delay 0                      ⤙ yL
>       yB' ← delay 0                      ⤙ yB
>     outA                                 ⤙ yL
>   where
>     Lowpass{ .. }                        = mLowpass
>
>     damp                                 = extraDampFactor / fromCentibels' lowpassQ
>     theta c                              = pi * c / rate (undefined :: p)
>
>     maybeAverageInput c c'               =
>       if indeedAverageInput
>         then (c + c') / 2
>         else c
>
> procOnePole            :: ∀ p . Clock p ⇒ Modulation → Signal p (Double, Double) Double
> procOnePole Modulation{ .. }             =
>   proc (x, fc) → do
>     let w0                               = 2 * pi * fc / sr
>     let a                                =
>           realPart $ (2.7182818284590452353602874713527 :+ 0) ** (0 :+ (-w0))
>     let c                                = 1 - a
>     rec
>       y'     ← delay 0                   ⤙ y
>       let y                              = c * x + (1 - c) * y'
>     outA                                 ⤙ y
>   where
>     Lowpass { .. }                       = mLowpass
>     sr                                   = rate (undefined :: p)
>
> procTwoPoles           :: ∀ p . Clock p ⇒ Modulation → Signal p (Double, Double) Double
> procTwoPoles Modulation{ .. }
>   | traceNot trace_P2P False             = undefined
>   | otherwise                            =
>   proc (x, fc) → do
>     rec
>       let y                              = m2n2_b0 * x + m2n2_b1 * x' + m2n2_b2 * x'' - m2n2_a1 * y' - m2n2_a2 * y''
>       y'     ← delay 0                   ⤙ y
>       y''    ← delay 0                   ⤙ y' 
>       x'     ← delay 0                   ⤙ x
>       x''    ← delay 0                   ⤙ x'
>     outA                                 ⤙ y
>   where
>     Lowpass { .. }                       = mLowpass
>     cs@CoeffsM2N2{ .. }                  = buildSystemM2N2 $ pickZerosAndPoles (2 * pi * lowpassFc) (lowpassQ / 960)
>
>     trace_P2P                            = unwords ["procTwoPoles", show cs]
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

Miscellaneous =========================================================================================================

> triangleWave           :: ∀ p . Clock p ⇒ Double → Signal p () Double
> triangleWave freq                        = 
>   proc _ → do
>     osc triangleWaveTable 0              ⤙ freq
>
> modVib                 :: ∀ p . Clock p ⇒ Double → Double → Signal p Double Double
> modVib rate depth                        =
>   proc sIn → do
>     vib    ← osc sineTable 0             ⤙ rate
>     sOut   ← delayLine1 0.2              ⤙ (sIn,0.1+depth*vib)
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
> sawtooth               :: ∀ p . Clock p ⇒  Double → Signal p () Double
> sawtooth freq                           = 
>   proc _ → do
>     osc sawtoothTable 0 ⤙ freq

Controller Curves =====================================================================================================

> qMidiSize128           :: Int            = 128
> qStepSize                                = qTableSize `div` qMidiSize128
>
> controlDenormal        :: Mapping → Double → (Double, Double) → Double
> controlDenormal ping@Mapping{msBiPolar} dIn (lo, hi)
>                                          = if msBiPolar
>                                              then controlBiPolar ping dNorm
>                                              else controlUniPolar ping dNorm
>   where
>     scale                                = profess (lo < hi) "inverted range in controlDenormal" (hi - lo)
>     dNorm                                = (dIn - lo) / scale
>
> controlUniPolar        :: Mapping → Double → Double
> controlUniPolar Mapping{ .. } dIn        = control xStart
>   where
>     control                              = case msContinuity of
>                                              Linear     → controlLinear
>                                              Concave    → controlConcave
>                                              Convex     → controlConvex
>                                              Switch     → controlSwitch
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
>     swapCont           :: Continuity → Continuity
>     swapCont cIn                         = case cIn of
>                                              Concave → Convex
>                                              Convex  → Concave
>     (left, right)      :: (Continuity, Continuity)
>                                          = if msContinuity == Concave || msContinuity == Convex
>                                              then if msMax2Min
>                                                     then (msContinuity, swapCont msContinuity)
>                                                     else (swapCont msContinuity, msContinuity)
>                                              else (msContinuity, msContinuity)
>     pingL                                = ping{msBiPolar = False, msContinuity = left}
>     pingR                                = ping{msBiPolar = False, msContinuity = right}
>     (addL, addR)                         = if msMax2Min
>                                              then (0, -1)
>                                              else (-1, 0)
>     dOut
>       | msContinuity == Switch           = (controlUniPolar ping dIn * 2) - 1
>       | dIn < 0.5                        = controlDenormal pingL dIn (0.0, 0.5) + addL
>       | otherwise                        = controlDenormal pingR dIn (0.5, 1.0) + addR

Type declarations =====================================================================================================

> data NoteOn                              =
>   NoteOn {
>     noteOnVel          :: Velocity
>   , noteOnKey          :: KeyNumber} deriving (Eq, Ord, Show)
>
> data Lowpass                             =
>   Lowpass {
>     lowpassType        :: ResonanceType
>   , lowpassFc          :: Double
>   , lowpassQ           :: Double} deriving (Eq, Show)
>
> data ModSignals                          =
>   ModSignals {
>     xModEnvPos         :: Double
>   , xModLfoPos         :: Double
>   , xVibLfoPos         :: Double} deriving (Show)
>
> data ModCoefficients                     =
>   ModCoefficients {
>     xModEnvCo          :: Double
>   , xModLfoCo          :: Double
>   , xVibLfoCo          :: Double} deriving (Eq, Show)
>
> defModCoefficients                       = ModCoefficients 0 0 0
>
> data ModTriple                           =
>   ModTriple {
>     coPitch            :: Double
>   , coFilterFc         :: Double
>   , coVolume           :: Double} deriving (Eq, Show)
>
> coAccess               :: ModDestType → ModTriple → Double
> coAccess md ModTriple{ .. }              =
>   case md of
>     ToPitch            → coPitch
>     ToFilterFc         → coFilterFc
>     ToVolume           → coVolume
>     _                  → error ("ModTriple only deals with ToPitch"
>                            ++ ", ToFilterFc, and ToVolume")
> defModTriple                             = ModTriple 0 0 0
>
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
> data LFO                                 =
>   LFO {
>     lfoDelay           :: Double
>   , lfoFrequency       :: Double
>   , lfoModTriple       :: ModTriple} deriving (Eq, Show)
>
> data Modulation                          =
>   Modulation {
>     mLowpass           :: Lowpass
>   , mModEnv            :: Maybe Envelope
>   , mModLfo            :: Maybe LFO
>   , mVibLfo            :: Maybe LFO
>   , toPitchCo          :: ModCoefficients
>   , toFilterFcCo       :: ModCoefficients
>   , toVolumeCo         :: ModCoefficients
>   , modGraph           :: Map ModDestType [Modulator]} deriving (Eq, Show)
>
> defModulation                            = Modulation
>                                              (Lowpass ResonanceNone 0 0) Nothing Nothing Nothing
>                                              defModCoefficients defModCoefficients defModCoefficients
>                                              Map.empty
>
> data Modulator                           =
>   Modulator {
>     mrModId            :: Word
>   , mrModSrc           :: ModSrc
>   , mrModDest          :: ModDestType
>   , mrModAmount        :: Double
>   , mrAmountSrc        :: ModSrc} deriving (Eq, Show)
>    
> defModulator                             = Modulator 0 defModSrc NoDestination 0 defModSrc
>
> data ModKey                              =
>   ModKey {
>     krSrc              :: ModSrc
>   , krDest             :: ModDestType
>   , krAmtSrc           :: ModSrc} deriving (Eq, Ord, Show)
>
> data ModDestType                         =
>     NoDestination
>   | ToPitch
>   | ToFilterFc
>   | ToVolume
>   | ToInitAtten
>   | ToChorus
>   | ToReverb
>   | ToLink Word deriving (Eq, Ord, Show)
>
> data ModSrcSource                        =
>     FromNoController
>   | FromNoteOnVel
>   | FromNoteOnKey
>   | FromLinked deriving (Eq, Ord, Show)
>
> data ModSrc                              =
>   ModSrc {
>     msMapping          :: Mapping
>   , msSource           :: ModSrcSource} deriving (Eq, Ord, Show)
>
> defModSrc                                = ModSrc defMapping FromNoController
>
> data Envelope                            =
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
>   , qqLoCutoffReson    :: ResonanceType
>   , qqHiCutoffReson    :: ResonanceType
>   , qqUseLFO           :: Bool
>   , qqChorusRate       :: Double
>   , qqChorusDepth      :: Double
>   , qqCascadeConfig    :: Int} deriving Show
>
> useModulators                            = qqUseModulators              defM
> chorusAllPercent                         = qqChorusAllPerCent           defM
> reverbAllPercent                         = qqReverbAllPerCent           defM
> useDefaultMods                           = qqUseDefaultMods             defM
> loCutoffReson                            = qqLoCutoffReson              defM
> hiCutoffReson                            = qqHiCutoffReson              defM
> useLFO                                   = qqUseLFO                     defM
> chorusRate                               = qqChorusRate                 defM
> chorusDepth                              = qqChorusDepth                defM
> cascadeConfig                            = qqCascadeConfig              defM
>
> defM                   :: ModulationSettings
> defM =
>   ModulationSettings {
>     qqUseModulators                      = True
>   , qqChorusAllPerCent                   = 0
>   , qqReverbAllPerCent                   = 0
>   , qqUseDefaultMods                     = True
>   , qqLoCutoffReson                      = ResonanceSVF2
>   , qqHiCutoffReson                      = ResonanceBandpass
>   , qqUseLFO                             = True
>   , qqChorusRate                         = 5.0   -- suggested default is 5 Hz
>   , qqChorusDepth                        = 0.001 -- suggested default is + or - 1/1000 (of the rate)
>   , qqCascadeConfig                      = 0
>   }

The End
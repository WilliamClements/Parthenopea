> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
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
> import Data.List ( singleton, foldl', partition )
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
> hasCycles m8rs                           = not $ null $ cyclicNodes $ makeGraph edgeList
>   where
>     nodeList           :: [(ModDestType, [Modulator])]
>                                          = filter outGoing (Map.assocs (compileMods m8rs))
>     edgeList           :: [(Node, [Node])]
>                                          = map
>                                              (BF.bimap nodeFrom (map (fromIntegral . mrModId)))
>                                              nodeList
>
>     nodeFrom           :: ModDestType → Node
>     nodeFrom mdt                         = case mdt of
>                                              ToLink mId       → fromIntegral mId
>                                              _                → error $ "only ToLink bears a ModDestType"
>                                                                           ++ " to be turned into a Node"
>
>     outGoing           :: (ModDestType, [Modulator]) → Bool
>     outGoing (mdt, _)                    = case mdt of
>                                              ToLink mId       → True
>                                              _                → False
>
> eliminateDanglingMods  :: [(Bool, (Int, [Modulator]))] → Int → [(Bool, (Int, [Modulator]))]
> eliminateDanglingMods triesIn mix        = newTry : eliminateDanglingMods (newTry : triesIn) (mix+1)
>   where
>     -- let's examine result of the previous generation
>     -- use it to produce next generation, dropping nodes that:
>     -- 1. expect linked sources but have none
>     --     or
>     -- 2. are superseded 
>     m8rsIn                               = profess
>                                              (mix <= 10)
>                                              "maximum of 10 tries exceeded..."
>                                              ((snd . snd) (head triesIn))
>     m8rsOut                              = filter shouldStay m8rsIn
>     newTry                               = (length m8rsOut == length m8rsIn, (mix, m8rsOut))
>
>     compiled                             = compileMods m8rsIn
>     superc             :: Map ModKey Word
>                                          = foldl' supercfr Map.empty m8rsIn
>
>     supercfr           :: Map ModKey Word → Modulator → Map ModKey Word
>     supercfr accum m8r                   = Map.insert newK newW accum
>       where
>         (newK, newW)                     = getModKeyPair m8r
>
>     checkLink          :: Modulator → Maybe [Modulator]
>     checkLink m8r@Modulator{mrModId}     = Map.lookup (ToLink mrModId) compiled
>
>     shouldStay m8r@Modulator{mrModId}    = linkageOk && not superceded
>       where
>         linkageOk                        = not (requiresLinks m8r) || maybe False (not . null) (checkLink m8r)
>         modKey                           = getModKey m8r
>         winner         :: Word           = fromJust (Map.lookup modKey superc)
>         superceded                       = mrModId /= winner
>
> requiresLinks, suppliesLink
>                        :: Modulator → Bool
> requiresLinks m8r@Modulator{mrModSrc}    = FromLinked == msSource mrModSrc
> suppliesLink m8r@Modulator{mrModDest}    = case mrModDest of
>                                              ToLink _      → True
>                                              _             → False
>
> siftMods m8rsIn                          = sifted
>   where
>     seed                                 = (False, (0, m8rsIn))
>     generations                          = eliminateDanglingMods (singleton seed) 0
>     successes                            = dropWhile (not . fst) generations
>     (_, (_, sifted))                     = head successes 
>
> compileMods            :: [Modulator] → Map ModDestType [Modulator]
> compileMods                              = foldl' nodeFolder Map.empty
>   where
>     nodeFolder accum m8r@Modulator{mrModDest}
>                                          =
>       let
>         soFar                            = fromMaybe [] (Map.lookup mrModDest accum)
>       in
>         Map.insert mrModDest (m8r : soFar) accum
>
> unpackModSrc           :: Word → Maybe ModSrc
> unpackModSrc wIn                         = mmapping
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
> addSrc                 :: ModSrc → Modulator → Maybe Modulator
> addSrc modSrc from                       = Just from{mrModSrc = modSrc}
>
> addDest                :: Word → Modulator → Maybe Modulator
> addDest wIn from
>   | (wIn .&. 0x8000) /= 0                = Just from{mrModDest = ToLink $ wIn .&. 0x7fff}
>   | otherwise                            = case wIn of
>                                              8       → Just from{mrModDest = ToFilterFc}
>                                              48      → Just from{mrModDest = ToInitAtten}
>                                              _ → Nothing
>
> addAmount              :: Int → Modulator → Maybe Modulator
> addAmount iIn from                       = if iIn == 0
>                                              then Nothing
>                                              else Just (from{mrModAmount = fromIntegral iIn})
>
> addAmtSrc              :: Maybe Modulator → ModSrc → Maybe Modulator
> addAmtSrc mm8n amtSrc@ModSrc{msSource}   = mm8n >>=   (\x → case msSource of
>                                                               FromLinked       → Nothing
>                                                               _                → Just x{mrAmountSrc = amtSrc})
> addAmtSrc'              :: ModSrc → Modulator → Maybe Modulator
> addAmtSrc' modSrc@ModSrc{msSource} m     = Just m >>= (\x → case msSource of
>                                                               FromLinked       → Nothing
>                                                               _                → Just x{mrAmountSrc = modSrc})
>
> defaultMods            :: [Modulator]    = if useDefaultMods
>                                              then [ makeDefaultMod 0 ms0 48 960     defModSrc
>                                                   , makeDefaultMod 1 ms1  8 (-2400) ms2 ]
>                                              else []
>  -- all three of these are negative unipolar, continuity varying                                                            
>   where                                                 --    cont    bipolar  neg  CC
>     ms0                                  = ModSrc   (Mapping Concave   False  True False) FromNoteOnVel
>     ms1                                  = ModSrc   (Mapping Linear    False  True False) FromNoteOnVel
>  -- some implementations do not include the amount source below in the second default modulator
>     ms2                                  = ModSrc   (Mapping Switch    False  True False) FromNoteOnVel
>
>     makeDefaultMod     :: Word → ModSrc → Word → Int → ModSrc → Modulator
>     makeDefaultMod mId ms igen amt amtSrc    = professIsJust'
>                                              $ Just defModulator{mrModId = mId}
>                                                >>= addSrc ms
>                                                >>= addDest igen
>                                                >>= addAmount amt
>                                                >>= addAmtSrc' amtSrc
>
> evaluateMods           :: ModDestType → Map ModDestType [Modulator] → NoteOn → Double
> evaluateMods md graph noon               = sum $ maybe [] (map (evaluateMod graph noon)) (Map.lookup md graph)
> 
> evaluateMod            :: Map ModDestType [Modulator] → NoteOn → Modulator → Double
> evaluateMod graph noon@NoteOn{noteOnVel, noteOnKey} m8r@Modulator{mrModId, mrModSrc, mrModAmount, mrAmountSrc}
>                                          = getValue mrModSrc * mrModAmount * getValue mrAmountSrc
>   where
>     getValue            :: ModSrc → Double
>     getValue msrc@ModSrc{msMapping, msSource}
>       | useModulators                    =
>           case msSource of
>             FromNoController     → 1
>             FromNoteOnVel        → evaluateNoteOn noteOnVel msMapping
>             FromNoteOnKey        → evaluateNoteOn noteOnKey msMapping
>             FromLinked           → evaluateMods (ToLink mrModId) graph noon
>       | otherwise                        = 0
>
> evaluateNoteOn         :: Int → Mapping → Double
>                                            -- TODO: express these "control" utilities as Signal Functions
> evaluateNoteOn n ping                    = controlDenormal ping (fromIntegral n / 128) (0, 1)
>
> calculateModFactor     :: String → Modulation → ModDestType → ModSignals → NoteOn → Double
> calculateModFactor tag m8n@Modulation{modGraph, toPitchSummary, toFilterFcSummary, toVolumeSummary}
>                    md msig@ModSignals{srModEnvPos, srModLfoPos, srVibLfoPos} noon
>  | traceNever msg False                  = undefined
>  | otherwise                             = fromCents (xmodEnv + xmodLfo + xvibLfo + xmods)
>  where
>    targetListIn        :: [Double]       = case md of
>                                              ToPitch        → toPitchSummary
>                                              ToFilterFc     → toFilterFcSummary
>                                              ToVolume       → toVolumeSummary
>                                              _              → error $ "Error in calculateModFactor "
>                                                                       ++ show tag ++ " " ++ show md
>    targetList                            = profess
>                                              (length targetListIn >= 3)
>                                              "bad targetList"
>                                              targetListIn
>
>    xmodEnv                 :: Double     = srModEnvPos * head targetList
>    xmodLfo                               = srModLfoPos * (targetList !! 1)
>    xvibLfo                               = srVibLfoPos  * (targetList !! 2)
>    xmods                                 = evaluateMods md modGraph noon
>
>    msg                                   = unwords ["calculateModFactor: "
>                                                     , show tag,    " + "
>                                                     , show xmodEnv, " + "
>                                                     , show xmodLfo, " + "
>                                                     , show xvibLfo, " + "
>                                                     , show xmods,   " = "
>                                                     , show (xmodEnv+xmodLfo+xvibLfo+xmods), " => "
>                                                     , show (fromCents (xmodEnv+xmodLfo+xvibLfo+xmods))]
>
> addResonance           :: ∀ p . Clock p ⇒ NoteOn → Modulation → Signal p (Double, ModSignals) Double
> addResonance noon m8n@Modulation{mLowPass}
>                                          = maybe delay' makeSF mLowPass
>   where
>     lp@LowPass{lowPassFc, lowPassQ}      = fromJust mLowPass
>
>     makeSF             :: LowPass → Signal p (Double, ModSignals) Double
>     makeSF _                             = case useResonanceType of
>                                              ResonanceNone         → error "usually don't need to protect against ResonanceNone"
>                                              ResonanceLowpass      → procButter
>                                              ResonanceBandpass     → procBandpass
>                                              ResonanceSVF          → procSVF
>
>     modulateFc         :: ModSignals → Double
>     modulateFc msig                      =
>       clip (20, 20000) (lowPassFc * calculateModFactor
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
>     procBandpass       :: Signal p (Double, ModSignals) Double
>     procBandpass                           = 
>       proc (x, msig) → do
>         let fc = modulateFc msig
>         let bw = lowPassQ / 10
>         y1 ← filterLowPassBW              ⤙ (x, fc)
>         y2 ← filterBandPass 2             ⤙ (x, fc, bw)
>         let y' = resonate x fc (y1 + y2)
>         outA                             ⤙ y'
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
>       | traceNever msg' False            = undefined
>       | otherwise                        = y'
>       where
>         y'                               = checkForNan y "resonate y"
>         msg'                             = unwords ["resonate\nsin  = ", show (checkForNan  x "resonate x")
>                                                           , "\nfc   = ", show (checkForNan fc "resonate fc")
>                                                           , "\nsout = ", show y']
>
> deriveModTarget        :: Maybe Int → Maybe Int → Maybe Int → ModTarget
> deriveModTarget toPitch toFilterFc toVolume
>                                          =
>   ModTarget (maybe 0 fromIntegral toPitch)
>             (maybe 0 fromIntegral toFilterFc)
>             (maybe 0 fromIntegral toVolume)
>
> deriveLFO              :: Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe LFO
> deriveLFO del mfreq toPitch toFilterFc toVolume
>   | traceNever msg False                 = undefined
>   | otherwise                            = if useLFO && anyJust
>                                              then Just $ LFO (fromTimecents del)
>                                                              freq
>                                                              (deriveModTarget toPitch toFilterFc toVolume)
>                                              else Nothing
>   where
>     freq               :: Double         = fromAbsoluteCents $ maybe 0 (clip (-16000, 4500)) mfreq
>     anyJust            :: Bool           = isJust toPitch || isJust toFilterFc || isJust toVolume
>     msg                                  = unwords ["deriveLFO ", show toPitch,    " "
>                                                                 , show toFilterFc, " "
>                                                                 , show toVolume]
>
> doLFO                  :: ∀ p . Clock p ⇒ Maybe LFO → Signal p () Double
> doLFO                                    = maybe (constA 0) makeSF
>   where
>     makeSF             :: LFO → Signal p () Double
>     makeSF lfo@LFO{lfoDelay, lfoFrequency}
>                                          = 
>       proc _ → do
>         y ← triangleWave lfoFrequency    ⤙ ()
>         z ← delayLine lfoDelay           ⤙ y
>         outA                             ⤙ z  
>
> triangleWaveTable      :: Table          = tableSinesN 16384 
>                                                          [      1,  0, -0.5,  0,  0.3,   0
>                                                           , -0.25,  0,  0.2,  0, -0.167, 0
>                                                           ,  0.14,  0, -0.125]
>
> triangleWave           :: ∀ p . Clock p ⇒ Double → Signal p () Double
> triangleWave freq                        = 
>   proc _ → do
>     osc triangleWaveTable 0              ⤙ freq
>
> modVib                 :: ∀ p . Clock p ⇒ Double → Double → Signal p Double Double
> modVib rate depth                        =
>   proc sin → do
>     vib   ← osc sineTable 0              ⤙ rate
>     sout  ← delayLine1 0.2               ⤙ (sin,0.1+depth*vib)
>     outA                                 ⤙ sout

see source https://karmafx.net/docs/karmafx_digitalfilters.pdf for the Notch case

> filterSVF              :: ∀ p . Clock p ⇒ Double → Signal p (Double,Double) Double
> filterSVF initQ
>   | traceNow msg False                   = undefined
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
>     msg                                  = unwords ["filterSVF initQ = " ++ show initQ
>                                                  ++ " " ++ show (fromCentibels' initQ)]

Controller Curves =====================================================================================================

> qTableSize             :: Int            = 1024
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
>         x              :: Double         = fromIntegral i / fromIntegral qTableSize

The use of these functions requires that their input is normalized between 0 and 1
(And the output is normalized, too)

> funLinear              :: Double → Double
> funLinear d                              = d
>
> funConcave d
>   | d >= 1                               = 1
>   | otherwise                            = quarterCircleTable ! truncate (d * fromIntegral qTableSize)
>
> funConvex d
>   | (1 - d) >= 1                         = 1
>   | otherwise                            = 1 - (quarterCircleTable ! truncate ((1 - d) * fromIntegral qTableSize))
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
>                                            "inverted range in controlDenormal"
>                                            (hi - lo)
>     dNorm =   (dIn - lo) / scale
>
> controlUniPolar        :: Mapping → Double → Double
> controlUniPolar ping@Mapping{msContinuity, msMax2Min} dIn
>                                          = fun xStart
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
> controlBiPolar ping@Mapping{msContinuity, msMax2Min} dIn
>                                          = dOut
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
>     srModEnvPos        :: Double
>   , srModLfoPos        :: Double
>   , srVibLfoPos        :: Double} deriving (Show)
>
> data ModTarget =
>   ModTarget {
>     mtPitch            :: Double
>   , mtFilterFc         :: Double
>   , mtVolume           :: Double} deriving (Eq, Show)
>
> chooseFromModTarget    :: ModDestType → ModTarget → Double
> chooseFromModTarget md mt@ModTarget{mtPitch, mtFilterFc, mtVolume}
>                                          = case md of
>                                              ToPitch     → mtPitch
>                                              ToFilterFc  → mtFilterFc
>                                              ToVolume    → mtVolume
>                                              _           → error ("ModTarget only deals with toPitch"
>                                                               ++ ", toFilterFc, and toVolume")
> defModTarget                             = ModTarget 0 0 0
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
>   , lModTarget         :: ModTarget} deriving (Eq, Show)
>
> data Modulation =
>   Modulation {
>     mLowPass           :: Maybe LowPass
>   , mModEnv            :: Maybe Envelope
>   , mModLfo            :: Maybe LFO
>   , mVibLfo            :: Maybe LFO
>   , toPitchSummary     :: [Double]
>   , toFilterFcSummary  :: [Double]
>   , toVolumeSummary    :: [Double]
>   , modGraph           :: Map ModDestType [Modulator]} deriving (Eq, Show)
>
> defModulation          :: Modulation
> defModulation                            = Modulation
>                                             Nothing Nothing Nothing Nothing
>                                             [] [] [] Map.empty
>
> data Modulator =
>   Modulator {
>     mrModId            :: Word
>   , mrModSrc           :: ModSrc
>   , mrModDest          :: ModDestType
>   , mrModAmount        :: Double
>   , mrAmountSrc        :: ModSrc} deriving (Eq, Show)
>    
> defModulator           :: Modulator
> defModulator                             = Modulator 0 defModSrc NoDestination 0 defModSrc
>
> data ModKey =
>   ModKey {
>     krSrc              :: ModSrc
>   , krDest             :: ModDestType
>   , krAmtSrc           :: ModSrc} deriving (Eq, Ord, Show)
>
> getModKeyPair          :: Modulator → (ModKey, Word)
> getModKeyPair m8r@Modulator{mrModId}     = (getModKey m8r, mrModId)
>
> getModKey              :: Modulator -> ModKey
> getModKey m8r@Modulator{mrModSrc, mrModDest, mrAmountSrc}
>                                          = ModKey mrModSrc mrModDest mrAmountSrc
>
> data ModDestType =
>     NoDestination
>   | ToPitch
>   | ToFilterFc
>   | ToVolume
>   | ToInitAtten
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
>     , msSource         :: ModSrcSource} deriving (Eq, Ord, Show)
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
>   , eModTarget         :: ModTarget} deriving (Eq, Show)
>
> data ModulationSettings =
>   ModulationSettings {
>     qqUseModulators    :: Bool
>   , qqUseDefaultMods   :: Bool
>   , qqUseResonanceType :: ResonanceType
>   , qqUseLFO           :: Bool} deriving Show
>
> useModulators                            = qqUseModulators              defM
> useDefaultMods                           = qqUseDefaultMods             defM
> useResonanceType                         = qqUseResonanceType           defM
> useLFO                                   = qqUseLFO                     defM
>
> defM                   :: ModulationSettings
> defM =
>   ModulationSettings {
>     qqUseModulators                      = True
>   , qqUseDefaultMods                     = True
>   , qqUseResonanceType                   = ResonanceLowpass -- ResonanceBandpass -- ResonanceNone
>   , qqUseLFO                             = True}
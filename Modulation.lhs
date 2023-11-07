> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module Modulation where
>
> import qualified Data.Audio              as A
> import qualified Data.Bifunctor          as BF
> import Data.Bits
> import Data.Graph (Graph)
> import qualified Data.Graph              as Graph
> import Data.List ( singleton, foldl', partition )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, mapMaybe )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..) )
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import Parthenopea

Modulator management ==================================================================================================

> resModulators         :: Modulation → [Modulator] → Modulation
> resModulators m8n m8rs
>   | traceNow msg False                   = undefined
>   | otherwise                            = assignModGraph m8n (compileMods (defaultMods ++ m8rs'))
>   where
>     msg                                  = unwords ["resModulators ", show (length m8rs)]
>     (linked, other)                      = splitMods m8rs
>     m8rs'                                = profess
>                                              (not $ hasCycles linked)
>                                              "cycles in modulator graph"
>                                              (linked ++ other)
>         
> assignModGraph         :: Modulation → Map ModDestType [Modulator] → Modulation
> assignModGraph m8n mgraph
>   | traceNow msg False               = undefined
>   | otherwise                        = m8n{modGraph = mgraph}
>   where
>     msg = unwords ["assignModGraph ", show mgraph]
>
> hasCycles              :: [Modulator] → Bool
> hasCycles ms
>   | traceNow msg False                   = undefined
>   | otherwise                            = not $ null $ cyclicNodes mgraph
>   where
>     msg                                  = unwords ["hasCycles edgeList ", show edgeList]
>     mgraph             :: Graph          = makeGraph edgeList
>
>     edgeList           :: [(Node, [Node])]
>                                          = map
>                                              (BF.bimap lookup (map (fromIntegral . mrModId)))
>                                              (Map.toList (compileMods ms))
>
>     lookup             :: ModDestType → Node
>     lookup mdt                           = case mdt of
>                                              ToLink mId       → fromIntegral mId
>                                              _                → error $ "only ToLink bears a ModDestType"
>                                                                           ++ " to be turned into a Node"
>
> generate               :: [(Bool, (Int, [Modulator]))] → Int → [(Bool, (Int, [Modulator]))]
> generate triesIn mix  
>   | traceNow msg False                   = undefined
>   | otherwise                            = newTry : generate triesOut (mix+1)
>   where
>     -- let's examine result of the previous generation
>     -- use it to produce next generation, dropping nodes that expect linked sources but have none
>     m8rsIn                               = profess
>                                              (mix <= 10)
>                                              "maximum of 10 tries exceeded..."
>                                              ((snd . snd) (head triesIn))
>     graph                                = compileMods m8rsIn
>     m8rsOut                              = filter (\m → not (hasModSource m) || hasModSources graph m) m8rsIn
>     newTry                               = (length m8rsOut == length m8rsIn, (mix, m8rsOut))
>     triesOut                             = newTry : triesIn
>
>     msg                                  = unwords ["generate ", show (length m8rsIn)]
>
> hasModSource, hasModDest
>                        :: Modulator → Bool
> hasModSource m8r@Modulator{mrModSrc}     = FromLinked == msType mrModSrc
> hasModDest   m8r@Modulator{mrModDest}    = case mrModDest of
>                                              ToLink _      → True
>                                              _             → False
>
> hasModSources          :: Map ModDestType [Modulator] → Modulator → Bool
> hasModSources graph m                    = isJust (Map.lookup (ToLink (mrModId m)) graph)
>
> splitMods              :: [Modulator] → ([Modulator], [Modulator])
> splitMods m8rsIn
>   | traceNow msg False                   = undefined
>   | otherwise                            = (m8rsSplit, other)
>   where
>     -- split off and "forget" (see above) non-linking modulators
>     (linked, other)                      = partition (\m → hasModSource m || hasModDest m) m8rsIn
>
>     seed                                 = (False, (0, linked))
>     generations                          = generate (singleton seed) 0
>     successes                            = dropWhile (not . fst) generations
>     (_, (_, m8rsSplit))                  = head successes 
>
>     msg                                  = unwords ["splitMods ", show (length linked, length other)]
>
> compileMods        :: [Modulator] → Map ModDestType [Modulator]
> compileMods                          = foldl' nodeFolder Map.empty
>   where
>     nodeFolder accum m8r@Modulator{mrModDest}
>                                      =
>       let
>         soFar      :: [Modulator]    = fromMaybe [] (Map.lookup mrModDest accum)
>       in
>         Map.insert mrModDest (m8r : soFar) accum
>
> unpackSrc              :: Word → Maybe ModSrc
> unpackSrc wIn                            =     Just defModSrc
>                                            >>= addIndex
>                                            >>= addCCBit
>                                            >>= addMax2Min
>                                            >>= addBiPolar
>   where
>     index                                = wIn `mod` 128
>     ccBit                                = (wIn `shift` (-7)) `mod` 2
>     max2Min                              = (wIn `shift` (-8)) `mod` 2
>     bipolar                              = (wIn `shift` (-9)) `mod` 2
>
>     addIndex from                        = case index of
>                                              0      → Just from{msType = FromNoController}
>                                              2      → Just from{msType = FromNoteOnVel}
>                                              3      → Just from{msType = FromNoteOnKey}
>                                              127    → Just from{msType = FromLinked}
>                                              _ → Nothing
>     addCCBit from                        = if ccBit /= 0   then Nothing
>                                                            else Just from{msCCBit = False}
>     addMax2Min from                      = if max2Min /= 0 then Just from{msMax2Min = True}
>                                                            else Just from{msMax2Min = False}
>     addBiPolar from                      = if bipolar /= 0 then Just from{msBiPolar = True}
>                                                            else Just from{msBiPolar = False}
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
> addAmtSrc mmod modSrc@ModSrc{msType}     = mmod >>= (\x → case msType of
>                                                             FromLinked          → Nothing
>                                                             _                   → Just x{mrAmountSrc = modSrc})
> addAmtSrc'              :: ModSrc → Modulator → Maybe Modulator
> addAmtSrc' modSrc@ModSrc{msType} m       = Just m >>= (\x → case msType of
>                                                               FromLinked        → Nothing
>                                                               _                 → Just x{mrAmountSrc = modSrc})
>
> defaultMods            :: [Modulator]
>                                          -- [makeDefaultMod ms0 48 960, makeDefaultMod 8 (-2400), makeDefaultMod 8 (-2400)]
> defaultMods                              = if useDefaultMods
>                                              then [ makeDefaultMod ms0 48 960     defModSrc
>                                                   , makeDefaultMod ms1  8 (-2400) ms2 ]
>                                              else []
>                                                              
>   where
>     ms0                :: ModSrc         = ModSrc   Concave   False  True False FromNoteOnVel
>     ms1                :: ModSrc         = ModSrc   Linear    False  True False FromNoteOnVel
>     ms2                :: ModSrc         = ModSrc   Switch    False  True False FromNoteOnVel
>
>     makeDefaultMod     :: ModSrc → Word → Int → ModSrc → Modulator
>     makeDefaultMod ms igen amt amtSrc    = professIsJust'
>                                              $ Just defModulator
>                                                >>= addSrc ms
>                                                >>= addDest igen
>                                                >>= addAmount amt
>                                                >>= addAmtSrc' amtSrc
>
> evaluateMods           :: ModDestType → Modulation → (AbsPitch, Volume) → Double
> evaluateMods md m8n@Modulation{modGraph} pv
>                                          = sum $ maybe [] (map evaluateMod) (Map.lookup md modGraph)
>   where
>     evaluateMod        :: Modulator → Double
>     evaluateMod m8r@Modulator{mrModId, mrModSrc, mrModAmount, mrAmountSrc}
>                                          = srcValue mrModSrc * mrModAmount * srcValue mrAmountSrc
>       where
>         rawValue        :: ModSrc → (Double, (Double, Double))
>         rawValue msrc@ModSrc{msType}
>           | useModulators                = case msType of
>                                              FromNoController     → (1, (0,1))
>                                              FromNoteOnVel        → (fromNoteOn (snd pv) False False, (0, 128))
>                                              FromNoteOnKey        → (fromNoteOn (fst pv) False False, (0, 128))
>                                                                                              -- WOX again, (0,1)?
>                                              FromLinked           → (evaluateMods (ToLink mrModId) m8n pv, (0, 1))
>           | otherwise                    = (1, (0,1)) -- WOX (0,1)?
>
>         srcValue       :: ModSrc → Double
>         srcValue msrc@ModSrc{msContinuity, msMax2Min, msBiPolar} = result
>           where
>             (val, (vMin, vMax)) = rawValue msrc
>
>             result
>               | Linear == msContinuity && not msMax2Min && not msBiPolar
>                                          = val
>               | Concave == msContinuity && msMax2Min && not msBiPolar
>                                          = -20/96 * log (ranged ^ 2 / range ^ 2)
>               | Switch == msContinuity && msMax2Min && not msBiPolar
>                                          = if val < (vMin + vMax) / 2
>                                              then vMin -- WOX does it need to be flipped at any point?
>                                              else vMax
>               | otherwise                = val
>
>             ranged = val - vMin
>             range = vMax - vMin
>
> calculateModFactor     :: String → Modulation → ModDestType → ModSignals → (AbsPitch, Volume) → Double
> calculateModFactor tag m8n@Modulation{modGraph, toPitchSummary, toFilterFcSummary, toVolumeSummary}
>                    md msig@ModSignals{srModEnvPos, srModLfoPos, srVibLfoPos} pv
>  | traceNever msg False                  = undefined
>  | otherwise                             = fact
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
>    x1                                    = srModEnvPos * head targetList
>    x2                                    = srModLfoPos * (targetList !! 1)
>    x3                                    = srVibLfoPos * (targetList !! 2)
>    x4                                    = evaluateMods md m8n pv
>    fact                                  = fromCents (x1 + x2 + x3 + x4)
>
>    msg                                   = unwords ["calculateModFactor: "
>                                                     , show x1, " + "
>                                                     , show x2, " + "
>                                                     , show x3, " + "
>                                                     , show x4, " = ", show (x1+x2+x3+x4), " => ", show fact]
>
> addResonance           :: ∀ p . Clock p ⇒ Reconciled → Modulation → Signal p (Double, ModSignals) Double
> addResonance recon@Reconciled{rNoteOnVel, rNoteOnKeyNumber} m@Modulation{mLowPass, toFilterFcSummary}
>                                          = maybe delay' makeSF mLowPass
>   where
>     
>     makeSF             :: LowPass → Signal p (Double, ModSignals) Double
>     makeSF lp@LowPass{lowPassFc, lowPassQ}
>       | traceIf msg False                = undefined
>       | otherwise                        =
>       proc (x, msig) → do
>         let fc = lowPassFc * calculateModFactor
>                                "addResonance"
>                                m
>                                ToFilterFc
>                                msig
>                                (rNoteOnVel, rNoteOnKeyNumber)
>         y ← filterLowPassBW              ⤙ (x, fc)
>         outA                             ⤙ resonate x fc y
>       where
>         msg = unwords ["addResonance/makeSF ", show lowPassFc ]
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
>       | otherwise                        = y
>       where
>         msg'                             = unwords ["resonate\nsin  = ", show x
>                                                           , "\nfc   = ", show fc
>                                                           , "\nsout = ", show y]
>
> deriveModTarget        :: Maybe Int → Maybe Int → Maybe Int → ModTarget
> deriveModTarget toPitch toFilterFc toVolume
>                                          =
>   ModTarget (maybe 0 fromIntegral toPitch)
>             (maybe 0 fromIntegral toFilterFc)
>             (maybe 0 fromIntegral toVolume)
>
> deriveLFO              :: Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe LFO
> deriveLFO del freq toPitch toFilterFc toVolume
>   | traceNever msg False                 = undefined
>   | otherwise                            = if useLFO && anyJust
>                                              then Just $ LFO (fromTimecents del)
>                                                              (maybe 8.176 fromAbsoluteCents freq)
>                                                              (deriveModTarget toPitch toFilterFc toVolume)
>                                              else Nothing
>   where
>     anyJust        :: Bool           = isJust toPitch || isJust toFilterFc || isJust toVolume
>     msg                              = unwords ["deriveLFO ", show toPitch,    " "
>                                                             , show toFilterFc, " "
>                                                             , show toVolume]
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
> triangleWaveTable      :: Table
> triangleWaveTable                        = tableSinesN 16384 
>                                                          [      1,  0, -0.5,  0,  0.3,   0
>                                                           , -0.25,  0,  0.2,  0, -0.167, 0
>                                                           ,  0.14,  0, -0.125]
>
> triangleWave           :: ∀ p . Clock p ⇒ Double → Signal p () Double
> triangleWave freq                           = 
>   proc _ → do
>     osc triangleWaveTable 0 ⤙ freq
>
> modVib                 :: ∀ p . Clock p ⇒ Double → Double → Signal p Double Double
> modVib rate depth =
>   proc sin → do
>     vib   ← osc sineTable 0  ⤙ rate
>     sout  ← delayLine1 0.2   ⤙ (sin,0.1+depth*vib)
>     outA ⤙ sout
>

Testing ===============================================================================================================

> aEqual a b
>   | a /= b                               = error "They had to be equal!"
>   | otherwise                            = a
>
> modulationTestSetup   :: [Modulator]
> modulationTestSetup                      = [m8r0, m8r1, m8r2, m8r3]
>   where
>     m8r0 = defModulator{mrModId = 0, mrModSrc = defModSrc{msType = FromLinked}, mrModDest = ToFilterFc}
>     m8r1 = defModulator{mrModId = 1, mrModDest = ToInitAtten}
>     m8r2 = defModulator{mrModId = 2, mrModSrc = defModSrc{msType = FromNoteOnKey}, mrModDest = ToFilterFc}
>     m8r3 = defModulator{mrModId = 3, mrModDest = ToLink 2}
>  
> modulationTest001     :: IO ()
> modulationTest001                       = do
>   let m8rsIn                            = modulationTestSetup
>   let m8r4 = defModulator{mrModId = 4, mrModSrc = defModSrc{msType = FromLinked}, mrModDest = ToLink 5}
>   let m8r5 = defModulator{mrModId = 5, mrModSrc = defModSrc{msType = FromLinked}, mrModDest = ToLink 4} 
>   let m8rsOut                           = m8rsIn ++ [m8r4, m8r5]
>
>   let m8n' = resModulators defModulation m8rsOut
>   print m8n'
>   return ()

Type declarations =====================================================================================================

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
>     mrModId          :: Word
>   , mrModSrc         :: ModSrc
>   , mrModDest        :: ModDestType
>   , mrModAmount      :: Double
>   , mrAmountSrc      :: ModSrc} deriving (Eq, Show)
>    
> defModulator           :: Modulator
> defModulator                             = Modulator 0 defModSrc NoDestination 0 defModSrc
>
> data ModDestType =
>     NoDestination
>   | ToPitch
>   | ToFilterFc
>   | ToVolume
>   | ToInitAtten
>   | ToLink Word deriving (Eq, Ord, Show)
>
> data ModSrcType =
>     FromNoController
>   | FromNoteOnVel
>   | FromNoteOnKey
>   | FromLinked deriving (Eq, Show)
>
> data ModSrc =
>   ModSrc {
>       msContinuity     :: Continuity
>     , msCCBit          :: Bool
>     , msMax2Min        :: Bool
>     , msBiPolar        :: Bool
>     , msType           :: ModSrcType} deriving (Eq, Show)
>
> defModSrc              :: ModSrc
> defModSrc                                = ModSrc Linear False False False FromNoController
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
> data Segments =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show
>
> data Reconciled =
>   Reconciled {
>     rSampleMode        :: A.SampleMode
>   , rSampleRate        :: Double
>   , rStart             :: Word
>   , rEnd               :: Word
>   , rLoopStart         :: Word
>   , rLoopEnd           :: Word
>   , rRootKey           :: AbsPitch
>   , rForceKey          :: Maybe AbsPitch
>   , rForceVel          :: Maybe Volume
>   , rNoteOnVel         :: Volume
>   , rNoteOnKeyNumber   :: AbsPitch
>   , rAttenuation       :: Double
>   , rVolEnv            :: Maybe Envelope
>   , rPitchCorrection   :: Maybe Double
>   , rModulation        :: Modulation
>   , rEffects           :: Effects} deriving (Eq, Show)
>
> data Effects =
>   Effects {
>     efChorus           :: Maybe Double
>   , efReverb           :: Maybe Double
>   , efPan              :: Maybe Double} deriving (Eq, Show)
>
> data ModulationSettings =
>   ModulationSettings {
>     qqUseModulators        :: Bool
>   , qqUseDefaultMods       :: Bool
>   , qqUseLowPass           :: Bool
>   , qqUseLFO               :: Bool} deriving Show
>
> defM                   :: ModulationSettings
> defM =
>   ModulationSettings {
>     qqUseModulators                      = True
>   , qqUseDefaultMods                     = False
>   , qqUseLowPass                         = True
>   , qqUseLFO                             = True}
>
> useModulators                            = qqUseModulators              defM
> useDefaultMods                           = qqUseDefaultMods             defM
> useLowPass                               = qqUseLowPass                 defM
> useLFO                                   = qqUseLFO                     defM

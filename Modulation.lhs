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
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( Signal, AudioSample, Clock(..) )
> import Euterpea.Music ( Volume, AbsPitch, Dur )
> import FRP.UISF.AuxFunctions ( ArrowCircuit(delay), constA, DeltaT )
> import Parthenopea
  
Modulator management ==================================================================================================

> resModulators         :: Modulation → [Modulator] → Modulation
> resModulators m8n m8rs
>   | traceIf msg False                    = undefined
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
>   | traceIf msg False                = undefined
>   | otherwise                        = m8n{modGraph = mgraph}
>   where
>     msg = unwords ["assignModGraph ", show mgraph]
>
> hasCycles              :: [Modulator] → Bool
> hasCycles ms
>   | traceIf msg False                    = undefined
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
> elimDanglingModSources :: [(Bool, (Int, [Modulator]))] → Int → [(Bool, (Int, [Modulator]))]
> elimDanglingModSources triesIn mix  
>   | traceIf msg False                    = undefined
>   | otherwise                            = newTry : elimDanglingModSources (newTry : triesIn) (mix+1)
>   where
>     -- let's examine result of the previous generation
>     -- use it to produce next generation, dropping nodes that expect linked sources but have none
>     m8rsIn                               = profess
>                                              (mix <= 10)
>                                              "maximum of 10 tries exceeded..."
>                                              ((snd . snd) (head triesIn))
>
>     lookup             :: Modulator → Maybe [Modulator]
>     lookup m8r                           = Map.lookup (ToLink (mrModId m8r)) (compileMods m8rsIn)
>
>     m8rsOut                              = filter shouldStay m8rsIn
>
>     shouldStay         :: Modulator → Bool
>     shouldStay m8r                       = not (requiresLinks m8r) || maybe False (not . null) (lookup m8r)
>
>     newTry                               = (length m8rsOut == length m8rsIn, (mix, m8rsOut))
>
>     msg                                  = unwords ["elimDanglingModSources ", show (length m8rsIn)]
>
> requiresLinks, suppliesLink
>                        :: Modulator → Bool
> requiresLinks m8r@Modulator{mrModSrc}    = FromLinked == msSource mrModSrc
> suppliesLink m8r@Modulator{mrModDest}    = case mrModDest of
>                                              ToLink _      → True
>                                              _             → False
>
> splitMods              :: [Modulator] → ([Modulator], [Modulator])
> splitMods m8rsIn
>   | traceIf msg False                    = undefined
>   | otherwise                            = (m8rsSplit, other)
>   where
>     -- split off non-linking modulators
>     (linked, other)                      = partition (\m → requiresLinks m || suppliesLink m) m8rsIn
>
>     seed                                 = (False, (0, linked))
>     generations                          = elimDanglingModSources (singleton seed) 0
>     successes                            = dropWhile (not . fst) generations
>     (_, (_, m8rsSplit))                  = head successes 
>
>     msg                                  = unwords ["splitMods ", show (length linked, length other)]
>
> compileMods            :: [Modulator] → Map ModDestType [Modulator]
> compileMods                              = foldl' nodeFolder Map.empty
>   where
>     nodeFolder accum m8r@Modulator{mrModDest}
>                                          =
>       let
>         soFar          :: [Modulator]    = fromMaybe [] (Map.lookup mrModDest accum)
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
> addAmtSrc mmod modSrc@ModSrc{msSource}   = mmod >>=   (\x → case msSource of
>                                                               FromLinked       → Nothing
>                                                               _                → Just x{mrAmountSrc = modSrc})
> addAmtSrc'              :: ModSrc → Modulator → Maybe Modulator
> addAmtSrc' modSrc@ModSrc{msSource} m     = Just m >>= (\x → case msSource of
>                                                               FromLinked       → Nothing
>                                                               _                → Just x{mrAmountSrc = modSrc})
>
> defaultMods            :: [Modulator]
>                                          -- [makeDefaultMod ms0 48 960
>                                          -- , makeDefaultMod 8 (-2400)
>                                          -- , makeDefaultMod 8 (-2400)]
> defaultMods                              = if useDefaultMods
>                                              then [ makeDefaultMod ms0 48 960     defModSrc
>                                                   , makeDefaultMod ms1  8 (-2400) ms2 ]
>                                              else []
>                                                              
>   where
>     ms0                :: ModSrc         = ModSrc   (Mapping Concave   False  True False) FromNoteOnVel
>     ms1                :: ModSrc         = ModSrc   (Mapping Linear    False  True False) FromNoteOnVel
>     ms2                :: ModSrc         = ModSrc   (Mapping Switch    False  True False) FromNoteOnVel
>
>     makeDefaultMod     :: ModSrc → Word → Int → ModSrc → Modulator
>     makeDefaultMod ms igen amt amtSrc    = professIsJust'
>                                              $ Just defModulator
>                                                >>= addSrc ms
>                                                >>= addDest igen
>                                                >>= addAmount amt
>                                                >>= addAmtSrc' amtSrc
>
> evaluateMods           :: ModDestType → Map ModDestType [Modulator] → (AbsPitch, Volume) → Double
> evaluateMods md graph (p, v)             = sum $ maybe [] (map (evaluateMod graph v p)) (Map.lookup md graph)
> 
> evaluateMod            :: Map ModDestType [Modulator] → Velocity → KeyNumber → Modulator → Double
> evaluateMod graph vel key m8r@Modulator{mrModId, mrModSrc, mrModAmount, mrAmountSrc}
>                                          = getValue mrModSrc * mrModAmount * getValue mrAmountSrc
>   where
>     getValue            :: ModSrc → Double
>     getValue msrc@ModSrc{msMapping, msSource}
>       | useModulators                    =
>           case msSource of
>             FromNoController     → 1
>             FromNoteOnVel        → fromNoteOn vel msMapping
>             FromNoteOnKey        → fromNoteOn key msMapping
>             FromLinked           → evaluateMods (ToLink mrModId) graph (key, vel)
>       | otherwise                        = 0
>
> fromNoteOn             :: Int → Mapping → Double
> fromNoteOn n ping@Mapping{msContinuity, msBiPolar, msMax2Min}
>                                          = controlDenormal ping (fromIntegral n / 128) (0, 1)
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
>    x4                                    = evaluateMods md modGraph pv
>    fact                                  = fromCents (x1 + x2 + x3 + x4)
>
>    msg                                   = unwords ["calculateModFactor: "
>                                                     , show x1, " + "
>                                                     , show x2, " + "
>                                                     , show x3, " + "
>                                                     , show x4, " = ", show (x1+x2+x3+x4), " => ", show fact]
>
> addResonance           :: ∀ p . Clock p ⇒ Velocity → KeyNumber → Modulation → Signal p (Double, ModSignals) Double
> addResonance noteOnVel noteOnKey m8n@Modulation{mLowPass, toFilterFcSummary}
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
>                                m8n
>                                ToFilterFc
>                                msig
>                                (noteOnVel, noteOnKey)
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
>     m8r0 = defModulator{mrModId = 0, mrModSrc = defModSrc{msSource = FromLinked}, mrModDest = ToFilterFc}
>     m8r1 = defModulator{mrModId = 1, mrModDest = ToInitAtten}
>     m8r2 = defModulator{mrModId = 2, mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                    , mrModAmount = 0.5
>                                    , mrModDest = ToFilterFc}
>     m8r3 = defModulator{mrModId = 3, mrModDest = ToLink 2}
>  
> modulationTest001     :: IO ()
> modulationTest001                       = do
>   let m8rsIn                            = modulationTestSetup
>   let m8r4 = defModulator{mrModId = 4, mrModSrc = defModSrc{msSource = FromLinked}, mrModDest = ToLink 5}
>   let m8r5 = defModulator{mrModId = 5, mrModSrc = defModSrc{msSource = FromLinked}, mrModDest = ToLink 4} 
>   let m8rsOut                           = m8rsIn ++ [m8r4, m8r5]
>
>   let m8n' = resModulators defModulation m8rsOut
>   print m8n'
>   return ()
>
> modulationTest002      :: IO ()
> modulationTest002                       = do
>   let m8rIn                             = modulationTestSetup !! 2
>   let graph                             = compileMods [m8rIn]
>
>   let eval                              = evaluateMods ToFilterFc graph 
>   let results                           = [eval (x, 64) | x ← [0..127]]
>   print results
>   return ()
>
> modulationTest003      :: IO ()
> modulationTest003                       = do
>   mapM_ tryIt allMappings
>     where
>       m8rIn                             = modulationTestSetup !! 2
>
>       tryIt            :: Mapping → IO ()
>       tryIt ping
>         | traceNow msg False          = undefined
>         | otherwise                   = do
>         traceIO ""
>         traceIO (show results)
>         traceIO ""
>         return ()
>
>         where
>           results                        = [controlDenormal ping (conv x) (0, 1) | x ← [0..127]]
>
>           conv         :: Int → Double
>           conv midi                      = fromIntegral midi / fromIntegral qMidiSize
>
>           msg                            = unwords ["mapping = ", show ping]
>
> modulationTest004      :: Mapping → [Double]
> modulationTest004 ping
>   | traceNow msg False                   = undefined
>   | otherwise                            = results
>   where
>     results                              = [controlDenormal ping (conv x) (0, 1) | x ← [0..127]]
>
>     conv         :: Int → Double
>     conv midi                            = fromIntegral midi / fromIntegral qMidiSize
>
>     msg = unwords ["modulationTest004 ", show ping]

Controller Curves =====================================================================================================

> qTableSize             :: Int            = 1024
> qMidiSize              :: Int            = 128
> qStepSize                                = qTableSize `div` qMidiSize
>
> quarterCircleTable     :: Array Int Double
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
> funConcave             :: Double → Double
> funConcave d
>   | d >= 1                               = 1
>   | otherwise                            = quarterCircleTable ! truncate (d * fromIntegral qTableSize)
>
> funConvex              :: Double → Double
> funConvex d
>   | (1 - d) >= 1                        = 1
>   | otherwise                           = 1 - (quarterCircleTable ! truncate ((1 - d) * fromIntegral qTableSize))
>
> funSwitch              :: Double → Double
> funSwitch d                             = if d < 0.5
>                                             then 0
>                                             else 1
>
> controlDenormal        :: Mapping → Double → (Double, Double) → Double
> controlDenormal ping@Mapping{msBiPolar} dIn (lo, hi)
>                                          = vOut
>   where
>     scale                                = profess (lo < hi)
>                                            "inverted range in controlDenormal"
>                                            (hi - lo)
>     vOut                                 = if msBiPolar
>                                              then controlBiPolar ping dNorm
>                                              else controlUniPolar ping dNorm
>     dNorm =   (dIn - lo) / scale
>
> controlUniPolar        :: Mapping → Double → Double
> controlUniPolar ping@Mapping{msContinuity, msMax2Min} d
>                                          = fun xStart
>   where
>     fun                                  = case msContinuity of
>                                              Linear     → funLinear
>                                              Concave    → funConcave
>                                              Convex     → funConvex
>                                              Switch     → funSwitch
>
>     xStart             :: Double         = if msMax2Min
>                                              then 1 - d
>                                              else d
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
>
>     pingL                                = ping{msBiPolar = False, msContinuity = left}
>     pingR                                = ping{msBiPolar = False, msContinuity = right}
>
>     (addL, addR)                         = if msMax2Min
>                                              then (0, -1)
>                                              else (-1, 0)
>
>     dOut
>       | msContinuity == Switch           = (controlUniPolar ping dIn * 2) - 1
>       | dIn < 0.5                        = controlDenormal pingL dIn (0.0, 0.5) + addL
>       | otherwise                        = controlDenormal pingR dIn (0.5, 1.0) + addR
>
> table2vals             :: [Double] → [(Double, Double, Double, Double)]
> table2vals                               = zipWith (curry convFun) [0 .. ]
>   where
>     convFun            :: (Int, Double) → (Double, Double, Double, Double)
>     convFun (i, y)                       = (fromIntegral i / 128, y, 0, 0)
>
> vals' = table2vals (modulationTest004 (allMappings !! 15))

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
> data ModSrcSource =
>     FromNoController
>   | FromNoteOnVel
>   | FromNoteOnKey
>   | FromLinked deriving (Eq, Show)
>
> data ModSrc =
>   ModSrc {
>     msMapping          :: Mapping
>     , msSource         :: ModSrcSource} deriving (Eq, Show)
>
> defModSrc              :: ModSrc
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
>     qqUseModulators        :: Bool
>   , qqUseDefaultMods       :: Bool
>   , qqUseLowPass           :: Bool
>   , qqUseLFO               :: Bool} deriving Show
>
> useModulators                            = qqUseModulators              defM
> useDefaultMods                           = qqUseDefaultMods             defM
> useLowPass                               = qqUseLowPass                 defM
> useLFO                                   = qqUseLFO                     defM
>
> defM                   :: ModulationSettings
> defM =
>   ModulationSettings {
>     qqUseModulators                      = False
>   , qqUseDefaultMods                     = False
>   , qqUseLowPass                         = True
>   , qqUseLFO                             = True}
>
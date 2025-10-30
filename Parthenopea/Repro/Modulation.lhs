> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeFamilies #-} 
> {-# LANGUAGE UnicodeSyntax #-}

Modulation
William Clements
November 6, 2023

> module Parthenopea.Repro.Modulation where
>
> import Control.Arrow ( Arrow(arr), (>>>) )
> import Control.Arrow.Operations
> import Data.Array.Unboxed
> import qualified Data.Bifunctor          as BF
> import Data.Bits
> import Data.Complex
> import Data.Graph (Graph)
> import qualified Data.Graph              as Graph
> import Data.List ( foldl', iterate', find )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.MemoTrie
> import Data.Word ( Word64 )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.Types ( AudRate, Clock(..), CtrRate, Signal )
> import Euterpea.Music ( AbsPitch )
> import GHC.Generics ( Generic ) 
> import Parthenopea.Debug
> import Parthenopea.SoundFont.Utility
>
> constA                 :: Arrow a ⇒ c → a b c
> constA                                   = arr . const

"A modulator is defined by its sfModSrcOper, its sfModDestOper, and its sfModSrcAmtOper"
--SoundFont spec

struct sfInstModList
{
  SFModulator sfModSrcOper;
  SFGenerator sfModDestOper;
  SHORT modAmount;
  SFModulator sfModAmtSrcOper;
  SFTransform sfModTransOper;
};

> data Modulator                           =
>   Modulator {
>     mrModId            :: Word
>   , mrModSrc           :: ModSrc
>   , mrModDest          :: ModDestType
>   , mrModAmount        :: Double
>   , mrAmountSrc        :: ModSrc} deriving (Eq, Show)
>    
> defModulator           :: Modulator
> defModulator                             =
>   Modulator
>    0 
>    defModSrc 
>    NoDestination 
>    0
>    defModSrc
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
> defModSrc              :: ModSrc
> defModSrc                                = ModSrc defMapping FromNoController
>
> data Mapping =
>   Mapping {
>     msContinuity     :: Continuity
>   , msBiPolar        :: Bool  
>   , msMax2Min        :: Bool
>   , msCCBit          :: Bool} deriving (Eq, Ord, Show)
>
> data Continuity =
>     Linear
>   | Concave
>   | Convex
>   | Switch deriving (Eq, Ord, Show, Enum)
>
> defMapping             :: Mapping
> defMapping                               = Mapping Linear False False False
> allMappings            :: [Mapping]
> allMappings                              = [Mapping cont bipolar max2min False
>                                                   | cont                  ← [Linear, Concave, Convex, Switch]
>                                                        , bipolar          ← [False, True]
>                                                              , max2min    ← [False, True]]                                          

Modulator management is complex considering small impact ==============================================================

Nonetheless, trying hard here for 100 percent correctness and support, even with high numbers of mods.

> resolveMods            :: Modulation → [Modulator] → [Modulator] → Modulation
> resolveMods m8n m8rs dm8rs               = m8n{mModsMap = compileMods checked}
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
>     ufolder uniquer m8r                  =
>       Map.insert (ModKey m8r.mrModSrc m8r.mrModDest m8r.mrAmountSrc) m8r uniquer
>
> freeOfCycles           :: [Modulator] → Bool
> freeOfCycles m8rs                        = null $ cyclicNodes $ makeGraph edgeList
>   where
>     nodeList           :: Map ModDestType [Modulator]
>     nodeList                             = Map.filterWithKey (\k _ → (isJust . outGoing) k) (compileMods m8rs)
>
>     edgeList           :: [(Node, [Node])]
>                                          =
>       map (BF.bimap nodeFrom (map (fromIntegral . mrModId))) (Map.toList nodeList)
>
>     nodeFrom           :: ModDestType → Node
>     nodeFrom mdt                         =
>       maybe
>         (error $ unwords ["only ToLink forms a ModDestType to be turned into a Node"])
>         fromIntegral
>         (outGoing mdt)
>
> -- | Calculates all the nodes that are part of cycles in a graph.
> cyclicNodes :: Graph → [Node]
> cyclicNodes graph                        = (map fst . filter isCyclicAssoc . assocs) graph
>   where
>     isCyclicAssoc                        = uncurry (reachableFromAny graph)
>
> -- | In the specified graph, can the specified node be reached, starting out
> -- from any of the specified vertices?
> reachableFromAny :: Graph → Node → [Node] → Bool
> reachableFromAny graph node              = elem node . concatMap (Graph.reachable graph)
>
> outGoing               :: ModDestType → Maybe Word
> outGoing                                 =
>   \case
>     ToLink mId                           → Just mId
>     _                                    → Nothing
>
> data Sifting                             =
>   Sifting {
>       ssCounter        :: Int
>     , ssCurrent        :: [Modulator]
>     , ssPrevious       :: [Modulator]} deriving Show 
>
> eliminateDanglingMods  :: Sifting → Sifting
> eliminateDanglingMods ting               =
>   Sifting (ting.ssCounter + 1) newTry ting.ssCurrent
>   where
>     -- examine result of the previous generation
>     -- use it to produce next generation, dropping nodes that:
>     -- 1. expect linked sources but have none
>     -- 2. consist of an outbound link (by Id) to non-existent mod
>     --     or
>     -- 3. are superseded 
>     newTry                               = profess
>                                              (ting.ssCounter <= 10)
>                                              "maximum of 10 tries exceeded..."
>                                              filter shouldStay ting.ssCurrent
>     shouldStay m8r                       = linkageInOk && linkageOutOk
>       where
>         linkageInOk                      =
>           FromLinked /= m8rSource || maybe False (not . null) (Map.lookup (ToLink m8r.mrModId) byModDestType)
>         linkageOutOk                     =
>           maybe True (\w → (isJust . find (\m → m.mrModId == w)) ting.ssCurrent) (outGoing m8r.mrModDest)
>         
>         m8rSource                        = msSource m8r.mrModSrc
>         byModDestType                    = compileMods ting.ssCurrent
>
> siftMods               :: [Modulator] → [Modulator]
> siftMods m8rs                            = final.ssCurrent
>   where
>     generations                          =
>       iterate' eliminateDanglingMods (Sifting 0 m8rs [])
>     final                                = head $ dropWhile unfinished generations
>     unfinished Sifting{ssCurrent, ssPrevious}
>                                          = ssCurrent /= ssPrevious
>
> compileMods            :: [Modulator] → Map ModDestType [Modulator]
> compileMods                              = foldl' mfolder Map.empty
>   where
>     mfolder m m8r                        = Map.insertWith (++) m8r.mrModDest [m8r] m
>
> renumberMods           :: [Modulator] → [Modulator]
> renumberMods m8rs                        = map renumber m8rs
>   where
>     subs               :: [(Word, Word)]
>     subs                                 = zipWith (\i m → (mrModId m, i)) [0..] m8rs
>     renumber           :: Modulator → Modulator
>     renumber m8r@Modulator{mrModId, mrModDest}
>                                          =
>       let
>         upd mid                          = deJust "upd mid" $ lookup mid subs
>       in
>         m8r{  mrModId                    = upd mrModId
>             , mrModDest                  = (\case (ToLink m)         → (ToLink $ upd m);
>                                                   o                  → o) mrModDest}
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
>
>     cont, bipolar, max2Min, ccBit, src
>                        :: Int
>     cont                                 = fromIntegral   (wIn `shift` (-10))    `mod` 64
>     bipolar                              = fromIntegral   (wIn `shift` (-9))     `mod` 2
>     max2Min                              = fromIntegral   (wIn `shift` (-8))     `mod` 2
>     ccBit                                = fromIntegral   (wIn `shift` (-7))     `mod` 2
>     src                                  = fromIntegral   wIn                    `mod` 128
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
> addAmtSrc'             :: ModSrc → Modulator → Maybe Modulator
> addAmtSrc' modSrc@ModSrc{msSource} m8r   = Just m8r >>= (\x → case msSource of
>                                                               FromLinked       → Nothing
>                                                               _                → Just x{mrAmountSrc = modSrc})
>
> defaultMods            :: [Modulator]
> defaultMods                              = if useDefaultMods
>                                              then [ makeDefaultMod 0 ms0 48 960     defModSrc
>                                                   , makeDefaultMod 1 ms1  8 (-2400) ms2 ] ++ specialDefaultMods
>                                              else []
>  -- all three of these are negative unipolar, continuity varying                                                            
>   where
>     ms0                                  = ModSrc   (Mapping Concave   False  True False) FromNoteOnVel
>     ms1                                  = ModSrc   (Mapping Linear    False  True False) FromNoteOnVel
>  -- some implementations do not include the amount source below in the second default modulator
>     ms2                                  = ModSrc   (Mapping Switch    False  True False) FromNoteOnVel
>
>     makeDefaultMod     :: Word → ModSrc → Word → Double → ModSrc → Modulator
>     makeDefaultMod mId ms igen amt aSrc  = deJust
>                                              "makeDefaultMod"
>                                              (Just defModulator{mrModId = mId}
>                                               >>= addSrc ms
>                                               >>= addDest igen
>                                               >>= addAmount amt
>                                               >>= addAmtSrc' aSrc)
>
>     ms3                                  = ModSrc   (Mapping Linear False False False) FromNoController
>
>     specialDefaultMods :: [Modulator]
>     specialDefaultMods                   = modChorus ++ modReverb
>       where
>         modChorus                        =
>           [makeDefaultMod 10 ms3 15 (fromRational chorusAllPercent * 10) defModSrc | chorusAllPercent > 0]
>         modReverb                        =
>           [makeDefaultMod 11 ms3 16 (fromRational reverbAllPercent * 10) defModSrc | reverbAllPercent > 0]
>
> evaluateMods           :: ModDestType → Map ModDestType [Modulator] → Double
> evaluateMods md graph                    = sum $ maybe [] (map evaluateMod) (Map.lookup md graph)
>   where
>     evaluateMod m8r                      =
>       let
>         getValue modSrc
>           | useModulators                =
>               case modSrc.msSource of
>                 FromLinked               → evaluateMods (ToLink m8r.mrModId) graph
>                 _                        → 1
>           | otherwise                    = 1
>       in
>         getValue m8r.mrModSrc * m8r.mrModAmount * getValue m8r.mrAmountSrc
>
> evaluateNoteOn         :: Int → Mapping → Double
> evaluateNoteOn n ping                    = controlDenormal ping (fromIntegral n / fromIntegral qMidiSize128) (0, 1)
>
> evaluateModSignals     :: String → Modulation → ModDestType → ModSignals → Double
> evaluateModSignals tag m8n md (ModSignals xenv xlfo xvib)
>   | traceNot trace_EMS False             = undefined
>   | otherwise                            = converter md (xmodEnv + xmodLfo + xvibLfo + xmods)
>  where
>    fName                                 = "evaluateModSignals"
>    trace_EMS                             = unwords [fName, show (md, mco)]
>
>    converter           :: ModDestType → (Double → Double)
>    converter                             =
>      \case
>         ToVolume                         → fromCentibels
>         _                                → fromCents
>
>    mco                 :: ModCoefficients
>                                          =
>      case md of
>        ToPitch                           → m8n.toPitchCo
>        ToFilterFc                        → m8n.toFilterFcCo
>        ToVolume                          → m8n.toVolumeCo
>        _                                 → error $
>          unwords [fName, "only ToPitch, ToFilterFc, and ToVolume supported", tag, show md]
>
>    xmodEnv                               = xenv * mco.xModEnvCo
>    xmodLfo                               = xlfo * mco.xModLfoCo
>    xvibLfo                               = xvib * mco.xVibLfoCo
>    xmods                                 = evaluateMods md m8n.mModsMap

Filters are complex AND have a large impact ===========================================================================

> cascadeCount           :: ResonanceType → Maybe Int
> cascadeCount resonType                   =
>   case resonType of
>     ResonanceNone                        → Nothing
>     _                                    → Just cascadeConfig
>
> addResonance           :: ∀ p . Clock p ⇒ Modulation → Signal p (Double, ModSignals) Double
> addResonance m8n@Modulation{mLowpass}
>                                          =
>   case cascadeCount lowpassType of
>     Nothing            →
>       proc (x, _)                        → do
>         y ← delay 0                      ⤙ x  
>         outA                             ⤙ y
>     Just count         →
>       case count of
>         0              → final
>         1              → stage >>> final
>         2              → stage >>> stage >>> final
>         3              → stage >>> stage >>> stage >>> final
>         4              → stage >>> stage >>> stage >>> stage >>> final
>         _              → error $ unwords [show count, "cascades are too many, not supported"]
>   where
>     Lowpass{lowpassType}                 = mLowpass
>
>     stage                                =
>         proc (sIn, msig)                 → do
>           let fc                         = modulateFc msig
>           pickled ← procFilter mLowpass  ⤙ (sIn, fc)
>           let sOut                       = pickled
>           outA                           ⤙ (sOut, msig)
>
>     final                                =
>         proc (sIn, msig)                 → do
>           let fc                         = modulateFc msig
>           pickled ← procFilter mLowpass  ⤙ (sIn, fc)
>           outA                           ⤙ pickled
>
>     modulateFc         :: ModSignals → Double
>     modulateFc msig                      =
>       clip freakRange (lowpassFc mLowpass * evaluateModSignals "modulateFc" m8n ToFilterFc msig)
>
> procFilter             :: ∀ p . Clock p ⇒ Lowpass → Signal p (Double, Double) Double
> procFilter lp@Lowpass{lowpassType}       =
>   case lowpassType of
>     ResonanceNone                        → error $ unwords ["should not reach procFilter if ResonanceNone"]
>     ResonanceSVF                         → procSVF lp
>     ResonanceChamberlin                  → procChamberlin lp

State Variable Filter =================================================================================================

see source https://karmafx.net/docs/karmafx_digitalfilters.pdf

> procSVF                :: ∀ p . Clock p ⇒ Lowpass → Signal p (Double,Double) Double
> procSVF lp                               =
>   proc (x, fc) → do
>     let f1                               = fudge * sin (theta fc)
>     rec
>       let yL                             = f1 * yB + yL'
>       let yH                             = (x + x') / 2 - yL' - damp * yB'
>       let yB                             = f1 * yH + yB'
>
>       x' ← delay 0                       ⤙ x
>       yL' ← delay 0                      ⤙ yL
>       yB' ← delay 0                      ⤙ yB
>     outA                                 ⤙ yL
>   where
>     damp                                 = 1 / fromCentibels (lowpassQ lp)
>     theta c                              = pi * c / rate (undefined :: p)
>     fudge                                = 1.0

see source https://ccrma.stanford.edu/~jos/svf/svf.pdf

> procChamberlin         :: ∀ p . Clock p ⇒ Lowpass → Signal p (Double,Double) Double
> procChamberlin _                         =
>   proc (x, fc) → do
>     let wcT                              = fudge * sin (theta fc)
>     rec
>       let yL                             = tracer "yL" $ wcT * p2 + yL'
>
>       let p0                             = (x - yL') - sqrt 2
>       let p1                             = p0 * wcT + p2'
>       let p2                             = p1 * wcT + yL'
>
>       yL' ← delay 0                      ⤙ yL
>       p2' ← delay 0                      ⤙ p2
>     outA                                 ⤙ yL
>   where
>     theta c                              = pi * c / rate (undefined :: p)
>     fudge                                = 1 -- WOX 1 / fromCentibels (lowpassQ lp)

Miscellaneous =========================================================================================================

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
>                                          =
>       if useLFO && anyJust
>         then Just $ LFO (fromTimecents del)
>                         (fromAbsoluteCents $ fromMaybe 0 mfreq)
>                         (deriveModTriple toPitch toFilterFc toVolume)
>         else Nothing
>   where
>     anyJust            :: Bool           = isJust toPitch || isJust toFilterFc || isJust toVolume
>
> doLFO                  :: ∀ p . Clock p ⇒ Maybe LFO → Signal p () Double
> doLFO                                    = maybe (constA 0) makeSF
>   where
>     makeSF             :: LFO → Signal p () Double
>     makeSF o                             = 
>       proc _ → do
>         y ← triangleWave o.lfoFrequency  ⤙ ()
>         z ← delayLine    o.lfoDelay      ⤙ y
>         outA                             ⤙ z
>
> triangleWave           :: ∀ p . Clock p ⇒ Double → Signal p () Double
> triangleWave freq                        = 
>   proc _ → do
>     osc triangleWaveTable 0              ⤙ freq
>
> modVib                 :: ∀ p . Clock p ⇒ Double → Double → Signal p Double Double
> modVib rateV depth                        =
>   proc sIn → do
>     vib    ← osc sineTable 0             ⤙ rateV
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
> controlUniPolar Mapping{msContinuity, msMax2Min} dIn
>                                          = control xStart
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
> controlBiPolar ping dIn                  = dOut
>   where
>     -- bipolar concave/convex:
>     --   if positive, swap the left half to the opposite
>     --   if negative, swap the right one
>
>     swapCont           :: Continuity → Continuity
>     swapCont cIn                         = case cIn of
>                                              Concave → Convex
>                                              Convex  → Concave
>                                              _ → error $ unwords ["swapCont"]
>     (leftC, rightC)    :: (Continuity, Continuity)
>                                          = if ping.msContinuity == Concave || ping.msContinuity == Convex
>                                              then if ping.msMax2Min
>                                                     then (ping.msContinuity, swapCont ping.msContinuity)
>                                                     else (swapCont ping.msContinuity, ping.msContinuity)
>                                              else (ping.msContinuity, ping.msContinuity)
>     pingL                                = ping{msBiPolar = False, msContinuity = leftC}
>     pingR                                = ping{msBiPolar = False, msContinuity = rightC}
>     (addL, addR)                         = if ping.msMax2Min
>                                              then (0, -1)
>                                              else (-1, 0)
>     dOut
>       | ping.msContinuity == Switch      = (controlUniPolar ping dIn * 2) - 1
>       | dIn < 0.5                        = controlDenormal pingL dIn (0.0, 0.5) + addL
>       | otherwise                        = controlDenormal pingR dIn (0.5, 1.0) + addR
>

Type declarations =====================================================================================================

> data ResonanceType                       =
>   ResonanceNone
>   | ResonanceSVF
>   | ResonanceChamberlin deriving (Bounded, Enum, Eq, Show)
>
> data KernelSpec                          =
>   KernelSpec {
>     ksFc               :: Int
>   , ksQ                :: Int
>   , ksSr               :: Int
>   , ksFast             :: Bool
>   , ksLen              :: Int} deriving (Eq, Generic, Ord, Show)
>
> defKernelSpec          :: Bool → KernelSpec
> defKernelSpec bFast                      = KernelSpec 13_500 0 44_100 bFast 300
>
> instance HasTrie KernelSpec where
>   newtype (KernelSpec :->: b)            = KernelSpecTrie { unKernelSpecTrie :: Reg KernelSpec :->: b } 
>   trie                                   = trieGeneric KernelSpecTrie 
>   untrie                                 = untrieGeneric unKernelSpecTrie
>   enumerate                              = enumerateGeneric unKernelSpecTrie
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
>     k1                                   = - (2 * mag * cos ph)
>     k2                                   = mag * mag
>
> buildSystemM2N2        :: ([Complex Double], [Complex Double]) → CoeffsM2N2
> buildSystemM2N2 (zeros, poles)           =
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

r is the resonance radius, w0 is the angle of the poles and b0 is the gain factor

> indeedReplaceRadius    :: Bool
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
> data ModCoefficients                     =
>   ModCoefficients {
>     xModEnvCo          :: Double
>   , xModLfoCo          :: Double
>   , xVibLfoCo          :: Double} deriving (Eq, Show)
> defModCoefficients     :: ModCoefficients
> defModCoefficients                       = ModCoefficients 0 0 0
>
> data ModTriple                           =
>   ModTriple {
>     coPitch            :: Double
>   , coFilterFc         :: Double
>   , coVolume           :: Double} deriving (Eq, Show)
> coAccess               :: ModDestType → ModTriple → Double
> coAccess md mTriple                      =
>   case md of
>     ToPitch            → mTriple.coPitch
>     ToFilterFc         → mTriple.coFilterFc
>     ToVolume           → mTriple.coVolume
>     _                  → error $ unwords["coAccess: ModTriple only deals with ToPitch, ToFilterFc, and ToVolume"]                         
> defModTriple           :: ModTriple
> defModTriple                             = ModTriple 0 0 0
>
> data ModSignals                          = ModSignals !Double !Double !Double
> defModSignals          :: ModSignals
> defModSignals                            = ModSignals 0 0 0
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
>   , mModEnv            :: Maybe FEnvelope
>   , mModLfo            :: Maybe LFO
>   , mVibLfo            :: Maybe LFO
>   , toPitchCo          :: ModCoefficients
>   , toFilterFcCo       :: ModCoefficients
>   , toVolumeCo         :: ModCoefficients
>   , mModsMap           :: Map ModDestType [Modulator]} deriving (Eq)
> instance Show Modulation where
>   show m                                 = unwords ["Modulation", show m.mLowpass, show m.mModEnv]
>
> defModulation          :: Modulation
> defModulation                            =
>   Modulation
>     (Lowpass ResonanceNone (defKernelSpec useFastFourier)) Nothing Nothing Nothing
>     defModCoefficients defModCoefficients defModCoefficients Map.empty
>
> data TimeFrame                           =
>   TimeFrame {
>     tfSecsSampled      :: Double
>   , tfSecsScored       :: Double
>   , tfSecsToPlay       :: Double
>   , tfLooping          :: Bool} deriving (Eq, Show)
> data EnvelopeExtras                      =
>   EnvelopeExtras {
>     eeTargetT          :: Double
>   , eeReleaseT         :: Double
>   , eePostT            :: Double} deriving (Eq, Show)
> data FEnvelope                           =
>   FEnvelope {
>     fExtras            :: Maybe EnvelopeExtras
>   , fSustainLevel      :: Double
>   , fModTriple         :: Maybe ModTriple
>
>   , fDelayT            :: Double
>   , fAttackT           :: Double
>   , fHoldT             :: Double
>   , fDecayT            :: Double
>   , fSustainT          :: Double} deriving (Eq, Show)
> data Segments                            =
>   Segments {
>     sAmps              :: [Double]
>   , sDeltaTs           :: [Double]} deriving Show
>
> minDeltaT, minUseful   :: Double
> minDeltaT                                = fromTimecents Nothing
> minUseful                                = 1/82
>
> ctrRate, audRate       :: Double
> ctrRate                                  = rate (undefined :: CtrRate)
> audRate                                  = rate (undefined :: AudRate)
>
> data FreeVerb =
>   FreeVerb
>   {
>       iiWetDryMix      :: Double
>     , iiG              :: Double
>     , iiGain           :: Double
>     , iiRoomSize       :: Double
>     , iiDamp           :: Double
>     , iiWet1           :: Double
>     , iiWet2           :: Double
>     , iiDry            :: Double
>     , iiWidth          :: Double
>     , iiCombDelayL     :: Array Word Word64
>     , iiCombDelayR     :: Array Word Word64
>     , iiCombLPL        :: Array Word FilterData
>     , iiCombLPR        :: Array Word FilterData
>     , iiAllPassDelayL  :: Array Word Word64
>     , iiAllPassDelayR  :: Array Word Word64
>   } deriving (Show, Eq, Ord)
>
> data FilterData =
>   FilterData
>   {
>       jGain            :: Double
>     , jChannelsIn      :: Word64
>     , jB               :: [Double]
>     , jA               :: [Double]
>   } deriving (Show, Eq, Ord)
>
> newOnePole             :: Double → FilterData
> newOnePole pole =
>   let
>     b0 = if pole > 0
>          then 1 - pole
>          else 1 + pole
>     a0 = 1
>     a1 = (-pole)
>   in
>     FilterData 1
>                2
>                [b0]
>                [a0, a1]
>
> windices               :: [Word]
> windices                                 = [0..7]
>
> makeFreeVerb           :: Double → Double → Double → FreeVerb
> makeFreeVerb roomSize damp width
>   | traceIf trace_MFV False = undefined
>   | otherwise =
>   let
>     wet = fvScaleWet * fvWetDryMix
>     dry = fvScaleDry * (1 - fvWetDryMix)
>     wet' = wet / (wet + dry)
>     dry' = dry / (wet + dry)
>     wet1 = wet' * (width/2 + 0.5)
>     wet2 = wet' * (1 - width) / 2
>     initCombDelay, initAllpassDelay
>                        :: Array Word Word64
>     initCombFilter     :: Array Word FilterData
>     initCombDelay    = array (0,7) $ zip windices fvCombDelays
>     initAllpassDelay = array (0,3) $ zip windices fvAllpassDelays
>     initCombFilter =   array (0,7) $ zip windices (replicate 8 $ newOnePole 0.9)
>
>     fvWetDryMix     = 0.2
>     fvCoefficient   = 0.5
>     fvFixedGain     = 0.015;
>     fvScaleWet      = 3;
>     fvScaleDry      = 2;
>     fvScaleDamp     = 0.4;
>     fvScaleRoom     = 0.28
>     fvOffsetRoom    = 0.7
>     fvCombDelays           :: [Word64] = [1617, 1557, 1491, 1422, 1356, 1277, 1188, 1116]
>     fvAllpassDelays        :: [Word64] = [225, 556, 441, 341]
>
>   in
>     FreeVerb fvWetDryMix
>                  fvCoefficient
>                  fvFixedGain
>                  (roomSize * fvScaleRoom + fvOffsetRoom)
>                  (damp * fvScaleDamp)
>                  wet1
>                  wet2
>                  dry'
>                  width
>                  initCombDelay
>                  initCombDelay
>                  initCombFilter
>                  initCombFilter
>                  initAllpassDelay
>                  initAllpassDelay
>   where
>     trace_MFV =
>       unwords [
>           "makeFreeVerb roomSize",       show roomSize
>         , "damp",                        show damp
>         , "width",                       show width]
>   
> eatFreeVerb            :: ∀ p . Clock p ⇒ FreeVerb → Signal p Double Double
> eatFreeVerb fv                           =
>     proc sinL → do
>       cdL0 ← comb (fv.iiCombDelayL ! 0) (fv.iiCombLPL ! 0) ⤙ sinL
>       cdL1 ← comb (fv.iiCombDelayL ! 1) (fv.iiCombLPL ! 1) ⤙ sinL
>       cdL2 ← comb (fv.iiCombDelayL ! 2) (fv.iiCombLPL ! 2) ⤙ sinL
>       cdL3 ← comb (fv.iiCombDelayL ! 3) (fv.iiCombLPL ! 3) ⤙ sinL
>       cdL4 ← comb (fv.iiCombDelayL ! 4) (fv.iiCombLPL ! 4) ⤙ sinL
>       cdL5 ← comb (fv.iiCombDelayL ! 5) (fv.iiCombLPL ! 5) ⤙ sinL
>       cdL6 ← comb (fv.iiCombDelayL ! 6) (fv.iiCombLPL ! 6) ⤙ sinL
>       cdL7 ← comb (fv.iiCombDelayL ! 7) (fv.iiCombLPL ! 7) ⤙ sinL
>
>       let sumL = cdL0+cdL1+cdL2+cdL3+cdL4+cdL5+cdL6+cdL7
>
>       let fp0L = sumL/8
>
>       fp1L ← allpass (fv.iiAllPassDelayL ! 0) ⤙ fp0L
>       fp2L ← allpass (fv.iiAllPassDelayL ! 1) ⤙ fp1L
>       fp3L ← allpass (fv.iiAllPassDelayL ! 2) ⤙ fp2L
>       fp4L ← allpass (fv.iiAllPassDelayL ! 3) ⤙ fp3L
>             
>       outA ⤙ fp4L
>
> comb                   :: ∀ p . Clock p ⇒ Word64 → FilterData → Signal p Double Double
> comb maxDel stkFilter                    =
>   proc sIn → do
>     rec
>       sOut ← delayLine secs ⤙ sIn + sOut * jGain stkFilter
>     outA ⤙ sOut
>   where
>     sr                                   = rate (undefined :: p)
>     secs               :: Double         = fromIntegral maxDel/sr
> 
> allpass                :: ∀ p . Clock p ⇒ Word64 → Signal p Double Double
> allpass maxDel                           =
>   proc sIn → do
>     sOut ← delayLine secs ⤙ sIn
>     outA ⤙ sOut
>   where
>     sr                                   = rate (undefined :: p)
>     secs               :: Double         = fromIntegral maxDel/sr

Returns the frequency

> fromAbsoluteCents      :: Int → Double
> fromAbsoluteCents acents                 = 8.176 * fromCents (fromIntegral acents)
>
> toAbsoluteCents        :: Double → Int
> toAbsoluteCents freq                     = round $ 100 * 12 * logBase 2 (freq / 8.176)

Returns the elapsed time in seconds

> fromTimecents          :: Maybe Int → Double
> fromTimecents mtimecents                 = pow 2 (maybe (- 12_000) fromIntegral mtimecents / 1_200)
>
> fromTimecents'         :: Maybe Int → Maybe Int → KeyNumber → Double
> fromTimecents' mtimecents mfact key      = pow 2 (base + inc)
>   where
>     base               :: Double         =
>       maybe (-12_000) fromIntegral mtimecents / 1_200
>     inc                :: Double         =
>       maybe 0 fromIntegral mfact * fromIntegral (60 - key) / fromIntegral qMidiSize128 / 1_200
>
> toTimecents            :: Double → Int
> toTimecents secs                         = round $ logBase 2 secs * 1_200
>
> teclip, tfclip, tqclip, tvclip, ticlip, tpclip, tcclip, tbclip, taclip, tkclip, tdclip,
>   t1clip, t2clip, t3clip
>                        :: Int → Maybe Int
> tmclip, tnclip         :: Word → Maybe Word
>
> teclip i                                 = Just $ clip (-12_000, 12_000) i
> tfclip i                                 = Just $ clip (1_500, 13_500) i
> tqclip i                                 = Just $ clip (0, 960) i
> tvclip i                                 = Just $ clip (-960, 960) i
> ticlip i                                 = Just $ clip (0, 1_000) i
> tpclip i                                 = Just $ clip (-500, 500) i
> tcclip i                                 = Just $ clip (-12_000, 5_000) i
> tbclip i                                 = Just $ clip (-12_000, 8_000) i
> taclip i                                 = Just $ clip (-16_000, 4_500) i
> tkclip i                                 = Just $ clip (-1_200, 1_200) i
> tdclip i                                 = Just $ clip (0, 1_440) i
> tmclip w                                 = Just $ clip (0, 127) w
> tnclip w                                 = Just $ clip (1, 127) w
> t1clip i                                 = Just $ clip (-120, 120) i
> t2clip i                                 = Just $ clip (-99, 99) i
> t3clip i                                 = Just $ clip (0, 1_200) i

Returns the frequency ratio

> fromCents              :: Double → Double
> fromCents cents                          = pow 2 (cents/12/100)
>
> fromCents'             :: Maybe Int → Maybe Int → Maybe Double
> fromCents' mcoarse mfine
>   | isNothing mcoarse && isNothing mfine = Nothing
>   | otherwise                            = Just $ fromCents $ coarse * 100 + fine
>   where
>     coarse = maybe 0 fromIntegral mcoarse
>     fine   = maybe 0 fromIntegral mfine
>
> freakRange             :: (Double, Double)
> freakRange                               = (20, 20_000)
>
> checkForNan            :: Double → String → Double
> checkForNan y msg                        =
>   profess
>     (not $ isNaN y || isInfinite y || isDenormalized y || abs y > 200_000)
>     (msg ++ " bad Double = " ++ show y)
>     y

Returns the amplitude ratio

> fromCentibels          :: Double → Double
> fromCentibels centibels                  = pow 10 (centibels/1000)
>
> toCentibels            :: Double → Double
> toCentibels ratio                        = logBase 10 (ratio * 1000)
>

Returns the amplitude ratio (based on input 10ths of a percent) 

> fromTithe              :: Maybe Int → Bool → Double
> fromTithe iS isVol                       =
>   if isVol
>     then 1 / fromCentibels jS
>     else (1000 - jS) / 1000
>   where
>     jS                 :: Double         = maybe 0 fromIntegral iS
>
> theE, epsilon, upsilon :: Double
> theE                                     = 2.718_281_828_459_045_235_360_287_471_352_7
> epsilon                                  = 1e-8               -- a generous little epsilon
> upsilon                                  = 1e10               -- a scrawny  big    upsilon
>    
> theE' :: Complex Double
> theE' = theE :+ 0
>
> theJ :: Complex Double
> theJ = 0 :+ 1

sampleUp returns power of 2 greater than OR EQUAL TO the input value (result at least 2**14)
sampleDown returns power of 2 less than OR EQUAL TO the input value (input enforced <= 2**31)
breakUp returns a list of integers approximating divisions of a floating point range

> sampleUp               :: Int → Int
> sampleUp i                               =
>   if i <= 0
>     then error "out of range for sampleUp"
>     else max 16_384 (head $ dropWhile (< i) (iterate' (* 2) 1))
>
> sampleDown             :: Int → Int
> sampleDown i                             =
>   if i <= 0 || i > 2_147_483_648
>     then error "out of range for sampleDown"
>     else head $ dropWhile (> i) (iterate' (`div` 2) 2_147_483_648)
>
> breakUp :: (Double, Double) → Double → Int → [Int]
> breakUp (xmin, xmax) base nDivs =
>   let
>     (ymin, ymax) =
>       if base == 0
>         then (xmin, xmax)
>         else (logBase base xmin, logBase base xmax)
>     delta = (ymax - ymin) / fromIntegral nDivs
>     oper =
>       if base == 0
>         then id
>         else pow base
>   in
>     map (round . oper . (+ ymin) . (* delta) . fromIntegral) ([0..nDivs] :: [Int])

Control Functions

The use of following functions requires that their input is normalized between 0 and 1
(And you can count on the output being likewise normalized!)

> controlLinear          :: Double → Double
> controlLinear                            = id
>
> quarterCircleTable     :: Array Int Double
>                                            -- TODO: use Table
> quarterCircleTable                       = array (0, qTableSize - 1) [(x, calc x) | x ← [0..(qTableSize - 1)]]
>   where
>     calc               :: Int → Double
>     calc i                               =
>       let
>         cD             :: Double         = fromIntegral i / tableSize
>       in
>         1 - sqrt (1 - cD*cD)
>
> qTableSize             :: Int
> qTableSize                               = 1024
> tableSize              :: Double
> tableSize                                = fromIntegral qTableSize
>
> controlConcave         :: Double → Double
> controlConcave doub
>   | doub >= 1                            = 1
>   | otherwise                            = quarterCircleTable ! truncate (doub * tableSize)
>
> controlConvex          :: Double → Double
> controlConvex doub
>   | (1 - doub) >= 1                      = 1
>   | otherwise                            = 1 - (quarterCircleTable ! truncate ((1 - doub) * tableSize))
>
> controlSwitch          :: (Ord a1, Fractional a1, Num a2) ⇒ a1 → a2
> controlSwitch doub                       = if doub < 0.5
>                                              then 0
>                                              else 1

Account for microtones specified by SoundFont scale tuning : 0 < x < 100 < 1200
Note result is incorrect overall when involves multiple root pitches

> calcMicrotoneRatio     :: AbsPitch → AbsPitch → Double → Double
> calcMicrotoneRatio rootp p x             = step ** fromIntegral (rootp - p)
>   where
>     step               :: Double         = 2 ** (x / 1_200)
          
Raises 'a' to the power 'b' using logarithms.

> pow                    :: Floating a ⇒ a → a → a
> pow x y                                  = exp (log x * y)

Returns the fractional part of 'x'.

> frac                   :: RealFrac r ⇒ r → r
> frac                                     = snd . properFraction
>
> class Coeff a where
>   azero                :: a
>   acomplex             :: a → Complex Double
>   aamp                 :: a → Double
>   ascale               :: Double → a → a
>   aadd                 :: a → a → a
>   amul                 :: a → a → a
>   asqrt                :: a → a
>
> instance Coeff Double where
>   azero                :: Double
>   azero                                  = 0
>   acomplex             :: Double → Complex Double
>   acomplex                               = (:+ 0)
>   aamp                 :: Double → Double
>   aamp                                   = abs
>   ascale               :: Double → Double → Double
>   ascale                                 = amul
>   aadd                 :: Double → Double → Double
>   aadd x y                               = x + y
>   amul                 :: Double → Double → Double
>   amul x y                               = x * y
>   asqrt                :: Double → Double
>   asqrt                                  = sqrt
>
> instance Coeff (Complex Double) where
>   azero                :: Complex Double
>   azero                                  = 0
>   acomplex             :: Complex Double → Complex Double
>   acomplex                               = id
>   aamp                 :: Complex Double → Double
>   aamp                                   = magnitude
>   ascale               :: Double → Complex Double → Complex Double
>   ascale s                               = amul (s :+ 0)
>   aadd                 :: Complex Double → Complex Double → Complex Double
>   aadd x y                               = x + y
>   amul                 :: Complex Double → Complex Double → Complex Double
>   amul x y                               = x * y
>   asqrt                :: Complex Double → Complex Double
>   asqrt                                  = sqrt

Signals of interest ===================================================================================================

> sawtoothTable          :: Table
> sawtoothTable                            = tableSinesN 16_384 
>                                                          [      1, 0.5  , 0.3
>                                                            , 0.25, 0.2  , 0.167
>                                                            , 0.14, 0.125, 0.111]
>
> triangleWaveTable      :: Table
> triangleWaveTable                        = tableSinesN 16_384 
>                                                          [      1,  0, -0.5,  0,  0.3,   0
>                                                           , -0.25,  0,  0.2,  0, -0.167, 0
>                                                           ,  0.14,  0, -0.125]
>
> sineTable :: Table
> sineTable = tableSinesN 4096 [1]
>
> deriveRange            :: Integral n ⇒ n → n → [n]
> deriveRange x y                          = if x >= y || y <= 0 then [] else [x..(y-1)]
>
> useModulators          :: Bool
> useModulators                            = True
> -- False to suppress all use of Modulators
> chorusAllPercent       :: Rational
> chorusAllPercent                         = 0
> -- force given Chorus level on ALL notes
> reverbAllPercent       :: Rational
> reverbAllPercent                         = 0
> -- force given Reverb level on ALL notes
> useDefaultMods         :: Bool
> useDefaultMods                           = True
> -- False to suppress all use of defaul Modulators
> useLFO                 :: Bool
> useLFO                                   = True
> -- False to suppress all uses of the low frequency oscillator
> chorusRate             :: Double
> chorusRate                               = 5.0
> -- configures chorus param
> -- suggested default is 5 Hz
> chorusDepth            :: Double
> chorusDepth                              = 0.001
> -- configures chorus param
> -- suggested default is + or - 1/1000 (of the rate)
> cascadeConfig          :: Int
> cascadeConfig                            = 0
> -- number of times to cascade the filter
> useFastFourier         :: Bool
> useFastFourier                           = True
> -- False to employ convolution in time domain

The End
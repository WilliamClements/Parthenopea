> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module ModulationTest where
>
> import Data.List ( foldl' )
> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Debug.Trace ( traceIO )
> import Euterpea ( (<<<), (>>>) )
> import Euterpea.IO.Audio.Basics ( outA, apToHz )
> import Euterpea.IO.Audio.BasicSigFuns ( osc )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types ( AudSF )
> import Euterpea.Music ( Volume, AbsPitch, Dur, absPitch, PitchClass(..) )
> import FRP.UISF.AuxFunctions ( constA )
> import HSoM.Examples.Additive ( sineTable, sfTest1 )
> import Modulation
> import Parthenopea

Testing ===============================================================================================================

A modulator is defined by its sfModSrcOper, its sfModDestOper, and its sfModSrcAmtOper

struct sfInstModList
{
  SFModulator sfModSrcOper;
  SFGenerator sfModDestOper;
  SHORT modAmount;
  SFModulator sfModAmtSrcOper;
  SFTransform sfModTransOper;
};

>
> aEqual                 :: (Eq a, Show a) ⇒ a → a → Bool
> aEqual a b
>   | a /= b                               = error (show a ++ " and " ++ show b ++ " had to be equal!?")
>   | otherwise                            = True
>
> vanillaModulatorWillNotBeEliminated
>                        :: IO Bool
> vanillaModulatorWillNotBeEliminated      = do
>   let m8r                                = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModDest = ToFilterFc}
>   let m8n                                = resolveMods defModulation [m8r] []
>   let succeeded                          = aEqual 1 (countMods m8n)
>   return succeeded
>
> unlinkedModulatorWillBeEliminated
>                        :: IO Bool
> unlinkedModulatorWillBeEliminated        = do
>   let m8r                                = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToFilterFc}
>   let m8n@Modulation{modGraph}           = resolveMods defModulation [m8r] []
>   let succeeded                          = aEqual 0 (countMods m8n)
>   return succeeded
>
> linkedModulatorWillNotBeEliminated
>                        :: IO Bool
> linkedModulatorWillNotBeEliminated       = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToFilterFc}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModDest = ToLink 0}
>   let m8n                                = resolveMods defModulation [m8r0, m8r1] []
>   let succeeded                          = aEqual 2 (countMods m8n)
>   return succeeded
>
> mutuallyCyclicModulatorsWillBeRejected
>                       :: IO Bool
> mutuallyCyclicModulatorsWillBeRejected   = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToLink 1}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromLinked}
>                                           , mrModDest = ToLink 0}
>   let m8n                                = resolveMods defModulation [m8r0, m8r1] []
>   let succeeded                          = aEqual 2 (countMods m8n)
>   return succeeded
>
> repeatedNoteOnsGiveSameResult
>                        :: IO Bool
> repeatedNoteOnsGiveSameResult                       = do
>   let m8r                               = defModulator{mrModId = 0, mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                                       , mrModAmount = 0.5
>                                                       , mrModDest = ToFilterFc}
>   let graph                             = compileMods [m8r]
>
>   let eval                              = evaluateMods ToFilterFc graph 
>   let results                           = [eval (NoteOn x 64) | x ← [0..127]]
>   let answer                            = aEqual True $ all (==0.25) results
>   return answer
>
> differentNoteOnsGiveDifferentResults
>                        :: IO Bool
> differentNoteOnsGiveDifferentResults    = do
>   let m8r                               = defModulator{mrModId = 0, mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                                       , mrModAmount = 0.5
>                                                       , mrModDest = ToFilterFc}
>   let graph                             = compileMods [m8r]
>
>   let eval                              = evaluateMods ToFilterFc graph 
>   let results                           = [eval (NoteOn 64 x) | x ← [0..127]]
>   let answer                            = fst $ foldl' (\(b,x1) x2 → (b && (x1 < x2),x2)) (True, -1) results
>   return answer
>
> supercededModulatorWillBeEliminated
>                        :: IO Bool
> supercededModulatorWillBeEliminated     = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.75                                           
>                                           , mrModDest = ToFilterFc}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.25
>                                           , mrModDest = ToFilterFc}
>   let m8n                                = resolveMods defModulation [m8r0, m8r1] []
>   let succeeded                          = aEqual 1 (countMods m8n)
>   print $ modGraph m8n
>   return succeeded
>
> unsupercededModulatorWillNotBeEliminated
>                        :: IO Bool
> unsupercededModulatorWillNotBeEliminated = do
>   let m8r0                               = defModulator{
>                                             mrModId = 0
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnKey}
>                                           , mrModAmount = 0.75                                           
>                                           , mrModDest = ToFilterFc}
>   let m8r1                               = defModulator{
>                                             mrModId = 1
>                                           , mrModSrc = defModSrc{msSource = FromNoteOnVel}
>                                           , mrModAmount = 0.75
>                                           , mrModDest = ToFilterFc}
>   let m8n                                = resolveMods defModulation [m8r0, m8r1] []
>   let succeeded                          = aEqual 2 (countMods m8n)
>   return succeeded
>
> modulatorTests        :: [IO Bool]       = [vanillaModulatorWillNotBeEliminated
>                                             , unlinkedModulatorWillBeEliminated
>                                             , linkedModulatorWillNotBeEliminated
>                                             , mutuallyCyclicModulatorsWillBeRejected
>                                             , repeatedNoteOnsGiveSameResult
>                                             , differentNoteOnsGiveDifferentResults
>                                             , supercededModulatorWillBeEliminated
>                                             , unsupercededModulatorWillNotBeEliminated]
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
>           conv midi                      = fromIntegral midi / fromIntegral qMidiSize128
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
>     conv midi                            = fromIntegral midi / fromIntegral qMidiSize128
>
>     msg = unwords ["modulationTest004 ", show ping]
>
> modulationTest005      :: IO ()
> modulationTest005
>   | traceNow msg False                   = undefined
>   | otherwise                            = do
>     putStrLn ("mt5 " ++ show avgGain)
>     return ()
>   where
>     minFc, maxFc       :: Int
>     minFc                                = 1500
>     maxFc                                = 13500
>
>     minFq, maxFq       :: Double
>     minFq                                = 0.80
>     maxFq                                = 1.25
>
>     minFt, maxFt       :: Double
>     minFt                                = apToHz $ absPitch $ (A,0)
>     maxFt                                = apToHz $ absPitch $ (C,7)
>
>     -- pick midpoints for now
>     myFc                                 = fromAbsoluteCents ((minFc + maxFc) `div` 2)
>     myFq                                 = (minFq + maxFq) / 2
>     myFt                                 = (minFt + maxFt) / 2
>     myV                                  = 1
>
>     doTheseQ           :: [Double]       = [fromIntegral x / 100 | x ← [80,90..125]]
>     theyHaveQ          :: Double         = fromIntegral $ length doTheseQ
>
>     doTheseFt          :: [Double]       = [(apToHz . absPitch) (C, x) | x ← [0..9]]
>     theyHaveFt         :: Double         = fromIntegral $ length doTheseFt
>     avgGain                              = sum (map iter doTheseFt) / theyHaveFt
>
>     iter               :: Double → Double
>     iter newFt
>       | traceNow msg' False              = undefined
>       | otherwise                        = rms
>       where
>         iterData                         = IterData newFt myFq newFt myV
>         dbls                             = toSamples 0.5 (driver iterData)
>         n              :: Double         = fromIntegral $ length dbls
>         rms                              = sum (map abs dbls) / n
>         msg'                             = unwords ["modulationTest005/iter ", show rms, "\n", show iterData]
>
>     driver             :: IterData → AudSF () Double
>     driver itd@IterData{theFc, theFq, theFt, theVel}
>                                          = 
>       proc () → do
>         a1 ← osc sineTable 0 <<< constA theFt ⤙ () 
>         a2 ← sf ⤙ (a1, theFc)
>         outA ⤙ a2*theVel
>       where
>         sf = filterSVF theFq
>
>     -- do the test from (A, 0) to (C, 9)
>     msg = unwords ["modulationTest005 ", show $ avgGain * 1000]
>
> table2vals             :: [Double] → [(Double, Double, Double, Double)]
> table2vals                               = zipWith (curry convFun) [0 .. ]
>   where
>     convFun            :: (Int, Double) → (Double, Double, Double, Double)
>     convFun (i, y)                       = (fromIntegral i / 128, y, 0, 0)
>
> vals' = table2vals (modulationTest004 (allMappings !! 15))
>         
> countMods            :: Modulation → Int
> countMods m8n@Modulation{modGraph}       = length $ concat (Map.elems modGraph)
>
> -- range = 0.75 (max resonance) ..    1.25 (no resonance)
> --         960 cB    -170-    ..      0 cB
>
> tLowSVF = outFile "lowSVF.wav" 10 $
>            sfTest1 (filterSVF {- (fromCents 13500) -} 0.80) 10 (absPitch (C,5)) 64 []
>
> data IterData =
>   IterData {
>     theFc              :: Double
>   , theFq              :: Double
>   , theFt              :: Double
>   , theVel             :: Double} deriving Show


> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module ModulationTest where
>
> import Data.List ( foldl' )
> import Debug.Trace ( traceIO )
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
>   let m8n                                = resolveMods defModulation [m8r]
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
>   let m8n@Modulation{modGraph}           = resolveMods defModulation [m8r]
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
>   let m8n                                = resolveMods defModulation [m8r0, m8r1]
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
>   let m8n                                = resolveMods defModulation [m8r0, m8r1]
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
>   let m8n                                = resolveMods defModulation [m8r0, m8r1]
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
>   let m8n                                = resolveMods defModulation [m8r0, m8r1]
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
> table2vals             :: [Double] → [(Double, Double, Double, Double)]
> table2vals                               = zipWith (curry convFun) [0 .. ]
>   where
>     convFun            :: (Int, Double) → (Double, Double, Double, Double)
>     convFun (i, y)                       = (fromIntegral i / 128, y, 0, 0)
>
> vals' = table2vals (modulationTest004 (allMappings !! 15))

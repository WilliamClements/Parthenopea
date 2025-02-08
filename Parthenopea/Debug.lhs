> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-} 
> {-# LANGUAGE UnicodeSyntax #-}

Debug
William Clements
February 2, 2025

> module Parthenopea.Debug
>        (  accommodate
>         , aEqual
>         , clip
>         , deJust
>         , diagnosticsEnabled
>         , notracer
>         , profess
>         , professInRange
>         , runTests
>         , runTestsQuietly
>         , traceAlways
>         , traceIf
>         , traceNever
>         , traceNot
>         , traceNow
>         , tracer
>         )
>         where
>
> import Data.List ( foldl' )
> import Data.Maybe
> import Debug.Trace
>
> accommodate            :: Ord n ⇒ (n, n) → n → (n, n)
> accommodate (xmin, xmax) newx            = (min xmin newx, max xmax newx)
>
> clip                   :: Ord n ⇒ (n, n) → n → n
> clip (lower, upper) val                  = min upper (max lower val)
>
> profess                :: Bool → String → a → a
> profess assertion msg something          = if not assertion
>                                              then error (unwords ["Failed assertion --", msg])
>                                              else something
>
> professInRange         :: (Eq a, Ord a, Show a) ⇒ (a, a) → a → String → a → a
> professInRange rng val role            = profess
>                                              (val == clip rng val)
>                                              (unwords ["out of", role, "range", show rng, show val])
>
> deJust                 :: ∀ a. String → Maybe a → a
> deJust tag item                          = profess (isJust item) (unwords["expected Just for", tag]) (fromJust item)
>
  
Tracing ===============================================================================================================

> traceIf, traceNow, traceAlways, traceNever, traceNot
>                        :: String → a → a
> traceIf str expr                         = if diagnosticsEnabled then trace str expr else expr
> traceNow                                 = trace
> traceAlways                              = trace
> traceNever _ expr                        = expr
> traceNot _ expr                          = expr
>
> tracer                 :: Show a ⇒ String → a → a
> tracer str x                             =
>   if True
>     then traceNow (unwords [str, "=", show x]) x
>     else x
>
> notracer               :: Show a ⇒ String → a → a
> notracer _ x                             = x

Test runner ===========================================================================================================

> aEqual                 :: (Eq a, Show a) ⇒ a → a → Bool
> aEqual x y
>   | x /= y                               = error (show x ++ " and " ++ show y ++ " had to be equal!?")
>   | otherwise                            = True
>
> runTests               :: [IO Bool] → IO ()
> runTests tests                           = do
>   results                                ← sequence tests
>   let nSuccesses::Int                    = foldl' (\n t → n + if t then 1 else 0) 0 results
>   putStrLn $ unwords ["results =", show results]
>   putStrLn $ unwords ["  ", show nSuccesses, "/", show $ length results]
>
> runTestsQuietly        :: [IO Bool] → IO Bool
> runTestsQuietly tests                    = do
>   results                                ← sequence tests
>   let nSuccesses                         = foldl' (\n t → n + if t then 1 else 0) 0 results
>   return (nSuccesses == length results)
>

Debugging Flags =======================================================================================================

> diagnosticsEnabled     :: Bool
> diagnosticsEnabled                       = False

The End
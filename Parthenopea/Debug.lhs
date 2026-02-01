> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Debug
William Clements
February 2, 2025

> module Parthenopea.Debug
>        (  aEqual
>         , diagnosticsEnabled
>         , notracer
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
> import Debug.Trace
  
Tracing ===============================================================================================================

> traceIf, traceNow, traceAlways, traceNever, traceNot
>                        :: String → a → a
> traceIf str expr                         = if diagnosticsEnabled then trace str expr else expr
> traceNow                                 = trace
> traceAlways                              = if diagnosticsLevel > 0 then traceNow else traceNot
> traceNever _ expr                        = expr
> traceNot _ expr                          = expr
>
> tracer                 :: Show a ⇒ String → a → a
> tracer str x                             =
>   if diagnosticsLevel > 0
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

Debugging Flags =======================================================================================================

> diagnosticsLevel       :: Rational
> diagnosticsLevel                         = 3/4
> 
> diagnosticsEnabled     :: Bool
> diagnosticsEnabled                       = diagnosticsLevel > 1/2

The End
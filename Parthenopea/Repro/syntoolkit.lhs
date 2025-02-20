> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> 

Port (C++ to Haskell) of STK (called here Syntoolkit)

> module Parthenopea.Repro.SynToolkit where
>
> import Data.Array.Unboxed
> import Data.Word
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

The End
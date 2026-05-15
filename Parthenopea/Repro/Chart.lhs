> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

Chart
William Clements
November 9, 2023

> module Parthenopea.Repro.Chart (
>          chartColors
>        , chartEnvelope
>        , chartPoints
>        , graphSF
>        , Section( .. ) ) where
>
> import Control.Lens
> import Data.Default.Class
> import qualified Data.Vector.Unboxed     as VU
> import Diagrams.Prelude
> import Euterpea.IO.Audio.Types
> import Graphics.Rendering.Chart
> import Graphics.Rendering.Chart.Backend.Diagrams
> import Parthenopea.Repro.Discrete
> import Parthenopea.Repro.Envelopes
> import Parthenopea.Repro.Synthesizer
> import Parthenopea.SoundFont.Utility

Chart =================================================================================================================

> data Section                             =
>   Section {
>     section_color    :: AlphaColour Double
>   , section_points   :: [(Double, Double)]}
>   deriving Show
>
> chartPoints            :: String → String → [Section] → IO Bool
> chartPoints title tag sects              =
>   do
>     let cars = plot_errbars_values .~ [symErrPoint x y dx dy | (x,y,dx,dy) ← vals']
>                    $ plot_errbars_title .~ "test"
>                    $ def
>     let plotVector = map (toPlot . stylize) sects
>     let layout' = layout_title .~ title $ layout_plots .~ toPlot cars : plotVector $ def
>     let chart' = toRenderable layout'
>     let path = concat ["chaP", tag, ".svg"]
>     putStrLn $ unwords ["rendering", path]
>     _ ← renderableToFile def path chart'
>     putStrLn $ unwords ["rendered", path]
>     return True
>   where
>     vals'                                = map padThem (concatMap section_points sects)
>                                              where padThem (x, y) = (x, y, 0, 0)
>
>     stylize            :: Section → Plot Double Double
>     stylize sect                         = 
>       let
>         noints = plot_points_style .~ filledCircles 2 sect.section_color
>                    $ plot_points_values .~ sect.section_points
>                    $ plot_points_title .~ "test data"
>                    $ def
>       in
>         toPlot noints
>
> chartColors            :: [AlphaColour Double]
>                                          =
>   cycle
>     [  blue    `withOpacity` 2
>     ,  orange  `withOpacity` 2
>     ,  green   `withOpacity` 2
>     ,  red     `withOpacity` 2
>     ,  purple  `withOpacity` 2]
>
> chartEnvelope          :: String → TimeFrame → FEnvelope → Double → IO Bool
> chartEnvelope tag tf fe clockRate        = do
>   print segs
>   case mdsig of
>     Nothing                              → return False
>     Just dsig                            →
>       do
>         chartDiscreteSig clockRate nPoints dsig tag
>         return True
>   where
>     (r, segs)                            = proposeSegments tf fe
>     mdsig                                = vetAsDiscreteSig clockRate r segs
>     nPoints            :: Int            = round $ clockRate * (tf.tfSecsScored + 0.5)
> chartDiscreteSig       :: Double → Int → DiscreteSig Double → String → IO Bool
> chartDiscreteSig clockRate nPoints dsig tag
>                                          = chartPoints "Discrete" tag [sec]
>   where
>     sec                                  = Section (opaque blue) (zip xs ys)
>     xs, ys             :: [Double]
>     xs                                   = map ((/ clockRate) . fromIntegral) [0::Int ..]
>     ys                                   = take nPoints (VU.toList dsig.dsigVec)
>
> graphSF                :: ∀ p . Clock p ⇒ Double → Double → Double → Signal p () Double → [(Double, Double)]
> graphSF secs clock startT sf
>   | abs (clock - sr) > epsilon           = error $ unwords [fName, "(clock,sr)=", show (clock, sr)]
>   | nPoints >= 2 ^ (22::Int)             = error $ unwords [fName, show nPoints, "is too large!"]
>   | nActual /= nPoints                   = error $ unwords [fName, "(nPoints, nActual)=", show (nPoints, nActual)]
>   | otherwise                            = zip onsets (VU.toList vec)
>   where
>     fName                                = "graphSF"
>     sr                                   = rate (undefined::p)
>
>     nPoints            :: Int            = round (secs * clock)
>     vec                :: VU.Vector Double
>                                          = VU.take nPoints (deJust fName (fromSignal fName secs sf)).dsigVec
>     nActual            :: Int            = VU.length vec
>     onsets             :: [Double]       = [startT + (fromIntegral i/clock) | i ← [0..]]

The End
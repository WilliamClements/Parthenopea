> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

Chart
William Clements
November 9, 2023

> module Chart ( Section( .. ), chartPoints, table2vals ) where
>
> import Control.Lens
> import Data.Colour
> import Data.Colour.Names
> import Data.Default.Class
> import Graphics.Rendering.Chart
> import Graphics.Rendering.Chart.Backend.Diagrams

Chart =================================================================================================================

> data Section                             =
>   Section {
>       section_color    :: AlphaColour Double
>     , section_points   :: [(Double, Double)]} deriving Show
>
> chartPoints            :: String → [Section] → IO ()
> chartPoints tag sects                    =
>   do
>     let cars = plot_errbars_values .~ [symErrPoint x y dx dy | (x,y,dx,dy) ← vals']
>                    $ plot_errbars_title .~ "test"
>                    $ def
>     let plotVector = map (toPlot . stylize) sects
>     let layout' = layout_title .~ "SFEnvelope" $ layout_plots .~ toPlot cars : plotVector $ def
>     let chart' = toRenderable layout'
>     let path = concat ["chaP", tag, ".svg"]
>     putStrLn $ unwords ["rendering", path]
>     _ ← renderableToFile def path chart'
>     putStrLn $ unwords ["rendered", path]
>     return ()
>   where
>     vals' = map padThem (concatMap section_points sects)
>
>     padThem            :: (Double, Double) → (Double, Double, Double, Double)
>     padThem (x, y) = (x, y, 0, 0)
>
>     stylize            :: Section → Plot Double Double
>     stylize Section{ .. }                = 
>       let
>         noints = plot_points_style .~ filledCircles 2 section_color
>                    $ plot_points_values .~ section_points
>                    $ plot_points_title .~ "test data"
>                    $ def
>       in
>         toPlot noints
>
> table2vals             :: Double → [Double] → [(Double, Double, Double, Double)]
> table2vals scalix                        = zipWith convFun [0..]
>   where
>     convFun            :: Int → Double → (Double, Double, Double, Double)
>     convFun i y                          = (fromIntegral i / scalix, y, 0, 0)

The End
> {-# LANGUAGE UnicodeSyntax #-}
>

Chart =================================================================================================================

> module Chart where
>
> import Control.Lens
> import Data.Colour
> import Data.Colour.Names
> import Data.Default.Class
> import Graphics.Rendering.Chart
> import Graphics.Rendering.Chart.Backend
> import Graphics.Rendering.Chart.Backend.Diagrams
> import Modulation
>
> cars = plot_errbars_values .~ [symErrPoint x y dx dy | (x,y,dx,dy) ← vals']
>           $ plot_errbars_title .~"test"
>           $ def
>
> noints = plot_points_style .~ filledCircles 2 (opaque blue)
>	          $ plot_points_values .~ [(x,y) |  (x,y,dx,dy) ← vals']
>           $ plot_points_title .~ "test data"
>           $ def
>
> layout = layout_title .~ "SFEnvelope"
>           $ layout_plots .~ [toPlot cars, toPlot noints]
>           $ def
>
> chart :: Renderable ()
> chart = toRenderable layout 
>
> zmain :: IO ()
> zmain = do
>   print $ map (\x → show x ++ " ") vals'
>   renderableToFile def "r00tour.svg" chart
>   putStrLn ("numDots=" ++ show (length vals'))
>   return ()

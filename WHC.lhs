> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE UnicodeSyntax #-}
>

Chart =====================================================================================

> module WHC  where
>
> import Control.Arrow
> import Control.Lens
> import Data.Colour
> import Data.Colour.Names
> import Data.Default.Class
> import Euterpea
> import Graphics.Rendering.Chart
> import Graphics.Rendering.Chart.Backend
> import Graphics.Rendering.Chart.Backend.Diagrams
> import HSoM
> -- import -- KnightsTour
> import Parthenopea
> import System.Environment(getArgs)
>
> --------- vals = [ (x,sin (exp x),sin x/2,cos x/10) | x ← [1..20]]
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
>
> fun :: Int -> Int -> [Double]
> fun n 0 = []
> fun n 1 = [1.0]
> fun n p = replicate n (fromIntegral p)
>
> ints :: [Int]
> ints = [3,4,2,4,5]
>
> qmain :: IO ()
> qmain = do
>   let x = map (fun 4) ints
>   putStrLn ("x=" ++ show x)
>   let y = concat x
>   putStrLn ("y=" ++ show y)
>   let z = concatMap (fun 4) ints
>   putStrLn ("z=" ++ show z)
>   return ()
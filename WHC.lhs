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
> import KnightsTour
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
> layout = layout_title .~ "Knight's Tour"
>           $ layout_plots .~ [toPlot cars, toPlot noints]
>           $ def
>
> chart :: Renderable ()
> chart = toRenderable layout 
>
> zmain :: IO ()
> zmain = do
>   renderableToFile def "r00tour.svg" chart
>   putStrLn ("numDots=" ++ show (length vals))
>   return ()
>
> reedyWav = tableSinesN 1024 [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15, 
>                              0.0, 0.02, 0.05, 0.03]
>
> reed :: Instr (Stereo AudRate)
> reed dur pch vol params = 
>     let reedy = osc reedyWav 0
>         freq  = apToHz pch
>         vel   = fromIntegral vol / 127 / 3
>         env   = envLineSeg [0, 1, 0.8, 0.6, 0.7, 0.6, 0] 
>                            (replicate 6 (fromRational dur/6))
>     in proc _ → do
>       amp ← env ⤙ ()
>       r1 ← reedy ⤙ freq
>       r2 ← reedy ⤙ freq + (0.023 * freq)
>       r3 ← reedy ⤙ freq + (0.019 * freq)
>       let [a1, a2, a3] = map (* (amp * vel)) [r1, r2, r3]
>       let rleft = a1 * 0.5 + a2 * 0.44 * 0.35 + a3 * 0.26 * 0.65
>           rright = a1 * 0.5 + a2 * 0.44 * 0.65 + a3 * 0.26 * 0.35
>       outA ⤙ (rleft, rright)
>
> saw = tableSinesN 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125, 
>                         0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]
>
> plk :: Instr (Stereo AudRate)
> plk dur pch vol params = 
>     let vel  = fromIntegral vol / 127 / 3
>         freq = apToHz pch
>         sf   = pluck saw freq SimpleAveraging
>     in proc _→ do
>          a ← sf ⤙ freq
>          outA ⤙ (a * vel * 0.4, a * vel * 0.6)
>
> myBass, myReed :: InstrumentName
> myBass = CustomInstrument "pluck-like"
> myReed = CustomInstrument "reed-like"
>
> myMap :: InstrMap (Stereo AudRate)
> myMap = [{- myBass, plk), (myReed, reed), -} (Violin, reed), (SynthBass1, plk)]
>
> -- this is for reference only; it compiles, but gives exception due to empty map
> takeItToTheWave :: FilePath → Music (Pitch, Volume) → IO()
> takeItToTheWave fp m = outFile fp secs sig
>   where
>     (secs, sig) = renderSF m myMap


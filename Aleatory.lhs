Examples using random number generation with Euterpea's Music data structures
William Clements
Last modified: 27-September-2022

> module Aleatory where
> import Euterpea
> import Euterpea.Music
> import System.Random
> import Debug.Trace
> import Data.List (unfoldr)
> import Control.Monad (replicateM)
> import Control.Monad.ST (runST)
> import Control.Monad.State
> import Fanfare

> main = do
>     seed  <- newStdGen
>     let rs = randomlist 10 seed
>     print rs
>     let moreRs = randomlist 10 seed
>     print ""
>     print moreRs

> randomlist :: Int -> StdGen -> [Int]
> randomlist n = take n . unfoldr (Just . random)

> gen1000Randoms :: Int -> [Int]
> gen1000Randoms seed =
> 	let rolls :: RandomGen g => Int -> g -> [Int]
> 	    rolls n = take n . unfoldr (Just . uniformR (0, 127))
> 	    pureGen = mkStdGen seed
> 	in
> 	    rolls 1000 pureGen

> randInts :: StdGen -> [Int]
> randInts g =
>    let (x, g') = random g
>    in x : randInts g'

> infiniteList = [9..]
> finiteList = [3,4,77]

> sGen :: StdGen
> sGen = mkStdGen 59

> randIntegers :: [Int]
> randIntegers = randomRs (0, 127) sGen

> getRandInteger :: StdGen -> [Int]
> getRandInteger sGen = take 10 randIntegers

> b1 = getRandInteger sGen
> b2 = getRandInteger sGen

> bully :: [Int]
> bully = randoms sGen

> c1 = take 5 $ bully
> c2 = take 5 $ bully

> take10 seed =
>    let g = mkStdGen seed
>        range = ('a', 'z')
>    in randomR range g

> type R a = State StdGen a

> runRandom :: R a -> Int -> a
> runRandom action seed = evalState action $ mkStdGen seed

> rand :: R Double
> rand = do
>    gen <- get
>    let (r, gen') = random gen
>    put gen'
>    return r

> randPair :: R (Double, Double)
> randPair = do
>    x <- rand
>    y <- rand
>    return (x, y)

> oneNormal :: R Double
> oneNormal = do
>    pair <- randPair
>    return $ boxMuller 0 1 pair
>
> normals :: R [Double]
> normals = mapM (\_ -> oneNormal) $ repeat ()

> boxMuller :: Double -> Double -> (Double, Double) -> Double
> boxMuller mu sigma (r1,r2) =  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2)

> someNormals :: {- Show p => -} Int -> R [Double]
> someNormals x = liftM (take x) normals

> myAlgorithm :: R [Bool]
> myAlgorithm = do
>    xs <- someNormals 10
>    ys <- someNormals 10
>    let xys = zip xs ys
>    return $ uncurry (<) <$> xys

comment runRandom myAlgorithm 42

> test :: Int -> IO [Int]
> test n = sequence $ replicate n $ randomRIO (1,6::Int)

> testMidi file = do
>   x <- importFile file
>   case x of Left err -> error err
>             Right m -> do
>                 let v = fromMidi m
>                 putStrLn $ show v
>                 play v

> playRandomly = do
>    z <- newStdGen
>    let durations = toRational <$> (randomRs (0.1,2) z :: [Float])
>    play $ line
>      [ note 4 duration
>      | (note, duration) <- zip [c, c, d, e, a, a, g] durations
>      ]

Yahozna ===========================================================================

> yahozna =
>     removeZeros
>     $ tempo (4/1)
>     $ transpose 0
>     $ keysig C Mixolydian
>     $ instrument DistortionGuitar
>     $ Modify (Phrase [Art $ Staccato (2/3)]) allYahozna

> guitarLick = line [c 3 hn, c 3 qn, c 3 qn, c 3 hn, rest hn]
>              :+: line [c 3 qn, c 3 qn, c 3 qn, c 3 qn]

> allYahozna :: Music Pitch
> allYahozna =
>    rest wn
>    :+: times 4 (guitarLick :=: transpose 7 guitarLick)
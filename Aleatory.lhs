Examples using random number generation with Euterpea's Music data structures
William Clements
Last modified: 27-September-2022

> module Aleatory where
> import Euterpea
> import System.Random
> import Debug.Trace
> import Data.List
> import Control.Monad (replicateM)
> import Control.Monad.ST (runST)

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
>    let (x, g') = next g
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

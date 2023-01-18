{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

Playground using random number generation with Euterpea's Music data structures
William Clements
Last modified: 27-September-2022

> module Aleatory where
>
> import Codec.Midi
> import Control.Concurrent
> import Control.Concurrent.STM.TChan
> import Data.Array
> import Data.List (unfoldr, elemIndex, sortBy, insertBy)
> import Euterpea
> import Euterpea.IO.MIDI.MEvent
> import FRP.UISF.AuxFunctions (DeltaT)
> import GHC.Conc
> import Parthenopea
> import System.Environment
> import System.Random

> oldmain = do
>     seed  <- newStdGen
>     let rs = randomlist 10 seed
>     print rs
>     seed2 <- newStdGen
>     let moreRs = randomlist 10 seed2
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
> randIntegers = randomRs (32, 92) sGen

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

> boxMuller :: Double -> Double -> (Double, Double) -> Double
> boxMuller mu sigma (r1,r2) =  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2)

> test :: Int -> IO [Int]
> test n = sequence $ replicate n $ randomRIO (1,6::Int)

> testMidi file = do
>   x <- importFile file
>   case x of Left err -> error err
>             Right m -> do
>                 let v = fromMidi m
>                 putStrLn $ show v
>                 play v
>
> {-
> arrayList :: [Int] -> Array Int [Int]
> arrayList [a] = listArray (0, 2) [[a]]
> -}
>
>
> playRandomly = do
>    z <- newStdGen
>    let durations = toRational <$> (randomRs (0.1,2) z :: [Float])
>    play $ line
>      [ note 4 duration
>      | (note, duration) <- zip [c, c, d, e, a, a, g] durations
>      ]

> nub l                   = nub' l []
>  where
>    nub' [] _           = []
>    nub' (x:xs) ls
>        | x `elem` ls   = nub' xs ls
>        | otherwise     = x : nub' xs (x:ls)

> returnPairList :: Int -> Music Pitch
> returnPairList seed = 
>    let z = mkStdGen seed
>        durations = toRational <$> (randomRs (0.1,2) z :: [Float])
>    in
>       line [note 4 duration
>            | (note, duration) <- zip [c,e,c,b] durations
>            ]
>
> getMidi :: [MEvent] -> Midi
> getMidi mevs = toMidi mevs
>
> makeDistArrayMusic :: Int -> Music (Pitch, Volume) -> Array Int Int
> makeDistArrayMusic nDivs m
>   | traceIf msg False = undefined
>   | otherwise = makeHistogram nDivs
>                 $ extractTimes
>                 $ collectMEvents
>                 $ m
>   where
>      msg = unwords
>            [ "histo="
>            , show
>              $ elems
>              $ makeHistogram nDivs
>              $ extractTimes
>              $ collectMEvents
>              $ m]
>
> collectMEvents :: Music (Pitch, Volume) -> Performance
> collectMEvents m = fst (musicToMEvents defCon (toMusic1 m))
>
> extractTimes :: Performance -> [Double]
> extractTimes p = map (\x -> fromRational $ eTime x) p
>
> defCon :: MContext
> defCon = MContext
>           {mcTime = 0
>            , mcInst = AcousticGrandPiano
>            , mcDur = metro 120 qn
>            , mcVol=100}
>
> metro :: Int -> Dur -> DurT
> metro setting dur  = 60 / (fromIntegral setting * dur)
>

Aleatory Music ====================================================================

> pitches :: [Pitch]
> pitches = map pitch randIntegers
>
> durations :: [Dur]
> durations = map toRat randIntegers
>   where
>     toRat :: Int -> Rational
>     toRat i =
>        let j = i `mod` 8
>        in (toRational j) / 8
>
> aleatoryMusic =
>    line $ [ note theDur thePitch
>           | (theDur, thePitch) <- zip durations pitches
>           ]
> 

Concurrent ========================================================================

> readQ :: TChan a -> IO a
> readQ = atomically . readTChan
>
> writeQ :: TChan a -> a -> IO ()
> writeQ ch v = atomically $ writeTChan ch v
> 
> realQ :: Int -> IO ()
> realQ seed = do
>    putStrLn "entering realQ..."
>    queue <- atomically $ newTChan
>    readerThread queue
>    mapM (sex2Quake queue) $ sextuplets (mkStdGen seed)
>    error "not expected for mapM to finish"
>
>    where
>
>       readerThread :: TChan Lake -> IO ThreadId
>       readerThread queue = forkIO sloop
>          where
>             sloop :: IO ()
>             sloop = do
>                lake <- readQ queue
>                music <- quake2Music lake
>                -- playS music
>                -- putStrLn $ show music
>                sloop
>
>       sex2Quake :: TChan Lake -> [Double] -> IO Lake
>       sex2Quake queue rs 
>          | (length rs >= 6) =
>               do
>                  ((getStdRandom $ randomR (100000,10000000)) >>= threadDelay)
>                  let lake = mkLake rs
>                  writeQ queue lake
>                  return lake
>          | otherwise = error "insufficiently sized list for sex2Quake" 
>
>       quake2Music :: Lake -> IO (Music (Pitch, Volume))
>       quake2Music lake@Lake 
>                    {cInst = iname
>                    , cWch = w
>                    , cPch = k
>                    , cVol = vol
>                    , cKey = ck
>                    , cVel = vel} =
>          do
>             putStrLn $ show iname
>             m <- return
>                  $ removeZeros
>                  $ instrument iname
>                  $ tempo (toRational vel)
>                  $ transpose (k - absPitch (C,4))
>                  $ addVolume vol
>                  $ mel w
>             return m
>          where mel _ = c 4 hn
>
> data Rake = Rake {bWch     :: Int, 
>                   bXPos    :: Double, 
>                   bYPos    :: Double,
>                   bSnd     :: PercussionSound,
>                   bVol     :: Volume,
>                   bVel     :: Double}
>    deriving (Show, Eq, Ord)
>
> rakeXDim, rakeYDim :: Double
> rakeXDim = 100.0
> rakeYDim = 161.8
>
> sex2Rake :: [Double] -> Rake
> sex2Rake rs
>    | (length rs >= 6) = 
>       Rake {
>          bWch =    floor           $ denorm (rs!!0) (0,4.99999)
>          , bXPos =                   denorm (rs!!1) (0,rakeXDim - 0.000001)
>          , bYPos =                   denorm (rs!!2) (0,rakeYDim - 0.000001)
>          , bSnd =  toEnum $ round  $ denorm (rs!!3) (0,percussionLimit)
>          , bVol =  round           $ denorm (rs!!4) (50,110)
>          , bVel =                    denorm (rs!!5) (2,5)}
>    | otherwise = error "insufficiently sized list for sex2Rake" 
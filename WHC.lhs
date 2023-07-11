> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Use camelCase" #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>

Chart =================================================================================================================

> module WHC  where
>
> import Control.Applicative
> import Control.Applicative.Alternative
> import Control.Arrow
> import Control.Lens
> import Data.Array
> import Data.Char
> import Data.Colour
> import Data.Colour.Names
> import Data.Default.Class
> import Data.Int
> import Data.List
> import Data.Word
> -- import Euterpea
> import Graphics.Rendering.Chart
> import Graphics.Rendering.Chart.Backend
> import Graphics.Rendering.Chart.Backend.Diagrams
> import HSoM
> -- import -- KnightsTour
> import Parthenopea
> import Synthesizer
> import qualified Text.FuzzyFind as T
> import System.Environment(getArgs)
> import GHC.Num (naturalAnd)
  
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
> fun :: Int → Int → [Double]
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
>
> newtype Parser a = P (String → [(a, String)])
>
> parse :: Parser a → String → [(a, String)]
> parse (P p) = p
>
> item :: Parser Char
> item = P (\case
>              []        → []
>              (x : xs)  → [(x, xs)])
>
> instance Functor Parser where
>   fmap g p = P (\inp → case parse p inp of
>                          [] → []
>                          [(v, out)] → [(g v, out)])
>
> string :: String → Parser String
> string [] = return []
> string (x:xs) = do
>                   char x
>                   string xs
>                   return (x:xs)
> char :: Char → Parser Char
> char x = sat (== x)
>
> sat :: (Char → Bool) → Parser Char
> sat p =
>   do
>     x ← item
>     if p x then return x else empty
>
> {-
> class Applicative f ⇒ Alternative f where
>   empty :: f a
>   (<|>)  :: f a → f a → f a
>   -- many :: f a → f [a]
>   many x = some x <!> pure []
>   -- some :: f a → f [a]
>   some x = pure (:) <*> x <*> many x
> -}
>
> instance Monad Parser where
>   p >>= f = P (\inp → case parse p inp of
>                        [] → []
>                        [(v, out)] → parse (f v) out)
>
> instance Alternative Parser where
>   empty = P (const [])
>   p <|> q = P (\inp → case parse p inp of
>                        [] → parse q inp
>                        [(v, out)] → [(v, out)])
>
> digit :: Parser Char
> digit = sat isDigit
>
> nat :: Parser Int
> nat =
>   do
>     xs ← some digit
>     return (read xs)
>
> mInteger :: Parser Int
> mInteger =
>   do
>     char '-'
>     n ← nat
>     return (-n)
>     -- <|> nat
>     
> instance Applicative Parser where
>   pure v = P (\inp → [(v, inp)])
>
>   pg <*> px = P (\inp → case parse pg inp of
>                          [] → []
>                          [(g, out)] → parse (fmap g px) out)
>
> m_gulag :: Parser String
> m_gulag = fmap show mInteger
>
> 
> convu8tos32 :: Word8 → Int32
> convu8tos32 = fromIntegral
>
> convs8tos32 :: Int8 → Int32
> convs8tos32 = fromIntegral
>
> convs8tou32 :: Int8 → Word32
> convs8tou32 = fromIntegral
>
> convs8tou8 :: Int8 → Word8
> convs8tou8 = fromIntegral
>
> busterHill :: Array Int Int
> busterHill = array (1, 99) [(3,7)]
>
> loopGain1 = 0.742
> loopGain2 = 0.733
> loopGain3 = 0.715
> loopGain4 = 0.697
> loopGain5 = 0.7
> loopGain6 = 0.7
>
> delayTime1 = 0.004799
> delayTime2 = 0.004999
> delayTime3 = 0.005399
> delayTime4 = 0.005801
> delayTime5 = 0.001051
> delayTime6 = 0.000337
>
> qstrings               :: [String]       = ["piano", "guitar", "snare"]
> istrings               :: [String]       = ["Grand Piano 2", "pianobad", "Guitar Harmonics", "snake"]
>
> -- findFuzzy              :: [String] → String → [T.Alignment]
> -- findFuzzy qs i = flip fuzzyFind [i] qs
>
> ymain :: IO ()
> ymain = do
>   -- let buzz = map tryAll (findFuzzy qstrings) istrings
>   let results = map tryAll qstrings
>   mapM_ eval results
>   -- let alignments = fuzzyFind qstrings istrings
>   -- putStrLn ("pairs=" ++ show showers)
>
> tryAll                 :: String → (String, [T.Alignment])
> tryAll q = (q, T.fuzzyFind [q] istrings)
>
> eval                   :: (String, [T.Alignment]) → IO ()
> eval (q, als) =
>   do
>     let n = length als
>     let str = if null als
>               then "empty"
>               else showAlign (maximum als)
>     putStrLn (q ++ ": num candidates = " ++ show n ++ " = " ++ str)
>
> showAlign              :: T.Alignment → String
> showAlign r = reformed
>   where
>     t = T.score r
>     u = T.result r
>     reformed = concatMap isos $ T.segments u
>     isos               :: T.ResultSegment → String
>     isos r = case r of
>               T.Gap s → s
>               T.Match s → s
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
> import Covers
> import Data.Array
> import Data.Char
> import qualified Data.Bifunctor          as BF
> import Data.Int
> import Data.List
> import qualified Data.Map                as Map
> import Data.Maybe
> import Data.Word
> import Euterpea
> import HSoM
> -- import -- KnightsTour
> import Parthenopea
> import Percussion
> import Synthesizer
> import qualified Text.FuzzyFind as T
> import System.Environment(getArgs)
> import GHC.Num (naturalAnd)
  
> --------- vals = [ (x,sin (exp x),sin x/2,cos x/10) | x ← [1..20]]
>
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
> newtype Parser a = Ped (String → [(a, String)])
>
> parse :: Parser a → String → [(a, String)]
> parse (Ped p) = p
>
> item :: Parser Char
> item = Ped (\case
>              []        → []
>              (x : xs)  → [(x, xs)])
>
> instance Functor Parser where
>   fmap g p = Ped (\inp → case parse p inp of
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
>   p >>= f = Ped (\inp → case parse p inp of
>                        [] → []
>                        [(v, out)] → parse (f v) out)
>
> instance Alternative Parser where
>   empty = Ped (const [])
>   p <|> q = Ped (\inp → case parse p inp of
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
>   pure v = Ped (\inp → [(v, inp)])
>
>   pg <*> px = Ped (\inp → case parse pg inp of
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
>
> type Si = (Int, [(String, Int)])
>
> foo                    :: Int → [(String, [String])]
> foo n                                    =
>   let
>     isOddLength        :: String → Maybe Int
>     isOddLength s                        = if length s `mod` 2 == 1
>                                              then Just $ length s
>                                              else Nothing
>     hoop               :: [Si]
>     hoop                                 = [(101, [("gh",3),("h",5),("i",7)]), (103, [("m", 11),("nnn",13)] )]
>
>     words                                = map (BF.second (map snd)) hoop
>     idents                               = map (BF.bimap show (map show)) words
>     matched                              = map (BF.second (mapMaybe isOddLength)) idents
>   in
>     idents
>
> primes :: (Integral a) ⇒ [a]
> primes = 2 : 3 : ([5,7..] `minus`
>                      foldr (\(x:xs) → (x:) . union xs) []
>                           [[p*p, p*p+2*p..] | p ← tail primes])
>
>  -- ordered lists, difference and union
> minus (x:xs) (y:ys) = case compare x y of 
>            LT → x : minus  xs  (y:ys)
>            EQ →     minus  xs     ys 
>            GT →     minus (x:xs)  ys
> minus  xs     _     = xs
>
> {-
> union (x:xs) (y:ys) = case (compare x y) of 
>            LT → x : union  xs  (y:ys)
>            EQ → x : union  xs     ys 
>            GT → y : union (x:xs)  ys
> union  xs     []    = xs
> union  []     ys    = ys
> -}

> m1 >=> m2 = \x →
>   let (y, s1) = m1 x
>       (z, s2) = m2 y
>    in (z, s1 ++ s2)
>
> mmm1 n = (-n, "foody")
> mmm2 n = (n+1, "goofy")
>
> dumb = foldr (\x y → (x + last y) : y) [-1]
> stupid, dumb :: [Int] → [Int]
> stupid = foldl' junk [0]
> junk xs y = [last xs + y]

> bug = [1,2,3,4,5]
> lFolder :: [Int] → Int → [Int]
> rFolder :: Int → [Int] → [Int]
>
> llll = foldl' lFolder [1] bug
> rrrr = foldr  rFolder [1] bug
>
> lFolder xs y = xs ++ [head xs + y]
> rFolder y xs = xs ++ [y]
>
> seed = singleton 1
> fuk :: [Int] → Int → [Int]
> fuk tries ynew = tries ++ singleton ( last tries * (ynew - 1) )
> 
> lik ynew tries = tries ++ singleton ( last tries * (ynew - 1) )
>
> boo = foldl' fuk [3] [10,20,30,40,50]
> bobby = foldl' fuk [3] [10, 20..100]
> gibby = foldr lik [3] [10, 20..100]
>
> dataset                :: [(Node, [Node])]
> dataset                                  = [(0, [3,4]), (1, [4,5]), (2, [0]), (3, [0]), (4, []), (5, [])]
>
> executive              :: IO ()
> executive                                = do
>   let graph = makeGraph dataset
>   print graph
>   print (cyclicNodes graph)
>   return ()
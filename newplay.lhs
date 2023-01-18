Playground

> module Main where
>
> import System.Environment
>
> main :: IO ()
> main = do
>  args <- getArgs
>  let seed :: Int
>      seed = read (args !! 0)
>  putStrLn ("what? " ++ show seed)
>  -- testQ seed
>
> testQ :: Int -> IO ()
> testQ seed = do
>  putStrLn ("what? " ++ show seed)
>  -- testQ seed
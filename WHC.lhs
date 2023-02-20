> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module WHC  where
>
> import Control.Arrow
>
> ymain :: IO ()
> ymain = putStrLn "Hello, Haskell!"
>
> zmain :: IO ()
> zmain = do
>    let
>        prepend x = arr (x ++)
>        append  x = arr (++ x)
>        withId  t = returnA <+> t
>        xform = withId (prepend "<") >>>
>                withId (append ">") >>>
>                withId (prepend "!" >>> append "!")
>        xs = ["test", "foobar"]
>               >>= runKleisli xform
>    mapM_ putStrLn xs
>
> minimalDo :: Int → Double
> minimalDo i = do
>   1.0
>
>
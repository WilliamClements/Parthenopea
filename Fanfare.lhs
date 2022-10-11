Fanfare

> module Fanfare where
> import Euterpea
> import Debug.Trace

> tFan1 = {- 12 -} (c 4 dqn :+: rest en)
>                  :+: (e 4 dqn :+: rest en)
>                  :+: (g 4 dwn) :+: rest hn
> bFan1 = {- 12 -} rest wn :+: rest dhn
>                  :+: (c 3 dhn :+: c 3 qn)
>                  :+: (c 3 qn :+: rest 0)

> tFan2 = {-  6 -} (f 4 dqn :+: rest en)
>                  :+: (e 4 dqn :+: rest en)
>                  :+: (d 4 hn)
> bFan2 = {-  6 -} (rest wn :+: rest hn)

> tFan3 = {-  4 -} (bf 3 qn) :+: (bf 3 qn) :+: (bf 3 hn)
> bFan3 = {-  4 -} rest wn

> tFan4 = {- 10 -} c 4 wn :+: rest wn :+: rest hn
> bFan4 = {- 10 -} rest dhn
>                  :+: (c 3 dhn :+: c 3 qn)
>                  :+: c 3 hn

> tFan5 = {-  0 -} rest 0
> bFan5 = {-  0 -} rest 0

> tFan =  {- 32 -} tFan1 :+: tFan2 :+: tFan3 :+: tFan4 :+: tFan5
> bFan =  {- 32 -} bFan1 :+: bFan2 :+: bFan3 :+: bFan4 :+: bFan5

> tAns1 = {- 14 -} c 4 hn :+: g 4 hn :+: f 4 wn :+: rest dwn
> bAns1 = {- 14 -} rest wn :+: rest wn :+: bf 2 dhn :+: a 2 qn :+: g 2 hn

> tAns2 = {-  8 -} (bf 3 hn)
>                  :+: (bf 3 hn :+: bf 3 qn)
>                  :+: a 3 qn :+: g 3 hn
> bAns2 = {-  8 -} rest wn :+: rest wn

> tAns3 = {- 21 -} a 3 hn :+: bf 3 hn :+: c 4 wn
>                  :+: rest wn :+: rest wn :+: rest wn :+: rest qn
> bAns3 = {- 21 -} rest wn :+: rest wn :+: rest hn
>                  :+: g 2 qn :+: g 2 qn :+: c 3 qn :+: g 2 hn
>                  :+: c 3 qn :+: c 3 qn :+: c 2 wn

> tAns =  {- 35 -} tAns1 :+: tAns2 :+: tAns3
> bAns =  {- 40 -} bAns1 :+: bAns2 :+: bAns3

>         {- 34 + 40 = 74 -}
> trebleAll = instrument Trumpet (tFan :+: tAns)
>         {- 72 -}
> bassAll = instrument Cello (bFan :+: bAns)

> bothParts = tempo (2/1) (trebleAll :=: bassAll)

> dumpOverall :: Int -> IO ()
> dumpOverall nn =
>           traceM ("trebleAll = "
>           ++ show (dur trebleAll)
>           ++ ", bassAll = "
>           ++ show (dur bassAll))

> dumpT :: Int -> IO ()
> dumpT nn =
>           traceM ("tFan = "
>           ++ show (dur tFan)
>           ++ ", tAns = "
>           ++ show (dur tAns))

> dumpB :: Int -> IO ()
> dumpB nn =
>           traceM ("bFan = "
>           ++ show (dur bFan)
>           ++ ", bAns = "
>           ++ show (dur bAns))

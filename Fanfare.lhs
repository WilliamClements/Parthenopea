Fanfare

> module Fanfare where
> import Euterpea
> import Debug.Trace

Fanfare proper

> tFan1 = {- 11 -} (c 4 dqn :+: rest en)
>                  :+: (e 4 dqn :+: rest en)
>                  :+: (g 4 wn) :+: rest dhn
> bFan1 = {- 11 -} rest wn :+: rest hn
>                  :+: (c 3 dhn :+: c 3 qn :+: c 3 qn)
>                  :=: (g 3 dhn :+: g 3 qn :+: g 3 qn)

> tFan2 = {-  6 -} (f 4 hn)
>                  :+: (e 4 hn)
>                  :+: (d 4 dqn :+: rest en)
> bFan2 = {-  6 -} (rest wn :+: rest hn)

> tFan3 = {-  4 -} (bf 3 qn) :+: (bf 3 qn) :+: (bf 3 hn)
> bFan3 = {-  4 -} rest wn

> tFan4 = {-  7 -} c 4 wn :+: rest dhn
> bFan4 = {-  7 -} rest hn
>                  :+: (c 3 dhn :+: c 3 qn :+: c 3 qn)
>                  :=: (g 3 dhn :+: g 3 qn :+: g 3 qn)

> tFan5 = {-  0 -} rest 0
> bFan5 = {-  0 -} rest 0

> tFan =  {- 28 -} tFan1 :+: tFan2 :+: tFan3 :+: tFan4 :+: tFan5
> bFan =  {- 28 -} bFan1 :+: bFan2 :+: bFan3 :+: bFan4 :+: bFan5

The fanfare's answer

> tAns1 = {- 12 -} c 4 hn :+: g 4 hn :+: f 4 wn :+: rest hn :+: bf 3 qn :+: rest qn
> bAns1 = {- 12 -} rest wn :+: rest hn
>                  :+: (bf 2 dhn :+: a 2 qn :+: g 2 hn)
>                  :=: (d 3 dhn :+: c 2 qn :+: d 2 hn)

> tAns2 = {-  6 -} bf 3 hn
>                  :+: a 3 qn :+: a 3 qn :+: g 3 hn
> bAns2 = {-  6 -} rest wn :+: rest hn

> tAns3 = {- 14 -} a 3 hn :+: bf 3 hn :+: c 4 wn
>                  :+: rest wn :+: rest hn
> bAns3 = {- 14 -} rest wn :+: rest qn
>                  :+: g 2 qn :+: g 2 qn :+: c 3 qn :+: g 2 hn
>                  :+: c 3 qn :+: c 3 qn :+: c 2 hn

> tAns4 = {-  0 -} rest 0
> bAns4 = {-  0 -} rest 0

> tAns =  {- 32 -} tAns1 :+: tAns2 :+: tAns3 :+: tAns4
> bAns =  {- 32 -} bAns1 :+: bAns2 :+: bAns3 :+: bAns4 

>         {- 28 + 32 = 60 -}
> trebleAll = instrument Trumpet (tFan :+: tAns)
>         {- 60 -}
> bassAll = instrument Cello (bFan :+: bAns)

> bothParts = removeZeros $ tempo (2/1) $ shiftPitches 5
>             (trebleAll :=: bassAll)

> dumpTB, dumpT, dumpB, dumpAll :: Int -> IO ()
> dumpTB nn =
>           traceM ("trebleAll = "
>           ++ show (dur trebleAll)
>           ++ ", bassAll = "
>           ++ show (dur bassAll))
> dumpT nn =
>           traceM ("tFan = "
>           ++ show (dur tFan)
>           ++ ", tAns = "
>           ++ show (dur tAns))
> dumpB nn =
>           traceM ("bFan = "
>           ++ show (dur bFan)
>           ++ ", bAns = "
>           ++ show (dur bAns))
> dumpAll nn = do
>                 dumpTB nn
>                 dumpT nn
>                 dumpB nn

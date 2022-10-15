Fanfare

> module Fanfare where
> import Euterpea
> import Euterpea.IO.MIDI.ToMidi2
> import Debug.Trace

Fanfare proper

> tFan1 = {- 14 -} rest wn :+: (c 4 dqn :+: rest en)
>                  :+: (e 4 dqn :+: rest en)
>                  :+: (g 4 wn) :+: rest hn
> bFan1 = {- 15 -} rest wn :+: rest wn :+: rest hn
>                  :+: (c 3 dhn :+: c 3 qn :+: c 3 qn)
>                  :=: (g 3 dhn :+: g 3 qn :+: g 3 qn)

> tFan2 = {-  6 -} (f 4 hn)
>                  :+: (e 4 hn)
>                  :+: (d 4 dqn :+: rest en)
> bFan2 = {-  6 -} (rest wn :+: rest hn)

> tFan3 = {- 10 -} (bf 3 qn) :+: (bf 3 qn) :+: (bf 3 hn) :+: c 4 wn :+: rest hn
> bFan3 = {- 10 -} rest wn :+: rest qn
>                  :+: (c 3 dhn :+: c 3 qn :+: c 3 qn)
>                  :=: (g 3 dhn :+: g 3 qn :+: g 3 qn)

> tFan4 = {-  0 -} rest 0
> bFan4 = {-  0 -} rest 0

> tFan5 = {-  0 -} rest 0
> bFan5 = {-  1 -} rest qn

> tFan =  {- 30 -} tFan1 :+: tFan2 :+: tFan3 :+: tFan4 :+: tFan5
> bFan =  {- 32 -} bFan1 :+: bFan2 :+: bFan3 :+: bFan4 :+: bFan5

The fanfare's answer

> tAns1 = {- 12 -} c 4 hn :+: g 4 hn :+: f 4 wn :+: rest hn :+: bf 3 qn :+: rest qn
> bAns1 = {- 10 -} rest wn
>                  :+: (bf 2 dhn :+: a 2 qn :+: g 2 hn)
>                  :=: (d 3 dhn  :+: c 2 qn :+: d 2 hn)

> tAns2 = {-  6 -} bf 3 hn :+: a 3 qn :+: a 3 qn :+: g 3 hn
> bAns2 = {-  6 -} rest wn :+: rest hn

> tAns3 = {- 14 -} a 3 hn :+: bf 3 hn :+: c 4 wn :+: rest wn :+: rest hn
> bAns3 = {- 15 -} rest wn :+: rest hn
>                  :+: g 2 qn :+: g 2 qn :+: c 3 qn :+: g 2 hn
>                  :+: c 3 qn :+: c 3 qn :+: c 2 hn

> tAns4 = {-  2 -} rest hn
> bAns4 = {-  1 -} rest qn

> tAns =  {- 34 -} tAns1 :+: tAns2 :+: tAns3 :+: tAns4
> bAns =  {- 32 -} bAns1 :+: bAns2 :+: bAns3 :+: bAns4 

>         {- 30 + 34 = 64 -}
> trebleAll = instrument Trumpet (tFan :+: tAns)
>         {- 32 + 32 = 64 -}
> bassAll = instrument Cello (bFan :+: bAns)

> bothParts :: Music Pitch
> bothParts = removeZeros
>             $ tempo (2/1)
>             $ shiftPitches 5 (trebleAll :=: bassAll)

> capture :: Music Pitch -> IO()
> capture m = writeMidi2 "fanfare.mid" m

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

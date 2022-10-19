Fanfare

> module Fanfare where
> import Euterpea
> import Euterpea.IO.MIDI.ToMidi2
> import Debug.Trace

Fanfare proper

> tFan1 = {- 14 -} rest wn :+: (c 4 dqn :+: rest en)
>                  :+: (e 4 dqn :+: rest en)
>                  :+: (g 4 wn) :+: rest wn
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

New

> addDur       :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)

> frag00 = line [g 3 qn, rest en, a 3 en, bf 3 wn]
> frag01 = tempo (5/2) (addDur qn [bf 3, c 4, bf 3, a 3, g 3])
> frag02 = line [g 3 qn, f 3 hn, g 3 wn]

> line00 = frag00 :+: frag01 :+: frag02

> frag10 = line [g 3 en, c 4 dqn, c 4 en, c 4 dqn, c 4 en, c 3 hn, rest qn]
> frag11 = line [c 3 en, g 3 dqn, g 3 en, g 3 dqn, g 3 en, c 3 hn]

> line01 = frag10 :+: frag11

> piece = removeZeros
>         $ tempo (3/2)
>         $ instrument(Vibraphone)
>         $ transpose 10 
>         $ line00 :+: line01 :+: line00 :+: line01

Newer still

> treblebob =
>    addDur qn
>       [ a 3, fs 4
>       , a 3, fs 4
>       , a 3, fs 4 
>       , a 3, fs 4
>       , a 3,  g 4
>       , b 3,  g 4]
>    :+: tempo (3/2)
>       (addDur en [
>           g 4, fs 4, e 4, d 4, e 4, fs 4
>           , a 4, fs 4, e 4, d 4, e 4, g 4])

> altobob = line 
>    [ rest qn, d 4 qn
>    , rest qn, d 4 qn
>    , rest qn, d 4 qn
>    , rest qn, d 4 qn
>    , rest qn, d 4 qn
>    , rest qn, d 4 qn
>    , rest wn, rest wn]

> bassbob =
>    line
>       [ d 2 dwn, rest hn
>       , g 2 wn, e 2 hn, c 2 hn]
>       

> bob = tempo (2/2)
>       $ (instrument Flute (times 2 treblebob))
>       :=: (instrument Oboe (times 2 altobob))
>       :=: (instrument Cello (times 2 bassbob))
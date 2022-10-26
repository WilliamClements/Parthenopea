Fanfare

> module Fanfare where
> import Euterpea
> import Euterpea.IO.MIDI.ToMidi2
> import Debug.Trace
> import Control.Monad


Utilities =========================================================================

> addDur       :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)

> capture :: Music Pitch -> IO()
> capture m = writeMidi2 "fanfare.mid" m

> foldMusic :: [Music a] -> Music a
> foldMusic as = foldr (:+:) (rest 0) as

> slur :: Rational -> Music a -> Music a
> slur rate = Modify (Phrase [Art (Slurred rate)])

Fanfare proper ====================================================================

> tFan1 = {- 14 -} rest wn :+: (c 4 dqn :+: rest en)
>                          :+: (e 4 dqn :+: rest en)
>                          :+: (g 4 wn) :+: rest wn
> bFan1 = {- 15 -} rest wn :+: rest wn :+: rest hn
>                  :+: (c 3 dhn :+: c 3 qn :+: c 3 qn)
>                  :=: (g 3 dhn :+: g 3 qn :+: g 3 qn)

> tFan2 = {-  6 -} (f 4 hn)
>                  :+: (e 4 hn)
>                  :+: (d 4 dqn :+: rest en)
> bFan2 = {-  6 -} (rest wn :+: rest hn)

> tFan3 = {- 10 -} line [bf 3 qn, bf 3 qn, bf 3 hn, c 4 wn, rest hn]
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

> tAns1 = {- 12 -} line [c 4 hn, g 4 hn, f 4 wn, rest hn, bf 3 qn, rest qn]
> bAns1 = {- 10 -} rest wn
>                  :+: (line [bf 2 dhn, a 2 qn, g 2 hn])
>                  :=: (line [ d 3 dhn, c 2 qn, d 2 hn])

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

> theFanfare :: Music Pitch
> theFanfare = removeZeros
>              $ tempo (2/1)
>              $ keysig C Major
>              $ transpose 5
>              $ (trebleAll :=: bassAll)

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

Alice =============================================================================

> frag00 = line [g 3 qn, rest en, a 3 en, bf 3 wn]
> frag01 = tempo (4/2) $ line [a 3 qn, bf 3 qn, a 3 qn, af 3 qn]
> frag02 = line [g 3 qn, f 3 dhn,  rest qn, g 3 dhn, rest dqn]

> line00 = foldMusic [frag00, frag01, frag02]

> frag10 = line [g 3 en, c 4 dqn, c 4 en, c 4 dqn, c 4 en, c 3 hn, rest dqn]
> frag11 = line [c 3 en, g 3 dqn, g 3 en, g 3 dqn, g 3 en, c 3 dqn, rest en]

> line01 = foldMusic [frag10, frag11]

> alice = removeZeros
>         $ tempo (2/1)
>         $ transpose 10 
>         $ instrument(Vibraphone)
>         $ foldMusic [rest hn, line00, line01, line00, line01]

Bob ===============================================================================

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
>    , rest wn]

> bassbob =
>    line
>       [ d 2 dwn, rest hn
>       , g 2 wn, e 2 hn, c 2 hn]
>       

> bob = tempo (4/2)
>       $ (instrument Flute (times 2 treblebob))
>       :=: (instrument Oboe (times 2 altobob))
>       :=: (instrument Cello (times 2 bassbob))

Bill ==============================================================================

> bill =
>    tempo (2/1)
>    $ transpose 4
>    $ (instrument Flute     ( g 4 hn :+: (times 2 treblebill)))
>      :=: (instrument Oboe  (bf 3 hn :+: (times 2 altobill)))
>      :=: (instrument Cello (ef 3 hn :+: (times 2 bassbill)))

> treblebill00 =
>      tempo (3/2) (line [ef 4 qn, f 4 qn, g 4 qn])
>      :+: (af 4 hn)
>         :+: tempo (2/1) (line [df 4 qn, ef 4 qn, f 4 qn, g 4 qn])
>         :+: line [af 4 qn, rest en, g 4 en, f 4 hn]
>      :+: (ef 4 wn) :+: line [rest hn, ef 4 qn, f 4 qn]
>      :+: (gf 4 hn)
>         :+: tempo (2/1) (line [b 3 qn, cs 4 qn, ef 4 qn, f 4 qn])
>         :+: gf 4 qn :+: line [rest en, f 4 en, ef 4 hn]
>      :+: (df 4 dwn) :+: rest hn
> altobill00 =
>       rest hn
>       :+: (f 4 hn) :+: rest dwn
>       :+: (c 4 hn) :+: rest dwn
>       :+: (ef 3 hn) :+: rest dwn
>       :+: (gf 3 hn) :+: rest dwn
> bassbill00 =
>       rest hn
>       :+: (df 3 hn) :+: rest dwn
>       :+: (af 2 hn) :+: rest dwn
>       :+: ( b 2 hn) :+: rest dwn
>       :+: (bf 2 hn) :+: rest dwn

> treblebill01 = 
>      tempo (1/3) (df 4 wn)
>      :+: d 4 hn :+: ef 4 hn :+: f 4 hn
>      :+: tempo (3/2) (line [g 3 qn, bf 3 qn, ef 4 qn])
>      :+: line [f 4 dqn, g 4 en, ef 4 hn, ef 4 hn] 
> altobill01 =
>       rest wn :+: rest wn :+: (af 2 hn)
>       :+: rest wn :+: (b 2 hn) :+: rest dwn :+: rest wn
>
> bassbill01 =
>       tempo (1/3) (bf 2 wn) :+: rest wn :+: rest dwn :+: rest wn
>
>
> treblebill = foldMusic [treblebill00, treblebill01]
> altobill   = foldMusic [altobill00,     altobill01]
> bassbill   = foldMusic [bassbill00,     bassbill01]
>

Copper ============================================================================

> copper =
>    removeZeros
>    $ transpose (-4) 
>    $ tempo (2/1)
>    $ instrument Banjo
>    $ ((line [c 5 qn, rest qn, c 5 qn, rest qn, c 5 qn, rest qn, c 5 qn, rest qn])
>      :+: (tempo (5/4) 
>          (line [c 5 qn, g 4 qn, a 4 qn, bf 4 qn, rest qn]))
>          :+: a 4 hn :+: g 4 wn)

Gold ==============================================================================

> gold =
>    removeZeros
>    $ transpose (-5)
>     $ tempo (2/1)
>     $ instrument AltoSax
>     $ (times 2
>     $ (line [c 4 hn,  c 5 dhn]
>       :+: addDur qn [ c 5, bf 4,  a 4,  g 4,
>                       g 4,  f 4,  a 4,  g 4,
>                       g 4,  f 4,  a 4,  g 4,
>                       g 4,  c 4,  c 4,  c 4,
>                       c 5, bf 4,  a 4,  g 4,
>                       g 4,  f 4,  e 4,  d 4,
>                       d 4, bf 3, bf 3]))
>     :+: c 4 hn

Silver ============================================================================

> silver00 = 
>   addDur hn
>   $  [ a 4,  e 5, cs 5,  a 4, fs 4,
>        a 4,  b 4, fs 4,  e 4,
>        a 4,  b 4, fs 4,  e 4,
>       fs 4, gs 4,  a 4, cs 5 ]

> silver01 =
>   addDur hn
>   $  [ e 4, fs 4,  g 4,
>       fs 4,  a 4,  d 5,
>       cs 5,  b 4, gs 4,
>       ds 5,  e 5, fs 5,
>        e 5, ds 5,  e 5,
>       fs 4,  e 4, as 4,
>        b 4, fs 4, gs 4,
>        a 4, cs 5 ] 

> allSilver :: [Music Pitch]
> allSilver =
>    [ rest dwn
>      , silver00, a 4 hn
>      , silver00, a 4 hn
>      , silver01, a 4 hn
>      , silver01, a 4 wn ]

> silver =
>     transpose (-5)
>     $ instrument MusicBox
>     $ tempo (2/1)
>     $ foldMusic allSilver
>       :+: foldMusic allSilver
>

Tools =============================================================================

> snippet :: Music Pitch
> snippet = line [c 4 en, e 4 en, bf 3 en, d 4 en, f 4 en]
>
> playSnippet :: () -> Int -> IO ()
> playSnippet () i =
>    let inst :: InstrumentName
>        inst = toEnum (i `mod` (fromEnum Gunshot))
>    in do
>       traceM ("InstrumentName = " ++ (show inst)) ;
>       play $ instrument inst snippet
>
> playSnippets :: IO ()
> playSnippets = 
>    foldM playSnippet () [0..]

> jingles :: [Music Pitch]
> jingles =
>    [theFanfare
>     , alice
>     , bob
>     , bill
>     , copper
>     , gold
>     , silver]

>
> playJingle :: () -> Music Pitch -> IO ()
> playJingle _ m =
>    do
>       traceM ( (show (dur m)) ++ " jingle time" )
>       play m
>       
> playJingles :: IO ()
> playJingles =
>    foldM playJingle () (cycle jingles)

> {-# LANGUAGE UnicodeSyntax #-}

Opus131
William Clements
July 10, 2025

> module Tunes.Opus131 (opus131) where
>
> import Euterpea.Music
> import Parthenopea.Music.Passage
> import Parthenopea.Music.Siren
> import Parthenopea.SoundFont.Directives
  
Opus131 ===============================================================================================================

> o131Tempo              :: Dur
> o131Tempo                                = 1
> o131Transpose          :: AbsPitch
> o131Transpose                            = 0
>
> qv1, qv2, qva, qvc     :: BandPart
> qv1                                      = makePitched Violin               o131Transpose      0         90
> qv2                                      = makePitched Violin               o131Transpose      0         90
> qva                                      = makePitched Viola                o131Transpose      0         80
> qvc                                      = makePitched Cello                o131Transpose      0         80
>
> qv1Enabled, qv2Enabled, qvaEnabled, qvcEnabled
>                        :: Bool
> qv1Enabled = True
> qv2Enabled = True
> qvaEnabled = True
> qvcEnabled = True
>
> o131shape1             :: [Marking]
> o131shape1                               = [Mark PPP, Mark PP, Mark PP, Mark FFF, Mark PP, SpanN 6, Mark PP]
>
> opus131                 :: Directives → Music (Pitch, [NoteAttribute])
> opus131 dives                            =
>   removeZeros
>   $ tempo o131Tempo
>   $ chord [qv1Music, qv2Music, qvaMusic, qvcMusic]
>
>   where
>
>   qv1Music                               =
>     if qv1Enabled then keysig Cs Minor $ orchestraPart qv1 qv1Line else rest 0
>   qv2Music                               =
>     if qv2Enabled then keysig Cs Minor $ orchestraPart qv2 qv2Line else rest 0
>   qvaMusic                               =
>     if qvaEnabled then keysig Cs Minor $ orchestraPart qva qvaLine else rest 0
>   qvcMusic                               =
>     if qvcEnabled then keysig Cs Minor $ orchestraPart qvc qvcLine else rest 0
>   
>   qv1Line = line [ qv1p1000_004, qv1p1005_008, qv1p1009_012, qv1p1013_016, toMusic1 (line [qv1p1017_020, qv1p1021_024
>                  , qv1p1025_028, qv1p1029_032, qv1p1033_036, qv1p1037_040, qv1p1041_044, qv1p1045_048
>                  , qv1p1049_052, qv1p1053_056, qv1p1057_060, qv1p1061_064])]
>   qv2Line = line [ qv2p1000_004, qv2p1005_008, qv2p1009_012, qv2p1013_016, toMusic1 (line [qv2p1017_020, qv2p1021_024
>                  , qv2p1025_028, qv2p1029_032, qv2p1033_036, qv2p1037_040, qv2p1041_044, qv2p1045_048
>                  , qv2p1049_052, qv2p1053_056, qv2p1057_060, qv2p1061_064])]
>   qvaLine = line [ qvap1000_004, qvap1005_008, qvap1009_012, qvap1013_016, toMusic1 (line [qvap1017_020, qvap1021_024
>                  , qvap1025_028, qvap1029_032, qvap1033_036, qvap1037_040, qvap1041_044, qvap1045_048
>                  , qvap1049_052, qvap1053_056, qvap1057_060, qvap1061_064])]
>   qvcLine = line [ qvcp1000_004, qvcp1005_008, qvcp1009_012, qvcp1013_016, toMusic1 (line [qvcp1017_020, qvcp1021_024
>                  , qvcp1025_028, qvcp1029_032, qvcp1033_036, qvcp1037_040, qvcp1041_044, qvcp1045_048
>                  , qvcp1049_052, qvcp1053_056, qvcp1057_060, qvcp1061_064])]

    Violin 1 Pt 1 Measure 0 → 16 --------------------------------------------------------------------------------------
   
>   -- 21 % 4 
>   qv1p1000_004 =
>     line [rest dhn
>         , passage dives qv1 o131shape1 (line [gs 4 qn, bs 4 hn, cs 5 hn, a 4 dhn
>                                             , gs 4 qn, fs 4 qn, a 4 qn, gs 4 qn, fs 4 qn, e 4 qn, fs 4 qn, gs 4 dhn])]
>   -- 4 % 1
>   qv1p1005_008 =
>     line [passage dives qv1 [Mark P, SpanN 9, Mark P] (line [b 4 hn, a 4 hn, es 4 qn, fs 4 qn, a 4 qn
>                                                      , g 4 qn, es 4 hn, es 5 qn, fs 5 hn, e 5 qn, ds 5 hn])]
>   -- 4 % 1
>   qv1p1009_012 =
>     line [passage dives qv1 [Mark P, Mark P] (line [fs 5 qn, e 5 qn])
>         , passage dives qv1 [Mark P, SpanN 2, Mark FF] (line [e 5 hn, ds 5 hn, e 5 wn, d 5 hn])
>         , passage dives qv1 [Mark FF, SpanN 1, Mark P] (line [cs 5 qn, bs 4 qn, b 4 hn])]
>   -- 4 % 1
>   qv1p1013_016 =
>     line [passage dives qv1 [Mark P, SpanN 9, Mark P] (line [d 4 qn, cs 4 qn, as 4 hn, b 4 hn, cs 5 qn, d 5 qn, b 4 qn
>                                                      , cs 5 qn, d 5 qn, e 5 dhn, d 5 hn])]
>
>   -- rollup 0 + 17.25 -> 17.25

    Violin 2 Pt 1 Measure 0 → 16 --------------------------------------------------------------------------------------
   
>   -- 19 % 4
>   qv2p1000_004 = toMusic1 (line [rest (4 * wn), rest dhn]::Music Pitch)
>   -- 4 % 1
>   qv2p1005_008 =
>     line [passage dives qv2 o131shape1 (line [cs 4 qn, es 4 hn, fs 4 hn, d 4 dhn
>                                       , cs 4 qn, b 3 qn, d 4 qn, cs 4 qn, b 3 qn, a 3 qn, b 3 qn, cs 4 qn])]
>
>   -- 17 % 4
>   qv2p1009_012 =
>     line [passage dives qv2 [Mark P, SpanN 2, Mark P] (line [bs 3 qn, ds 4 qn, gs 3 hn, gs 4 qn])
>         , passage dives qv2 [Mark PP, SpanN 5, Mark FF] (line [bs 4 hn, cs 5 qn, b 4 qn, as 4 qn, cs 4 qn, b 4 qn, a 3 qn])
>         , passage dives qv2 [Mark FF, SpanN 2, Mark PP] (line [gs 3 qn, a 3 qn, ds 4 qn, es 4 qn])]
>   -- 4 % 1
>   qv2p1013_016 =
>     passage dives qv2 [Mark P, SpanN 11, Mark P]
>                  (line [d 4 qn, b 3 qn, as 3 qn, cs 4 qn, d 4 qn, b 3 hn, as 3 qn, b 3 qn, gs 3 qn, a 3 qn, b 3 qn
>                      , cs 4 dhn, b 3 qn])
>
>   -- rollup 0 + 17 -> 17

    Viola Pt 1 Measure 0 → 16 -----------------------------------------------------------------------------------------
   
>   -- 5 % 1
>   qvap1000_004 = toMusic1 (line [rest (5 * wn)]::Music Pitch)
>   -- 15 % 4
>   qvap1005_008 = toMusic1 (line [rest (3 * wn), rest dhn]::Music Pitch)
>   -- 18 % 4
>   qvap1009_012 =
>     passage dives qva o131shape1
>                 (line [gs 3 qn, bs 3 hn, cs 4 hn, a 3 dhn
>                      , gs 3 qn, fs 3 qn,  a 3 qn, gs 3 qn, fs 3 qn, e 3 qn, fs 3 qn, gs 3 dhn])
>   -- 4 % 1
>   qvap1013_016 =
>     passage dives qva [Mark P, SpanN 12, Mark P]
>                 (line [gs 4 qn, as 4 qn, fs 4 hn, g 3 qn, fs 3 qn, es 3 qn, d 3 qn, f 3 qn, e 3 qn
>                      , d 3 qn, cs 3 qn, d 3 qn, e 3 qn, fs 3 hn])
>
>   -- rollup 0 + 17.25 -> 17.25

    Cello Pt 1 Measure 0 → 16 -----------------------------------------------------------------------------------------
   
>   -- 5 % 1
>   qvcp1000_004 = toMusic1 (line [rest (5 * wn)]::Music Pitch)
>   -- 4 % 1
>   qvcp1005_008 = toMusic1 (line [rest (4 * wn)]::Music Pitch)
>   -- 15 % 4
>   qvcp1009_012 = line [rest (3 * wn), rest dhn]
>   -- 18 % 4
>   qvcp1013_016 =
>     passage dives qvc o131shape1
>                 (line [cs 3 qn, es 3 hn, fs 3 hn, d 3 dhn
>                      , cs 3 qn,  b 2 qn, d 3 qn, cs 3 qn, b 2 qn, a 2 qn, b 2 qn, cs 3 dhn])
>
>   -- rollup 0 + 17.25 -> 17.25

    Violin 1 Pt 1 Measure 17 → 32 -------------------------------------------------------------------------------------

>   -- 15 % 4
>   qv1p1017_020 = line [cs 5 qn, d 5 qn, e 5 qn, fs 5 dhn, fs 5 qn, a 5 qn, gs 5 qn, fs 5 qn, e 5 dhn
>                      , ds 5 qn, gs 5 qn]
>   -- 4 % 1 
>   qv1p1021_024 = line [bs 5 hn, cs 6 hn, b 5 qn, a 5 qn, rest qn, fs 5 qn, as 5 hn, b 5 hn
>                      , a 5 qn, gs 5 qn, rest hn]
>   -- 4 % 1 
>   qv1p1025_028 = line [rest dhn, b 5 qn, ds 6 hn, e 6 dqn, b 4 en, bs 4 hn, cs 5 qn, b 4 qn, as 4 qn, cs 5 qn
>                      , b 4 qn, a 4 qn]
>   -- 4 % 1
>   qv1p1029_032 = line [gs 4 qn, b 4 qn, a 4 qn, gs 4 qn, fs 4 qn, a 4 qn, gs 4 qn, fs 4 qn, e 4 qn, e 5 qn
>                      , fs 5 qn, as 5 qn, b 5 qn, d 5 qn, e 5 qn, gs 5 qn]
>
>   -- rollup 17.25 + 15.75 -> 33

    Violin 2 Pt 1 Measure 17 → 32 -------------------------------------------------------------------------------------

>   -- 4 % 1
>   qv2p1017_020 = line [as 3 hn, b 3 qn, cs 4 qn, d 4 qn, as 4 qn, b 4 qn, bs 4 qn, bs 4 qn, cs 5 qn, a 4 qn
>                      , as 4 qn, gs 4 qn, fss 4 qn, gs 4 qn, ds 5 qn]
>   -- 4 % 1
>   qv2p1021_024 = line [rest wn, rest qn, cs 5 hn, as 4 qn, fs 4 qn, fs 4 qn, d 4 qn, ds 4 qn, rest qn, b 4 qn
>                      , a 4 qn, gs 4 qn]
>   -- 17 % 4
>   qv2p1025_028 = line [fs 4 qn, gs 4 qn, fs 4 qn, e 4 qn, a 4 qn, gs 4 qn, c 4 qn, b 3 qn, e 4 dhn, es 4 qn
>                      , fs 4 qn, e 4 qn, ds 4 qn, b 3 hn]
>   -- 15 % 4
>   qv2p1029_032 = line [d 4 qn, cs 4 qn, e 4 qn, a 3 qn, cs 4 qn, bs 3 qn, gs 3 hn, b 4 qn, ds 5 qn, cs 5 qn
>                     , b 4 qn, rest hn, b 4 qn]
>
>   -- rollup 17 + 16 -> 33

    Viola Pt 1 Measure 17 → 32 ----------------------------------------------------------------------------------------
   
>   -- 15 % 4
>   qvap1017_020 = line [g 3 qn, fs 3 qn, e 3 qn, d 3 qn, e 3 qn, fs 3 qn, fs 4 hn, e 4 qn, ds 4 qn, cs 4 dhn, bs 3 hn]
>   -- 4 % 1
>   qvap1021_024 = line [rest qn, gs 3 hn, es 3 qn, cs 3 qn, cs 4 qn, a 3 qn, as 3 qn, rest qn, fs 3 hn, ds 3 qn
>                      , b 3 qn, gs 4 qn, fs 4 qn, e 4 qn]
>   -- 4 % 1
>   qvap1025_028 = line [rest qn, b 4 qn, a 4 qn, gs 4 qn, fs 4 qn, b 3 qn, a 3 qn, gs 3 hn, e 3 hn, cs 3 hn
>                      , fs 3 hn, fs 4 qn]  
>   -- 4 % 1
>   qvap1029_032 = line [e 4 qn, e 3 hn, e 3 qn, fs 3 qn, ds 3 hn, ds 4 qn, cs 4 qn, gs 4 qn, fs 4 hn, fs 4 qn
>                      , b 4 qn, gs 4 qn, fs 4 qn]
>
>   -- rollup 17.25 + 15.75 -> 33

    Cello Pt 1 Measure 17 → 32 ---------------------------------------------------------------------------------------
   
>   -- 4 % 1
>   qvcp1017_020 = line [e 3 qn, d 3 qn, cs 3 qn, b 2 qn, cs 3 qn, d 3 qn, ds 3 hn, e 3 qn, fs 3 qn, fss 3 qn
>                      , gs 3 qn, a 3 qn, gs 3 qn, fs 3 hn]
>   -- 15 % 4
>   qvcp1021_024 = line [es 3 qn, rest qn, cs 2 qn, es 2 hn, fs 2 hn, e 2 qn, ds 2 qn, rest qn, b 2 qn
>                      , d 3 hn, e 3 qn, b 3 qn]
>   -- 4 % 1
>   qvcp1025_028 = line [ds 3 hn, e 3 (hn + dhn), e 3 qn, gs 2 hn, a 2 qn, g 2 qn, fs 2 qn, as 2 qn, b 2 qn, d 3 qn]
>   -- 4 % 1
>   qvcp1029_032 = line [e 3 qn, gs 2 qn, a 2 qn, cs 3 qn, ds 3 qn, fs 2 qn, gs 2 qn, bs 3 qn, cs 3 qn, rest hn
>                      , e 4 qn, ds 4 qn, gs 4 qn, e 3 qn, ds 4 qn]
>
>   -- rollup 17.25 + 15.75 -> 33

    Violin 1 Pt 1 Measure 33 → 48 -------------------------------------------------------------------------------------

>   -- 4 % 1
>   qv1p1033_036 = line [as 5 qn, cs 5 qn, ds 5 qn, fss 5 qn, gs 5 dhn, fss 5 hn, cs 6 hn, b 5 hn, as 5 qn
>                      , cs 6 qn, b 5 qn]
>   -- 17 % 4
>   qv1p1037_040 = line [as 5 qn, cs 6 qn, b 5 qn, as 5 qn, b 5 qn, ds 6 qn, cs 6 qn, b 5 qn
>                      , as 5 hn, b 5 qn, cs 6 qn, b 5 hn, cs 6 qn, ds 6 hn]
>   -- 15 % 4
>   qv1p1041_044 = line [ds 5 (dhn + wn + dhn), ef 5 qn, d 5 qn, f 5 qn, ef 5 qn, d 5 qn]
>   -- 4 % 1
>   qv1p1045_048 = line [ef 5 qn, gf 5 qn, f 5 qn, ef 5 qn, rest qn, bf 5 qn, af 5 qn, g 5 qn, rest (wn + qn)
>                      , af 5 qn, gf 5 qn, f 5 qn]
>
>   -- rollup 33 + 16 -> 49

    Violin 2 Pt 1 Measure 33 → 48 -------------------------------------------------------------------------------------

>   -- 17 % 4
>   qv2p1033_036 = line [as 4 qn, fss 4 hn, as 4 qn, ds 4 qn, e 4 qn, b 4 qn, as 4 hn, as 3 qn, gs 3 qn
>                      , gs 4 qn, cs 4 qn, g 4 hn, fs 4 hn]
>   -- 15 % 4
>   qv2p1037_040 = line [as 4 qn, b 4 qn, chord [e 4 qn, cs 5 qn]
>                      , addDur qn [fs 4, b 4, as 4, b 4, e 5, cs 5, b 4, as 4, fs 5, ds 5, cs 5, b 4]]
>   -- 4 % 1
>   qv2p1041_044 = addDur qn [fss 4, as 4, gs 4, fss 4, b 4, ds 5, cs 5, b 4, fss 4, as 4, gs 4, gf 4
>                           , f 4, d 4, ef 4, f 4]
>   -- 4 % 1
>   qv2p1045_048 = line [bf 3 qn, ef 4 qn, f 4 qn, bf 4 qn, d 5 hn, ef 5 hn
>                      , chord [ef 4 qn, c 5 qn], rest hn, af 4 qn, c 5 hn, df 5 hn]
>
>   -- rollup 33 + 16 -> 49

    Viola Pt 1 Measure 33 → 48 ----------------------------------------------------------------------------------------
   
>   -- 4 % 1
>   qvap1033_036 = line [e 4 qn, rest dwn, ds 4 qn, fss 4 hn, gs 4 hn, e 4 dhn, ds 4 qn]
>   -- 4 % 1
>   qvap1037_040 = line [cs 4 qn, e 4 qn, ds 4 qn, cs 4 qn, ds 4 qn, fs 4 qn, e 4 qn, ds 4 qn, fs 3 (wn + qn)
>                      , b 3 qn, as 3 qn, b 3 qn]
>   -- 4 % 1
>   qvap1041_044 = line [addDur qn [as 3, cs 4, b 3, as 3, gs 3, b 3, as 3, gs 3, as 3, cs 4, b 3], bf 3 hn
>                      , af 3 qn, gf 3 qn, f 3 qn]
>   -- 4 % 1
>   qvap1045_048 = line [gf 3 qn, bf 3 qn, af 3 qn, gf 4 qn, f 4 hn, ef 4 qn, ef 3 qn, g 3 hn, af 3 hn, f 3 qn
>                      , rest hn, df 3 qn]
>
>   -- rollup 33 + 16 -> 49

    Cello Pt 1 Measure 33 → 48 ----------------------------------------------------------------------------------------
   
>   -- 23 % 4
>   qvcp1033_036 = line [addDur qn [cs 4, e 4, ds 4, cs 4, b 3, cs 4, ds 4], ds 3 dhn, e 3 hn, fs 3 (wn + wn + dhn)]
>   -- 9 % 4
>   qvcp1037_040 = line [addDur qn [fs 2, cs 3, e 3, ds 3, cs 3, ds 3, fs 3, e 3, ds 3]]
>   -- 4 % 1
>   qvcp1041_044 = line [addDur wn [ds 3, gs 2, ef 3, bf 2]]
>   -- 17 % 4
>   qvcp1045_048 = line [ef 3 hn, ef 2 hn, rest (wn + qn), ef 2 qn, df 2 qn, c 2 qn, rest qn, f 4 qn
>                      , ef 4 qn, df 4 hn]
>   
>   -- rollup 33 + 16.25 -> 49.25

    Violin 1 Pt 1 Measure 49 → 64 -------------------------------------------------------------------------------------

>   -- 17 % 4
>   qv1p1049_052 = line [rest qn, addDur qn [bf 5, af 5, gf 5, af 5, af 5, bf 4], bf 5 hn, bf 5 qn, c 5 qn, c 6 hn
>                      , c 6 qn, df 5 qn, df 6 hn]
>   -- 31 % 8
>   qv1p1053_056 = line [df 6 qn, e 5 qn, rest en, fs 5 en, as 5 qn, b 5 qn, gs 5 dqn, gs 5 qn, gs 5 en, fs 5 en
>                      , e 5 en, ds 5 en, e 5 en, fs 5 dqn, fs 5 en, e 5 en, ds 5 en, cs 5 en, ds 5 en, e 5 dqn]
>   -- 4 % 1
>   qv1p1057_060 = line [e 5 en, ds 5 en, cs 5 qn, fs 5 en, e 5 en, ds 5 qn, gs 5 en, fs 5 en, e 5 qn, a 5 en
>                      , gs 5 en, fs 5 qn, b 5 en, a 5 en, gs 5 qn, cs 6 en, b 5 en, a 5 qn, d 6 en, cs 6 en, b 5 qn
>                      , e 6 en, ds 6 en, cs 6 qn]
>   -- 33 % 8
>   qv1p1061_064 = line [fs 6 en, rest en, d 6 qn, cs 6 en, d 6 en, b 5 en, rest (wn + qn), e 6 qn, gs 6 hn
>                      , a 6 hn, fs 6 dhn]
>
>   -- rollup 49 + 16.25 -> 65.25

    Violin 2 Pt 1 Measure 49 → 64 -------------------------------------------------------------------------------------

>   -- 17 % 4
>   qv2p1049_052 = line [bf 4 qn, df 5 qn, cf 5 qn, bf 4 qn, af 4 qn, df 4 dqn, cf 4 en, bf 3 hn, ef 4 dqn, df 4 en
>                      , c 4 hn, f 4 dqn, ef 4 en, df 4 hn]
>   -- 15 % 4
>   qv2p1053_056 = line [gf 4 dqn, e 4 en, ds 4 hn, rest qn, addDur en [b 4, bs 4, bs 4, ds 5, gs 4, cs 4, ds 4, e 4
>                      , fs 4, e 4, b 3], fs 4 qn, gs 4 en, a 4 dqn, a 4 en, gs 4 en, fs 4 en]
>   -- 33 % 8
>   qv2p1057_060 = line [e 4 en, fs 4 en, gs 4 qn, fs 4 en, gs 4 en, a 4 qn, gs 4 en, a 4 en, b 4 qn, a 4 en, b 4 en
>                      , cs 5 dqn, b 4 en, cs 5 en, d 5 qn, cs 5 en, ds 5 en, e 5 qn, ds 5 en, as 4 en, gs 4 en
>                      , g 4 en, cs 5 en, b 4 en, as 4 qn]
>   -- 31 % 8
>   qv2p1061_064 = line [b 4 en, cs 5 en, d 5 en, e 4 en, a 4 qn, gs 4 en, cs 5 en, fs 5 en, rest en, d 5 qn
>                      , cs 5 en, d 5 en, b 4 qn, a 4 en, rest hn, d 5 qn, es 5 dqn, fs 5 en, d 5 qn, fs 5 qn]
>
>   -- rollup 49 + 16 -> 65

    Viola Pt 1 Measure 49 → 64 ----------------------------------------------------------------------------------------
   
>   -- 17 % 4
>   qvap1049_052 = line [f 3 hn, gf 3 hn, f 3 qn, df 3 hn, gf 3 en, g 3 dqn, ef 3 hn, af 3 en, a 3 dqn, f 3 hn
>                      , a 3 en, bf 3 dqn]
>   -- 31 % 8
>   qvap1053_056 = line [fs 3 hn, as 3 en, b 3 en, b 3 en, cs 4 en, cs 4 en, ds 4 en, ds 4 qn
>                      , addDur en [ds 3, bs 3, cs 4, e 4, ds 4, cs 4, b 3, cs 4], ds 4 dqn, ds 4 en, cs 4 en
>                      , b 3 en, a 3 en, b 3 en, cs 4 dqn] 
>   -- 4 % 1
>   qvap1057_060 = line [addDur en [cs 4, b 3, a 3, ds 4, ds 4, cs 4, b 3, e 4, e 4, ds 4, cs 4, fs 4, fs 4, e 4]
>                      , ds 4 qn, gs 4 en, fs 4 en, e 4 qn, e 4 en, d 4 en, cs 4 en
>                      , addDur en [g 3, fs 3, f 3, ds 4, e 4, e 3, fs 3], gs 3 qn]
>   -- 31 % 8
>   qvap1061_064 = line [addDur en [fs 3, gs 3, a 3, b 3, a 3, fs 3, gs 3, as 3, d 4, cs 4], b 3 qn, a 3 en, e 3 en
>                      , d 4 qn, cs 4 en, cs 5 dqn, cs 4 en, es 4 den, fs 4 sn, d 4 hn, fs 4 qn, d 5 qn]
>
>   -- rollup 49 + 16 -> 65

    Cello Pt 1 Measure 49 → 64 ----------------------------------------------------------------------------------------
   
>   -- 15 % 4
>   qvcp1049_052 = line [rest hn, df 2 qn, f 2 hn, gf 2 qn, ef 2 qn, g 2 hn, af 2 qn, f 2 qn, a 2 hn
>                      , bf 2 qn, ds 2 qn]
>   -- 4 % 1
>   qvcp1053_056 = line [as 2 hn, b 2 hn, gs 2 dhn, fs 2 qn, e 2 qn, gs 2 qn, fs 2 hn, ds 2 qn, fs 2 qn, e 2 hn]
>   -- 33 % 8
>   qvcp1057_060 = line [cs 2 qn, e 2 qn, ds 2 qn, fs 2 qn, e 2 qn, gs 2 qn, fs 2 qn, a 2 qn, gs 2 qn, b 2 qn
>                      , a 2 qn, cs 3 qn, b 2 qn, d 3 qn, cs 3 qn, e 3 dqn]
>   -- 41 % 8
>   qvcp1061_064 = line [d 3 en, e 3 qn, fs 3 qn, gs 3 qn, a 3 qn, d 3 en, e 3 qn, d 3 en, e 3 en, fs 3 en, e 3 qn
>                      , gs 4 qn, a 4 (wn + wn + dhn)]
>
>   -- rollup 49.25 + 17 -> 66.25
 
The End
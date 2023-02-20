> {-# LANGUAGE UnicodeSyntax #-}

Covers
William Clements
January 13, 2023

> module Covers where

> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import Parthenopea ( addDur, dim, triad )
  
Saucy Sailor ======================================================================

> ssailor =
>     removeZeros
>     $ tempo 1
>     $ transpose 0
>     $ keysig C Mixolydian
>     $ instrument Violin
>       ((if includeOpen then xOpenT        else rest 0)
>          :+: (if includeSong then xSongT  else rest 0)
>          :+: (if includeClos then xClosT  else rest 0))
>       :=: instrument AcousticGuitarSteel
>       ((if includeOpen then xOpenG        else rest 0)
>          :+: (if includeSong then xSongG  else rest 0)
>          :+: (if includeClos then xClosG  else rest 0))
>       :=: instrument ElectricBassPicked
>       ((if includeOpen then xOpenB        else rest 0)
>          :+: (if includeSong then xSongB  else rest 0)
>          :+: (if includeClos then xClosB  else rest 0))
>       where
>         includeOpen = False
>         includeSong = True
>         includeClos = False
>
>         xOpenT = rest 0
>         xSongT = line [rest hn, addVolume 80 xSongTA, addVolume 115 xSongTB]
>         xClosT = rest 0
>
>         xOpenG = rest 0
>         xSongG = line [rest dhn, addVolume 85 (times 7 xSongGrep)]
>         xClosG = rest 0
>
>         xOpenB = rest 0
>         xSongB = addVolume 100
>                  $ line [rest dhn
>                       , times 6 (line [xSongB1, xSongB2])
>                       , xSongB1
>                       , f 1 wn]
>         xClosB = rest 0
>
>         xSongTA = line [xSongT1, xSongT2, xSongT3, xSongT4, xSongT5, xSongT6]
>
>         xSongTB = line [xSongT7]
>
>         xSongGrep   =
>           line [times 4 xSongGC, xSongGFsus2, times 2 xSongGC, xSongGF, xSongGBf]
>         xSongGC     =
>           line [ g 3 sn,  c 4 sn, e 4 en,  g 4 en,  g 4 en, f 4 en, g 4 en]
>         xSongGFsus2 =
>           line [ g 3 sn,  c 4 sn, f 4 en,  g 4 en,  g 4 en, f 4 en, g 4 en]
>         xSongGF     =
>           line [ f 3 sn,  a 3 sn, c 4 en,  f 4 en,  c 4 en, a 3 en, c 4 en]
>         xSongGBf    =
>           line [ f 3 sn, bf 4 sn, d 4 en,  f 4 en,  d 4 en, bf 3 en, d 4 en]
>
>         xSongB1 = line [ times 4 (line [c 2 en, c 2 (hn+en)]),
>                          f 1 en, f 1 (hn+en), c 2 en, c 2 (hn+en), c 2 dhn]
>
>         xSongB2 = line [ a 1 qn, f 1 qn, a 1 qn, bf 1 qn, d 2 qn, bf 1 qn]
>    --  2  (see xSongT)
>         xSongT1 = line [
>    --  3        Come      me       ow -     -n      one
>           line [f 5 en,  g 5 en,  f 5 en, e 5 en,  c 5 qn],
>    --  3         Come      me      fai -    -ai-    -ai-   -air-    one
>           line [f 5 en,  g 5 en,  f 5 sn, g 5 sn, f 5 sn, e 5 sn, c 5 qn],
>    --  3         Co-      -ome      now    un- 
>           line [e 5 en,  f 5 en,  g 5 qn, c 6 qn],
>    --  3         to-      -o       me
>           line [bf 5 en, a 5 en, g 5 hn],
>    --  3         Could    you      fa-     -an-      -cy
>           line [c 6 den, bf 5 sn, af 5 en, g 5 en,  f 5 qn],
>    --  3          a        poor                        sail-    -or      lad
>           line [g 5 en, f 5 en, tempo (3/2) (line [e 5 qn, d 5 qn, c 5 qn])],
>    --  4         Who       has     just    come    from
>           line [e 5 en,  f 5 en,  g 5 qn, c 5 qn, bf 4 qn],
>    --  5  (27)       sea...
>           line [dim 1.0 (c 5 wn), rest qn]]
>
>         xSongT2 = line [
>    --  3         You      are       rag-   -ged      luv
>           line [ f 5 en,  g 5 en,  f 5 en, e 5 en,  c 5 qn],
>    --  3         And     you're      dir-     -ty      luv
>           line [ f 5 en,  g 5 en,  f 5 en, e 5 en,  c 5 qn],
>    --  3         And        your    clothes  smell
>           line [ e 5 den,  f 5 sn,  g 5 qn, c 6 qn],
>    --  3          much    of      tar
>           line [bf 5 en, a 5 en, g 5 hn],
>    --  3         So       be       go-      -one     you
>           line [c 6 den, bf 5 sn, af 5 en, g 5 en,  f 5 qn],
>    --  3         sauc-     -y      sail-    -or     lad
>           line [g 5 en,  f 5 en,  e 5 den, d 5 sn,  c 5 qn],
>    --  4         So         be      gone    you      Jack
>           line [e 5 den,  f 5 sn,  g 5 dqn, c 5 en, bf 4 qn],
>    --  4.5  (26.5)         Tar
>           line [dim 1.0 (c 5 wn), rest en]]
>
>         xSongT3 = line [
>    --  3.5          If       I        am     rag-    -ged      luv
>           line [f 5 en,  f 5 en,  g 5 en, f 5 den,  e 5 sn, c 5 qn],
>    --  3         And       I'm      dir-    ty       luv 
>           line [f 5 en,  g 5 en,  f 5 den, e 5 sn,  c 5 qn],
>    --  3         And        me    clothes  smell     
>           line [e 5 den,  f 5 sn,  g 5 qn, c 6 qn],
>    --  3          much     of      tar
>           line [bf 5 en, a 5 en, g 5 hn],
>    --  3           I      have      si-      -il-    -il-    -il-   -ver 
>           line [c 6 en, bf 5 en, af 5 sn, bf 5 sn, af 5 sn, g 5 sn, f 5 qn],
>    --  3        in         me      pock-   -et      luv
>           line [g 5 den,  f 5 sn,  e 5 en, d 5 en,  c 5 qn],
>    --  4         And     gold     in    great
>           line [e 5 qn, g 5 qn, c 5 qn, bf 4 qn],
>    --  5    (27.5)       store...
>           line [dim 1.0 (c 5 wn), rest qn]]
>
>         xSongT4 = line [
>    --  3          And      then    whe-      -n    she 
>           line [f 5 en,  g 5 en, f 5 en,  e 5 en, c 5 qn],
>    --  3         heard     him     say-   -ay-    -ay-     -ay      so 
>           line [f 5 en,  g 5 en, f 5 sn, g 5 sn, f 5 sn, e 5 sn,  c 5 qn],
>    --  3         On        her      bend-      -ed     
>           line [e 5 den,  f 5 sn,  g 5 dqn, c 6 en],
>    --  3          knee    she     fell
>           line [bf 5 den, a 5 sn, g 5 hn],
>    --  3           I      will      ma-      -a-     -a-     -ry 
>           line [c 6 den, bf 5 sn, af 5 en, g 5 en,  f 5 en, f 5 en],
>    --  3         my       dear     he-     -en-     -ry
>           line [g 5 en,  f 5 en,  e 5 en, f 5 sn, d 5 sn,  c 5 qn],
>    --  3         For      I       love     a      sail-     -or
>           line [e 5 en, f 5 en, g 5 den, c 5 sn, c 5 den, bf 4 sn],
>    --  5 (26)      lad      s'    well...
>           line [bf 4 den, d 5 sn, dim 1.0 (c 5 wn)]]
>
>         xSongT5 = line [
>    --  3                            Do-      -o      you      think    that  
>           line [tempo (3/2) (line [f 5 qn,  g 5 qn, f 5 qn]), f 5 den, e 5 sn],
>    --  4          I-        -I     am       fool-     ish     luv 
>           line [c 5 qn, f 5 en,  g 5 en,  f 5 en, e 5 en,  c 5 qn],
>    --  3          Do         you     think   that
>           line [e 5 den,  f 5 sn,  g 5 qn, c 6 qn],
>    --  3         I        am       mad?
>           line [bf 5 en, a 5 en, g 5 hn],
>    --  3          For       to       wed      with     
>           line [c 6 den, bf 5 sn, af 5 en, g 5 en,  f 5 qn],
>    --  3          a        poor     coun-    -try    girl
>           line [g 5 en,  f 5 en,  e 5 den, d 5 sn,  c 5 qn],
>    --  3         Where      no      for-   -tune's
>           line [e 5 en,  f 5 en,  g 5 qn, c 5 qn],
>    --  6 (28)        to       be      had...
>           line [bf 4 en, d 5 en, dim 1.0 (c 5 wn), rest qn]]
>
>         xSongT6 = line [
>    --  3           I       will      cross           the
>           line [f 5 en,  g 5 en, f 5 (den + qn),  c 5 sn],
>    --  3         bri-      -ny      o-     -o-     -o-     -o-     -cean
>           line [f 5 en,  g 5 en,  f 5 sn, g 5 sn, f 5 sn, e 5 sn,  c 5 qn],
>    --  3           I       will    whis-   -tle
>           line [e 5 en,  f 5 en,  g 5 qn, c 6 qn],
>    --  3           a-      -and    sing
>           line [bf 5 en, a 5 en, g 5 hn],
>    --  3          And    since     you-    -ou      have      re- 
>           line [c 6 en, bf 5 en, af 5 en, g 5 en,  f 5 den, f 5 sn],
>    --  3         -fused     the    of-      fer    luv
>           line [ g 5 en, f 5 en,  e 5 en, d 5 en,  c 5 qn],
>    --  3.75        So-     ome      o-     -ther   girl     shall   wear     
>           line [e 5 en, f 5 en, g 5 den, c 5 sn, c 5 den, bf 4 sn, bf 4 den],
>    --  4.25 (26)  th'    ring...
>           line [d 5 sn, dim 1.0 (c 5 wn)]]
>
>         xSongT7 = line [
>    --  3.75                           Oh        I      am
>           line [tempo (3/2) (line [f 5 qn,  g 5 qn, f 5 qn])
>    --             fro-     -o-      -o-   -lick-   -some
>                ,  f 5 sn, g 5 sn, f 5 sn, e 5 sn, c 5 den],
>    --  3.25      and      I        am       ea-     -ea-    -sy
>           line [f 5 sn,  f 5 en, g 5 en,  f 5 en, e 5 en,  c 5 qn],
>    --  3         goo-      -ood     temp-  -ered
>           line [e 5 en,  f 5 en,  g 5 qn, c 6 qn],
>    --  3           a-      -a        a-     -and   free
>           line [bf 5 sn, c 6 sn, bf 5 sn, a 5 sn, g 5 hn],
>    --  3          And      I         do-     -on't   give     a
>           line [c 6 en, bf 5 en, af 5 en, g 5 en,  f 5 den, f 5 sn],
>    --  3           sing-    -le      pin    me       boys
>           line [g 5 en,  f 5 en,  e 5 den, d 5 sn,  c 5 qn],
>    --  4          what      the      world   thinks   o-      -of
>           line [e 5 den,  f 5 sn,  g 5 qn, c 5 qn, bf 4 en, d 5 en],
>    --  5  (28)             me...
>           line [dim 0.5 (c 5 wn), rest qn]]
>    --  2 + 27 + 26.5 + 27.5 + 26 + 28 + 26 + 28  = 2 + 189 = 191
>    --                                         7 * 27 = 189

Yahozna ===========================================================================

> yahozna =
>    removeZeros
>    $ tempo 4
>    $ transpose 0
>    $ keysig C Mixolydian
>    $ addVolume 75 (line [rest dwn, rest dwn])
>      :+: (addVolume 75 yahoznaGuitar
>           :=: (addVolume 75 yahoznaBassIntro :+: addVolume 75 yahoznaBass)
>           :=: addVolume 110 choirPart)

> guitarLick 
>   =    line [ c 3 hn,  c 3 qn,  c 3 qn,  c 3 wn]
>    :+: line [ c 3 qn,  c 3 qn,  c 3 qn,  c 3 qn]

> bassIntroLick1
>   =    line [rest dwn, rest hn, ef 2 qn, c 2 qn, ef 2 qn, c 2 qn]

> bassIntroLick2
>   = line [rest dwn, rest hn]
>     :+: addDur sn [ c 3,  b 2, bf 2,  a 2, af 2,  g 2, gf 2,  f 2
>                   , e 2, ef 2,  d 2, df 2,  c 2,  b 1, bf 1,  a 1]

> bassLick
>   = line [c 2 dhn, c 2 hn, c 2 hn, c 2 qn, bf 1 hn, f 2 hn]

> yahoznaGuitar = 
>    instrument DistortionGuitar
>    $ times 12 (guitarLick :=: transpose 7 guitarLick)

> yahoznaBassIntro = 
>    instrument ElectricBassPicked
>    $ times 3 bassIntroLick1 :+: bassIntroLick2

> yahoznaBass = 
>    instrument ElectricBassPicked
>    $ times 8 bassLick

> choirPart
>   = instrument AltoSax
>     $ times 6 (line [rest dwn, rest dwn])
>     :+: line [g 3 (wn + wn), a 3 wn, a 3 dwn, a 3 hn, bf 3 wn, bf 3 wn]
>     :+: line [rest wn, bf 3 hn,  c 4 hn,  a 3 dwn]

Slot =======================================================================

> slot :: Int → Music (Pitch, Volume)
> slot n =
>    removeZeros
>    $ tempo 2
>    $ transpose 0
>    $ keysig G Dorian
>    $ chord [ addVolume 100 $ instrument Violin              (vSlotV n)
>            , addVolume 100 $ instrument OrchestralHarp      (vSlotG n)
>            , addVolume 100 $ instrument Cello               (vSlotC n)
>            , addVolume 100                                  (vSlotP n)]
>      
> vSlotV :: Int → Music Pitch
> vSlotV n
>   = line [ vSlotV01, times n (line [vSlotV02, vSlotV03])]
> vSlotV01
>   = rest dhn
> vSlotV02
>   = times 4 (rest dhn)
> vSlotV03
>   = line [ f 4 en,  g 4 en,  a 4 en,  b 4 en,  c 5 en,  d 5 en
>         ,  c 5 qn,          bf 4 en,  a 4 en, bf 4 en,  f 4 en
>         ,  f 4 en,  c 4 en,  c 4 en,  e 4 en,  f 4 en,  e 4 en
>         ,  f 4 en,  g 4 en,  f 4 qn, rest qn]
>
> vSlotG :: Int → Music Pitch
> vSlotG n
>   = line [ vSlotG01, times n vSlotG02]
> vSlotG01
>   = rest dhn
> vSlotG02
>   = line [ times 4
>            $ triad C (CustomMode "Sus2") (G, 3) dhn
>          , times 4
>            $ triad F Major               (F, 3) dhn]
>
> vSlotC :: Int → Music Pitch
> vSlotC n
>   = times n (line [vSlotC01, vSlotC02, vSlotC03])
>
> vSlotC01
>   = line [ rest hn, rest en, c 3 en ]
>
> vSlotC02
>   = line [ c 2 qn,  c 2 qn,  c 2 qn
>          , d 2 qn,  d 2 qn,  d 2 qn
>          ,ef 2 qn, ef 2 qn, ef 2 qn
>          , e 2 qn,  e 2 qn,  e 2 qn]
> vSlotC03
>   = line [f 2 hn, rest qn, times 2 (rest dhn)]
>
> vSlotP n
>   = times n (line [vSlotP01, vSlotP02, vSlotP03])
>
> vSlotP01
>   = line [ rest hn, rest en, perc LowTom en ]
>
> vSlotP02
>   = times 8 (line [ perc Maracas en, perc Maracas en, perc HighAgogo en ])
>
> vSlotP03
>   = line [perc ClosedHiHat hn, rest qn, times 2 (perc OpenHiHat dhn)]
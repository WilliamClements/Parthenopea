> {-# LANGUAGE UnicodeSyntax #-}

Covers
William Clements
January 13, 2023

> module Covers where

> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import Parthenopea ( addDur, ascent, descent, dim, grace, triad, slur )
> import Codec.Midi (Message(PitchWheel))
  
Saucy Sailor ==============================================================================

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
>         xSongT = line [rest hn, addVolume 90 xSongTA, addVolume 115 xSongTB]
>         xClosT = rest 0
>
>         xOpenG = rest 0
>         xSongG = line [rest dhn, addVolume 60 (times 7 xSongGrep)]
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
>           line [times 2 (xSongGC :+: xSongGC')
>                 , xSongGFsus2, xSongGC :+: xSongGC', xSongGF, xSongGBf]
>         xSongGC     =
>           line [ g 3 sn,  c 4 sn, e 4 en,  g 4 en,  g 4 en, f 4 en, g 4 en]
>         xSongGC'    =
>           line [ g 3 sn,  c 4 sn, e 4 en,  g 4 en,  g 4 en, e 4 en, g 4 en]
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

Yahozna ===================================================================================

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

Slot ======================================================================================

> slot :: Int → Music (Pitch, Volume)
> slot n =
>    removeZeros
>    $ tempo 2
>    $ transpose 0
>    $ keysig G Dorian
>    $ chord [ addVolume 110 $ instrument Violin              (vSlotV n)
>            , addVolume  80 $ instrument OrchestralHarp      (vSlotG n)
>            , addVolume  80 $ instrument Cello               (vSlotC n)
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
>          , times 2
>            $ triad F Major               (F, 3) (2*dhn)]
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

TC ========================================================================================

> licks = 38
>
> basicLick :: Music (Pitch, Volume)
> basicLick =
>   removeZeros
>   $ tempo 1
>   $ transpose (-12)
>   $ keysig A Major
>   $ chord [ addVolume 105 $ instrument ChorusedPiano       tcV
>           , addVolume  50 $ instrument AcousticGuitarNylon tcG
>           , addVolume  60 $ instrument Cello               tcC
>           , addVolume 100                                  tcP]
>   where
>
>   gUnit :: Music Pitch
>   gUnit = addDur qn [f 5, a 5, b 5, a 5, f 5, a 5, b 5, a 5
>                    , e 5, a 5, b 5, a 5, e 5, a 5, b 5, a 5]
>   tcG = line [times (licks - 1) gUnit, dim 1 gUnit]
>
>   cUnit :: Music Pitch
>   cUnit = addDur (2 * wn) [d 4, a 3]
>   tcC = line [rest (16 * qn), times (licks - 3) cUnit, dim 1 (times 2 cUnit)]
>
>   tcV = line [rest (28 * qn), tcV01, tcV02, tcV03, tcV04, tcV05, tcV06, tcV07, tcV08
>                             , tcV09]
>
>   tcV01 = line [tcV01A, tcV01B, tcV01C, tcV01D]
>
>   tcV01A = tempo (3/4) (line [rest qn, cs 6 qn,  d 6 en, e 6 en])
>
>   tcV01B = -- measure 5
>     line [  
>   {-  5   -}   tempo (3/2)   (line [cs 6 den, d 6 sn]) 
>              , cs 6 dhn  -- ((2*en/3) + qn*2 + (hn/3))
>              , tempo (3/2)   (line [b 5 en, rest qn])
>   {-  2   -} , b 5 (1/12 + hn)
>   {-  1   -} , tempo (3/2)   (line [ grace 2 (a 5 qn), a 5 sn, b 5 sn])]
>
>   tcVnt58 =
>     line [     tempo (3/2)   (line [rest en, cs 6 sn,  d 6 sn,  e 6 en])
>              , tempo (5/4)   (line [ e 6 en,  e 6 sn,  d 6 sn, cs 6 sn])
>              , tempo (5/4)   (line [cs 6 sn,  d 6 sn, cs 6 en,  b 5 sn])
>              , cs 6 den, b 5 sn
>              , a 5 qn]
>
>   tcV01C = -- measures 6, 7
>     line [
>   {-  1   -}   tempo (3/2)   (line [ cs 6 qn])
>   {-  4   -} , cs 6  (1/12 + qn), descent (Cs, 6) (qn*3)
>   {-  4   -} , tempo (5/4) tcVnt58
>   {-  1   -} , grace (-2) (b 5 den), a 5 sn
>   {-  1   -} , tempo (3/2)   (line [a 5 en, g 5 qn])
>   {-  1   -} , g 5   (qn + 1/12)
>   {-  2   -} , tempo (3/2)   (line [g 5 en, a 5 en, g 5 en, g 5 qn])
>   {-  1   -} , tempo (3/2)   (line [tempo (3/4) (line [b 5 sn, a 5 tn]), g 5 qn])
>   {-  0.5 -} , g 5 sn, a 5 sn
>   {-  0.5 -} , a 5 tn, g 5 tn, a 5 tn, g 5 tn]
>
>   tcV01D = -- measure 8
>     line [
>   {-  1   -}   tempo (3/2)   (line [f 5 sn, e 5 sn, e 5 qn])
>   {-  7   -} , e 5 wn, ascent (F,5) dhn]
>
>   tcV02 = line [tcV02A, tcV02B, tcV02C, tcV02D, tcV02E, tcV02F, tcV02G]
>
>   tcV02A = -- measure 9-10
>     line [
>   {-  1.7 -}   rest qn, tempo (3/2) (line [cs 6 en, cs 6 en])
>   {-  2.3 -} , d 6 ((1/12) + qn), d 6 en, cs 6 en
>   {-  1   -} , tempo (3/2)   (line [cs 6 qn, d 6 sn, cs 6 sn])
>   {-  3.5 -} , rest qn, rest dqn, cs 6 qn
>   {-  0.5 -} , tempo (3/2)   (line [d 6 sn, e 6 sn, fs 6 sn])
>   {-  5   -} , e 6 dhn, rest hn
>   {-  2   -} , tempo (3/2)   (line [rest qn,  f 6 qn, cs 6 qn])]
>
>   tcV02B = rest 0
>
>   tcV02C = -- measure 11
>     line [
>   {-  2   -}   tempo (3/2)   (line [ e 6 en,  e 6 sn, d 6 sn, cs 6 en
>                                   , cs 6 sn, cs 6 sn, cs 6 sn
>                                   , b 5 sn, cs 6 sn,  b 5 sn])
>   {-  2   -} , a 5 en, b 5 en, b 5 en, a 5 en
>   {-  2   -} , tempo (3/2)   (line [cs 6 en, cs 6 sn, b 5 sn, a 5 en, a 5 qn, a 5 en])
>   {-  2   -} , tempo (3/2)   (line [ g 5 qn, a 5 en, g 5 qn, f 5 en])]
>
>   tcVnt59 =
>     line [
>   {-  1   -}    tempo (3/4)  (line [a 5 en, tempo (3/2) (line [a 5 en, b 5 sn]), a 5 en])
>   {-  1   -}  , tempo (3/4)  (line [a 5 en, a 5 en, b 5 en])
>   {-  1   -}  , tempo (3/4)  (line [cs 6 en, cs 6 sn, b 5 sn, a 5 en])]
>
>   tcV02D = -- measure 12-13
>     line [
>   {-  1   -}   tempo (3/2)   (line [ f 5 qn, e 5 en])
>   {-  2   -} , e 5 sn, f 5 sn, e 5 dqn
>   {-  1   -} , tempo (3/2)   (line [rest qn, g 4 en])
>   {-  1   -} , g 4 qn
>   {-  2   -} , tempo (3/2)   (line [f 4 en, f 4 en, g 4 en
>                                   , a 4 en, a 4 en, b 4 en])
>   {-  3   -} , tempo (11/12) (line [cs 5 den, cs 5 den, cs 5 den, ds  5 en])
>   {-  2   -} , tempo (3/2)   (line [f 5 en, f 5 en,  g 5 en
>                                   , a 5 en, b 5 en, cs 6 en])
>   {-  2   -} , tempo (3/2)   (line [cs 6 en, cs 6 sn, b 5 sn, a 5 en
>                                   ,  a 5 sn,  b 5 sn, a 5 sn, g 5 sn, a 5 en])
>   {-  2   -} , tempo 3       tcVnt59]
>
>   tcV02E = -- measure 14
>     line [
>   {-  2   -}   tempo (3/2)   (line [a 5 en, e 6 qn, e 6 sn, d 6 en, e 6 en, g 6 sn])
>   {-  4   -} , fs 6 en, e 6 en, rest dhn
>   {-  1   -} , tempo (3/2)   (line [rest en, b 5 sn, cs 6 den])
>   {-  1   -} , cs 6 en, e 6 sn, cs 6 sn]
>
>   tcVnt60  =
>     line [
>   {-  1   -}   f 5 en, e 5 en
>   {-  0.5 -} , tempo (3/2)   (line [e 5 en, f 5 sn])
>   {-  1   -} , e 5 sn, d 5 sn, e 5 en]
>
>   tcV02F = -- measure 15
>     line [
>   {-  1   -}   e 6 en, cs 6 en
>   {-  1   -} , tempo (3/2)   (line [cs 6 qn, b 5 sn, a 5 sn])
>   {-  1   -} , tempo (3/2)   (line [a 5 en, b 5 en, a 5 tn, b 5 tn, a 5 tn, b 5 tn])
>   {-  1   -} , a 5 sn, a 5 tn, g 5 tn, f 5 en
>   {-  2   -} , f 5 qn, e 5 qn
>   {-  2   -} , tempo (5/4)   tcVnt60]
>
>   tcV02G = -- measure 16
>     line [
>   {-  1   -}   d 5 en, e 5 en, cs 5 en
>   {-  1   -} , tempo (3/2)   (line [cs 5 en, b 4 sn])
>   {-  4   -} , cs 5 wn
>   {-  2   -} , cs 5 qn, descent (Cs,6) qn]
>
>   tcV03 = line [tcV03A, tcV03B, tcV03C, tcV03D, tcV03E, tcV03F, tcV03G, tcV03H]
>
>   tcV03A = -- measure 17
>     line [
>   {-  3   -}   rest hn, g 5 sn, g 5 sn, f 5 en
>   {-  1   -} , f 5 sn, e 5 sn, e 5 tn, f 5 tn, e 5 tn, d 5 tn 
>   {-  1   -} , e 5 en, tempo (3/2) (line [e 5 en, d 5 sn])
>   {-  1   -} , cs 5 en, cs 5 sn, d 5 sn
>   {-  1   -} , rest en, g 5 sn, g 5 sn
>   {-  1   -} , f 5 en, f 5 tn, g 5 tn, f 5 tn, e 5 tn]
>
>   tcVnt01 = 
>     line [
>   {-  3   -}   tempo (3/2)   (line [f 5 en, e 5 en, d 5 en])
>              , e 5 en, e 5 en, d 5 en, cs 5 en]
>
>   tcVnt02 = 
>     line [
>   {-  3.5 -}   cs 5 en, d 5 en, cs 5 en, d 5 en, e 5 en, e 5 en, e 5 en]
>
>   tcVnt03 = 
>     line [
>   {-  2   -}   e 5 qn, e 5 qn
>   {-  0.4 -} , tempo (5/6)   (line [e 5 en, f 5 sn, e 5 sn, d 5 sn])]
>
>   tcV03B = -- measure 18
>     line [
>   {-  2   -}   tempo (3/2)   tcVnt01
>   {-  2   -} , tempo (7/4)   tcVnt02
>   {-  2   -} , tempo (7/4)   tcVnt03
>   {-  2   -} , ascent (E,5) hn
>        ]
>   tcV03C = -- measure 19
>     line [
>   {-  2.5 -}   rest qn, cs 6 sn,  d 6 sn, e 6 sn, cs 6 den
>   {-  1.5 -}          , cs 6 en, tempo (3/2) (line [b 5 qn,  b 5 en])
>   {-  2   -}          ,  a 5 qn,  a 5 en, cs 6 sn,  d 6 sn
>   {-  1   -}          , tempo (3/2) (line [e 6 en, d 6 qn])
>   {-  1   -}          ,  cs 6 en, cs 6 sn, d 6 sn]
>
>   tcVnt04 =
>     line [
>   {-  3   -}   rest qn, cs 5 qn, d 5 en, e 5 en
>   {-  4   -}          , cs 5 qn, cs 5 qn, cs 5 en, b 4 en, b 4 en, b 4 en]
>
>   tcVnt05 =
>     line [
>   {-  3   -}   cs 5 en, b 4 en, cs 5 en, d 5 en, cs 5 qn         
>   {-  1   -} , tempo (3/2)   (line [grace (-4) (cs 5 en), b 4 en, a 4 en])
>   {-  3   -} , b 4 en, b 4 en, b 4 qn, rest en, cs 5 en]
>
>   tcVnt06 =
>     line [
>   {-  1   -}   tempo (5/4)   (line [b 4 en, a 4 sn, a 4 sn, a 4 sn])
>   {-  1   -} , tempo (3/2)   (line [a 4 en, g 4 en, g 4 en])]
>
>   tcVnt07 =
>     line   [
>   {-  1   -}   grace (-2)    (b 4 en), a 4 en
>   {-  1.5 -} , tempo (5/6)   (line [c 5 en, cs 5 den])
>   {-  2   -} , a 4 en, b 4 en, a 4 sn, b 4 sn, a 4 sn, g 4 sn]
>
>   tcV03D = -- measure 20-22
>     line [
>   {-  2   -}   cs 6 den, cs 6 sn, cs 6 qn
>   {-  4   -} , tempo (7/4)   tcVnt04
>   {-  4   -} , tempo (7/4)   tcVnt05
>   {-  1.5 -} , tempo (4/3)   tcVnt06
>   {-  1   -} , rest qn -- WOX
>   {-  1.5 -} , tempo 3       (line [g 4 en,  f 4 en, f 4 en, f 4 en, e 4 en
>                               , e 4 en,  e 4 en, f 4 en, e 4 en])
>   {-  2.5 -} , tempo (3/5)   (line [e 4 en, cs 4 en, d 4 en])
>   {-  0.5 -} , e 4 sn, f 4 sn
>   {-  4   -} , e 4 wn
>   {-  3   -} , tempo (3/2)   tcVnt07]
>
>   tcV03E = -- measure 23
>     line [
>   {-  2   -}   tempo (3/2)   (line [ a 4 qn, g 4 en, f 4 en, e 4 qn])
>   {-  1   -} , tempo (7/4)   (line [ f 4 sn, e 4 sn, d 4 sn, f 4 sn, e 4 sn, d 4 en])
>   {-  0.5 -} , e 4 en
>   {-  0.5 -} , tempo (3/2)   (line [ e 4 en, d 4 sn])
>   {-  2   -} , tempo (3/2)   (line [cs 4 qn, b 3 hn])
>   {-  2   -} , b 3 en, b 3 en, grace (-2) (cs 4 en), b 3 en]
>
>   tcV03F = rest 0 -- measure 24
>
>   tcV03G = rest 0
>   tcV03H = rest 0
>
>   tcV04 = line [tcV04A, tcV04B, tcV04C, tcV04D, tcV04E, tcV04F, tcV04G, tcV04H]
>
>   tcV04A = -- measure 24-25
>     line [
>   {-  4   -}   b 3 en, a 3 en, a 3 dhn
>   {-  2   -} , tempo (3/2)   (line [f 3 qn, f 3 en, f 3 en, f 3 en, g 3 en])
>   {-  0.7 -} , tempo (3/2)   (line [f 3 en, g 3 qn, f 3 en])
>   {-  1.3 -} , f 3 ((1/6)+qn), tempo (3/2) (line [f 3 qn, f 3 en])
>   {-  2   -} , tempo (7/4)   (line [ a 3 en, a 3 en, a 3 en
>                                    , a 3 en, b 3 en, b 3 en, a 3 en])
>   {-  4   -} , tempo (3/2)   (line [cs 4 qn, b 3 en]), cs 4 dhn]
>
>   tcV04B =  -- measure 26
>     line [
>   {-  1   -}   tempo (3/2)   (line [a 3 en, a 3 qn])
>   {-  1   -} , f 3 sn, f 3 sn, f 3 sn, g 3 sn
>   {-  3   -} , rest sn, g 3 sn, f 3 sn, e 3 sn, f 3 hn
>   {-  3   -} , tempo (13/12) (line [a 3 den, g 3 den, g 3 en, g 3 en
>                                   , grace (-3) (b 3 den)])]
>
>   tcVnt08 =
>     line   [
>   {-  1   -}   e 5 en, e 5 en
>   {-  1   -} , tempo (3/2)   (line [g 5 en, e 5 en, e 5 en])
>   {-  1   -} , grace (-2) (e 5 qn)]
>
>   tcV04C = -- measure 27-28
>     line [
>   {-  4   -}   a 3 (wn+(3/20))
>   {-  3   -} , tempo (5/3)   (line [ a 3 qn,  e 4 qn, a 4 qn, a 4 qn])
>   {-  4   -} , tempo (3/2)   (line [cs 5 qn, cs 5 qn,  e 5 qn
>                                    , e 5 qn,  g 5 qn,  g 5 qn])
>   {-  2   -} , tempo (3/2)   (line [ g 5 qn,  g 5 qn,  g 5 en, e 5 en])
>   {-  2   -} , tempo (3/2)   tcVnt08
>   {-  1   -} , tempo (3/2)   (line [rest en, e 5 en, e 5 en])]
>
>   tcV04D = rest 0
>
>   tcVnt09 = 
>     line [
>   {-  1   -}   tempo (5/2)   (line [b 5 en, b 5 sn, a 5 sn, g 5 sn])]
>
>   tcV04E = -- measure 29-31
>     line [
>   {-  2   -}   tempo (3/2)   (line [ e 5 en,  g 5 en, e 5 en, grace (-2) (e 5 en), e 5 en
>                                   ,  e 5 en])
>   {-  2   -} , tempo (3/2)   (line [ g 5 qn,  e 5 en, g 5 qn, e 5 en])
>   {-  2   -} , tempo (3/2)   (line [cs 6 en, cs 6 tn, b 5 tn
>                                    , a 5 sn,  a 5 tn, b 5 tn, a 5 sn
>                                    , g 5 sn,  g 5 sn, g 5 sn, f 5 sn, a 5 en])
>   {-  2   -} , grace (-4)    (a 5 (dhn + (1/10)))
>   {-  2   -} , tempo (5/4)   (line [a 5 sn, b 5 en])
>   {-  2   -} , tempo (3/2)   (line [b 5 en, b 5 tn, a 5 tn, g 5 sn, grace (-1) (b 5 qn)])
>   {-  2   -} , a 5 ((1/6) + qn), a 5 sn, b 5 den
>   {-  4   -} , tempo (17/16) (line [cs 6 den, b 5 den,   b 5 en, cs 6 den
>                                    , e 6 en, cs 6 en,    b 5 sn,  a 5 sn ])
>   {-  2   -} , tempo (3/2)   (line [tcVnt09, cs 5 qn]), cs 5 qn 
>   {-  1   -} , tempo (5/8)   (line [g 5 dsn,  g 5 sn])
>   {-  1   -} , tempo (3/2)   (line [g 5 sn, f 5 sn,  e 5 en, f 5 en])
>   {-  2   -} , tempo (3/2)   (line [f 5 en, d 5 en, cs 5 en]), d 5 qn]
>
>   tcV04F = -- measure 32
>     line [
>   {-  4   -}    cs 5 en, cs 5 sn, d 5 sn, descent (E,5) dhn
>   {-  4   -}  , rest hn,  a 6 sn, a 6 sn, e 6 en, a 6 qn]
>
>   tcV04G = rest 0
>   tcV04H = rest 0
>
>   tcV05 = line [tcV05A, tcV05B, tcV05C, tcV05D, tcV05E, tcV05F, tcV05G, tcV05H]
>
>   tcV05A = -- measure 33
>     line [
>   {-  1   -}   tempo (5/4)   (line [a 6 en, f 6 sn, e 6 en])
>   {-  2   -} , e 6 en, e 6 en, e 6 tn, f 6 tn, g 6 den
>   {-  1   -} , tempo (5/4)   (line [g 6 dsn, g 6 tn, f 6 tn]), e 6 sn, rest sn
>   {-  1   -} , rest qn
>   {-  2   -} , tempo (3/2)   (line [rest qn, a 6 en, grace (-1) (e 6 qn), g 6 en])
>   {-  1   -} , f 6 sn, e 6 sn, f 6 en]
>
>   tcV05B = -- measure 34
>     line [
>   {-  6   -}   e 6 sn, descent (E,6) (en + ddqn), rest dqn, rest hn
>   {-  2   -} , f 6 sn, d 6 sn, cs 6 en, cs 6 sn, d 6 sn, e 6 en]
>
>   tcVnt10 =
>     line [
>   {-  1   -}   tempo (5/4)   (line [ e 6 en, e 6 sn, d 6 sn, cs 6 sn])
>   {-  1   -} , tempo (3/2)   (line [ f 6 en, f 6 sn, d 6 sn, cs 6 en])
>   {-  1   -} , tempo (5/4)   (line [cs 6 sn, d 6 sn, e 6 den])]
>
>   tcV05C = -- measure 35
>     line [
>   {-  2   -}   tempo (3/2)   tcVnt10
>   {-  2   -} , tempo (3/2)   (line [e 6 en,  e 6 sn, d 6 sn
>                                  , grace (-1) (cs 6 dqn), d 6 en])
>   {-  1   -} , d 6 sn, e 6 sn, d 6 sn, cs 6 sn
>   {-  1   -} , tempo (3/2)   (line [cs 6 en, b 5 en, b 5 en])
>   {-  2   -} , b 5 sn, a 5 sn, a 5 sn, b 5 sn, cs 6 sn, b 5 sn, cs 6 en]
>
>   tcV05D = -- measure 36
>     line [
>   {-  1   -}   tempo (5/4)   (line [cs 6 en, b 5 sn, a 5 sn, g 5 sn])
>   {-  1   -} , tempo (3/2)   (line [a 5 sn, g 5 sn, f 5 en
>                                   , tempo (3/2) (line [f 5 en, g 5 sn])])
>   {-  1   -} , f 5 sn, d 5 sn, f 5 sn, f 5 sn
>   {-  1   -} , tempo (3/2)   (line [f 5 en, e 5 en, e 5 en])
>   {-  4   -} , rest dhn, cs 6 sn, d 6 sn, e 6 en]
>
>   tcVnt11 =
>     line [
>   {-  1   -}   tempo (3/2)   (line [a 5 en, f 5 qn])
>   {-  1   -} , f 5 sn, g 5 sn, f 5 sn, e 5 sn
>   {-  1   -} , tempo (3/2)   (line [e 5 en, e 5 en, e 5 en])]
>
>   tcV05E = -- measure 37
>     line [
>   {-  1   -}   e 6 sn, e 6 tn, d 6 tn, cs 6 sn, cs 6 sn
>   {-  1   -} , cs 6 sn, b 5 sn, tempo (3/2) (line [g 5 sn, a 5 sn, g 5 sn])
>   {-  2   -} , b 5 sn, b 5 sn, a 5 sn, a 5 sn, a 5 qn
>   {-  2   -} , tempo (3/2)   tcVnt11
>   {-  2   -} , tempo (3/2)   (line [grace (-7) (b 5 en), a 5 qn, a 5 qn, a 5 en])]
>
>   tcVnt12 =
>     line [
>   {-  1   -}    tempo (3/2)  (line [a 5 tn, cs 6 tn, a 5 tn, g 5 tn, e 5 qn])
>   {-  1   -}  , a 5 sn, cs 6 sn, a 5 en
>   {-  1   -}  , tempo (3/2)  (line [a 5 en, grace (-2) (e 5 sn)]), d 5 sn, cs 5 sn]
>
>   tcVnt13 =
>     line [
>   {-  1   -}    tempo (7/4)  (line [d 5 en, cs 5 en, a 4 sn, g 4 en])
>   {-  2   -}  , e 4 sn, a 4 en, d 4 en, e 4 en, d 4 sn
>   {-  0.5 -}  , tempo (3/2)  (line [e 4 sn, d 4 sn, cs 4 sn])]
>
>   tcVnt14 =
>     line [
>   {-  2   -}   tempo (7/4)   (line [cs 4 en, d 4 en, cs 4 en, d 4 en, e 4 qn, e 4 en])
>   {-  1   -} , e 4 sn, f 4 sn, f 4 sn, f 4 sn]
>
>   tcV05F = -- measure 38-39
>     line [
>   {-  2   -}   tempo (3/2)   (line [g 5 qn, a 5 en]), a 5 en, a 5 en
>   {-  2   -} , tempo (3/2)   tcVnt12
>   {-  2   -} , tempo (7/4)   tcVnt13
>   {-  3   -} , b 3 qn, tempo (7/4) (line [cs 4 qn, cs 4 en, cs 4 qn, a 3 qn])
>   {-  2   -} , tempo (3/2)   tcVnt14
>   {-  1   -} , g 4 sn, a 4 sn, g 4 tn, a 4 tn, g 4 tn, f 4 tn
>   {-  1   -} , tempo (3/2)   (line [g 4 en, cs 5 en, f 4 en])
>   {-  0.5 -} , tempo (3/2)   (line [f 4 sn, e 4 sn, d 4 sn])
>   {-  0.9 -} , e 4 (en+(1/10))
>   {-  0.6 -} , tempo (5/4)   (line [d 4 sn, e 4 sn, d 4 sn])
>   {-  1   -} , c 4 sn, d 4 en, d 4 sn]
>
>   tcV05G = -- measure 40
>     line [
>   {-  2   -}   tempo (5/4)   (line [d 4 en, grace (-1) (c 4 en), b 3 en, a 3 en, g 3 en])
>   {-  2   -} , a 3 hn
>   {-  2   -} , a 3 qn, tempo (3/2) (line [a 3 en, a 3 en, g 3 en])
>   {-  2   -} , addDur sn [g 3, a 3, g 3, g 3, f 3, f 3, f 3, g 3]]
>
>   tcV05H = rest 0
>
>   tcV06 = line [tcV06A, tcV06B, tcV06C, tcV06D, tcV06E, tcV06F, tcV06G]
>
>   tcV06A = -- measure 41
>     line [
>   {-  4   -}   f 3 sn, e 3 den, e 3 qn, a 3 sn, a 3 den, a 3 den, a 3 sn
>   {-  3   -} , e 4 sn, e 4 den, e 4 qn, tempo (3/2) (line [f 4 en, e 4 en, e 4 en])
>   {-  1   -} , e 4 sn, f 4 sn,  f 4 sn, e 4 sn]
>
>   tcV06B = -- measure 42
>     line [
>   {-  4   -}   e 3 wn
>   {-  2   -} , tempo (3/2)   (line [rest qn, f 4 en, f 4 en,  g 4 qn])
>   {-  2   -} , tempo (3/2)   (line [a 4 en,  a 4 qn, b 4 qn, cs 5 en])
>          ]
>   tcVnt15 =
>     line [
>   {-  1   -}   tempo (3/2)   (line [ ds 6 en, ds 6 sn])
>   {-  1   -} , cs 6 sn, b 5 sn, b 5 sn, cs 6 sn
>   {-  1   -} ,  b 5 sn, a 5 sn, b 5 en, rest sn, b 5 sn]
>
>   tcV06C = -- measure 43
>     line [
>   {-  2   -}   tempo (3/2)   (line [ds 5 en, ds 5 qn,  f 5 qn,  g 5 en])
>   {-  2   -} , tempo (3/2)   (line [ a 5 en,  a 5 qn,  b 5 qn,  b 5 en])
>   {-  2   -} , tempo (3/2)   (line [cs 6 en, cs 6 qn, cs 6 qn, cs 6 en])
>   {-  2   -} , tempo (3/2)   tcVnt15]
>
>   tcV06D = -- measure 44
>     line [
>   {-  2   -}   a 4 hn
>   {-  2   -} , tempo (7/4)   (line [ b 5 en, a 5 sn, gs 5 sn, f 5 en
>                                    , a 5 en, a 5 en,  gs 5 sn, f 5 sn, d 5 en])
>   {-  2   -} , tempo (7/4)   (line [ a 5 en, a 5 en, gs 5 sn, f 5 sn
>                                  , gs 5 sn, f 5 sn,  f 5 en, f 5 en, ef 5 en])
>   {-  2   -} , cs 5 en, ef 5 en, tempo (3/2) (line [rest qn, ef 5 en])]
>
>   tcV06E = -- measure 45
>     line [
>   {-  2   -}   tempo (3/2)   (line [cs 5 en, b 4 en, a 4 en]), b 4 sn, b 4 sn, rest en
>   {-  3   -} , tempo (5/3)   (line [rest en, b 4 en, a 4 en, g 4 en, f 4 en, f 4 en
>                                   ,  g 4 en, a 4 en])
>   {-  2   -} , b 4 ((3/20)+hn+1/12)
>   {-  1   -} , tempo (3/2)   (line [b 4 en])]
>
>   tcV06F = -- measure 46
>     line [
>   {-  3   -}   tempo (3/2)   (line [cs 5 en, ds 5 hn, cs 5 en, ds 5 qn, cs 5 en])
>   {-  2   -} , tempo (3/2)   (line [cs 5 en, e 5 sn, ds 5 sn, e 5 en, ds 5 dqn])
>   {-  1.5 -} , tempo (3/2)   (line [ds 5 en, e 5 sn, ds 5 sn, cs 5 en
>                                   , cs 5 en, cs 5 sn])
>   {-  1.5 -} , ds 5 dqn]
>
>   tcVnt16 =
>     line [
>   {-  2   -}   d 5 qn, ds 5 sn, e 5 sn, ds 5 sn, cs 5 sn
>   {-  1   -} , tempo (5/4)   (line [ds 5 en, ds 5 sn, cs 5 sn, b 4 sn])]
>
>   tcV06G = -- measure 47
>     line [
>   {-  2   -}   tempo (3/2)   tcVnt16
>   {-  0.7 -} , tempo (3/2)   (line [cs 5 en, cs 5 en])
>   {-  0.7 -} , a 4 ((1/12)+(3/32))
>   {-  1.1 -} , tempo (4/3)   (line [b 4 en, cs 5 en, ds 5 en])
>   {-  1   -} , times 2 (grace (-2) (e 5 en))
>   {-  2.5 -} , tempo (4/5)   (line [ds 5 sn,  e 5 sn, ds 5 sn, cs 5 sn
>                                 , ds 5 sn, cs 5 sn,  b 4 sn, cs 5 tn, b 5 tn])]
>
>   tcV07 = line [tcV07A, tcV07B, tcV07C, tcV07D, tcV07E, tcV07F, tcV07G, tcV07H]
>
>   tcVnt17 =
>     line [
>             a 4 en, b 4 en, tempo (3/2) (line [cs 5 en, cs 5 sn]), ds 5 en, ds 5 en]
>
>   tcV07A = -- measure 48
>     line [
>   {-  2   -}   tempo (5/4) tcVnt17
>   {-  2   -} , f 5 en, f 5 sn, g 5 sn, a 5 sn, a 5 sn, a 5 sn, b 5 sn
>   {-  4   -} , a 5 (dhn+(1/12)), tempo (3/2) (line [e 6 en, e 6 en])]
>
>   tcVnt18 =
>     line [
>   {-  2   -}   e 6 en, cs 6 en, cs 6 en, cs 6 en
>   {-  2.5 -} , tempo (6/7)   (line [f 6 en, f 6 en, d 6 en])
>   {-  3.5 -} , tempo (10/13) (line [f 6 en, d 6 en, f 6 en, d 6 en, d 6 en])]
>
>   tcV07B = -- measure 49
>     line [
>   {-  4   -}   tempo (3/2) (addDur en [cs 6, e 6, cs 6, e 6, cs 6, cs 6
>                                      , cs 6, e 6, e 6, cs 6, e 6, cs 6])
>   {-  4   -}   , tempo (7/4) tcVnt18]
>
>   tcV07C = -- measure 50
>     line [
>   {-  8   -}   tempo (3/2) (addDur en [d 6, f 6, f 6, d 6, f 6, d 6
>                                      , f 6, d 6, d 6, d 6, g 6, g 6
>                                      , e 6, g 6, e 6, g 6, e 6, e 6
>                                      , e 6, g 6, g 6, e 6, g 6, e 6])]
>
>   tcV07D = -- measure 51
>     line [
>   {-  6   -}   tempo (3/2) (addDur en [g 6, e 6, e 6, e 6, a 6, a 6
>                                      , f 6, a 6, f 6, a 6, f 6, f 6
>                                      , f 6, a 6, a 6, f 6, f 6, g 6])
>   {-  0.7 -} , tempo (3/2)   (line [f 6 en, a 6 en])
>   {-  1.3 -} , grace (-2) (b 6 ((1/12)+den)), a 6 sn]
>
>   tcV07E = -- measure 52
>     line [
>   {-  2   -}   tempo (3/2)   (line [a 6 en, b 6 en, a 6 en, b 6 den, a 6 sn, b 6 en])
>   {-  1   -} , tempo (3/2)   (line [a 6 en, g 6 en, g 6 en])
>   {-  0.5 -} , tempo (3/2)   (line [a 6 sn, g 6 sn, e 6 sn])
>   {-  0.5 -} , g 6 sn, grace (-3) (g 6 sn)
>   {-  1   -} , tempo (5/4)   (line [e 6 en, gs 6 sn, e 6 sn, g 6 sn])
>   {-  1   -} , tempo (3/2)   (line [e 6 en
>                                 , tempo (3/2) (line [g 6 en, grace (-2) (e 6 sn)
>                                                   , ds 6 en, a 6 sn])])
>   {-  1   -} , grace (-3) (f 6 sn), d 6 sn, f 6 sn, d 6 sn
>   {-  1   -} , tempo (5/4)   (line [d 6 den, a 5 sn, g 5 sn])]
>
>   tcV07F = -- measure 53
>     line [
>   {-  1   -}   tempo (5/4)   (line [a 5 en, a 5 sn, g 5 sn, e 5 sn])    
>   {-  0.5 -} , tempo (3/2)   (line [ds 5 en, e 5 sn])
>   {-  2.5 -} , ds 5 dqn, tempo (3/2) (line [ds 5 en, e 5 en, f 5 en])
>   {-  4   -} , tempo (3/2) (addDur en [g 5, g 5, a 5, a 5, cs 6, cs 6
>                                      , ds 6, ds 6, e 6, e 6,  f 6,  f 6])]
>
>   tcV07G = -- measure 54
>     line [
>   {-  1   -}   tempo (3/2)   (line [grace (-2) (g 6 en), f 6 en, e 6 en])
>   {-  1   -} , f 6 en, grace 1 (e 6 en)
>   {-  2   -} , tempo (3/2)   (line [grace (-2) (g 6 en), f 6 en, e 6 en]), e 6 qn
>   {-  2   -} , tempo (3/2)   (line [rest qn, e 6 en, f 6 en, f 6 en, f 6 en])
>   {-  2   -} , tempo (3/2)   (line [f 6 qn, f 6 en]), grace (-2) (cs 7 qn)]
>
>   tcVnt19 =
>     line [
>   {-  2   -}   cs 6 en, tempo (3/4) (line [cs 6 sn, d 6 sn, b 6 sn]), a 6 en
>   {-  1   -} , tempo (3/2)   (line [a 6 en, g 6 sn, f 6 sn, e 6 en])]
>
>   tcV07H = -- measure 55
>     line [
>   {-  1   -}   tempo (3/2)   (line [grace (-2) (cs 7 en), b 6 qn])
>   {-  0.7 -} , tempo (3/2)   (line [grace 4 (g 6 (en+(1/12)))
>                                   , tempo (3/2) (line [a 6 sn])])
>   {-  1.3 -} , g 6 ((1/12)+sn), e 6 sn, b 4 en
>   {-  1.0 -} , tempo (3/2)   (line [rest en, a 6 en, b 6 en])
>   {-  2   -} , tempo (3/2)   tcVnt19
>   {-  0.5 -} , tempo (3/2)   (line [rest sn, f 6 sn, e 6 sn])
>   {-  1.0 -} , tempo (5/4)   (line [f 6 sn, f 6 sn, e 6 en, d 6 sn])
>   {-  0.5 -} , tempo (3/2)   (line [e 6 sn, d 6 sn, grace (-1) (cs 6 sn)])]
>
>   tcV08 = line [tcV08A, tcV08B, tcV08C, tcV08D, tcV08E, tcV08F, tcV08G, tcV08H]
>
>   tcV08A = -- measure 56
>     line [
>   {-  0.7 -}   tempo (3/2)   (line [cs 6 en, d 6 en])
>   {-  1.3 -} , cs 6 ((1/12) + qn)
>   {-  2   -} , tempo (3/2)   (line [cs 6 en, cs 6 en, b 5 en
>                                    , b 5 en, cs 6 sn, b 5 sn, a 5 en])
>   {-  3   -} , b 5 sn, a 5 sn, g 5 en, a 5 hn
>   {-  1   -} , tempo (5/4)   (line [rest den, a 5 sn, b 5 sn])]
>
>   tcV08B = -- measure 57
>     line [
>   {-  1   -}   cs 6 sn, cs 6 sn, tempo (3/2) (line [cs 6 sn, b 5 sn, a 5 sn])
>   {-  1   -} , tempo (7/4)   (line [cs 6 en, cs 6 sn, b 5 sn, a 5 sn, cs 6 en])
>   {-  1   -} , tempo (3/2)   (line [cs 6 tn, b 5 tn, a 5 sn, d 6 en, d 6 en])
>   {-  1   -} , tempo (3/2)   (line [ d 6 tn, b 5 tn, a 5 sn, cs 6 en, cs 6 sn, b 5 sn])
>   {-  2   -} , a 5 sn, cs 6 sn, b 5 sn, a 5 sn, d 6 sn, d 6 sn, d 6 tn, b 5 tn, a 5 sn
>   {-  1   -} , cs 6 sn, cs 6 tn, b 5 tn, a 5 sn, cs 6 sn
>   {-  1   -} , cs 6 tn, b 5 tn, a 5 sn, a 5 sn, a 5 sn]
>
>   tcV08C = -- measure 58
>     line [
>   {-  2   -}   grace (-2) (a 5 sn), gs 5 sn
>              , grace (-2) (as 5 sn), g 5 sn, e 5 sn, as 5 sn, d 5 den
>   {-  2   -} , grace (-1) (cs 5 sn), cs 5 en, a 5 sn, g 5 sn, e 5 sn, g 5 sn
>   {-  2   -} , e 5 sn, d 5 sn, g 5 sn, e 5 tn, ef 5 tn, d 5 en, d 5 en
>   {-  2   -} , cs 5 qn, grace (-12) (a 4 en), cs 5 en]
>
>   tcV08D = -- measure 59
>     line [
>   {-  4   -}   tempo (5/4) (addDur en [d 5, e 5, f 5, e 5, d 5]), descent (E,5) hn
>   {-  2   -} , tempo (3/2)   (line [d 4 qn, cs 4 en, d 4 en, d 4 en, e 4 en])
>   {-  2   -} , tempo (3/2)   (line [e 4 en,  f 4 en, e 4 en, d 4 qn, d 4 en])]
>
>   tcVnt20 =
>     line [
>   {-  4   -}   tempo (3/2)   (line [grace (-2) (e 5 en), cs 6 en
>                          ,  a 5 en, cs 6 qn, cs 6 en])]
>
>   tcV08E = -- measure 60
>     line [
>   {-  4   -}   e 4 en, e 4 (dqn + hn + 3/7)
>   {-  4   -} , tempo (7/8) tcVnt20]
>
>   tcVnt21 =
>     line [
>   {-  7   -}   tempo (3/2)   (line [cs 6 qn, cs 6 en, cs 6 qn, cs 6 en, cs 6 qn, cs 6 en
>                                   , cs 6 sn,  d 6 sn, cs 6 sn])]
>
>   tcV08F = -- measure 61
>     line [
>   {-  4   -}   tempo (7/8)   tcVnt21
>   {-  4   -} , b 5 dhn, tempo (3/2) (line [b 5 en, a 5 en, a 5 en])]
>
>   tcV08G = -- measure 62
>     line [
>   {-  0.7 -}   tempo (3/2)   (line [a 5 en, b 5 en])
>   {-  5.3 -} , cs 6 ((1/12) + hn), ascent (Cs, 6) qn, rest hn
>   {-  2.0 -} , tempo (3/2)   (line [cs 6 qn, cs 6 qn, cs 6 qn])]
>
>   tcV08H = -- measure 63
>     line [
>   {-  2   -} tempo (3/2)     (line [cs 6 qn, cs 6 qn, d 6 en, cs 6 en])
>   {-  3.3 -} , tempo (3/2)   (line [d 6 en, cs 6 en]), b 5 ((1/12) + hn + (1/12))
>   {-  0.7 -} , tempo (3/2)   (line [cs 6 en, a 5 en])
>   {-  2   -} , tempo (3/2)   (line [cs 6 qn, e 6 en, cs 6 en, a 5 en, a 5 en])]
>
>   tcV09 =
>     line [tcV09A, tcV09B, tcV09C, tcV09D, tcV09E
>         , tcV09F, tcV09G, tcV09H, tcV09J, tcV09K, tcV09L]
>
>   tcV09A = -- measure 64
>     line [
>   {-  6   -}   a 5 den, b 5 sn, a 5 wn, rest qn
>   {-  2   -} , tempo (3/2) (addDur en [a 4, b 4, c 5, d 5, ef 5, f 5])]
>
>   tcVnt22 =
>     line [
>   {-  3.5 -}   fs 5 en, gs 5 en, a 5 en, b 5 en, c 6 en
>              , d 6 en, tempo (3/2) (line [c 6 sn, d 6 sn, c 6 sn])]
>
>   tcV09B = -- measure 65-66
>     line [
>   {-  2   -}   tempo (7/4) tcVnt22
>   {-  1   -} , tempo (3/2)   (line [tempo (3/2) (line [b 5 en, c 6 sn]), b 5 en, a 5 en])
>   {-  1   -} , b 5 tn, a 5 tn, gs 5 tn, fs 5 tn, gs 5 sn, a 5 tn, gs 5 tn
>   {-  2   -} , tempo (3/2)   (line [fs 5 en, gs 5 sn, fs 5 sn, gs 5 sn, fs 5 sn, f 5 sn
>                                   ,  a 5 tn, fs 5 tn, ef 5 sn
>                                   ,  d 5 en, grace (-1) (ef 5 sn)])
>   {-  1   -} , tempo (3/2)   (line [ef 5 sn, c 5 sn, d 5 sn, c 5 sn, b 4 sn, a 4 sn])
>   {-  1   -} , tempo (3/2)   (line [a 4 en, a 4 sn, g 4 en, g 4 sn])]
>
>   tcV09C = -- measure 67
>     line [
>   {-  1   -}   tempo (5/4) (line [a 4 sn, g 4 sn, e 4 sn, d 4 en])
>   {-  2   -} , tempo (7/4) (addDur en [e 4, g 4, d 4, e 4, f 4, g 4, g 4])
>   {-  1   -} , tempo (7/4) (line [a 4 en, a 4 sn, g 4 sn, f 4 en, g 4 sn])
>   {-  2   -} , a 4 qn, tempo (3/2) (line [a 4 qn, a 4 en])
>   {-  2   -} , tempo (3/2) (line [grace (-1) (c 5 en), d 5 en, d 5 en
>                                 , ds 5 en, fs 5 en, fs 5 en])]
>
>   tcV09D = -- measure 68
>     line [
>   {-  1.5 -}   tempo (3/2) (line [fs 5 en, fs 5 en, gs 5 en, a 5 sn, b 5 en])
>   {-  0.5 -} , b 5 sn, c 6 sn
>   {-  1   -} , tempo (3/2) (line [d 6 en, grace (-3) (d 6 sn), c 6 sn, b 5 en])
>   {-  1   -} , tempo (7/4) (line [ds 6 en, ds 6 sn,  c 6 sn, b 5 sn, d 6 en])
>   {-  1   -} , tempo (3/2) (line [ d 6 sn,  c 6 sn,  b 5 en, grace (-5) (e 6 en)])
>   {-  1   -} , tempo (5/4) (line [ e 6 en,  d 6 en, cs 6 sn])
>   {-  2   -} , tempo (7/4) (line [ d 6 sn, cs 6 sn,  e 6 en
>                       , grace 3 (cs 6 en), b 5 en, cs 6 sn
>                                 , b 5 en, cs 6 sn, b 5 en])]
>
>   tcV09E = -- measure 69
>     line [
>   {-  2   -}   tempo (3/2) (line [cs 6 sn,  b 5 sn, cs 6 qn]), cs 6 qn
>   {-  3.7 -} , tempo (3/2) (line [ b 5 qn,  a 5 en]),  a 5 (hn + (1/6))
>   {-  0.3 -} , tempo (3/2) (line [ c 6 en])
>   {-  2   -} , tempo (3/2) (line [rest en, grace (-2) (a 5 qn)
>                                 ,  f 5 en,  d 5 en, d 5 en])]
>
>   tcV09F = -- measure 70
>     line [
>   {-  4   -}   d 5 wn
>   {-  4   -} , tempo (3/2) (line [rest en, c 6 qn, rest en, grace (-2) (a 5 qn)])
>   {-  2   -} , tempo (3/2) (line [f 5 qn, d 5 qn, d 5 en, cs 5 en])]
>
>   tcV09G = -- measure 71
>     line [
>   {-  6   -}   cs 5 dqn, tempo (3/2) (line [cs 5 en]), d 5 ((1/24) + dhn), rest qn
>   {-  2   -} , tempo (3/2) (line [e 6 sn, cs 6 den, f 6 sn, cs 6 en, f 6 sn, cs 6 qn])]
>
>   tcV09H = -- measure 72
>     line [
>   {-  4   -}   tempo (3/2) (line [d 6 den, cs 6 sn, a 5 en]), g 5 dqn, fs 5 en, rest qn
>   {-  2   -} , tempo (3/2) (line [rest sn, fs 6 sn, d 6 qn, d 6 den, e 6 sn, d 6 en])
>   {-  1   -} , d 6 en, e 6 en
>   {-  1   -} , tempo (3/2) (line [ e 6 sn, fs 6 sn, e 6 sn, d 6 sn,  d 6 en])]
>
>   tcV09J = -- measure 73
>     line [
>   {-  6   -}   slur 3 (line [c 6 hn, cs 6 dhn, c 6 qn])
>   {-  2   -} , tempo (3/2) (line [ rest sn, d 6 sn, cs 6 qn, cs 6 en, d 6 en, cs 6 en])]
>
>   tcV09K = -- measure 74
>     line [
>   {-  3   -}   tempo (3/2) (line [cs 6 en]), a 5 ((1/6) + hn)
>   {-  3.8 -} , tempo (3/2) (line [ d 6 en, d 6 sn, a 5 sn]), e 5 (hn + 1/12 + den)
>   {-  1.2 -} , e 5 sn, d 5 qn]
>
>   tcV09L = -- measure 75
>     dim 3 $ line [
>   {-  1   -}   tempo (3/2) (line [e 5 qn, d 5 en])
>   {-  7   -}   , e 5 (wn + qn), rest hn]
>
>   tcP = rest 0

TC ========================================================================================

>
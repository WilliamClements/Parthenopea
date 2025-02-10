> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE ScopedTypeVariables #-}

Covers
William Clements
January 13, 2023

> module Parthenopea.Tunes.Covers (
>                littleSailor, slot, ssailor, deathlessHorsie, basicLick, packardGoose, yahozna, littleDH
>                , greenMore, testslot) where

> import Data.Map (Map)
> import Euterpea.Music
> import HSoM.Examples.MoreMusic hiding (grace)
> import Percussion
> import Parthenopea.Siren
> import Parthenopea.SoundFont.SFSpec
  
Saucy Sailor ==========================================================================================================

> ssTempo                :: Dur
> ssTempo                                  = 1
> ssTranspose            :: AbsPitch
> ssTranspose                              = 0
>
> ssLead1_, ssLead2_, ssPicked_, ssBass_
>                        :: BandPart
> ssLead1_                                 = makePitched Flute                ssTranspose          0         90
> ssLead2_                                 = makePitched Flute                ssTranspose          0        115
> ssPicked_                                = makePitched AcousticGuitarNylon  ssTranspose          0         75
> ssBass_                                  = makePitched ElectricBassPicked   ssTranspose          0        100
>
> littleSailor           :: Music (Pitch, Volume)
> littleSailor =
>     removeZeros
>     $ instrument Clarinet
>     $ addVolume 100
>     $ transpose (-12)
>     $ line
>       [
>    --  3        Come      me       ow -     -n      one
>           line [f 5 en,  g 5 en,  f 5 en, e 5 en,  c 5 qn],
>    --  3         Come      me      fai -    -ai-    -ai-   -air-    one
>           line [f 5 en,  g 5 en,  f 5 sn, g 5 sn, f 5 sn, e 5 sn, c 5 qn],
>    --  3         Co-      -ome      now    un- 
>           line [e 5 en,  f 5 en,  g 5 qn, c 6 qn],
>    --  3         to-      -o       me
>           line [bf 5 en, a 5 en, g 5 hn]
>       ]
>
> ssailor                :: Map InstrumentName InstrumentName → Music (Pitch, [NoteAttribute])
> ssailor dynMap =
>     removeZeros
>     $ tempo                              ssTempo
>     $ keysig C Mixolydian
>     $ ((if includeOpen then xOpenT        else rest 0)
>          :+: (if includeSong then xSongT  else rest 0)
>          :+: (if includeClos then xClosT  else rest 0))
>       :=:
>       ((if includeOpen then xOpenG        else rest 0)
>          :+: (if includeSong then xSongG  else rest 0)
>          :+: (if includeClos then xClosG  else rest 0))
>       :=: 
>       ((if includeOpen then xOpenB        else rest 0)
>          :+: (if includeSong then xSongB  else rest 0)
>          :+: (if includeClos then xClosB  else rest 0))
>
>       where
>         includeOpen = False
>         includeSong = True
>         includeClos = False
>
>         ssLead1                          = replace ssLead1_    dynMap
>         ssLead2                          = replace ssLead2_    dynMap
>         ssPicked                         = replace ssPicked_   dynMap
>         ssBass                           = replace ssBass_     dynMap
>
>         xOpenT = rest 0
>         xSongT = line [rest hn, bandPart ssLead1 xSongTA, bandPart ssLead2 xSongTB]
>         xClosT = rest 0
>
>         xOpenG = rest 0
>         xSongG = line [rest dhn, bandPart ssPicked (times 7 xSongGrep)]
>         xClosG = rest 0
>
>         xOpenB = rest 0
>         xSongB = bandPart ssBass 
>                  $ line [rest dhn
>                       , times 6 (line [xSongB1, xSongB2])
>                       , xSongB1
>                       , c 2 wn]
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

Yahozna ===============================================================================================================

> yaTempo                :: Dur
> yaTempo                                  = 4
> yaTranspose            :: AbsPitch
> yaTranspose                              = 0
>
> yaGuitar, yaBass, yaChoir
>                        :: (InstrumentName, Velocity)
> yaGuitar                                 = (ElectricGuitarJazz,  75)
> yaBass                                   = (ElectricBassPicked,  75)
> yaChoir                                  = (AltoSax,            110)
>
> yahozna                :: Music (Pitch, Volume)
> yahozna =
>   removeZeros
>   $ tempo yaTempo
>   $ transpose yaTranspose
>   $ keysig C Mixolydian
>   $          bandPart_ yaGuitar (line [rest dwn, rest dwn] :+: yahoznaGuitar)
>          :=: bandPart_ yaBass (yahoznaBassIntro :+: yahoznaBass)
>          :=: bandPart_ yaChoir choirPart
>   where
>     bandPart_          :: (InstrumentName, Velocity) → Music Pitch → Music (Pitch, Volume)
>     bandPart_ (inst, vel) m              = mMap (, vel) (instrument inst m)
>     guitarLick 
>       =    line [ c 3 hn,  c 3 qn,  c 3 qn,  c 3 wn]
>        :+: line [ c 3 qn,  c 3 qn,  c 3 qn,  c 3 qn]
>
>     bassIntroLick1
>       =    line [rest dwn, rest hn, ef 2 qn, c 2 qn, ef 2 qn, c 2 qn]
>
>     bassIntroLick2
>       =     line [rest dwn, rest hn]
>         :+: addDur sn [ c 3,  b 2, bf 2,  a 2, af 2,  g 2, gf 2,  f 2
>                   , e 2, ef 2,  d 2, df 2,  c 2,  b 1, bf 1,  a 1]
>
>     bassLick
>       = line [c 2 dhn, c 2 hn, c 2 hn, c 2 qn, bf 1 hn, f 2 hn]
>
>     yahoznaGuitar = 
>       instrument ElectricGuitarJazz
>       $ times 12 (guitarLick :=: transpose 7 guitarLick)
>
>     yahoznaBassIntro = 
>       instrument ElectricBassPicked
>       $ times 3 bassIntroLick1 :+: bassIntroLick2
>
>     yahoznaBass = 
>       instrument ElectricBassPicked
>       $ times 8 bassLick
>
>     choirPart
>       = instrument AltoSax
>         $ times 6 (line [rest dwn, rest dwn])
>           :+: line [g 3 (wn + wn), a 3 wn, a 3 dwn, a 3 hn, bf 3 wn, bf 3 wn]
>           :+: line [rest wn, bf 3 hn,  c 4 hn,  a 3 dwn]

Slot ==================================================================================================================

> testslot               :: Music (Pitch, Volume)
> testslot =
>    removeZeros
>    $ tempo 2
>    $ transpose (-24)
>    $ keysig G Dorian
>    $ addVolume 110 $ instrument SynthBass1 (line [ f 4 en ] )
>
> slotTempo              :: Dur
> slotTempo                                = 2
> slotTranspose          :: AbsPitch
> slotTranspose                            = 0
>
> slotLead_, slotStrum_, slotBass_, slotPerc_
>                        :: BandPart
> slotLead_                                = makePitched Violin              slotTranspose 0                100
> slotStrum_                               = makePitched AcousticGuitarNylon slotTranspose 0                100
> slotBass_                                = makePitched SynthBass1          slotTranspose 0                110
> slotPerc_                                = makeNonPitched                                                 100
>
> slot                   :: Int → DynMap → Music (Pitch, [NoteAttribute])
> slot nn dynMap                           =
>    removeZeros
>    $ tempo slotTempo
>    $ keysig G Dorian
>    $ chord [ bandPart slotLead          (vSlotV nn)
>            , bandPart slotStrum         (vSlotG nn)
>            , bandPart slotBass          (vSlotC nn)
>            , bandPart slotPerc          (vSlotP nn)]
>    where
>      slotLead                            = replace slotLead_ dynMap
>      slotStrum                           = replace slotStrum_ dynMap
>      slotBass                            = replace slotBass_ dynMap
>      slotPerc                            = replace slotPerc_ dynMap
>      
>      vSlotV            :: Int → Music Pitch
>      vSlotV n                            = line [ vSlotV01, times n (line [vSlotV02, vSlotV03])]
>      vSlotV01, vSlotV02, vSlotV03
>                        :: Music Pitch
>      vSlotV01                            = rest dhn
>      vSlotV02                            = times 4 (rest dhn)
>      vSlotV03                            =
>        line [ f 4 en,  g 4 en,  a 4 en,  b 4 en,  c 5 en,  d 5 en
>            ,  c 5 qn,          bf 4 en,  a 4 en, bf 4 en,  f 4 en
>            ,  f 4 en,  c 4 en,  c 4 en,  e 4 en,  f 4 en,  e 4 en
>            ,  f 4 en,  g 4 en,  f 4 qn, rest qn]
>
>      vSlotG            :: Int → Music Pitch
>      vSlotG n                            = line [ vSlotG01, times n vSlotG02]
>      vSlotG01, vSlotG02
>                        :: Music Pitch
>      vSlotG01                            = rest dhn
>      vSlotG02                            =
>        line [ times 4 (triad C (CustomMode "Sus2") (G, 3) dhn)
>             , times 2 (triad F Major               (F, 3) (2*dhn))]
>
>      vSlotC            :: Int → Music Pitch
>      vSlotC n                            = times n (line [vSlotC01, vSlotC02, vSlotC03])
>
>      vSlotC01, vSlotC02, vSlotC03
>                        :: Music Pitch
>      vSlotC01                            = line [ rest hn, rest en, c 3 en ]
>
>      vSlotC02                            =
>        line [ c 2 qn,  c 2 qn,  c 2 qn
>             , d 2 qn,  d 2 qn,  d 2 qn
>             ,ef 2 qn, ef 2 qn, ef 2 qn
>             , e 2 qn,  e 2 qn,  e 2 qn]
>      vSlotC03                            = line [f 2 hn, rest qn, times 2 (rest dhn)]
>
>      vSlotP            :: Int → Music Pitch
>      vSlotP n                            = times n (line [vSlotP01, vSlotP02, vSlotP03])
>
>      vSlotP01, vSlotP02, vSlotP03
>                        :: Music Pitch
>      vSlotP01                            = line [ rest hn, rest en, perc LowTom en ]
>
>      vSlotP02                            = times 8 (line [ perc Maracas en, perc Maracas en, perc HighAgogo en ])
>
>      vSlotP03                            = line [perc ClosedHiHat hn, rest qn, times 2 (perc OpenHiHat dhn)]

TC ====================================================================================================================

> tcTranspose            :: AbsPitch
> tcTranspose                              = 3
> leadTranspose          :: AbsPitch
> leadTranspose                            = 0
> repeatTranspose        :: AbsPitch
> repeatTranspose                          = 0
> bassTranspose          :: AbsPitch
> bassTranspose                            = 0
> tcTempo                :: Dur
> tcTempo                                  = 1
>
> tcLead_, tcRepeat_, tcBass_, tcPerc_
>                        :: BandPart
> tcLead_                                  = makePitched OverdrivenGuitar        tcTranspose leadTranspose  100
> tcRepeat_                                = makePitched FrenchHorn              tcTranspose repeatTranspose 50
> tcBass_                                  = makePitched FretlessBass            tcTranspose bassTranspose   85
> tcPerc_                                  = makeNonPitched                                                 100
>
> basicLick :: DynMap → Music (Pitch, [NoteAttribute])
> basicLick dynMap =
>   removeZeros
>   $ tempo                                tcTempo
>   $ keysig A Major
>   $ chord [ bandPart tcLead    tcV
>           , bandPart tcRepeat  tcG
>           , bandPart tcBass    tcC
>           , bandPart tcPerc    tcP]
>   where
>
>   tcLead                                 = replace tcLead_         dynMap
>   tcRepeat                               = replace tcRepeat_       dynMap
>   tcBass                                 = replace tcBass_         dynMap
>   tcPerc                                 = replace tcPerc_         dynMap
>
>   licks1                                 = 20
>   licks2                                 = 18
>   licks3                                 = licks1 + licks2 - 3
>
>   gUnit :: Music Pitch
>   gUnit = addDur qn [f 3, a 3, b 3, a 3, f 3, a 3, b 3, a 3
>                    , e 3, a 3, b 3, a 3, e 3, a 3, b 3, a 3]
>   gUnit' = transpose 12 gUnit
>   tcG = line [times licks1 gUnit, times (licks2 - 1) (chord [gUnit, gUnit']), dim 1 gUnit]
>
>   cUnit :: Music Pitch
>   cUnit = addDur (2 * wn) [d 3, a 2]
>   tcC = line [rest (16 * qn), times licks3 cUnit, dim 1 (times 2 cUnit)]
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
>   {-  4   -} , cs 6  (1/12 + qn), descendFrom tcLead (Cs, 6) Cs chromatic (qn*3)
>   {-  4   -} , tempo (5/4) tcVnt58
>   {-  1   -} , grace (-2) (b 5 den), a 5 sn
>   {-  1   -} , tempo (3/2)   (line [a 5 en, g 5 qn])
>   {-  0.5 -} , g 5  (qn + 1/12)
>   {-  2   -} , tempo (3/2)   (line [a 5 en, g 5 en, g 5 qn, a 5 en])
>   {-  1   -} , tempo (3/2)   (line [tempo (3/4) (line [b 5 sn, a 5 tn]), g 5 qn])
>   {-  0.5 -} , g 5 sn, a 5 sn
>   {-  0.5 -} , a 5 tn, g 5 tn, a 5 tn, g 5 tn]
>
>   tcV01D = -- measure 8
>     line [
>   {-  1   -}   tempo (3/2)   (line [f 5 sn, e 5 sn, e 5 qn])
>   {-  7   -} , e 5 wn, ascendFrom tcLead (F,5) F chromatic dhn]
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
>   {-  4   -} , f 6 en, e 6 en, rest dhn
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
>   {-  2   -} , cs 5 qn, descendFrom tcLead (Cs,6) Cs chromatic qn]
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
>   {-  3   -}   tempo (3/2)   (line [f 5 en, e 5 en, d 5 en]), e 5 en, e 5 en, d 5 en, cs 5 en]
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
>   {-  2   -} , ascendFrom tcLead (E,5) E chromatic hn
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
>   {-  1   -} , rest qn -- TODO: resolve
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
>   {-  4   -}    cs 5 en, cs 5 sn, d 5 sn, descendFrom tcLead (E,5) E chromatic dhn
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
>   {-  6   -}   e 6 sn, descendFrom tcLead (E,6) E chromatic (en + ddqn), rest dqn, rest hn
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
>   {-  1   -}   tempo (3/2)   (line [ d 6 en, d 6 sn])
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
>   {-  3   -} , tempo (5/3)   (line [rest en, b 4 en, a 4 en, g 4 en, f 4 en, f 4 en,  g 4 en, a 4 en])
>   {-  2   -} , b 4 ((3/20)+hn+1/6)
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
>   {-  3.5 -}
>     tempo (3/2)
>       (line [a 6 en, b 6 en, a 6 en, b 6 den, a 6 sn, b 6 en, a 6 en, g 6 en, g 6 en, a 6 sn, g 6 sn, e 6 sn])
>   {-  0.5 -}
>     , g 6 sn, grace (-3) (g 6 sn)
>   {-  1   -} , tempo (5/4)   (line [e 6 en, gs 6 sn, e 6 sn, g 6 sn])
>   {-  1   -}
>     , tempo (3/2)
>       (line [e 6 en, tempo (3/2) (line [g 6 en, grace (-2) (e 6 sn), ds 6 en, a 6 sn])])
>   {-  1   -} , grace (-3) (f 6 sn), d 6 sn, f 6 sn, d 6 sn
>   {-  1   -} , tempo (5/4)   (line [d 6 den, a 5 sn, g 5 sn])]
>
>   tcV07F = -- measure 53
>     line [
>   {-  1   -}   tempo (5/4)   (line [a 5 en, a 5 sn, g 5 sn, e 5 sn])    
>   {-  0.5 -} , tempo (3/2)   (line [chord [ds 5 en, a 5 en], e 5 sn])
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
>   {-  4   -}   tempo (5/4) (addDur en [d 5, e 5, f 5, e 5, d 5]), descendFrom tcLead (E,5) E chromatic hn
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
>   {-  5.3 -} , cs 6 ((1/12) + hn), descendFrom tcLead (Cs, 6) Cs chromatic qn, rest hn
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

DH ====================================================================================================================

> littleDH               :: Music (Pitch, Volume)
> littleDH =
>   instrument RhodesPiano $  addVolume 98 $ line [b 4 en, t32 [b 4 en, gs 4 sn, b 4 en, gs 5 sn], fs 5 dqn, rest qn]
>
> dhTempo                :: Dur
> dhTempo                                  = 1
> dhTranspose            :: AbsPitch
> dhTranspose                              = 3
>
> dhVibe_, dhLead_, dhBass_, dhOrgn_, dhSynh_, dhPerc_
>                        :: BandPart
> dhVibe_                                  = makePitched Vibraphone            dhTranspose      0         75
> dhLead_                                  = makePitched Violin                dhTranspose      0        100
> dhBass_                                  = makePitched ElectricBassPicked    dhTranspose      0         75
> dhOrgn_                                  = makePitched ReedOrgan             dhTranspose      0         90
> dhSynh_                                  = makePitched FrenchHorn            dhTranspose      0        100
> dhPerc_                                  = makeNonPitched                                              100
>
> dhMeasuresIntro, dhMeasuresB1, dhMeasuresA, dhMeasuresCs, dhMeasuresB2, dhMeasuresOutro, dhMeasuresAll
>                        :: Int
> dhMeasuresIntro                          = 12
> dhMeasuresB1                             = 22
> dhMeasuresA                              = 30
> dhMeasuresCs                             = 42
> dhMeasuresB2                             = 18
> dhMeasuresOutro                          = 0
> 
> dhMeasuresAll                            = dhMeasuresIntro + dhMeasuresB1 + dhMeasuresA + dhMeasuresCs + dhMeasuresB2 + dhMeasuresOutro
>
> deathlessHorsie        :: Map InstrumentName InstrumentName → Music (Pitch, [NoteAttribute])
> deathlessHorsie dynMap =
>   removeZeros
>   $ tempo dhTempo
>   $ chord [vibeMusic, percMusic, leadMusic, bassMusic, synthMusic, organMusic]
>
>   where
>
>     dhVibe                               = replace dhVibe_ dynMap
>     dhLead                               = replace dhLead_ dynMap
>     dhBass                               = replace dhBass_ dynMap
>     dhOrgn                               = replace dhOrgn_ dynMap
>     dhSynh                               = replace dhSynh_ dynMap
>     dhPerc                               = replace dhPerc_ dynMap
>
>     vibeMusic =
>      keysig B Mixolydian
>      $ bandPart dhVibe vibeLine
>
>     percMusic = bandPart dhPerc percLine
>
>     leadMusic = 
>      keysig B Mixolydian
>      $ bandPart dhLead leadLine
>   
>     bassMusic = 
>      keysig B Mixolydian
>      $ bandPart dhBass bassLine
>   
>     organMusic =
>      keysig B Mixolydian
>      $ bandPart dhOrgn organLine
>
>     synthMusic =
>      keysig B Mixolydian
>      $ bandPart dhSynh synthLine
>
>     vibeLine    = times (dhMeasuresAll `div` 2) (addDur qn [ds 5, e 5, ds 5, cs 5, gs 4, fs 4, b 4, cs 5, e 5, cs 5])
>
>     bassLine    = line [times (dhMeasuresIntro - 4)           (rest (5 * qn))
>                       , times (4 + dhMeasuresB1)              (line [b 1 dhn, b 1 hn])
>                       , times dhMeasuresA                     (line [a 1 dhn, a 1 hn])
>                       , times dhMeasuresCs                    (line [cs 2 dhn, cs 2 hn])
>                       , times dhMeasuresB2                    (line [b 1 dhn, b 1 hn])
>                       , times dhMeasuresOutro                 (line [b 1 dhn, b 1 hn])]
>
>     percLineI   = line [
>       line [rest (5 * 5 * qn), rest hn, percm AcousticSnare [5 * qn, qn], rest wn]
>      , t32 [rest en, percm AcousticSnare [en, en, en, qn, qn, en]
>           , chord [line [percm CrashCymbal1 [dqn], percm ClosedHiHat [dqn, dqn, qn], percm OpenHiHat [en]
>                        , percm ClosedHiHat [en, en, en]]
>                  , line [rest qn, percm BassDrum1 [dqn, en, dqn], rest qn, percm BassDrum1 [hn]]]
>           , chord [line [rest en, percm ClosedHiHat [en, en], percm OpenHiHat [en], percm ClosedHiHat [en, en]
>                        , rest dqn, percm ClosedHiHat [dqn], percm ClosedHiHat [en, en], percm OpenHiHat [dqn], percm ClosedHiHat [en]]
>                  , line [rest qn, percm BassDrum1 [en], percm AcousticSnare [dqn] 
>                       ,  rest qn, percm LowTom [en], rest en, percm LowTom [en], percm BassDrum1 [en], rest dhn]]]
>           , chord [line [t32 [percm ClosedHiHat [dqn, qn, en]], percm OpenHiHat [en]
>                        , percm ClosedHiHat [en], percm OpenHiHat [sn], percm ClosedHiHat [en, sn]]
>                  , line [t32 [rest qn, percm BassDrum1 [en, dqn]], rest hn]]
>      , chord [line [percm OpenHiHat [en], percm ClosedHiHat [sn, sn]
>                   , t32 [percm ClosedHiHat [qn, en], percm OpenHiHat [qn], percm ClosedHiHat [en]]
>                   , percm ClosedHiHat [en, en]
>                   , t32 [rest qn, percm CrashCymbal1 [en]]]
>             , line [rest qn, percm BassDrum1 [qn], rest qn, rest den, percm BassDrum1 [sn]
>                   , t32 [percm AcousticSnare [en], percm LowTom [en], percm BassDrum1 [en]]]]]
>
>     leadLineI   = line [rest (11 * 5 * qn), rest wn, rest den, fs 4 tn, gs 4 tn]
>
>     organLine   = line [chord [b 4 (8 * 5 * qn), b 5 (8 * 5 * qn)]]
>
>     synthLine   = line [rest (4 * 5 * qn), chord [grace (-2) (gs 4 (10*qn)), grace (-2) (cs 4 (10*qn))]
>                       , grace (-2) (gs 4 qn), a 4 qn, fs 4 (5*qn), rest dhn
>                       , chord [grace (-2) (gs 4 (9*qn)), grace (-2) (cs 4 (9*qn))], rest en, cs 4 en
>                       , chord [grace (-2) (gs 4 qn), cs 4 qn]
>                       , chord [grace (-2) (a 4 qn), grace (-2) (b 3 qn)]
>                       , chord [fs 4 (7*qn), b 3 (7*qn)]]
>
>     p001_004    = line [p001, p002, p003, p004]
>     l001_004    = line [l001, l002, l003, l004]
>
>     p001        = chord [line [percm CrashCymbal1 [qn], percm ClosedHiHat [en]
>                              , t32 [percm ClosedHiHat [en, sn, qn, en], percm OpenHiHat [qn, qn]
>                                   , percm ClosedHiHat [en, en]]]
>                        , line [percm BassDrum1 [hn], chord [percm AcousticSnare [dhn], percm BassDrum1 [dhn]]]]
>     p002        = chord [line [percm ClosedHiHat [qn, qn], percm OpenHiHat [qn, qn, qn]]
>                        , line [t32 [rest qn, percm BassDrum1 [en]], percm BassDrum1 [dhn]
>                              , percm AcousticSnare [qn]]]
>     p003        = chord [t32 [percm CrashCymbal1 [dqn], percm OpenHiHat [qn, en], percm ClosedHiHat [dqn, en, en]
>                             , percm OpenHiHat [en], percm ClosedHiHat [en, en, en]]
>                        , t32 [rest (dhn + qn), percm BassDrum1 [en], rest dhn]]
>     p004        = chord [t32 [percm ClosedHiHat [qn, en, qn, en, dqn], percm RideCymbal1 [dqn], rest dqn]
>                        , t32 [rest dqn, percm BassDrum1 [qn], percm AcousticSnare [qn, en, en]
>                             , rest en, percm BassDrum1 [en], percm AcousticSnare[en, en], percm BassDrum1 [en, en]]]
>
>     l001        = line [b 4 en, t32 [b 4 en, gs 4 sn], b 4 en, t32 [b 4 en, gs 4 sn], grace (-2) (ds 5 en), rest qn
>                       , t32 [fs 4 en, gs 4 sn], b 4 en, t32 [b 4 en, gs 4 sn]]
>     l002        = line [b 4 en, t32 [b 4 en, gs 4 sn, b 4 en, gs 5 sn], fs 5 dqn, rest qn
>                       , chord [grace 2 (gs 4 (qn + hn + en)), grace 2 (cs 5 (qn + hn + en))], b 4 en]
>     l003        = line [rest en, chord [line [gs 4 en, gs 4 sn, fs 4 en, fs 4 (sn + (5 * qn))]
>                                       , line [cs 5 en, cs 5 sn, b 4 en, b 4 (sn + (5 * qn))]]]
>     l004        = rest 0
>
>     p005_008    = line [p005, p006, p007, p008]
>     l005_008    = line [l005, l006, l007, l008]
>
>     p005        = chord [line [t32 [percm RideCymbal1 [qn, en]], percm OpenHiHat [qn]
>                              , t32 [percm ClosedHiHat [qn, en]], percm OpenHiHat [qn, qn]]
>                        , line [percm BassDrum1 [hn, hn, qn]]]
>     p006        = chord [line [t32 [percm OpenHiHat [qn, en]], rest wn]
>                        , t32 [rest dqn, percm AcousticSnare [sn, den, en, sn, den, en, sn, den, en, qn, sn, sn]]]
>     p007        = chord [t32 [percm CrashCymbal1 [dqn], percm OpenHiHat [dqn], percm ClosedHiHat [dqn, en, en]
>                             , percm OpenHiHat [en], percm ClosedHiHat [en, en, en]]
>                        , line [percm BassDrum1 [hn, dhn]]]
>     p008        = chord [t32 [percm OpenHiHat [qn], percm ClosedHiHat [en, en, en, en], percm OpenHiHat [qn]
>                             , percm ClosedHiHat [en, dqn], rest qn, percm CrashCymbal1 [en]]
>                        , t32 [rest qn, percm BassDrum1 [en, dqn], rest hn, percm HighTom [en], percm LowTom [en]
>                             , percm AcousticSnare [qn], percm BassDrum1 [en]]]
>     l005        = chord [cs 5 (9*qn), line [b 4 wn, rest (5*qn)], line [fs 4 wn, rest (5*qn)], line [b 3 wn, rest (5*qn)]]
>     l006        = line [rest en, fs 4 sn, gs 4 sn]
>     l007        = line [b 4 en, t32 [b 4 en, gs 4 sn], b 4 en, t32 [b 4 en, gs 4 sn], ds 5 en
>                       , ds 5 tn, cs 5 tn, ds 5 tn, cs 5 (tn + qn), ds 5 tn, cs 5 tn, ds 5 tn, cs 5 (tn + qn)]
>     l008        = line [ds 5 tn, cs 5 tn, ds 5 tn, cs 5 (tn + qn), ds 5 tn, cs 5 tn, ds 5 tn, cs 5 (tn + qn)
>                       , ds 5 tn, cs 5 tn, ds 5 tn, cs 5 (tn + qn)]
>
>     p009_012    = line [p009, p010, p011, p012]
>     l009_012    = line [l009, l010, l011, l012]
>
>     p009        = chord [t32 [percm CrashCymbal1 [qn], percm ClosedHiHat [en], percm OpenHiHat [dqn]
>                             , percm ClosedHiHat [qn, en, qn], percm OpenHiHat [en], percm ClosedHiHat [en, en, en]]
>                        , percm BassDrum1 [hn, dhn]]
>     p010        = chord [t32 [percm ClosedHiHat [qn, en, en, en, en, en, en, en], percm OpenHiHat [qn]
>                             , percm ClosedHiHat [en], percm OpenHiHat [qn], percm ClosedHiHat [en]]
>                        , t32 [rest qn, percm BassDrum1 [en, en], rest (qn + (3*dqn)), percm ClosedHiHat []]]
>     p011        = chord [t32 [percm OpenHiHat [qn, en, dqn], percm ClosedHiHat [dqn, en, en]
>                             , percm OpenHiHat [en], percm ClosedHiHat [qn], percm OpenHiHat [en]]
>                        , t32 [rest (5*en), percm BassDrum1 [en, dqn], rest dhn]]
>     p012        = chord [t32 [percm OpenHiHat [qn], percm ClosedHiHat [en, qn, en, qn], percm OpenHiHat [en]
>                             , percm ClosedHiHat [dhn]]
>                        , t32 [rest qn, percm BassDrum1 [en, dqn], rest dqn, rest qn, percm BassDrum1 [en]
>                             , percm AcousticSnare [qn], percm BassDrum1 [en]]]
> -- 6/4
>     l009        = line [ds 5 qn, e 5 tn, t32 [ds 5 tn, cs 5 tn, b 4 tn], gs 4 tn, cs 5 en, b 4 sn
>                       , gs 4 sn, fs 4 (en+hn+qn)]
> -- 5/4
>     l010        = line [rest qn, fs 4 en, t32 [fs 4 en, ds 4 sn], fs 4 en
>                       , fs 4 sn, ds 4 sn, grace (-2) (gs 4 en), fs 4 dqn]
> -- 7/4
>     l011        = line [t32 [grace (-2) (ds 5 qn), fs 5 en], fs 5 hn, fs 5 sn, gs 5 sn, fs 5 sn, gs 5 sn
>                       , t32 [gs 5 qn, fs 5 en], descendFrom dhLead (Fs, 5) Fs chromatic hn]
> -- 2/4
>     l012        = line [rest qn, t32 [ds 5 en, cs 5 en, cs 5 en]]
>
>     p013_016    = line [p013, p014, p015, p016]
>     l013_016    = line [l013, l014, l015, l016]
>
>     p013        = chord [t32 [percm CrashCymbal1 [qn], percm OpenHiHat [en, dqn], percm ClosedHiHat [qn, en, en, en]
>                             , percm OpenHiHat [en], percm ClosedHiHat [en, en], rest en]
>                        , t32 [percm BassDrum1 [dqn], rest qn, percm BassDrum1 [en, dqn], rest (dqn+qn), percm BassDrum1 [en]]]
>     p014        = chord [t32 [percm ClosedHiHat [en, en, en, qn, en, qn, en], percm RideCymbal1 [dqn, dqn]]
>                        , t32 [percm BassDrum1 [qn, en, qn, en, en], percm HighTom [sn, sn], percm LowTom [sn, sn]
>                             , rest en, percm HighTom [sn, sn], percm LowTom [sn, sn]
>                             , rest en, percm HighTom [sn, sn], percm LowTom [sn, sn]]]
>     p015        = chord [t32 [percm CrashCymbal1 [dqn, dqn, qn], percm ClosedHiHat [en, qn, en]
>                             , percm CrashCymbal1 [qn, en]]
>                        , t32 [percm BassDrum1 [dqn], rest qn, percm BassDrum1 [en], percm AcousticSnare [dqn]
>                             , percm BassDrum1 [dqn, dqn]]]
>     p016        = chord [t32 [percm ClosedHiHat [en, en], percm OpenHiHat [en], percm CrashCymbal1 [dqn, qn, en]
>                             , percm RideBell [qn, en], percm ClosedHiHat [en], percm RideBell [en]
>                             , percm ClosedHiHat [en]]
>                        , t32 [rest qn, percm BassDrum1 [en], percm AcousticSnare [dqn], rest dqn
>                             , percm BassDrum1 [dqn], percm AcousticSnare [dqn]]]
>
>     l013        = line [grace (-2) (ds 5 qn)
>                       , t32 [t32 [e 5 sn, ds 5 sn, cs 5 sn], ds 5 en, t32 [ds 5 sn, cs 5 sn, b 5 sn]]
>                       , grace (-2) (ds 5 sn), cs 5 den
>                       , t32 [cs 5 en, cs 5 en, cs 5 sn, b 4 sn]
>                       , t32 [ds 5 en, cs 5 sn], tempo (5/4) (line [cs 5 sn, ds 5 tn, cs 5 tn, b 4 tn])]
>     l014        = line [t32 [cs 5 en, cs 5 sn], b 4 dqn, rest den, b 5 sn
>                       , t32 [a 5 en, a 5 en, t32 [grace 1 (gs 5 en), gs 5 sn]], fs 5 en
>                       , t32 [gs 5 tn, fs 5 tn, e 5 tn], e 5 sn]
>     l015        = line [e 5 sn, grace (-2) (fs 5 en), fs 5 (sn + dqn), rest en, ds 6 sn, ds 6 tn, b 5 tn
>                       , t32 [fs 5 en, b 5 sn], a 5 qn]
>     l016        = line [t32 [chord [b 4 sn, fs 5 sn], e 5 den, chord [b 4 en, e 5 en]]
>                       , t32 [b 4 sn, a 4 en, e 4 en, ds 4 sn]
>                       , e 4 sn, ds 4 tn, e 4 tn, ds 4 tn, e 4 tn, ds 4 tn, b 3 tn, a 3 dhn]
>
>     p017_020    = line [p017, p018, p019, p020]
>     l017_020    = line [l017, l018, l019, l020]
>
>     p017        = chord [t32 [percm RideBell [dqn, dqn, qn, en], rest en, percm RideBell [qn, dqn]]
>                        , t32 [percm BassDrum1 [dqn], rest qn, percm BassDrum1 [en, dqn], rest qn
>                             , percm BassDrum1 [en, dqn]]]
>     p018        = chord [line [t32 [percm RideBell [dqn, qn], percm ClosedHiHat [en]], percm CrashCymbal1 [en]
>                             , percm OpenHiHat [sn], percm ClosedHiHat [sn], percm CrashCymbal1 [qn]
>                             , t32 [percm ClosedHiHat [qn, en]]]
>                        , t32 [rest qn, percm BassDrum1 [en], percm AcousticSnare [dqn]
>                             , rest dqn, percm BassDrum1 [dqn], percm AcousticSnare [dqn]]]
>     p019        = chord [t32 [percm CrashCymbal1 [qn, qn], percm OpenHiHat [qn] 
>                             , percm OpenHiHat [qn], percm CrashCymbal1 [qn], percm OpenHiHat [qn]]
>                        , t32 [percm BassDrum1 [hn], rest en, percm BassDrum1 [en], percm AcousticSnare [dqn]
>                             , rest qn, percm BassDrum1 [en + hn], rest en, percm BassDrum1 [en]]]
>     p020        = chord [t32 [percm ClosedHiHat [dqn, qn, en], percm OpenHiHat [qn, qn, qn]]
>                        , t32 [percm AcousticSnare [dqn], rest qn, percm BassDrum1 [en]
>                             , rest (hn + en), percm BassDrum1 [en]]]
>
>     l017        = line [rest qn, b 6 (qn + den), fs 4 tn, gs 4 tn
>                       , tempo (5/2) (line [times 4 (chord [a 4 en, a 5 en]), rest en])]
>     l018        = line [t32 [chord [ line [a 4 en, b 4 sn, gs 4 en, gs 4 sn], line [a 5 en, b 5 sn, gs 5 en, gs 5 sn]]]
>                       , chord [line [a 4 sn, fs 4 sn, fs 5 sn, a 4 sn], line [a 5 sn, fs 5 sn, b 5 sn, rest sn]]
>                       , a 5 sn, gs 5 sn, fs 5 sn, fs 5 sn, fs 5 en, e 6 en
>                       , chord [line [gs 5 sn, gs 5 sn, fs 5 sn, b 5 sn], line [ds 6 sn, ds 6 sn, b 5 sn, rest sn]]] 
>     l019        = line [t32 [b 5 en, cs 6 qn], cs 6 sn, cs 6 sn, cs 6 sn, ds 6 sn
>                       , tempo (7/4) (line [ds 6 sn, cs 6 sn, b 5 sn, cs 6 sn, b 5 sn, cs 6 sn, b 5 sn])
>                       , grace (-2) (fs 5 sn), fs 5 sn, ds 5 sn, grace (-1) (f 5 sn), f 5 sn, d 5 tn, cs 5 tn
>                       , t32 [e 5 sn, d 5 sn, rest sn]]
>     l020        = line [rest en, t32 [d 5 sn, cs 5 sn, ds 5 sn, cs 5 qn, b 4 en]
>                       , rest en, t32 [ds 5 sn, cs 5 sn, ds 5 sn, cs 5 qn, b 4 en
>                       , grace (-2) (ds 5 en), cs 5 sn, cs 5 sn, e 5 sn, cs 5 sn]]
>
>     p021_024    = line [p021, p022, p023, p024]
>     l021_024    = line [l021, l022, l023, l024]
>
>     p021        = chord [t32 [percm RideBell [dqn, qn, en], percm CrashCymbal1 [qn, qn, qn], rest en
>                             , percm CrashCymbal1 [qn]]
>                        , t32 [percm BassDrum1 [dqn], rest qn, percm BassDrum1 [en], percm AcousticSnare [hn + en]
>                             , percm BassDrum1 [en, dqn]]]
>     p022        = chord [line [t32 [percm OpenHiHat [qn], percm ClosedHiHat [en]], percm CrashCymbal1 [qn + sn, den]
>                              , rest qn, percm OpenHiHat [en, en]]
>                        , line [t32 [rest qn, percm BassDrum1 [en]], percm AcousticSnare [den, sn, en]
>                              , percm BassDrum1 [en], percm AcousticSnare [sn], percm BassDrum1 [sn, en], rest sn
>                              , percm BassDrum1 [sn], rest sn, percm BassDrum1 [sn]]]
>     p023        = chord [line [percm CrashCymbal1 [qn], percm RideCymbal1 [qn], percm CrashCymbal1 [qn]
>                              , percm RideCymbal1 [qn], percm CrashCymbal1 [qn]]
>                        , line [chord [percm BassDrum1 [dqn], percm AcousticSnare [dqn]], percm BassDrum1 [sn, sn]
>                              , percm AcousticSnare [dqn], percm BassDrum1 [en, qn]]]
>     p024        = chord [line [percm CrashCymbal1 [qn, qn], percm OpenHiHat [qn], rest en
>                              , percm CrashCymbal1 [dqn]]
>                        , line [percm BassDrum1 [en, sn, sn], percm AcousticSnare[qn], rest sn
>                              , percm BassDrum1 [sn], percm AcousticSnare[en, sn, den, sn, sn]
>                              , percm BassDrum1 [sn, sn]]]
>
>     l021        = line [tempo (5/4) (line [b 4 en, fs 4 sn, chord [line [fs 4 sn, fs 4 sn], line [b 4 sn, b 4 sn]]])
>                       , chord [line [fs 4 dqn, rest dqn, t32 [gs 4 qn, grace (-2) (gs 4 en)]]
>                              , line [ b 4 dqn, rest dqn, t32 [cs 5 qn, grace (-2) (cs 5 en)]]]]
>     l022        = line [chord [fs 4 dhn, b 4 dhn], rest qn, fs 6 tn, b 5 tn, rest den]
>     l023        = line [ds 5 den, b 4 sn, addDur sn [cs 5, b 4, b 4, fs 5,    fs 5, gs 5, gs 5, a 5
>                                                    ,  a 5, b 5,  b 5, cs 6], cs 6 en, cs 6 en]
>     l024        = line [e 5 en, ds 5 en, ds 5 qn, t32 [ds 5 qn, e 5 en], e 5 en, ds 5 en, ds 5 qn]
>
>     p025_028    = line [p025, p026, p027, p028]
>     l025_028    = line [l025, l026, l027, l028]
>
>     p025        = chord [line [percm CrashCymbal1 [qn], percm RideCymbal1 [en, sn, sn, qn, den, sn, den, sn]]
>                        , line [chord [percm BassDrum1 [dqn], percm AcousticSnare [dqn]], percm BassDrum1 [sn, sn]
>                              , percm AcousticSnare [dqn], percm BassDrum1 [en, qn]]]
>     p026        = chord [line [percm RideCymbal1 [qn, hn, sn], percm OpenHiHat [den + qn]]
>                        , line [percm BassDrum1 [den, sn], percm AcousticSnare [dqn], percm BassDrum1 [sn, en, en]
>                              , percm AcousticSnare [sn]
>                              , chord [percm LowTom [sn, sn, sn, sn], percm AcousticSnare [sn, sn, sn, sn]]]]
>     p027        = chord [line [rest en, percm RideCymbal1 [en], percm CrashCymbal1 [qn, qn, en, en, den, sn]]
>                        , line [chord [percm AcousticSnare [sn, sn], percm LowTom [sn, sn]]
>                              , chord [percm AcousticSnare [en, qn], percm BassDrum1 [en, den, sn]]
>                              , percm AcousticSnare [dqn], percm BassDrum1 [sn, sn, qn]]]
>     p028        = chord [line [percm CrashCymbal1 [den, sn, den, sn, den, sn, den, sn]
>                              , tempo (5/4) (percm CrashCymbal1 [den, en])]
>                        , line [rest en, percm BassDrum1 [sn, sn], percm AcousticSnare [sn, sn]
>                              , percm BassDrum1 [sn, sn], percm AcousticSnare [qn, en], percm BassDrum1 [sn, sn]
>                              , tempo (5/4) (line [rest sn, percm BassDrum1 [sn], percm AcousticSnare [sn]
>                                                 , percm BassDrum1 [sn], percm AcousticSnare [sn]])]]
>
>     l025        = line [ds 5 hn, chord [line [grace (-1) (e 6 qn), descendFrom dhLead (E, 6) E chromatic qn]
>                                ,        line [grace (-2) (b 5 qn), descendFrom dhLead (B, 5) B chromatic qn]]
>                       , chord [addDur sn [ds 5, e 5, e 5, gs 5]
>                       ,        addDur sn [fs 4, a 4, a 4, cs 5]]]
> -- 6/4
>     l026        = line [chord [line [cs 5 sn, b 4 den], line [fs 5 sn, e 5 sn, e 5 sn, fs 5 sn]]
>                       , addDur sn [fs 5, gs 5, gs 5, a 5, a 5, b 5, b 5, cs 6]
>                       , t32 [grace (-3) (e 6 sn), cs 6 sn, e 6 sn], cs 6 (en + hn)]
> -- 5/4
>     l027        = line [e 5 qn, ds 5 en, b 4 en, addDur sn [a 4, e 4, ds 4, b 3], a 3 dqn, b 3 en]
> -- 4/4
>     l028        = line [b 3 qn, t32 [rest en, addDur en [fs 4, a 4, a 4, cs 5, cs 5, e 5, fs 5, fs 5]]]
>
>     p029_032    = line [p029, p030, p031, p032]
>     l029_032    = line [l029, l030, l031, l032]
>
>     p029        = chord [line [percm CrashCymbal1 [qn, den, sn, en, sn, sn, en, en, en, en]]
>                        , line [percm BassDrum1 [dqn, sn, sn], percm AcousticSnare [dqn]
>                              , percm BassDrum1 [en], percm AcousticSnare [qn]]]
>     p030        =
>       t32 [percm BassDrum1 [sn, sn], percm LowTom [sn, sn], percm BassDrum1 [sn], percm AcousticSnare [sn, sn]
>          , percm LowFloorTom [sn, sn], percm AcousticSnare [sn], percm LowFloorTom [sn], percm BassDrum1 [sn]
>          , percm AcousticSnare [sn], percm LowFloorTom [sn], percm BassDrum1 [sn], rest sn, percm BassDrum1 [sn]
>          , percm LowTom [sn, sn], percm BassDrum1 [sn], percm LowTom [sn, sn], percm BassDrum1 [sn]
>          , percm AcousticSnare [sn], percm BassDrum1 [sn], percm AcousticSnare [sn], percm BassDrum1 [sn]
>          , percm AcousticSnare [sn], percm BassDrum1 [sn], rest sn]
>     p031        = chord [line [percm CrashCymbal1 [qn], percm RideCymbal1 [den, sn, den, sn, den, sn, den, sn]]
>                        , line [percm BassDrum1 [dqn, sn, sn], percm AcousticSnare [dqn]
>                              , percm BassDrum1 [en, den, sn]]]
>     p032        = chord [line [percm RideCymbal1 [den, sn, den, en], percm OpenHiHat [en, en, en, en, en, sn]]
>                        , line [percm BassDrum1 [en, en], percm AcousticSnare [den, en, en, en, en, en, en, sn]]]
>
>     l029        = line [addDur sn [gs 5, a 5, a 5, b 5], t32 [addDur en [b 5, cs 6, cs 6]], e 6 en, cs 6 en
>                       , grace 4 (gs 5 sn), e 5 sn, ds 5 en, gs 5 sn, e 5 sn, grace (-1) (ds 5 sn), b 4 sn]
>     l030        = line [chord [line [e 5 sn, fs 5 sn, fs 5 sn, gs 5 sn], b 4 qn], line [a 5 en, a 5 qn, a 5 en]
>                       , line [fs 6 sn, b 5 sn, b 5 sn, a 5 sn, a 5 sn, gs 5 sn, addDur tn [gs 5, a 5, gs 5, fs 5]]]
>     l031        = line [grace (-2) (fs 5 qn), e 5 en, addDur tn [fs 5, gs 5, fs 5, gs 5]
>                       , chord [line [fs 5 dqn, rest en], b 5 hn], t32 [rest en, e 6 en, e 6 en]]
>     l032        = t32 [e 6 en, ds 6 sn, e 6 en, ds 6 sn, grace (-2) (fs 6 (dqn + qn)), e 6 en, grace 3 (b 5 dqn)
>                      , chord [d 4 dqn, g 4 dqn, b 4 dqn]]
>
>     p033_036    = line [p033, p034, p035, p036]
>     l033_036    = line [l033, l034, l035, l036]
>
>     p033        = chord [line [percm CrashCymbal1 [qn], percm RideCymbal1 [qn, dden, tn+den]
>                              , percm CrashCymbal1 [sn+qn]]
>                        , line [percm BassDrum1 [dqn], t32 [percm BassDrum1 [en, sn]], percm AcousticSnare [dden]
>                              , percm BassDrum1 [tn + en], percm AcousticSnare [tn, tn, sn + den]
>                              , percm BassDrum1 [sn]]]
>     p034        = chord [line [rest sn, percm CrashCymbal1 [den, den, den], percm RideCymbal1 [sn + qn]
>                              , percm CrashCymbal1 [sn + qn]]
>                        , line [percm BassDrum1 [sn], percm AcousticSnare [sn]
>                              , percm BassDrum1 [sn, sn], percm AcousticSnare [sn, sn]
>                              , percm BassDrum1 [sn], percm AcousticSnare [tn, tn], rest den, percm AcousticSnare [sn]
>                              , percm BassDrum1 [en, sn], percm AcousticSnare [sn], t32 [percm BassDrum1 [sn]
>                              , percm HighTom [sn, sn]], percm LowTom [sn], percm BassDrum1 [sn]]]
>     p035        = chord [line [percm CrashCymbal1 [qn, den], percm OpenHiHat [sn], percm CrashCymbal1 [den, sn]
>                              , rest dqn, percm CrashCymbal1 [sn, sn]]
>                        , line [percm BassDrum1 [dqn, sn, sn], percm AcousticSnare [den, sn], rest en
>                              , percm AcousticSnare [sn, sn, sn, sn], rest tn, percm AcousticSnare [tn]
>                              , rest tn, percm AcousticSnare [tn]]]
>     p036        =
>       chord [line [t32 [percm CrashCymbal1 [en, sn]], rest en, percm CrashCymbal1 [qn, dsn], percm OpenHiHat [tn]
>                  , rest en, percm CrashCymbal1 [qn], tempo (5/4) (line [rest den, percm CrashCymbal1 [en]])]
>            , line [t32 [percm BassDrum1 [sn], percm LowFloorTom [sn], percm BassDrum1 [sn]]
>                  , percm LowFloorTom [tn, tn, tn, tn], chord [percm BassDrum1 [qn], percm AcousticSnare [qn]]
>                  , percm LowFloorTom [tn, tn, tn, tn, en]
>                  , chord [percm BassDrum1 [dden, tn], percm AcousticSnare [qn]]
>                  , tempo (5/4) (line [percm AcousticSnare [sn, sn, sn], percm BassDrum1 [sn], percm LowFloorTom [sn]])]]
>
> -- 7/4
>     l033        =
>       line [t32 [chord[line [b 5 en, fs 5 qn], line [b 4 dqn]], grace 2 (e 5 en), e 5 sn, fs 5 sn
>                , t32 [e 5 sn, ds 5 sn, e 5 sn]]
>           , grace 1 (ds 5 dden), chord [ds 5 tn, b 5 tn]
>           , tempo (7/8) (line [b 5 sn, fs 5 en, chord [e 5 en, e 6 en], fs 5 den, t32 [b 5 sn, ds 5 en]
>                        , e 5 tn, ds 5 tn, e 5 tn, ds 5 tn, ds 5 tn, cs 5 sn, cs 5 tn])]
> -- 3/4
>     l034        =
>       line [cs 5 den, cs 5 sn, cs 5 sn, b 4 sn, b 4 en, chord [cs 5 sn, grace (-2) (gs 5 sn)], gs 5 sn, fs 5 en]
>     l035        = line [grace (-2) (gs 4 qn), fs 4 hn, addDur sn [a 4, b 4, b 4, cs 5, a 4, b 4, b 4, cs 5]]
>     l036        =
>       line [addDur tn [b 4, cs 5, b 4, a 4], b 4 en, grace (-2) (cs 5 sn), ds 5 sn, cs 5 sn, b 4 sn
>             , cs 5 sn, fs 4 sn, gs 4 sn, fs 4 (sn + hn)]
>
>     p037_040    = line [p037, p038, p039, p040]
>     l037_040    = line [l037, l038, l039, l040]
>
>     p037        =
>       chord [line [percm CrashCymbal1 [qn], chord [percm OpenHiHat [qn], percm CrashCymbal1 [qn]]
>                  , percm CrashCymbal1 [hn, den, sn]]
>            , line [percm BassDrum1 [dqn, sn, sn], percm AcousticSnare [dqn], percm BassDrum1 [sn, sn]
>                  , percm AcousticSnare [en], percm BassDrum1 [sn], percm AcousticSnare [sn]]]
>     p038        =
>       line [rest sn, chord [percm CrashCymbal1 [en, sn], percm OpenHiHat [en, sn]]
>             , t32 [times 2 (line [percm HighTom [sn], percm LowTom [sn], percm BassDrum1 [sn]])
>                  , percm LowFloorTom [sn], percm AcousticSnare [en]]
>             , percm AcousticSnare [en], percm LowTom [en], percm HighTom [qn], percm AcousticSnare [en]]
>     p039        =
>       chord [line [percm CrashCymbal1 [qn, qn, qn, qn, den, sn]]
>            , line [percm BassDrum1 [qn, en, sn, sn], percm AcousticSnare [hn], percm BassDrum1 [qn]]]
>     p040        =
>       chord [line [percm CrashCymbal1 [qn, den], percm ClosedHiHat [sn], percm OpenHiHat [qn]
>                  , percm CrashCymbal1 [en, sn, sn, en, en]]
>            , line [rest en, percm BassDrum1 [sn, sn], percm AcousticSnare [dqn, sn]
>                  , percm BassDrum1 [sn, en, sn, sn, en, en]]]
>
>     l037        =
>       line [t32 [ds 4 sn, gs 4 sn, ds 4 sn], ds 4 (en + hn), rest qn
>           , grace (-2) (fs 4 sn), gs 4 sn, gs 4 sn, a 4 sn]
>     l038        =
>       line [a 4 sn, b 4 sn, addDur tn [b 4, cs 5, b 4, a 4]
>           , addDur sn [b 4, fs 4, a 4, fs 4, b 4, cs 5, cs 5, ds 5, e 5]
>           , fs 5 en, e 5 (sn + qn)]
>     l039        =
>       line [grace (-2) (cs 5 sn), b 4 en, a 4 sn, t32 [a 4 sn, b 4 sn, a 4 sn]
>           , gs 4 sn, a 4 sn, gs 4 sn, fs 4 sn, a 4 sn, b 4 sn, t32 [cs 5 sn, b 4 sn, a 4 sn]
>           , gs 4 sn, a 4 sn, gs 4 sn, fs 4 en, a 4 sn]
>     l040        =
>       line [gs 4 sn, fs 4 sn, t32 [addDur sn [a 4, gs 4, fs 4, a 4, gs 4, fs 4]]
>           , tempo (5/4) (line [a 4 sn, a 4 tn, gs 4 tn, fs 4 tn]), fs 4 hn
>           , addDur sn [a 4, cs 5, cs 5, a 4]]
>
>     p041_044    = line [p041, p042, p043, p044]
>     l041_044    = line [l041, l042, l043, l044]
>
>     p041        =
>       chord [line [percm CrashCymbal1 [en, en], rest (hn + sn), percm CrashCymbal1 [den], rest qn]
>            , line [percm AcousticSnare [en, en]
>                  , t32 [times 3 (line [percm HighTom [sn], percm LowTom [sn], percm BassDrum1 [sn]])]
>                  , percm LowTom [sn], percm AcousticSnare [sn], rest sn, percm BassDrum1 [en, sn]
>                  , t32 [percm HighFloorTom [sn], percm AcousticSnare [sn], rest qn]]]
>     p042        =
>       chord [line [tempo (5/4) (line [percm CrashCymbal1 [dqn, qn]]), rest dden
>                  , percm CrashCymbal1 [tn, en, en, en, en]]
>            , line [tempo (5/4) (line [rest en, percm AcousticSnare [en, qn, en]]), percm AcousticSnare [dden]
>                  , percm BassDrum1 [tn, en, en], percm AcousticSnare [en], percm BassDrum1 [sn]
>                  , percm AcousticSnare [sn]]]
>     p043        =
>       chord [line [percm CrashCymbal1 [qn], percm RideCymbal1 [en, sn, sn, en, en], percm CrashCymbal1 [qn, qn]]
>            , line [percm BassDrum1 [den, sn, en, sn, sn], percm AcousticSnare [den]
>                  , percm BassDrum1 [sn, dden, tn, dden, tn]]]
>     p044        =
>       chord [line [percm CrashCymbal1 [qn, qn, qn, qn, qn]]
>            , line [percm BassDrum1 [dden, tn, dden, tn, dden, tn, dden, tn, dden, tn]]]
>
>     l041        =
>       line [b 4 dhn, rest sn, grace (-2) (fs 5 sn), tempo (5/4) (addDur tn [fs 5, gs 5, fs 5, e 5, fs 5])
>           , a 5 sn, grace (-1) (a 5 sn), a 5 en]
>     l042        =
>       line [t32 [a 5 en, grace (-2) (gs 5 qn), a 5 qn, cs 5 en], e 6 den, cs 6 tn, ds 6 tn, e 6 en, ds 6 dqn]
>     l043        =
>       line [ds 5 en, e 5 en, e 5 en, ds 5 sn, ds 5 en, grace (-1) (e 5 en), e 5 sn, grace (-2) (gs 5 en), gs 5 en
>           , t32 [gs 5 sn, a 5 sn, gs 5 sn], fs 5 sn, gs 5 sn]
>     l044        =
>       line [grace (-2) (b 5 (hn + en)), b 5 en, t32 [addDur en [b 5, a 5, a 5, a 5, b 5, a 5]]]
>
>     p045_048    = line [p045, p046, p047, p048]
>     l045_048    = line [l045, l046, l047, l048]
>
>     p045        =
>       chord [line [percm CrashCymbal1 [qn], percm RideCymbal1 [qn, den, sn, en, en, en, en]]
>            , line [percm BassDrum1 [dqn, sn, sn], percm AcousticSnare [den], percm BassDrum1 [sn, en, en]
>                  , percm AcousticSnare [en], percm BassDrum1 [sn], percm AcousticSnare [sn]]]
>     p046        =
>       chord [tempo (5/4) (line [percm RideCymbal1 [den, qn + sn, en, en, den, en, den, en, den]])
>            , tempo (5/4) (times 25 (percm AcousticSnare [sn]))
>            , tempo (5/4) (line [percm BassDrum1 [den, qn + sn, en, en, den, en, den, en, den]])]
>     p047        =
>       chord [line [percm CrashCymbal1 [qn], percm RideCymbal1 [en, sn, sn, qn, qn, qn]]
>            , line [percm BassDrum1 [dqn, sn, sn], percm AcousticSnare [dqn], percm BassDrum1 [en, dden, tn]]]
>     p048        =
>       chord [line [percm RideCymbal1 [den, sn, qn, qn], rest hn]
>            , line [times 3 (line [percm BassDrum1 [en, sn], percm AcousticSnare [sn]])
>                  , times 2 (line [percm BassDrum1 [sn, sn], percm AcousticSnare [sn], percm BassDrum1 [sn]])]]
>
>     l045        =
>       line [chord [line [grace (-4) (ds 6 en), ds 6 en], line [e 5 en, e 5 en]], rest sn, a 5 en, a 5 sn, a 5 sn
>           , b 5 en, ds 6 sn, t32 [ds 6 en, a 5 sn, a 5 en, a 5 sn], b 5 en, grace (-4) (ds 6 en)]
>     l046        =
>       line [grace (-4) (ds 6 en), chord [e 5 en, b 5 en]
>           , t32 [e 5 en, a 5 en, a 5 en, grace (-2) (b 5 en), grace 3 (gs 5 en), fs 5 en], fs 5 dqn, rest en]
>     l047        =
>       t32 [trill 1 tn (ds 5 dqn), ds 5 en, rest en, a 3 en, e 5 en, ds 5 en, ds 5 en, e 5 en, ds 5 en, e 5 en
>          , grace (-2) (fs 5 en), descendFrom dhLead (Fs, 5) Fs chromatic qn]
>     l048        =
>       line [rest en, grace (-4) (ds 6 en), b 5 sn, a 5 sn, a 3 sn, gs 5 sn, chord [e 5 sn, a 5 sn], fs 5 sn
>           , b 5 sn, gs 5 sn, a 5 sn, b 5 sn, ds 6 sn, cs 6 sn, cs 5 tn, fs 4 tn, rest sn, e 6 sn, e 6 sn]
>
>     p049_052    = line [p049, p050, p051, p052]
>     l049_052    = line [l049, l050, l051, l052]
>
> -- 4/4
>     p049        =
>       chord [line [percm CrashCymbal1 [qn], percm RideBell [en, sn, sn + den, sn, en, en]]
>            , line [percm BassDrum1 [dqn, sn, sn], percm AcousticSnare [dqn], percm BassDrum1 [en]]]
> -- 3/4
>     p050        =
>       chord [line [tempo (2/3) (t32 [percm CrashCymbal1 [dqn], rest den, percm CrashCymbal1 [den]])]
>            , line [tempo (2/3) (t32 [percm BassDrum1 [en], percm AcousticSnare [sn], percm LowTom [en, sn]
>                                    , percm AcousticSnare [en, sn], chord [percm BassDrum1 [sn], percm LowTom [sn]]
>                                    , percm AcousticSnare [sn, sn]])]]
> -- 8/4
>     p051        =
>       chord [line [t32 [percm CrashCymbal1 [qn, en]], rest en
>                  , percm CrashCymbal1 [5 * sn, sn, qn, qn, qn, en, en, den, sn]]
>            , line [t32 [chord [percm BassDrum1 [qn], percm AcousticSnare [qn]], percm BassDrum1 [en]]
>                  , percm HighFloorTom [en], chord [percm BassDrum1 [sn], percm HighFloorTom [sn]]
>                  , percm HighFloorTom [sn, sn, sn, sn], chord [percm BassDrum1 [sn], percm HighFloorTom [sn]]
>                  , chord [percm BassDrum1 [dqn], percm HighFloorTom [dqn]], percm BassDrum1 [sn, sn]
>                  , percm AcousticSnare [en], percm BassDrum1 [en, en, en]
>                  , percm AcousticSnare [den], percm BassDrum1 [sn]]]
>     p052        =
>       chord [line [rest sn, percm CrashCymbal1 [sn, sn, sn, en], percm OpenHiHat [qn], percm CrashCymbal1 [dqn, qn]]
>            , line [rest sn, percm AcousticSnare [sn, sn], percm BassDrum1 [sn], percm AcousticSnare [sn]
>                  , percm BassDrum1 [sn], percm LowTom [en, en], percm BassDrum1 [en], percm LowTom [dden]
>                  , percm AcousticSnare [tn], percm BassDrum1 [sn], percm LowTom [den]]]
>
> --
>     l049        =
>       line [e 6 en, ds 6 qn, ds 6 en, b 5 en, a 5 en, b 5 sn, gs 5 en, gs 5 sn]
> -- 3/4
>     l050        =
>       line [tempo (2/3) (t32 [fs 5 en, fs 5 sn, gs 5 en, gs 5 sn
>                             , chord [line [fs 5 en, fs 5 sn, b 5 en, b 5 sn]
>                                    , line [e 6 en, rest sn, ds 6 en, ds 6 sn]]])]
> -- 8/4
>     l051        =
>       line [chord [fs 5 sn, b 5 sn], addDur sn [cs 5, a 4, fs 4, e 4, a 4, fs 4, e 4, e 4, ds 4, b 3]
>           , a 3 (sn + hn + en), b 4 en, b 5 sn, d 6 en, a 5 sn, fs 5 sn, grace (-2) (fs 5 en), fs 5 sn]
>     l052        =
>       line [grace (-2) (fs 5 sn), a 5 sn, fs 5 sn, grace (-2) (fs 5 sn), e 5 sn, d 5 en, b 4 sn, b 4 hn, rest qn]
>
>     p053_056    = line [p053, p054, p055, p056]
>     l053_056    = line [l053, l054, l055, l056]
>
>     p053        =
>       chord [line [percm CrashCymbal1 [en, en], percm OpenHiHat [en], percm CrashCymbal1 [en]
>                  , tempo (7/4) (line [rest (3 * sn), percm RideCymbal1 [sn], rest (3 * sn), percm RideCymbal1 [sn]
>                  , rest (3 * sn), percm RideCymbal1 [sn], rest en]), percm CrashCymbal1 [qn]]
>            , line [percm BassDrum1 [en, en], percm AcousticSnare [en]
>                  , t32 [percm BassDrum1 [en], percm AcousticSnare [sn]]
>                  , tempo (7/4) (line [times 3 (line [percm HighTom [sn], percm LowTom [sn]
>                                                    , percm AcousticSnare [sn], percm BassDrum1 [sn]])
>                                     , percm HighTom [sn], percm LowTom [sn]])
>                  , t32 [percm BassDrum1 [en, en, en]]]]
>     p054        =
>       chord [line [percm RideCymbal1 [qn, en, en, en, en, en, en, en], t32 [percm RideCymbal1 [en, sn]]]
>            , line [t32 [percm AcousticSnare [en], times 2 (line [percm BassDrum1 [sn], percm LowTom [sn]])]
>                  , percm BassDrum1 [en, en, en, en, en, en, en], t32 [percm BassDrum1 [en, sn]]]]
>            
>     p055        =
>       t32 [times 12 (line [chord [percm BassDrum1 [sn], percm RideCymbal1 [sn]], percm AcousticSnare [sn]])
>          , chord [line [percm BassDrum1 [den, en, sn]], line [percm CrashCymbal1 [den, den]]]]
>     p056        =
>       t32 [chord [line [percm CrashCymbal1 [den, den, dqn, den, den], percm RideBell [den, den + en, qn]]
>                 , line [chord [percm BassDrum1 [den, en], percm AcousticSnare [den, en]], percm BassDrum1 [sn]
>                       , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn]
>                       , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn]
>                       , chord [percm BassDrum1 [sn, sn], percm AcousticSnare [sn, sn]], percm AcousticSnare [sn, sn]
>                       , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn]
>                       , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn]
>                       , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn]
>                       , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn, sn, sn, sn]]]]
>
>     l053        =
>       t32 [e  5 en, e  5 en, t32 [e 5 sn, ds 5 sn, cs 5 sn], ds 5 en, grace 3 (c 5 en), c 5 en, grace 4 (c 5 en)
>          , cs 5 en, ds 5 qn, c 5 en, cs 5 en, e 5 en, e 5 en, t32 [e 5 sn, ds 5 sn, c 5 sn]]
> -- 4/4
>     l054        =
>       t32 [ds 5 en, grace 3 (c 5 en), c 5 en, c 5 en, cs 5 en, ds 5 en, ds 5 en, grace 3 (c 5 en), cs 5 en
>          , e  5 en, e 5 en, t32 [e 5 sn, ds 5 sn, c 5 sn]]
> -- 5/4
>     l055        =
>       line [tempo (4/5) (t32 [ds 5 sn, ds 5 sn, cs 5 en, cs 5 en, c 5 en, cs 5 en, ds 5 en, grace (-1) (e 5 en)
>                        , ds 5 en, cs 5 en, ds 5 en, e 5 en, es 5 en])]
> -- 6/4
>     l056        =
>       line [fs 5 den, fs 5 sn, fs 5 sn, fs 5 tn, e 5 tn, chord [line [b 4 sn, b 4 sn], line [ds 5 sn, ds 5 sn]]
>           , ds 5 sn, e 5 sn, ds 5 en, tempo (5/4) (line [ds 5 sn, ds 5 tn, e 5 tn, ds 5 tn]), ds 5 sn, b 4 sn
>           , cs 5 qn, descendFrom dhLead (Cs, 5) Cs chromatic qn]
>
>     p057_060    = line [p057, p058, p059, p060]
>     l057_060    = line [l057, l058, l059, l060]
>
>     p057        =
>       chord [t32 [percm CrashCymbal1 [en, qn, en, den, sn, dqn], percm ClosedHiHat [en, en, en, en, en, en]]
>            , t32 [chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn]
>                 , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn]
>                 , chord [percm BassDrum1 [sn, sn], percm AcousticSnare [sn, sn]], percm AcousticSnare [sn]
>                 , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn]
>                 , chord [percm BassDrum1 [sn, sn], percm AcousticSnare [sn, sn]], percm AcousticSnare [sn, sn, sn, sn, sn]
>                 , rest dhn]]
>
>     p058        =
>       chord [t32 [chord [percm CrashCymbal1 [en], percm ClosedHiHat [en]]
>                  , percm ClosedHiHat [en, en, en, en, en, en, en, en, en, en, en], percm CrashCymbal1 [den, den]]
>            , t32 [percm BassDrum1 [den, en, sn], percm AcousticSnare [en], percm LowTom [sn, sn]
>                 , chord [percm BassDrum1 [sn], percm LowTom [sn]], percm LowTom [sn]
>                 , chord [percm BassDrum1 [sn], percm LowTom [sn]], percm LowTom [sn, sn]
>                 , chord [percm BassDrum1 [sn], percm LowTom [sn]], percm LowTom [sn]
>                 , chord [percm BassDrum1 [sn], percm LowTom [sn]], percm AcousticSnare [sn]
>                 , percm LowTom [sn, sn, sn, sn, sn], percm BassDrum1 [den, den]]]
>     p059        =
>       chord [line [percm CrashCymbal1 [en, en, en, en, en, en, en, en], rest qn]
>            , line [percm BassDrum1    [en, en, en, en, en, en, en, en]
>                  , t32 [percm AcousticSnare [sn, sn, sn, sn, sn, sn]]]]
>     p060        =
>       chord [line [percm CrashCymbal1 [qn, qn, qn, qn, qn]]
>            , line [percm BassDrum1 [en, en], t32 [percm BassDrum1 [en], percm HighTom [en], percm BassDrum1 [en]]
>                  , percm BassDrum1 [en, en], chord [percm BassDrum1 [en], percm LowTom [en]]
>                  , percm BassDrum1 [en, en, en]]]
>
>     l057        =
>       line [grace (-2) (cs 5 en), cs 5 en, t32 [ds 5 sn, e 5 tn, ds 5 tn, addDur en [cs 5, cs 5, cs 5, cs 5, cs 5]]
>           , cs 5 sn, ds 5 tn, cs 5 tn, bs 4 qn, cs 5 en]
>     l058        =
>       line [cs 5 qn, ds 5 tn, cs 5 tn, bs 4 (den + en), grace (-1) (a 4 en), t32 [gs 4 qn, bs 4 en]
>           , cs 5 sn, ds 5 tn, cs 5 tn, bs 4 sn, bs 4 sn]
>     l059        =
>       line [cs 5 sn, e 5 sn, t32 [b 4 tn, a 4 tn, gs 4 tn], fs 5 sn, t32 [gs 5 en, fs 5 sn], gs 5 en, fs 5 en
>           , descendFrom dhLead (Fs, 5) Fs chromatic qn, grace (-5) (b 5 dqn)]
>     l060        =
>       t32 [b  5 en, b 5 en, b 5 en, b 5 sn, fss 5 sn, gs 5 en, gs 5 en, gs 5 en, t32 [b 5 sn, a 5 sn, gs 5 sn]
>          , gs 5 en, gs 5 en, gs 5 en, b 5 sn, a 5 sn, gs 5 en, gs 5 en, b 5 en]
>
>     p061_064    = line [p061, p062, p063, p064]
>     l061_064    = line [l061, l062, l063, l064]
>
>     p061        =
>       chord [
>         line [percm CrashCymbal1 [qn, qn, qn], tempo (5/4) (line [percm CrashCymbal1 [en, en, en, en, en]])]
>       , line [percm BassDrum1 [en, en]
>             , tempo (5/4)
>                 (line [percm BassDrum1 [sn], percm AcousticSnare [sn, sn, sn, sn]
>                , chord [percm BassDrum1 [sn], percm AcousticSnare [sn]], percm AcousticSnare [sn, sn, sn, sn]
>                , times 5 (line [chord [percm BassDrum1 [sn], percm AcousticSnare [sn]]
>                               , percm AcousticSnare [sn]])])]]
>     p062        =
>       chord [line [percm CrashCymbal1 [en, en, en, en], percm RideBell [en], percm CrashCymbal1 [en, en, en]
>                  , tempo (5/4) (line [percm CrashCymbal1 [en, en, sn]])]
>            , line [percm BassDrum1 [en, sn, sn], percm AcousticSnare [en], percm BassDrum1 [en, en, sn, sn]
>                  , percm AcousticSnare [en], percm BassDrum1 [sn, sn]
>                  , tempo (5/4) (line [percm BassDrum1 [sn], percm LowTom [sn], percm BassDrum1 [sn]
>                                     , percm LowTom [sn], percm BassDrum1 [sn]])]]
>     p063        =
>       chord [
>         line [tempo (5/4) (line [rest sn, percm CrashCymbal1 [en, en, en, en, sn]]), percm CrashCymbal1 [en, en, qn]
>             , t32 [percm CrashCymbal1 [en, en, en]]]
>      ,  line [tempo (5/4) (line [percm LowTom [sn], percm BassDrum1 [sn], percm LowTom [sn], percm BassDrum1 [sn]
>                              , rest sn, percm BassDrum1 [sn], percm AcousticSnare [sn], percm BassDrum1 [sn]
>                              , percm AcousticSnare [sn], percm BassDrum1 [sn]])
>             , percm BassDrum1 [en, en], t32 [percm BassDrum1 [en], percm AcousticSnare [en, en]
>             , chord [percm BassDrum1 [en, en, en], percm AcousticSnare [en, en, en]]]]]
>     p064        =
>       chord [
>         tempo (5/4) (line [percm CrashCymbal1 [en, den, en, den, en, den, en, den, en, den]])
>       , tempo (5/4) (line [percm BassDrum1 [sn], percm AcousticSnare [sn], percm BassDrum1 [sn], percm HighTom [sn]
>                          , percm AcousticSnare [sn], percm BassDrum1 [sn], percm AcousticSnare [sn]
>                          , percm BassDrum1 [sn], percm AcousticSnare [sn], percm BassDrum1 [sn], percm BassDrum1 [sn]
>                          , percm HighTom [sn], percm BassDrum1 [sn], percm HiMidTom [sn], percm LowTom [sn]
>                          , percm BassDrum1 [sn], percm LowTom [sn], percm BassDrum1 [sn], percm AcousticSnare [sn, sn]
>                          , percm BassDrum1 [sn], percm AcousticSnare [sn], percm BassDrum1 [sn], percm LowTom [sn]
>                          , percm AcousticSnare [sn]])]
>
>     l061        =
>       t32 [grace (-2) (cs 6 sn), gs 5 sn, gs 5 en, gs 5 en, gs 5 en, gs 5 sn, fs 5 sn, e 5 en, fs 5 en
>          , grace (-2) (fs 5 en), gs 4 en, b 4 en, cs 5 qn, cs 5 den, gs 5 den]
>     l062        =
>       line [b 5 qn, b 5 qn, t32 [cs 6 en, b 5 sn, cs 6 en, b 5 sn, cs 6 en, b 5 sn, cs 6 en, b 5 sn, cs 6 en
>           , grace (-4) (e 6 sn), cs 6 sn, b 5 en]]
>     l063        =
>       line [b 5 en, t32 [cs 6 sn, grace (-2) (gs 6 en), cs 6 en, b 5 den, cs 6 sn], e 6 sn, cs 6 en, b 5 den
>           , b 5 en, grace (-2) (gs 5 en), fs 5 en]
>     l064        =
>       line [t32 [e 5 qn, cs 5 sn, b 4 sn], rest dqn, cs 6 en, cs 6 en, b 5 en, b 5 en
>           , t32 [gs 5 en, fs 5 sn]]
>
>     p065_068    = line [p065, p066, p067, p068]
>     l065_068    = line [l065, l066, l067, l068]
>
>     p065        =
>       chord [line [tempo (5/4) (line [percm CrashCymbal1 [en, den, en, en, sn]])
>                  , t32 [percm CrashCymbal1 [sn, sn, qn, den, den, den, den]]]
>            , line [tempo (5/4) (line [percm HighFloorTom [sn], percm LowTom [sn]
>                                     , percm HighFloorTom [sn], percm LowTom [sn, sn]
>                                     , percm BassDrum1 [sn], percm HighTom [sn]
>                                     , percm BassDrum1 [sn], percm HighTom [sn]
>                                     , chord [percm BassDrum1 [sn], percm HighTom [sn]]])
>                  , t32 [percm HiMidTom [sn], percm LowTom [sn, sn], percm AcousticSnare [sn]
>                       , percm BassDrum1 [sn], percm LowTom [sn], chord [percm BassDrum1 [en], percm HighTom [en]]
>                       , percm LowTom [sn], percm HighTom [sn], chord [percm BassDrum1 [sn], percm LowTom [sn]]
>                       , percm LowTom [sn]
>                       , chord [percm BassDrum1 [sn], percm LowTom [sn]], percm AcousticSnare [sn, sn]
>                       , chord [percm BassDrum1 [sn], percm LowTom [sn]], percm AcousticSnare [sn, sn]]]]
>     p066        =
>       line [percm CrashCymbal1 [tn], percm HighTom [tn],  chord [percm BassDrum1 [sn], percm HighTom [sn]]
>           , percm CrashCymbal1 [tn], percm HiMidTom [tn], chord [percm BassDrum1 [sn], percm HiMidTom [sn]]
>           , percm CrashCymbal1 [tn], percm LowTom [tn],   chord [percm BassDrum1 [sn], percm LowTom [sn]]
>           , percm CrashCymbal1 [tn], percm HighTom [tn],  chord [percm BassDrum1 [sn], percm HighTom [sn]]
>           , percm CrashCymbal1 [tn], percm HiMidTom [tn], chord [percm BassDrum1 [sn], percm HiMidTom [sn]]
>           , percm CrashCymbal1 [tn], percm LowTom [tn],   chord [percm BassDrum1 [sn], percm LowTom [sn]]
>           , percm CrashCymbal1 [tn], percm HiMidTom [tn], chord [percm BassDrum1 [sn], percm HiMidTom [sn]]
>           , percm CrashCymbal1 [tn], percm HighTom [tn],  chord [percm BassDrum1 [sn], percm HighTom [sn]]
>           , percm CrashCymbal1 [tn], percm LowTom [tn],   chord [percm BassDrum1 [sn], percm LowTom [sn]]
>           , percm CrashCymbal1 [tn], percm HighTom [tn],  chord [percm BassDrum1 [sn], percm HighTom [sn]]]
>     p067        =
>       line [percm CrashCymbal1 [tn], percm HiMidTom [tn], chord [percm BassDrum1 [sn], percm HiMidTom [sn]]
>           , percm CrashCymbal1 [tn], percm LowTom [tn],   chord [percm BassDrum1 [sn], percm LowTom [sn]]
>           , percm CrashCymbal1 [tn], percm HighTom [tn],  chord [percm BassDrum1 [sn], percm HighTom [sn]]
>           , percm CrashCymbal1 [tn], percm HiMidTom [tn], chord [percm BassDrum1 [sn], percm HiMidTom [sn]]
>           , percm CrashCymbal1 [tn], percm LowTom [tn],   chord [percm BassDrum1 [sn], percm LowTom [sn]]
>           , percm CrashCymbal1 [tn], percm HighTom [tn], percm BassDrum1 [sn]
>           , t32 [percm CrashCymbal1 [sn], percm HiMidTom [sn], percm BassDrum1 [sn]
>                , percm CrashCymbal1 [sn], percm LowTom [sn], percm BassDrum1 [sn]
>                , percm BassDrum1 [en], percm AcousticSnare [sn]], percm AcousticSnare [tn, tn, tn, tn]]
>     p068        =
>       chord [t32 [percm CrashCymbal1 [dqn], percm RideBell [en, en, en, en, en, en, en, en, en, qn, en]]
>            , t32 [percm BassDrum1 [qn, en], percm AcousticSnare [qn], percm BassDrum1 [en, qn, en]
>                 , percm AcousticSnare [qn], percm BassDrum1 [en, qn, en]]]
>
>     l065        =
>       line [gs 5 sn, fs 5 (den + dqn), fs 6 en, fs 5 en, addDur tn [fs 5, e 5, ds 5, cs 5 ], fs 5 en
>           , addDur tn [fs 5, gs 5, fs 5, e 5]]
>     l066        =
>       line [fs 5 en, addDur tn [fs 5, e 5, ds 5, cs 5], trill 1 tn (gs 5 dqn)
>             , t32 [b 5 sn, gs 5 sn, fs 5 sn, gs 5 den, gs 5 den, g 4 en, fs 5 en, fs 5 en]]
>     l067        =
>       line [t32 [fs 5 sn, e 5 sn, ds 5 sn, fs 5 dsn, fs 5 dsn, fs 5 sn, e 5 sn, ds 5 sn]
>           , trill 1 tn (fs 5 dqn), t32 [fs 5 en, gs 5 en, gs 5 en], gs 5 tn, fs 5 tn, rest sn, grace (-5) (b 5 en)]
>     l068        =
>       line [t32 [b 5 en, addDur sn [b 5, gs 5, fs 5, cs 5]]
>           , chord [line [grace (-4) (gs 5 qn), rest qn], line [cs 6 qn, ascendFrom dhLead (Cs, 6) Cs chromatic qn]]
>           , rest qn, grace (-2) (ds 6 qn)]
>
>     p069_072    = line [p069, p070, p071, p072]
>     l069_072    = line [l069, l070, l071, l072]
>
>     p069        =
>       t32 [chord [line [percm RideBell [qn, en], percm CrashCymbal1 [qn, sn, sn, qn, en, qn, en, den, den]]
>                 , line [percm AcousticSnare [qn], percm BassDrum1 [en, qn, sn, sn]
>                       , percm AcousticSnare [qn], percm BassDrum1 [en, qn, sn, en]
>                       , percm AcousticSnare [sn], percm BassDrum1 [en, en]]]]
>                   
>     p070        =
>       chord [line [t32 [percm CrashCymbal1 [qn, en]], percm RideCymbal1 [en, en]
>                  , tempo (5/4) (line [percm RideCymbal1 [en, en, sn]]), percm RideCymbal1 [en, en]
>                  , percm CrashCymbal1 [en, en]]
>            , line [t32 [percm BassDrum1 [en, en, en]]
>                  , chord [line [percm BassDrum1 [en], t32 [percm BassDrum1[en, sn]]
>                               , tempo (5/4) (line [percm BassDrum1 [en, en, sn]])
>                               , percm BassDrum1 [en, en, en, en]]
>                         , line [percm AcousticSnare [en], t32 [percm AcousticSnare [en, sn]]
>                               , tempo (5/4) (line [rest sn, percm AcousticSnare [sn, en, sn]])
>                               , rest qn, percm AcousticSnare [en], percm HighTom [en]]]]]
>     p071        =
>       chord [line [percm CrashCymbal1 [en, en, en, qn, en, en, en], rest qn]
>            , line [percm BassDrum1 [en, en, en, qn, en, en, en]
>                  , tempo (5/4) (line [percm BassDrum1 [en], percm AcousticSnare [en, sn]])]]
> -- 10/4
>     p072        =
>       t32 [chord [line [times 15 (percm CrashCymbal1 [qn])]
>                 , line [times 15 (percm BassDrum1 [den, sn])]
>                 , line [times 11 (percm AcousticSnare [qn]), rest wn]]]
>
>     l069        =
>       line [ds 6 en, t32 [cs 6 qn, e 6 qn, grace (-3) (e 6 qn)], cs 6 en
>           , tempo (5/4) (line [chord [line [b 4 en, b 4 den], line [b 5 en, b 5 sn, cs 6 en]]])
>           , t32 [grace (-3) (e 6 qn), cs 6 en]]
>     l070        =
>       line [t32 [b 5 en, gs 5 en, grace (-2) (fs 6 en)], e 5 qn
>           , tempo (5/4) (line [cs 6 sn, b 5 sn, cs 6 en, cs 6 sn])
>           , grace (-2) (cs 6 en), gs 6 en, t32 [rest qn, b 5 en]]
>     l071        =
>       line [cs 6 sn, grace (-3) (e 6 sn), ds 6 sn, e 6 sn
>           , t32 [e 6 sn, cs 6 sn, b 5 en, t32 [a 5 en, cs 6 sn], grace 1 (gs 5 en), cs 6 en, b 5 qn
>                , addDur sn [a 5, b 5, a 5, g 5], a 5 en, t32 [addDur sn [gs 5, fs 5, e 5]], b 5 en]]
>     l072        =
>       line [tempo (5/4) (line [a 5 den, b 5 sn, a 5 sn])
>           , t32 [gs 5 en, fs 5 en, ds 5 en, grace (-3) (e 6 qn), cs 6 en, b 5 en, a 5 en, gs 5 en]
>           , chord [gs 5 qn, cs 6 qn]]
>
>     p073_076    = line [p073, p074, p075, p076]
>     l073_076    = line [l073, l074, l075, l076]
>
>     p073        = rest 0
>     p074        =
>       chord [t32 [times 4 (percm CrashCymbal1 [en, en, en]), rest en, percm OpenHiHat [qn]]
>            , t32 [percm BassDrum1 [qn, en], percm AcousticSnare [qn], percm BassDrum1 [en, qn, sn, sn]
>                 , percm AcousticSnare [qn], percm BassDrum1 [en], percm AcousticSnare [sn, en, sn, sn, sn]]]
>     p075        =
>       t32 [chord [percm CrashCymbal1 [sn], percm BassDrum1 [sn]], percm HighTom [sn, sn], percm AcousticSnare [sn]
>          , percm HiMidTom [sn, sn, sn, sn, sn], percm BassDrum1 [sn], percm LowTom [sn, sn, sn, sn, sn]
>          , percm BassDrum1 [sn], percm LowTom [sn, sn], percm AcousticSnare [sn], percm LowTom [sn, sn]
>          , chord [percm BassDrum1 [sn], percm ClosedHiHat [sn]], percm LowTom [sn, sn], percm AcousticSnare [en, en, sn, sn]]
>     p076        =
>       chord [t32 [perc CrashCymbal1 dqn, percm RideCymbal1 [qn, en, qn, en], percm ClosedHiHat [en, en, en]
>                 , perc CrashCymbal1 en, percm ClosedHiHat [en, en]]
>            , t32 [perc BassDrum1 dqn, perc AcousticSnare qn, percm BassDrum1 [en, qn, en, dqn, qn, sn, sn]]]
>
>     l073        =
>       t32 [a 5 sn, b 5 sn, a 5 sn, gs 5 sn, a 5 en, gs 5 sn, fs 5 sn, e 5 sn, rest sn, gs 5 sn
>          , grace (-2) (gs 5 (sn + qn + dqn)), grace (-2) (gs 5 dqn), rest en]
>     l074        =
>       line [rest sn, b 5 tn, gs 5 tn, rest en, ds 5 en, gs 5 en, fs 5 dsn, fs 5 tn, e 5 tn, ds 5 tn, rest sn
>           , addDur en [gs 5, b 5, gs 5, fs 5]]
>     l075        =
>       line [e  5 en, gs 5 sn, b 5 sn, b 5 en, cs 6 sn, cs 6 sn, cs 6 sn, cs 6 sn, chord [b 5 sn, ds 6 sn]
>           , cs 6 (sn + qn), addDur sn [gs 6, e 6, gs 6, e 6]]
>     l076        =
>       line [tempo (5/4) (line [gs 6 sn, e 6 sn, e 6 sn, b 6 tn, e 6 tn, rest sn])
>           , t32 [grace 1 (ds 6 en), b 5 en, t32 [addDur sn [e 5, ds 6, ds 6]], cs 6 dqn, b 4 en, e 6 en, ds 6 en
>                , e 6 en, e 6 en, t32 [addDur sn [e 6, ds 6, b 5]]]]
>     p077_080    = line [p077, p078, p079, p080]
>     l077_080    = line [l077, l078, l079, l080]
>
>     p077        =
>       chord [t32 [times 15 (perc ClosedHiHat en)]
>            , t32 [percm BassDrum1 [en, en, en, qn, en, den, den, den, den, dqn]]
>            , t32 [percm AcousticSnare [en, en, en, dqn, den, den, den, den, en, en, en]]]
> -- 10/4
>     p078        =
>       chord [t32 [times 30 (perc ClosedHiHat en)]
>            , t32 [percm AcousticSnare [en, en, en, en, en], times 11 (perc HighFloorTom qn)
>                 , percm AcousticSnare [dqn]]
>            , t32 [percm BassDrum1 [5 * en], times 11 (perc BassDrum1 qn), rest dqn]]
>     p079        = rest 0
>     p080        =
>       chord [line [chord [perc CrashCymbal1 en, perc ClosedHiHat en], times 5 (perc ClosedHiHat en)
>                  , t32 [times 3 (perc ClosedHiHat en)], perc OpenHiHat en, perc ClosedHiHat en]
>            , line [perc BassDrum1 en, t32 [percm BassDrum1 [en, sn], perc AcousticSnare qn
>                                         , perc BassDrum1 en], perc BassDrum1 en, t32 [percm BassDrum1 [en, sn]]
>                  , perc AcousticSnare en, perc BassDrum1 en, perc AcousticSnare qn]]
>
>     l077        =
>       line [b 4 sn, e 6 sn, ds 6 sn, e 6 sn
>           , t32 [grace (-4) (gs 6 qn), t32 [addDur sn [b 6, gs 6, e 5]], e 6 den, grace 1 (ds 6 en), b 5 sn, ds 6 en
>                , gs 6 sn, fs 6 den, e 6 sn, fs 6 sn, e 6 den, e 6 sn]]
>     l078        =
>       line [t32 [grace 3 (cs 6 en), e 6 sn], cs 6 en, b 5 den, b 5 sn
>           , t32 [gs 5 en, gs 5 sn, gs 5 en, gs 5 sn, gs 5 en, gs 5 en, gs 5 en]
>           , gs 5 sn, b 5 sn, gs 5 tn, b 5 tn, gs 5 tn, fs 5 tn]
>     l079        =
>       t32 [gs 5 en, gs 5 en, gs 5 en, gs 5 sn, b 5 sn, gs 5 sn, b 5 sn, gs 5 en
>          , chord [line [grace 1 (fss 5 (dqn+dqn)), fss 5 en, fss 5 en, fss 5 en]
>                 , line [rest dqn, e 5 en, e 5 en, e 5 en, e 5 en, e 5 en, e 5 en]]]
>     l080        =
>       line [tempo (7/4) (line [b 5 sn, gs 5 sn, fss 5 sn, b 4 en, b 5 sn, gs 5 sn]), grace (-2) (cs 6 en), gs 5 sn
>           , t32 [addDur tn [gs 5, fs 5, e 5], fs 5 qn, e 5 sn, cs 5 sn
>                , grace (-4) (e 5 en), fss 5 en, fss 5 en, b 5 qn, gs 5 en]]
>     p081_084    = line [p081, p082, p083, p084]
>     l081_084    = line [l081, l082, l083, l084]
>
>     p081        =
>       chord [line [perc OpenHiHat en, perc ClosedHiHat en, percm RideCymbal1 [en, sn, sn, qn]
>                  , t32 [percm RideCymbal1 [qn, en]], rest en, percm RideCymbal1 [sn, sn]]
>            , line [percm BassDrum1 [qn, en, sn, sn, qn]
>                  , t32 [percm BassDrum1 [qn, en]],   rest en, percm BassDrum1 [sn, sn]]]
> -- 9/4
>     p082        =
>       chord [line [percm RideBell [en, sn, sn, en, sn, sn, en], t32 [percm RideBell [en, sn]]
>                  , percm RideBell [sn, en, sn, sn, sn, sn, sn]
>                  , rest sn, percm RideCymbal1 [sn, en, sn, en, en, sn, en, sn, sn, en]]
>            , line [roll tn (perc AcousticSnare (dhn + en)), times 4 (perc AcousticSnare tn)
>                  , roll tn (perc AcousticSnare (qn + wn))]
>            , line [percm BassDrum1 [en, sn, sn, en, sn, sn, en], t32 [percm BassDrum1 [en, sn]]
>                  , percm BassDrum1 [sn, den, en, en, sn, sn, en, sn, en, en, sn, en, sn, sn, en]]]
>     p083        =
>       chord [perc RideCymbal1 qn, t32 [perc BassDrum1 en, roll tn (perc AcousticSnare qn)]]
>     p084        =
>       chord [line [percm CrashCymbal1 [qn, en], t32 [percm CrashCymbal1 [en, sn]], percm CrashCymbal1 [en, en, qn]
>                  , t32 [percm CrashCymbal1 [qn, en]]]
>            , line [rest qn, perc AcousticSnare en, t32 [percm AcousticSnare [en, sn]]
>                  , t32 [percm AcousticSnare [sn, sn, sn, sn, sn, sn]], tempo (5/4) (times 5 (perc AcousticSnare sn))
>                  , t32 [times 6 (perc AcousticSnare sn)]]
>            , line [t32 [percm BassDrum1 [qn, en]], perc BassDrum1 en, t32 [percm BassDrum1 [en, sn, den, den]]
>                  , percm BassDrum1 [qn], t32 [percm BassDrum1 [qn, en]]]]
>
>     l081        =
>       line [b 5 en, gs 5 sn, b 5 tn, gs 5 tn, fss 5 qn, t32 [fs 5 en, gs 5 en, gs 5 en, t32 [a 5 en, gs 5 en, fss 5 en]
>           , a 5 en, gs 5 sn, fs 5 sn, gs 5 qn]]
>     l082        =
>       t32 [g 4 en, tempo (5/4) (addDur sn [gs 5, fss 5, gs 5, fss 5, e 5]), fss 5 en, gs 5 sn
>          , addDur dtn [gs 5, a 5, gs 5, e 5], gs 5 qn, gs 5 en, b 5 en, b 5 en, t32 [addDur sn [b 5, a 5, gs 5]]
>          , addDur sn [a 5, d 5, e 5, fs 5, e 5, d 5]]
>     l083        =
>       line [tempo (3/4) (line [tempo (5/4) (line [addDur tn [gs 5, a 5, b 5, a 5, gs 5], a 5 sn
>                              , addDur tn [a 5, gs 5, fs 5]])
>                              , t32 [chord [b 4 en, e 5 en], cs 5 sn, chord [cs 5 en, fs 5 en], e 5 sn]
>                              , tempo (5/4) (line [addDur sn [b 4, cs 5, cs 5, e 5, e 5]])])
>           , tempo (5/4) (line [cs 5 en, b 4 sn, cs 5 sn, b 4 sn])]
> -- 6/4
>     l084        =
>       line [t32 [chord [gs 4 en, cs 5 en], fs 4 sn], e 4 en, t32 [cs 5 en, b 4 hn, b 4 en]
>           , grace (-1) (a 3 en), b 3 (dqn + qn)]
>     p085_088    = line [p085, p086, p087, p088]
>     l085_088    = line [l085, l086, l087, l088]
>
>     p085        =
>       chord [line [tempo (5/4) (line [percm CrashCymbal1 [7 * sn, 3 * sn], rest (3 * 5 * sn)])]
>            , line [tempo (5/4) (line [chord [perc BassDrum1 sn, perc AcousticSnare sn]
>                                     , times 6 (perc AcousticSnare sn), perc LowTom sn, times 4 (perc AcousticSnare sn)
>                                     , perc HighTom sn, t32 [perc HiMidTom sn, perc HiMidTom sn, perc LowTom sn]
>                                     , perc HighTom sn, perc LowTom sn, perc HiMidTom sn, perc LowTom sn
>                                     , perc HiMidTom sn, times 5 (perc LowTom sn)])]]
>     p086        =
>       chord [line [percm CrashCymbal1 [qn, qn, den, qn], percm RideCymbal1 [sn, sn, sn, sn, sn]]
>            , line [percm BassDrum1 [qn, den, sn], perc AcousticSnare sn, percm BassDrum1 [sn, sn]
>                  , perc AcousticSnare en, percm BassDrum1 [sn, sn], perc LowTom sn, percm BassDrum1 [en, sn]
>                  , perc LowTom sn]]
>     p087        =
>       chord [line [percm RideCymbal1 [en, en, den, sn], rest den, percm RideCymbal1 [sn, hn]]
>            , line [percm BassDrum1 [sn, sn], perc AcousticSnare sn, perc BassDrum1 sn, perc AcousticSnare sn
>                  , percm BassDrum1 [sn, sn], perc AcousticSnare sn, perc BassDrum1 sn, perc AcousticSnare tn
>                  , perc BassDrum1 tn, perc AcousticSnare tn, percm BassDrum1 [tn, sn, den]
>                  , percm AcousticSnare [tn, tn, sn, sn, sn, sn]]]
>     p088        =
>       line [chord [perc ClosedHiHat den, perc BassDrum1 den, perc AcousticSnare den], perc BassDrum1 sn
>           , perc OpenHiHat en, perc BassDrum1 en, chord [perc ClosedHiHat en, perc AcousticSnare en]
>           , times 3 (perc ClosedHiHat en), perc OpenHiHat qn]
>
> -- 4/4
>     l085        =
>       t32 [addDur en [gs 3, gs 3, gs 3, fs 3, fs 3, fs 3, gs 3, b 3, b 3,b 3, cs 4, cs 4]]
> -- 5/4
>     l086        =
>       line [cs 4 (wn + qn)]
> -- 0/4
>     l087        = rest 0
>     l088        =
>       line [chord [b 4 qn, ds 5 qn], e 5 qn, chord [ds 5 qn, e 5 qn], cs 5 qn, gs 4 qn]
>
>     p089_092    = line [p089, p090, p091, p092]
>     l089_092    = line [l089, l090, l091, l092]
>
>     p089        =
>       chord [line [times 6 (perc ClosedHiHat en), perc OpenHiHat qn, percm ClosedHiHat [en, en]]
>            , line [percm BassDrum1 [den, sn], perc AcousticSnare dqn, perc BassDrum1 dqn, rest sn
>                  , perc AcousticSnare en, perc BassDrum1 sn]]
>     p090        =
>       chord [line [perc OpenHiHat qn, times 8 (perc ClosedHiHat en)]
>            , line [percm BassDrum1 [ddqn, tn, tn], perc AcousticSnare hn, rest sn, times 3 (perc BassDrum1 sn)]]
>     p091        =
>       chord [line [perc OpenHiHat en, times 5 (perc ClosedHiHat en)
>                  , chord [line [times 4 (perc ClosedHiHat en)], perc RideCymbal1 hn]]
>            , line [percm BassDrum1 [den, sn, den, sn, sn, en, sn, sn], percm AcousticSnare [en, en, en, sn]]]
>     p092        =
>       line [perc ClosedHiHat sn, perc BassDrum1 sn, perc OpenHiHat sn, perc BassDrum1 sn, percm ClosedHiHat [en, sn]
>           , chord [line [percm ClosedHiHat [sn, sn, sn, en, sn], perc OpenHiHat den, perc ClosedHiHat den
>           , perc OpenHiHat sn], line [perc AcousticSnare dhn]]]
>
>     l089        =
>       line [fs 4 qn, b 4 qn, cs 5 qn, e 5 qn, cs 5 en, rest en]
>     l090        =
>       line [ds 5 qn, e 5 en, rest en, ds 5 qn, cs 5 qn, gs 4 qn]
>     l091        =
>       line [fs 4 qn, b 4 qn, cs 5 qn, e 5 qn, cs 5 en, rest en]
>     l092        =
>       line [ds 5 qn, e 5 en, rest en, ds 5 qn, cs 5 qn, gs 4 qn]
>
>     p093_096    = line [p093, p094, p095, p096]
>     l093_096    = line [l093, l094, l095, l096]
>
>     p093        =
>       chord [line [percm ClosedHiHat [en, en, sn], perc OpenHiHat den, percm ClosedHiHat [sn, sn, sn]
>                  , perc OpenHiHat den, percm ClosedHiHat [en, sn], perc OpenHiHat den]
>            , line [percm BassDrum1 [en, en], perc AcousticSnare hn, rest en, percm BassDrum1 [sn, sn, qn]]]
>     p094        =
>       chord [line [percm ClosedHiHat [sn, sn, sn], perc OpenHiHat den, percm ClosedHiHat [sn, sn, sn]
>                  , perc OpenHiHat den, percm ClosedHiHat [sn, sn, sn], perc OpenHiHat den, percm ClosedHiHat [sn, sn]]
>            , line [percm BassDrum1 [dqn, en], perc AcousticSnare hn, rest en, perc BassDrum1 en]]
>     p095        =
>       chord [
>         line [perc ClosedHiHat sn, percm OpenHiHat [den, qn], rest hn, perc ClosedHiHat en, perc OpenHiHat en]
>       , line [perc BassDrum1 (hn+en), perc HighTom en, perc LowMidTom sn, perc LowTom sn, percm HighFloorTom [sn, sn]
>             , t32 [chord [percm BassDrum1 [en, en, en], line [percm LowFloorTom [en, en], rest en]]]]]
>     p096        =
>       t32 [ chord [
>         line [perc CrashCymbal1 dqn, perc OpenHiHat dqn, percm ClosedHiHat [qn, en, en, en], perc OpenHiHat en
>             , percm ClosedHiHat [en, en, en]]
>       , line [percm BassDrum1 [dqn+qn, en, dqn], rest dhn]]]
>
>     l093        = line [fs 4 qn, b 4 qn, cs 5 qn,  e 5 qn, cs 5 qn]
>     l094        = line [ds 5 qn, e 5 qn, ds 5 qn, cs 5 qn, gs 4 qn]
>     l095        = line [fs 4 qn, b 4 qn, cs 5 qn,  e 5 qn, cs 5 en, fs 4 sn, gs 4 sn]
>     l096        =
>       line [b 4 en, t32 [b 4 en, gs 4 sn], b 4 en, t32 [b 4 en, gs 4 sn], grace (-2) (ds 5 qn), rest en
>           , t32 [fs 4 en, gs 4 sn], b 4 en, t32 [b 4 en, gs 4 sn]]
>
>     p097_100    = line [p097, p098, p099, p100]
>     l097_100    = line [l097, l098, l099, l100]
>
>     p097        =
>       t32 [ chord [
>         line [percm ClosedHiHat [qn, en], perc CrashCymbal1 dqn, percm OpenHiHat [qn, en]
>             , percm ClosedHiHat [en, en, en, qn], perc OpenHiHat en]
>       , line [rest qn, perc BassDrum1 en, perc AcousticSnare dqn, rest qn, percm BassDrum1 [en, qn, en]
>             , perc AcousticSnare dqn]]]
>     p098        =
>       t32 [ chord [
>         line [percm ClosedHiHat [en, en, en], percm OpenHiHat [qn, en], percm ClosedHiHat [dqn, en, en]
>             , perc OpenHiHat en, percm ClosedHiHat [en, en, en]]
>       , line [rest qn, perc LowTom hn, percm BassDrum1 [dhn, dqn]]]]
>     p099        =
>       chord [
>         line [percm ClosedHiHat [den, sn, dden], perc OpenHiHat tn, rest qn, percm CrashCymbal1 [qn, qn]]
>       , line [percm BassDrum1 [den, sn], chord [perc BassDrum1 qn, perc AcousticSnare qn], rest dhn]]
>     p100        =
>       t32 [chord [
>         line [perc CrashCymbal1 dqn, percm OpenHiHat [qn, en], percm ClosedHiHat [dqn, en, en]
>             , perc OpenHiHat en, percm ClosedHiHat [en, en, en]]
>       , line [percm BassDrum1 [hn + en, en, dqn], rest dhn]]]
>
> -- 6/4
>     l097        =
>       line [b 4 en, b 4 sn, a 4 sn, b 4 en, grace (-2) (gs 5 en)
>           , chord [line [cs 5 qn, rest en, a 4 en, gs 4 hn]
>                  , line [fs 5 qn, rest en, ds 5 en, cs 5 hn]]]
> -- 4/4
>     l098        =
>       line [rest en, chord [line [ a 4 en, t32 [gs 4  qn,  b 4 en, cs 5 en, cs 5 en, fs 4 en], fs 4 qn]
>                           , line [ds 5 en, t32 [cs 5 dqn, rest qn,  b 4 en], b 4 qn]]]
>     l099        =
>       line [rest en, chord [line [fs 4 en, fs 4 qn, descendFrom dhLead (Fs, 4) Fs chromatic qn]
>                           , line [ b 4 en,  b 4 qn, descendFrom dhLead ( B, 4) B chromatic qn]
>                           , line [rest en, ds 5 qn, descendFrom dhLead (Ds, 5) Ds chromatic qn]]
>           , rest en, t32 [fs 4 en, gs 4 sn], b 4 en, b 4 sn, gs 4 sn]
> -- 6/4
>     l100        =
>       line [chord [fs 4 en, b 4 en], t32 [b 4 en, gs 4 sn]
>           , tempo (7/4) (line [addDur sn [ds 5, cs 5, ds 5, cs 5, ds 5], cs 5 en])
>           , t32 [b 4 en, grace (-4) (ds 5 en), t32 [addDur sn [ds 5, cs 5, b 4]], cs 5 en, cs 5 en, cs 5 sn, b 4 sn]
>           , fs 4 sn, e 4 sn, fs 4 qn, t32 [fs 4 en, gs 4 sn]]
>
>     p101_104    = line [p101, p102, p103, p104]
>     l101_104    = line [l101, l102, l103, l104]
>
>     p101        =
>       t32 [chord [
>         line [percm OpenHiHat [qn, en], percm ClosedHiHat [en, en, en, en, en, en, dhn]]
>       , line [rest qn, percm BassDrum1 [en, dhn], rest qn, percm HighFloorTom [en, en], percm BassDrum1 [en, en]]]]
>     p102        =
>       t32 [chord [
>         line [perc CrashCymbal1 dqn, perc OpenHiHat dqn, percm ClosedHiHat [qn, en, en, en], perc OpenHiHat en
>             , percm ClosedHiHat [en, en, en]]
>       , line [percm BassDrum1 [dhn, dhn + dqn]]]]
>     p103        =
>       t32 [chord [
>         line [percm ClosedHiHat [en, qn, qn, en, en, en], perc RideCymbal1 qn, perc ClosedHiHat hn
>             , perc CrashCymbal1 en]
>       , line [percm BassDrum1 [qn, en, dqn], rest (hn+en), percm AcousticSnare [en, qn], perc BassDrum1 en]]]
>     p104        =
>       t32 [chord [
>         line [percm CrashCymbal1 [dqn, dqn], percm ClosedHiHat [qn, en, en, en], perc OpenHiHat en
>             , percm ClosedHiHat [en, en, en]]
>       , line [perc BassDrum1 dhn, chord [perc BassDrum1 dqn, perc AcousticSnare dqn], rest dhn]]]
>
> -- 4/4
>     l101        =
>       line [b 4 en, b 4 sn, gs 4 sn, t32 [rest en, fs 5 en, ds 5 en, grace (-2) (ds 5 en), cs 5 en, b 4 sn, cs 4 sn]
>           , b 4 sn, b 4 tn, gs 4 tn, b 4 en]
>     l102        =
>       line [t32 [fs 4 qn, fs 4 en], ds 4 qn, fs 4 en, b 3 en, t32 [fs 4 sn, ds 4 sn, fs 4 en, b 3 en], b 3 qn]
>     l103        =
>       line [t32 [b 3 qn, a 3 en], a 3 dqn, cs 4 dqn, cs 7 qn]
>     l104        =
>       line [rest en, t32 [fs 3 en, gs 3 sn], b 3 en, t32 [b 3 en, gs 3 sn]
>           , chord [line [b 3 en, t32 [b 3 en, gs 3 sn]]
>                  , line [fs 4 en, fs 4 en]]
>           , cs 4 qn, rest en, t32 [fs 3 en, gs 3 sn]]
>
>     p105_109    = line [p105, p106, p107, p108, p109, p110, p111]
>     l105_109    = line [l105, l106, l107, l108, l109]
>
>     p105        =
>       t32 [chord [
>         line [perc OpenHiHat qn, percm ClosedHiHat [en, qn, en], percm OpenHiHat [qn, en, dhn]]
>       , line [rest qn, percm BassDrum1 [en, dqn], rest dqn, roll tn (perc AcousticSnare dhn)]]]
>     p106        =
>       t32 [chord [
>         line [percm CrashCymbal1 [dqn, qn, en], percm ClosedHiHat [qn, en, en, en], perc OpenHiHat en
>             , percm ClosedHiHat [en, en, en]]
>       , line [percm BassDrum1 [dqn+qn, en], chord [perc BassDrum1 dqn, perc AcousticSnare dqn], rest dhn]]]
>     p107        =
>       t32 [chord [
>         line [percm OpenHiHat [qn, en, en, en, en, qn], percm ClosedHiHat [en, qn, en, qn, en]]
>       , line [rest qn, percm BassDrum1 [en, dqn+qn, en], perc AcousticSnare dhn]]]
>     p108        =
>       t32 [chord [
>         line [perc CrashCymbal1 dqn, percm ClosedHiHat [qn, dqn, en, dqn, en, en, en]]
>       , line [perc BassDrum1 dqn, perc SideStick qn, perc BassDrum1 en, perc AcousticSnare dqn
>             , percm SideStick [qn, en], percm AcousticSnare [en, en, en]]]]
>     p109        =
>       t32 [chord [
>         line [perc RideBell en, percm ClosedHiHat [en, en], perc RideBell en, percm ClosedHiHat [en, en, en, en, en]
>             , percm RideBell [qn, qn, qn]]
>       , line [percm AcousticSnare [en, en, en, en, en, en, en, en, en], roll sn (perc AcousticSnare hn)]
>       , line [percm BassDrum1 [qn, dqn, dqn, en, dqn], rest dqn]]]
>     p110        =
>       t32 [chord [
>         line [percm RideBell [qn, en], perc OpenHiHat en, percm ClosedHiHat [en, en, qn], perc OpenHiHat en
>             , percm ClosedHiHat [qn, en], percm RideBell [qn, en]]
>       , line [percm BassDrum1 [qn+dqn, en], perc AcousticSnare hn, perc BassDrum1 en]]]
>     p111        =
>       t32 [chord [
>         line [perc RideBell en, percm ClosedHiHat [en, en], percm RideBell [qn, en], percm ClosedHiHat [en, en]]]]
>     l105        =
>       line [b 3 en, t32 [b 3 en, gs 3 sn, b 3 en, b 3 en, rest en]
>           , chord [t32 [grace (-2) ( b 3 qn), gs 3 en, fs 3 dqn, grace (-2) (gs 4 den), fs 4 den]
>                  , t32 [grace (-2) (ds 5 qn), ds 5 en, cs 4 dqn, grace (-2) (cs 5 dqn)]]]
> -- 6/4
>     l106        =
>       line [fs 4 qn, t32 [rest qn, chord [a 4 en, ds 5 en]]
>           , chord [line [gs 4 (dhn + 1/6), fs 4 (1/12)], line [cs 5 (dhn + 1/6), b 4 (1/12)]]]
> -- 3.5/4
>     l107        =
>       chord [line [rest en, fs 4 en, rest sn, fs 4 sn, gs 4 hn]
>            , line [rest en,  b 4 en, rest sn,  b 4 sn, cs 5 hn]]
> -- 20.5/4
>     l108        =
>       chord [line [t32 [fs 4 en], fs 4 (1/24 + 10*qn)]
>            , line [rest en, b 4 (10*qn)]]
>     l109        =
>       rest (10*qn)
> -- checked
>
>
>     percLine    = line [percLineI, p001_004, p005_008, p009_012, p013_016
>                                  , p017_020, p021_024, p025_028, p029_032
>                                  , p033_036, p037_040, p041_044, p045_048
>                                  , p049_052, p053_056, p057_060, p061_064
>                                  , p065_068, p069_072, p073_076, p077_080
>                                  , p081_084, p085_088, p089_092, p093_096
>                                  , p097_100, p101_104, p105_109]
>     leadLine    = line [leadLineI, l001_004, l005_008, l009_012, l013_016
>                                  , l017_020, l021_024, l025_028, l029_032
>                                  , l033_036, l037_040, l041_044, l045_048
>                                  , l049_052, l053_056, l057_060, l061_064
>                                  , l065_068, l069_072, l073_076, l077_080
>                                  , l081_084, l085_088, l089_092, l093_096
>                                  , l097_100, l101_104, l105_109]

DH ====================================================================================================================

PG ====================================================================================================================

> pgTempo                :: Dur
> pgTempo                                  = 1
> pgTranspose            :: AbsPitch
> pgTranspose                              = 0
>
> pgLead_                :: BandPart
> pgLead_                                  = makePitched ElectricGuitarClean               pgTranspose     0     100
> pgBass_                :: BandPart
> pgBass_                                  = makePitched SynthBass1 pgTranspose     0     100
> pgPerc_                :: BandPart
> pgPerc_                                  = makeNonPitched                                         100
>
> packardGoose           :: Map InstrumentName InstrumentName → Music (Pitch, [NoteAttribute])
> packardGoose dynMap                      =
>   removeZeros
>   $ tempo pgTempo
>   $ chord [leadMusic, bassMusic, percMusic]
>
>   where
>
>     pgLead                               = replace pgLead_ dynMap
>     pgBass                               = replace pgBass_ dynMap
>     pgPerc                               = replace pgPerc_ dynMap
>
>     leadMusic = bandPart pgLead leadLine
>     bassMusic = bandPart pgBass bassLine
>     percMusic = bandPart pgPerc percLine
>
>     lead025_028 =
>       line [line [fs 4 en, times 10 (fs 4 sn)]
>           , line [fs 4 en, times 4 (fs 4 sn), fs 4 en, ascendFrom pgLead (F,4) F chromatic hn]
>           , line [rest (dhn + en), as 5 en, tempo (5/4) (line [gs 5 sn, as 5 sn, gs 5 sn, fs 5 en])]]
>     lead029_032 =
>       line [line [t32 [fs 5 sn, gs 5 sn, fs 5 sn], e 5 en, t32 [e 5 qn, as 5 sn, b 4 sn], gs 5 qn]
>           , line [t32 [as 5 qn, gs 5 qn, fs 5 qn], fs 5 sn, gs 5 sn, fs 5 sn, e 5 sn]
>           , line [fs 5 dwn]]
>     lead033_036 =
>       line [line [fs 5 qn, as 5 qn, d 5 qn]
>           , tempo (5/6) (line [gs 5 sn, fs 5 sn, e 5 en, grace (-2) (as 5 en), gs 5 en, grace (-2) (gs 5 en)])
>           , tempo (4/3) (line [gs 5 sn, fs 5 en, fs 5 sn, fs 5 sn, e 5 den, fs 5 qn, e 5 qn])
>           , line [fs 4 en, cs 4 sn, gs 5 sn, fs 5 sn, e 5 dsn, as 5 sn, fs 5 sn, e 5 dsn, b 5 en]]
>     lead037_040 =
>       line [t32 [addDur en [b 5, as 5, as 5, as 5, gs 5, gs 5, gs 5, fs 5, fs 5]]
>           , line [addDur tn [fs 5, gs 5, fs 5, e 5], ds 5 en, grace (-2) (ds 5 qn), cs 5 qn]
>           , tempo (5/6) (addDur en [cs 5, ds 5, e 5, ds 5, ds 5])
>           , line [e 5 sn, ds 5 en, ds 5 sn, e 5 sn, ds 5 sn, b 4 en, b 4 sn, cs 5 sn, ds 5 en]] 
>     lead041_044 =
>       line [line [addDur en [ds 5, cs 5, cs 5, cs 5, cs 5, ds 5]]
>           , line [e 5 dhn]
>           , line [cs 5 en, ds 5 en, a 4 en, t32 [fs 4 sn, e 4 sn, cs 4 sn], e 4 qn]
>           , line [addDur sn [e 4, fs 4, e 4, cs 4], grace 2 (b 3 hn)]]
>     lead045_048 =
>       line [line [b 3 den, fs 3 sn, b 3 en, as 3 en, addDur sn [b 3, as 3, b 3, as 3]]
>           , line [fs 3 dhn]
>           , tempo (5/6) (line [fs 3 en, t32 [fs 4 qn, fs 4 en, fs 4 qn, fs 4 en]])
>           , tempo (5/6) (line [t32 [fs 4 qn, fs 4 en, fs 4 qn, fs 4 sn, e 4 sn], cs 4 en])]
>     lead049_052 =
>       line [line [e 4 en, fs 4 qn, tempo (4/3) (t32 [fs 4 qn, e 4 en, fs 4 qn, e 4 en])]
>           , line [times 6 (fs 4 en)]
>           , line [fs 4 sn, e 4 (den + en), tempo (5/3) (addDur en [e 4, fs 4, e 4, fs 4, e 4])]
>           , tempo (7/6) (times 7 (fs 4 en))]
>     lead053_056 =
>       line [line [fs 4 en, t32 [fs 4 sn, e 4 sn, cs 4 sn, e 4 qn, e 4 en, fs 4 qn, e 4 en]]
>           , line [fs 4 qn, e 4 sn, fs 4 den, fs 4 en, fs 4 en]
>           , line [fs 4 en, b 4 en, as 4 den, fs 4 sn, as 4 sn, fs 4 sn, e 4 en]
>           , line [e 4 en, grace 5 (b 3 (en + hn))]]
>     lead057_060 =
>       line [line [gs 3 en, fs 3 en, t32 [gs 3 qn, gs 3 en], gs 3 qn]
>           , line [as 3 qn, gs 3 en, gs 3 sn, gs 3 sn, grace (-2) (as 3 qn)]
>           , line [as 3 hn, grace (-1) (b 3 en), as 3 en]
>           , line [grace (-1) (b 3 en), as 3 qn, b 3 en, as 3 en, e 3 en]]
>     lead061_064 =
>       line [line [ e 3 qn, fs 3 en, fs 3 dqn]
>           , line [fs 3 hn, times 4 (chord [fs 5 qn, grace (-2) (ds 5 qn)])]
>           , line [chord [b 5 qn, fs 5 qn, grace (-2) (ds 5 qn)]
>                 , chord [b 5 qn, fs 5 qn, grace 2 (cs 4 qn)]
>                 , chord [b 5 qn, fs 5 qn, cs 5 qn]]]
>     lead065_068 =
>       line [line [chord [b 5 qn, fs 5 qn, cs 5 qn]
>                 , chord [chord [b 5 hn, fs 5 hn]
>                        , line [grace (-2) (ds 5 en), cs 5 en, t32 [ds 5 en, cs 5 en, b 4 en]]]]
>           , chord [fs 5 dhn, cs 5 dhn]
>           , chord [line [tempo (5/4) (times 5 (fs 5 en)), fs 5 qn]
>                  , line [tempo (5/4) (line [grace (-2) (ds 5 en), times 4 (ds 5 en)])]]
>           , chord [line [grace (-2) (ds 5 dqn), grace (-2) (ds 5 dqn)]
>                  , line [fs 5 dqn, fs 5 dqn]]]
>     lead069_072 =
>       line [t32 [chord [line [times 2 (grace (-2) (ds 5 qn)), cs 5 qn]
>                       , line [fs 5 qn, fs 5 qn, fs 5 qn]
>                       , line [rest hn, b 5 qn]]]
>           , chord [ b 5 (qn + dhn + en)
>                  , fs 5 (qn + dhn + en)
>                  , cs 5 (qn + dhn + en)]
>           , chord [grace (-2) (ds 5 en)
>                  , fs 5 (en + hn)]
>           , chord [line [fs 5 hn, fs 5 qn]
>                  , line [grace (-2) (ds 5 en), cs 5 en, b 4 qn, grace (-2) (ds 5 en)]]]
>     lead073_076 =
>       line [t32 [chord [line [fs 5 en, rest qn, fs 5 qn, fs 5 qn, fs 5 qn]
>                       , line [grace (-2) (ds 5 en), cs 5 en, b 4 en
>                             , grace (-2) (ds 5 qn), cs 5 qn, grace 2 (cs 5 en)]
>                       , line [rest (dqn + en), b 5 en, cs 5 en]]]
>           , chord [cs 6 dhn, fs 5 dhn, cs 5 dhn]
>           , chord [line [b 4 en, fs 4 en, rest en, fs 5 dqn]
>                  , line [rest dqn, grace (-2) (ds 5 dqn)]]
>           , chord [line [fs 5 dqn, fs 5 dqn]
>                  , line [times 2 (grace (-2) (ds 5 dqn))]]]
>     lead077_080 =
>       line [chord [line [fs 5 en, fs 5 hn, rest en]
>                  , line [ds 5 en, grace 2 (cs 5 hn), b 4 en]]
>           , tempo (7/6) (line [b 4 en, as 4 sn, g 4 sn, b 4 en, as 4 sn, g 4 sn, b 4 en, as 4 sn, g 4 sn, b 4 en])
>           , t32 [as 4 en, g 4 en, b 4 en, as 4 en, g 4 en, b 4 qn, as 4 en, g 4 sn, g 4 sn]
>           , line [t32 [grace (-1) (b 4 qn), as 4 en], grace (-2) (cs 5 en), b 4 sn, as 4 sn, b 4 en, rest en]]
>     lead081_084 =
>       line [line [b 4 en, t32 [b 4 sn, as 4 sn, g 4 sn], b 4 en, as 4 sn, g 4 sn
>                 , t32 [addDur sn [b 4, as 4, g 4, b 4, as 4, g 4]]]
>           , tempo (5/6) (line [b 4 en, fs 4 en, grace (-1) (g 4 en), fs 4 en, fs 4 en])
>           , addDur en [e 4, e 4, e 4, fs 4, g 4, gs 4]
>           , line [a 4 en, grace (-1) (as 4 en), b 4 en, cs 5 en, cs 5 en, d 5 en]]
>     lead085_088 =
>       line [line [fs 5 en, g 5 en, a 5 en, t32 [g 5 sn, a 5 sn, g 5 sn], fs 5 en, grace (-1) (g 5 en)]
>           , line [fs 5 en, e 5 en, fs 5 hn]
>           , line [fs 5 dqn, a 5 qn, g 5 en]
>           , line [a 5 en, t32 [a 5 sn, as 5 sn, a 5 sn], g 5 en, addDur sn [g 5, fs 5, fs 5, g 5, fs 5, e 5]]]
>     lead089_092 =
>       line [line [t32 [grace (-1) (a 5 qn), a 5 en], grace (-1) (as 5 en), a 5 dqn]
>           , line [as 5 hn, g 4 en, grace (-1) (as 5 en)]
>           , line [as 5 en, a 5 en, a 5 en, as 5 sn, a 5 (sn + qn)]
>           , line [trill 1 tn (cs 5 qn), t32 [addDur sn [b 5, cs 6, b 5]]
>                 , a 5 sn, b 5 sn, t32 [grace 2 (a 5 qn), a 5 en]]]
>     lead093_096 =
>       line [line [grace (-2) (g 5 dqn), b 3 dqn]
>           , line [g 5 sn, a 5 sn, g 5 en, g 5 en, fs 5 dqn]
>           , line [fs 5 (qn + den), chord [cs 4 sn, cs 5 sn], addDur sn [e 5, e 5, e 5, cs 5]]
>           , line [times 6 (e 5 sn), times 6 (chord [e 5 sn, cs 5 sn])]]
>     lead097_100 =
>       line [line [times 8 (chord [e 5 sn, cs 5 sn]), times 2 (chord [e 5 sn, ds 5 sn])
>                 , times 2 (chord [e 5 sn, cs 5 sn])]
>           , line [times 2 (chord [e 5 den, ds 5 den]), times 6 (chord [e 5 sn, cs 5 sn])]
>           , line [times 24 (chord [e 5 sn, cs 5 sn])]]
>     lead101_104 =
>       line [line [times 2 (chord [e 5 sn, cs 5 sn]), fs 3 sn, g 3 sn, a 3 en, chord [a 4 sn, fs 4 sn]
>                 , addDur sn [b 4, cs 5, a 4, gs 4, fs 4]]
>           , line [e 5 en, e 4 en, b 3 hn, cs 4 hn, b 3 qn, grace (-2) (fs 3 dhn)]]
>     lead105_108 =
>       line [fs 3 (dhn + dhn + qn), b 6 qn, rest en, cs 6 en, b 5 dqn, as 5 en, t32 [rest qn, a 5 en]]
>     lead109_112 =
>       line [line [addDur en [gs 5, gs 5, gs 5, gs 5, as 5, as 5, gs 5, as 5, cs 6, as 5], as 5 qn]
>           , line [rest den, cs 6 sn, b 5 qn, as 5 qn, rest en, as 5 en
>                 , grace 2 (gs 5 en), gs 5 en, gs 5 en, as 5 en]]
>     lead113_116 =
>       line [line [t32 [gs 5 qn, gs 5 qn, gs 5 en, fs 5 en], fs 5 en, fs 5 en]
>           , line [gs 5 en, fs 5 en, tempo (5/4) (line [fs 5 en, fs 5 en, t32 [fs 5 en, gs 5 en, fs 5 en], e 5 en])]
>           , t32 [e 5 qn, e 5 en, ds 5 en, b 4 qn, ds 5 en, fs 5 en, ds 5 en]
>           , line [t32 [ds 5 qn, grace 2 (cs 5 en)], addDur en [cs 5, cs 5, ds 5, fs 5]]]
>     lead117_120 =
>       line [line [chord [fs 5 (dhn + hn), grace 2 (cs 5 (dhn + hn))], chord [ds 5 den, as 4 den], fs 4 sn]
>           , line [addDur sn [b 4, fs 4, b 4, fs 4, b 4, cs 5, ds 5], grace (-2) (ds 5 sn), cs 5 qn]
>           , line [cs 5 dqn, gs 5 qn, fs 5 en]]
>     lead121_124 =
>       line [gs 5 en, fs 5 en, gs 5 en, fs 5 en, grace (-2) (as 5 (qn + dhn + en))
>           , t32 [b 5 sn, as 5 sn, gs 5 sn], b 6 qn, b 4 qn, cs 6 qn, rest dqn, fs 5 en]
>     lead125_128 =
>       line [gs 5 dhn, cs 5 qn, times 2 (grace (-2) (gs 6 qn))
>           , t32 [gs 5 qn, fss 5 en], gss 5 dhn, fs 5 en, fs 5 en, e 5 qn]
>     lead129_132 =
>       line [grace (-2) (gs 5 qn), grace (-2) (gs 5 en), fs 5 en, grace (-2) (as 5 qn), as 5 hn, b 5 en
>           , as 5 (en + sn), t32 [grace 2 (gs 5 qn), fs 5 en
>                                , chord [line [e 5 qn, cs 5 qn, cs 5 dqn]
>                                       , line [fs 5 qn, fs 5 en, e 5 en, fs 5 en, e 5 en]]]
>          , chord [b 4 en, cs 4 en], as 4 en]
>     lead133_136 =
>       line [tempo (2/3) (line [e 5 qn, chord [cs 5 qn, fs 5 qn], grace (-2) (gs 5 en), gs 5 sn, gs 5 sn, gs 5 en
>                             , grace (-3) (gss 5 en), gs 5 sn, gs 5 sn, gs 5 en, grace (-2) (gs 5 en)
>                             , times 2 (chord [cs 6 sn, gs 5 sn])])
>           , addDur en [gs 5, fs 5, gs 5, gs 5, gs 5, fs 5]]
>     lead137_138 =
>       line [grace (-2) (as 5 en), b 5 sn, as 5 sn, gs 5 en, as 5 en, as 5 (qn + sn), cs 6 en
>           , grace (-3) (css 6 (sn + qn)), b 5 sn, as 5 sn, gs 5 en]
>     lead139_143 =
>       line [addDur sn [as 5, gs 5, fs 5, cs 5, cs 5, ds 5], cs 6 en, t32 [grace (-3) (e 6 qn), ds 6 qn, cs 6 qn]
>           , tempo (7/6) (line [addDur qn [fs 6, e 6, ds 6, cs 6, fs 6, e 6, ds 6]])
>           , t32 [grace (-1) (e 6 sn), e 5 sn, cs 6 sn, b 5 sn, cs 6 en], b 5 dqn, cs 6 en, grace (-2) (cs 6 dqn)
>           , rest en]
>     lead144_149 =
>       line [line [tempo (5/6) (line [fs 5 en, e 5 en, fs 5 en, fs 5 en, grace (-2) (gs 5 en)])]
>           , line [gss 5 dqn, fs 5 tn, e 5 tn, cs 5 tn, rest tn, cs 5 hn
>                 , t32 [cs 5 qn, grace (-2) (cs 5 en)], cs 5 qn]
>           , line [e 5 (dhn + dhn + qn), b 4 qn, t32 [as 4 qn, fs 4 en]]]
>     lead150_153 =
>       line [line [e 4 hn, fs 4 qn]
>           , chord [line [grace (-2) (cs 5 sn), times 15 (cs 5 sn), grace (-2) (ds 5 en), times 10 (ds 5 sn)
>                        , times 8 (cs 5 sn)]
>                  , line [times 16 (e 5 sn), grace (-2) (fs 5 en), times 10 (fs 5 sn), times 8 (e 5 sn)]]]
>     lead154_155 =
>       chord [line [e 5 en, ds 5 en, e 5 en, ds 5 qn
>                  , tempo (8/7) (line [grace (-1) (e 5 en), ds 5 en, cs 5 en, b 4 en
>                                     , grace (-1) (e 5 en), ds 5 en, cs 5 en, ds 5 en])]
>            , line [cs 5 en, b 4 en, c 5 en, rest qn]
>                  , tempo (8/7) (line [grace (-2) (cs 5 en), b 4 en, as 4 en, rest en
>                                     , grace (-2) (cs 5 en), b 4 en, as 4 en, b 4 en])]
>     lead156_162 =
>       chord [line [t32 [e 5 en, e 5 en, e 5 en], e 5 sn, e 5 sn, e 5 sn, e 5 (sn + qn)
>                  , grace (-1) (e 5 hn), ds 5 (qn + dqn), e 5 en, ds 5 (qn + dqn), ds 5 en, fs 5 en, fs 5 (en + hn)
>                  , e 5 en, g 4 (en + dqn), fs 3 (en + qn + qn), t32 [fs 3 qn, e 6 en, ds 6 qn, cs 6 en]]
>            , line [t32 [grace (-2) (cs 5 en), rest qn], rest sn, b 4 sn, as 4 sn, b 4 (sn + qn)
>                  , grace (-2) (cs 5 hn), b 4 (qn + dqn), cs 5 en, b 4 (qn + dqn), ds 5 en, rest dhn
>                  , cs 5 en, d 4 (en + dqn), rest (dqn + dhn)]]
>     lead163_164 =
>       line [cs 6 hn, rest en, fs 6 en, tempo (2/3) (line [t32 [fs 6 qn, e 6 en], e 6 en, rest en])]
>     lead165_168 =
>       line [gs 6 qn, gs 6 en, fs 6 en, grace (-2) (as 6 qn), grace (-2) (as 6 en), as 6 en
>           , tempo (9/8) (line [times 4 (line [as 6 en, gs 6 en]), as 6 en]), gs 6 en, fs 6 en
>           , grace (-3) (ass 6 qn), chord [fs 6 hn, b 6 hn]]
>     lead169_172 =
>       line [line [ds 6 qn, fs 5 en, grace (-2) (as 5 en), t32 [rest en, ass 5 en, ass 5 en]]
>           , line [as 5 en, a 5 en, as 5 wn, chord [grace (-2) (as 5 qn), grace (-2) (as 5 qn)]]
>           , tempo (2/3) (line [t32 [e 5 en, fs 5 qn], fs 5 en, e 5 en])]
>     lead173_178 =
>       line [line [fs 5 (dhn + en), as 4 sn, gs 4 sn, fs 6 qn, rest qn]
>           , line [rest qn, fs 3 en, gs 3 en, t32 [times 5 (gs 3 en), gss 3 en, gss 3 en, a 3 en, a 3 en]
>                 , as 3 en, grace (-1) (b 3 den), grace(-2) (b 3 den)
>                 , grace (-1) (b 3 qn), grace (-3) (cs 4 dhn), fs 3 qn]]
>     lead179_184 =
>       line [line [grace 1 (fs 3 dhn), rest en, as 3 en, gs 3 dqn, b 3 en, as 3 hn, rest qn]
>           , line [t32 [rest en, b 3 en, b 3 en], tempo (5/4) (line [addDur en [b 3, b 3, b 3, c 4, c 4]])]
>           , chord [line [grace (-2) (gss 3 hn), fs 3 qn]
>                  , line [grace (-1) (cs 4 hn), b 3 qn]
>                  , line [fs 4 dhn]]
>           , rest dhn]
>     lead185_188 =
>       line [rest dhn, tempo (10/6) (line [rest qn, times 8 (b 3 en)])
>           , t32 [b 3 qn, grace (-2) (cs 4 qn), grace 2 (b 3 qn)], cs 4 sn, b 3 den, cs 4 hn, b 3 qn]
>     lead189_192 =
>       line [line [rest (dhn + hn + en), grace (-2) (as 3 en)]
>           , line [gs 3 qn, gs 3 en, fs 3 en, grace 1 (as 3 qn)]
>           , line [gs 3 en, fs 3 en, tempo (5/4) (line [grace (-2) (as 3 en), addDur en [gs 3, fs 3, e 3, e 3]])]]
>     lead193_196 =
>       line [t32 [addDur qn [e 3, e 3, e 3], addDur en [e 3, fs 3, gs 3]]
>           , line [fs 3 hn, rest en, chord [g 4 en, b 4 en]]
>           , line [rest en, grace (-2) (as 3 en), rest qn, gs 3 qn]
>           , line [gs 3 sn, e 3 den, rest dqn, e 3 en]]
>     lead197_201 =
>       line [line [rest en, e 3 en, rest en, e 3 dqn]
>           , line [fs 3 en, fs 3 en, rest hn]
>           , line [rest sn, as 3 sn, gs 3 en, gs 3 qn, fs 3 en, e 3 en]
>           , line [rest dhn]
>           , line [e 3 en, e 3 en, e 3 sn, gs 3 sn, fs 3 en, grace (-2) (gs 3 en), gs 3 sn, fs 3 sn]]
>     lead202_204 =
>       line [line [e 3 en, fs 3 en, ascendFrom pgLead (Fs, 3) Fs chromatic hn]
>           , line [rest en, grace 2 (gs 3 hn), as 3 en]
>           , line [grace (-3) (b 3 dqn), grace (-4) (bs 3 dqn)]]
>     lead205_207 =
>       line [grace (-3) (cs 4 (dhn + hn)), b 3 qn, rest dqn, cs 4 en, cs 4 sn, cs 4 sn, cs 4 en]
>     lead208_212 =
>       line [line [css 4 sn, css 4 sn, css 4 sn, d 4 sn, d 4 sn, ds 4 sn, dss 4 sn, e 4 sn
>                 , tempo (5/4) (line [addDur sn [e 4, e 4, es 4, f 4, f 4]])]
>           , line [addDur sn [fs 4, fs 4, fs 4, ds 4], tempo (5/4) (line [addDur sn [ds 4, ds 4, e 4, ds 4, fs 3]])
>                 , fs 3 qn]
>           , line [fs 3 en, fs 3 en, t32 [fs 3 qn, e 3 hn]]
>           , line [t32 [grace (-2) (as 3 qn), gs 3 en], grace (-2) (a 3 hn)]
>           , line [rest qn, tempo (5/4) (addDur en [cs 4, cs 4, cs 4, cs 4, gss 3])]]
>     lead213_216 =
>       line [t32 [cs 4 en, cs 4 en, gs 3 en, cs 4 en, cs 4 en, gs 3 qn, gs 3 qn]
>           , line [rest qn, fs 3 dqn, fs 3 sn, fs 3 sn]
>           , t32 [addDur en [gs 3, gs 3, gs 3, gs 3, gss 3, gss 3, gss 3, gss 3, fs 3]]
>           , line [fs 3 en, fs 3 en, e 4 en, e 4 sn, e 4 sn
>                 , tempo (5/4) (line [addDur sn [e 4, e 4, es 4, f 4, fs 4]])]]
>     lead217_220 =
>       line [line [fs 4 sn, g 4 sn, gs 4 sn, a 4 sn, as 4 sn, cs 5 sn, as 4 sn, cs 5 sn, g 4 sn, e 4 sn, e 4 en]
>           , line [rest qn, cs 4 sn, css 4 sn, d 4 sn, ds 4 sn, tempo (5/4) (line [addDur sn [e 4, e 4, es 4, f 4, f 4]])]
>           , line [fs 4 sn, fss 4 sn, g 4 sn, g 4 sn, f 4 sn, e 4 (den + en), rest en]
>           , line [t32 [e 5 qn, fs 6 en, e 5 sn, gs 4 en], fs 5 en, b 5 sn, e 5 en, es 5 sn]]
>     lead221_224 =
>       line [t32 [gs 5 qn, cs 6 en, grace 2 (gs 5 en), e 5 en, cs 6 en, b 4 en, cs 4 en, b 4 en]
>           , t32 [b 3 en, cs 4 en, a 3 en, cs 4 en, gs 3 en, cs 5 en, fs 3 en, b 3 en, e 5 en]
>           , line [e 5 sn, e 4 en, e 5 en, ds 4 sn, e 4 en, rest en, fs 4 sn, fss 3 sn]
>           , line [fs 3 dhn]]
>     lead225_228 =
>       line [e 3 dwn, rest dwn]    
>
>     leadLine = line [rest (24 * dhn), lead025_028, lead029_032, lead033_036, lead037_040, lead041_044
>                       , lead045_048, lead049_052, lead053_056, lead057_060, lead061_064, lead065_068
>                       , lead069_072, lead073_076, lead077_080, lead081_084, lead085_088, lead089_092
>                       , lead093_096, lead097_100, lead101_104, lead105_108, lead109_112, lead113_116
>                       , lead117_120, lead121_124, lead125_128, lead129_132, lead133_136, lead137_138
>                       , lead139_143, lead144_149, lead150_153, lead154_155, lead156_162, lead163_164
>                       , lead165_168, lead169_172, lead173_178, lead179_184, lead185_188, lead189_192
>                       , lead193_196, lead197_201, lead202_204, lead205_207, lead208_212, lead213_216
>                       , lead217_220, lead221_224, lead225_228]
>
>     bassLine = times 28 (line [pgBassI, pgBassII])
>       where
>         pgBassI =
>           line [fs 2 en, fs 2 en, fs 2 en, rest (dqn + dhn), fs 2 en, fs 2 en, rest hn, fs 2 en, rest qn, e 3 dqn]
>         pgBassII =
>           line [ e 2 en,  e 2 en,  e 2 en, rest (dqn + dhn),  e 2 en,  e 2 en, rest hn,  e 2 en, rest qn, b 2 dqn]
>
>     percLine = line [rest (8 * dhn), perc009_012, perc013_016, perc017_020, perc021_024, perc025_028
>                       , perc029_032, perc033_036, perc037_040, perc041_044, perc045_048, perc049_052
>                       , perc053_056, perc057_060, perc061_064, perc065_068, perc069_070, perc071_072
>                       , perc073_076, perc077_080, perc081_082, perc083_084, perc085_088, perc089_092
>                       , perc093_096, perc097_100, perc101_104, perc105_108, perc109_112, perc113_116
>                       , perc117_120, perc121_124, perc125_128, perc129_132, perc133_137, perc138_142
>                       , perc143_144, perc145_148, perc149_153, perc154_156, perc157_160, perc161_164
>                       , perc165_168, perc169_172, perc173_178, perc179_184, perc185_188, perc189_192
>                       , perc193_196, perc197_201, perc202_204, perc205_207, perc208_212, perc213_216
>                       , perc217_220, perc221_224, perc225_228]
>
>     perc009_012 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percSDqn, rest hn]
>                  , line [percCHHqn, percCHHen, percCHHsn, percCHHsn, percCHHen, percCHHsn, percCHHsn]]
>           , line [percCHHen, percCHHsn, percCHHsn
>                 , percCHHen, percCHHsn, percCHHsn
>                 , percCHHen, percCHHsn, percCHHsn]
>           , chord [ line [percCHHen, percCHHsn, percCHHsn
>                         , percCHHen, percCHHsn, percCHHsn
>                         , percCHHen, percCHHsn, percCHHsn]
>                    , line [percBDen, percBDen, rest hn]]
>           , chord [ line [percBDqn, rest en, percBDen, rest qn]
>                   , line [percCHHsn, percCHHsn, percCHHsn, percCHHsn
>                         , percCHHen, percCHHsn, percCHHsn
>                         , percCHHen, percCHHsn, percCHHsn]]]
>     perc013_016 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percCHHsn, percCHHsn, percCHHsn, percCHHsn
>                         , percCHHen, percCHHen
>                         , percCHHen, percCHHsn, percCHHsn]]
>           , line [percCHHen, percCHHsn, percCHHsn, percCHHen, percCHHsn, percCHHsn
>                 , percCHHsn, percCHHsn, percCHHsn, percCHHsn]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCHHen, percCHHsn, percCHHsn
>                        , percCHHen, percCHHsn, percCHHsn
>                        , percCHHen, percCHHsn, percCHHsn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percCHHen, percCHHen
>                         , percCHHen, percCHHsn, percCHHsn
>                         , percCHHen, percCHHsn, percCHHsn]]]
>
>     perc017_020 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percCHHen, percCHHen, percCHHen, percCHHsn, percCHHsn, percCHHen, percCHHsn, percCHHsn]]
>           , line [percCHHsn, percCHHsn, percCHHsn, percCHHsn
>                 , percOHHen, times 6 percOHHsn]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [times 8 percOHHsn, percCHHen, percCHHen]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percCHHqn, rest qn, times 4 percCHHsn]]]
>
>     perc021_024 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn], line [times 3 percCHHen, times 6 percCHHsn]]
>           , line [times 12 percCHHsn]
>           , chord [line [percBDen, percBDen, rest hn], line [times 12 percOHHsn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn], line [times 12 percOHHsn]]]
>
>     perc025_028 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest wn]
>                  , line [percOHHqn, percOHHen, percOHHsn, percOHHsn, t32 [times 3 percOHHqn]
>                        , percOHHqn, percOHHen, percOHHen]]
>           , chord [line [percBDen, percBDen, rest hn, percBDqn, rest en, percBDen, rest qn]
>                  , line [percCCen, percCCen, percCCqn, perc PedalHiHat qn]]]
>
>     perc029_032 =
>       line [chord [line [percBDen, percBDen, percBDqn, perc PedalHiHat qn]
>                  , line [perc PedalHiHat qn, percLTen, percLTen
>                          , tempo (5/4) (line [perc LowMidTom en, perc HighTom sn, perc LowMidTom en])]]
>           , chord [line [times 3 (perc PedalHiHat qn)], line [percCCqn, times 4 percHTen]]
>           , chord [line [percBDen, percBDen, rest hn], line [perc PedalHiHat qn, rest en, percCHHen, percOHHqn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percOHHen, percCHHen, percOHHen, times 3 percCHHen]]]
>
>     perc033_036 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn], line [percCHHqn, perc RideCymbal1 hn]]
>           , line [t32 [percCHHqn, percOHHen], percCHHsn, perc RideCymbal1 den, perc RideCymbal1 qn]
>           , chord [line [percBDen, percBDen, rest hn], line [times 6 percCHHsn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn], line [times 4 percCHHsn, percRCen, percCHHen]]]
>
>     perc037_040 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percCHHqn, perc RideCymbal1 en, percCHHen, percHTsn, perc HiMidTom den]]
>           , line [percCHHen, percCHHen, t32 [perc HiMidTom qn, percHTqn, rest qn]]
>           , chord [line [percBDen, percBDen, rest hn], line [percCCen, percCCen, rest en, times 3 percCHHen]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn], line [percOHHqn, percCHHqn, percCHHqn]]]
>
>     perc041_044 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percCHHqn, perc HiMidTom en, perc HiMidTom en, t32 [percHTqn, perc HiMidTom en]]]
>           , line [rest en
>                 , tempo (5/4) (line [perc HiMidTom en, perc HiMidTom den, percHTen, perc HiMidTom den])
>                 , rest en]
>           , chord [line [percBDen, percBDen, rest hn], line [percCCqn, times 4 percCHHen]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percCHHen, percCHHen, percCHHqn, percCHHen, percCHHen]]]
>
>     perc045_048 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percCHHen, percLTsn, percLTsn, perc LowMidTom qn, rest qn]]
>           , line [rest qn, t32 [percm HighFloorTom [en, en, en]], percm HighFloorTom [en, en]]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest qn, percCHHen, percCHHen]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percCHHqn, percCHHqn, percCHHen, percCHHen]]]
>
>     perc049_052 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percCHHen, percCHHen, percCHHqn, rest qn]]
>           , line [rest qn, times 4 (chord [perc HighFloorTom en, percSDen])]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest qn, percCHHen, percCHHen]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percm ClosedHiHat [en, en, en, en, en, en]]]]
>
>     perc053_056 =
>       line [chord [line [percBDen, percBDen
>                        , tempo (6/5) (line [percBDen, percSDen, percSDen, perc HiMidTom en, perc LowMidTom en
>                                           , perc HiMidTom en, percSDen, perc LowMidTom en, perc HiMidTom en
>                                           , perc LowMidTom en, percSDen, rest en])]
>                  , line [percCHHqn, tempo (6/5) (line [percOHHqn, percOHHqn, rest wn])]]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest qn, percOHHen, percOHHen]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percCHHqn, times 4 percCHHen]]]
>
>     perc057_060 =
>       line [chord [line [percBDen, percBDen]
>                  , percOHHqn]
>           , tempo (6/5) (chord [line [times 11 percBDen,                              rest en]
>                               , line [percm LowTom      [qn, qn, en, qn, qn, en, en], rest en]
>                               , line [percm ClosedHiHat [qn, qn, en, qn, qn, en, en], rest en]])
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest qn, perc PedalHiHat qn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percSDqn, rest en, percm LowTom [en, en, en]]
>                  , line [times 3 (perc PedalHiHat qn)]]]
>
>     perc061_064 =
>       line [chord [line [percBDen, percBDen]
>                  , perc PedalHiHat qn]
>           , tempo (6/5) (chord [line [percBDqn, rest (wn + qn)]
>                               , line [percm AcousticSnare [en, en, sn, sn, en, en, en, sn, sn, en, en, en], rest qn]])
>           , chord [line [percBDen, percBDen, rest dqn, percBDen]
>                  , line [percCCen, percCCen, rest qn, percCHHqn]]
>           , chord [line [percBDqn, percSDen, percBDen, rest qn]
>                  , line [percSDqn, rest hn]
>                  , line [percCHHqn, rest en, percCHHen, rest qn]]]
>
>     perc065_068 =
>       line [tempo (7/4) (chord [line [percBDen, percLTen, percBDen, percLTen, percBDen, percLTen, percLTen
>                                     , percBDen, percLTen, percBDen, percLTen, percBDen, percLTen, percLTen
>                                     , percBDen, percLTen, percBDen, percBDen
>                                     , tempo (4/3) (times 4 percSDsn)]
>                               , line [percm ClosedHiHat [qn, qn, dqn, qn, qn, dqn, qn, qn], rest dqn]])
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest qn, perc PedalHiHat qn]]
>           , chord [line [percBDqn, percSDen, percBDen, rest qn]
>                  , line [percSDqn, rest hn]
>                  , line [percCHHqn, rest en, percCHHen, rest qn]]]
>
>     perc069_070 =
>       tempo (5/4) (line [perc HighFloorTom en, percBDen, perc HighFloorTom en, percBDen, percBDen, percBDen
>                        , perc HighFloorTom en, percBDen, perc HighFloorTom en, percBDen, percBDen
>                        , perc HighFloorTom en, percBDen, percSDen, percSDen])
>
>     perc071_072 =
>       line [chord [line [percBDen, percBDen, rest hn]
>                  , line [percCHHen, percCHHen, rest qn, perc PedalHiHat qn]]
>           , chord [line [percBDqn, percSDen, percBDen, rest qn]
>                  , line [percSDqn, rest hn]
>                  , line [percCHHqn, rest en, percCHHen, percOHHqn]]]
>
>     perc073_076 =
>       line [tempo (7/6) (line [percBDen, percSDen, percSDen, percBDen, percBDen, percBDen, percSDen
>                               , percSDen, percBDen, percBDen, percBDen, percSDen, percSDen, rest en])
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest qn, perc PedalHiHat qn]]
>           , chord [line [percBDqn, percSDen, percBDen, rest en, percSDen]
>                  , line [percCHHqn, rest en, perc ClosedHiHat dqn]]]
>
>     perc077_080 =
>       line [chord [line [rest qn, t32 [percHTen, percHTen, percLTen, times 3 percBDen]]
>                  , line [percCHHqn, percCCqn, rest qn]]
>           , line [perc LowTom den, percOHHsn, times 2 percLTsn, rest dqn]
>           , chord [line [percBDen, percBDen, rest dqn, percBDen]
>                  , line [percCCen, percCCen, rest qn, percOHHqn]]
>           , chord [line [percBDqn, percSDen, percBDen, rest qn]
>                  , line [percOHHqn, rest en, percOHHen, percOHHqn]]]
>
>     perc081_082 =
>       chord [line [chord [percBDen, percLTen], t32 [percLTsn, percLTsn, percBDsn], percSDen, percHTsn, percHTsn
>                  , tempo (5/6) (line [percHTen, percHTen, percBDen, percLTen, percLTen]), percLTqn]
>            , line [perc PedalHiHat qn, rest sn, perc CrashCymbal1 den
>                  , tempo (5/6) (line [percCCqn, rest dqn]), percHTqn]]
>
>     perc083_084 =
>       chord [line [percBDen, percBDen, rest dqn, percBDen, chord [percBDqn, percSDqn], percSDen, percBDen, rest qn]
>            , line [percCCen, percCCen, rest qn, perc PedalHiHat qn, percOHHqn, percOHHen, percOHHen, percOHHqn]]
>
>     perc085_088 =
>       line [chord [line [percBDen, percBDen, percBDen, tempo (4/3) (times 4 percHTen)]
>                  , line [percOHHqn, rest en, percCCen, rest qn]]
>           , tempo (2/3) (chord [line [times 4 percHTsn, percHTsn, perc HighTom den]
>                               , line [percOHHqn, rest sn, perc OpenHiHat den]])
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest qn, perc PedalHiHat qn]]
>           , chord [line [chord [percBDqn, percSDqn], percSDqn, rest qn]
>                  , line [percOHHqn, rest en, percCHHen, rest qn]]]
>
>     perc089_092 =
>       line [chord [line [percBDen, percBDen, t32 [rest qn, percBDen], times 3 percSDsn, percBDsn]
>                  , line [percCHHqn, percOHHqn, rest qn]]
>           , chord [line [times 4 percLTsn, t32 [percLTen, percBDqn], percBDqn]
>                  , line [rest hn, percCHHqn]]
>           , chord [line [percBDen, percBDen, rest dqn, percBDen]
>                  , line [percCCen, percCCen, rest qn, perc PedalHiHat qn]]
>           , chord [line [percSDqn, percSDen, percBDen, rest qn]
>                  , line [percCHHqn, rest en, percCHHen, percCHHqn]]]
>
>     perc093_096 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest en, tempo (5/3) (percm LowTom [en, en, en, en, en])
>                        , times 4 percLTen]
>                  , line [percCCqn, percCCqn, rest wn]]
>           , line [times 2 (chord [percBDen, percLTen, percCCen]), rest qn, percCHHqn]
>           , chord [line [percBDqn, percSDen, percBDen, rest qn]
>                  , line [percOHHqn, rest en, percOHHen, rest qn]]]
>
>     perc097_100 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percOHHqn, percCHHqn, perc PedalHiHat qn]]
>           , chord [line [perc LowTom dhn]
>                  , line [times 3 (perc PedalHiHat qn)]]
>           , chord [line [percBDen, percBDen, rest dqn, percBDen]
>                  , line [chord [percCCen, perc PedalHiHat en], percCCen, rest hn]]
>           , chord [line [percBDqn, percSDen, percBDen, rest qn]
>                  , line [percCCqn, rest en, percOHHen, rest qn]]]
>
>     perc101_104 =
>       line [chord [line [percBDen, percBDen, percBDen, percLTqn, rest en]
>                  , line [times 3 (perc PedalHiHat qn)]]
>           , line [times 9 (perc PedalHiHat qn)]]
>
>     perc105_108 =
>       line [times 12 (perc PedalHiHat qn)]
>
>     perc109_112 =
>       line [chord [line [percBDen, percBDen, percBDen, rest dqn]
>                  , line [times 3 (perc PedalHiHat qn)]
>                  , line [percCCen, times 5 percRCen]]
>           , chord [line [times 3 (perc PedalHiHat qn)]
>                  , line [times 6 percRCen]]
>           , chord [line [percBDen, percBDen, rest hn, percBDqn, rest en, percBDen, rest qn]
>                  , line [times 6 (perc PedalHiHat en)]
>                  , line [times 12 percRCen]]]
>
>     perc113_116 =
>       line [line [rest qn, tempo (6/5)
>                             (tempo (5/4) (line [perc LowMidTom en, percHTen, perc LowMidTom en, perc LowMidTom en
>                                               , percBDen, perc LowMidTom en, percHTen, perc LowMidTom en
>                                               , perc LowMidTom en, percBDen, perc LowMidTom en, percHTen
>                                               , perc LowMidTom en, percBDen, percLTen]))]
>           , line [tempo (2/3)
>                     (tempo (5/4) (line [perc LowMidTom sn, percHTsn, percm LowMidTom [sn, sn], percLTsn
>                                      , perc LowMidTom sn, percHTsn, percm LowMidTom [sn, sn], percLTsn
>                                      , percm LowMidTom [sn, sn], percLTsn, perc LowMidTom sn, percLTsn
>                                      , perc LowMidTom sn, percHTsn, perc LowMidTom sn
>                                      , chord [percBDen, percLTen, percCCen]]))]]
>
>     perc117_120 =
>       line [chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, rest hn]]
>           , chord [line [t32 [times 3 percBDqn], percBDqn, percBDen, percBDen, rest hn]
>                  , line [t32 [times 3 percSDqn], percSDqn, rest dhn]
>                  , line [t32 [times 3 percCCqn], percCCqn, percCCen, percCCen, rest hn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percSDqn, rest hn]
>                  , line [times 3 percOHHqn]]]
>
>     perc121_124 =
>       line [chord [line [times 10 percBDen, t32 [percBDqn, percBDen]]
>                  , line [percCHHqn, percOHHqn, percOHHen, percCHHen, percOHHqn, percOHHqn, percCHHqn]]
>           , chord [line [percBDen, percBDen, t32 [percBDqn, percBDqn, rest en, percBDen]
>                        , percBDen, percBDen, rest en, percBDen, t32 [rest en, percBDen, percBDen]]
>                  , line [times 6 (perc PedalHiHat qn)]]]
>
>     perc125_128 =
>       chord [line [times 12 (line [rest en, percBDsn, percBDsn])]
>            , line [times 12 (perc PedalHiHat qn)]]
>
>     perc129_132 =
>       line [chord [line [times 3 (line [rest en, percBDsn, percBDsn])]
>                   , line [times 3 (perc PedalHiHat qn)]]
>           , chord [line [percSDqn, perc BassDrum1 den, percBDsn, t32 [percBDqn, percBDen]]
>                   , line [percCCqn, percOHHqn, t32 [percOHHqn, percOHHen]]]
>           , chord [line [rest qn, percBDqn, t32 [percBDqn, percBDen]]
>                  , line [times 3 percCHHqn]]
>           , chord [line [rest en, percBDen, rest den, percBDsn, percBDqn]
>                  , line [percCHHen, percCCen, percCHHen, percCCen, percCHHqn]]]
>
>     perc133_137 =
>       line [chord [line [percBDen, percBDen, percBDqn, percBDen, percBDen
>                        , rest en, percBDen, perc BassDrum1 den, percSDqn]
>                  , line [percOHHen, percCCen, percOHHqn, percRCen, percRCqn, percRCen, percRCqn, percRCen, percRCen]]
>           , chord [line [percBDen, percBDen, perc BassDrum1 den, percBDsn, percSDqn, percBDen, percBDen, rest en
>                        , percBDen, percSDen, percBDen, percBDen, percBDen, t32 [percBDqn, percBDen], percSDen, percBDen]
>                  , line [percm RideCymbal1 [en, en + den, sn, qn, en, qn, en, en, qn, qn, en, en, en]]]]
>
>     perc138_142 =
>       chord [line [rest en, percBDen, percBDen, percBDsn, percBDsn, percSDen, percBDen
>                  , percBDqn, percBDen, percBDsn, percBDsn, percSDen, percBDen
>                  , rest en, percBDen, percBDqn, percBDen, percBDen
>                  , percBDen, percBDen, times 3 percBDqn, percSDqn, rest en, percSDsn, percSDsn]
>            , line [percm RideCymbal1 [en, qn, en, en, qn, en, qn, en, qn, en, qn], percCCen, perc CrashCymbal1 dqn
>                 , times 3 percCCqn, percm PedalHiHat [qn, qn]]]
>
>     perc143_144 =
>       chord [line [percSDqn, percBDqn, percBDqn, percBDqn, percBDen, percBDen, percBDqn]
>            , line [percCHHqn, percCHHqn, percm PedalHiHat [qn, qn, qn, qn]]]
>
>     perc145_148 =
>       chord [line [percBDqn, rest en, percBDen, percBDqn
>                  , tempo (2/3) (line [percBDsn, percSDsn, percBDen, percBDsn, percSDsn, percBDen])
>                  , percBDqn, perc BassDrum1 den, percBDsn, percBDen, percSDen, percBDqn, percBDen, percBDen, rest qn]
>            , line [percm CrashCymbal1 [dqn, dqn]
>                  , tempo (2/3) (line [rest sn, percCCsn, percCCen, rest sn, percCCsn, percCCen])
>                  , percOHHqn, perc OpenHiHat hn, percCCqn, rest en, percCCen, percCCqn]]
>
>     perc149_153 =
>       line [chord [line [percBDsn, rest en, percBDsn, rest en, percBDsn, rest den, percBDsn, rest sn]
>                  , line [times 12 percLTsn]
>                  , line [percm CrashCymbal1 [den, den, qn, en]]]
>           , chord [line [rest en, percBDsn, rest den, percBDsn, rest sn, percBDsn, rest den]
>                  , line [times 5 percLTsn, percHTsn, times 2 percLTsn]]
>           , tempo (2/3) (t32 [percBDsn, percHTsn, chord [percLTsn, percCCsn], percHTsn, perc HighFloorTom en
>                            , percBDsn, percHTsn, chord [percLTsn, percCHHsn], percHTsn, percBDen
>                            , percBDsn, chord [percHTsn, percCCsn], percLTsn, percHTsn, percBDen
>                            , percBDsn, chord [percHTsn, percCCsn], percLTsn, percHTsn, percBDen])]
>
>     perc154_156 =
>       line [tempo (5/6) (chord [line [times 5 (line [percBDsn, rest den])]
>                               , line [times 5 (line [percCCsn, rest den])]])
>           , t32 [chord [line [times 5 percLTen, percHTen, percLTen, percLTen, percBDen]
>                       , line [perc ClosedHiHat dqn, rest dqn, perc CrashCymbal1 dqn]]]]
>
>     perc157_160 =
>       line [chord [line [t32 [percLTen, percBDen, percLTen, percLTen, percLTen, percHTen, percSDen, percLTen, percBDen]
>                        , percBDen, percBDen, percHTen, percBDen, t32 [rest en, percBDen, percBDen]]
>                  , line [percCHHqn, rest qn, percCCqn, percCCqn, rest qn, percCCqn]]
>           , chord [t32 [times 9 percBDqn]
>                  , t32 [times 9 percCCqn]]]
>
>     perc161_164 =
>       chord [line [percBDen, percBDen, percBDqn, rest dhn, percSDqn, percBDen, percBDen, rest hn
>                  , percBDqn, rest qn, percBDen, rest en]
>            , line [percLTen, percLTen, percLTqn, rest (10*qn)]
>            , line [percCCen, percCCen, percCCqn, percOHHqn, times 9 percCHHqn]]
>
>     perc165_168 =
>       chord [line [percBDen, percBDen, percBDqn, t32 [rest qn, percSDqn, percSDqn], percSDqn, rest qn
>                  , percBDen, percBDen, rest hn, percBDqn, percSDen, percBDen, rest qn]
>            , line [percCHHqn, percCHHqn, t32 [percOHHqn, times 2 (chord [percOHHqn, percCCqn])]
>                  , chord [percOHHqn, percCCqn], times 4 percOHHqn, times 3 percCHHqn]]
>
>     perc169_172 =
>       chord [line [rest qn, roll sn (perc AcousticSnare wn), rest qn, percBDen, percBDen, rest hn, percBDqn, rest hn]
>            , line [percm PedalHiHat [qn, qn, qn, qn, qn, qn], percCCen, percCCen, percm PedalHiHat [qn, qn]
>                  , percOHHqn, percOHHqn, percCCqn]]
>
>     perc173_178 =
>       line [line [percCHHqn, rest (hn + dhn)]
>           , chord [line [percBDen, percBDen, rest hn, percBDqn, rest en, percBDen, rest qn
>                        , percBDen, percBDen, percBDqn, rest qn, rest en, percBDen, rest hn]
>                  , line [percCHHen, percCHHen, rest hn, percOHHqn, rest en, percCHHen, rest en, percCHHen, percCHHen
>                        , percCHHqn, percCHHqn]]]
>
>     perc179_184 =
>       line [chord [line [percBDen, percBDen, rest hn] 
>                  , line [percCCen, percCCen, percCCqn, rest qn]]
>           , line [percBDqn, rest en, percBDen, rest qn, percBDen, percBDen, percBDqn, rest qn]
>           , line [rest qn, t32 [percHTqn, perc LowMidTom en], rest qn]
>           , chord [line [percBDen, percBDen, rest hn, percBDen, rest (en + hn)]
>                  , line [percCCen, percCCen, times 10 percCHHen]]]
>
>     perc185_188 =
>       line [chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [percCHHen, percCHHen, percCHHqn, rest qn]]
>           , line [t32 [perc PedalHiHat en, percCCqn], perc PedalHiHat hn]
>           , line [percCCen, percCCen, perc PedalHiHat hn]
>           , chord [line [rest dqn, percBDen, rest qn]
>                  , line [percCHHqn, percCHHqn, rest qn]]]
>
>     perc189_192 =
>       line [line [perc PedalHiHat qn, rest en, percCCen, percCCqn]
>           , chord [line [percLTqn, rest hn]
>                  , line [percm PedalHiHat [qn, qn]]]
>           , chord [line [percBDqn, rest en, percBDen]
>                  , line [percm PedalHiHat [qn, qn]]]
>           , t32 [chord [line [rest en, percBDen, times 6 percLTen]]]]
>
>     perc193_196 =
>       line [chord [line [percLTqn, t32 [percHTen, percLTen, percSDen], percSDen, percSDen
>                        , t32 [percSDqn, perc HighFloorTom en], rest hn]
>                  , line [percm PedalHiHat [qn, qn, qn, qn, qn, en, en]]]]
>
>     perc197_201 =
>       line [line [t32 [times 3 percSDen], tempo (5/4) (line [chord [percBDen, percSDen], times 4 percSDen])]
>           , chord [line [percBDen, percBDen, rest en, percBDen, percBDqn]
>                  , line [percCCqn, rest hn]]
>           , chord [line [percSDen, rest en, percBDen, rest en, percBDen]
>                  , line [rest qn, percCHHqn, rest en, percSDen]]
>           , tempo (9/12) (line [tempo (5/4) (times 4 (chord [line [percBDsn, percLTsn, percBDsn, percLTsn, percLTsn]
>                                                            , line [percCHHen, perc ClosedHiHat den]]))
>                                , line [rest en]])]
>
>     perc202_204 =
>       line [chord [line [percBDen, percBDen, rest dqn, percBDen]
>                  , line [percCCen, percCCen, rest hn]]
>           , line [percSDqn, percSDen, percBDen, rest en, percBDen]
>           , chord [line [rest dqn, percSDsn, percSDsn, percSDen, percBDen]
>                  , line [rest en, percCHHsn, percCHHsn, percCHHqn, rest qn]]]
>
>     perc205_207 =
>       line [chord [line [times 2 (line [rest sn, chord[percBDsn, percSDsn]]), percBDqn, rest qn]
>                  , line [percm ClosedHiHat [en, en, qn], rest qn]]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, perc CrashCymbal1 hn]]
>           , chord [percBDqn, percCHHqn]
>           , rest hn]
>
>     perc208_212 =
>       line [percBDen, percBDen
>           , tempo (5/4) (line [times 2 (chord [line [rest en, percBDen, rest en, percBDen, rest en]
>                                              , line [percCHHqn, percCHHqn, percCHHen]])
>                               , chord [line [rest en, perc BassDrum1 den]
>                                      , line [perc ClosedHiHat (den + en)]]])
>           , chord [line [percBDen, percBDen, rest hn, chord [percBDqn, percSDqn], rest en, percBDen, rest qn]
>                  , line [percCCen, percCCen, rest qn, percCHHen, percCHHen, percCHHqn, times 4 percCHHen]]
>           , chord [line [percBDen, percBDen, perc AcousticSnare hn]
>                  , line [percCHHqn, rest hn]]]
>
>     perc213_216 =
>       line [line [perc AcousticSnare dhn]
>           , chord [line [percBDen, percBDen, rest hn]
>                        , line [percCCen, percCCen, perc ClosedHiHat hn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [percSDqn, rest en, percSDen, rest qn]
>                  , line [percOHHqn, rest en, perc OpenHiHat dqn]]
>           , chord [line [percBDen, percBDen, percBDqn, t32 [times 3 percSDen]]
>                  , line [times 3 (perc PedalHiHat qn)]]]
>
>     perc217_220 =
>       line [chord [t32 [times 7 percSDen, percSDqn]
>                  , line [times 3 percCHHqn]]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, times 2 (perc PedalHiHat qn)]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [times 3 (perc PedalHiHat qn)]]
>           , chord [line [percBDen, percBDen, percBDqn, rest qn]
>                  , line [times 3 (perc PedalHiHat qn)]]]
>
>     perc221_224 =
>       line [chord [line [t32 [percSDqn, percSDen], percm AcousticSnare [dqn, en]]
>                  , line [times 3 percCHHqn]]
>           , chord [line [percBDen, percBDen, rest hn]
>                  , line [percCCen, percCCen, percCHHqn, percCHHqn]]
>           , chord [line [percBDqn, rest en, percBDen, rest qn]
>                  , line [perc PedalHiHat qn, percCHHqn, percCHHqn]]
>           , chord [line [rest (hn + en), percBDen]
>                  , line [times 3 (perc PedalHiHat qn)]]]
>
>     perc225_228 =
>       line [chord [line [percm HighFloorTom [en, sn, sn, en, en, qn]]
>                  , line [percm PedalHiHat [qn, qn, qn]]]
>           , chord [line [percBDen, percBDen, rest hn, percBDqn, rest (hn + dhn)]
>                  , line [times 6 (perc PedalHiHat qn), rest dhn]]]

Hills of Greenmore ====================================================================================================

> hgTempo                :: Dur
> hgTempo                                  = 1
> hgTranspose            :: AbsPitch
> hgTranspose                              = 0
>
> hgLead_                :: BandPart
> hgLead_                                  = makePitched Accordion                         hgTranspose     0     100
> hgBass_                :: BandPart
> hgBass_                                  = makePitched SynthBass1                        hgTranspose     0     100
> hgPerc_                 :: BandPart
> hgPerc_                                  = makeNonPitched                                                      100
>
> greenMore              :: Map InstrumentName InstrumentName → Music (Pitch, [NoteAttribute])
> greenMore dynMap                         =
>   removeZeros
>   $ tempo hgTempo
>   $ chord [leadMusic, bassMusic, percMusic]
>
>   where
>
>     hgLead                               = replace hgLead_ dynMap
>     hgBass                               = replace hgBass_ dynMap
>     hgPerc                               = replace hgPerc_ dynMap
>
>     leadMusic = bandPart hgLead leadLine
>     bassMusic = bandPart hgBass bassLine
>     percMusic = bandPart hgPerc percLine
>
>     leadLine, bassLine, percLine
>                        :: Music Pitch
>     leadLine = line [lead001, lead002, lead003, lead004]
>     bassLine = line [bass001, bass002, bass003, bass004]
>     percLine = line []
>
>     lead001  = line [ g 5 qn, fs 5 en, fs 5 qn, e 5 en]
>     lead002 = line [ d 5 qn,  g 4 en,  g 4 sn,  a 4 sn,  b 4 qn,  a 4 en, g 4 en]
>     lead003  = line [ g 4 en,  a 4 en,  b 4 qn,  a 4 qn, g 4 qn]
>     lead004 = line [ a 4 wn]

>     bass001  = line [ c 3 dhn, g 3 dhn]
>     bass002 = line [ d 3 dhn, c 3 dhn, g 3 dhn]
>     bass003  = line [ ]
>     bass004 = line [ ]

The End
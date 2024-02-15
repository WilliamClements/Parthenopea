> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Fanfare
William Clements
November 11, 2022

> module Fanfare where

> import Data.Map (Map)
> import qualified Data.Map                as Map
> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import HSoM.Examples.MoreMusic ( roll )
> import Parthenopea
> import Percussion
  
  "triads"

> pTriadG,  pTriadC,   pTriadBf,   pTriadD,   pTriadd    :: Dur → Music Pitch
> pTriadEf, pTriadF,   pTriadfs,   pTriadb,   pTriadAf   :: Dur → Music Pitch
> pTriade,  pTriadAug, pTriadSusA, pTriada               :: Dur → Music Pitch
> pTriadDim7A                                            :: Dur → Music Pitch
>
> pTriadC      = triad  C Major              ( E, 3)
> pTriadD      = triad  D Major              ( A, 2)
> pTriadd      = triad  D Minor              ( D, 3)
> pTriadEf     = triad Ef Major              (Ef, 3)
> pTriade      = triad  E Minor              ( E, 3)
> pTriadF      = triad  F Major              ( F, 3)
> pTriadfs     = triad Fs Minor              (Fs, 3)
> pTriadG      = triad  G Major              ( D, 3)
> pTriadAf     = triad Af Major              ( C, 3)
> pTriada      = triad  A Minor              ( C, 3)
> pTriadb      = triad  B Minor              (Fs, 3)
> pTriadBf     = triad Bf Major              ( D, 3)
> pTriadAug    = triad E (CustomMode "Aug")  ( E, 3)
> pTriadSusA   = triad A (CustomMode "Sus4") ( A, 3)
> pTriadDim7A  = triad G (CustomMode "Dim")  ( G, 3)

Fanfare ===============================================================================================================

> ffTempo                :: Rational       = 5 / 2
> ffTranspose            :: AbsPitch       = 5
>
> ffTempoCap             :: Rational       = 4
> ffTransposeCap         :: AbsPitch       = 0
>
> theFanfare isCap dynMap
>              = removeZeros
>              $ aggrandize
>              $ tempo fgTempo
>              $ keysig C Mixolydian
>              $ (tTreble1 :=: bBass) :+: (tTreble1 :=: tTreble2 :=: bBass)
>    where
>
>       fgTempo                            = if isCap then ffTempoCap     else ffTempo
>       xpo                                = if isCap then ffTransposeCap else ffTranspose
>
>       ffLeadI_                           = makePitched Trumpet      xpo 0            80
>       ffLeadII_                          = makePitched Flute        xpo 0            80
>       ffPickedI_                         = makePitched Trombone     xpo 0           110
>
>
>       ffLeadI                            = replace ffLeadI_    dynMap
>       ffLeadII                           = replace ffLeadII_   dynMap
>       ffPickedI                          = replace ffPickedI_  dynMap
>
>       tTreble1 = bandPart ffLeadI   $ tFan :+: tAns
>       tTreble2 = bandPart ffLeadII  $ uFan :+: uAns
>       bBass    = bandPart ffPickedI $ bFan :+: bAns
>
>       tFan =  {- 32 -} line [tFan1, tFan2, tFan3, tFan4, tFan5]
>       uFan =  {- 32 -} line [uFan1, uFan2, uFan3]
>       bFan =  {- 32 -} line [bFan1, bFan2, bFan3, bFan4, bFan5]
>
>       tFan1 = {- 16 -} line [rest wn,  c 4 dqn, rest en]
>                        :+: line [e 4 dqn, rest en,  g 4 wn, rest wn]
>       uFan1 = {- 16 -} line [rest wn,  e 4 dqn, rest en]
>                        :+: line [g 4 dqn, rest en,  c 5 wn, rest wn]
>       bFan1 = {- 15 -} line [rest wn, rest wn, rest hn]
>                        :+: line [ c 3 dhn,  c 3 qn,  c 3 qn]
>                        :=: line [ g 3 dhn,  g 3 qn,  g 3 qn]
>
>       tFan2 = {-  6 -} line [ f 4 hn,  e 4 hn,  d 4 dqn, rest en]
>       uFan2 = {-  6 -} line [ a 4 hn,  g 4 hn,  f 4 dqn, rest en]
>       bFan2 = {-  6 -} line [rest wn, rest hn]
>
>       tFan3 = {- 10 -} line [bf 3 qn, bf 3 qn, bf 3 hn, c 4 wn, rest hn]
>       uFan3 =          line [ d 4 qn,  d 4 qn,  d 4 hn, e 4 wn, rest hn]
>       bFan3 = {- 10 -} line [rest wn, rest qn]
>                        :+: line [ c 3 dhn,  c 3 qn,  c 3 qn]
>                        :=: line [ g 3 dhn,  g 3 qn,  g 3 qn]
>
>       tFan4 = {-  0 -} rest 0
>       bFan4 = {-  0 -} rest 0
>
>       tFan5 = {-  0 -} rest 0
>       bFan5 = {-  1 -} rest qn

The fanfare's answer

>       tAns1 = {- 10 -} line [c 4 hn, g 4 hn,  f 4 wn, rest hn]
>       uAns1 =          line [e 4 hn, c 5 hn, bf 4 wn, rest hn]
>       bAns1 = {- 10 -} rest wn
>                        :+: line [bf 2 dhn, a 2 qn, g 2 hn]
>                        :=: line [ d 3 dhn, c 2 qn, d 2 hn]
>
>       tAns2 = {-  6 -} line [ bf 3 qn, rest qn, bf 3 hn,  a 3 qn, a 3 qn]
>       uAns2 = {-  6 -} line [  d 4 qn, rest qn,  d 4 hn,  c 4 qn, c 4 qn]
>       bAns2 = {-  6 -} line [rest wn, rest hn]
>
>       tAns3 = {- 10 -} line [  g 3 hn,  a 3 hn, bf 3 hn,  c 4 wn]
>       uAns3 = {- 10 -} line [ bf 3 hn,  c 4 hn,  d 4 hn,  e 4 wn]
>       bAns3 = {-  8 -} line [rest wn, rest wn]            
>
>       tAns4 = {-  6 -} line [rest wn, rest hn]
>       bAns4 = {-  8 -} line [  g 2 qn,  g 2 qn,  c 3 qn,  g 2 hn]
>                        :+: line [ c 3 qn,  c 3 qn,  c 2 qn]
>
>       tAns =  {- 32 -} line [tAns1, tAns2, tAns3, tAns4]
>       uAns =  {- 32 -} line [uAns1, uAns2, uAns3]
>       bAns =  {- 32 -} line [bAns1, bAns2, bAns3, bAns4]

Alice =================================================================================================================

> aliceTempo                               = 2
> aliceTranspose                           = 10
> aliceLead_                               = makePitched Vibraphone aliceTranspose 0 100
>
> alice dynMap =
>         removeZeros
>         $ aggrandize
>         $ tempo                          aliceTempo
>         $ keysig G Dorian
>         $ bandPart aliceLead
>         $ line [rest hn, line00, line01, line00, line01]
>
>    where
>       
>       aliceLead                          = replace aliceLead_ dynMap
>
>       frag00 = line [g 3 qn, rest en, a 3 en, bf 3 wn]
>       frag01 = tempo (5/2) $ line [a 3 qn, bf 3 qn, a 3 qn,  g 3 hn]
>       frag02 = line [f 3 hn, g 3 ddhn]
>
>       line00 = line [frag00, frag01, frag02]
>
>       frag10 = line [g 3 en, c 4 dqn, c 4 en, c 4 dqn, c 4 en, c 3 ddhn]
>       frag11 = line [c 3 en, g 3 dqn, g 3 en, g 3 dqn, g 3 en, c 3 wn, rest hn]
>
>       line01 = line [frag10, frag11]

Bob ===================================================================================================================

> bobTempo                                 = 2
> bobTranspose                             = 0
>
> bobTreble_                               = makePitched Violin        bobTranspose 0 100
> bobAlto_                                 = makePitched Oboe          bobTranspose 0  80
> bobBass_                                 = makePitched Cello         bobTranspose 0  75
>
> bob :: Int → DynMap → Music (Pitch, [NoteAttribute])
> bob nRepeats dynMap = removeZeros
>       $ aggrandize
>       $ tempo bobTempo
>       $ keysig D Mixolydian
>       $     bandPart bobTreble (times nRepeats treblebob)
>         :=: bandPart bobAlto   (times nRepeats altobob)
>         :=: bandPart bobBass   (times nRepeats bassbob)
>
>    where
>
>       bobTreble                          = replace bobTreble_ dynMap
>       bobAlto                            = replace bobAlto_ dynMap
>       bobBass                            = replace bobBass_ dynMap
>
>       treblebob = addDur qn
>          [ a 3, fs 4
>          , a 3, fs 4
>          , a 3, fs 4 
>          , a 3, fs 4
>          , a 3,  g 4
>          , b 3,  g 4]
>          :+: tempo (3/2) (addDur en
>          [ g 4, fs 4
>          , e 4, d 4
>          , e 4, fs 4
>          , a 4, fs 4
>          , e 4, d 4
>          , e 4, g 4])
>
>       altobob = line 
>          [ rest qn, d 4 qn
>          , rest qn, d 4 qn
>          , rest qn, d 4 qn
>          , rest qn, d 4 qn
>          , rest qn, d 4 qn
>          , rest qn, d 4 qn
>          , rest wn]
>
>       bassbob =
>          line [ d 2 dwn, rest hn,  g 2 wn,  e 2 hn,  c 2 hn]
>       

Bill ==================================================================================================================

> alto00 = rest hn
>          :+: f 4 hn :+: rest dwn
>          :+: c 4 wn :+: rest wn
>          :+: ef 3 hn :+: rest dwn
>          :+: gf 3 wn :+: rest wn
>
> billTempo                                = 2
> billTranspose                            = 0
>
> billTreble_                              = makePitched Violin              billTranspose 0  80
> billAlto_                                = makePitched RhodesPiano         billTranspose 0 110
> billBass_                                = makePitched Cello               billTranspose 0  60
>
> bill nRepeats dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo     billTempo
>    $ keysig Ef Mixolydian
>
>    $     bandPart billTreble (rest dwn :+: g 4 hn :+: times nRepeats treble)
>      :=: bandPart billAlto   (rest dwn :+: bf 3 hn :+: times nRepeats alto)
>      :=: bandPart billBass   (rest dwn :+: ef 3 hn :+: times nRepeats bass)
>
>    where
> 
>    billTreble                            = replace billTreble_ dynMap
>    billAlto                              = replace billAlto_ dynMap
>    billBass                              = replace billBass_ dynMap
>
>    treble00 =
>        tempo (3/2) (line [ef 4 qn, f 4 qn, g 4 qn])
>          :+: af 4 hn
>          :+: tempo 2 (line [df 4 qn, ef 4 qn, f 4 qn, g 4 qn])
>          :+: line [af 4 qn, rest en, g 4 en, f 4 hn]
>          :+: ef 4 wn :+: line [rest hn, ef 4 qn, f 4 qn]
>          :+: gf 4 hn
>          :+: tempo 2 (line [b 3 qn, df 4 qn, ef 4 qn, f 4 qn])
>          :+: gf 4 qn :+: line [rest en, f 4 en, ef 4 hn]
>          :+: df 4 wn :+: rest wn
>    bass00 =
>          rest hn
>          :+: df 3 hn :+: rest dwn
>          :+: af 2 wn :+: rest wn
>          :+:  b 2 hn :+: rest dwn
>          :+: bf 2 wn :+: rest wn
>
>    treble01 = 
>          line [df 4 hn,  d 4 wn, rest wn]
>          :+: line [ d 4 dwn, ef 4 hn,  f 4 hn]
>          :+: tempo (3/2) (line [g 3 qn, bf 3 qn, ef 4 qn])
>          :+: line [f 4 dqn, g 4 en, ef 4 hn, bf 3 hn, ef 4 hn] 
>    alto01 =
>          line [rest wn, rest wn, rest hn, af 2 wn, gf 2 wn,  f 2 hn, b 2 hn, bf 2 wn, rest wn]
>    bass01 =
>          rest wn
>          :+: tempo (1/3) (bf 2 wn)
>          :+: line [rest wn, rest dwn, rest wn]
>
>    treble = line [treble00, treble01]
>    alto   = line [alto00,     alto01]
>    bass   = line [bass00,     bass01]
>

Copper ================================================================================================================

> copperTempo                              = 2
> copperTranspose                          = -4
>
> copperLead_                              = makePitched Banjo copperTranspose 0 100
>
> copper :: Int → DynMap → Music (Pitch, [NoteAttribute])
> copper n dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo 2
>    $ keysig C Dorian
>    $ bandPart copperLead
>    $ times n
>    $ line [c 5 qn, rest qn, c 5 qn, rest qn, c 5 qn, rest qn, c 5 qn, rest qn]
>      :+: tempo (5/4) (line [c 5 qn, g 4 qn, a 4 qn, bf 4 qn, rest qn])
>      :+: line [ a 4 hn,  g 4 wn]
>
>    where
>
>      copperLead                          = replace copperLead_ dynMap

Gold ==================================================================================================================

> goldTempo                                = 2
> goldTranspose                            = -5
>
> goldLead_                                = makePitched Harpsichord goldTranspose 0 100
>
> gold dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo 2
>    $ keysig C Mixolydian
>    $ bandPart goldLead
>    $ times 2
>      (line [c 4 hn,  c 5 dhn]
>       :+: addDur qn [ c 5, bf 4,  a 4,  g 4,
>                       g 4,  f 4,  a 4,  g 4,
>                       g 4,  f 4,  a 4,  g 4,
>                       g 4,  c 4,  c 4,  c 4,
>                       c 5, bf 4,  a 4,  g 4,
>                       g 4,  f 4,  e 4,  d 4,
>                       d 4, bf 3, bf 3])
>       :+: c 4 hn
>   where
>     goldLead                             = replace goldLead_ dynMap

Silver ================================================================================================================

> silverTempo                              = 2
> silverTranspose                          = -5
>
> silverLead_                              = makePitched MusicBox silverTranspose 0 100
>     
> silver dynMap =
>     removeZeros
>     $ aggrandize
>     $ tempo 2
>     $ keysig A Mixolydian
>     $ bandPart silverLead
>     $ line allSilver
>
>     where
>
>     silverLead                           = replace silverLead_ dynMap
>
>     silver00 = 
>        addDur hn
>           [ a 4,  e 5, cs 5,  a 4, fs 4,
>             a 4,  b 4, fs 4,  e 4,
>             a 4,  b 4, fs 4,  e 4,
>            fs 4, gs 4,  a 4, cs 5 ]
>
>     silver01 =
>        addDur hn
>           [ e 4, fs 4,  g 4,
>            fs 4,  a 4,  d 5,
>            cs 5,  b 4, gs 4,
>            ds 5,  e 5, fs 5,
>             e 5, ds 5,  e 5,
>            fs 4,  e 4, as 4,
>             b 4, fs 4, gs 4,
>             a 4, cs 5 ] 
>
>     allSilver :: [Music Pitch]
>     allSilver =
>        [ rest dwn
>         , silver00, a 4 hn
>         , silver00, a 4 hn
>         , silver01, a 4 hn
>         , silver01, a 4 wn ]

Snake =================================================================================================================

> gSnip01 = line [ c 4 hn,  g 4 hn,  b 3 hn, g 4 qn, a 3 qn
>                , a 3 qn,  d 4 qn, chord [d 4 dwn, fs 4 dwn]]
> gSnip02 = line [  g 4 qn,  g 3 qn,  c 4 hn, rest wn, rest wn]
> gSnip03 = addDur en [  g 4, fs 4,  e 4,  d 4,       e 4,  d 4,  c 4,  b 3
>                     ,  c 4,  b 3,  a 3,  g 3,       a 3,  g 3]
> gSnip04 = addDur en [             fs 3,  g 3,       a 3,  g 3,  a 3, b 3
>                     ,  c 4,  b 3,  a 3,  g 3,      fs 3,  g 3, fs 3, e 3
>                     ,  d 3,  e 3,  d 3, cs 3]
> gSnip05 = addDur en [  b 4,  a 4,  g 4, fs 4,       g 4,  fs 4,  e 4,  d 4
>                     ,  e 4,  d 4,  c 4,  b 3,       c 4,  b 3]
> gSnip06 = addDur en [              a 3,  b 3,       c 4,  b 3,  c 4, d 4
>                     ,  e 4,  d 4,  c 4,  b 3,       a 3,  b 3,  a 3, g 3
>                     , fs 3,  g 3, fs 3,  f 3]
> gSnip07 = line [ c 4 hn, e 4 hn, fs 4 hn, g 4 dqn, g 4 en]
>
> snakeTempo                               = 2
> snakeTranspose                           = 0
>
> snakeLead_                               = makePitched    AltoSax    snakeTranspose  0  100
> snakeSecond_                             = makePitched    TenorSax   snakeTranspose  0  100
>
> snake dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo snakeTempo
>    $ keysig D Mixolydian
>    $     bandPart snakeLead treblePart
>      :=: bandPart snakeSecond altoPart
>
>    where
>
>    snakeLead                             = replace snakeLead_ dynMap
>    snakeSecond                           = replace snakeSecond_ dynMap
>
>    treblePart = line [treb00, treb01, treb02, treb03, treb04]
>    altoPart   = line [alto00, alto01, alto02, alto03, alto04]
>
>    -- Intro
>    treb00 = times 4 gSnip01
>    alto00 = times 3 (gSnip02 :+: gHead00)
>             :+:     (gSnip02 :+: gHead01)
>    altoZZ = times 4 (gSnip02 :+: gHead00)
>
>    -- Tight Solo Runs
>    treb01 = times 16 (rest wn)
>    alto01 = line [  gSnip03,  gTail01, gHead01,  gSnip03, gTail02, gHead01
>                  ,  gSnip03,  gTail03, gHead01,  gSnip03, gSnip04]
>
>    -- Pedal Tones
>    cee = c 3 dhn :+: rest qn
>    dee = d 3 dhn :+: rest qn
>    ceecee = c 3 dwn :+: rest hn
>    deedee = d 3 dwn :+: rest hn
>    
>    -- Relief
>    treb02 = times 3 (line [gSnip07, gTail11, gHead11])
>                    :+: line [gSnip07, gTail12, gHead01]
>    alto02 = times 3 (line [ceecee, deedee])
>                    :+: line [ceecee, dee, gHead02]
>
>    -- Doubled Runs
>    treb03a = rest wn :+: rest hn
>             :+: line [  a 4 wn,fs 4 wn,  d 4 wn,  g 4 wn
>                      ,  b 3 wn, c 4 wn,  gHead02'
>                      , rest hn, a 4 wn,  fs 4 wn,  d 4 wn
>                      ,  g 4 wn, b 3 wn,  c 4 wn, rest wn, rest hn ]
>    treb03b = rest wn :+: rest hn
>             :+: line [  rest wn, rest wn,  rest wn,  rest wn
>                      ,  rest wn, rest wn,  rest hn
>                      , rest hn, d 5 wn,  a 4 wn,  fs 4 wn
>                      ,  b 4 wn, g 4 wn,  a 4 wn, rest wn, rest hn ]
>
>    alto03a = line [ gSnip03, gTail01, gHead01
>                   , gSnip03, gTail02, gHead01
>                   , gSnip03, gTail03, gHead01
>                   , gSnip03, gSnip04]
>    alto03b = line [ gSnip05, gTail21, gHead02
>                   , gSnip05, gTail22, gHead02
>                   , gSnip05, gTail23, gHead02
>                   , gSnip05, gSnip06]
>
>    -- The DEVIL is in the doubles
>    treb03 = treb03a :=: treb03b
>    alto03 = alto03a :=: alto03b
>
>    -- Outro
>    treb04 = Modify (Phrase [Dyn $ Diminuendo 0.7]) treb00
>    alto04 = Modify (Phrase [Dyn $ Diminuendo 0.7]) altoZZ
>
>    -- Figures to lead into next phrase, i.e. the beginning of it
>    -- 3 slow ns, 3 fast ns, 3 fast-harmonized ns, 3 slow-inverted ns
>    gHead00 = line [rest qn,  c 4 qn,  g 4 qn,  a 4 qn]
>    gHead01 = line [rest hn, rest en,  c 4 en,  g 4 en,  a 4 en]
>    gHead02 = line [rest hn, rest en,  e 4 en,  b 4 en,  c 5 en]
>    gHead02' = line [rest en, e 4 en, b 3 en, c 4 en]
>    gHead11 = line [rest qn,  g 4 qn,  c 4 qn,  b 3 qn]
>
>    -- Phrase finishers
>    gTail01 = line [fs 3 en,  e 3 en,  d 3 dhn, rest qn]
>    gTail02 = line [fs 3 en,  g 3 en, fs 3 dhn, rest qn]
>    gTail03 = line [fs 3 en,  g 3 en,  a 3 dhn, rest qn]
>    gTail11 = line [ chord [fs 4 qn, a 4 qn]
>                   , chord [ g 4 qn, b 4 qn]
>                   , chord [fs 4 hn, a 4 hn]]
>    gTail12 = line [ a 4 qn,  d 4 qn,  d 4 hn]
>    gTail21 = line [ a 3 en,  g 3 en, fs 3 dhn, rest qn]
>    gTail22 = line [ a 3 en,  b 3 en,  a 3 dhn, rest qn]
>    gTail23 = line [ a 3 en,  b 3 en,  d 4 dhn, rest qn]

Country In The Morning ================================================================================================

> citmTempo                               = 1
> citmTranspose                           = 0
>
> citmLead_                               = makePitched AltoSax               citmTranspose 0 100
> citmStrum_                              = makePitched AcousticGuitarSteel   citmTranspose 0 100
> citmBass_                               = makePitched ElectricBassFingered  citmTranspose 0 100
>
> getCITM dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo citmTempo
>    $ keysig G Major
>    $ chord [     
>         bandPart citmLead
>           (line [a1 :+: rest qn, b1, a1 :+: a 4 qn, c1])
>      ,
>         bandPart citmStrum
>           (line [a2, b2, a2, c2])
>      ,
>         bandPart citmBass
>           (line [ a3 :+: line [a 2 qn, b 2 qn, cs 3 qn], b3
>                 , a3 :+: line [d 2 qn, e 2 qn, fs 2 qn], c3])]
>
>    where
>
>       citmLead                           = replace citmLead_ dynMap
>       citmStrum                          = replace citmStrum_ dynMap
>       citmBass                           = replace citmBass_ dynMap
>       
>
>       dTriad, eTriad, aTriad, gTriad :: Dur → Music Pitch
>       dTriad =  triad  D Major ( D, 3)
>       eTriad =  triad  E Major ( B, 2)
>       aTriad =  triad  A Major (Cs, 3)
>       gTriad =  triad  G Major ( D, 3)
>
>    -- 32 = a1 :=: a2 :=: a3
>
>       a1 = line [fs 4 wn, g 4 hn, fs 4 en, e 4 qn, e 4 en
>                  , fs 4 en, a 4 dwn, rest en]
>       a2 = line [dTriad wn, aTriad wn, dTriad dwn, rest hn]
>       a3 = line [ d 2 wn,  a 2 wn, d 2 dqn, d 3 en
>                 , a 2 den, a 2 sn,  b 2 den, cs 3 sn, d 3 qn]
>
>    -- 32 = b1 :=: b2 :=: b3
>
>       b1 = line [fs 4 dhn, fs 4 qn, g 4 hn, gs 4 hn
>                 , a 4 wn,   g 4 hn, rest hn]
>       b2 = line [dTriad wn, gTriad hn, eTriad hn, aTriad dwn, rest hn]
>       b3 = line [ d 2 wn,  g 2 hn,  e 2 hn
>                 , a 2 dhn, a 2 qn,  b 2 hn,  cs 3 hn]
>
>    -- 32 = c1 :=: c2 :=: c3
>
>       c1 = line [b 4 qn, b 4 qn, b 4 qn, b 4 qn, a 4 qn
>                 , a 4 qn, g 4 qn, fs 4 qn, e 4 qn
>                 , d 4 dwn, rest qn]
>       c2 = line [gTriad dhn, eTriad qn,  aTriad wn
>                 ,   rest qn, dTriad dwn, rest qn]
>       c3 = line [ g 2 dhn,  e 2 qn,  a 2 qn,   a 2 qn
>                 , g 2 qn,  fs 2 qn,  e 2 qn,   d 2 qn
>                 , d 3 den,  d 3 sn,  a 2 den,  a 2 sn, b 2 den, cs 3 sn
>                 , d 2 hn, rest qn]

Whelp Narp ============================================================================================================

> wnTempo                                  = 2
> wnTranspose                              = 4
>
> wnLead_                                  = makePitched    Vibraphone           wnTranspose 0 100
> wnStrings_                               = makePitched    OrchestralHarp       wnTranspose 0 100
> wnBass_                                  = makePitched    ElectricBassPicked   wnTranspose 0 100
> wnPerc_                                  = makeNonPitched                                    100
>
> whelpNarp dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo wnTempo
>    $ keysig C Mixolydian
>    $     bandPart wnLead                    (wnAltoI :+: wnAltoII)
>      :=: bandPart wnStrings                (wnTenorI :+: wnTenorII)
>      :=: bandPart wnBass                (wnBaritoneI :+: wnBaritoneII)
>      :=: bandPart wnPerc                    (wnPercI :+: wnPercII)
>   where
>
>   wnLead                                 = replace wnLead_      dynMap
>   wnStrings                              = replace wnStrings_   dynMap
>   wnBass                                 = replace wnBass_      dynMap
>   wnPerc                                 = replace wnPerc_      dynMap
>
> -- It is in 3/4 for 7 measures; the eighth measure is shortened to 2/4;
> -- then it is in 4/4 except for one 5/4 measure at the end of each repeat.
>
>   wntrip01 = tempo (3/2) $ line [g 3 qn, c 4 qn, d 4 qn, e 4 hn, d 4 qn]
>   wntrip02 = tempo (3/2) $ line [c 4 qn, f 4 qn, g 4 qn]
>   wntrip03 = tempo (3/2) $ line [f 4 hn, f 4 qn, f 4 hn, e 4 qn]
>   wntrip04 = tempo (3/2) $ line [c 4 qn, d 4 qn, g 4 qn]
>   wntrip05 = tempo (3/2) $ line [c 4 hn, c 4 qn, c 4 hn, bf 3 qn]
>
>   wnlick01 = line [c 4 hn, bf 3 hn, d 4 hn]
>   wnRepeat = line [wntrip01, wnlick01, c 4 hn]
>   wnlick03 = line [a 3 qn, f 4 hn, a 3 qn,  f 4 qn, f 4 qn, rest qn,
>                    c 4 qn, f 4 dhn, rest hn]
>   wnlick20 = line [f 4 qn, f 4 hn, bf 3 qn, f 4 qn, f 4 qn, rest qn,
>                 bf 3 qn, f 4 dhn, rest hn]
>   wnt__F   = triad F Major ( A, 4)
>   wnt__C   = triad C Major ( G, 4)
>   wnt__C'  = triad C Major ( C, 5)

Alto-----

>   wnaIntro01A = times 3 wnRepeat
>   wnaIntro02A = line [wntrip01, wnlick01]
>   wnaIntroA = line [wnaIntro01A, wnaIntro02A]
>   wnaIntroB = wnaIntroA
>
>   wnaExposeA =
>     line [wnlick03, bf 3 qn, wntrip02, f 4 hn, wntrip03, d 4 hn, c 4 wn, rest wn
>         , wnlick03, bf 3 qn, wntrip02, f 4 hn, wntrip03, d 4 hn, c 4 wn]
>   wnaImposeA = line [wnlick20, bf 3 qn, wntrip04, f 4 hn, wntrip03, d 4 hn]
>   wnaSlide = line [wntrip05, a 3 hn, g 3 hn]
>   wnaSegue = line [d 4 qn :+: e 4 hn]
>
>   wnaExposeB = wnaExposeA
>   wnaImposeB = wnaImposeA
>
>   wnAltoI =  line [wnaIntroA, wnaExposeA,
>                   rest qn, wnaSegue, wnaImposeA, c 4 wn,
>                   rest qn, wnaSegue, wnaImposeA, c 4 hn, wnaSlide]
>   wnAltoII = line [wnaIntroB, wnaExposeB,
>                   rest qn, wnaSegue, wnaImposeB, c 4 wn,
>                   rest qn, wnaSegue, wnaImposeB, c 4 hn, wnaSlide]

Tenor-----

>   wnTenorIA = tempo (1/44) $ rest wn
>   wnTenorIB = rest 0
>
>   wnTenorIIA = tempo (2/23) $ rest wn
>   wnTenorIIB = times 2
>                $ line
>                   [ addDur qn [c 3, d 3, e 3, f 3, g 3, a 3, bf 3],
>                     addDur qn [c 4, d 4, e 4, f 4, g 4, a 4, bf 4],
>                     line [c 5 wn],
>                     tempo (5/2) $ addDur qn [c 5, d 5, e 5, f 5, d 5],
>                     line [c 5 wn],
>                     tempo (5/2) $ addDur qn [c 5, d 5, e 5, f 5, d 5],
>                     line [c 5 dwn]]
>   wnTenorIIC =
>     transpose (-12)
>     $ times 4
>     $ line [
>       line [wnt__F  qn,  wnt__F  hn,  wnt__F  qn],
>       line [wnt__C  qn,  wnt__C  hn,  wnt__C  qn],
>       line [wnt__C' qn,  wnt__C' hn,  wnt__C' qn],
>       line [wnt__C  qn,  wnt__C  hn,  wnt__C  qn]]
>   wnTenorIID = transpose (-12) $ wnt__C dhn
>              
>   wnTenorI =  line [wnTenorIA, wnTenorIB]
>   wnTenorII = line [wnTenorIIA, wnTenorIIB, wnTenorIIC, wnTenorIID]

Baritone-----

>   wnBaritoneI = tempo (1/2) $ line [wnbIntroA, wnbExposeA]
>   wnBaritoneII = tempo (1/2) $ line [wnbIntroB, wnbExposeB]
>
>   wnbIntro01A  = line [c 2 dhn, g 1 hn, c 2 wn, g 1 hn
>                      , c 2 wn,  g 1 hn, c 2 wn, g 1 hn]
>   wnbIntro02A  = rest 0
>
>   wnbIntro01B = wnbIntro01A
>   wnbIntro02B = wnbIntro02A
>
>   wnbIntroA = line [wnbIntro01A, wnbIntro02A]
>   wnbIntroB = line [wnbIntro01B, wnbIntro02B]
>
>   wnbExpose01A = times 2 $ line [ f 1 wn,  f 1 wn,
>                                   c 2 wn,  c 2 wn]
>   wnbExpose02A = times 2 $ line [bf 1 wn, bf 1 wn,
>                                   f 1 wn,  f 1 wn]
>   wnbExpose01B = wnbExpose01A
>   wnbExpose02B = wnbExpose02A
>
>   wnbExposeA = line [wnbExpose01A, wnbExpose02A :+: rest qn]
>   wnbExposeB = line [wnbExpose01B, wnbExpose02B :+: rest qn]

Percussion-----

>   p1 = perc LowTom
>   p2 = perc AcousticSnare
>   p3 = perc CrashCymbal2
>   r1 = tempo (3/2) $ line [p1 qn, p1 qn, p1 qn, p2 hn, p1 qn]
>
>   wnPercI = times 44 $ rest wn
>   wnPercII = wnP1
>   wnP1 = (r1 :+: p1 hn :+: rest dwn)
>      :+: (r1 :+: p1 hn :+: rest dwn)
>      :+: (r1 :+: p1 hn :+: rest dwn)
>      :+: (r1 :+: rest qn :+: roll sn (p2 qn) :+: p3 wn)

Roger =================================================================================================================

> rogerTempo                               = 1
> rogerTranspose                           = 0
>
> rogerAlto_                               = makePitched Flute               rogerTranspose 12 100
> rogerTenor_                              = makePitched AcousticGuitarNylon rogerTranspose  0 100
>
> roger :: DynMap → Music (Pitch, [NoteAttribute])
> roger dynMap                             =
>    removeZeros
>    $ aggrandize
>    $ tempo                               rogerTempo
>    $ keysig Cs Dorian
>    $     bandPart rogerAlto  (line [cAltoI,  cAltoIIA, cAltoIIB])
>      :=: bandPart rogerTenor (line [cTenorI, cTenorII])
>   where
>
>   rogerAlto                              = replace rogerAlto_  dynMap
>   rogerTenor                             = replace rogerTenor_ dynMap
>
>   ct_fs, ct_cs, ct_cs', ct__B, ct__B'          :: Dur → Music Pitch
>   ct__A, ct__E                                 :: Dur → Music Pitch
>   ct_fs    = triad Fs Minor ( A, 3)
>   ct_cs    = triad Cs Minor (Gs, 3)
>   ct_cs'   = triad Cs Minor ( E, 3)
>   ct__B    = triad  B Major (Fs, 3)
>   ct__B'   = triad  B Major (Ds, 3)
>   ct__A    = triad  A Major ( A, 3)
>   ct__E    = triad  E Major (Gs, 3)
>
>   cAltoI = line [cAltoIA, cAltoIB, cAltoIC
>                         , cAltoIB, cAltoIC'
>                , cAltoID, cAltoIE, cAltoIF
>                ,  b 3 wn, b 3 sn, cs 4 sn, ds 4sn, e 4 sn ]
>   cAltoIIA =
>      line [fs 4 hn, rest en, cs 4 en, ds 4 qn, e 4 hn, rest en,
>             e 4 en, fs 4 en, e 4 en, fs 4 dwn, rest qn,
>             a 3 sn, b 3 sn, cs 4 sn, ds 4 sn,
>            cAltoIB, cAltoIC, cAltoIB, cAltoIC'']
>   cAltoIIB = line [cAltoIF, b 3 wn, cAltoIIB', b 3 wn]
>
>
>   cAltoIA = line [ a 4 wn, gs 4 hn, rest en, e 4 en, fs 4 en, e 4 en,
>                  fs 4 dwn, rest qn, ds 4 qn ]
>   cAltoIB = pSnippet01
>   cAltoIC = line   [ e 4 qn, cs 4 qn, b 3 wn]
>   cAltoIC' = line  [ e 4 qn, cs 4 qn, b 3 hn]
>   cAltoIC'' = line  [ e 4 qn, cs 4 qn, b 3 qn]
>   cAltoID =
>     tempo (3/2)
>     (line [ b 3 en, a 3 qn, gs 3 qn, a 3 en])
>   cAltoIE =  line  [
>      b 3 qn, fs 3 wn, rest en, rest qn,
>     gs 3 en,  b 3 en, e 4 en, ds 4 dqn, e 4 sn, ds 4 sn, cs 4 wn,
>      b 3 hn, a 3 wn]
>   cAltoIF =
>      tempo (3/2)
>      (line [a 3 qn, gs 3 en, a 3 qn, gs 3 en, a 3 qn, b 3 en])
>   cAltoIIB' =
>      tempo (3/2)
>      (line [gs 5 en, e 5 en, cs 5 en,
>             e 5 en, cs 5 en, gs 4 en,
>             cs 5 en, gs 4 en, e 4 en,
>             gs 4 en, e 4 en, cs 4 en,
>
>             fs 5 en, ds 5 en, b 4 en,
>             ds 5 en, b 4 en, fs 4 en,
>              e 5 en, cs 5 en, a 4 en,
>             cs 5 en, a 4 en, e 4 en,
>
>              a 4 en, e 4 en, cs 4 en,
>              e 4 en, cs 4 en, a 3 en
>             ])
>
>   cTenorI = line [ cTenorIA,  cTenorIB,  cTenorIC]
>   cTenorII = line [cTenorIIA, cTenorIIB, cTenorIIC]
>
>   cTenorIA =  line [ ct_fs wn, ct_cs wn,  ct__B wn,  ct__B' wn]
>   cTenorIB =  line [ ct__A wn, ct__E wn,  ct__A wn,  ct__E wn]
>   cTenorIC =  line [ ct__B wn, ct__B' wn, ct_cs wn,  ct_cs' hn,
>                      ct__B hn, ct__A wn,  ct__A dhn, ct__B wn, rest qn]
>   cTenorIIA = line [ ct_fs wn, ct_cs wn,  ct__B wn,  ct__B' wn,
>                      ct__A wn, ct__E wn,  ct__A wn,  ct__E  wn]
>   cTenorIIB = line [ ct__B wn, ct_cs wn,  ct__B hn,  ct__A wn]
>   cTenorIIC = line [ ct__B wn]

Way Pos' T' Purple ====================================================================================================

> wayposTempo                              = 1
> wayposTranspose                          = 0
>
> wayposLead_                              = makePitched    HonkyTonkPiano wayposTranspose 0 100
> wayposStrum_                             = makePitched    OrchestralHarp wayposTranspose 0 100
> wayposBass_                              = makePitched    Bassoon        wayposTranspose 0  50
> wayposPerc_                              = makeNonPitched                                  100
>
> waypostpurple dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo                               wayposTempo
>    $ keysig C Major
>          (bandPart wayposLead  leadPart
>       :=: bandPart wayposStrum strumPart
>       :=: transpose 12 (bandPart wayposBass bassPart))
>       :=: bandPart wayposPerc  percPart
>   where
>
>   wayposLead                             = replace wayposLead_ dynMap
>   wayposStrum                            = replace wayposStrum_ dynMap
>   wayposBass                             = replace wayposBass_ dynMap
>   wayposPerc                             = replace wayposPerc_ dynMap
>
>   includeOpen = True
>   includeCont = True
>   includePool = True
>   includeClos = True
>
>   leadPart                               =
>             (if includeOpen then pOpenT  else rest 0)
>         :+: (if includeCont then pContT  else rest 0)
>         :+: (if includePool then pPoolT  else rest 0)
>         :+: (if includeClos then pClosT  else rest 0)
>
>   strumPart                              =
>             (if includeOpen then pOpenG  else rest 0)
>         :+: (if includeCont then pContG  else rest 0)
>         :+: (if includePool then pPoolG  else rest 0)
>         :+: (if includeClos then pClosG  else rest 0)
>
>   bassPart                               =
>             (if includeOpen then pOpenB  else rest 0)
>         :+: (if includeCont then pContB  else rest 0)
>         :+: (if includePool then pPoolB  else rest 0)
>         :+: (if includeClos then pClosB  else rest 0)
>
>   percPart                               =
>             (if includeOpen then pOpenP  else rest 0)
>         :+: (if includeCont then pContP  else rest 0)
>         :+: (if includePool then pPoolP  else rest 0)
>         :+: (if includeClos then pClosP  else rest 0)

  "open"

>   pOpenT1 = line [ d 4 qn, g 4 qn, b 3 qn, c 4 qn]
>   pOpenT2 = tempo 3 $ line [ f 4 qn, e 4 qn, d 4 qn, a 3 qn, d 4 qn, f 4 qn]
>   pOpenT3 = tempo 3 $ line [bf 3 qn, ef 4 qn, g 4 qn]
>   pOpenT4a = line [ a 4 dhn, rest hn]
>   pOpenT4b = line [ a 4 dhn, b 4 qn, d 5 qn]
>   pOpenT5a = line [pOpenT1, pOpenT2, pOpenT1, pOpenT2, pOpenT3, pOpenT4a]
>   pOpenT5b = line [pOpenT1, pOpenT2, pOpenT1, pOpenT2, pOpenT3, pOpenT4b]
>   pOpenT  = line [pOpenT5a, pOpenT5b]
>   pClosT123A = tempo 3
>                (addDur qn [g 3, b 3, d 4, b 3, d 4, g 4,
>                            d 4, g 4, b 4]) -- throw away , g 4, b 4, d 5])
>   pClosT123B = tempo 3
>                (addDur qn [c 5, g 4, e 4, bf 4, f 4, d 4,
>                            a 4, f 4, d 4]) -- throw away , e 4, c 4, g 3])
>   pClosT123C = tempo 3
>                (addDur qn [g 4, ef 4, bf 4])
>   pClosT4a = line [ a 4 dhn, rest hn]
>   pClosT4b = line [ a 4 dhn, b 4 qn, d 5 wn]
>   pClosT5a = line [pClosT123A, pClosT123B,
>                   pClosT123A, pClosT123B, pClosT123C, pClosT4a]
>   pClosT5b = line [pClosT123A, pClosT123B,
>                 pClosT123A, pClosT123B, pClosT123C, pClosT4b]
>   pClosT  = line [pClosT5a, pClosT5b]
>
>   pOpenG1 = line [pTriadG dhn, pTriadC qn, pTriadBf qn, pTriadd qn]
>   pOpenG2 = line [pTriadEf qn, pTriadF hn, rest dhn]
>   pOpenG3  = line [pOpenG1, pOpenG1, pOpenG2]
>   pOpenG  = line [pOpenG3, pOpenG3]
>   pClosG2 = line [pTriadEf qn, pTriadF hn, rest dhn, pTriadC wn]
>   pClosG3  = line [pOpenG1, pOpenG1, pClosG2]
>   pClosG = line [pOpenG3, pClosG3]
>
>   pOpenB1 = line [g 2 dhn, c 2 qn, bf 1 qn, d 2 qn]
>   pOpenB2 = line [ef 2 qn, f 2 wn, rest qn]
>   pOpenB3 = line [pOpenB1, pOpenB1, pOpenB2]
>   pOpenB  = line [pOpenB3, pOpenB3]
>   pClosB2 = line [ef 2 qn, f 2 wn, rest qn, c 2 wn]
>   pClosB3 = line [pOpenB1, pOpenB1, pClosB2]
>   pClosB  = line [pOpenB3, pClosB3]
>   pOpenP  = rest 0
>   pClosP  = rest 0

  "cont"

>   pContT1 = line [cs 5 en,  b 4 en, cs 5 qn, fs 4 qn,
>                    d 4 qn, fs 4 qn,  d 4 qn,  g 4 wn, rest hn]
>   pContT2 = line [ b 4 qn,  d 5 en, fs 4 en,  d 4 qn,
>                    e 4 qn,  b 4 en,  g 4 en,  e 4 qn,
>                    c 4 qn,  a 4 en,  f 4 en,  c 4 qn,
>                    f 4 qn,  a 4 hn]
>   pContT  = line [pContT1, pContT1, pContT2, pContT2]
>
>   pContG1 = line [pTriadfs wn, pTriadb hn, pTriadC wn, rest hn]
>   pContG2 = line [pTriadb  wn, pTriade hn, pTriadF wn, rest hn]
>   pContG  = line [pContG1, pContG1, pContG2, pContG2]
>
>   pContB1 = line [fs 2 wn,  b 1 hn, c 2 wn, rest hn]
>   pContB2 = line [ b 1 wn, fs 2 hn, c 2 wn, rest hn]
>   pContB3 = transpose (-7) pContB1
>   pContB4 = transpose (-7) pContB2
>   pContB  = line [pContB1, pContB2, pContB3, pContB4]
>   pContP  = rest 0

  "pool"

>   pPoolT1 = line [gs 4 dqn, a 4 en, gs 4 en, fs 4 en, e 4 hn, d 4 qn]
>   pPoolT2 = line [gs 4 en, fs 4 en,  a 4 en,  b 4 en, c 4 qn]
>   pPoolT  = line [pPoolT1, pPoolT2, pPoolT2,
>                   pPoolT1, pPoolT2, pPoolT2]
>
>   pPoolG1 = line [pTriadAug dhn, pTriadAug dhn, pTriada dhn, pTriadD dhn]
>   pPoolG  = line [pPoolG1, pPoolG1]
>
>   pPoolB1 = line [d 2 dhn, d 2 dhn, d 2 dhn, d 2 dhn]
>   pPoolB  = line [pPoolB1, pPoolB1]
>   pPoolP  = rest 0

> littlePendingtonArnt =
>   instrument TenorSax $ addVolume 70 (e 3 dwn)

Pendington Arnt  ======================================================================================================

> pArntTempo                              = 1
> pArntTranspose                          = 0
>
> pArntLead_                              = makePitched TenorSax               pArntTranspose 0 100
> pArntStrum_                             = makePitched AcousticGuitarNylon    pArntTranspose 0 100
> pArntBass_                              = makePitched Cello                  pArntTranspose 0 100
>
> pendingtonArnt :: Int → DynMap → Music (Pitch, [NoteAttribute])
> pendingtonArnt nRepeats dynMap =
>    removeZeros
>    $ aggrandize
>    $ tempo pArntTempo
>    $ keysig C Lydian
>    $ times nRepeats
>    $ bandPart pArntLead
>      ((if includeOpen then zOpenT        else rest 0)
>         :+: (if includeClos then zClosT  else rest 0))
>      :=: bandPart pArntStrum
>      ((if includeOpen then zOpenG        else rest 0)
>         :+: (if includeClos then zClosG  else rest 0))
>      :=: bandPart pArntBass
>      ((if includeOpen then zOpenB        else rest 0)
>         :+: (if includeClos then zClosB  else rest 0))
>
>      where
>
>      pArntLead                          = replace pArntLead_ dynMap
>      pArntStrum                         = replace pArntStrum_ dynMap
>      pArntBass                          = replace pArntBass_ dynMap
>
>      includeOpen                        = True
>      includeClos                        = False -- intentional, TODO: cleanup
>
> zOpenT1 = addDur en [c 3, e 3, fs 3, g 3,
>                      a 3, b 3,  c 4, e 4]
> zOpenT2 = line [fs 4 qn, fs 4 en, g 4 qn, g 4 en,
>                  e 4 qn,  e 4 en, f 4 qn, f 4 en]
> zOpenT3 = line [e 4 qn,  e 4 en,
>                 Modify (Phrase [Dyn $ Diminuendo 0.9]) (e 4 dwn),
>                 rest qn, rest dqn]
> zOpenT = line [zOpenT1, zOpenT2, zOpenT3]
> zClosT = zOpenT
>
> zOpenG1 = rest wn
> zOpenG2 = line [pTriadD dqn, pTriadG dqn,
>                 pTriadC dqn, pTriadBf dqn,
>                 pTriadSusA dqn,
>                 Modify (Phrase [Dyn $ Diminuendo 0.9]) (pTriadDim7A dwn),
>                 rest qn, rest dqn]
> zOpenG = line [zOpenG1, zOpenG2]
> zClosG = zOpenG
>
> zOpenB1 = line [c 3 hn, a 2 hn, d 3 dqn, g 3 dqn, c 3 dqn, bf 2 dqn]
> zOpenB2 = line [a 2 dqn,
>                 Modify (Phrase [Dyn $ Diminuendo 0.9]) (g 2 dwn),
>                 rest qn, rest dqn]
> zOpenB = line [zOpenB1, zOpenB2]
> zClosB = zOpenB

Pan ===================================================================================================================

> panTempo                              = 1
> panTranspose                          = 0
>
> panLead_                              = makePitched AltoSax                panTranspose 0 100
> panStrum_                             = makePitched AcousticGuitarSteel    panTranspose 0 100
> panBass_                              = makePitched AcousticBass           panTranspose 0 100
>
> pan dynMap =
>     removeZeros
>     $ tempo panTempo
>     $ keysig Af Mixolydian
>     $ bandPart panLead
>       ((if includeOpen then xOpenT        else rest 0)
>          :+: (if includeClos then xClosT  else rest 0))
>       :=: bandPart panStrum
>       ((if includeOpen then xOpenG        else rest 0)
>          :+: (if includeClos then xClosG  else rest 0))
>       :=: bandPart panBass
>       ((if includeOpen then xOpenB        else rest 0)
>          :+: (if includeClos then xClosB  else rest 0))
>       where
>
>         panLead                          = replace panLead_ dynMap
>         panStrum                         = replace panStrum_ dynMap
>         panBass                          = replace panBass_ dynMap
>
>         includeOpen = True
>         includeClos = True
>
> xOpenT1 = addDur hn [gf 3, c 4, ef 4, af 4]
> xOpenT2 = line [gf 3 hn, af 3 sn,  bf 3 sn,  c 4 sn,
>                 df 4 sn, ef 4 sn,   f 4 sn, gf 4 sn, f 4 sn]
> xOpenT3 = line [ef 4 hn, df 4 dhn, ef 4 qn, df 4 en, c 4 en, df 4 en]
> xOpenT = line [xOpenT1, xOpenT2, xOpenT3]
> xClosT = xOpenT
>
> xOpenG1 = rest wn
> xOpenG2 = line [pTriadAf hn]
> xOpenG = line [xOpenG1, xOpenG2]
> xClosG = xOpenG
>
> xOpenB1 = line [gf 1 hn]
> xOpenB2 = line [af 1 hn]
> xOpenB = line [xOpenB1, xOpenB2]
> xClosB = xOpenB

Rattan ================================================================================================================

> ratTempo                                 = 1
> ratTranspose                             = 0
>
> ratLead_                                 = makePitched Flute           ratTranspose 0 100
> ratBass_                                 = makePitched SynthBass1      ratTranspose 0 100
> ratPerc_                                 = makeNonPitched                             100
> 
> rattan dynMap =
>     removeZeros
>     $ aggrandize
>     $ tempo ratTempo
>     $ keysig E Locrian
>     $ chord [vpart, spart, ppart]
>
>   where
>
>   ratLead                                = replace ratLead_ dynMap
>   ratBass                                = replace ratBass_ dynMap
>   ratPerc                                = replace ratPerc_ dynMap
>
>   vpart                                  = bandPart ratLead (times 2 vline)
>   spart                                  = bandPart ratBass (times 2 sline)
>   ppart                                  = bandPart ratPerc (times 2 pline)
>
> vline = line [vl_l01a, vl_101b, vl_l01a, vl_101b', vl_l01a, vl_101b, vl_l01a, vl_101b''
>             , vl_201a, vl_201a, vl_201a, vl_201a,  vl_201b, vl_201b, vl_201b, vl_201b
>             , vl_301a, vl_301a, vl_301a, vl_301a,  vl_301b]
>
> vl_l01a = line [chord [af 4 dqn, d 5 dqn, g 5 dqn]
>               , chord [af 4 en,  d 5 en,  g 5 en]
>               , rest en, f 5 en, e 5 en, d 5 en
>               , c 5 en, d 5 en, e 5 en, f 5 en]
> vl_101b = line [e 5 qn, c 5 qn]
> vl_101b' = e 5 hn
> vl_101b'' = line [e 5 en, d 5 en, c 5 en, b 4 en]
> vl_201a = triad A Minor (A, 4) qn
> vl_201b = triad F Major (A, 4) qn
> vl_301a = chord [af 4 qn, d 5 qn, gf 5 qn]
> vl_301b = tempo (3/2) (line [c 4 en, d 4 en, e 4 en, d 4 en, e 4 en, fs 4 en, e 4 en, fs 4 en, gs 4 en])
> vl_102 = rest 0
> vl_103 = rest 0
>
> sline = line [sl_l01, sl_l01, sl_l01, sl_l01, sl_102, sl_103]
>
> sl_l01 = line [e 2 qn, e 2 qn, e 2 qn, e 2 qn, bf 2 qn, bf 2 qn, c 3 qn, c 3 qn]
> sl_102 = line [a 2 qn, a 2 qn, a 2 qn, a 2 qn, ef 2 qn, ef 2 qn, ef 2 qn, ef 2 qn]
> sl_103 = line [d 2 qn, d 2 qn, d 2 qn, d 2 qn, c 2 qn, d 2 qn, e 2 qn]
>
> pline   = line [pline01, pline02, pline03]
> pline01 = line [times 2 (line [percLTqn, percHTen, roll sn (perc AcousticSnare wn), percCCen, percCCqn])]
> pline02 = line [times 2 (line [percCHHen, percOHHqn, percOHHqn, percBDqn])]
> pline03 = line [times 2 (line [roll en (perc ClosedHiHat 2), percCHHen, percOHHqn, percOHHqn, percBDqn])]

Kit ===================================================================================================================

> kitTempo                                 = 2
> kitTranspose                             = 0
>
> kitLead_                                 = makePitched FrenchHorn kitTranspose 0 100
> kitPerc_                                 = makeNonPitched                        100
>
> kit                    :: DynMap → Music (Pitch,[NoteAttribute])
> kit dynMap =
>   removeZeros
>   $ aggrandize
>   $ tempo kitTempo
>   $ chord [bandPart kitPerc npart, bandPart kitLead tpart]
>
>   where
>
>   kitLead                                = replace kitLead_ dynMap
>   kitPerc                                = replace kitPerc_ dynMap
>
>   p1 = perc LowTom
>   p2 = perc AcousticSnare
>   p3 = perc CrashCymbal2
>
>   c1qn = chord [c 4 qn, e 4 qn, fs 4 qn, g 4 qn]
>   c1en = chord [c 4 en, e 4 en, fs 4 en, g 4 en]
>   c2qn = chord [b 3 qn, ds 4 qn, f 4 qn, g 4 qn]
>   c2en = chord [b 3 en, ds 4 en, f 4 en, g 4 en]
>   c3qn = chord [ds 4 qn, f 4 qn, a 4 qn, c 5 qn]
>   c3en = chord [ds 4 en, f 4 en, a 4 en, c 5 en]
>   c4qn = chord [e 4 qn, fs 4 qn, as 4 qn, c 5 qn]
>   c4en = chord [e 4 en, fs 4 en, as 4 en, c 5 en]
>   c5qn = chord [b 4 qn, ds 5 qn]
>   c5en = chord [b 4 en, ds 5 en]
>   c6qn = chord [a 4 qn, cs 5 qn]
>   c6en = chord [a 4 en, cs 5 en]
>   c7qn = chord [g 4 qn, b 4 qn]
>   c7en = chord [g 4 en, b 4 en]
>
>   npart = line [npart01, npart02]
>   npart01 = times 4 nline01
>   npart02 = percCCqn
>   nline01 = line [p1 qn, p1 en, p1 en, p1 en, p1 en, p1 en, p1 en, p2 en, p2 en, p2 qn, p2 qn]
>   tpart =   line [tpart01, tpart02]
>   tpart01 = line [tline01, tline01, tline02, tline02]
>   tpart02 = line [tline03, tline04]
>   tline03 = line [c5en, c6qn, c6en]
>   tline04 = line [c6en, c6qn, c7en]
>   tline01 = line [c1qn, times 6 c1en, times 2 c2en, times 2 c2qn]
>   tline02 = line [c3qn, times 6 c3en, times 2 c4en, times 2 c4qn]

Pit ===================================================================================================================

> pitTempo                                 = 1
> pitTranspose                             = 0
>
> pitLead_                                 = makePitched EnglishHorn           pitTranspose 0 100
> pitStrum_                                = makePitched AcousticGuitarSteel   pitTranspose 0 100
> pitBass_                                 = makePitched SynthBass1            pitTranspose 0 100
>
> pit1 = line [b 3 en, cs 4 en, ds 4 en, cs 4 en, fs 4 en, ds 4 en, cs 4 en, f 4 en]
> pit2 = line [c 4 en, ds 4 en,  f 4 en, ds 4 en, fs 4 en, ds 4 en, d 4 en, ds 4 en]
> pitA = times 2 $ line [times 4 pit1, times 4 pit2]
> pitB = times 2 $ line [times 4 $ b 2 wn, times 4 $ gs 2 wn]
> pitC = line [rest (4 * wn),  b 3 sn,  c 4 sn, cs 4 sn,  d 4 sn
>                           , ds 4 sn,  e 4 sn,  f 4 sn, fs 4 sn
>                           ,  g 4 sn, gs 4 sn,  a 4 sn, as 4 sn
>                           , times 4 (chord [b 4 qn, ds 4 qn, fs 4 qn])
>                           , times 2 (chord [c 5 qn,  d 5 qn, fs 4 qn])
>                           , times 2 (chord [c 5 qn, fs 5 qn, as 4 qn])
>                           ,  c 5 sn,  b 4 sn,  as 4 sn,  a 4 sn
>                           , gs 4 sn,  f 4 sn,   e 4 sn, ds 4 sn
>                           ,  d 4 sn, cs 4 sn,   c 4 sn,  b 3 sn
>                           , as 3 sn,  a 3 sn,  gs 3 sn,  g 3 sn
>                           , fs 3 sn,  f 3 sn,   e 3 sn,  f 3 sn
>                           , fs 3 wn,  f 3 wn,  fs 3 qn, gs 3 qn
>                           ,  a 3 wn, chord [ d 3 hn,  f 3 hn,  b 3 hn]
>                                    , chord [ds 3 wn, fs 3 wn,  c 4 wn]
>             ]
>
> pit dynMap =
>   removeZeros
>   $ aggrandize
>   $ tempo pitTempo
>   $ keysig B Lydian
>   $     bandPart pitLead pitA
>     :=: bandPart pitBass pitB
>     :=: bandPart pitStrum pitC
>
>   where
>
>   pitLead                                = replace pitLead_ dynMap
>   pitStrum                               = replace pitStrum_ dynMap
>   pitBass                                = replace pitBass_ dynMap

Dit ===================================================================================================================

> ditTempo                                 = 1
> ditTranspose                             = -6
>
> ditBass_                                  = makePitched SynthBass1 ditTranspose 0 100
>
> dit dynMap = 
>   removeZeros
>   $ aggrandize
>   $ tempo ditTempo
>   $ keysig Af Mixolydian
>   $ bandPart ditBass (times 2 ditbody)
>
>   where
>
>   ditBass                                = replace ditBass_ dynMap
>
> ditbody =
>   line [rest dhn, ds 4 qn, fs 4 den, e 4 sn, ds 4 qn, e 4 den, cs 4 sn, a 3 qn, ds 3 den, ds 3 sn
>       , ds 4 den, e 4 sn, ds 4 sn, cs 4 en, cs 4 sn, b 3 den, a 3 sn, fs 3 den
>       , e 3 sn, ds 3 den, e 3 sn, fs 3 den, gs 3 sn, fs 3 den, gs 3 sn, fs 3 den, gs 3 sn
>       , fs 3 qn, fs 4 dqn, fs 4 en, t32 [e 4 en, ds 4 en, cs 4 en], cs 4 den, cs 4 sn
>       , b 3 den, cs 4 sn, ds 4 qn] 

Dadadada ==============================================================================================================

> da4Tempo                                 = 1
> da4Transpose                             = 0
>
> da4Lead_                                 = makePitched AltoSax da4Transpose 0 100
>
> dadadada =
>   removeZeros
>   $ tempo da4Tempo
>   $ keysig Af Mixolydian
>   $ bandPart da4Lead_ (times 4 body)
>
>   where
>
>   da4Lead                                = replace da4Lead_
>
>   body = line [af 4 en, bf 4 en, c 5 en, bf 4 en, c 5 en, bf 4 en]

Deydumpdum ============================================================================================================

> d3Tempo                                  = (3/2)
> d3Transpose                              = 0
>
> d3Lead_                                  = makePitched Clarinet             d3Transpose          0        100
> d3Bass_                                  = makePitched ElectricBassPicked   d3Transpose          0        100
>
> deyDumpDum dynMap =
>   removeZeros
>   $ aggrandize
>   $ tempo d3Tempo
>   $ keysig Af Mixolydian
>   $     bandPart d3Lead (line [         times 1 leadI])
>     :=: bandPart d3Bass (line [rest wn, times 2 bassI])
>
>   where
>
>   d3Lead                                = replace d3Lead_ dynMap
>   d3Bass                                = replace d3Bass_ dynMap
>
>   leadI = line [rest (hn + den), e 4 sn, a 3 en, rest en
>              , cs 4 (wn + den), e 4 sn, a 3 en, rest en
>              , c 4 en, rest en, b 3 en, rest en
>              , a 3 (wn + hn + den), e 4 sn, cs 4 en, rest en
>              , d 4 (wn + den), fs 4 sn, a 4 en, rest en, fs 4 en, rest en, f 4 en, rest en
>              , e 4 wn
>              , a 3 (hn + den), a 3 sn, c 4 en, rest en
>              , b 3 (wn + den), c 4 sn, c 4 en, rest en, c 4 en, rest en, b 3 en, rest en
>              , a 3 dhn, e 4 qn
>              , a 4 (hn + den), e 5 sn, a 4 en, rest en
>              , c 5 (wn + den), e 5 sn, a 4 en, rest en
>              , c 5 en, rest en, b 4 en, rest en, c 5 hn, b 4 hn
>              , a 4 (hn + den), b 4 sn, a 4 en, rest en
>              , g 4 hn, b 4 qn, d 5 qn, g 5 qn, b 5 qn
>              , c 5 en, rest en, b 4 en, rest en, c 5 hn, b 4 hn]
>
>   bassAtA = line [times 2 (line [a 2 hn, e 2 hn])]
>   bassAtD = line [times 2 (line [d 3 hn, a 2 hn])]
>   bassAtE = line [times 2 (line [e 2 hn, b 2 hn])]
>   bassAtG = line [times 2 (line [g 2 hn, d 2 hn])]
>
>   bassI =  line [bassAtA, bassAtA, bassAtD, bassAtA, bassAtE, bassAtA]
>   bassII = line [bassAtA, bassAtD, bassAtG, bassAtA, bassAtE, bassAtA]
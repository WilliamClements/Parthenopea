> {-# LANGUAGE UnicodeSyntax #-}

Cecil
William Clements
December 18, 2022

> module Cecil where

> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import Parthenopea ( triad )

Cecil's Asleep ========================================================================================================

> cecil :: Music (Pitch, Volume)
> cecil =
>    removeZeros
>    $ tempo 1
>    $ transpose 0
>    $ keysig G Dorian
>    $ addVolume 100
>    $ instrument Violin
>      ((if      includeI    then rAltoI     else rest 0)
>        :+: (if includeII   then rAltoII    else rest 0)
>        :+: (if includeIII  then rAltoIII   else rest 0)
>        :+: (if includeIV   then rAltoIV    else rest 0))
>      :=: instrument TenorSax
>      ((if      includeI    then rTenrI     else rest 0)
>        :+: (if includeII   then rTenrII    else rest 0)
>        :+: (if includeIII  then rTenrIII   else rest 0)
>        :+: (if includeIV   then rTenrIV    else rest 0))
>      :=: instrument AcousticBass
>      ((if      includeI    then rBassI     else rest 0)
>        :+: (if includeII   then rBassII    else rest 0)
>        :+: (if includeIII  then rBassIII   else rest 0)
>        :+: (if includeIV   then rBassIV    else rest 0))
>
>    where
>
>       includeI     = True
>       includeII    = True
>       includeIII   = True
>       includeIV    = True
>
> -- Section I "Cecil's Asleep"
>
> rAltoI  = line [rAltoIAB, c 5 sn, rAltoIC, rest sn, cutOutAllOfTheNoise, whatIsGoingOn]
>
> rAltoIAB
>         = line [rAltoIA, rest sn, rAltoIA, rest sn
>               , rAltoIB, c 5  sn, rAltoIA, rest sn
>               , rAltoIA, rest sn, rAltoIA, rest sn
>               , rAltoIB]
>
> rAltoIA = tempo 3 (line [bf 4 qn, bf 4 qn, bf 4 qn])
>           :+: line [g 4 den]
> rAltoIB = tempo 3 (line [bf 4 qn, bf 4 qn, bf 4 qn])
>           :+: line [c 5 den]
> rAltoIC = tempo 3 (line [ d 5 qn, d 5 qn,  d 5 qn])
>           :+: line [g 4 den]
>
> cutOutAllOfTheNoise =
>   line [g 4 en, f 4 qn, d 4 qn, c 4 en, bf 3 qn, g 3 hn]
>   
> whatIsGoingOn
>         = line [bf 4 qn, c 5 qn, d 5 qn, d 4 qn, g 4 hn, rest hn]
>
> rTenrIA = transpose (-12) rAltoIA
> rTenrIB = transpose (-12) rAltoIB
>
> rTenrI  =
>   line [rTenrIA, rest sn, rTenrIA, rest sn, rTenrIB,  c 4 sn, rTenrIA, rest sn
>       , rTenrIA, rest sn, rTenrIA, rest sn, rTenrIB, rest sn, rest (4*wn)]
>
> rBassI  = line [  rBassIA,  rBassIB]
>
> rBassIA = line [  g 2 wn, f 2 qn, ef 2 qn, d 2 hn, f 2 qn, d 2 qn, g 2 wn
>               ,  rest hn]
> rBassIB = line [transpose (-12) cutOutAllOfTheNoise, g 2 hn,  d 2 hn, g 2 wn]
>
> -- Section II (like Section I until not including "cut out all of the noiae!" etc.
>
> rAltoII = line [rAltoIIAB, d 5 sn, rAltoIIC, rAltoIID', rAltoIID'']
>
> rAltoIIB  = rAltoIB
> rAltoIIAB = rAltoIAB
> rAltoIIC = line [f 5 en, f 5 en, e 5 qn, d 5 wn]
>
> r2triad1 = triad  D Major                (A,  3)
> r2triad2 = triad  B (CustomMode "Dim")   (B,  3)
> r2triad3 = triad  G Major                (B,  3)
> r2triad4 = triad  C Major                (C,  4)
> r2triad5 = triad  F Major                (A,  3)
> r2triad6 = triad  E Major                (Gs, 3)
> r2triad7 = triad Bf Major                (Bf, 3)
> r2triad8 = triad Ef Major                (Bf, 3)
> r2triad9 = triad  G Minor                (D,  4)
> r2triadA = triad  A Major                (A,  3)
> r2triadB = triad  D Minor                (A,  3)
>
> weBetterCall = line [rest en, r2triad1 en, r2triad1 en, r2triad1 en
>                     , r2triad1 dqn, r2triad1 en]
> thePrincipal  = line [r2triad2 en, r2triad2 en, r2triad2 hn, rest qn] 
> thePrincipal' = line [r2triad3 en, r2triad3 en, r2triad3 hn, rest qn]
> rAltoIID'     = line [weBetterCall, thePrincipal] 
> rAltoIID''    = line [weBetterCall, thePrincipal'] 
>
> rTenrII   = line [rTenrI, rest hn, rest wn]
>
> rBassII   = line [  rBassIIA,  g 2 wn, rBassIIB]
>
> rBassIIA  = rBassIA
> rBassIIB  = line [d 2 wn, g 2 wn, d 2 wn, g 2 wn]
>
> -- Section III "Cecil comes from a broken home ... so it's all right!"
>
> rAltoIII  = line [rAltoIIIA, rAltoIIIB, rAltoIIIC, rAltoIIID
>                 , rAltoIIIE, rAltoIIIF, rAltoIIIG, rAltoIIIH
>                 , rAltoIIII, rAltoIIIJ, rAltoIIIK]
>
> --                 Ce-      cil    comes     from     a
> rAltoIIIA = line [ g 4 qn, a 4 qn, b 4 qn,  a 4 en, a 4 en]
>
> --                 bro-     ken    home -     He
> rAltoIIIB = line [ g 4 qn, a 4 qn, b 4 dqn, b 4 en]
>
> --                 has      to      do       all      the
> rAltoIIIC = line [ a 4 en, a 4 en, b 4 qn,  c 5 qn,  b 4 qn]
>
> --                 chores   in       his      home -   he
> rAltoIIID = line [ a 4 qn, b 4 en, b 4 en,  a 4 dqn, a 4 en]
>
> --                 did      n't     get       to      bed    'til
> rAltoIIIE = line [ b 4 en, b 4 en, c 5 en,  c 5 en,  d 5 qn, d 5 qn]
>
> --                twelve    last    night    so       it's
> rAltoIIIF = line [ e 5 qn, g 4 qn, c 5 qn,  c 5 en,  c 5 en]
>
> --                  all     right    
> rAltoIIIG = line [ d 5 wn, d 5 qn, rest hn]
>
> --                 for      Ce-     cil      to      sleep
> rAltoIIIH = line [ d 4 sn, d 4 sn, e 4 sn, fs 4 sn,  g 4 qn, rest qn]
>
> --                  Yes,     it's    all    right
> rAltoIIII = line [ d 4 qn, d 4 qn, d 5 wn,  d 5 qn, rest qn]
>
> --                  for      Ce-     cil      to
> rAltoIIIJ = line [ d 5 qn, d 5 qn, cs 5 qn, c 5 qn]
>
> --                 Slee     -ee-    -ee-     -eep             He's       a
> rAltoIIIK = line [ b 4 wn, b 4 wn, b 4 wn,  e 5 wn, rest qn, e 5 den, f 5 sn]
> 
> rTenrIII  = line [rTenrIIIA, rTenrIIIB, rTenrIIIC, rTenrIIID, rTenrIIIE]
>
> rTenrIIIA  = line [r2triad3 wn, r2triad3 wn, r2triad1 wn, r2triad1 wn]
> rTenrIIIB  = line [r2triad3 wn, r2triad4 wn, r2triad1 wn]
> rTenrIIIC  = line [r2triad1 qn, rest dhn, rest wn]
> rTenrIIID  = line [r2triad1 wn, r2triad1 qn, rest qn, r2triad1 wn, r2triad3 wn]
> rTenrIIIE  = line [r2triad5 wn, r2triad6 wn, r2triad4 wn, r2triad2 hn]
>
> rBassIII  = line [ rBassIIIA, rBassIIIB, rBassIIIC
>                  , rBassIIID, rBassIIIE, rBassIIIF]
>
> rBassIIIA = line [ g 2 hn, d 2 hn,  g 2 hn,  d 2 hn]
> rBassIIIB = line [ d 2 hn, a 1 hn,  d 2 hn, fs 2 hn]
> rBassIIIC = line [ g 2 wn, c 2 wn]
> rBassIIID = line [ d 2 wn, d 2 qn, rest dhn, rest wn, d 2 wn, d 2 qn, rest qn
>                 ,  d 2 qn, d 2 qn, e 2 qn, fs 2 qn]
> rBassIIIE = line [g 2 wn, f 2 wn, e 2 wn, c 2 wn]
> rBassIIIF = line [g 2 hn]
>
> -- Section IV "He's a Frog!"
>
> rAltoIV   = line [rAltoIVA, rAltoIVB, rAltoIVC, rAltoIVD, rAltoIVE, rAltoIVF]
>   
> --                 Frog      Hes     a      Frog     in       the
> rAltoIVA  = line [g 5 qn,  g 5 den, a 5 sn, g 5 qn, e 5 den, f 5 sn]
> --                 Lit-      tle    for-    est     glade      He's       a
> rAltoIVB  = line [g 5 den, g 5 sn, a 5 den, a 5 sn, g 5 qn,  g 5 den,  g 5 sn]
> --                 Frog      Hes     a      Frog     in       the
> rAltoIVC  = line [f 5 qn,  f 5 den, g 5 sn, f 5 qn, d 5 den, ef 5 sn]
> --                 Lit-      tle    for-    est     glade      He's       a
> rAltoIVD  = line [f 5 den, f 5 sn, g 5 den, g 5 sn, f 5 qn,  f 5 den,  f 5 sn]
> --                 Frog      Hes     a      Frog     He's      a
> rAltoIVE  = line [e 5 qn,  e 5 den, f 5 sn, e 5 qn, cs 5 den, d 5 sn]
> --                Frog he's a Frog He's a
> rAltoIVF  = line [d 5 hn,  c 5 hn, bf 4 hn]
>
> rTenrIV   = transpose (-12) (line [rTenrIVA, rTenrIVB, rTenrIVC, rTenrIVD
>                 , rTenrIVE, rTenrIVF])
>
> rTenrIVA  = line [r2triad4 qn,  r2triad4 den, r2triad5 sn, r2triad4 qn
>                 , r2triad4 den, r2triad4 sn]
> rTenrIVB  = line [r2triad4 den, r2triad4 sn,  r2triad5 den, r2triad5 sn
>                 , r2triad4 qn,  r2triad4 den, r2triad4 sn]
> rTenrIVC  = line [r2triad7 qn,  r2triad7 den, r2triad8 sn, r2triad7 qn
>                 , r2triad7 den, r2triad7 sn]
> rTenrIVD  = line [r2triad7 den, r2triad7 sn,  r2triad8 den, r2triad8 sn
>                 , r2triad7 qn,  r2triad7 den, r2triad7 sn]
> rTenrIVE  = line [r2triadA qn,  r2triadA den, r2triadB sn, r2triadA qn
>                 , r2triadA den, r2triadA sn]
> rTenrIVF  = line [r2triad3 hn,  r2triad5  hn, r2triad8 hn]
>
> rBassIV   = rest 0

Abby Cissa ============================================================================================================

> littleAbby :: Music (Pitch, Volume)
> littleAbby =
>    removeZeros
>    $ tempo 1
>    $ transpose 0
>    $ keysig C Major
>    $ addVolume 80
>    $ instrument Trumpet
>      (line [e 5 en,  f 5 en,  g 5 qn, c 6 qn, bf 5 en, a 5 en, g 5 hn])
>      -- (line [f 5 en,  g 5 en])
>      -- (c 5 wn)
>      -- (line [f 5 en,  g 5 en, f 5 en, e 5 en,  c 5 qn])
>
> abby :: Music (Pitch, Volume)
> abby =
>    removeZeros
>    $ tempo 1
>    $ transpose 0
>    $ keysig C Major
>    $ addVolume 100
>    $ instrument RhodesPiano
>      aLink
>
>    where
>
>    aLink, aLinkI, aLinkII, aLinkIII :: Music Pitch
>    aLink  = line [aLinkI, aLinkII, aLinkI
>                  , tempo 2 (line [aLinkII, aLinkIII])]
>    aLinkI = line [aLinkIA, aLinkIB, aLinkIC, aLinkID, aLinkIE, aLinkIF]
>
>    aLinkIA, aLinkIB, aLinkIC, aLinkID, aLinkIE, aLinkIF :: Music Pitch
>    --               Ab-        -by    Cis     Ab-      -by    Cis
>    aLinkIA = line [c 4 den,  e 4 sn, xNote, a 4 den, fs 4 sn, yNote]
>    --                Ab-      -by     Cis-    -sa
>    aLinkIB = line [b 4 den,  a 4 sn, g 4 en, d 5 hn, rest en]
>    --               He      is       the      fair-     y     that
>    aLinkIC = line [c 5 qn, a 4 den, a 4 sn,  g 4 en, c 5 qn, c 5 en]
>    --               lives   in    the lib-      rar-    y 
>    aLinkID = line [c 5 qn, a 4 den, g 4 sn, fs 4 en, c 5 qn, rest en]
>    --                His     is    the       face   that
>    aLinkIE = line [c 5 qn, a 4 en,  a 4 en,  f 4 qn, c 5 qn]
>    --              haunts... It's    Mis-     -ter    Gall     (-ant's!)
>    aLinkIF = line [d 5 dhn, g 4 sn, g 4 sn,  a 4 sn,  d 5 sn]
>
>    xNote   = Modify (Phrase [Art (Staccato (1/4))]) (g 4 qn)
>    yNote   = Modify (Phrase [Art (Staccato (1/4))]) (d 4 qn)
>
>    aLinkII = line [aLinkIIA, aLinkIIB, aLinkIIC, aLinkIID, aLinkIIB, aLinkIIC] 
>
>    aLinkIIA = line [c 5 qn, mister,  e 4 en, mister,  e 4 en, mister, e 4 en]
>    aLinkIIB = line [gallant, e 4 en, gallant, e 4 qn
>                   ,  e 4 en, gallant, e 4 en]
>    aLinkIIC = line [ratat1 dqn, ratat1 qn, mcgrath, grayce
>                   , ratat2 qn, ratat2 qn, ratat3 hn]
>    aLinkIID = line [mister, e 4 en, mister, e 4 en, mister, e 4 en
>                   , mister, e 4 en]
>
>    mister  = chord [ g 4 en,  c 5 en]
>    mcgrath = chord [ e 4 en,  g 4 en]
>    grayce  = chord [ d 4 qn,  f 4 qn]
>    fat     = chord [ a 4 en,  c 5 en]
>    fingers = chord [ b 4 en,  d 5 en]
>    gallant = triad Fs (CustomMode "Dim") (Fs, 4) en
>    ratat1  = triad F  Major              (F, 4)
>    ratat2  = triad C  Major              (C, 4)
>    ratat3  = triad G  Major              (B, 3)
>
>    aLinkIII   =
>      line [aLinkIIIA, aLinkIIIB, aLinkIIIC, aLinkIIID, aLinkIIIE, aLinkIIIF]
>    aLinkIIIA  =
>      line [mister,  e 4 en, mister,  e 4 en, mister,  e 4 en, mister,  e 4 en]
>    aLinkIIIB  =
>      line [gallant, e 4 en, gallant, e 4 en, gallant, e 4 en, gallant, e 4 en]
>    aLinkIIIC  =
>      line [fat,     f 4 en, fat,     f 4 en, fat,     f 4 en, fat,     f 4 en]
>    aLinkIIID  =
>      line [fingers, g 4 en, fingers, g 4 en, fingers, g 4 en, fingers, g 4 en]
>    aLinkIIIE  = line [chord [ c 4 hn, e 4 hn, g 4 hn, a 4 hn, c 5 hn]]
>    aLinkIIIF  = line [aLinkIIIF1, aLinkIIIF2, aLinkIIIF3, aLinkIIIF4]
>    aLinkIIIF1 = line [c 3 tn, d 3 tn, e 3 tn, f 3 tn, g 2 tn, a 3 tn, b 3 tn]
>    aLinkIIIF2 = transpose 12 $ line [aLinkIIIF1, c 5 dqn]
>    aLinkIIIF3 = line [c 5 tn, b 4 tn, a 4 tn, g 4 tn, f 4 tn, e 4 tn, d 4 tn]
>    aLinkIIIF4 = transpose (-12) $ line [aLinkIIIF3, c 3 dqn]

There Goes W.J. =======================================================================================================

> wj :: Music (Pitch, Volume)
> wj =
>    removeZeros
>    $ tempo 1
>    $ transpose 0
>    $ keysig G Dorian
>    $ addVolume 100
>    $ instrument AltoSax
>       (line [wjAltoI,  wjAltoII,  wjAltoIII,  wjAltoIV])
>      :=: instrument HammondOrgan
>       (line [wjTenorI, wjTenorII, wjTenorIII, wjTenorIV])
>      :=: instrument ElectricBassFingered
>       (line [wjBassI,  wjBassII,  wjBassIII,  wjBassIV])
>
>    where
>
>    --                    There     goes    dou-      ble       you     jay
>    wjAltoI     = line  [ bf 4 qn, g 4 qn, bf 4 en, bf 4 en, bf 4 qn, c 5 wn]
>    wjBassI     = line  [ bf 2 qn, g 2 qn, bf 2 en, bf 2 en, bf 2 qn, c 3 wn]
>
>    wjTenorI    = line  [ wjTenorIA, wjTenorIB]
>    wjTenorIA   = line  [ minrI]
>    wjTenorIB   = line  [ suspI, rslvI]
>
>    --                   What     has     he      got       to      say
>    wjAltoII    = line [ c 5 en, c 5 en, c 5 qn, bf 4 qn, bf 4 qn, g 4 dhn, rest qn]
>    wjBassII    = line [ c 3 en, c 3 en, c 3 qn, bf 2 qn, bf 2 qn, g 2 dhn, rest qn]
>
>    wjTenorII   = line [ wjTenorIIA, wjTenorIIB]
>    wjTenorIIA  = line [ triad C Major (G, 3) hn
>                       , triad G Minor (G, 3) hn
>                       , triad G Minor (G, 3) dhn, rest qn]
>    wjTenorIIB  = rest 0
>
>    --                    He       says    phys-     ics      is
>    wjAltoIII  = line [ bf 4 qn, bf 4 qn, bf 4 en, bf 4 qn, bf 4 en
>    --                    ever-     y      thing
>                     ,   a 4 en,  a 4 en,  a 4 dhn]
>
>    wjTenorIII = line [suspII, rslvII]
>
>    wjBassIII  = line [  c 3 wn, f 4 wn]
>
>    --                   He's     om-       ni-      po-    tent
>    wjAltoIV   = line [ g 5 hn, g 5 qn, bf 5 dqn, g 5 en, g 5 wn]  
>    wjTenorIV  = line [ g 4 hn, g 4 qn, bf 4 dqn, g 4 en, g 4 wn]
>    wjBassIV   = line [ g 3 hn, g 3 qn, bf 3 dqn, g 3 en, g 3 wn]
>
>    minrI  = triad G Minor               (D, 4) wn
>    suspI  = triad C (CustomMode "Sus4") (F, 4) hn
>    rslvI  = triad C Major               (E, 4) hn
>
>    suspII = triad F (CustomMode "Sus4") (C, 4) wn
>    rslvII = triad F Major               (C, 4) wn

Shelby Parsley ========================================================================================================

> spEflat'      = triad Ef Major               (Bf, 3)
>
> littleshelby           :: Music (Pitch, Volume)
> littleshelby =
>   removeZeros
>   $ tempo 1
>   $ transpose 0
>   $ keysig Ef Mixolydian
>   (instrument Vibraphone             (addVolume 100 (line [rest qn, spEflat' qn]))
>   :=: instrument ElectricBassPicked  (addVolume 100 (line [ ef 2 wn ])))
>
> shelby :: Music (Pitch, Volume)
> shelby =
>   removeZeros
>   $ tempo 1
>   $ transpose 0
>   $ keysig Ef Mixolydian
>   $ addVolume 80
>   $ if skip_SP then rest 0
>                 else
>     (instrument Vibraphone
>       (line [spAltoI,  spAltoII,  spAltoIII])
>     :=: instrument ElectricBassPicked
>       (line [spBassI,  spBassII,  spBassIII]))
>        :+:
>     instrument RhodesPiano
>       (line [ chord [ triad B (CustomMode "Sus4") (B, 3) wn
>   --                            Does     It       Have    Trays?
>                       , line [ e 4 en, ds 4 en, cs 4 qn, b 3 dqn]]]
>    :+: line [ chord [ triad D Major               (A, 3) wn
>   --                            Does     It       Have    Trays?
>                       , line [fs 4 en,  f 4 en,  e 4 qn, d 4 dqn]]]
>    :+: line [ chord [ triad C Major               (G, 3) wn
>   --                            Does     It       Have    Trays ...
>                       , line [ e 4 en,  c 4 en,  e 4 qn, g 4 dqn]]]
>   --      ... Bill?
>    :+: triad A Major (Cs, 4) wn
>    :+: line [ chord [ triad B (CustomMode "Sus4") (B, 3) wn
>   --                            Does     It       Have    Trays?
>                       , line [ e 4 en, ds 4 en, cs 4 qn, b 3 dqn]]]
>    :+: line [ chord [ triad D Major               (A, 3) wn
>   --                            Does     It       Have    Trays?
>                       , line [fs 4 en,  f 4 en,  e 4 qn, d 4 dqn]]]
>    :+: line [ chord [ triad C Major               (G, 3) wn
>   --                            Does     It       Have    Trays ...
>                       , line [ e 4 en,  c 4 en,  e 4 en, g 4 dqn]]]
>   --      ... Bill?
>    :+: triad A Major (Cs, 4) wn
>    :+: line [  spTriF qn, spTriF qn
>             ,  spTriE qn, spTriE qn
>             , spTriEf qn, spTriEf qn, spTriB wn, rest wn]
>    :+: line [spTrie en, spTrie en, spTrie qn, spTriB' hn
>             ,  spTrig en, spTrig en, spTrig qn, spTriD  hn])
>    :=: (instrument ElectricBassPicked
>           (times 2 (line [b 2 wn, d 3 wn, c 3 wn, a 2 wn]))
>             :+: line [f 3 hn, e 3 hn, ef 3 hn, b 2 dwn])
>
>   where
>
>   skip_SP      = False
>
>   spAltoI      = line [spAltoIA, spAltoIB]
>   spAltoII     = line [spAltoIIA, spAltoIIB]
>   spAltoIII    = line [spAltoIIIA, spAltoIIIB, spAltoIIIC, spAltoIIID]
>
>   spBassI      = line [spBassIA,   spBassIB]
>   spBassII     = line [spBassIIA,  spBassIIB]
>   spBassIII    = line [spBassIIIA, spBassIIIB, spBassIIIC]
>
>   spEflat      = triad Ef Major               (Bf, 3)
>   spEfMin      = triad Ef Minor               (Ef, 4)
>   spAfDim      = triad  C (CustomMode "Dim")  (C,  4)
>   spAflat      = triad Af Major               (Af, 3)
>   spEfDim      = triad  G (CustomMode "Dim")  (Df, 4)
>   spTriF       = triad  F Major               ( C, 4)
>   spTriE       = triad  E Major               ( B, 3)
>   spTrie       = triad  E Minor               ( B, 3)
>   spTriEf      = triad Ef Major               (Ef, 4)
>   spTriB       = triad  B Major               (Ds, 4)
>   spTriB'      = triad  B Major               ( B, 3)
>   spTrig       = triad  G Minor               ( D, 4)
>   spTriD       = triad  D Major               ( D, 4)

Section I - the story

>   spAltoIA  = line [
>   --                    He's        Shel-
>      line [ rest qn, spEflat en, spEflat dqn, rest qn
>   --                      by         Pars-    el-       y
>           , rest qn, spAfDim en, spAfDim qn, f 4 en, spAflat qn],
>   --                    He's         short
>      line [ rest qn, spEflat en, spEflat dqn, rest qn
>   --                      but        he's       migh-     igh-       ty
>           , rest den,   gf 4 sn, gf 4 en, spAfDim qn, f 4 en, spAflat qn],        
>   --                    He        calls
>      line [ rest qn, spEflat en, spEflat dqn, rest qn
>   --                     Bill       Griff    and       if
>          , rest qn, spAfDim en, spAfDim qn, f 4 en, spAflat qn],
>   --                   He        gets
>      line [ rest qn, spEflat en, spEflat dqn, rest qn
>   --                   thrown      out      on  his       ear
>           , rest den, gf 4 sn,   gf 4 en, spAfDim qn, f 4 en, spAflat qn],
>   --                    That's       why
>      line [ rest qn, spEflat en, spEflat dqn, rest qn
>   --                   we're       here
>           , rest qn, spEfMin en, spEfMin qn
>   --                     to                     cheer
>           , chord [f 4 en, af 4 en], chord [ef 4 qn, gf 4 qn]]]
>
>   spAltoIB  = rest 0
>   spBassIA  = times 5
>               $ line [ ef 2 wn, af 1 wn]
>   spBassIB  = rest 0
>

Section II - improvisation

>   spAltoIIA = line [
>   --              Shel-       by!
>      line [ spEfDim en, spEfDim qn,   spEfDim hn, rest en
>          ,     rest qn, spEfMin en, spEfMin qn, af 4 en, gf 4 en, ef 4 en],
>   --          Shel-       by!
>      line [ spEfDim en, spEfDim qn,   spEfDim hn, rest en
>          ,     rest qn, spEfMin en, spEfMin qn, af 4 en, gf 4 en, ef 4 en],
>   --          Shel-       by!
>      line [ spEfDim en, spEfDim qn,   spEfDim hn, rest en
>          ,     rest qn, spEfMin en, spEfMin qn, af 4 en, gf 4 en, ef 4 en],
>   --          Shel-       by!
>      line [ spEfDim en, spEfDim qn,   spEfDim hn, rest en
>          ,     rest qn, spEfMin en, spEfMin qn, af 4 en, gf 4 en, f 4 en]]
>   spAltoIIB    = ef 4 wn
>
>   spBassIIA    = times 4
>                $ line [ ef 2 wn, af 1 wn]
>   spBassIIB    = rest 0
>

Section III - shame on you!

>   spAltoIIIA   = line
>      [ 
>   --     Shel     by       you     be-     lieve
>         c 4 en, bf 3 qn,  g 3 qn, bf 3 en,  c 4 qn
>   --     all     you       hear    and       then
>      , ef 4 en,  c 4 qn, bf 3 qn,  g 3 en, bf 3 qn
>   --     tell     it        a-      a-
>      ,  c 4 en, bf 3 qn,  g 3 qn, bf 3 en
>   --    gain     as
>      ,  c 4 en, bf 3 en
>   --              if                   it's
>      , chord [b 3 en, gf 4 en], chord [b 3 qn,  f 4 qn]
>   --                 true
>      , triad Ef Major ( G, 3) hn, rest en
>   --         shame                        on
>      , chord [b 3 en, gf 4 en], chord [b 3 qn,  f 4 qn]]
>   --                            you
>   spAltoIIIB   = line [triad Ef Major (Bf, 3) hn, rest en]
>   spAltoIIIC   = spAltoIIIA
>   spAltoIIID   = line [ chord [g 3 hn, bf 3 hn, c 4 hn, ef 4 hn], rest en]
>
>   spBassIIIA   = times 18
>                $ line [ ef 2 en, ef 3 en]
>   spBassIIIB   = times 24
>                $ line [ ef 2 en, ef 3 en]
>   spBassIIIC   = line [ ef 2 den, ef 2 sn, ef 3 den, ef 2 sn]

We Hate Her ===========================================================================================================

> weHateHer :: Music (Pitch, Volume)
> weHateHer =
>    removeZeros
>    $ tempo 1
>    $ transpose 0
>    $ keysig G Dorian
>    $ addVolume 100
>    $ instrument Harmonica whhMel
>
> littleWeHateHer =
>    addVolume 100
>    $ instrument AltoSax (bf 4 (2 * wn) {- :+: rest wn :+: bf 4 en :+: rest wn :+: bf 4 qn -})
>
> whhMel :: Music Pitch
>    --                      We
> whhMel = line  [rest dqn, g 4 en
>    --             Hate     Her     We      Hate      Her     We  
>               ,  bf 4 en, g 4 qn, g 4 en, bf 4 en,  g 4 qn, g 4 en
>    --             real-     ly      real-    ly      Hate     Her     She
>               ,   a 4 en, f 4 en, a 4 en,  c 5 en, bf 4 en, g 4 qn, g 4 en
>    --              gave     us       a       te-      st      It
>               ,  bf 4 en, g 4 qn, g 4 en, bf 4 en,  g 4 qn, g 4 en
>    --              real-     ly    was       a        me-     -ss     We
>               ,   a 4 en, f 4 en, a 4 en,  c 5 en, bf 4 en, g 4 qn, g 4 en
>    --             chea-     ted    We      chea-      ted    We
>               ,  bf 4 en, g 4 qn, g 4 en, bf 4 en,  g 4 qn, g 4 en
>    --              did-    n't     do      our       best
>               ,   d 4 en, d 4 en, e 4 en, fs 4 en,  g 4 wn]               

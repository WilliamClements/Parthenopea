> {-# LANGUAGE UnicodeSyntax #-}

Players
William Clements
February 6, 2023

> module Players where

> import Euterpea.IO.MIDI.Play
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.Music
> import HSoM.Performance
  
playerOne =============================================================================================================

> p1PMap :: String → Player (Pitch, [NoteAttribute])
> p1PMap "PlayerOne"    = playerOne
>
> p1Con  :: Context Note1
> p1Con  = Context {   cTime    = 0,
>                      cPlayer  = playerOne,
>                      cInst    = AcousticGrandPiano,
>                      cDur     = metro 120 qn,
>                      cPch     = 0,
>                      cKey     = (C, Major),
>                      cVol     = 100 }
>
> playerOne :: Player (Pitch, [NoteAttribute])
> playerOne  = MkPlayer {  pName         = "PlayerOne",
>                          playNote      = defPlayNote defNasHandler,
>                          interpPhrase  = p1Interp}
>
> p1Interp :: PMap a
>             → Context a
>             → [PhraseAttribute]
>             → Music a
>             → ([MEvent], DurT)
> p1Interp pm 
>   c@Context {  cTime = t, cPlayer = pl, cInst = i, 
>                cDur = dt, cPch = k, cVol = v}
>   (pa:pas) m =
>   let  pfd@(pf,dur)  =  p1Interp pm c pas m
>        loud x        =  p1Interp pm c (Dyn (Loudness x) : pas) m
>        stretch x     =  let  t0 = eTime (head pf);  r  = x/dur
>                              upd e@MEvent {eTime = t, eDur = d} = 
>                                let  dt  = t-t0
>                                     t'  = (1+dt*r)*dt + t0
>                                     d'  = (1+(2*dt+d)*r)*d
>                                in e {eTime = t', eDur = d'}
>                         in (map upd pf, (1+x)*dur)
>        inflate x     =  let  t0  = eTime (head pf);  
>                              r   = x/dur
>                              upd e@MEvent {eTime = t, eVol = v} = 
>                                  e {eVol =  round ( (1+(t-t0)*r) * 
>                                             fromIntegral v)}
>                         in (map upd pf, dur)
>   in case pa of
>     Dyn (Accent x) →
>         ( map (\e → e {eVol = round (x * fromIntegral (eVol e))}) pf, dur)
>     Dyn (StdLoudness l) → 
>         case l of 
>            PPP  → loud 40;       PP → loud 50;   P    → loud 60
>            MP   → loud 70;       SF → loud 80;   MF   → loud 90
>            NF   → loud 100;      FF → loud 110;  FFF  → loud 120
>     Dyn (Loudness x)     →  p1Interp pm
>                              c{cVol = round x} pas m
>     Dyn (Crescendo x)    →  inflate   x ; Dyn (Diminuendo x)  → inflate (-x)
>     Tmp (Ritardando x)   →  stretch   x ; Tmp (Accelerando x) → stretch (-x)
>     Art (Staccato x)     →  (map (\e → e {eDur = x * eDur e}) pf, dur)
>     Art (Legato x)       →  (map (\e → e {eDur = x * eDur e}) pf, dur)
>     Art (Slurred x)      → 
>         let  lastStartTime  = foldr (max . eTime) 0 pf
>              setDur e       =   if eTime e < lastStartTime
>                                 then e {eDur = x * eDur e}
>                                 else e
>         in (map setDur pf, dur) 
>     Art _                → pfd
>     Orn _                → pfd

The End
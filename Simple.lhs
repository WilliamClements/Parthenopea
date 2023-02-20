> {-# LANGUAGE UnicodeSyntax #-}

Simple examples with Euterpea's Music data structures
William Clements
Last modified: 27-September-2022

> module Simple where
> import Euterpea.Music
> import Debug.Trace ( trace, traceM )
> import Euterpea.IO.MIDI.MidiIO ( getAllDevices, name )

> bb12Together :: Music Pitch
> bb12Together = tempo 1.75 (instrument Woodblock (drumsPart 79))
>                           :=: instrument FretlessBass (bassPart 32)

> drumsPart :: AbsPitch → Music Pitch 
> drumsPart x = if stdvol < thresh then doRest dTime else doNote dTime (pitch x)

> bassPart :: AbsPitch → Music Pitch
> bassPart x = if stdvol < thresh then doRest dTime else doNote dTime (pitch x)

> doRest :: Dur → Music a
> doRest d =
>    trace ("doRest d=" ++ show d)
>    Prim (Rest d)

> doNote :: Dur → Pitch → Music Pitch
> doNote d p =
>    trace ("doNote d=" ++ show d ++ " p=" ++ show p)
>    Prim (Note d p)

> stdvol = 800
> thresh = 20
> dTime = wn

> t251 :: Music Pitch
> t251 =  let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
>             gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
>             cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
>         in dMinor :+: gMajor :+: cMajor

> vt251 :: Music (Pitch, Volume)
> vt251 = addVolume 100 t251

> tPitch :: Pitch → Pitch 
> tPitch p = pitch (absPitch p - 4)

> tN :: PitchClass → Octave → Dur → Music Pitch
> tN pc o d = doNote d (tPitch (pc, o))
> uN :: PitchClass → Octave → Dur → Music Pitch
> uN pc o d = doNote (d*2) (tPitch (pc, o))

> what100 :: Dur → Music Pitch
> what100 dBase =            
>         let twoMinor = tN D 4 dBase :=: tN F 4 dBase :=: tN A 4 dBase
>             fiveMajor = tN G 4 dBase :=: tN B 4 dBase :=: tN D 5 dBase
>             oneMajor = uN C 4 dBase :=: uN E 4 dBase :=: uN G 4 dBase
>         in twoMinor :+: fiveMajor :+: oneMajor

comment ----------------------------------------------

> data BluesPitchClass = Ro | Mt | Fo | Fi | MS
> type BluesPitch = (BluesPitchClass, Octave)

> bluesPcToPc :: BluesPitchClass → PitchClass
> bluesPcToPc bluesPc = case bluesPc of
>    Ro → C; Mt → Ef; Fo → F; Fi → G; MS → Bf

> bluesPitchToPitch :: BluesPitch → Pitch
> bluesPitchToPitch (bp, o) = (bluesPcToPc bp, o)

> ro, mt, fo, fi, mS :: Octave → Dur → Music BluesPitch
> ro o d = note d (Ro, o)
> mt o d = note d (Mt, o)
> fo o d = note d (Fo, o)
> fi o d = note d (Fi, o)
> mS o d = note d (MS, o)

> mel :: Music Pitch
> mel = fromBlues ( ro 4 qn :+: mt 3 qn :+: fo 4 hn :+: fi 4 hn :+: mS 4 hn )

> fromBlues :: Music BluesPitch → Music Pitch
> fromBlues (Prim (Note d bp)) = Prim (Note d (bluesPitchToPitch bp))
> fromBlues (Prim (Rest d)) = Prim (Rest d)
> fromBlues (m1 :+: m2) = fromBlues m1 :+: fromBlues m2
> fromBlues (m1 :=: m2) = fromBlues m1 :=: fromBlues m2
> fromBlues (Modify c m) = Modify c (fromBlues m)

> f1 :: Int → [Pitch] → [Pitch]
> f1 _ [] = []
> f1 n ps = map (trans n) ps

> f2 :: [Dur] → [Music a]
> f2 = map u
>    where u :: Dur → Music a
>          u d = Prim(Rest d)

comment f2 (d:ds) = (Prim(Rest d)) : (f2 ds)

> f3 :: [Music Pitch] → [Music Pitch]
> f3 = map u
>    where u :: Music Pitch → Music Pitch
>          u (Prim(Note d p)) = Prim(Note (d/2) p) :+: Prim(Rest (d/2))
>          u _ = error "f3 takes a list of notes" 

> dog::[a]→[a]
> dog xs =
>    let rev acc [] = acc
>        rev acc (x:xs) = rev (x:acc) xs
>    in rev [] xs

> cat::[a]→[a]
> cat xs = let rev mop acc [] = acc
>              rev mop acc (x:xs) = rev mop (acc `mop` x) xs
>              in rev revMop [] xs

> revMop a b = b:a
> mouse xs = foldl revMop [] xs

> house xs = foldl tMop [] xs
>            where tMop::[r]→r→[r]
>                  tMop a b = b:a

> louse::[a]→[a]
> louse = foldl (flip (:)) []

> hat :: Int → Int
> hat = ( \x → (x + 1) )

> blip = flip (flip revMop)

> toPitches :: [AbsPitch] → [Pitch]
> toPitches xs = map pitch xs

> quarters :: [AbsPitch] → Music Pitch
> quarters xs = foldr (:+:) (rest 0) (map (note wn) (toPitches xs))

> j0, j1 :: InstrumentName
> j0 = Cello
> j1 = Banjo
> baggy = instrument j0 (quarters [32..42])
> taggy = instrument j1 (rest qn :+: quarters [42..52])

> mello = baggy :=: taggy

> doAllDevices :: Int → IO ()
> doAllDevices a =
>    do
>       allDevices ← getAllDevices
>       let numInput = length (fst allDevices)
>           numOutput = length (snd allDevices)
>       traceM ("numInput = " ++ show numInput ++ ", numOutput = " ++ show numOutput)

> dumpDevices = do
>   (devsIn, devsOut) ← getAllDevices
>   let f (devid, devname) = "  "++show devid ++ "\t" ++ name devname ++ "\n"
>       strIn = concatMap f devsIn
>       strOut = concatMap f devsOut
>   putStrLn "\nInput devices: " >> putStrLn strIn
>   putStrLn "Output devices: " >> putStrLn strOut
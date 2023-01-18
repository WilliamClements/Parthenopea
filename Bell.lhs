Bell Instrument
Donya Quick
Last modified: 22-July-2016

> {-# LANGUAGE Arrows #-}
> module Bell where
>
> import Data.Functor.Constant
> import Euterpea
> import FRP.UISF
> import Parthenopea

From the book:

> bell1 :: Instr (Mono AudRate)
> bell1 dur ap vol (x:xs) = error "list must be empty"
> bell1 dur ap vol [] =
>   let freq = apToHz ap
>       volR = fromIntegral vol / 100
>       durR = fromRational dur
>       sfs = map
>             (\p -> constA (freq*p) >>> osc tab1 0)
>             [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
>   in proc () -> do
>       aenv <- envExponSeg [0, 1, 0.0001] [0.003, durR - 0.003] -< ()
>       a1 <- foldSF (+) 0 sfs -< ()
>       outA -< a1 * aenv * volR / 9
>
> tab1 = tableSinesN 4096 [1]
> belltest1 = outFile "bell1.wav" 6 (bell1 6 (absPitch (C, 5)) 100 [])

> bell2 :: Instr (Mono AudRate)
> bell2 dur ap vol (x:xs) = error "list must be empty"
> bell2 dur ap vol [] =
>   let freq = apToHz ap
>       volR = fromIntegral vol / 100
>       durR = fromRational dur
>       sfs = map
>             (mySF freq durR)
>             [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
>   in proc () -> do
>     a1 <- foldSF (+) 0 sfs -< ()
>     dlsig <- envExpon 0.0005 5 0.002 -< ()
>     z <- delayLine1 (1/440) -< (a1, dlsig)
>     outA -< (0.5*a1 + 0.5*z) * volR / 9
>
> mySF freq durR p = proc () -> do
>    s <- osc tab1 0 <<<  constA (freq*p) -< ()
>    aenv <- envExponSeg [0, 1, 0.0001] [0.003, durR/p - 0.003] -< ()
>    outA -< s * aenv
>
> belltest2 = outFile "bell2.wav" 6 (bell2 6 (absPitch (C, 5)) 100 [])

The following instrument is intended to emulate a bell sound.

> sineTable :: Table
> sineTable = tableSinesN 4096 [1]

> bellInstr :: Instr (AudSF () Double)
> bellInstr dur ap vol pfields = 
>   let dur' = fromRational dur 
>       f = apToHz ap
>   in  proc () -> do
>         x1 <- osc sineTable 0 -< f
>         x2 <- osc sineTable 0 -< f*4.1
>         x3 <- osc sineTable 0 -< f*6.05
>         x4 <- osc sineTable 0 -< f*8.2
>         env1 <- envLineSeg [1.0, 0.2, 0, 0] [1, 2, 100] -< ()
>         env2 <- envLineSeg [0, 0.8, 0, 0] [0.05, 2, 100] -< ()
>         env3 <- envLineSeg [0, 0.5, 0, 0] [0.08, 2, 100] -< ()
>         env4 <- envLineSeg [0, 0.3, 0, 0] [0.015, 1, 100] -< ()
>         envx1 <- envLineSeg [0,1,1,0] [0.0001*dur',0.9999*dur', 0.0001*dur'] -< ()
>         envx2 <- envLineSeg [1,0.5,0.2,0,0] [0.05,0.2,3,100] -< ()
>         let envs = envx2
>             partials = ((x1*env1) + (x2*env2) + (x3*env3) + (x4*env4)) / 4
>         outA -< 0.95 * envs * partials
> belltestInstr = outFile "bellInstr.wav" 6 (bellInstr 6 (absPitch (C, 5)) 100 [])

Constructing the InstrMap:

> bellName :: InstrumentName
> bellName = CustomInstrument "Bell Instrument"

> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(bellName, bellInstr)]

Single note demonstration:

> bellNote = writeWav "bellNote.wav" myInstrMap 
>     (tempo 0.5 $ transpose 12 $ instrument bellName (g 5 wn))

Scale with bells truncated by note duration:


> mel1 = toMusic1 $ line $ map ($ en) [c 5, d 5, e 5, f 5, g 5, a 5, b 5, c 6]

> bellMel = writeWav "bellMel.wav" myInstrMap 
>    (tempo 0.5 $ transpose (-12) $ instrument bellName mel1) 

Allowed to resonate:

> bellMel2 = writeWav "bellMel2.wav" myInstrMap 
>     (parMod wn $ tempo 0.5 $ transpose 12 $ instrument bellName mel1)

Music modifier to parallelize notes and stretch their lengths by d amount

> parMod d (Prim (Rest d')) = rest d
> parMod d (Prim (Note d' x)) = note (d+d') x
> parMod d (m1 :+: m2) = (parMod d m1) :=: ((rest $ dur m1) :+: parMod d m2)
> parMod d (m1 :=: m2) = parMod d m1 :=: parMod d m2
> parMod d (Modify c m) = Modify c $ parMod d m
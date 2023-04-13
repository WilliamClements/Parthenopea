> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

> module Signals where

> import qualified Codec.Wav            as W
> import qualified Codec.SoundFont      as F
> import Control.Arrow ( returnA, (<<<), (>>>), Arrow(arr) )
> import Control.Arrow.ArrowP ( ArrowP(ArrowP) )
> import Control.DeepSeq (NFData)
> import Control.SF.SF ( SF(SF) )
> import Data.Array.Unboxed
> import qualified Data.Audio           as A
> import Data.Int ( Int8, Int16, Int32 )
> import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe, mapMaybe)
> import Data.Typeable ( typeOf )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types ( Clock(..), AudioSample, AudSF, SigFun, numChans)
> import Euterpea.IO.MIDI.MidiIO (MidiMessage(..), Message(..))
> import Euterpea.Music ( Music, Pitch, PitchClass(F, C, D, E), NoteAttribute, d, dur )
> import FRP.UISF
> import FRP.UISF.Asynchrony ( Automaton(..) )
> import FRP.UISF.AuxFunctions (DeltaT)
> import FRP.UISF.Graphics
> import FRP.UISF.UITypes (LayoutType, nullLayout, nullTP)
> import HSoM.MUI ( defaultMUIParams, midiIn, midiOut, runMUI, runMUI', selectOutput)
> import HSoM.Examples.FFT ( fftA )
> import HSoM.Examples.MUIExamples1 (getDeviceIDs)
> import HSoM.Examples.MUIExamples2 (mergeS, removeNull)
> import Numeric ( showHex )
> import Parthenopea
> import System.Environment ( getArgs )
> import System.IO ( hSeek, withBinaryFile, SeekMode(AbsoluteSeek), IOMode(ReadMode) )
> import Text.Printf ( PrintfArg(formatArg) )
   
> -- WOX oldmain = runGraph True
>
> clockedSFToUISF :: forall a b c . (NFData b, Clock c) ⇒ DeltaT → SigFun c a b → UISF a [(b, Time)]
> clockedSFToUISF buffer ~(ArrowP sf) = let r = rate (undefined :: c) 
>   in asyncVT r buffer (toAutomaton sf)
> toAutomaton :: forall a b . SF a b → Automaton (→) a b
> toAutomaton ~(SF f) = Automaton $ \a → let (b, sf) = f a in (b, toAutomaton sf)
>
> fftZ :: AudSF Double (SEvent [Double])
> fftZ = fftA 100 1024
>
> bling :: AudSF () (SEvent [Double])
> bling = proc () → do
>         sig ← fftZ ⤙ 0.0
>         outA ⤙ Just $ fromMaybe [] sig
>
> pamper :: AudSF () Double
> pamper = bling >>> flatten
>
> tbling :: Double → IO ()
> tbling secs = outFileNorm "pamper.wav" secs pamper
> 
> -- WOX comber = constA 440 >>> fftZ >>> flatten
> comber :: AudSF () Double
> comber = s441' >>> fftZ >>> flatten
>
> tcomber :: Double → IO ()
> tcomber secs = outFileNorm "comber.wav" secs comber
>
> flatten :: AudSF (SEvent [Double]) Double
> flatten = proc jxs  → do
>           let list = fromMaybe [] jxs
>               warg = if null list
>                      then 0.0
>                      else head list
>           outA ⤙ warg
>
> averageValues :: [Double] → Double
> averageValues ys = sum $ map (*fact) ys
>   where
>     fact :: Double
>     fact = (1.0::Double) / fromIntegral (length ys)
>
> averageSignals :: [AudSF () Double] → AudSF () Double
> averageSignals = foldSF (+) 0
>
> waveLen440 :: Double
> waveLen440 = 1.0/440.0 -- = 0.00227272
>
> {-
> secondCounter :: SF () Integer
> secondCounter = proc () → do
>    rec count ← fcdelay 0 1.0 ⤙ count + 1
>    outA ⤙ count
> -}
>
> sineTable441 :: Table
> sineTable441 = tableSinesN 100 [1]

> s441 :: AudSF () Double
> s441 = proc () → do
>          rec s ← delayLineT 100 sineTable441 ⤙ s
>          outA ⤙ s

> ts441 = outFile "s441.wav" 5 s441

> s441' :: AudSF () Double
> s441' = proc () → do
>          rec s ← delayLineT 100 sineTable441 ⤙ s
>          outA ⤙ s

> good = outFile "good.wav" 10 
>        (osc sineTable 0 <<< envExpon 20 10 10000 :: AudSF () Double)

> bad  = outFile "bad.wav" 10 
>        (osc sineTable 0 <<< envLine  20 10 10000 :: AudSF () Double)

> echo :: AudSF Double Double
> echo = proc s → do
>          rec fb  ← delayLine 0.5 ⤙ s + 0.7*fb
>          outA ⤙ fb/3

> modVib :: Double → Double → AudSF Double Double
> modVib rate depth =
>   proc sin → do
>     vib   ← osc sineTable 0  ⤙ rate
>     sout  ← delayLine1 0.2   ⤙ (sin,0.1+0.005*vib)
>     outA ⤙ sout

> tModVib :: IO ()
> tModVib = outFile "modvib.wav" 6 $
>                   constA 220 >>> osc sineTable 0 >>> modVib 9 0.001

> sineTable :: Table
> sineTable = tableSinesN 4096 [1]

> flute ::  Time → Double → Double → Double → Double 
>           → AudSF () Double
> flute dur amp fqc press breath = 
>   proc () → do
>     env1   ← envLineSeg  [0, 1.1*press, press, press, 0] 
>                           [0.06, 0.2, dur-0.16, 0.02]  ⤙ ()
>     env2   ← envLineSeg  [0, 1, 1, 0] 
>                           [0.01, dur-0.02, 0.01]       ⤙ ()
>     envib  ← envLineSeg  [0, 0, 1, 1] 
>                           [0.5, 0.5, dur-1]            ⤙ ()
>     flow   ← noiseWhite 42    ⤙ ()
>     vib    ← osc sineTable 0  ⤙ 5
>     let  emb = breath*flow*env1 + env1 + vib*0.1*envib
>     rec  flute  ← delayLine (1/fqc)    ⤙ out
>          x      ← delayLine (1/fqc/2)  ⤙ emb + flute*0.4
>          out    ← filterLowPassBW ⤙ (x-x*x*x + flute*0.4, 2000)
>     outA ⤙ out*amp*env2
>
> tFlute :: IO ()
> tFlute =      outFile "tFlute.wav"  5 $ flute 5 0.3 440 0.99 0.2 
> tFlute2 :: IO ()
> tFlute2 = outFileNorm "tFlute2.wav" 5 $ flute 5 0.7 440 0.99 0.2 

> colorSwatchUI :: UISF () ()
> colorSwatchUI = setSize (300, 220) $ pad (4,0,4,0) $ leftRight $ 
>     proc _ → do
>         r ← newColorSlider "R" ⤙ ()
>         g ← newColorSlider "G" ⤙ ()
>         b ← newColorSlider "B" ⤙ ()
>         e ← unique ⤙ (r,g,b)
>         let rect = withColor' (rgbE r g b) (rectangleFilled ((0,0),d))
>         pad (4,8,0,0) $ canvas d ⤙ fmap (const rect) e
>   where
>     d = (170,170)
>     newColorSlider l = title l $ withDisplay $ viSlider 16 (0,255) 0
>
> xlay, ylay :: LayoutType
> xlay = Stretchy 1000
> ylay = Stretchy 800
> lay :: Layout
> lay = makeLayout xlay ylay
>
> histWidget :: Int
>          → Maybe Double
>          → Either (Array Int (Music (Pitch, [NoteAttribute]))) (Array Int Int)
>          → UISF () ()
> histWidget nBins md mori =
>   setSize (1100, 900) $ pad (4,0,4,0) $ leftRight $ 
>   proc _ → do
>     _ ← title "Histogram" (histogram lay) ⤙ Just (normalize bins)
>     returnA ⤙ ()
>   where
>     bins :: Array Int Int
>     bins = case mori of
>        Left mu → scoreMusic nBins mu
>        Right ht → ht
>
> normalize :: Array Int Int → [Double]
> normalize bins =
>   map cv $ elems bins
>     where
>       cv :: Int → Double
>       cv x =  fromIntegral x / fromIntegral (maximum bins)

runGraph ==========================================================================

> runGraph :: Bool → IO ()
> runGraph doNoise = runMUI defaultUIParams{
>                             uiCloseOnEsc=True
>                             , uiTitle="Plot Signals"
>                             , uiSize=(1200,500)} $ myGraph doNoise
>
> straightNoise, flangedNoise, straightA440, flangedA440 :: AudSF () Double
>
> straightNoise = supplySound True  False
> flangedNoise =  supplySound True  True
> straightA440 =  supplySound False False
> flangedA440 =   supplySound False True
>
> tstraightA440, tflangedA440, tstraightNoise, tflangedNoise :: Time → IO ()
>
> tstraightA440  secs = outFileNorm "straightA440.wav"   secs straightA440
> tflangedA440   secs = outFileNorm "flangedA440.wav"    secs flangedA440
> tstraightNoise secs = outFileNorm "straightNoise.wav"  secs straightNoise
> tflangedNoise  secs = outFileNorm "flangedNoise.wav"   secs flangedNoise
>
> supplyPseudoSound :: Bool → Bool → AudSF () Double
> supplyPseudoSound doNoise doFlange = proc () → do
>   bSig ← if doNoise then noiseBLI nSeed ⤙ nCPS
>                     else osc tab2 0 ⤙ 0.440
>
>   dlsig ← envExpon 0.0005 3 0.0050 ⤙ ()
>   z ← delayLine1 0.0050 ⤙ (bSig, dlsig)
>
>   let outsig = if doFlange
>                then 0.5*bSig + 0.5*z
>                else bSig
>   if not (checkSig outsig)
>      then outA ⤙ 0
>      else outA ⤙ outsig
>
>   where
>     nCPS = 200
>     nSeed = 555
>     tab2 = tableSinesN 4096 [1.0, 0.5, 0.33]
>     checkSig :: Double → Bool
>     checkSig sig
>       | traceIf msg False = undefined
>       | otherwise = True
>         where
>           msg = unwords ["psuedo sound = ", show sig]
>
> flangeSFMono :: DeltaT → AudSF () Double → AudSF () Double
> flangeSFMono secs sig = proc () → do
>   bSig ← sig ⤙ ()
>   dlsig ← envExpon 0.0005 secs 0.0050 ⤙ ()
>   z ← delayLine1 0.0050 ⤙ (bSig, dlsig)
>   outA ⤙ 0.5*bSig + 0.5*z
>
> flangeSFStereo :: DeltaT → AudSF () (Double, Double) → AudSF () (Double, Double)
> flangeSFStereo secs sig = proc () → do
>   bSig ← sig ⤙ ()
>   dlsig ← envExpon 0.0005 secs 0.0050 ⤙ ()
>   z1 ← delayLine1 0.0050 ⤙ (fst bSig, dlsig)
>   z2 ← delayLine1 0.0050 ⤙ (snd bSig, dlsig)
>   outA ⤙ (0.5*fst bSig + 0.5*z1 , 0.5*snd bSig + 0.5*z2)
>
> supplySound :: Bool → Bool → AudSF () Double
> supplySound doNoise doFlange = proc () → do
>   bSig ← if doNoise then noiseBLI nSeed ⤙ nCPS
>                     else osc tab2 0 ⤙ 440
>
>   dlsig ← envExpon 0.0005 3 0.0050 ⤙ ()
>   z ← delayLine1 0.0050 ⤙ (bSig, dlsig)
>
>   outA ⤙ if doFlange then 0.5*bSig + 0.5*z
>                      else bSig
>   where
>     nCPS = 200
>     nSeed = 555
>     tab2 = tableSinesN 4096 [1.0, 0.5, 0.33]
>
> doSomething :: [(Double, Time)] → [(Double, Time)]
> doSomething vs
>   | traceIf msg False = undefined
>   | otherwise = case vs of
>       [] → []
>       vv:vvs →  vv:vvs
>   where
>     msg = unwords ["vs=", show vs]
>
> filterFeed :: UISF [(Double, Time)] [(Double, Time)]
> filterFeed = do
>   -- let r = rate (undefined :: c) 
>   proc vs → do
>     outA ⤙ doSomething vs
>   
> myEnvGraph :: Double → UISF () ()
> myEnvGraph dur = 
>   let
>     sf :: AudSF () Double
>              -- start dur end
>     sf = envExpon 0.1 dur 0.9
>
>   in proc () → do
>     ex' ← clockedSFToUISF dur sf ⤙ ()
>     ex ← filterFeed ⤙ ex'
>     title "(Values, Time)" display ⤙ ex
>     _ ← title "Qit" (realtimeGraph lay dur Magenta) ⤙ ex
>     outA ⤙ ()
>   
> myGraph :: Bool → UISF () ()
> myGraph doNoise =
>   let qit doFlange =
>         case (doNoise, doFlange) of
>               (False, False) →  "Straight A440"
>               (False, True)  →  "Flanged A440"
>               (True, False)  →  "Straight Noise"
>               (True, True)   →  "Flanged Noise"
>   in proc () → do
>
>   e0 ← clockedSFToUISF 10.0 $ supplyPseudoSound doNoise False ⤙ ()
>   e2 ← clockedSFToUISF 10.0 $ supplyPseudoSound doNoise True  ⤙ ()
> 
>   _ ← title (qit False)  (realtimeGraph lay 0.5 Magenta) ⤙ e0
>   _ ← title (qit True)   (realtimeGraph lay 0.5 Cyan)    ⤙ e2
> 
>   outA ⤙ ()
>
> listboxMUI1 :: IO ()
> listboxMUI1 =  runMUI defaultMUIParams{uiSize=(200,100)} $ proc _ → do
>     rec x ← listbox [C, D, E, F] 0 ⤙ (Nothing, Nothing)
>         let x' = if x<0 then 0 else x
>     display ⤙ x
>
> convolve :: (Ix a, Integral a, Num b) ⇒ Array a b → Array a b → Array a b
> convolve x1 x2 = x3
>    where m1 = snd $ bounds x1
>          m2 = snd $ bounds x2
>          m3 = m1 + m2
>          x3 = listArray (0,m3)
>                 [
>                    sum [ x1 ! k * x2 ! (n-k) | k ← [max 0 (n-m2)..min n m1] ]
>                        | n ← [0..m3]
>                 ]
>
> linspace :: Double → Double → Int → [Double]
> linspace a b n =
>   map (\ i → a + fromIntegral i * inc) [(0::Int) .. (n - 1)]
>  where
>   inc = (b - a) / fromIntegral (n - 1)
>
> -- linspace 0.0 1.0 5 == [ 0.0, 0.25, 0.5, 0.75 1.0 ]
>

fft =======================================================================================

> checkItem ldt lfft
>   | traceAlways msg False = undefined
>   | otherwise = True
>   where msg = unwords [ "\nfftEx ldt =" ++ show (typeOf ldt) ++ " lfft =" ++ show (typeOf lfft)]
>
> nFFTBins :: Int
> nFFTBins = defBins
>
> fftEx :: UISF () ()
> fftEx = proc _ → do 
>   fKnob ← title "Frequency" (hSlider (1,2000) 440) ⤙ ()
>   e0 ← clockedSFToUISF 400 sig ⤙ fKnob
>   let (ts, ffts) = unzip e0
>       histTable :: Array Int Int
>       histTable = scoreOnsets nFFTBins ts
>   
>   let ok = checkItem ts ffts
>       ok2 = ok || error "bad fft"
>   display ⤙ ok2
>
>   _ ← title "FFT"  (histogram     lay)   ⤙ Just $ normalize histTable
>   _ ← title "Sine" (realtimeGraph lay 0.02 Black) ⤙ e0
>   
>   outA ⤙ ()
>   where
>     sig :: AudSF Double Double
>     sig = proc fVal → do
>       sVal ← osc (tableSinesN 4096 [1]) 0 ⤙ fVal
>       -- WOX fftData ← fftA 100 256 ⤙ sVal
>       outA ⤙ sVal
>
> tfft :: IO ()
> tfft = runMUI defaultUIParams{
>                              uiTitle="FFT"
>                              , uiSize=(1200,500)} fftEx
>
> exampleEnv :: [(Double, Double)]
> exampleEnv = [(0.9, 0.001), (0.7, 0.0015), (0.9, 0.0019)]
>
> displayEnvelope :: AudSF () Double → UISF () ()
> displayEnvelope env = proc () → do
>   _ ← title "Envelope" (realtimeGraph lay 0.02 Black) ⤙ exampleEnv
>   outA ⤙ ()
>
> timeEx :: UISF () ()
> timeEx = title "Time" $ accumTime >>> display <<< spacer
>
> -- | This example shows off 'button's and state by presenting a plus and 
> -- minus button with a counter that is adjusted by them.
> buttonEx :: UISF () ()
> buttonEx = title "Buttons" $ topDown $ proc _ → do
>  (x,y) ← leftRight (proc _ → do
>    x ← edge <<< button "+" ⤙ ()
>    y ← edge <<< button "-" ⤙ ()
>    returnA ⤙ (x, y)) ⤙ ()
>  rec v ← delay 0 ⤙ (case (x,y) of
>            (Just _, Nothing) → v+1
>            (Nothing, Just _) → v-1
>            _ → v)
>  display ⤙ v
>  spacer ⤙ ()
>
> -- | This example shows off the 'checkbox' widgets.
> checkboxEx :: UISF () ()
> checkboxEx = title "Checkboxes" $ topDown $ proc _ → do
>   x ← checkbox "Monday" False ⤙ ()
>   y ← checkbox "Tuesday" True ⤙ ()
>   z ← checkbox "Wednesday" True ⤙ ()
>   let v = bin x ++ bin y ++ bin z
>   displayStr ⤙ v
>   spacer ⤙ ()
>   where
>     bin True = "1"
>     bin False = "0"
>
> -- | This example shows off the 'radio' button widget.
> radioButtonEx :: UISF () ()
> radioButtonEx = title "Radio Buttons" $ topDown $ radio list 0 >>> arr (list!!)
>                                                                >>> displayStr
>                                                                >>> spacer
>   where
>     list = ["apple", "orange", "banana"]
>
> -- | This example shows off integral sliders (horizontal 'hiSlider's in 
> --   this case).
> shoppinglist :: UISF () ()
> shoppinglist = title "Shopping List" $ topDown $ proc _ → do
>   a ← spacer <<< title "apples"  (hiSlider 1 (0,10) 3) ⤙ ()
>   b ← spacer <<< title "bananas" (hiSlider 1 (0,10) 7) ⤙ () 
>   title "total" display ⤙ (a + b)
>
> -- | This example shows off both vertical sliders as well as the 'canvas' 
> -- widget.  The canvas widget can be used to easily create custom graphics 
> -- in the GUI.  Here, it is used to make a color swatch that is 
> -- controllable with RGB values by the sliders.
> colorDemo :: UISF () ()
> colorDemo = title "Color" $ leftRight $ proc _ → do
>   r ← newColorSlider (coloredUIText Red "R") ⤙ ()
>   g ← newColorSlider (coloredUIText Green "G") ⤙ ()
>   b ← newColorSlider (coloredUIText Blue "B") ⤙ ()
>   changed ← unique ⤙ (r,g,b)
>   pad (4,8,0,0) $ canvas' layout rect ⤙ changed
>   where
>     layout = makeLayout (Stretchy 10) (Stretchy 10)
>     newColorSlider l = title l $ topDown $ proc _ → do
>       v ← viSlider 16 (0,255) 0 ⤙ ()
>       _ ← setSize (22,22) displayStr ⤙ showHex v ""
>       returnA ⤙ v
>     rect (r,g,b) d = withColor' (rgbE r g b) (rectangleFilled ((0,0),d))
>
> -- | This example shows off the 'textbox' widget.  Text can be typed in, and 
> -- that text is transferred to the 'display' widget below when the button 
> -- is pressed.
> textboxdemo :: UISF () ()
> textboxdemo = title "Saving Text" $ topDown $ proc _ → do
>   str ← leftRight $ label "Text: " >>> textbox "" ⤙ Nothing
>   b ← button "Save text to below" ⤙ ()
>   rec str' ← delay "" ⤙ if b then str else str'
>   leftRight $ label "Saved value: " >>> displayStr ⤙ str' 
>
> uitext :: UIText
> uitext =
>     coloredUIText Red       "H" `appendUIText`
>     coloredUIText Yellow    "e" `appendUIText`
>     coloredUIText Green     "l" `appendUIText`
>     coloredUIText Cyan      "l" `appendUIText`
>     coloredUIText Blue      "o" `appendUIText`
>     coloredUIText Magenta  " W" `appendUIText`
>     coloredUIText Red       "o" `appendUIText`
>     coloredUIText Yellow    "r" `appendUIText`
>     coloredUIText Green     "l" `appendUIText`
>     coloredUIText Cyan      "d" `appendUIText`
>     coloredUIText Blue      "!"
> uitext' = fontUIText Helvetica18 uitext
>
> uitextdemo = title "Color and Fonts" $ constA Nothing >>> textField CharWrap uitext'
>                                                       >>> constA ()

importing sampled sound (from SoundFont or Wave file) =====================================

> data SampleSpec = 
>   SampleSpec {   ssCount  :: Double
>                , ssRate   :: Double
>                , ssChans  :: Double} deriving Show
>   
> getSFForPhase          ::  forall a p. (ArrowCircuit a, Clock p) ⇒ 
>                              SampleSpec
>                              → Double
>                              → ArrowP a p () Double
> getSFForPhase spec iphs =
>   let frac    :: RealFrac r ⇒ r → r
>       frac = snd . properFraction
>       secs  :: Double            = ssCount spec / ssRate spec
>       delta   :: Double
>       delta = ssChans spec / (secs * ssRate spec)
>   in proc () → do
>     rec
>       let phase = if next > 1 then frac next else next
>       next ← delay iphs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> getSFForOutput         :: forall a p u. (ArrowCircuit a, Clock p, WaveAudioSample u) ⇒
>                            ArrowP a p Double u 
>                            → SampleSpec
>                            → (Int, Int)
>                            → (DeltaT, ArrowP a p () u)
> getSFForOutput sig0 spec range
>   | traceAlways msg False = undefined
>   | otherwise = (secs, sig)
>   where
>     secs  :: Double            = ssCount spec / ssRate spec
>     sig   :: ArrowP a p () u   = getSFForPhase spec 0 >>> sig0
>     msg = unwords [ "SampleSpec=", show spec, " (st,en)=", show range, " secs = ", show (ssCount spec / ssRate spec)]

import from SoundFont file ================================================================

> doSoundFont :: FilePath → Effect → IO ()
> doSoundFont inFile effect =
>   do
>     putStrLn "entering doSoundFont"
>     putStrLn ("inFile=" ++ inFile)
>     maybeAudio ← F.importFile inFile
>     case maybeAudio of
>       Left s → putStrLn $ "SoundFont decoding error: " ++ s
>       Right soundFont → do
>         let sdata = F.smpl (F.sdta soundFont)
>         let m24 = F.sm24 (F.sdta soundFont)
>         case m24 of
>           Nothing → print "16-bit"
>           Just s24data → do
>             print "24-bit"
>         let shdrs = F.shdrs (F.pdta soundFont)
>         let bs =bounds shdrs
>         let upper = min 10 (snd bs)
>         mapM_ (shdrDo sdata m24 shdrs) [0..upper]
>     putStrLn "leaving doSoundFont"
>
> shdrDo :: A.SampleData Int16 → Maybe (A.SampleData Int8) → Array Word F.Shdr → Word → IO ()
> shdrDo sdata m24 shdrs n = do
>   let shdr = shdrs ! n
>   let filename = F.sampleName shdr ++ ".wav"
>   let (st, en) :: (Int, Int)                    = (fromIntegral (F.start shdr), fromIntegral (F.end shdr))
>   let sr       :: Double                        = fromIntegral $ F.sampleRate shdr  
>   let ns       :: Double                        = fromIntegral (en - st + 1)
>   let secs     :: DeltaT                        = ns / sr
>   let sig0                                      = getSoundFontSamplingSF sdata m24 (st, en)
>   let spec                                      = SampleSpec ns sr 1
>   let (secs' :: DeltaT, sig :: AudSF () Double) = getSFForOutput sig0 spec (st, en)
>   print ("spec=" ++ show spec ++ "secs=" ++ show secs ++ "secs'=" ++ show secs')
>   outFileNorm filename secs' sig
>   print filename
>
> getSoundFontSamplingSF    :: forall a p. (ArrowCircuit a, Clock p) =>
>                            A.SampleData Int16 →
>                     Maybe (A.SampleData Int8) →
>                            (Int, Int) →
>                     ArrowP a p Double Double
> getSoundFontSamplingSF sdt m24 (st, en) = (arr . extractDataPoint) ()
>  where
>    extractDataPoint    :: forall u. (AudioSample u, Num u) ⇒ () → Double → u
>    extractDataPoint () pos = 
>      let
>        nc = numChans (undefined :: u)
>        numS :: Double
>        numS = fromIntegral $ (en - st + 1) `div` nc
>        idx :: Int 
>        idx = case m24 of
>              Nothing → st + truncate (numS * pos)
>              Just _  → error "24-bit not yet supported"
>      in fromIntegral $ sdt ! idx
> 

import from Wave file =====================================================================

> doWave :: FilePath → IO ()
> doWave inFile =
>   do
>     putStrLn "entering doWave"
>     putStrLn ("inFile=" ++ inFile)
>     maybeAudio ← W.importFile inFile
>     case maybeAudio of
>       Left s → putStrLn $ "wav decoding error: " ++ s
>       Right aud → do
>         let chans::Double = fromIntegral $ A.channelNumber aud
>             sdta = A.sampleData aud
>             (st, en) =  bounds sdta {- WOX (0, 26) -}
>             count::Double = fromIntegral (en - st + 1) / chans
>             rate::Double = fromIntegral $ A.sampleRate aud
>             secs::Double = count / rate
>             spec::SampleSpec = SampleSpec count rate chans
>         putStrLn $ "chans=" ++ show chans ++ " range=" ++ show (st, en) ++ " #=" ++ show count
>         putStrLn $ "rate=" ++ show rate ++ " secs=" ++ show secs
>         if 1 == A.channelNumber aud
>           then do
>             let sig0  :: AudSF Double Double                       = getWaveSamplingSF sdta (st, en)
>             let (secs :: DeltaT, sig :: AudSF () Double)           = getSFForOutput sig0 spec (st, en)
>             outFileNorm "outMono.wav" secs sig
>             putStrLn "Mono wave file produced"
>           else do
>             let sig0  :: AudSF Double (Double, Double)             = getWaveSamplingSF sdta (st, en)
>             let (secs :: DeltaT, sig :: AudSF () (Double, Double)) = getSFForOutput sig0 spec (st, en)
>             outFileNorm "outStereo.wav" secs sig
>             putStrLn "Stereo wave file produced"
>     putStrLn "leaving doWave"
>
> getWaveSamplingSF         :: forall a p u. (ArrowCircuit a, Clock p, WaveAudioSample u) =>
>                            A.SampleData Int32 → (Int, Int) → ArrowP a p Double u
> getWaveSamplingSF sdta (st, en) = (arr . extractDataPoint) ()
>   where
>     extractDataPoint   :: forall u. (WaveAudioSample u) ⇒ () → Double → u
>     extractDataPoint () pos = retrieve sdta idx
>       where
>         nc = numChans (undefined :: u) 
>         numS :: Double
>         numS = fromIntegral $ (en - st + 1) `div` nc
>         idx :: Int
>         idx = st + truncate (numS * pos)
>
> vmain :: IO ()
> vmain = do
>   putStrLn "This freezes the machine! Don't run it!!"
>   if False then
>     runUI (defaultUIParams {uiSize=(500, 520), uiCloseOnEsc=True}) $ 
>       leftRight $ bottomUp (timeEx >>> buttonEx) >>> checkboxEx >>> radioButtonEx >>>
>       leftRight (shoppinglist >>> colorDemo) >>> textboxdemo >>> uitextdemo
>            else
>     putStrLn "Later!!!"
>
> echoUI :: UISF () ()
> echoUI = proc _ → do
>   (mi, mo) ← getDeviceIDs ⤙ ()
>   m ← midiIn ⤙ mi
>   r ← title "Decay rate"        $ withDisplay (hSlider (0, 0.9) 0.5) ⤙ ()
>   ef ← title "Echoing frequency" $ withDisplay (hSlider (1, 10) 10) ⤙ ()
>   
>   rec let m' = removeNull $ mergeS m s
>       s <- vdelay -< (1/ef, fmap (mapMaybe (decay 0.1 r)) m')
>   
>   midiOut ⤙ (mo, m')
>
> echoMUI = runMUI' echoUI
>
> decay :: Time → Double → MidiMessage → Maybe MidiMessage
> decay dur r m =
>   let f c k v d = if v > 0
>                   then let v' = truncate (fromIntegral v*r)
>                        in Just (ANote c k v' d)
>                   else Nothing
>   in case m of 
>     ANote c k v d      → f c k v d
>     Std (NoteOn c k v) → f c k v dur
>     _                  → Nothing

=================
Bifurcate example

Here is an example with some ideas borrowed from Gary Lee Nelson's
composition "Bifurcate me, Baby!"

The basic idea is to evaluate the logistic growth function at
different points and convert the value to a musical note.  The growth
function is given by

  x_(n+1) = r x_n (1 - x_n)

We start with an initial population x_0 and iteratively apply the
growth function to it, where r is the growth rate.  For certain values
of r, the population stablizes to a certain value, but as r increases,
the period doubles, quadruples, and eventually leads to chaos.  It is
one of the classic examples in chaos theory.

First we define the growth function which, given a rate r and
current population x, generates the next population.

> grow :: Double -> Double -> Double
> grow r x = r * x * (1-x)

Then we define a signal 'tick' that pulsates at a given frequency
specified by slider f.  This is the signal that will drive the
simulation.  The timer function takes in a frequency.

The next thing we need is a time-varying population.  This is where 
the delay function and the rec keyword come in handy.  We initialize 
the 'pop' signal with the value 0.1, and then on every tick, we 
grow it with the instantaneous value of the growth rate signal.

We can now write a simple function that maps a population value to a
musical note:

> popToNote :: Double -> [MidiMessage]
> popToNote x = [ANote 0 n 64 0.05] where n = truncate (x * 127)

Finally, to play the note, we simply send the current population to 
popToNote, and send the result to the selected Midi output device.  

> bifurcate = runMUI (defaultMUIParams {uiSize=(300,500), uiTitle="Bifurcate!"}) $ proc _ -> do
>   mo <- selectOutput -< ()
>   f  <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
>   r  <- title "Growth rate" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
>   
>   tick <- timer -< 1.0 / f
>   rec pop <- delay 0.1 -<  maybe pop (const $ grow r pop) tick
>       
>   _ <- title "Population" $ display -< pop
>   midiOut -< (mo, fmap (const (popToNote pop)) tick)
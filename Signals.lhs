> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

> module Signals where

> import Codec.Wav
> import Control.Arrow
> import Control.Arrow.ArrowP
> import Control.DeepSeq (NFData)
> import Control.SF.SF
> import Data.Array.Unboxed
> import Data.Audio
> import Data.Binary ( Word8 )
> import qualified Data.ByteString      as   B
> import qualified Data.ByteString.Lazy as   BL
> import Data.Int ( Int16, Int32 )
> import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
> import Data.Typeable ( typeOf )
> import Data.Word ( Word8 )
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.BasicSigFuns
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Types
> import Euterpea.Music hiding (SF)
> import FRP.UISF
> import FRP.UISF.Asynchrony ( Automaton(..) )
> import FRP.UISF.AuxFunctions (DeltaT)
> import FRP.UISF.Graphics
> import FRP.UISF.UITypes (LayoutType, nullLayout, nullTP)
> import HSoM.MUI ( defaultMUIParams, runMUI )
> import HSoM.Examples.FFT ( fftA )
> import Numeric ( showHex )
> import Parthenopea( defBins, scoreOnsets, scoreMusic, traceAlways, traceIf )
> import System.Environment ( getArgs )
> import System.IO ( hSeek, withBinaryFile, SeekMode(AbsoluteSeek), IOMode(ReadMode) )
> import Text.Printf (PrintfArg(formatArg))
  
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
> {-
> bling :: SigFun SlwRate () [Double]
> bling = proc () → do
>           -- WOX bSig ← (fftZ <<< constA 440)
>           -- vib   ← osc sineTable 0  ⤙ rate
>           bSig' ⤙ Just [] -- WOX sigbreakup <<< bSig
>           outA ⤙ bSig'
>         where
>           breakup :: SEvent [Double] → Double
>           breakup xs = head $ fromMaybe [] xs
>           sigbreakup :: SigFun SlwRate [Double] Double
>           sigbreakup = arr breakup
> -}
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
> primes :: Int → [Int]
> primes n = [x | x ← [2..n], prime x]
>
> prime :: Int → Bool
> prime n = factors n == [1,n]
>
> factors :: Int → [Int]
> factors n = [x | x ← [1..n], n `mod` x == 0]
>
> seq1, seq2 :: Fractional a => [a]
> seq1 = [3.0, -2.0, 4.9, 3.2, -0.5]
> seq2 = [-3.0, 44.6, 7.1]
>
> array1 :: (IArray a e, Ix i, Fractional e, Num i) => a i e
> array1 = listArray (0, 4) seq1
> array2 :: (IArray a e, Ix i, Fractional e, Num i) => a i e
> array2 = listArray (0, 2) seq2
> array3 :: (Ix a, Integral a, Fractional b) => Array a b
> array3 = conv array1 array2
>
> conv :: (Ix a, Integral a, Num b) ⇒ Array a b → Array a b → Array a b
> conv x1 x2 = x3
>    where m1 = snd $ bounds x1
>          m2 = snd $ bounds x2
>          m3 = m1 + m2
>          x3 = listArray (0,m3)
>                 [
>                    sum [ x1!k * x2!(n-k) | k ← [max 0 (n-m2)..min n m1] ]
>                        | n ← [0..m3]
>                 ]
> h1, h2, h3 :: Array Int Integer
> h1 = listArray (0,3) [ 1, 2, 3, 4 ]
> h2 = listArray (0,4) [ 1, 2, 3, 4, 5 ]
> h3 = listArray (0,7) [ 1, 4, 10, 20, 30, 34, 31, 20 ]
>
> linspace :: Double → Double → Int → [Double]
> linspace a b n =
>   map (\ i → a + fromIntegral i * inc) [(0::Int) .. (n - 1)]
>  where
>   inc = (b - a) / fromIntegral (n - 1)
>
> -- linspace 0.0 1.0 5 == [ 0.0, 0.25, 0.5, 0.75 1.0 ]
>

fft ===============================================================================

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
> radioButtonEx = title "Radio Buttons" $ topDown $ radio list 0 >>> arr (list!!) >>> displayStr >>> spacer
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
> uitextdemo = title "Color and Fonts" $ constA Nothing >>> textField CharWrap uitext' >>> constA ()
>
> combineTwoWord8s :: Word8 → Word8 → Int16
> combineTwoWord8s x y = z
>   where
>     x', y', z :: Int16
>     x' = fromIntegral x
>     y' = fromIntegral y
>     z = x' + y'*256
>
> combineFourWord8s :: Word8 → Word8 → Word8 → Word8 → Int32
> combineFourWord8s x1 x2 x3 x4 = z
>   where
>     x1', x2', x3', x4', z :: Int32
>     x1' = fromIntegral x1
>     x2' = fromIntegral x2
>     x3' = fromIntegral x3
>     x4' = fromIntegral x4
>     z = x1' + x2'*256 + x3'*65536 + x4'*16777216
>
> combineWord8sToInt16s :: [Word8] → [Int16]
> combineWord8sToInt16s [] = []
> combineWord8sToInt16s [_] = []
> combineWord8sToInt16s (x:y:xs) = combineTwoWord8s x y : combineWord8sToInt16s xs
>
> combineWord8sToInt32s :: [Word8] → [Int32]
> combineWord8sToInt32s xs
>   | 4 > length xs = []
>   | otherwise     = combineFourWord8s (xs!!0) (xs!!1) (xs!!2) (xs!!4) : combineWord8sToInt32s (drop 4 xs)
>
> normalizeInt16 :: Int16 → Double
> normalizeInt16 n16 = fromIntegral n16 / 32768
>
> normalizeInt32 :: Int32 → Double
> normalizeInt32 n32 = fromIntegral n32 / 2147483648
>
> readFromSamples :: UArray Int Int32 → Double → Double
> readFromSamples samples pos =
>   let theBounds = bounds samples
>       sz = snd theBounds - fst theBounds + 1
>       idx = truncate (fromIntegral sz * pos)  -- range must be [0,size]
>   in normalizeInt32 $ samples ! idx
>
> readFromSamplesA :: Arrow a ⇒ UArray Int Int32 → a Double Double
> readFromSamplesA = arr . readFromSamples
>
> wavLookup ::  (Clock p, ArrowCircuit a)
>                ⇒ UArray Int Int32 → Double → DeltaT → Double → ArrowP a p () Double
> wavLookup samples sr secs iphs =
>   wavLookup_ sr secs iphs >>> readFromSamplesA samples
>
> wavLookup_ :: (Clock p, ArrowCircuit a)
>                ⇒ Double → DeltaT → Double → ArrowP a p () Double
> wavLookup_ sr secs phs =
>   let frac :: RealFrac r ⇒ r → r
>       frac = snd . properFraction
>   in  proc () → do
>     rec
>       let delta = 1 / (sr * secs)
>           phase = if next > 1 then frac next else next
>       next ← delay phs ⤙ frac (phase + delta)
>     outA ⤙ phase
>
> waveAudioToSF :: Audio Int32 → (DeltaT, AudSF () Double)
> waveAudioToSF (Audio rate channels samples) =
>   let theBounds = bounds samples
>       sz = snd theBounds - fst theBounds + 1
>       secs = fromIntegral sz / fromIntegral rate
>       sig :: AudSF () Double
>       sig = wavLookup samples (fromIntegral rate) secs 0
>   in (secs, sig)
>
> doWave :: FilePath → IO ()
> doWave inFile =
>   do
>     putStrLn "entering doWave"
>     putStrLn ("inFile=" ++ inFile)
>     maybeAudio <- importFile inFile
>     case maybeAudio :: Either String (Audio Int32) of
>       Left s → putStrLn $ "wav decoding error: " ++ s
>       Right aud → do
>         let (secs, sig) = waveAudioToSF aud
>         putStrLn "created signal"
>         outFile "outout.wav" secs sig
>         putStrLn "outout.wav produced"
>     putStrLn "leaving doWave"
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
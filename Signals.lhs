> {-#  LANGUAGE Arrows  #-}

> module Signals where

> import Aleatory
> import Codec.Midi hiding (Time)
> import Control.Concurrent
> import Control.SF.SF
> import Data.Array
> import qualified Data.ByteString as Byte (findIndex, writeFile, ByteString, concat, pack, length)
> import Data.Char
> import Data.List
> import Data.Maybe (listToMaybe, catMaybes)
> import Data.Typeable
> import Euterpea hiding (SF)
> import Euterpea.IO.Audio.BasicSigFuns
> import FFT
> import FRP.UISF
> import FRP.UISF.AuxFunctions (DeltaT)
> import FRP.UISF.Graphics
> import FRP.UISF.UITypes (LayoutType)
> import GHC.Base hiding (foldr)
> import MidiWidgets
> import MUI
> import Numeric
> import Parthenopea

Note! In this file:
  imports of HSOM.MUI , called MUI , are being grabbed from MUI.hs

> main = runGraph True

> bling :: AudSF () Double
> bling = proc x -> do
>           let cps = 500
>           bSig <- noiseBLI 555 -< cps
>           outA -< bSig
>
> averageValues :: [Double] -> Double
> averageValues ys = sum $ map (*fact) ys
>   where
>     fact :: Double
>     fact = (1.0::Double) / (fromIntegral $ length ys)
>
> averageSignals :: [AudSF () Double] -> AudSF () Double
> averageSignals ss =
>   -- arr $ foldr (+) 0 ss
>   foldSF (+) 0 ss
>
> waveLen440 :: Double
> waveLen440 = 1.0/440.0 -- = 0.00227272
>
> secondCounter :: AudSF () Integer
> secondCounter = proc () -> do
>    rec count <- delay 1 -< count + 1
>    outA -< count
> 
> tbling :: Double -> IO ()
> tbling secs = outFileNorm "bling.wav" secs bling
>
> sineTable441 :: Table
> sineTable441 = tableSinesN 100 [1]

> s441 :: AudSF () Double
> s441 = proc () -> do
>          rec s <- delayLineT 100 sineTable441 -< s
>          outA -< s

> ts441 = outFile "s441.wav" 5 s441

> good = outFile "good.wav" 10 
>        (osc sineTable 0 <<< envExpon 20 10 10000 :: AudSF () Double)

> bad  = outFile "bad.wav" 10 
>        (osc sineTable 0 <<< envLine  20 10 10000 :: AudSF () Double)

> echo :: AudSF Double Double
> echo = proc s -> do
>          rec fb  <- delayLine 0.5 -< s + 0.7*fb
>          outA -< fb/3

> modVib :: Double -> Double -> AudSF Double Double
> modVib rate depth =
>   proc sin -> do
>     vib   <- osc sineTable 0  -< rate
>     sout  <- delayLine1 0.2   -< (sin,0.1+0.005*vib)
>     outA -< sout

> tModVib = outFile "modvib.wav" 6 $
>                   constA 220 >>> osc sineTable 0 >>> modVib 9 0.001

> sineTable :: Table
> sineTable = tableSinesN 4096 [1]

> flute ::  Time -> Double -> Double -> Double -> Double 
>           -> AudSF () Double
> flute dur amp fqc press breath = 
>   proc () -> do
>     env1   <- envLineSeg  [0, 1.1*press, press, press, 0] 
>                           [0.06, 0.2, dur-0.16, 0.02]  -< ()
>     env2   <- envLineSeg  [0, 1, 1, 0] 
>                           [0.01, dur-0.02, 0.01]       -< ()
>     envib  <- envLineSeg  [0, 0, 1, 1] 
>                           [0.5, 0.5, dur-1]            -< ()
>     flow   <- noiseWhite 42    -< ()
>     vib    <- osc sineTable 0  -< 5
>     let  emb = breath*flow*env1 + env1 + vib*0.1*envib
>     rec  flute  <- delayLine (1/fqc)    -< out
>          x      <- delayLine (1/fqc/2)  -< emb + flute*0.4
>          out    <- filterLowPassBW -< (x-x*x*x + flute*0.4, 2000)
>     outA -< out*amp*env2
>
> tFlute = outFile "tFlute.wav" 5 $ flute 5 0.3 440 0.99 0.2 
> tFlute2 = outFileNorm "tFlute2.wav" 5 $ flute 5 0.7 440 0.99 0.2 

> ui1 ::  UISF () ()
> ui1 =   setSize (150,150) $ 
>   proc _ -> do
>     ap <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
>     title "Pitch" display -< pitch ap
>
> mui1  =  runMUI' ui1
>
> colorSwatchUI :: UISF () ()
> colorSwatchUI = setSize (300, 220) $ pad (4,0,4,0) $ leftRight $ 
>     proc _ -> do
>         r <- newColorSlider "R" -< ()
>         g <- newColorSlider "G" -< ()
>         b <- newColorSlider "B" -< ()
>         e <- unique -< (r,g,b)
>         let rect = withColor' (rgbE r g b) (rectangleFilled ((0,0),d))
>         pad (4,8,0,0) $ canvas d -< fmap (const rect) e
>   where
>     d = (170,170)
>     newColorSlider l = title l $ withDisplay $ viSlider 16 (0,255) 0

> xlay, ylay :: LayoutType
> xlay = Stretchy 1000
> ylay = Stretchy 800
> lay = makeLayout xlay ylay
>
> hist1 :: Int
>          -> (Maybe Double)
>          -> Either (Music (Pitch, Volume)) (Array Int Int)
>          -> UISF () ()
> hist1 nDivs md mori =
>   setSize (1100, 900) $ pad (4,0,4,0) $ leftRight $ 
>   proc _ -> do
>     h <- title "Histogram" (histogram lay) -< Just (toNormalizedList distArray)
>     returnA -< ()
>   where
>     distArray :: Array Int Int
>     distArray = case mori of
>        Left mu ->  makeDistArrayMusic nDivs mu
>        Right ht -> ht
>
>     lo, hi :: Double
>     lo = 0 -- TODO: fromIntegral $ minimum $ distArray
>     hi = fromIntegral $ maximum $ distArray
>
> toNormalizedList :: Array Int Int -> [Double]
> toNormalizedList distArray = map cv $ elems distArray
>    where
>       cv = (\x -> fromIntegral x / hilo)
>       hilo :: Double
>       hilo = fromIntegral $ maximum distArray - minimum distArray

runGraph ==========================================================================

> runGraph :: Bool -> IO ()
> runGraph doNoise = runMUI defaultUIParams{
>                             uiCloseOnEsc=True
>                             , uiTitle="Plot Signals"
>                             , uiSize=(1200,500)} $ myGraph doNoise
>
> straightNoise, flangedNoise, straightA440, flangedA440 :: AudSF () Double
>
> straightNoise = commonSound True  False
> flangedNoise =  commonSound True  True
> straightA440 =  commonSound False False
> flangedA440 =   commonSound False True
>
> tstraightA440, tflangedA440, tstraightNoise, tflangedNoise :: Time -> IO ()
>
> tstraightA440  secs = outFileNorm "straightA440.wav"   secs straightA440
> tflangedA440   secs = outFileNorm "flangedA440.wav"    secs flangedA440
> tstraightNoise secs = outFileNorm "straightNoise.wav"  secs straightNoise
> tflangedNoise  secs = outFileNorm "flangedNoise.wav"   secs flangedNoise
>
> commonSound :: Bool -> Bool -> AudSF () Double
> commonSound doNoise doFlange = proc () -> do
>   rec
>     bSig <- if doNoise == True then noiseBLI nSeed -< nCPS
>                                else osc tab2 0 -< 440
>
>     dlsig <- envExpon 0.0005 5 0.09 -< ()
>     z <- delayLine1 (1/440) -< (bSig, dlsig)
>
>   outA -< if doFlange == True then 0.5*bSig + 0.5*z
>                               else bSig
>   where
>     nCPS = 200
>     nSeed = 555
>     tab2 = tableSinesN 4096 [1.0, 0.5, 0.33]
>
> myEnvGraph :: UISF () ()
> myEnvGraph = 
>   let
>     sf :: SigFun AudRate () Double
>     sf = envExpon 0.1 0.5 0.9
>   in proc () -> do
>     ex <- clockedSFToUISF 1 sf -< ()
>     _ <- title "Qit" (realtimeGraph lay 0.1 Magenta) -< ex
>     outA -< ()
>   
> myGraph :: Bool -> UISF () ()
> myGraph doNoise =
>   let qit doFlange =
>         case (doNoise, doFlange) of
>               (False, False) ->  "Straight A440"
>               (False, True)  ->  "Flanged A440"
>               (True, False)  ->  "Straight Noise"
>               (True, True)   ->  "Flanged Noise"
>   in proc () -> do
>
>   e0 <- clockedSFToUISF 1 $ commonSound doNoise False -< ()
>   e2 <- clockedSFToUISF 1 $ commonSound doNoise True  -< ()
> 
>   _ <- title (qit False)  (realtimeGraph lay 0.005 Magenta) -< e0
>   _ <- title (qit True)   (realtimeGraph lay 0.005 Cyan)    -< e2
> 
>   outA -< ()
>
> listboxMUI1 =  runMUI defaultMUIParams{uiSize=(200,100)} $ proc _ -> do
>     rec x <- listbox [C, D, E, F] 0 -< (Nothing, Nothing)
>         let x' = if x<0 then 0 else x
>     display -< x
>
> primes :: Int -> [Int]
> primes n = [x | x <- [2..n], prime x]
>
> prime :: Int -> Bool
> prime n = factors n == [1,n]
>
> factors :: Int -> [Int]
> factors n = [x | x <- [1..n], n `mod` x == 0]
>
> seq1 = [3.0, -2.0, 4.9, 3.2, -0.5]
> seq2 = [-3.0, 44.6, 7.1]
>
> arr1 = listArray (0, 4) seq1
> arr2 = listArray (0, 2) seq2
> arr3 = conv arr1 arr2
>
> junk :: () -> Array Int Double
> junk () = arr1
>
> conv :: (Ix a, Integral a, Num b) => Array a b -> Array a b -> Array a b
> conv x1 x2 = x3
>    where m1 = snd $ bounds x1
>          m2 = snd $ bounds x2
>          m3 = m1 + m2
>          x3 = listArray (0,m3)
>                 [
>                    sum [ x1!k * x2!(n-k) | k <- [max 0 (n-m2)..min n m1] ]
>                        | n <- [0..m3]
>                 ]
> h1, h2, h3 :: Array Int Integer
> h1 = listArray (0,3) [ 1, 2, 3, 4 ]
> h2 = listArray (0,4) [ 1, 2, 3, 4, 5 ]
> h3 = listArray (0,7) [ 1, 4, 10, 20, 30, 34, 31, 20 ]
>
> linspace :: Double -> Double -> Int -> [Double]
> linspace a b n =
>   map (\ i -> a + (fromIntegral i) * inc) [(0::Int) .. (n - 1)]
>  where
>   inc = (b - a) / (fromIntegral (n - 1))
>
> -- linspace 0.0 1.0 5 == [ 0.0, 0.25, 0.5, 0.75 1.0 ]
>

fft ===============================================================================

> checkItem ldt lfft
>   | traceAlways msg False = undefined
>   | otherwise = True
>   where msg = unwords [ "\nfftEx ldt =" ++ show (typeOf ldt) ++ " lfft =" ++ show (typeOf lfft)]
>
> nFFTDivs :: Int
> nFFTDivs = defDivs
>
> fftEx :: UISF () ()
> fftEx = proc _ -> do 
>   fKnob <- title "Frequency" (hSlider (1,2000) 440) -< ()
>   e0 <- clockedSFToUISF 400 sig -< fKnob
>   let (ts, ffts) = unzip e0
>       histTable :: Array Int Int
>       histTable = makeHistogram nFFTDivs ts
>   
>   let ok = checkItem ts ffts
>       ok2 = case ok of
>             True -> True
>             False -> error "bad fft"
>   display -< ok2
>
>   _ <- title "FFT"  (histogram     lay)   -< Just $ toNormalizedList histTable
>   _ <- title "Sine" (realtimeGraph lay 0.02 Black) -< e0
>   
>   outA -< ()
>   where
>     sig :: SigFun CtrRate Double Double
>     sig = proc fVal -> do
>       sVal <- osc (tableSinesN 4096 [1]) 0 -< fVal
>       fftData <- fftA 100 256 -< sVal
>       outA -< sVal -- (ss, fftData)
>
> tfft = runMUI defaultUIParams{
>                              uiTitle="FFT"
>                              , uiSize=(1200,500)} fftEx

CAPTCHA ===========================================================================

> capture_par :: Music (Pitch, Volume) -> IO()
> capture_par m = writeMidi2_par "Parthenopea.mid" m
>
> writeMidi2_par :: ToMusic1 a => FilePath -> Music a -> IO ()
> writeMidi2_par fn m = exportMidiFile_par fn $ toMidiUPM2_par defUpm $ perform m
>
> exportMidiFile_par :: FilePath -> Midi -> IO ()
> exportMidiFile_par fn = Byte.writeFile fn . makeFile_par
>
> toMidiUPM2_par :: UserPatchMap -> [MEvent] -> Midi
> toMidiUPM2_par upm pf
>   | traceIf msg False = undefined
>   | otherwise =
>      let split     = resolveMEventInsts_par $ splitByInst pf
>          insts     = map fst split
>          rightMap  =  if (allValid upm insts)
>                       then upm
>                       else (makeGMMap insts)
>    in Midi  (if length split == 1  then SingleTrack
>                                    else MultiTrack)
>             (TicksPerBeat division)
>             (map (fromAbsTime . mevsToMessages rightMap) split)
>   where msg = unwords [ "\ntoMidiUPM2_par upm =" ++ show upm ++ " pf =" ++ show pf]
>
> resolveMEventInsts_par :: [(InstrumentName, [MEvent])] -> [(InstrumentName, [MEvent])]
> resolveMEventInsts_par = map f1 where
>     f1 (iname, mevs) = (resolveInstrumentName_par iname, map f2 mevs)
>     f2 mev = mev{eInst = resolveInstrumentName_par (eInst mev)}
>
> resolveInstrumentName_par :: InstrumentName -> InstrumentName
> resolveInstrumentName_par x@(CustomInstrument s) =
>     let iName = instNameOnly_par s
>         allInsts = take 128 $ enumFrom AcousticGrandPiano
>         i = maybe (-1) id $ findIndex (==iName) $ map show $ allInsts
>     in  if i >= 0 then allInsts !! i else x
> resolveInstrumentName_par x = x
>
> instNameOnly_par :: String -> String
> instNameOnly_par [] = []
> instNameOnly_par (x:xs) = if x==' ' then [] else x : instNameOnly_par xs
>
> makeFile_par :: Midi -> Byte.ByteString
> makeFile_par (Midi ft td trs) = 
>     let ticksPerQn = 
>             case td of TicksPerBeat x -> x
>                        TicksPerSecond x y -> 
>                            error ("(makeFile_par) Don't know how "++
>                            "to handle TicksPerSecond yet.")
>         header = makeHeader_par ft (length trs) ticksPerQn
>         body = map makeTrack_par trs
>     in  Byte.concat (header:body)
>
> makeHeader_par :: FileType -> TrackCount -> TicksPerQN -> Byte.ByteString
> makeHeader_par ft numTracks ticksPerQn = 
>     let 
>         ft' = case ft of SingleTrack -> [0x00, 0x00]
>                          MultiTrack -> [0x00, 0x01]
>                          MultiPattern -> error ("(makeHeader_par) Don't know "++
>                                          "how to handle multi-pattern yet.")
>         numTracks' = padByte_par_par 2 numTracks
>         ticksPerQn' = padByte_par_par 2 ticksPerQn
>     in  if numTracks > 16 then error ("(makeHeader_par) Don't know how to "++
>                                "handle >16 tracks!")
>         else Byte.concat [midiHeaderConst_par, Byte.pack ft', numTracks', ticksPerQn']
>
> makeTrack_par :: Track Ticks -> Byte.ByteString
> makeTrack_par t = 
>     let body = makeTrackBody_par t
>         header = makeTrackHeader_par body
>     in  Byte.concat [header, body]
>
> trackHeaderConst_par :: Byte.ByteString
> trackHeaderConst_par = Byte.pack [0x4D, 0x54, 0x72, 0x6B] 
>
> endOfTrack_par = Byte.concat [to7Bits_par 96, Byte.pack [0xFF, 0x2F, 0x00]]
>
> makeTrackHeader_par :: Byte.ByteString -> Byte.ByteString
> makeTrackHeader_par tbody = 
>     let len = Byte.length tbody
>         f = Byte.pack . map (fromIntegral . binStrToNum_par . reverse) . 
>             breakBinStrs_par 8 . pad_par (8*4) '0' . numToBinStr_par
>     in  Byte.concat [trackHeaderConst_par, f len]
>
> padByte_par_par :: Integral a => Int -> a -> Byte.ByteString
> padByte_par_par byteCount i = 
>   let b = Byte.pack [fromIntegral i] 
>       n = Byte.length b
>       padding = Byte.pack $ take (byteCount - n) $ repeat 0x00
>   in  if n < byteCount then Byte.concat [padding, b] else b
>
> makeTrackBody_par :: Track Ticks -> Byte.ByteString 
> makeTrackBody_par [] = endOfTrack_par -- end marker, very important!
> makeTrackBody_par ((ticks, msg):rest) = 
>     let b = msgToBytes_par msg
>         b' = [to7Bits_par ticks, msgToBytes_par msg, makeTrackBody_par rest]
>     in  if Byte.length b > 0 then Byte.concat b'             
>         else makeTrackBody_par rest
>
> binStrToNum_par :: String -> Int
> binStrToNum_par [] = 0
> binStrToNum_par ('0':xs) = 2* binStrToNum_par xs
> binStrToNum_par ('1':xs) = 1 + 2*binStrToNum_par xs
> binStrToNum_par _ = error "bad data."
>
> breakBinStrs_par :: Int -> String -> [String]
> breakBinStrs_par i s = if length s <= i then [s] else take i s : breakBinStrs_par i (drop i s)
>
> numToBinStr_par :: (Integral a, Show a) => a -> String
> numToBinStr_par i = showIntAtBase 2 intToDigit i ""
>
> msgToBytes_par :: Message -> Byte.ByteString
> msgToBytes_par (NoteOn c k v) = 
>     Byte.concat [Byte.pack [0x90 + fromIntegral c], padByte_par 1 k, padByte_par 1 v]
> msgToBytes_par (NoteOff c k v) = 
>     Byte.concat [Byte.pack [0x80 + fromIntegral c], padByte_par 1 k, padByte_par 1 v]
> msgToBytes_par (ProgramChange c p) =  
>     Byte.concat [Byte.pack [0xC0 + fromIntegral c], padByte_par 1 p]
> msgToBytes_par (ControlChange c n v) =  
>     Byte.concat [Byte.pack [0xB0 + fromIntegral c], padByte_par 1 n, padByte_par 1 v]
> msgToBytes_par (TempoChange t) = -- META EVENT, HAS NO CHANNEL NUMBER
>     Byte.concat [Byte.pack [0xFF, 0x51, 0x03], fixTempo_par t]
> msgToBytes_par x = error ("(msgToBytes_par) Message type not currently "++ 
>                "supported: "++show x)
>
> to7Bits_par :: (Integral a, Show a) => a -> Byte.ByteString
> to7Bits_par =  Byte.pack . map (fromIntegral . binStrToNum_par . reverse) .
>            fixBinStrs_par . map (padTo_par 7 . reverse). reverse . 
>            breakBinStrs_par 7 . reverse . padTo_par 7 . numToBinStr_par
>
>
> padByte_par :: Integral a => Int -> a -> Byte.ByteString
> padByte_par byteCount i = 
>   let b = Byte.pack [fromIntegral i] 
>       n = Byte.length b
>       padding = Byte.pack $ take (byteCount - n) $ repeat 0x00
>   in  if n < byteCount then Byte.concat [padding, b] else b
>
> pad_par :: Int -> a -> [a] -> [a]
> pad_par b x xs = if length xs >= b then xs else pad_par b x (x:xs)
>
> fixBinStrs_par :: [String] -> [String]
> fixBinStrs_par xs = 
>     let n = length xs
>         bits = take (n-1) (repeat '1') ++ "0"
>     in  Prelude.zipWith (:) bits xs
>
> fixTempo_par = Byte.pack . map (fromIntegral . binStrToNum_par . reverse) . 
>            breakBinStrs_par 8 . pad_par (4*6) '0' . numToBinStr_par
>
> padTo_par :: Int -> String -> String
> padTo_par i xs = if length xs `mod` i == 0 then xs else padTo_par i ('0':xs)
>
> type TrackCount = Int
> type TicksPerQN = Int
>
> midiHeaderConst_par :: Byte.ByteString
> midiHeaderConst_par = 
>     Byte.pack [0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06] 
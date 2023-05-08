> {-# LANGUAGE UnicodeSyntax #-}

> module LoadEmAll where
>
> import Aleatory
> import Baking ( bakedJingle )
> import Cecil ( abby, cecil, shelby, weHateHer, wj )
> import Control.Monad (foldM)
> import Covers ( slot, ssailor )
> import Debug.Trace ( traceIO, traceM )
> import Euterpea
> import Fanfare
> import HSoM
> import Parthenopea ( aggrandize, durS, playDM, pSnippet02 )
> import Players
> import Signals
> import SunPyg

LoadEmAll =================================================================================

To force inclusion of everything I might want to reference in ghci

> wrapStuff :: Int → Rational
> wrapStuff n =
>   let toss, rN :: Rational
>       toss = dur (theFanfare 1)
>       rN = fromIntegral n
>   in toss - rN

Play Jingles ==============================================================================

> playSnippet :: () → Int → IO ()
> playSnippet () i =
>    let inst :: InstrumentName
>        inst = toEnum (i `mod` fromEnum Gunshot)
>    in do
>       traceIO ("InstrumentName = " ++ show inst)
>       play $ instrument inst pSnippet02
>
> playSnippets :: IO ()
> playSnippets = 
>    foldM playSnippet () [0..]
>
> ajingles, bjingles
>  , cjingles, djingles :: [(String, Music (Pitch, [NoteAttribute]))]
>
> ajingles =
>    [("theFanfare"      , aggrandize (theFanfare 4))
>    , ("slot"           , aggrandize (slot 4))
>    , ("alice"          , aggrandize alice)
>    , ("bob"            , aggrandize (bob 4))
>    , ("copper"         , aggrandize (copper 2))
>    , ("gold"           , aggrandize gold)
>    , ("silver"         , aggrandize silver)]
> bjingles =
>    [("getCITM"         , aggrandize getCITM)
>    , ("bake"           , bakedJingle 345)
>    , ("bill"           , aggrandize (bill 4))
>    , ("roger"          , aggrandize roger)]
> cjingles =
>    [("cecil"           , aggrandize cecil)
>    , ("abby"           , aggrandize abby)
>    , ("wj"             , aggrandize wj)
>    , ("shelby"         , aggrandize shelby)
>    , ("weHateHer"      , aggrandize weHateHer)]
> djingles =
>    [("waypostpurple"   , aggrandize waypostpurple)
>    , ("whelpNarp"      , aggrandize whelpNarp)
>    , ("snake"          , aggrandize snake)
>    , ("pendingtonArnt" , aggrandize (pendingtonArnt 2))
>    , ("ssailor"        , aggrandize ssailor)]
>
> playJingle :: () → (String, Music (Pitch, [NoteAttribute])) → IO ()
> playJingle _ (s, m) =
>    do
>       traceM ( show s ++ " " ++ show (durS (dur m)) ++ " seconds" )
>       playDM Nothing m
>       
> playJingles :: [(String, Music (Pitch, [NoteAttribute]))] → IO ()
> playJingles jingles =
>    foldM playJingle () (cycle jingles)
>

CAPTCHA ===================================================================================

> {-
> exportMidiFile_par :: FilePath → Midi → IO ()
> exportMidiFile_par fn = Byte.writeFile fn . makeFile_par

> capture_par :: Music (Pitch, Volume) → IO()
> capture_par m = writeMidi2_par "Parthenopea.mid" m
>
> writeMidi2_par :: ToMusic1 a ⇒ FilePath → Music a → IO ()
> writeMidi2_par fn m = exportMidiFile_par fn $ toMidiUPM2_par defUpm $ perform m
>
> exportMidiFile_par :: FilePath → Midi → IO ()
> exportMidiFile_par fn = Byte.writeFile fn . makeFile_par
>
> toMidiUPM2_par :: UserPatchMap → [MEvent] → Midi
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
> resolveMEventInsts_par :: [(InstrumentName, [MEvent])] → [(InstrumentName, [MEvent])]
> resolveMEventInsts_par = map f1 where
>     f1 (iname, mevs) = (resolveInstrumentName_par iname, map f2 mevs)
>     f2 mev = mev{eInst = resolveInstrumentName_par (eInst mev)}
>
> resolveInstrumentName_par :: InstrumentName → InstrumentName
> resolveInstrumentName_par x@(CustomInstrument s) =
>     let iName = instNameOnly_par s
>         allInsts = take 128 $ enumFrom AcousticGrandPiano
>         i = maybe (-1) id $ findIndex (==iName) $ map show $ allInsts
>     in  if i >= 0 then allInsts !! i else x
> resolveInstrumentName_par x = x
>
> instNameOnly_par :: String → String
> instNameOnly_par [] = []
> instNameOnly_par (x:xs) = if x==' ' then [] else x : instNameOnly_par xs
>
> makeFile_par :: Midi → Byte.ByteString
> makeFile_par (Midi ft td trs) = 
>     let ticksPerQn = 
>             case td of TicksPerBeat x → x
>                        TicksPerSecond x y → 
>                            error ("(makeFile_par) Don't know how "++
>                            "to handle TicksPerSecond yet.")
>         header = makeHeader_par ft (length trs) ticksPerQn
>         body = map makeTrack_par trs
>     in  Byte.concat (header:body)
>
> makeHeader_par :: FileType → TrackCount → TicksPerQN → Byte.ByteString
> makeHeader_par ft numTracks ticksPerQn = 
>     let 
>         ft' = case ft of SingleTrack → [0x00, 0x00]
>                          MultiTrack → [0x00, 0x01]
>                          MultiPattern → error ("(makeHeader_par) Don't know "++
>                                          "how to handle multi-pattern yet.")
>         numTracks' = padByte_par_par 2 numTracks
>         ticksPerQn' = padByte_par_par 2 ticksPerQn
>     in  if numTracks > 16 then error ("(makeHeader_par) Don't know how to "++
>                                "handle >16 tracks!")
>         else Byte.concat [midiHeaderConst_par, Byte.pack ft', numTracks', ticksPerQn']
>
> makeTrack_par :: Track Ticks → Byte.ByteString
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
> makeTrackHeader_par :: Byte.ByteString → Byte.ByteString
> makeTrackHeader_par tbody = 
>     let len = Byte.length tbody
>         f = Byte.pack . map (fromIntegral . binStrToNum_par . reverse) . 
>             breakBinStrs_par 8 . pad_par (8*4) '0' . numToBinStr_par
>     in  Byte.concat [trackHeaderConst_par, f len]
>
> padByte_par_par :: Integral a ⇒ Int → a → Byte.ByteString
> padByte_par_par byteCount i = 
>   let b = Byte.pack [fromIntegral i] 
>       n = Byte.length b
>       padding = Byte.pack $ take (byteCount - n) $ repeat 0x00
>   in  if n < byteCount then Byte.concat [padding, b] else b
>
> makeTrackBody_par :: Track Ticks → Byte.ByteString 
> makeTrackBody_par [] = endOfTrack_par -- end marker, very important!
> makeTrackBody_par ((ticks, msg):rest) = 
>     let b = msgToBytes_par msg
>         b' = [to7Bits_par ticks, msgToBytes_par msg, makeTrackBody_par rest]
>     in  if Byte.length b > 0 then Byte.concat b'             
>         else makeTrackBody_par rest
>
> binStrToNum_par :: String → Int
> binStrToNum_par [] = 0
> binStrToNum_par ('0':xs) = 2* binStrToNum_par xs
> binStrToNum_par ('1':xs) = 1 + 2*binStrToNum_par xs
> binStrToNum_par _ = error "bad data."
>
> breakBinStrs_par :: Int → String → [String]
> breakBinStrs_par i s = if length s <= i then [s] else take i s : breakBinStrs_par i (drop i s)
>
> numToBinStr_par :: (Integral a, Show a) ⇒ a → String
> numToBinStr_par i = showIntAtBase 2 intToDigit i ""
>
> msgToBytes_par :: Message → Byte.ByteString
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
> to7Bits_par :: (Integral a, Show a) ⇒ a → Byte.ByteString
> to7Bits_par =  Byte.pack . map (fromIntegral . binStrToNum_par . reverse) .
>            fixBinStrs_par . map (padTo_par 7 . reverse). reverse . 
>            breakBinStrs_par 7 . reverse . padTo_par 7 . numToBinStr_par
>
>
> padByte_par :: Integral a ⇒ Int → a → Byte.ByteString
> padByte_par byteCount i = 
>   let b = Byte.pack [fromIntegral i] 
>       n = Byte.length b
>       padding = Byte.pack $ take (byteCount - n) $ repeat 0x00
>   in  if n < byteCount then Byte.concat [padding, b] else b
>
> pad_par :: Int → a → [a] → [a]
> pad_par b x xs = if length xs >= b then xs else pad_par b x (x:xs)
>
> fixBinStrs_par :: [String] → [String]
> fixBinStrs_par xs = 
>     let n = length xs
>         bits = take (n-1) (repeat '1') ++ "0"
>     in  Prelude.zipWith (:) bits xs
>
> fixTempo_par = Byte.pack . map (fromIntegral . binStrToNum_par . reverse) . 
>            breakBinStrs_par 8 . pad_par (4*6) '0' . numToBinStr_par
>
> padTo_par :: Int → String → String
> padTo_par i xs = if length xs `mod` i == 0 then xs else padTo_par i ('0':xs)
>
> type TrackCount = Int
> type TicksPerQN = Int
>
> midiHeaderConst_par :: Byte.ByteString
> midiHeaderConst_par = 
>     Byte.pack [0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06] 
> -}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

SoundFont support =====================================================================================================

> module SoundFont where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import qualified Data.Audio              as A
> import Data.Array.Unboxed ( array, Array, (!), IArray(bounds), (//), assocs )
> import qualified Data.Bifunctor          as BF
> import Data.Char
> import Data.Foldable ( Foldable(foldl'), find, minimumBy )
> import Data.Int ( Int8, Int16, Int32 )
> import Data.List ( find, foldr, minimumBy, singleton, foldl' )
> import qualified Data.Map                as Map
> import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing, catMaybes, mapMaybe )
> import Data.Time.Clock
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.IO ( outFile, outFileNorm )
> import Euterpea.IO.Audio.Render ( renderSF, Instr, InstrMap )
> import Euterpea.IO.Audio.Types ( AudRate, AudSF, Mono, Stereo )
> import Euterpea.Music
> import Parthenopea
> import Synthesizer
> import qualified System.FilePattern.Directory
>                                          as FP
> import qualified Text.FuzzyFind          as FF
  
notes on three kinds of scoring =======================================================================================

In order of when they occur in the overall process:

1. FuzzyFind      - Presented with each *.sf2 file, we record qualifying items into roster based on their names
                    in the file -- e.g. "Iowa Viola-pp". The qualification scores are a function of FuzzyFind
                    results on keywords we have associated with GM InstrumentName and PercussionSound. The resulting
                    rosters typically include some GM items with MANY candidates and some with zero.
  
2. static scoring - Before rendering, we bind maximum of one instrument or percussion per each GM item, resulting 
                    in runtime map of highest scoring candidate for each given GM item. Static scoring
                    (see computeStaticScore) takes into account attributes like stereo, 24-bit, number
                    of splits. See Locators.

3. zone scoring   - While rendering, presented with a note, choose the zone that best fits (highest score) the
                    required pitch, velocity, etc. These attributes are weighted in score computation. Note that
                    zone scoring determines runtime choice of percussion instruments -- zone associations in
                    db are irrelevant; only the instrument associations matter.

importing sampled sound (from SoundFont (*.sf2) files) ================================================================

> data PerGMKey =
>   PerGMKey {
>     pWordF             :: Word
>   , pWordI             :: Word
>   , pWordZ             :: Maybe Word} deriving (Eq, Ord, Show)
>
> data PerGMScored =
>   PerGMScored {
>     pStaticScore       :: Int
>   , pPerGMKey          :: PerGMKey} deriving (Eq, Ord, Show)
>
> type InstrLocator      = Map.Map InstrumentName PerGMScored
> type PercLocator       = Map.Map PercussionSound PerGMScored
> type Locators          = (InstrLocator, PercLocator)
>
> type NameMap           = Map.Map String Word
> type NameMaps          = (NameMap, NameMap)
>
> type ZoneCache         = Map.Map PerGMKey [(Word, SFZone)]
>
> data SFRoster =
>   SFRoster {
>     zInit              :: [SoundFontInit]
>   , zFiles             :: Array Int SFFile
>   , zZoneCache         :: ZoneCache
>   , zLocs              :: Locators}
>
> data SFFile =
>   SFFile {
>     zFilename          :: FilePath
>   , zNickname          :: String
>   , zArrays            :: SoundFontArrays
>   , zWordF             :: Word}
>
> data SoundFontArrays = 
>   SoundFontArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssIMods            :: Array Word F.Mod
>   , ssShdrs            :: Array Word F.Shdr
>   , ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
>
> data SoundFontInit =
>   SoundFontInit {
>     iFilename          :: FilePath
>   , iNickname          :: String
>   , iInstSpecs         :: [(String, ([Hints], InstrumentName))]
>   , iPercSpecs         :: [(String, [(String, ([Hints], PercussionSound))])]}
>
> data Hints =
>   DLow
>   | DMed
>   | DHigh
>   | DScore Double deriving Show
>
> data SFZone =
>   SFZone {
>     zStartOffs         :: Maybe Int
>   , zEndOffs           :: Maybe Int
>   , zLoopStartOffs     :: Maybe Int
>   , zLoopEndOffs       :: Maybe Int
>
>   , zStartCoarseOffs   :: Maybe Int
>   , zEndCoarseOffs     :: Maybe Int
>   , zLoopStartCoarseOffs
>                        :: Maybe Int
>   , zLoopEndCoarseOffs :: Maybe Int
>
>   , zInstIndex         :: Maybe Word
>   , zKeyRange          :: Maybe (AbsPitch, AbsPitch)
>   , zVelRange          :: Maybe (Volume, Volume)
>   , zKey               :: Maybe Word
>   , zVel               :: Maybe Word
>   , zInitAtten         :: Maybe Int
>   , zCoarseTune        :: Maybe Int
>   , zFineTune          :: Maybe Int
>   , zSampleIndex       :: Maybe Word
>   , zSampleMode        :: Maybe A.SampleMode
>   , zScaleTuning       :: Maybe Int
>   , zExclusiveClass    :: Maybe Int
>
>   , zDelayVolEnv       :: Maybe Int
>   , zAttackVolEnv      :: Maybe Int
>   , zHoldVolEnv        :: Maybe Int
>   , zDecayVolEnv       :: Maybe Int
>   , zSustainVolEnv     :: Maybe Int 
>   , zReleaseVolEnv     :: Maybe Int
>
>   , zChorus            :: Maybe Int -- designated M
>   , zReverb            :: Maybe Int -- designated VH
>   , zPan               :: Maybe Int -- designated VH
>
>   , zRootKey           :: Maybe Word
>
>   , zModLfoToPitch     :: Maybe Int
>   , zVibLfoToPitch     :: Maybe Int
>   , zModEnvToPitch     :: Maybe Int
>   , zInitFc            :: Maybe Int
>   , zInitQ             :: Maybe Int -- designated L
>   , zModLfoToFc        :: Maybe Int
>   , zModEnvToFc        :: Maybe Int
>   , zModLfoToVol       :: Maybe Int
>   , zDelayModLfo       :: Maybe Int
>   , zFreqModLfo        :: Maybe Int -- designated M
>   , zDelayVibLfo       :: Maybe Int
>   , zFreqVibLfo        :: Maybe Int -- designated M
>   , zDelayModEnv       :: Maybe Int
>   , zAttackModEnv      :: Maybe Int -- designated M
>   , zHoldModEnv        :: Maybe Int
>   , zDecayModEnv       :: Maybe Int
>   , zSustainModEnv     :: Maybe Int -- designated M
>   , zReleaseModEnv     :: Maybe Int -- designated L
>   , zKeyToModEnvHold   :: Maybe Int
>   , zKeyToModEnvDecay  :: Maybe Int
>   , zKeyToVolEnvHold   :: Maybe Int
>   , zKeyToVolEnvDecay  :: Maybe Int
>
>   , zModulators        :: Maybe (Word, Word)} deriving Show
>
> defInstrumentZone      :: SFZone
> defInstrumentZone                        = SFZone Nothing Nothing Nothing Nothing
>
>                                            Nothing Nothing Nothing Nothing
>
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing
>
>                                            Nothing Nothing Nothing
>     
>                                            Nothing
> 
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing Nothing Nothing
>                                            Nothing Nothing
>
>                                            Nothing
>
> fileName               :: SFRoster → Word → String
> fileName sfrost wordF = zFilename (zFiles sfrost ! fromIntegral wordF)
>
> instrName              :: SFFile → F.Inst → String
> instrName sffile = F.instName
>
> zoneName               :: SFFile → F.Inst → SFZone → String
> zoneName sffile iinst sfzone =
>   let
>     arrays = zArrays sffile
>     msin = zSampleIndex sfzone
>   in
>     instrName sffile iinst
>     ++ "/"
>     ++ maybe "Global" (\a → F.sampleName (ssShdrs arrays ! a)) msin

slurp in instruments from SoundFont (*.sf2) files =====================================================================

> fileByIndex            :: SFRoster → Word → SFFile
> fileByIndex sfrost wFile = zFiles sfrost ! fromIntegral wFile
>
> doSoundFontMusic       :: [SoundFontInit] → [(String, Music (Pitch, [NoteAttribute]))] → IO ()
> doSoundFontMusic sfdb songs =
>   do
>
>     putStrLn "processing..."
>
>     ts1                ← getCurrentTime
>
>     sffilesp           ← CM.zipWithM readSoundFontFile [0..] sfdb
>     zonesP             ← cacheZones (map fst sffilesp)
>     putStrLn ("# available instr ="      ++ show (Map.size zonesP))
>
>     ts2                ← getCurrentTime
>
>     putStrLn ("___load files: "          ++ show (diffUTCTime ts2 ts1))
>
>     let ((iloc, ploc), probs)
>                        = foldr (uncurry (chooseIAndP zonesP)) ((Map.empty, Map.empty), []) sffilesp
>     mapM_ putStrLn probs
>     let sfrost         = SFRoster sfdb
>                                   (array (0, length sffilesp - 1) (zip [0..] (map fst sffilesp)))
>                                   zonesP
>                                   (iloc, ploc)
>     let imap           = assignInstruments sfrost iloc
>     let pmap           = assignAllPercussion sfrost ploc
>     let imap'          = imap ++ [doAssignP sfrost pmap]
>
>     ts3                ← getCurrentTime
>
>     putStrLn ("___prepare instruments: " ++ show (diffUTCTime ts3 ts2))
>     _                  ← renderSongs sfrost imap' songs
>
>     ts4                ← getCurrentTime
>
>     putStrLn ("___render songs: "        ++ show (diffUTCTime ts4 ts3))
>     return ()
>  
> doSoundFontDig         :: String → FilePath → IO ()
> doSoundFontDig prefix outFilename =
>   do
>
>     putStrLn "digging..."
>
>     ts1                ← getCurrentTime
>
>     fps                ← FP.getDirectoryFiles "." (singleton "edit*.sf2")
>     sffilesp           ← CM.zipWithM (openSoundFontFile prefix) [0..] fps
>
>     ts2 ← getCurrentTime
>
>     putStrLn ("___load files: " ++ show (diffUTCTime ts2 ts1))
>
>     zc                 ← cacheZones sffilesp
>     ts3                ← getCurrentTime
>     putStrLn ("___cache zones: " ++ show (diffUTCTime ts3 ts2))
>     spillRosters zc sffilesp outFilename
>
>     ts4 ← getCurrentTime
>
>     putStrLn ("___write rosters: " ++ show (diffUTCTime ts4 ts3))
>
> openSoundFontFile      :: String → Word → FilePath → IO SFFile
> openSoundFontFile nick wFile filename    = do
>   putStr (show wFile ++ " " ++ filename)
>   ts1 ← getCurrentTime
>   maybeAudio ← F.importFile filename
>   case maybeAudio of
>     Left s               → error $ "SoundFont decoding error: " ++ s
>     Right soundFont      → do
>       let pdata = F.pdta soundFont
>       let sdata = F.sdta soundFont
>       let arrays = SoundFontArrays
>                      (F.insts pdata)
>                      (F.ibags pdata)
>                      (F.igens pdata)
>                      (F.imods pdata)
>                      (F.shdrs pdata)
>                      (F.smpl sdata)
>                      (F.sm24 sdata)
>       let sffile = SFFile filename nick arrays wFile
>       let nBits = case ssM24 (zArrays sffile) of
>             Nothing          → 16
>             Just s24data     → 24
>       ts2 ← getCurrentTime
>       putStrLn (" (" ++ show nBits ++ ") loaded in " ++ show (diffUTCTime ts2 ts1))
>       putStrLn ("openSoundFontFile ssInsts" ++ show (bounds $ ssInsts arrays))
>       putStrLn ("openSoundFontFile ssIBags" ++ show (bounds $ ssIBags arrays))
>       putStrLn ("openSoundFontFile ssIGens" ++ show (bounds $ ssIGens arrays))
>       putStrLn ("openSoundFontFile ssIMods" ++ show (bounds $ ssIMods arrays))
>       putStrLn ("openSoundFontFile ssShdrs" ++ show (bounds $ ssShdrs arrays))
>       return sffile
>     
> readSoundFontFile      :: Word
>                           → SoundFontInit
>                           → IO (SFFile
>                                 , (   [(String, ([Hints], InstrumentName))]
>                                    ,  [(String, [(String, ([Hints], PercussionSound))])]))
> readSoundFontFile wFile sfinit =
>   let
>     (filename, (ilist, plist)) = (iFilename sfinit, (iInstSpecs sfinit, iPercSpecs sfinit))
>   in do
>     sffile ← openSoundFontFile "no nickname" wFile filename 
>     return (sffile, (ilist, plist))
>
> renderSong             :: SFRoster
>                           → InstrMap (Stereo AudRate)
>                           → FilePath
>                           → Music (Pitch, [NoteAttribute])
>                           → IO ()
> renderSong sfrost imap name song =
>   do
>     ts1 ← getCurrentTime
>     let is = Map.keys $ fst $ listI song initCase (Map.empty, Map.empty)
>     let ps = Map.keys $ listP song initCase Map.empty
>     putStrLn ("\n" ++ name ++ ": " ++ show (length is) ++ " <- instruments, percussion -> " ++ show (length ps))
>     let results = printChoices sfrost is ps
>     mapM_ (putStrLn . snd) (fst results)
>     mapM_ (putStrLn . snd) (snd results)
>
>     -- render song only if all OK
>     if all fst (fst results) && all fst (snd results)
>       then do
>         let path = name ++ ".wav"
>         putStr path
>         let (d,s) = renderSF song imap
>         outFile path d s
>         ts2 ← getCurrentTime
>         putStrLn (" (dur=" ++ show d ++ ") written in " ++ show (diffUTCTime ts2 ts1))
>       else
>         putStrLn "skipping..."
>     return ()
>
> renderSongs            :: SFRoster
>                           → InstrMap (Stereo AudRate)
>                           → [(String, Music (Pitch, [NoteAttribute]))]
>                           → IO ()
> renderSongs sfrost imap songs =
>   do
>     mapM_ (uncurry (renderSong sfrost imap)) songs
>
> countSoundFontGMItems  :: [SoundFontInit] → IO ()
> countSoundFontGMItems sfinits = do
>   putStrLn "\nInstruments:"
>   let finI = foldl' combineHistograms zeroHistogramI (map countInstrumentsF sfinits)
>   mapM_ printOneI (assocs finI)
>   putStrLn "\nPercussion:"
>   let finP = foldl' combineHistograms zeroHistogramP (map countPercussionF sfinits)
>   mapM_ printOneP (assocs finP)
>
> printOneI              :: (Int, Int) -> IO ()
> printOneI (iname', count)                = putStrLn (show iname ++ ":=" ++ show count)
>   where
>     iname              :: InstrumentName = toEnum iname'
>     
> printOneP              :: (Int, Int) -> IO ()
> printOneP (psound', count)               = putStrLn (show psound ++ " ::= " ++ show count)
>   where
>     psound             :: PercussionSound= toEnum psound'
>     
> countInstrumentsF      :: SoundFontInit → Histogram
> countInstrumentsF sfinit                 = countIF (iInstSpecs sfinit)
>
> countPercussionF      :: SoundFontInit → Histogram
> countPercussionF sfinit                  = countPF (iPercSpecs sfinit)
>
> countIF                :: [(String, ([Hints], InstrumentName))] → Histogram
> countIF iss = foldl' combineHistograms zeroHistogramI (map countOneI iss)
>
> countPF                :: [(String, [(String, ([Hints], PercussionSound))])] → Histogram
> countPF pss = foldl' combineHistograms zeroHistogramP (map countOneP' pss)
>
> countOneI              :: (String, ([Hints], InstrumentName)) → Histogram
> countOneI (_, (_, iname)) = zeroHistogramI // singleton (fromEnum iname, 1)
>
> countOneP'             :: (String, [(String, ([Hints], PercussionSound))]) → Histogram
> countOneP' (_, sshp) = foldl' combineHistograms zeroHistogramP (map countOneP sshp)
>
> countOneP             :: (String, ([Hints], PercussionSound)) → Histogram
> countOneP (_, (_, psound)) = zeroHistogramP // singleton (fromEnum psound, 1)

tournament among GM instruments from SoundFont files ==================================================================

> chooseI                :: ZoneCache
>                           → SFFile
>                           → [(String, ([Hints], InstrumentName))]
>                           → NameMaps
>                           → (Locators, [String])
>                           → (Locators, [String])
> chooseI _ _ [] _ cur                     = cur
> chooseI zc sffile (x:xs) (nMapI, nMapZ) ((iloc, ploc), probs) =
>   let
>     nameI              :: String         = fst x 
>     hints              :: [Hints]        = (fst.snd) x
>     iname              :: InstrumentName = (snd.snd) x
>
>     mwInstr            :: Maybe Word     = Map.lookup nameI nMapI
>
>     pergm              :: PerGMKey       = PerGMKey (zWordF sffile) (fromJust mwInstr) Nothing
>
>     ((iloc', ploc'), probs')
>                        :: (Locators, [String])
>       | isNothing mwInstr                = ((iloc, ploc), ("Instrument "
>                                                           ++ nameI
>                                                           ++ " not found in "
>                                                           ++ zFilename sffile) : probs)
>       | otherwise                        = ((xaStaticScoring zc
>                                                              sffile
>                                                              pergm
>                                                              nameI
>                                                              iname
>                                                              hints
>                                                              iloc
>                                             , ploc), probs)
>   in
>     chooseI zc sffile xs (nMapI, nMapZ) ((iloc', ploc'), probs')
>
> chooseP                  :: ZoneCache
>                           → SFFile
>                           → [(String, [(String, ([Hints], PercussionSound))])]
>                           → NameMaps
>                           → (Locators, [String])
>                           → (Locators, [String])
> chooseP _ _ [] _ cur                  = cur
> chooseP zc sffile (x:xs) (nMapI, nMapZ) ((iloc, ploc), probs) =
>   let
>     zF                 :: Word           = zWordF sffile
>     nameI              :: String         = fst x
>     sounds             :: [(String, ([Hints], PercussionSound))]
>                                          = snd x
>
>     mwInstr            :: Maybe Word     = Map.lookup nameI nMapI
>
>     ((iloc', ploc'), probs')
>                        :: (Locators, [String])
>       | isNothing mwInstr                = ((iloc, ploc), ("Instrument "
>                                                           ++ nameI
>                                                           ++ "not found in "
>                                                           ++ zFilename sffile) : probs)
>       | otherwise                        = chooseSound zc
>                                                        sffile
>                                                        (fromJust mwInstr)
>                                                        sounds
>                                                        (nMapI, nMapZ)
>                                                        ((iloc, ploc), probs)
>   in
>     chooseP zc sffile xs (nMapI, nMapZ) ((iloc', ploc'), probs')
>   where
>     chooseSound        :: ZoneCache
>                           → SFFile
>                           → Word
>                           → [(String, ([Hints], PercussionSound))]
>                           → NameMaps
>                           → (Locators, [String])
>                           → (Locators, [String])
>     chooseSound _ _ _ [] _ cur           = cur
>     chooseSound zc sffile wordI (x:xs) (nMapI, nMapZ) ((iloc, ploc), probs) =
>       let
>         zF             :: Word           = zWordF sffile
>         nameZ          :: String         = fst x
>         psound         :: PercussionSound= (snd.snd) x
>         hints          :: [Hints]        = (fst.snd) x
>
>         mwZone         :: Maybe Word     = Map.lookup nameZ nMapZ
>         pergm          :: PerGMKey       = PerGMKey zF wordI Nothing
>
>         ((iloc', ploc'), probs')
>                        :: (Locators, [String])
>           | isNothing mwZone             =  ((iloc, ploc)
>                                            , ("Zone " ++ show nameZ
>                                               ++ " not found in "
>                                               ++ zFilename sffile) : probs)
>           | otherwise                    = ((iloc, xaStaticScoring zc
>                                                                    sffile
>                                                                    pergm
>                                                                    nameZ
>                                                                    psound
>                                                                    hints
>                                                                    ploc)
>                                             , probs)
>       in
>         chooseSound zc sffile wordI xs (nMapI, nMapZ) ((iloc', ploc'), probs')
>
> chooseIAndP            :: ZoneCache
>                           → SFFile
>                           → (  [(String, ([Hints], InstrumentName))]
>                              , [(String, [(String, ([Hints], PercussionSound))])])
>                           → (Locators, [String])
>                           → (Locators, [String])
> chooseIAndP zc sffile (is, ps) ((iloc, ploc), probs)
>   | traceIf msg False = undefined
>   | otherwise =
>   let
>     arrays = zArrays sffile
>     (nMapI, nMapZ)                 = makeNameMaps arrays
>     ((iloc', ploc'), probs')       = chooseI zc sffile is
>                                              (nMapI, nMapZ) ((iloc, ploc), probs)
>     ((iloc'', ploc''), probs'')    = chooseP zc sffile ps
>                                              (nMapI, nMapZ) ((iloc', ploc'), probs')
>   in
>     ((iloc'', ploc''), probs'')
>   where
>     msg = unwords ["chooseIAndP ", show (is, ps)]
>
> xaStaticScoring        :: forall a. (Ord a, Show a) ⇒
>                           ZoneCache
>                           → SFFile
>                           → PerGMKey
>                           → String
>                           → a
>                           → [Hints]
>                           → Map.Map a PerGMScored
>                           → Map.Map a PerGMScored
> xaStaticScoring zc sffile pergm name kind hints loc
>   | traceIf msg False = undefined
>   | otherwise = loc'
>   where
>     loc' 
>       | myScore > oldScore               = Map.insert kind pergm' loc
>       | otherwise                        = loc
>
>     oldScore           :: Int            = maybe (-5000)
>                                                  pStaticScore
>                                                  (Map.lookup kind loc)
>     myScore            :: Int            = computeStaticScore zc sffile pergm hints
>     pergm'             :: PerGMScored    = PerGMScored myScore pergm
>     msg = unwords ["xaStaticScoring ", show kind, " old=", show oldScore, " new=", show myScore] 
>
> computeStaticScore     :: ZoneCache → SFFile → PerGMKey → [Hints] → Int
> computeStaticScore zc sffile pergm hints
>   | traceIf msg False = undefined
>   | otherwise = sum weightedScores
>   where
>     zs                                   = tail $ getZonesFromCache zc pergm
>     arrays                               = zArrays sffile
>
>     desires            :: [Desires]      = [desireReStereo
>                                           , desireRe24Bit
>                                           , desireReMaxSplits
>                                           , desireReUnsupported]
>     empiricals         :: [Int]          = [   scoreBool $ isStereoInst arrays zs
>                                              , scoreBool $ is24BitInst  arrays zs
>                                              , scoreBool $ hasMaxSplits arrays zs
>                                              , scoreBool $ instConforms arrays zs]
>
>     s                  :: [Int]          = map scoreDesire    desires
>     s'                 :: [Int]          = zipWith (*) s      empiricals
>
>     baseScores         :: [Int]          = [  sfscore hints
>                                              , head s'
>                                              , (head.tail) s'
>                                              , (head.tail.tail) s'
>                                              , (head.tail.tail.tail) s']
>     weights            :: [Int]          = [   weightInstrumentHints
>                                              , weightStereo
>                                              , weight24Bit
>                                              , weightMaxSplits
>                                              , weightConformant]
>     weightedScores     :: [Int]          = zipWith (*) baseScores weights
>
>     msg = unwords [   "computeStaticScore "  , show baseScores
>                     , " X "                  , show weights
>                     , " = "                  , show weightedScores]
>
> isStereoInst, is24BitInst, hasMaxSplits
>                        :: SoundFontArrays → [(Word, SFZone)] → Bool
>
> isStereoInst arrays zs                   = isJust $ find (isStereoZone arrays) (map snd zs)
>       
> isStereoZone           :: SoundFontArrays → SFZone → Bool
> isStereoZone arrays zone                 = stype == SampleTypeRight || stype == SampleTypeLeft
>   where
>     sin = fromJust $ zSampleIndex zone
>     shdr = ssShdrs arrays ! sin
>     stype = toSampleType $ F.sampleType shdr 
>         
> instConforms           :: SoundFontArrays → [(Word, SFZone)] → Bool
> instConforms arrays                      = all (zoneConforms arrays)
>
> zoneConforms           :: SoundFontArrays → (Word, SFZone) → Bool
> zoneConforms arrays (_, zone)            =
>   or unsupported
>   where
>     unsupported        :: [Bool]
>     unsupported                          =
>       [
>           isJust $ zScaleTuning          zone
>         , isJust $ zExclusiveClass       zone
>         , isJust $ zChorus               zone
>         , isJust $ zModLfoToPitch        zone
>         , isJust $ zModEnvToPitch        zone
>         , isJust $ zInitFc               zone
>         , isJust $ zInitQ                zone
>         , isJust $ zModLfoToFc           zone
>         , isJust $ zModEnvToFc           zone
>         , isJust $ zModLfoToVol          zone
>         , isJust $ zDelayModLfo          zone
>         , isJust $ zFreqModLfo           zone
>         , isJust $ zDelayVibLfo          zone
>         , isJust $ zFreqVibLfo           zone
>         , isJust $ zDelayModEnv          zone
>         , isJust $ zAttackModEnv         zone
>         , isJust $ zHoldModEnv           zone
>         , isJust $ zDecayModEnv          zone
>         , isJust $ zSustainModEnv        zone
>         , isJust $ zReleaseModEnv        zone
>         , isJust $ zKeyToModEnvHold      zone
>         , isJust $ zKeyToModEnvDecay     zone
>         , isJust $ zKeyToVolEnvHold      zone
>         , isJust $ zKeyToVolEnvDecay     zone
>         , isJust $ zModulators           zone
>       ]
>
> is24BitInst arrays _                     = isJust $ ssM24 arrays       
> hasMaxSplits _ zs                        = length zs > fromIntegral splitThreshold
 
extract data from SoundFont per instrument ============================================================================

> buildZone              :: SFFile → F.Inst → SFZone → Word → (Word, SFZone)
> buildZone sffile iinst fromZone bagIndex = (bagIndex, zone)
>   where
>     arrays = zArrays sffile
>     ibags = ssIBags arrays
>     xgeni = F.genNdx $ ibags!bagIndex
>     ygeni = F.genNdx $ ibags!(bagIndex + 1)
>     gens = if ygeni - xgeni < 0
>            then error "SoundFont file is corrupt: degenerate generator list"
>            else getGens sffile iinst [xgeni..ygeni-1]
>     xmodi = F.modNdx $ ibags!bagIndex
>     ymodi = F.modNdx $ ibags!(bagIndex + 1)
>     zone = fromGens (fromZone {zModulators =
>                                  if xmodi == ymodi
>                                    then Nothing
>                                    else Just (xmodi, ymodi)}) gens
>
> getGens                :: SFFile → F.Inst → [Word] → [F.Generator]
> getGens sffile iinst words
>   | traceIf msg False = undefined
>   | otherwise = gens
>   where
>     arrays = zArrays sffile
>     selected = map (doGenerator sffile iinst) words
>     filtered = filter isJust selected
>     gens = map fromJust filtered
>
>     msg = unwords ["getGens=", show words]
> 
> doGenerator :: SFFile → F.Inst → Word → Maybe F.Generator
> doGenerator sffile iinst zw              = Just $ ssIGens (zArrays sffile) ! zw
>
> fromGens               :: SFZone → [F.Generator] → SFZone
> fromGens iz []                           = iz
> fromGens iz (g:gs)                       = fromGens iz' gs
>   where iz' = addGen iz g
>   
> addGen                 :: SFZone → F.Generator → SFZone
> addGen iz gen =
>   case gen of
>   F.StartAddressOffset i         → iz {zStartOffs =                Just i}
>   F.EndAddressOffset i           → iz {zEndOffs =                  Just i}
>   F.LoopStartAddressOffset i     → iz {zStartOffs =                Just i}
>   F.LoopEndAddressOffset i       → iz {zEndOffs =                  Just i}
>
>   F.StartAddressCoarseOffset i   → iz {zStartCoarseOffs =          Just i}
>   F.EndAddressCoarseOffset i     → iz {zEndCoarseOffs =            Just i}
>   F.LoopStartAddressCoarseOffset i
>                                  → iz {zLoopStartCoarseOffs =      Just i}
>   F.LoopEndAddressCoarseOffset i
>                                  → iz {zLoopEndCoarseOffs =        Just i}
>
>   F.InstIndex w                  → iz {zInstIndex =                Just w}
>   F.KeyRange a b                 → iz {zKeyRange =                 Just (fromIntegral a, fromIntegral b)}
>   F.VelRange a b                 → iz {zVelRange =                 Just (fromIntegral a, fromIntegral b)}
>   F.Key i                        → iz {zKey =                      Just i}
>   F.Vel i                        → iz {zVel =                      Just i}
>   F.InitAtten i                  → iz {zInitAtten =                Just i}
>   F.CoarseTune i                 → iz {zCoarseTune =               Just i}
>   F.FineTune i                   → iz {zFineTune =                 Just i}
>   F.SampleIndex w                → iz {zSampleIndex =              Just w}
>   F.SampleMode a                 → iz {zSampleMode =               Just a}
>   F.ScaleTuning i                → iz {zScaleTuning =              Just i}
>   F.ExclusiveClass i             → iz {zExclusiveClass =           Just i}
>
>   F.DelayVolEnv i                → iz {zDelayVolEnv =              Just i}
>   F.AttackVolEnv i               → iz {zAttackVolEnv =             Just i}
>   F.HoldVolEnv i                 → iz {zHoldVolEnv =               Just i}
>   F.DecayVolEnv i                → iz {zDecayVolEnv =              Just i}
>   F.SustainVolEnv i              → iz {zSustainVolEnv =            Just i}
>   F.ReleaseVolEnv i              → iz {zReleaseVolEnv =            Just i}
>
>   F.Chorus i                     → iz {zChorus =                   Just i}
>   F.Reverb i                     → iz {zReverb =                   Just i}
>   F.Pan i                        → iz {zPan =                      Just i}
>
>   F.RootKey w                    → iz {zRootKey =                  Just (fromIntegral w)}
>
>   F.ModLfoToPitch i              → iz {zModLfoToPitch =            Just i}
>   F.VibLfoToPitch i              → iz {zVibLfoToPitch =            Just i}
>   F.ModEnvToPitch i              → iz {zModEnvToPitch =            Just i}
>   F.InitFc i                     → iz {zInitFc =                   Just i}
>   F.InitQ i                      → iz {zInitQ =                    Just i}
>   F.ModLfoToFc i                 → iz {zModLfoToFc =               Just i}
>   F.ModEnvToFc i                 → iz {zModEnvToFc =               Just i}
>   F.ModLfoToVol i                → iz {zModLfoToVol =              Just i}
>   F.DelayModLfo i                → iz {zDelayModLfo =              Just i}
>   F.FreqModLfo i                 → iz {zFreqModLfo =               Just i}
>   F.DelayVibLfo i                → iz {zDelayVibLfo =              Just i}
>   F.FreqVibLfo i                 → iz {zFreqVibLfo =               Just i}
>   F.DelayModEnv i                → iz {zDelayModEnv =              Just i}
>   F.AttackModEnv i               → iz {zAttackModEnv =             Just i}
>   F.HoldModEnv i                 → iz {zHoldModEnv =               Just i}
>   F.DecayModEnv i                → iz {zDecayModEnv =              Just i}
>   F.SustainModEnv i              → iz {zSustainModEnv =            Just i}
>   F.ReleaseModEnv i              → iz {zReleaseModEnv =            Just i}
>   F.KeyToModEnvHold i            → iz {zKeyToModEnvHold =          Just i}
>   F.KeyToModEnvDecay i           → iz {zKeyToModEnvDecay =         Just i}
>   F.KeyToVolEnvHold i            → iz {zKeyToVolEnvHold =          Just i}
>   F.KeyToVolEnvDecay i           → iz {zKeyToVolEnvDecay =         Just i}
>   _                              → iz
>
> sfscore                :: [Hints] → Int
> sfscore = foldr sfscorehint 0
>
> sfscorehints            :: Hints → Int
> sfscorehints h = case h of
>                  DLow            → -1
>                  DMed            → 0
>                  DHigh           → 1
>                  DScore x        → 0 
>             
> sfscorehint            :: Hints → Int →  Int
> sfscorehint hint accum = accum + sfscorehints hint   
>
> makeNameMap            :: forall a. () ⇒ Array Word a → (a → String) → NameMap
> makeNameMap as getString                 =
>   let
>     (ast, aen) = bounds as 
>     avs = map akv [ast..aen-1]
>   in
>     Map.fromList avs
>   where
>     akv                :: Word → (String, Word)
>     akv anum = (getString (as ! anum), anum)
> 
> makeNameMapI arrays                      = makeNameMap (ssInsts arrays) F.instName
> makeNameMapP arrays                      = makeNameMap (ssShdrs arrays) F.sampleName
>     
> makeNameMaps           :: SoundFontArrays → NameMaps
> makeNameMaps arrays = (makeNameMapI arrays, makeNameMapP arrays)

prepare the specified instruments and percussion ======================================================================

> cacheZones             :: [SFFile] → IO ZoneCache
> cacheZones sffiles                       = do
>   return $ Map.fromList $ concatMap cacheF sffiles
>
>   where
>     cacheF             :: SFFile → [(PerGMKey, [(Word, SFZone)])]
>     cacheF sffile                        = concatMap (cacheI sffile) [fst boundsI..snd boundsI-1]
>       where
>         arrays = zArrays sffile
>         boundsI = bounds (ssInsts arrays)
>
>     cacheI             :: SFFile → Word → [(PerGMKey, [(Word, SFZone)])]
>     cacheI sffile wI                     = [(pergm, zs)]
>       where
>         wF             :: Word           = zWordF sffile
>         pergm          :: PerGMKey       = PerGMKey wF wI Nothing 
>         zs             :: [(Word, SFZone)]
>                                          = getZones sffile (wF, wI)
>        
>     getZones           :: SFFile → (Word, Word) → [(Word, SFZone)]
>     getZones sffile (zF, zI)
>       | traceIf msg False = undefined
>       | otherwise = gList ++ oList
>       where
>         arrays = zArrays sffile
>         iinst  = ssInsts arrays ! zI
>         jinst  = ssInsts arrays ! (zI+1)
>         ibagi  = F.instBagNdx iinst
>         jbagi  = F.instBagNdx jinst
>         gIx    = if jbagi - ibagi < 2
>                  then error ("must have one global zone and at least one other zone"
>                              ++ " (" ++ show (zF, zI) ++ ")")
>                  else singleton ibagi
>         oIx    = [ibagi+1..jbagi-1]
>         gList  = map (buildZone sffile iinst defInstrumentZone)   gIx
>         oList  = map (buildZone sffile iinst ((snd.head) gList))  oIx
>         msg = unwords ["getZones=", show (zF, zI) ] -- show $ length gList, " ", show $ length oList
>
> getZonesFromCache      :: ZoneCache → PerGMKey → [(Word, SFZone)]
> getZonesFromCache zc pergm               = fromJust $ Map.lookup pergm zc
>
> assignInstruments      :: SFRoster
>                           → InstrLocator
>                           → [(InstrumentName, Instr (Stereo AudRate))]
> assignInstruments sfrost                 = Map.foldrWithKey (ipal2imap sfrost) []
>
> ipal2imap              :: SFRoster
>                           → InstrumentName
>                           → PerGMScored
>                           → [(InstrumentName, Instr (Stereo AudRate))]
>                           → [(InstrumentName, Instr (Stereo AudRate))]
> ipal2imap sfrost iname pergm accum       = (iname, assignInstrument sfrost pergm') : accum
>   where
>     pergm' = pPerGMKey pergm
>
> assignAllPercussion    :: SFRoster
>                           → PercLocator
>                           → [(PercussionSound, (Word, Word))]
> assignAllPercussion sfrost               = Map.foldrWithKey (ppal2pmap sfrost) []
>
> ppal2pmap              :: SFRoster
>                           → PercussionSound
>                           → PerGMScored
>                           → [(PercussionSound, (Word, Word))]
>                           → [(PercussionSound, (Word, Word))]
> ppal2pmap sfrost psound pergm accum      = (psound, (pWordF pergm', pWordI pergm')) : accum
>   where
>     pergm' = pPerGMKey pergm
>
> doAssignP              :: SFRoster
>                           → [(PercussionSound, (Word, Word))]
>                           → (InstrumentName, Instr (Stereo AudRate))
> doAssignP sfrost pmap                    = (Percussion, assignPercussion sfrost pmap)

define signal functions for playing instruments =======================================================================

> assignInstrument       :: SFRoster → PerGMKey → Instr (Stereo AudRate)
> assignInstrument sfrost pergm dur pch vol params
>                                          =
>   let
>     sig                :: AudSF () (Double, Double)
>                                          = constructSig sfrost
>                                                         (pWordF pergm, pWordI pergm)
>                                                         dur pch vol params
>   in proc _ → do
>     (zL, zR) ← sig ⤙ ()
>     outA ⤙ (zL, zR)
>
> constructSig           :: SFRoster
>                           → (Word, Word)
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → AudSF () (Double, Double)
> constructSig sfrost (wF, wI) dur pch vol params
>   | traceIf msg False = undefined
>   | otherwise                            =
>   let
>     zc                 :: ZoneCache      = zZoneCache sfrost
>     zones              :: [(Word, SFZone)]
>                                          = tail $ getZonesFromCache zc (PerGMKey wF wI Nothing)
>     ((zoneL, shdrL), (zoneR, shdrR))
>                        :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                                          = setZone sfrost (wF, wI) zones pch vol
>     (reconL, reconR)   :: (Reconciled, Reconciled)
>                                          = reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>     pch'               :: AbsPitch       = fromMaybe pch (rForceKey reconL)
>     vol'               :: Volume         = fromMaybe vol (rForceVel reconL)
>     sr                 :: Double         = fromIntegral $ F.sampleRate shdrL
>     sig                :: AudSF () (Double, Double)
>     ok                 :: Bool           = checkReconcile
>                                              ((zoneL, shdrL), (zoneR, shdrR))
>                                              reconL
>                                              reconR
>     sffile             :: SFFile         = fileByIndex sfrost wF
>     arrays             :: SoundFontArrays= zArrays sffile 
>     sig = if not ok
>           then error "SFZone and F.Shdr could not be reconciled"
>           else eutSynthesize (reconL, reconR) sr dur pch' vol' params
>                              (ssData arrays) (ssM24 arrays)
>   in sig
>   where
>     msg = unwords ["constructSig ", show (wF, wI)]
>
> assignPercussion       :: SFRoster
>                           → [(PercussionSound, (Word, Word))]
>                           → Instr (Stereo AudRate)
> assignPercussion sfrost pmap dur pch vol params
>                                          =
>   let
>     ps                 :: PercussionSound
>                                          = toEnum (pch - 35)
>     (wF, wI) = case lookup ps pmap of
>                Nothing   → error (   "Percussion does not have "
>                                      ++ show ps ++ " in the supplied pmap.")
>                Just x    → x
>     sig                :: AudSF () (Double, Double)
>                                          = constructSig sfrost (wF, wI) dur pch vol params
>   in proc _ → do
>     (zL, zR) ← sig ⤙ ()
>     outA ⤙ (zL, zR)

zone selection ========================================================================================================

> setZone                :: SFRoster
>                           → (Word, Word)
>                           → [(Word, SFZone)]
>                           → AbsPitch
>                           → Volume
>                           → ((SFZone, F.Shdr), (SFZone, F.Shdr))
> setZone sfrost (wF, wI) zones pch vol
>   | traceIf msg False                    = undefined
>   | otherwise                            = ((zoneL, sampleL) ,(zoneR, sampleR))
>   where
>     zone = selectBestZone sfrost wF zones pch vol
>     (zoneL, zoneR) = selectZonePair sfrost (wF, wI) zones zone
>     sffile = fileByIndex sfrost wF
>     arrays = zArrays sffile
>     iinst = ssInsts arrays ! wI
>     sinL = fromJust $ zSampleIndex zoneL
>     sinR = fromJust $ zSampleIndex zoneR
>     sampleL = ssShdrs arrays ! sinL
>     sampleR = ssShdrs arrays ! sinR
>     msg = unwords [
>             "setZone ( wF, wI = ", show (wF, wI)
>             , ", sinL, sinR = ", show (sinL, sinR)
>             , " , ",     show (zoneName sffile iinst zoneL)
>             , ")"]
>
> selectBestZone         :: SFRoster
>                           → Word
>                           → [(Word, SFZone)]
>                           → AbsPitch
>                           → Volume
>                           → SFZone
> selectBestZone sfrost zF zones pch vol   =
>   let
>     sffile = fileByIndex sfrost zF
>     scores = mapMaybe (scoreOneZone sffile pch vol) zones
>     whichZ = if null scores
>              then error "no qualified zone"
>              else minimumBy compareScores scores
>   in
>     snd whichZ
>
> selectZonePair         :: SFRoster
>                           → (Word, Word)
>                           → [(Word, SFZone)]
>                           → SFZone
>                           → (SFZone, SFZone)
> selectZonePair sfrost (zF, zI) zones zone
>                                          = (zoneL, zoneR)
>   where
>     sffile = fileByIndex sfrost zF
>     arrays = zArrays sffile 
>     shdr = ssShdrs arrays ! fromJust (zSampleIndex zone)
>     stype = F.sampleType shdr
>     slink = F.sampleLink shdr
>     mlinked = find (withSlink slink) zones
>     -- TODO: should base it strictly on Stereo sample type?
>     ozone = case mlinked of
>             Nothing → zone
>             Just _  → snd $ fromJust $ find (withSlink slink) zones
>     (zoneL, zoneR) = if toSampleType stype == SampleTypeLeft
>                      then (zone, ozone)
>                      else (ozone, zone)
>     withSlink          :: Word → (Word, SFZone) → Bool
>     withSlink toMatch (wZ, zone)         = fromJust (zSampleIndex zone) == toMatch
>
> compareScores          :: (Ord a) ⇒ (a, b) → (a, b) → Ordering
> compareScores (a1, b1) (a2, b2)          = compare a1 a2 
>                         
> scoreOneZone           :: SFFile
>                           → AbsPitch
>                           → Volume
>                           → (Word, SFZone)
>                           → Maybe (Int, SFZone)
> scoreOneZone sffile pch vol dzone        =
>     if qualify zone
>     then Just (score, zone)
>     else Nothing
>   where
>     qualify            :: SFZone → Bool
>     qualify z = DAllOn /= desireReStereo || isStereoZone (zArrays sffile) z
>
>     score = score1 + score2
>     zone = snd dzone
>
>     score1 = computePitchDistance pch (zKeyRange zone)
>     score2 = computeVolumeDistance vol (zVelRange zone)
>
> computePitchDistance   :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> computePitchDistance cand mrange         =
>   case mrange of
>     Nothing                   → 1000
>     Just (rangeMin, rangeMax) → let
>                                   dist1 = abs $ cand - rangeMin
>                                   dist2 = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 100 * max dist1 dist2
>                                   else 1000 * min dist1 dist2
>
> computeVolumeDistance  :: (Num a, Ord a) ⇒ a → Maybe (a, a) → a
> computeVolumeDistance cand mrange        =
>   case mrange of
>     Nothing                   → 100
>     Just (rangeMin, rangeMax) → let
>                                   dist1 = abs $ cand - rangeMin
>                                   dist2 = abs $ cand - rangeMax
>                                 in
>                                   if cand >= rangeMin && cand <= rangeMax
>                                   then 10 * max dist1 dist2
>                                   else 100 * min dist1 dist2

reconcile zone and sample header ======================================================================================

> sumOfMaybeInts         :: [Maybe Int] → Int
> sumOfMaybeInts                           = foldr ((+).fromMaybe 0) 0
>       
> addIntToWord           :: Word → Int → Word
> addIntToWord w i                         = fromIntegral sum
>   where
>     iw               :: Int              = fromIntegral w
>     sum              :: Int              = iw + i
>
> reconcileLR            :: ((SFZone, F.Shdr), (SFZone, F.Shdr)) → (Reconciled, Reconciled)
> reconcileLR ((zoneL, shdrL), (zoneR, shdrR))
>   | traceIf msg False                    = undefined
>   | otherwise                            =
>   let
>     recL = reconcile (zoneL, shdrL)
>     recR = reconcile (zoneR, shdrR)
>     recR' = recR{
>               rRootKey         = rRootKey recL
>             , rPitchCorrection = rPitchCorrection recL} 
>   in
>     (recL, recR')
>   where
>     msg = unwords ["reconcileLR zoneL=", show zoneL
>                  , ", shdrL=",           show shdrL
>                  , ", zoneR=",           show zoneR
>                  , ", shdrR=",           show shdrR]
>
> reconcile              :: (SFZone, F.Shdr) → Reconciled 
> reconcile (zone, shdr)                   =
>   Reconciled {
>     rSampleMode      = fromMaybe             A.NoLoop                 (zSampleMode    zone)
>   , rStart           = addIntToWord          (F.start shdr)           (sumOfMaybeInts [zStartOffs     zone, zStartCoarseOffs     zone])
>   , rEnd             = addIntToWord          (F.end shdr)             (sumOfMaybeInts [zEndOffs       zone, zEndCoarseOffs       zone])
>   , rLoopStart       = addIntToWord          (F.startLoop shdr)       (sumOfMaybeInts [zLoopStartOffs zone, zLoopStartCoarseOffs zone])
>   , rLoopEnd         = addIntToWord          (F.endLoop shdr)         (sumOfMaybeInts [zLoopEndOffs   zone, zLoopEndCoarseOffs   zone])
>   , rRootKey         = fromMaybe             (F.originalPitch shdr)   (zRootKey       zone)
>   , rPitchCorrection = resolvePitchCorrection(F.pitchCorrection shdr) (zCoarseTune    zone) (zFineTune zone)
>   , rForceKey        = fmap                  fromIntegral             (zKey           zone)
>   , rForceVel        = fmap                  fromIntegral             (zVel           zone)
>   , rAttenuation     = resolveAttenuation                             (zInitAtten     zone)
>   , rEnvelope        = deriveEnvelope        (zDelayVolEnv   zone)
>                                              (zAttackVolEnv  zone)
>                                              (zHoldVolEnv    zone)
>                                              (zDecayVolEnv   zone)
>                                              (zSustainVolEnv zone)
>                                              (zReleaseVolEnv zone)
>   , rEffects         = deriveEffects         (zChorus zone)
>                                              (zReverb zone)
>                                              (zPan    zone)}
>
> checkReconcile         :: ((SFZone, F.Shdr), (SFZone, F.Shdr))
>                           → Reconciled
>                           → Reconciled
>                           → Bool
> checkReconcile ((zoneL, shdrL), (zoneR, shdrR)) reconL reconR
>   | traceIf msg False                    = undefined
>   | otherwise                            = True
>   where
>     msg = unwords ["checkReconcile=", show shdrL
>                                     , show zoneL
>                                     , show reconL]

emit text indicating what choices we made for GM items ================================================================

> printChoices           :: SFRoster → [InstrumentName] → [PercussionSound] → ([(Bool, String)], [(Bool, String)])
> printChoices sfrost is ps                = (map (showI sfrost) is, map (showP sfrost) ps)
>   where
>     showI              :: SFRoster → InstrumentName → (Bool, String)
>     showI sfrost iname                   = result
>       where
>         (iloc, _)                         = zLocs sfrost
>         mpergm                            = Map.lookup iname iloc
>         result
>           | isJust mpergm                 = (True, show iname ++ "/" ++ showPerGM sfrost (fromJust mpergm))
>           | iname == Percussion           = (True, show iname ++ "(pseudo-instrument)")
>           | otherwise                     = (False, show iname ++ " not found")
>
>     showP               :: SFRoster → PercussionSound → (Bool, String)
>     showP sfrost psound                  = result
>       where
>         (_, ploc) = zLocs sfrost
>         mpergm = Map.lookup psound ploc
>         result = if isJust mpergm
>                    then (True, show psound ++ "/" ++ showPerGM sfrost (fromJust mpergm))
>                    else (False, show psound ++ " not found")
>
> showPerGM              :: SFRoster → PerGMScored → String
> showPerGM sfrost pergm'                  =
>   let
>     pergm = pPerGMKey pergm'
>     wFile = pWordF pergm
>     wInst = pWordI pergm
>     wZone = pWordZ pergm
>
>     sffile = zFiles sfrost ! fromIntegral wFile
>     arrays = zArrays sffile
>     iinst = ssInsts arrays ! fromIntegral wInst
>     ishdr = ssShdrs arrays ! fromIntegral (fromJust wZone)
>     showZ = case wZone of
>             Nothing     → ""
>             Just zWord  → "/" ++ F.sampleName ishdr
>   in
>        show (pWordF pergm)
>     ++ "/"
>     ++ instrName sffile iinst
>     ++ showZ
>     ++ " (s="
>     ++ show (pStaticScore pergm')
>     ++ ")"
>

mine instrument/percussion candidates (sampled sounds) from SoundFont (*.sf2) files ===================================

> spillRosters           :: ZoneCache → [SFFile] → FilePath → IO ()
> spillRosters zc sffilesp outFile         = do
>   putStrLn ("spillRosters " ++ show (length zc))
>   appendFile outFile prolog
>   mapM_ (spillF zc outFile) sffilesp
>   appendFile outFile epilog
>   where
>     flist              :: String         = concat $ zipWith reformat [0..] (map format sffilesp)
>     prolog =     "> {-# LANGUAGE ScopedTypeVariables #-}\n"
>                ++ "> {-# LANGUAGE UnicodeSyntax #-}\n"
>                ++ ">\n"
>                ++ "> module SyntheticRosters where\n"
>                ++ ">\n"
>                ++ "> import Euterpea.Music\n"
>                ++ "> import SoundFont\n"
>                ++ "\nThis file was generated from the SoundFont files.\n\n"
>                ++ "> soundFontDatabase      :: [SoundFontInit]\n"
>                ++ "> soundFontDatabase =\n"
>                ++ ">   [\n"
>                ++ flist
>                ++ ">   ]\n>\n"
>     format             :: SFFile → String
>     format sffile                        =
>       let
>         nick           :: String         = zNickname sffile
>         ilistname      :: String         = nick ++ "Inst"
>         plistname      :: String         = nick ++ "Perc"
>       in
>         "SoundFontInit "
>         ++ "\"" ++ zFilename sffile ++ "\" "
>         ++ "\"" ++ nick ++ "\" "
>         ++ ilistname ++ " " ++ plistname ++ "\n"
>     reformat           :: Int → String → String
>     reformat which iline = iline'
>       where
>         iline' = (if which == 0
>                  then ">       "
>                  else ">     , ")
>                  ++ iline
>     epilog =      ">\n"
>
> spillF                 :: ZoneCache → FilePath → SFFile → IO ()
> spillF zc outFile sffile
>   | traceIf msg False                    = undefined
>   | otherwise                            = do
>     spillI zc outFile sffile
>     spillP zc outFile sffile
>   where
>     msg = unwords ["spillF " ++ zFilename sffile]
>
> spillI                 :: ZoneCache → FilePath → SFFile → IO ()
> spillI _ outFile sffile                  = do
>     appendFile outFile prolog
>     appendFile outFile $ concat $ emitData $ specifyInstruments $ mapMaybe qualifyI inps
>     appendFile outFile epilog
>   where
>     prolog                               = "> "       ++ zNickname sffile ++ "Inst =\n>   [\n"
>     epilog                               = ">   ]\n"
>
>     insts                                = ssInsts $ zArrays sffile
>     shdrs                                = ssShdrs $ zArrays sffile
>     boundsI                              = bounds insts
>
>     inps               :: [String]       = map (\wI → quoteCodeString (F.instName (insts ! wI)))
>                                                [fst boundsI..snd boundsI-1]
>
>     qualifyI           :: String → Maybe (InstrumentName, String)
>     qualifyI inp                         = findMatchingInstrument' inp ffThreshold 
>
> spillP                 :: ZoneCache → FilePath → SFFile → IO ()
> spillP zc outFile sffile                 = do
>     appendFile outFile prolog
>     appendFile outFile $ concat $ emitData $ specifyPercussion eable
>     appendFile outFile epilog
>   where
>     prolog                               = "> " ++ zNickname sffile ++ "Perc =\n>   [\n"
>     epilog                               = ">   ]\n"
>
>     insts                                = ssInsts $ zArrays sffile
>     shdrs                                = ssShdrs $ zArrays sffile
>     boundsI                              = bounds insts
>
>     getInputI          :: Word → String
>     getInputI wI                         = quoteCodeString $ F.instName (insts ! wI)
>
>     getInputP          :: Word → String
>     getInputP sin                        = quoteCodeString $ F.sampleName (shdrs ! sin)
>
>     pergms                               = map (\i → PerGMKey (zWordF sffile) i Nothing )
>                                                [fst boundsI..snd boundsI-1]
>     seeds                                = map (\p → (tail $ getZonesFromCache zc p, pWordI p))
>                                                pergms
>     words                                = map (BF.first (map (fromJust . zSampleIndex . snd)))               
>                                                seeds
>     idents                               = map (BF.bimap (map getInputP) getInputI)
>                                                words
>     matched                              = map (BF.first (mapMaybe (`findMatchingPercussion'` ffThreshold)))
>                                                idents
>     eable                                = filter (not . (null . fst))
>                                                matched

Table-driven emission → SyntheticRosters ==============================================================================

> data Emission =
>   StringToField String Int
>   | EndOfLine deriving Show
>
> emitData               :: [Emission] → [String]
> emitData es
>   | traceIf msg False                    = undefined
>   | otherwise                            = foldl' emitItem [""] es
>   where
>     msg = unwords ["emitData=", show es]
>
> emitItem               :: [String] → Emission → [String]
> emitItem outP e = outP'
>   where
>     outP' = init outP ++ [last outP ++ makeString e]
>
> makeString             :: Emission → String
> makeString e = case e of
>                StringToField str sz → str ++ replicate (sz - length str) ' '
>                EndOfLine            → "\n"
>
> specifyInstruments     :: [(InstrumentName, String)] → [Emission]
> specifyInstruments ipairs                = concat $ zipWith specifyOneI [0..] ipairs
>   where
>     specifyOneI         :: Int → (InstrumentName, String) → [Emission]
>     specifyOneI nthI (iname, ident)      = esL : es0 : es1 : es2 : es3 : [es4]
>       where
>         esL = StringToField "> " 2
>         es0 = StringToField "" 4
>         es1 = if nthI == 0
>               then StringToField "" 2
>               else StringToField "," 2
>         ifield = "(" ++ show ident ++ ","
>         es2 = StringToField ifield 25
>         jfield = "([],  " ++ show iname ++ "))"
>         es3 = StringToField jfield 35
>         es4 = EndOfLine
>
> specifyPercussion      :: [([(PercussionSound, String)], String)] → [Emission]
> specifyPercussion ipairs                 = concat $ zipWith specifyOneI [0..] ipairs
>   where
>     specifyOneI         :: Int → ([(PercussionSound, String)], String) → [Emission]
>     specifyOneI nthI (ipairs, istring)   = concat $ zipWith (specifyOneP nthI istring (length ipairs - 1) )
>                                                             [0..]
>                                                             ipairs
>
>     specifyOneP         :: Int → String → Int → Int → (PercussionSound, String) → [Emission]
>     specifyOneP nthI istring top nthP (psound, pstring)  
>                                          = 
>       if nthP == 0
>       then withI nthI istring (psound, pstring) top nthP
>       else withoutI (psound, pstring) top nthP 
>
>     withI               :: Int → String → (PercussionSound, String) → Int → Int → [Emission]
>     withI nthI istring (psound, pstring) top nthP = esL : es0 : es1 : es2 : es3 : es4 : [es5]
>       where
>         esL = StringToField "> " 2
>         es0 = StringToField "" 4
>         es1 = if nthI == 0
>               then StringToField "" 2
>               else StringToField "," 2
>         ifield = "(" ++ show istring ++ ","
>         es2 = StringToField ifield 25
>         jfield = "[ (" ++ show pstring ++ ","
>         es3 = StringToField jfield 27
>         kfield = "([], " ++ show psound ++ "))" ++ if nthP == top then "])" else ""
>         es4 = StringToField kfield 27
>         es5 = EndOfLine
>
>     withoutI           :: (PercussionSound, String) → Int → Int → [Emission]
>     withoutI (psound, pstring) top nthP = esL : es0 : es1 : es2 : [es3]
>       where
>         esL = StringToField "> " 2
>         es0 = StringToField "" 31
>         jfield = ", (" ++ show pstring ++ ","
>         es1 = StringToField jfield 27
>         kfield = "([], " ++ show psound ++ "))" ++ if nthP == top then "])" else ""
>         es2 = StringToField kfield 27
>         es3 = EndOfLine
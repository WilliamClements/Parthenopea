> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
June 16, 2025

> module Eng.Command (
>         batchProcessor ) where
>
> import qualified Codec.SoundFont         as F
> import Control.Lens hiding ( element )
> import qualified Control.Monad           as CM
> import Data.IntSet ( IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.List
> import Data.Maybe
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec
> import GHC.Ix
> import qualified System.FilePattern.Directory
>                                          as FP
  
The Generator types are numbered 0 to 60. =============================================================================

> data GenEnum                             =
>     StartAddressOffset | EndAddressOffset | LoopStartAddressOffset | LoopEndAddressOffset
>   | StartAddressCoarseOffset | ModLfoToPitch | VibLfoToPitch | ModEnvToPitch | InitFc | InitQ
>   | ModLfoToFc | ModEnvToFc | EndAddressCoarseOffset | ModLfoToVol | Unused1 | Chorus | Reverb
>   | Pan | Unused2 | Unused3 | Unused4 | DelayModLfo | FreqModLfo | DelayVibLfo | FreqVibLfo
>   | DelayModEnv | AttackModEnv | HoldModEnv | DecayModEnv | SustainModEnv | ReleaseModEnv
>   | KeyToModEnvHold | KeyToModEnvDecay | DelayVolEnv | AttackVolEnv | HoldVolEnv | DecayVolEnv
>   | SustainVolEnv | ReleaseVolEnv | KeyToVolEnvHold | KeyToVolEnvDecay | InstIndex | Reserved1
>   | KeyRange | VelRange | LoopStartAddressCoarseOffset | Key | Vel | InitAtten | Reserved2
>   | LoopEndAddressCoarseOffset | CoarseTune | FineTune | SampleIndex | SampleMode
>   | Reserved3 | ScaleTuning | ExclusiveClass | RootKey | Unused5 | ReservedGen
>   deriving (Enum, Eq, Show)

The per-file telemetry data includes:
1. vector of 61 per-generator infos - each with
   a. occurrence count
   b. out-of-range values encountered, if any
2. envelope statistics summed up over all instruments in the file

We list out both sets of data for each file, then combine them across files, and list out the rollup.

> data GenData                             =
>   GenData {
>       _genEnum         :: GenEnum
>     , _genRange        :: Maybe (Int, Int)
>     , _genOccur        :: Int
>     , _genOutOfRange   :: IntSet}
>   deriving (Eq, Show)
> makeGenData            :: GenEnum → GenData
> makeGenData ge                           =
>   let
>     mclip                                = allClipVector VB.! fromEnum ge
>   in
>     GenData ge mclip  0 IntSet.empty
> addTwoGenDatas         :: GenData → GenData → GenData
> addTwoGenDatas (GenData ge1 mclip1 k1 c1) (GenData _ _ k2 c2)
>                                          =
>   GenData
>     ge1
>     mclip1 
>     (k1 + k2)
>     (c1 `IntSet.union` c2)
>
> data EnvData                             =
>   EnvData {
>     edPlace            :: String
>   , edTime             :: String}
>   deriving (Eq, Show)
> defEnvData            :: EnvData
> defEnvData                             = EnvData "" ""
>
> data GenSum                              =
>   GenSum {
>     _gsFilename        :: FilePath
>   , _gsGenData         :: VB.Vector GenData
>   , _gsEnvData         :: EnvData}
>   deriving (Eq, Show)
> defGenSum             :: GenSum
> defGenSum                              = GenSum "" VB.empty defEnvData
> makeGenSum             :: FilePath → VB.Vector GenData → GenSum
> makeGenSum filename genData =
>   GenSum filename genData defEnvData
> addTwoGenSums          :: GenSum → GenSum → GenSum
> addTwoGenSums (GenSum _ gd1 ed1) (GenSum _ gd2 ed2)     -- WOX must combine ed2 with ed1
>                                          =
>   GenSum "<rollup" (VB.zipWith addTwoGenDatas gd1 gd2) ed1
>
> openSoundFontFile      :: Int → FilePath → IO SFFileBoot
> openSoundFontFile wFile filename         = do
>   putStrLn filename
>   result                                 ← F.importFile filename
>   case result of
>     Left s                               →
>       error $ unwords ["openSoundFontFile", "decoding error", s, show filename]
>     Right soundFont                      → do
>       let pdata                          = F.pdta soundFont
>       let sdata                          = F.sdta soundFont
>       let boota                          =
>             FileArrays
>               (F.insts pdata) (F.ibags pdata)
>               (F.igens pdata) (F.imods pdata)
>               (F.shdrs pdata)
>       let samplea                        = SampleArrays (F.smpl  sdata) (F.sm24  sdata)
>       return $ SFFileBoot 
>                  wFile 
>                  filename 
>                  boota 
>                  samplea
>
> makeLenses ''GenData
> makeLenses ''GenSum
>
> batchProcessor         :: IO ()
> batchProcessor                           = do
>   timeThen                               ← getZonedTime
>   print timeThen
>   sf2s                                   ← FP.getDirectoryFilesIgnoreSlow "." ["*.sf2"] []
>
>   proceed sf2s
>
>   timeNow                                ← getZonedTime
>   print timeNow
>   putStrLn "\nThe End"
>
> proceed                :: [FilePath] → IO ()
> proceed sf2s                             = do
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFilesBoot                         = VB.fromList extraction
>   if VB.null vFilesBoot
>     then return ()
>     else do
>       fData                              ← openInvestigation vFilesBoot
>       putStrLn $ unwords ["check 61 =", show (VB.length fData)]
>       return ()
>
> shredOneFile           :: SFFileBoot → IO GenSum
> shredOneFile sffile                      =
>   return $ makeGenSum sffile.zFilename vGenData'
>   where
>     vGenData           :: VB.Vector GenData
>     vGenData                             = VB.generate 61 (makeGenData . toEnum)
>     vGenData'          :: VB.Vector GenData
>     vGenData'                            = foldl' shredOne vGenData sffile.zFileArrays.ssIGens
>
>     upd                :: VB.Vector GenData → GenEnum → Maybe Int → VB.Vector GenData
>     upd is ge val                        = 
>       let
>         i                                = fromEnum ge
>         genData                          = is VB.! i
>         mclip                            = allClipVector VB.! i
>         inrange                          = maybe True probe mclip
>           where
>             probe      :: (Int, Int) → Bool
>             probe clip = inRange clip (fromJust val)
>         collect k                    = IntSet.insert k (genData ^. genOutOfRange)
>         outOfRange'                  = if inrange
>                                          then genData ^. genOutOfRange
>                                          else collect $ fromJust val
>         fileData'                    = ((genOccur +~ 1) . (genOutOfRange .~ outOfRange')) genData
>       in
>         VB.update is $ VB.singleton (i, fileData')
>
>     shredOne       :: VB.Vector GenData → F.Generator → VB.Vector GenData
>         
>     -- 0..4
>     shredOne is (F.StartAddressOffset _)
>                                          = upd is StartAddressOffset Nothing
>     shredOne is (F.EndAddressOffset _)
>                                          = upd is EndAddressOffset Nothing
>     shredOne is (F.LoopStartAddressOffset _)
>                                          = upd is LoopStartAddressOffset Nothing
>     shredOne is (F.LoopEndAddressOffset _)
>                                          = upd is LoopEndAddressOffset Nothing
>     shredOne is (F.StartAddressCoarseOffset _)
>                                          = upd is StartAddressCoarseOffset Nothing
>         
>         -- 5..9
>     shredOne is (F.ModLfoToPitch val)
>                                          = upd is ModLfoToPitch (Just val)
>     shredOne is (F.VibLfoToPitch val)
>                                          = upd is VibLfoToPitch (Just val)
>     shredOne is (F.ModEnvToPitch val)
>                                          = upd is ModEnvToPitch (Just val)
>     shredOne is (F.InitFc val)
>                                          = upd is InitFc (Just val)
>     shredOne is (F.InitQ val)
>                                          = upd is InitQ (Just val)
>         
>         -- 10..13
>     shredOne is (F.ModLfoToFc val)
>                                          = upd is ModLfoToFc (Just val)
>     shredOne is (F.ModEnvToFc val)
>                                          = upd is ModEnvToFc (Just val)
>     shredOne is (F.EndAddressCoarseOffset _)
>                                          = upd is EndAddressCoarseOffset Nothing
>     shredOne is (F.ModLfoToVol val)
>                                          = upd is ModLfoToVol (Just val)
>         
>         -- 15..17
>     shredOne is (F.Chorus val)
>                                          = upd is Chorus (Just val)
>     shredOne is (F.Reverb val)
>                                          = upd is Reverb (Just val)
>     shredOne is (F.Pan val)
>                                          = upd is Pan (Just val)
>         
>         -- 21..24
>     shredOne is (F.DelayModLfo val)
>                                          = upd is DelayModLfo (Just val)
>     shredOne is (F.FreqModLfo val)
>                                          = upd is FreqModLfo (Just val)
>     shredOne is (F.DelayVibLfo val)
>                                          = upd is DelayVibLfo (Just val)
>     shredOne is (F.FreqVibLfo val)
>                                          = upd is FreqVibLfo (Just val)
>         
>         -- 25..30
>     shredOne is (F.DelayModEnv val)
>                                          = upd is DelayModEnv (Just val)
>     shredOne is (F.AttackModEnv val)
>                                          = upd is AttackModEnv (Just val)
>     shredOne is (F.HoldModEnv val)
>                                          = upd is HoldModEnv (Just val)
>     shredOne is (F.DecayModEnv val)
>                                          = upd is DecayModEnv (Just val)
>     shredOne is (F.SustainModEnv val)
>                                          = upd is SustainModEnv (Just val)
>     shredOne is (F.ReleaseModEnv val)
>                                          = upd is ReleaseModEnv (Just val)
>         
>         -- 31..41
>     shredOne is (F.KeyToModEnvHold val)
>                                          = upd is KeyToModEnvHold (Just val)
>     shredOne is (F.KeyToModEnvDecay val)
>                                          = upd is KeyToModEnvDecay (Just val)
>     shredOne is (F.DelayVolEnv val)
>                                          = upd is DelayVolEnv (Just val)
>     shredOne is (F.AttackVolEnv val)
>                                          = upd is AttackVolEnv (Just val)
>     shredOne is (F.HoldVolEnv val)
>                                          = upd is HoldVolEnv (Just val)
>     shredOne is (F.DecayVolEnv val)
>                                          = upd is DecayVolEnv (Just val)
>     shredOne is (F.SustainVolEnv val)
>                                          = upd is SustainVolEnv (Just val)
>     shredOne is (F.ReleaseVolEnv val)
>                                          = upd is ReleaseVolEnv (Just val)
>     shredOne is (F.KeyToVolEnvHold val)
>                                          = upd is KeyToVolEnvHold (Just val)
>     shredOne is (F.KeyToVolEnvDecay val)
>                                          = upd is KeyToVolEnvDecay (Just val)
>     shredOne is (F.InstIndex _)
>                                          = upd is InstIndex Nothing
>         
>         -- 43..48
>     shredOne is (F.KeyRange _ _)
>                                          = upd is KeyRange Nothing
>     shredOne is (F.VelRange _ _)
>                                          = upd is VelRange Nothing
>     shredOne is (F.LoopStartAddressCoarseOffset _)
>                                          = upd is LoopStartAddressCoarseOffset Nothing
>     shredOne is (F.Key val)
>                                          = upd is Key ((Just . fromIntegral) val)
>     shredOne is (F.Vel val)
>                                          = upd is Vel ((Just . fromIntegral) val)
>     shredOne is (F.InitAtten val)
>                                          = upd is InitAtten (Just val)
>         
>         -- 50..54
>     shredOne is (F.LoopEndAddressCoarseOffset _)
>                                          = upd is LoopEndAddressCoarseOffset Nothing
>     shredOne is (F.CoarseTune val)
>                                          = upd is CoarseTune (Just val)
>     shredOne is (F.FineTune val)
>                                          = upd is FineTune (Just val)
>     shredOne is (F.SampleIndex _)
>                                          = upd is SampleIndex Nothing
>     shredOne is (F.SampleMode _)
>                                          = upd is SampleMode Nothing
>         
>         -- 56..58
>     shredOne is (F.ScaleTuning val)
>                                          = upd is ScaleTuning (Just val)
>     shredOne is (F.ExclusiveClass val)
>                                          = upd is ExclusiveClass (Just val)
>     shredOne is (F.RootKey val)
>                                          = upd is RootKey ((Just . fromIntegral) val)
>         
>         -- 60
>     shredOne is (F.ReservedGen _ _)
>                                          = upd is ReservedGen Nothing
>
> openInvestigation      :: VB.Vector SFFileBoot → IO (VB.Vector GenSum)
> openInvestigation vFilesBoot             = do
>   vGenSum                                ← CM.mapM shredOneFile vFilesBoot
>   mapM_ showOneFile vGenSum
>
>   let vData                              = VB.foldl' addTwoGenSums defGenSum vGenSum
>   showOneFile vData
>   return vGenSum
>   where
>     showOneFile        :: GenSum → IO ()
>     showOneFile sData                      = do
>       putStrLn (sData ^. gsFilename)
>       mapM_ print (sData ^. gsGenData)

The End
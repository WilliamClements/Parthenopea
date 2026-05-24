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

The per-file diagnostic data includes:
1. vector of 61 per-generator infos - each with
   a. occurrence count
   b. out-of-range values encountered, if any
2. envelope statistics summed up over all instruments in the file

We list out both sets of data for each file, then list out the rollup sets.

> data GenData                             =
>   GenData {
>     _genEnum           :: GenEnum
>   , _genRange          :: Maybe (Int, Int)
>   , _genOccur          :: Int
>   , _genOutOfRange     :: IntSet}
>   deriving (Eq, Show)
> makeGenData            :: GenEnum → GenData
> makeGenData ge                           =
>   let
>     mclip                                = allClipVector VB.! fromEnum ge
>   in
>     GenData ge mclip  0 IntSet.empty
>
> data EnvData                             =
>   EnvData {
>     edPlace            :: String
>   , edTime             :: String}
>   deriving (Eq, Show)
> defEnvData            :: EnvData
> defEnvData                               = EnvData "" ""
>
> data GenSum                              =
>   GenSum {
>     _gsFilename        :: FilePath
>   , _gsGenData         :: VB.Vector GenData
>   , _gsEnvData         :: EnvData}
>   deriving (Eq, Show)
> defGenSum             :: GenSum
> defGenSum                                = GenSum "" VB.empty defEnvData
> makeGenSum             :: FilePath → VB.Vector GenData → GenSum
> makeGenSum filename genData =
>   GenSum filename genData defEnvData
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
>       mapM_ putStrLn fData
>       putStrLn $ unwords ["numFiles=", show (VB.length fData)]
>
> shredFile              :: SFFileBoot → IO GenSum
> shredFile sffile                         =
>   return $ makeGenSum sffile.zFilename vGenData'
>   where
>     vGenData           :: VB.Vector GenData
>     vGenData                             = VB.generate 61 (makeGenData . toEnum)
>     vGenData'          :: VB.Vector GenData
>     vGenData'                            = foldl' shredGen vGenData sffile.zFileArrays.ssIGens
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
>     shredGen       :: VB.Vector GenData → F.Generator → VB.Vector GenData
>         
>     -- 0..4
>     shredGen is (F.StartAddressOffset _)
>                                          = upd is StartAddressOffset Nothing
>     shredGen is (F.EndAddressOffset _)
>                                          = upd is EndAddressOffset Nothing
>     shredGen is (F.LoopStartAddressOffset _)
>                                          = upd is LoopStartAddressOffset Nothing
>     shredGen is (F.LoopEndAddressOffset _)
>                                          = upd is LoopEndAddressOffset Nothing
>     shredGen is (F.StartAddressCoarseOffset _)
>                                          = upd is StartAddressCoarseOffset Nothing
>         
>         -- 5..9
>     shredGen is (F.ModLfoToPitch val)
>                                          = upd is ModLfoToPitch (Just val)
>     shredGen is (F.VibLfoToPitch val)
>                                          = upd is VibLfoToPitch (Just val)
>     shredGen is (F.ModEnvToPitch val)
>                                          = upd is ModEnvToPitch (Just val)
>     shredGen is (F.InitFc val)
>                                          = upd is InitFc (Just val)
>     shredGen is (F.InitQ val)
>                                          = upd is InitQ (Just val)
>         
>         -- 10..13
>     shredGen is (F.ModLfoToFc val)
>                                          = upd is ModLfoToFc (Just val)
>     shredGen is (F.ModEnvToFc val)
>                                          = upd is ModEnvToFc (Just val)
>     shredGen is (F.EndAddressCoarseOffset _)
>                                          = upd is EndAddressCoarseOffset Nothing
>     shredGen is (F.ModLfoToVol val)
>                                          = upd is ModLfoToVol (Just val)
>         
>         -- 15..17
>     shredGen is (F.Chorus val)
>                                          = upd is Chorus (Just val)
>     shredGen is (F.Reverb val)
>                                          = upd is Reverb (Just val)
>     shredGen is (F.Pan val)
>                                          = upd is Pan (Just val)
>         
>         -- 21..24
>     shredGen is (F.DelayModLfo val)
>                                          = upd is DelayModLfo (Just val)
>     shredGen is (F.FreqModLfo val)
>                                          = upd is FreqModLfo (Just val)
>     shredGen is (F.DelayVibLfo val)
>                                          = upd is DelayVibLfo (Just val)
>     shredGen is (F.FreqVibLfo val)
>                                          = upd is FreqVibLfo (Just val)
>         
>         -- 25..30
>     shredGen is (F.DelayModEnv val)
>                                          = upd is DelayModEnv (Just val)
>     shredGen is (F.AttackModEnv val)
>                                          = upd is AttackModEnv (Just val)
>     shredGen is (F.HoldModEnv val)
>                                          = upd is HoldModEnv (Just val)
>     shredGen is (F.DecayModEnv val)
>                                          = upd is DecayModEnv (Just val)
>     shredGen is (F.SustainModEnv val)
>                                          = upd is SustainModEnv (Just val)
>     shredGen is (F.ReleaseModEnv val)
>                                          = upd is ReleaseModEnv (Just val)
>         
>         -- 31..41
>     shredGen is (F.KeyToModEnvHold val)
>                                          = upd is KeyToModEnvHold (Just val)
>     shredGen is (F.KeyToModEnvDecay val)
>                                          = upd is KeyToModEnvDecay (Just val)
>     shredGen is (F.DelayVolEnv val)
>                                          = upd is DelayVolEnv (Just val)
>     shredGen is (F.AttackVolEnv val)
>                                          = upd is AttackVolEnv (Just val)
>     shredGen is (F.HoldVolEnv val)
>                                          = upd is HoldVolEnv (Just val)
>     shredGen is (F.DecayVolEnv val)
>                                          = upd is DecayVolEnv (Just val)
>     shredGen is (F.SustainVolEnv val)
>                                          = upd is SustainVolEnv (Just val)
>     shredGen is (F.ReleaseVolEnv val)
>                                          = upd is ReleaseVolEnv (Just val)
>     shredGen is (F.KeyToVolEnvHold val)
>                                          = upd is KeyToVolEnvHold (Just val)
>     shredGen is (F.KeyToVolEnvDecay val)
>                                          = upd is KeyToVolEnvDecay (Just val)
>     shredGen is (F.InstIndex _)
>                                          = upd is InstIndex Nothing
>         
>         -- 43..48
>     shredGen is (F.KeyRange _ _)
>                                          = upd is KeyRange Nothing
>     shredGen is (F.VelRange _ _)
>                                          = upd is VelRange Nothing
>     shredGen is (F.LoopStartAddressCoarseOffset _)
>                                          = upd is LoopStartAddressCoarseOffset Nothing
>     shredGen is (F.Key val)
>                                          = upd is Key ((Just . fromIntegral) val)
>     shredGen is (F.Vel val)
>                                          = upd is Vel ((Just . fromIntegral) val)
>     shredGen is (F.InitAtten val)
>                                          = upd is InitAtten (Just val)
>         
>         -- 50..54
>     shredGen is (F.LoopEndAddressCoarseOffset _)
>                                          = upd is LoopEndAddressCoarseOffset Nothing
>     shredGen is (F.CoarseTune val)
>                                          = upd is CoarseTune (Just val)
>     shredGen is (F.FineTune val)
>                                          = upd is FineTune (Just val)
>     shredGen is (F.SampleIndex _)
>                                          = upd is SampleIndex Nothing
>     shredGen is (F.SampleMode _)
>                                          = upd is SampleMode Nothing
>         
>         -- 56..58
>     shredGen is (F.ScaleTuning val)
>                                          = upd is ScaleTuning (Just val)
>     shredGen is (F.ExclusiveClass val)
>                                          = upd is ExclusiveClass (Just val)
>     shredGen is (F.RootKey val)
>                                          = upd is RootKey ((Just . fromIntegral) val)
>         
>         -- 60
>     shredGen is (F.ReservedGen _ _)
>                                          = upd is ReservedGen Nothing
>
> addGenDatas            :: GenData → GenData → GenData
> addGenDatas gd1 gd2
>                                          =
>   GenData
>     (gd1 ^. genEnum)
>     (gd1 ^. genRange)
>     ((gd1 ^. genOccur) + (gd1 ^. genOccur))
>     ((gd1 ^. genOutOfRange) `IntSet.union` (gd2 ^. genOutOfRange))
>
> addGenSums             :: GenSum → GenSum → GenSum
> addGenSums (GenSum _ gd1 ed1) (GenSum _ gd2 ed2)     -- WOX must combine ed2 with ed1
>                                          =
>   GenSum "<rollup>" (VB.zipWith addGenDatas gd1 gd2) ed1
>
> openInvestigation      :: VB.Vector SFFileBoot → IO (VB.Vector String)
> openInvestigation vFilesBoot             = do
>   vGenSum                                ← CM.mapM shredFile vFilesBoot
>   let vRollup                            = VB.foldl' addGenSums defGenSum vGenSum
>   let filesOutput                        = VB.concatMap showFile vGenSum
>   let rollupOutput                       = showFile vRollup
>   putStr "\nRollup:"
>   print $ length rollupOutput
>   return $ filesOutput VB.++ rollupOutput
>   where
>     showFile           :: GenSum → VB.Vector String
>     showFile gensum                   =
>       VB.singleton (gensum ^. gsFilename) VB.++ VB.map show (gensum ^. gsGenData)

The End
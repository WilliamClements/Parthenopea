> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
June 16, 2025

> module Eng.Command (
>         batchProcessor ) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec
> import qualified System.FilePattern.Directory
>                                          as FP
  
Implement PCommand ====================================================================================================

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
>   putStrLn "The End"
>
> proceed                :: [FilePath] → IO ()
> proceed sf2s                             = do
>   CM.unless (null sf2s)                  (putStrLn "surveySoundFonts")
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFilesBoot                         = VB.fromList extraction
>   if VB.null vFilesBoot
>     then return ()
>     else do
>       putStrLn "extracted!"
>       vCounts                            ← CM.mapM countEm vFilesBoot
>       print vCounts
>       let vCounts'                       = VB.foldl' (VB.zipWith (+)) (VB.replicate 61 0) vCounts
>       print vCounts'
>       let jj                             = VB.zipWith showItem (VB.generate 61 id) vCounts'
>       mapM_ putStrLn jj
>   where
>     upd                :: VB.Vector Int → Int → VB.Vector Int
>     upd is i                             = VB.update is $ VB.singleton (i, 1 + (is VB.! i))
>
>     showItem           :: Int → Int → String
>     showItem k c                         =
>       let
>         genEnum        :: GenEnum        = toEnum k
>       in
>         unwords [show k, show genEnum, show c]
>
>     countEm            :: SFFileBoot → IO (VB.Vector Int)
>     countEm sffile                       = CM.foldM countOne (VB.replicate 61 0) sffile.zFileArrays.ssIGens
>       where
>         countOne       :: VB.Vector Int → F.Generator → IO (VB.Vector Int)
>         countOne is (F.StartAddressOffset _)
>                                          = return $ upd is (fromEnum StartAddressOffset)
>         countOne is (F.EndAddressOffset _)
>                                          = return $ upd is (fromEnum EndAddressOffset)
>         countOne is (F.LoopStartAddressOffset _)
>                                          = return $ upd is (fromEnum LoopStartAddressOffset)
>         countOne is (F.LoopEndAddressOffset _)
>                                          = return $ upd is (fromEnum LoopEndAddressOffset)
>         countOne is (F.StartAddressCoarseOffset _)
>                                          = return $ upd is (fromEnum StartAddressCoarseOffset)
>         countOne is (F.ModLfoToPitch _)
>                                          = return $ upd is (fromEnum ModLfoToPitch)
>         countOne is (F.VibLfoToPitch _)
>                                          = return $ upd is (fromEnum VibLfoToPitch)
>         countOne is (F.ModEnvToPitch _)
>                                          = return $ upd is (fromEnum ModEnvToPitch)
>         countOne is (F.InitFc _)
>                                          = return $ upd is (fromEnum InitFc)
>         countOne is (F.InitQ _)
>                                          = return $ upd is (fromEnum InitQ)
>         countOne is (F.ModLfoToFc _)
>                                          = return $ upd is (fromEnum ModLfoToFc)
>         countOne is (F.ModEnvToFc _)
>                                          = return $ upd is (fromEnum ModEnvToFc)
>         countOne is (F.EndAddressCoarseOffset _)
>                                          = return $ upd is (fromEnum EndAddressCoarseOffset)
>         countOne is (F.ModLfoToVol _)
>                                          = return $ upd is (fromEnum ModLfoToVol)
>         countOne is (F.Chorus _)
>                                          = return $ upd is (fromEnum Chorus)
>         countOne is (F.Reverb _)
>                                          = return $ upd is (fromEnum Reverb)
>         countOne is (F.Pan _)
>                                          = return $ upd is (fromEnum Pan)
>         countOne is (F.DelayModLfo _)
>                                          = return $ upd is (fromEnum DelayModLfo)
>         countOne is (F.FreqModLfo _)
>                                          = return $ upd is (fromEnum FreqModLfo)
>         countOne is (F.DelayVibLfo _)
>                                          = return $ upd is (fromEnum DelayVibLfo)
>         countOne is (F.FreqVibLfo _)
>                                          = return $ upd is (fromEnum FreqVibLfo)
>         countOne is (F.DelayModEnv _)
>                                          = return $ upd is (fromEnum DelayModEnv)
>         countOne is (F.AttackModEnv _)
>                                          = return $ upd is (fromEnum AttackModEnv)
>         countOne is (F.HoldModEnv _)
>                                          = return $ upd is (fromEnum HoldModEnv)
>         countOne is (F.DecayModEnv _)
>                                          = return $ upd is (fromEnum DecayModEnv)
>         countOne is (F.SustainModEnv _)
>                                          = return $ upd is (fromEnum SustainModEnv)
>         countOne is (F.ReleaseModEnv _)
>                                          = return $ upd is (fromEnum ReleaseModEnv)
>         countOne is (F.KeyToModEnvHold _)
>                                          = return $ upd is (fromEnum KeyToModEnvHold)
>         countOne is (F.KeyToModEnvDecay _)
>                                          = return $ upd is (fromEnum KeyToModEnvDecay)
>         countOne is (F.DelayVolEnv _)
>                                          = return $ upd is (fromEnum DelayVolEnv)
>         countOne is (F.AttackVolEnv _)
>                                          = return $ upd is (fromEnum AttackVolEnv)
>         countOne is (F.HoldVolEnv _)
>                                          = return $ upd is (fromEnum HoldVolEnv)
>         countOne is (F.DecayVolEnv _)
>                                          = return $ upd is (fromEnum DecayVolEnv)
>         countOne is (F.SustainVolEnv _)
>                                          = return $ upd is (fromEnum SustainVolEnv)
>         countOne is (F.ReleaseVolEnv _)
>                                          = return $ upd is (fromEnum ReleaseVolEnv)
>         countOne is (F.KeyToVolEnvHold _)
>                                          = return $ upd is (fromEnum KeyToVolEnvHold)
>         countOne is (F.KeyToVolEnvDecay _)
>                                          = return $ upd is (fromEnum KeyToVolEnvDecay)
>         countOne is (F.InstIndex _)
>                                          = return $ upd is (fromEnum InstIndex)
>         countOne is (F.KeyRange _ _)
>                                          = return $ upd is (fromEnum KeyRange)
>         countOne is (F.VelRange _ _)
>                                          = return $ upd is (fromEnum VelRange)
>         countOne is (F.LoopStartAddressCoarseOffset _)
>                                          = return $ upd is (fromEnum LoopStartAddressCoarseOffset)
>         countOne is (F.Key _)
>                                          = return $ upd is (fromEnum Key)
>         countOne is (F.Vel _)
>                                          = return $ upd is (fromEnum Vel)
>         countOne is (F.InitAtten _)
>                                          = return $ upd is (fromEnum InitAtten)
>         countOne is (F.LoopEndAddressCoarseOffset _)
>                                          = return $ upd is (fromEnum LoopEndAddressCoarseOffset)
>         countOne is (F.CoarseTune _)
>                                          = return $ upd is (fromEnum CoarseTune)
>         countOne is (F.FineTune _)
>                                          = return $ upd is (fromEnum FineTune)
>         countOne is (F.SampleIndex _)
>                                          = return $ upd is (fromEnum SampleIndex)
>         countOne is (F.SampleMode _)
>                                          = return $ upd is (fromEnum SampleMode)
>         countOne is (F.ScaleTuning _)
>                                          = return $ upd is (fromEnum ScaleTuning)
>         countOne is (F.ExclusiveClass _)
>                                          = return $ upd is (fromEnum ExclusiveClass)
>         countOne is (F.RootKey _)
>                                          = return $ upd is (fromEnum RootKey)
>         countOne is (F.ReservedGen _ _)
>                                          = return $ upd is (fromEnum ReservedGen)
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
> data SFFileBoot                          =
>   SFFileBoot {
>     zWordFBoot         :: !Int
>   , zFilename          :: FilePath
>   , zFileArrays        :: FileArrays
>   , zSample            :: SampleArrays}
>
> data GenEnum                              =
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
>   deriving (Enum, Show)

The End
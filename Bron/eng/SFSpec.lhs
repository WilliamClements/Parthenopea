> {-# LANGUAGE UnicodeSyntax #-}

SFSpec
William Clements
April 16, 2023

> module Eng.SFSpec where
>
> import qualified Codec.SoundFont         as F
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Int ( Int8, Int16 )
> import qualified Data.Vector.Strict      as VB
  
implementing SoundFont spec ===========================================================================================

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
> data FileArrays                          = 
>   FileArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssIMods            :: Array Word F.Mod
>   , ssShdrs            :: Array Word F.Shdr}
>
> data SampleArrays                        = 
>   SampleArrays {
>     ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}

Generator Shredding ===================================================================================================

> teclip, tfclip, tqclip, tvclip, ticlip, tpclip, tcclip, tbclip, taclip, tkclip, tdclip,
>   t1clip, t2clip, t3clip, tmclip, tnclip
>                      :: (Int, Int)
>
> teclip                                   = (-12_000, 12_000)
> tfclip                                   = (1_500, 13_500)
> tqclip                                   = (0, 960)
> tvclip                                   = (-960, 960)
> ticlip                                   = (0, 1_000)
> tpclip                                   = (-500, 500)
> tcclip                                   = (-12_000, 5_000)
> tbclip                                   = (-12_000, 8_000)
> taclip                                   = (-16_000, 4_500)
> tkclip                                   = (-1_200, 1_200)
> tdclip                                   = (0, 1_440)
> tmclip                                   = (0, 127)
> tnclip                                   = (1, 127) 
> t1clip                                   = (-120, 120)
> t2clip                                   = (-99, 99)
> t3clip                                   = (0, 1_200)
>
> allClipVector          :: VB.Vector (Maybe (Int, Int), Int)
> allClipVector                            = VB.fromList $ zip allClip allDefault
> allClip                :: [Maybe (Int, Int)]
> allClip                                  =
>   [ Nothing, Nothing, Nothing, Nothing, Nothing          -- StartAddressOffset
>                                                          --  | EndAddressOffset
>                                                          --  | LoopStartAddressOffset
>                                                          --  | LoopEndAddressOffset
>                                                          --  | StartAddressCoarseOffset
>   , Just teclip, Just teclip, Just teclip                -- ModLfoToPitch | VibLfoToPitch | ModEnvToPitch
>   , Just tfclip, Just tqclip                             -- InitFc | InitQ
>   , Just teclip, Just teclip                             -- ModLfoToFc | ModEnvToFc
>   , Nothing                                              -- EndAddressCoarseOffset
>   , Just tvclip                                          -- ModLfoToVol
>   , Nothing                                              -- Unused1
>   , Just ticlip, Just ticlip                             -- Chorus | Reverb
>   , Just tpclip                                          -- Pan
>   , Nothing, Nothing, Nothing                            -- Unused2 | Unused3 | Unused4
>   , Just tcclip, Just taclip                             -- DelayModLfo | FreqModLfo
>   , Just tcclip, Just taclip                             -- DelayVibLfo | FreqVibLfo
>   , Just tcclip, Just tbclip, Just tcclip                -- DelayModEnv | AttackModEnv | HoldModEnv
>   , Just tbclip, Just ticlip, Just tbclip                -- DecayModEnv | SustainModEnv | ReleaseModEnv
>   , Just tkclip, Just tkclip                             -- KeyToModEnvHold | KeyToModEnvDecay
>   , Just tcclip, Just tbclip, Just tcclip                -- DelayVolEnv | AttackVolEnv | HoldVolEnv
>   , Just tbclip, Just tdclip, Just tbclip                -- DecayVolEnv | SustainVolEnv | ReleaseVolEnv
>   , Just tkclip, Just tkclip                             -- KeyToVolEnvHold | KeyToVolEnvDecay
>   , Nothing, Nothing                                     -- InstIndex | Reserved1
>   , Nothing, Nothing                                     -- KeyRange | VelRange
>   , Nothing                                              -- LoopStartAddressCoarseOffset
>   , Just tmclip, Just tnclip                             -- Key | Vel
>   , Just tdclip                                          -- InitAtten
>   , Nothing, Nothing                                     -- Reserved2 | LoopEndAddressCoarseOffset
>   , Just t1clip, Just t2clip                             -- CoarseTune | FineTune
>   , Nothing, Nothing, Nothing                            -- SampleIndex | SampleMode | Reserved3
>   , Just t3clip                                          -- ScaleTuning
>   , Just tnclip                                          -- ExclusiveClass
>   , Just tmclip                                          -- RootKey
>   , Nothing, Nothing                                     -- Unused5 | ReservedGen
>   ]
>
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
>
> valueBearing        :: VB.Vector GenEnum
> valueBearing                          = VB.fromList
>   [ ModLfoToPitch, VibLfoToPitch, ModEnvToPitch
>   , InitFc, InitQ
>   , ModLfoToFc, ModEnvToFc
>   , ModLfoToVol
>   , Chorus, Reverb, Pan
>   , DelayModLfo, FreqModLfo
>   , DelayVibLfo, FreqVibLfo
>   , DelayModEnv, AttackModEnv, HoldModEnv
>   , DecayModEnv, SustainModEnv, ReleaseModEnv
>   , KeyToModEnvHold, KeyToModEnvDecay
>   , DelayVolEnv, AttackVolEnv, HoldVolEnv
>   , DecayVolEnv, SustainVolEnv, ReleaseVolEnv
>   , KeyToVolEnvHold, KeyToVolEnvDecay
>   , Key, Vel
>   , InitAtten
>   , CoarseTune, FineTune
>   , ScaleTuning
>   , ExclusiveClass
>   , RootKey]
>
> allDefault          :: [Int]
> allDefault                            =
>  [ 0, 0, 0, 0, 0                         -- (addresses)
>  , 0, 0, 0                               -- ModLfoToPitch, VibLfoToPitch, ModEnvToPitch        
>  , 13_500, 0                             -- InitFc, InitQ
>  , 0, 0                                  -- ModLfoToFc, ModEnvToFc
>  , 0                                     -- EndAddressCoarseOffset
>  , 0                                     -- ModLfoToVol
>  , 0                                     -- Unused1
>  , 0, 0, 0                               -- Chorus, Reverb, Pan
>  , 0, 0, 0                               -- Unused2, Unused3, Unused4
>  , -12_000, 0                            -- DelayModLfo, FreqModLfo
>  , -12_000, 0
>  , -12_000, -12_000, -12_000             -- DelayModEnv, AttackModEnv, HoldModEnv
>  , -12_000, 0, -12_000                   -- DecayModEnv, SustainModEnv, ReleaseModEnv
>  , 0, 0                                  -- KeyToModEnvHold, KeyToModEnvDecay
>  , -12_000, -12_000, -12_000             -- DelayVolEnv, AttackVolEnv, HoldVolEnv
>  , -12_000, 0, -12_000                   -- DecayVolEnv, SustainVolEnv, ReleaseVolEnv
>  , 0, 0                                  -- KeyToVolEnvHold, KeyToVolEnvDecay
>  , 0, 0                                  -- InstIndex, Reserved1
>  , 0, 0                                  -- KeyRange, VelRange
>  , 0                                     -- LoopStartAddressCoarseOffset
>  , -1, -1                                -- Key, Vel
>  , 0                                     -- InitAtten
>  , 0, 0                                  -- Reserved2, LoopEndAddressCoarseOffset
>  , 0, 0                                  -- CoarseTune, FineTune
>  , 0, 0, 0                               -- SampleModes
>  , 100                                   -- ScaleTuning
>  , 0                                     -- ExclusiveClass
>  , -1                                    -- RootKey
>  , 0, 0]                                 -- Unused5, ReservedGen

The End
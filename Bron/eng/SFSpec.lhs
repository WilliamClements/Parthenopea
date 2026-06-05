> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE UnicodeSyntax #-}

SFSpec
William Clements
April 16, 2023

> module Eng.SFSpec where
>
> import qualified Codec.SoundFont         as F
> import Control.Lens hiding ( element, ix )
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Int ( Int8, Int16 )
> import Data.IntSet ( IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
  
The Generator types are numbered 0 to 60. =============================================================================

The per-file diagnostic data (GenSum)includes:
1. (raw data) vector (GenData)of 61 per-generator infos - each with
   a. occurrence count
   b. all values encountered, partitioned by out-of-range
   c. point accumulator
2. envelope statistics summed up over all instruments in the file
3. numerical dispersion for found values of each Generator type
  
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
> data GenData                             =
>   GenData {
>     _gId               :: GenEnum
>   , _gOccur            :: Int
>   , _gAccum            :: Int
>   , _gAccumSquares     :: Double
>   , _gWildValues       :: IntSet}
>   deriving (Eq, Show)
> makeLenses ''GenData
> makeGenData            :: GenEnum → GenData
> makeGenData gen                          = GenData gen 0 0 0  IntSet.empty     
> isEmptyGenData         :: GenData → Bool
> isEmptyGenData gd                        = (gd ^. gOccur) == 0
> addGenDatas            :: GenData → GenData → GenData
> addGenDatas gd1 gd2                      =
>   GenData
>     (gd1 ^. gId)
>     ((gd1 ^. gOccur) + (gd2 ^. gOccur))
>     ((gd1 ^. gAccum) + (gd2 ^. gAccum))
>     ((gd1 ^. gAccumSquares) + (gd2 ^. gAccumSquares))
>     ((gd1 ^. gWildValues) `IntSet.union` (gd2 ^. gWildValues))
>
> type BagIndex                            = Int
> type GenSlate                            = VB.Vector GenData
>
> initGenSlate           :: GenSlate
> initGenSlate                             = VB.generate 61 (makeGenData . toEnum)
>
> data EState                              =
>   EOff | EOnSmall | EOnLarge
>   deriving (Eq, Ord, Show)
> categorize             :: Maybe Int → EState
> categorize mint                          =
>   case mint of
>     Nothing            → EOff
>     Just n             → if n < 0
>                            then EOnSmall
>                            else EOnLarge
>
> data EConfig                             =
>   EConfig {
>     _eConfigDelay      :: EState
>   , _eConfigAttack     :: EState
>   , _eConfigHold       :: EState
>   , _eConfigDecay      :: EState
>   , _eConfigRelease    :: EState}
>   deriving (Eq, Ord, Show)
> makeLenses ''EConfig
> makeEConfig            :: Maybe Int → Maybe Int → Maybe Int → Maybe Int → Maybe Int → EConfig
> makeEConfig delay attack hold decay release
>                                          =
>   EConfig 
>     (categorize delay) 
>     (categorize attack)
>     (categorize hold)
>     (categorize decay) 
>     (categorize release)
> initEC                 :: EConfig
> initEC                                   = makeEConfig Nothing Nothing Nothing Nothing Nothing
> mark                   :: Map EConfig Int → EConfig → Map EConfig Int
> mark m ec                                = Map.insertWith (+) ec 1 m
>
> data GenSumLevel                         =
>  GSZoneLevel | GSInstLevel | GSFileLevel | GSRollLevel
>  deriving (Enum, Eq, Show)
>
> data GenSum                              =
>   GenSum {
>     _gsLevel           :: GenSumLevel
>   , _gsTag             :: String
>   , _gsGenSlate        :: GenSlate
>   , _gsSubSums         :: VB.Vector GenSum
>   , _gsModEnvMap       :: Map EConfig Int
>   , _gsVolEnvMap       :: Map EConfig Int}
>   deriving (Eq, Show)
> makeLenses ''GenSum
> makeGenSum             :: GenSumLevel → String → VB.Vector GenData → VB.Vector GenSum → Map EConfig Int → Map EConfig Int → GenSum
> makeGenSum                               = GenSum
> initGenSum             :: String → GenSum
> initGenSum tag                           = makeGenSum GSZoneLevel tag initGenSlate VB.empty Map.empty Map.empty
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

Returns the frequency ratio

> fromCents              :: Double → Double
> fromCents cents                          = pow 2 (cents/12/100)
>
> fromCents'             :: Maybe Int → Maybe Int → Maybe Double
> fromCents' mcoarse mfine
>   | isNothing mcoarse && isNothing mfine = Nothing
>   | otherwise                            = Just $ fromCents $ coarse * 100 + fine
>   where
>     coarse, fine       :: Double
>     coarse                               = maybe 0 fromIntegral mcoarse
>     fine                                 = maybe 0 fromIntegral mfine

Returns the frequency

> fromAbsoluteCents      :: Double → Double
> fromAbsoluteCents acents                 = 8.176 * fromCents acents
>
> toAbsoluteCents        :: Double → Int
> toAbsoluteCents freq                     = round $ 100 * 12 * logBase 2 (freq / 8.176)

Returns the elapsed time in seconds

> fromTimecents          :: Double → Double
> fromTimecents timecents                  = pow 2 (timecents / 1_200)
>
> toTimecents            :: Double → Int
> toTimecents secs                         = round $ logBase 2 secs * 1_200

Returns the amplitude ratio

> fromCentibels, toCentibels
>                        :: Double → Double
> fromCentibels centibels                  = pow 10 (centibels/1000)
> toCentibels ratio                        = logBase 10 (ratio * 1000)

Returns the amplitude ratio (based on input 10ths of a percent) 

> fromTithe              :: Double → Double
> fromTithe x                              = x / 1000

> fromTithe'              :: Maybe Int → Bool → Double
> fromTithe' iS isVol                      =
>   if isVol
>     then 1 / fromCentibels jS
>     else (1000 - jS) / 1000
>   where
>     jS                 :: Double         = maybe 0 fromIntegral iS

Raises 'a' to the power 'b' using logarithms

> pow                    :: Floating a ⇒ a → a → a
> pow x y                                  = exp (log x * y)

Generator Shredding ===================================================================================================

> clip                   :: Ord n ⇒ (n, n) → n → n
> clip (lower, upper) val                  = min upper (max lower val)
>
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
> data Unit                                = NoUnit | Centibels | Cents | AbsoluteCents | TimeCents | Tenths
>   deriving (Eq, Show)
> makePrisms ''Unit
>
> data Spec where
>   Spec       :: {_gClip :: Maybe (Int, Int)
>                , _gDefault :: Int
>                , _gUnit :: Unit
>                } → Spec
>   deriving (Eq, Show)
> makeLenses ''Spec
>
> data Means where
>   Means       :: {mUnit :: String
>                , mDefault :: Double
>                , mPopulationMean :: Double
>                , mSampleMean :: Maybe Double
>                } → Means
>   deriving (Eq, Show)
>
> specVector             :: VB.Vector Spec
> specVector                               = VB.zipWith3 Spec allClip allDefault allUnits
>   where
>     allClip            :: VB.Vector (Maybe (Int, Int))
>     allClip                              = VB.fromList
>       [ Nothing, Nothing, Nothing, Nothing, Nothing          -- StartAddressOffset
>                                                              --  | EndAddressOffset
>                                                              --  | LoopStartAddressOffset
>                                                              --  | LoopEndAddressOffset
>                                                              --  | StartAddressCoarseOffset
>       , Just teclip, Just teclip, Just teclip                -- ModLfoToPitch | VibLfoToPitch | ModEnvToPitch
>       , Just tfclip, Just tqclip                             -- InitFc | InitQ
>       , Just teclip, Just teclip                             -- ModLfoToFc | ModEnvToFc
>       , Nothing                                              -- EndAddressCoarseOffset
>       , Just tvclip                                          -- ModLfoToVol
>       , Nothing                                              -- Unused1
>       , Just ticlip, Just ticlip                             -- Chorus | Reverb
>       , Just tpclip                                          -- Pan
>       , Nothing, Nothing, Nothing                            -- Unused2 | Unused3 | Unused4
>       , Just tcclip, Just taclip                             -- DelayModLfo | FreqModLfo
>       , Just tcclip, Just taclip                             -- DelayVibLfo | FreqVibLfo
>       , Just tcclip, Just tbclip, Just tcclip                -- DelayModEnv | AttackModEnv | HoldModEnv
>       , Just tbclip, Just ticlip, Just tbclip                -- DecayModEnv | SustainModEnv | ReleaseModEnv
>       , Just tkclip, Just tkclip                             -- KeyToModEnvHold | KeyToModEnvDecay
>       , Just tcclip, Just tbclip, Just tcclip                -- DelayVolEnv | AttackVolEnv | HoldVolEnv
>       , Just tbclip, Just tdclip, Just tbclip                -- DecayVolEnv | SustainVolEnv | ReleaseVolEnv
>       , Just tkclip, Just tkclip                             -- KeyToVolEnvHold | KeyToVolEnvDecay
>       , Nothing, Nothing                                     -- InstIndex | Reserved1
>       , Nothing, Nothing                                     -- KeyRange | VelRange
>       , Nothing                                              -- LoopStartAddressCoarseOffset
>       , Just tmclip, Just tnclip                             -- Key | Vel
>       , Just tdclip                                          -- InitAtten
>       , Nothing, Nothing                                     -- Reserved2 | LoopEndAddressCoarseOffset
>       , Just t1clip, Just t2clip                             -- CoarseTune | FineTune
>       , Nothing, Nothing, Nothing                            -- SampleIndex | SampleMode | Reserved3
>       , Just t3clip                                          -- ScaleTuning
>       , Just tnclip                                          -- ExclusiveClass
>       , Just tmclip                                          -- RootKey
>       , Nothing, Nothing                                     -- Unused5 | ReservedGen
>       ]
>     allDefault      :: VB.Vector Int
>     allDefault                        = VB.fromList 
>       [ 0, 0, 0, 0, 0                         -- (addresses)
>       , 0, 0, 0                               -- ModLfoToPitch, VibLfoToPitch, ModEnvToPitch        
>       , 13_500, 0                             -- InitFc, InitQ
>       , 0, 0                                  -- ModLfoToFc, ModEnvToFc
>       , 0                                     -- EndAddressCoarseOffset
>       , 0                                     -- ModLfoToVol
>       , 0                                     -- Unused1
>       , 0, 0, 0                               -- Chorus, Reverb, Pan
>       , 0, 0, 0                               -- Unused2, Unused3, Unused4
>       , -12_000, 0                            -- DelayModLfo, FreqModLfo
>       , -12_000, 0
>       , -12_000, -12_000, -12_000             -- DelayModEnv, AttackModEnv, HoldModEnv
>       , -12_000, 0, -12_000                   -- DecayModEnv, SustainModEnv, ReleaseModEnv
>       , 0, 0                                  -- KeyToModEnvHold, KeyToModEnvDecay
>       , -12_000, -12_000, -12_000             -- DelayVolEnv, AttackVolEnv, HoldVolEnv
>       , -12_000, 0, -12_000                   -- DecayVolEnv, SustainVolEnv, ReleaseVolEnv
>       , 0, 0                                  -- KeyToVolEnvHold, KeyToVolEnvDecay
>       , 0, 0                                  -- InstIndex, Reserved1
>       , 0, 0                                  -- KeyRange, VelRange
>       , 0                                     -- LoopStartAddressCoarseOffset
>       , -1, -1                                -- Key, Vel
>       , 0                                     -- InitAtten
>       , 0, 0                                  -- Reserved2, LoopEndAddressCoarseOffset
>       , 0, 0                                  -- CoarseTune, FineTune
>       , 0, 0, 0                               -- SampleModes
>       , 100                                   -- ScaleTuning
>       , 0                                     -- ExclusiveClass
>       , -1                                    -- RootKey
>       , 0, 0]                                 -- Unused5, ReservedGen
>
> allUnits               :: VB.Vector Unit
> allUnits                                 = VB.update clear changes
>   where
>     clear                                = VB.replicate 61 NoUnit
>
>     makeUpdate         :: Unit → VB.Vector GenEnum → VB.Vector (Int, Unit)
>     makeUpdate unit                      = VB.map disperse
>                                              where disperse gen = (fromEnum gen, unit)
>     changes            :: VB.Vector (Int, Unit)
>     changes                              =       makeUpdate Centibels        usesCentibels
>                                            VB.++ makeUpdate Cents            usesCents
>                                            VB.++ makeUpdate AbsoluteCents    usesAbsoluteCents
>                                            VB.++ makeUpdate TimeCents        usesTimeCents
>                                            VB.++ makeUpdate Tenths           usesTenths
>
> unitAction              :: Unit → (String, Double → Double)
> unitAction Centibels                     = ("centibels to volume ratio",     fromCentibels)
> unitAction Cents                         = ("cents to Hz ratio",             fromCents)
> unitAction AbsoluteCents                 = ("absolute cents to Hz",          fromAbsoluteCents)
> unitAction TimeCents                     = ("time cents to seconds",         fromTimecents)
> unitAction Tenths                        = ("from tenths",                   fromTithe)
> unitAction unit                          = error $ unwords ["no action for", show unit]
>
> valueBearing           :: VB.Vector GenEnum
> valueBearing                             = VB.fromList
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
> noNumericDefault, usesCentibels, usesCents, usesAbsoluteCents, usesTimeCents, usesTenths
>                        :: VB.Vector GenEnum
> noNumericDefault                         = VB.fromList [  ExclusiveClass, Key, RootKey, Vel]
> usesCentibels                            = VB.fromList [  InitQ, ModLfoToVol, SustainModEnv, SustainVolEnv, InitAtten]
> usesCents                                = VB.fromList [  ModLfoToPitch, VibLfoToPitch, ModEnvToPitch
>                                                         , ModLfoToFc, ModEnvToFc, FreqVibLfo, FineTune]
> usesAbsoluteCents                        = VB.fromList [  InitFc, FreqModLfo, FreqVibLfo] 
> usesTimeCents                            = VB.fromList [  DelayModLfo, DelayVibLfo
>                                                         , DelayModEnv, AttackModEnv, HoldModEnv, DecayModEnv, ReleaseModEnv
>                                                         , DelayVolEnv, AttackVolEnv, HoldVolEnv, DecayVolEnv, ReleaseVolEnv]
> usesTenths                               = VB.fromList [  Chorus, Reverb, Pan]

The End
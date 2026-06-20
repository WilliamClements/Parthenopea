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
> import Data.Char
> import Data.Int ( Int8, Int16 )
> import Data.IntSet ( IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Set (Set)
> import qualified Data.Set                as Set
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
  
The Generator types are numbered 0 to 60. =============================================================================

The per-file diagnostic data (GenSum) includes:
1. numerical dispersion for found values of each Generator type
2. envelope statistics summed up over all instruments in the file
3. (raw data) vector (GenData) of 61 per-generator infos - each with
   a. occurrence count
   b. point accumulator
   c. all out-of-range values encountered
  
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
>   deriving (Enum, Eq, Ord, Show)
>
> data GenData                             =
>   GenData {
>     _gGen              :: GenEnum
>   , _gOccur            :: Int
>   , _gWildValues       :: IntSet
>   , _gAccum            :: Double
>   , _gSquares          :: Double
>   , _gUnitAccum        :: Double
>   , _gUnitSquares      :: Double}
>   deriving (Eq, Show)
> makeLenses ''GenData
> makeGenData            :: GenEnum → GenData
> makeGenData gen                          = GenData gen 0 IntSet.empty 0 0 0 0      
> isEmptyGenData         :: GenData → Bool
> isEmptyGenData gd                        = (gd ^. gOccur) == 0
> addGenDatas            :: GenData → GenData → GenData
> addGenDatas gd1 gd2                      =
>   GenData
>     (gd1 ^. gGen)
>     ((gd1 ^. gOccur) + (gd2 ^. gOccur))
>     ((gd1 ^. gWildValues) `IntSet.union` (gd2 ^. gWildValues))
>     ((gd1 ^. gAccum) + (gd2 ^. gAccum))
>     ((gd1 ^. gSquares) + (gd2 ^. gSquares))
>     ((gd1 ^. gUnitAccum) + (gd2 ^. gUnitAccum))
>     ((gd1 ^. gUnitSquares) + (gd2 ^. gUnitSquares))
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
> categorize             :: Maybe Double → EState
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
> makeEConfig            :: Maybe Double → Maybe Double → Maybe Double → Maybe Double → Maybe Double → EConfig
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
>  deriving (Enum, Eq, Ord, Show)
>
> data GenSum                              =
>   GenSum {
>     _gsLevel           :: GenSumLevel
>   , _gsTag             :: String
>   , _gsGenSlate        :: GenSlate
>   , _gsSubSums         :: VB.Vector GenSum
>   , _gsZoneCount       :: Int
>   , _gsModEnvMap       :: Map EConfig Int
>   , _gsVolEnvMap       :: Map EConfig Int}
>   deriving (Eq, Show)
> makeLenses ''GenSum
> makeGenSum             :: GenSumLevel → String → VB.Vector GenData → VB.Vector GenSum
>                           → Int → Map EConfig Int → Map EConfig Int → GenSum
> makeGenSum                               = GenSum
> addGenSums         :: GenSum → GenSum → GenSum
> addGenSums (GenSum level tagAdd slate1 _ nz1 mod1 vol1) (GenSum _ _ slate2 _ nz2 mod2 vol2)
>                                          =
>   GenSum
>     level 
>     tagAdd
>     (VB.zipWith addGenDatas slate1 slate2)
>     VB.empty
>     (nz1 + nz2)
>     (Map.unionWith (+) mod1 mod2)
>     (Map.unionWith (+) vol1 vol2)
>
> openSoundFontFile      :: Int → FilePath → IO SF2
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
>       return $ SF2 
>                  wFile 
>                  filename 
>                  boota 
>                  samplea
>
> fixName                :: String → String
> fixName name
>   | null name                            = "<noname>"
>   | otherwise                            = map (\cN → if goodChar cN then cN else '_') name
>                                              where goodChar cN = isAscii cN && not (isControl cN)
>
> data SF2                                 =
>   SF2 {
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
> fromSemitones          :: Double → Double
> fromSemitones semitones                  = pow 2 (semitones/12)
>
> fromCents'             :: Maybe Int → Maybe Int → Maybe Double
> fromCents' mcoarse mfine
>   | isNothing mcoarse && isNothing mfine = Nothing
>   | otherwise                            = Just $ fromCents $ coarse * 100 + fine
>   where
>     coarse, fine       :: Double
>     coarse                               = maybe 0 fromIntegral mcoarse
>     fine                                 = maybe 0 fromIntegral mfine
>
> fromMicrotones         :: Double → Double
> fromMicrotones centsPerKey               = pow 2 (centsPerKey / 1_200)

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
> teclip                                   = (-12_000, 12_000) -- ModLfoToPitch | VibLfoToPitch | ModEnvToPitch
>                                                              -- | ModLfoToFc | ModEnvToFc
> tfclip                                   = (1_500, 13_500)   -- InitFc
> tqclip                                   = (0, 960)          -- InitQ
> tvclip                                   = (-960, 960)       -- ModLfoToVol
> ticlip                                   = (0, 1_000)        -- Chorus | Reverb | SustainModEnv
> tpclip                                   = (-500, 500)       -- Pan
> tcclip                                   = (-12_000, 5_000)  -- DelayModLfo | DelayVibLfo | DelayModEnv | HoldModEnv
>                                                              -- | DelayVolEnv | HoldVolEnv
> tbclip                                   = (-12_000, 8_000)  -- AttackModEnv
> taclip                                   = (-16_000, 4_500)  -- DelayModEnv | AttackModEnv | DecayModEnv
>                                                              -- | ReleaseModEnv
> tkclip                                   = (-1_200, 1_200)   -- KeyToModEnvHold | KeyToModEnvDecay
>                                                              -- | KeyToVolEnvHold | KeyToVolEnvDecay
> tdclip                                   = (0, 1_440)        -- SustainVolEnv | InitAtten
> tmclip                                   = (0, 127)          -- Key | RootKey
> tnclip                                   = (1, 127)          -- Vel | ExclusiveClass
> t1clip                                   = (-120, 120)       -- CoarseTune
> t2clip                                   = (-99, 99)         -- FineTune
> t3clip                                   = (0, 1_200)        -- ScaleTuning
>
> data Unit                                = NoUnit | Centibels | Cents | AbsoluteCents | Timecents
>                                                   | TenthsOfAPercent | Points | CoarsePoints | Keys | Semitones
>                                                   | CentsPerKey | TimecentsPerKey
>   deriving (Eq, Show)
>
> data Spec where
>   Spec       :: {_gClip :: Maybe (Int, Int)
>                , _gDefault :: Int
>                , _gUnit :: Unit
>                } → Spec
>   deriving (Eq, Show)
> makeLenses ''Spec
>
> data GenResult where
>   GenResult   :: {rUnit :: String
>                 , rClip    :: (Double, Double)
>                 , rDefault :: Double
>                } → GenResult
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
>     makeUpdate         :: Unit → Set GenEnum → [(Int, Unit)]
>     makeUpdate unit                      =
>       let
>         erect          :: [(Int, Unit)] → GenEnum → [(Int, Unit)]
>         erect ius gen                    = (fromEnum gen, unit) : ius
>       in
>         Set.foldl erect []
>
>     changes            :: VB.Vector (Int, Unit)
>     changes                              =
>       VB.fromList $ concat [
>           makeUpdate Centibels            centibelsUsers
>         , makeUpdate Cents                centsUsers
>         , makeUpdate AbsoluteCents        absoluteCentsUsers
>         , makeUpdate Timecents            timeCentsUsers
>         , makeUpdate TenthsOfAPercent     tenthsUsers
>         , makeUpdate Points               pointsUsers
>         , makeUpdate CoarsePoints         coarsePointsUsers
>         , makeUpdate Keys                 keysUsers
>         , makeUpdate Semitones            semitonesUsers
>         , makeUpdate CentsPerKey          centsPerKeyUsers
>         , makeUpdate TimecentsPerKey      timecentsPerKeyUsers]
>
> unitAction              :: Unit → (String, Double → Double)
> unitAction Centibels                     = ("volume ratio (from centibels)",     fromCentibels)
> unitAction Cents                         = ("Hz ratio (from cents)",             fromCents)
> unitAction AbsoluteCents                 = ("Hz (from absolute cents)",          fromAbsoluteCents)
> unitAction Timecents                     = ("seconds (from time cents)",         fromTimecents)
> unitAction TenthsOfAPercent              = ("tenths of a percent",               (/ 1000))
> unitAction Points                        = ("sample points (no conversion)",     id)
> unitAction CoarsePoints                  = ("32768 sample points",               (* 32768))
> unitAction Keys                          = ("MIDI keys",                         id)
> unitAction Semitones                     = ("semitones",                         fromSemitones)
> unitAction CentsPerKey                   = ("Hz ratio (from cents per key)",     fromMicrotones)
> unitAction TimecentsPerKey               = ("seconds (from time cents per key)", fromTimecents)
> unitAction NoUnit                        = ("no unit",                           id)
>
> allGens                :: VB.Vector GenEnum
> allGens                                  = VB.generate 61 toEnum
>
> centibelsUsers, centsUsers, absoluteCentsUsers, timeCentsUsers
>  , tenthsUsers, pointsUsers, coarsePointsUsers, keysUsers
>  , semitonesUsers, centsPerKeyUsers
>  , timecentsPerKeyUsers
>                        :: Set GenEnum
> centibelsUsers                           = Set.fromList [  InitQ, ModLfoToVol, SustainModEnv, SustainVolEnv, InitAtten]
> centsUsers                               = Set.fromList [  ModLfoToPitch, VibLfoToPitch, ModEnvToPitch
>                                                          , ModLfoToFc, ModEnvToFc, FineTune]
> absoluteCentsUsers                       = Set.fromList [  InitFc, FreqModLfo, FreqVibLfo] 
> timeCentsUsers                           = Set.fromList [  DelayModLfo, DelayVibLfo
>                                                          , DelayModEnv, AttackModEnv, HoldModEnv, DecayModEnv, ReleaseModEnv
>                                                          , DelayVolEnv, AttackVolEnv, HoldVolEnv, DecayVolEnv, ReleaseVolEnv]
> tenthsUsers                              = Set.fromList [  Chorus, Reverb, Pan]
> pointsUsers                              = Set.fromList [  StartAddressOffset, EndAddressOffset
>                                                          , LoopStartAddressOffset, LoopEndAddressOffset]
> coarsePointsUsers                        = Set.fromList [  StartAddressCoarseOffset, EndAddressCoarseOffset
>                                                          , LoopStartAddressCoarseOffset, LoopEndAddressCoarseOffset]
> keysUsers                                = Set.fromList [  Key, RootKey, Vel]
> semitonesUsers                           = Set.fromList [  CoarseTune]
> centsPerKeyUsers                         = Set.fromList [  ScaleTuning]
> timecentsPerKeyUsers                     = Set.fromList [  KeyToModEnvHold, KeyToModEnvDecay
>                                                          , KeyToVolEnvHold, KeyToVolEnvDecay]

The End
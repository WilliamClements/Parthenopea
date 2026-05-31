> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE UnicodeSyntax #-}

ShredFile
William Clements
May 28, 2026

> module Eng.ShredFile where
>
> import qualified Codec.SoundFont         as F
> import qualified Data.Bifunctor          as BF
> import Control.Lens hiding ( element, ix )
> import Data.Array.Unboxed
> import Data.IntMap.Strict ( IntMap )
> import qualified Data.IntMap.Strict      as IntMap
> import Data.IntSet ( IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec
  
The Generator types are numbered 0 to 60. =============================================================================

The per-file diagnostic data includes:
1. vector of 61 per-generator infos - each with
   a. static data from the spec (clip ranges and defaults)
   b. occurrence count
   c. all values encountered, partitioned by out-of-range
2. envelope statistics summed up over all instruments in the file

We list out both sets of data for each file, then list out the rollup sets.

> data GenData                             =
>   GenData {
>     _gId               :: GenEnum
>   , _gStatic           :: StaticData
>   , _gOccur            :: Int
>   , _gAccum            :: Int
>   , _gAccumWith        :: Int
>   , _gGoodValues       :: IntSet
>   , _gWildValues       :: IntSet}
>   deriving (Eq, Show)
> makeLenses ''GenData
> makeGenData            :: GenEnum → GenData
> makeGenData ge                           =
>   let
>     static                               = staticDataVector VB.! fromEnum ge
>   in
>     GenData ge static 0 0 0 IntSet.empty IntSet.empty
> isEmptyGenData       :: GenData → Bool
> isEmptyGenData gd                        = (gd ^. gOccur) == 0
>
> addGenDatas            :: GenData → GenData → GenData
> addGenDatas gd1 gd2
>                                          =
>   GenData
>     (gd1 ^. gId)
>     (StaticData (gd1 ^. (gStatic . gClip)) (gd1 ^. (gStatic . gDefault)))
>     ((gd1 ^. gOccur) + (gd2 ^. gOccur))
>     ((gd1 ^. gAccum) + (gd2 ^. gAccum))
>     ((gd1 ^. gAccumWith) + (gd2 ^. gAccumWith))
>     ((gd1 ^. gGoodValues) `IntSet.union` (gd2 ^. gGoodValues))
>     ((gd1 ^. gWildValues) `IntSet.union` (gd2 ^. gWildValues))
>
> type BagIndex                            = Int
> type GenSlate                            = VB.Vector GenData
>
> initGenSlate           :: GenSlate
> initGenSlate                             = VB.generate 61 (makeGenData . toEnum)
>
> addGenSlates           :: GenSlate → GenSlate → GenSlate
> addGenSlates                            = VB.zipWith addGenDatas
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
> initConfig             :: EConfig
> initConfig                               = makeEConfig Nothing Nothing Nothing Nothing Nothing
> mark                   :: Map EConfig Int → EConfig → Map EConfig Int
> mark mc ec                               = Map.insertWith (+) ec 1 mc
>
> data GenSumType                          =
>  GSTZone | GSTInst | GSTFile | GSTRollup
>  deriving (Enum, Eq, Show)
>
> data GenSum                              =
>   GenSum {
>     _gsType            :: GenSumType
>   , _gsTag             :: String
>   , _gsGenSlate        :: GenSlate
>   , _gsSubSums         :: VB.Vector GenSum
>   , _gsModEnvMap       :: Map EConfig Int
>   , _gsVolEnvMap       :: Map EConfig Int}
>   deriving (Eq, Show)
> makeLenses ''GenSum
> makeGenSum             :: GenSumType → String → VB.Vector GenData → VB.Vector GenSum → Map EConfig Int → Map EConfig Int → GenSum
> makeGenSum                               = GenSum
> initGenSum             :: String → GenSum
> initGenSum tag                           = makeGenSum GSTZone tag initGenSlate VB.empty Map.empty Map.empty

Getting down to business ==============================================================================================
   
From one SoundFont file we produce one GenSum, to collect interesting aspects about Generators.
Later on, a rollup GenSum will be produced - per-file GenSums added together.

> shredFile              :: SFFileBoot → IO GenSum
> shredFile sffile                         = do
>   return $ IntMap.foldlWithKey shredInst (initGenSum sffile.zFilename) owners
>   where
>     loadInst k                   = sffile.zFileArrays.ssInsts ! fromIntegral k
>
>     shredInst          :: GenSum → Int → IntSet → GenSum
>     shredInst genSumIn _ bags            = genSumOut
>       where
>         genSumOut      :: GenSum         = genSumIn & gsGenSlate  .~ foldl' addGenSlates initGenSlate slates
>                                                     & gsModEnvMap .~ addEnvMaps (genSumIn ^. gsModEnvMap) modMap
>                                                     & gsVolEnvMap .~ addEnvMaps (genSumIn ^. gsVolEnvMap) volMap
>
>         slates_, slates
>                        :: [GenSlate]
>         slates_                          =
>           let
>             shredBag   :: BagIndex → GenSlate
>             shredBag ibag                = foldl' shredGen initGenSlate gens
>               where
>                 igen, jgen
>                        :: Int
>                 igen                     = fromIntegral $ F.genNdx (loadBag ibag)
>                 jgen                     = fromIntegral $ F.genNdx (loadBag (ibag + 1))
>                 loadBag kbag             = sffile.zFileArrays.ssIBags ! fromIntegral kbag
>                 loadGen kgen             = sffile.zFileArrays.ssIGens ! fromIntegral kgen
>
>                 gens                     = VB.generate (jgen - igen) (loadGen . (+ igen))
>
>             zones                    = map shredBag (IntSet.toList bags)
>           in
>             zones
>
>         slates                           = 
>           let
>             firstZone  :: GenSlate       = head slates_
>             globalZone :: Bool           = isEmptyGenData $ firstZone VB.! fromEnum SampleIndex
>
>             replace    :: GenData → GenData → GenData
>             replace gd1 gd2
>               | gd1E && not gd2E         = gd2
>               | not gd1E && gd2E         = gd1
>               | otherwise                = gd2
>               where
>                 gd1E                     = isEmptyGenData gd1
>                 gd2E                     = isEmptyGenData gd2
>           in
>             if globalZone
>               then map (VB.zipWith replace firstZone) (tail slates_)
>               else slates_        
>
>         cfgs           :: [(EConfig, EConfig)]
>         cfgs                             = map (VB.foldl' examineIf (initConfig, initConfig)) slates
>         modMap, volMap :: Map EConfig Int
>         (modMap, volMap)                = (foldl' mark Map.empty (map fst cfgs), foldl' mark Map.empty (map snd cfgs))
> 
>     owners             :: IntMap IntSet
>     owners                               = foldl' erect IntMap.empty [ii..(ij-1)]
>       where
>         (wi, wj)                         = bounds sffile.zFileArrays.ssInsts
>         (ii, ij)                         = BF.bimap fromIntegral fromIntegral (wi, wj)
>
>         erect m kinst                    =
>           let
>             iinst                        = loadInst kinst
>             jinst                        = loadInst (kinst + 1)
>
>             ibag                         = fromIntegral (F.instBagNdx iinst)
>             jbag                         = fromIntegral (F.instBagNdx jinst)
>           in
>             if ibag == jbag
>               then m
>               else IntMap.insert kinst (IntSet.fromList [ibag..(jbag - 1)]) m
>
>     examineIf          :: (EConfig, EConfig) → GenData → (EConfig, EConfig)
>     examineIf acc genData                = if isEmptyGenData genData
>                                               then acc
>                                               else examine acc (genData ^. gId, head (IntSet.toList (genData ^. gGoodValues)))
>
>     examine            :: (EConfig, EConfig) → (GenEnum, Int) → (EConfig, EConfig)
>     examine (ecMod, ecVol) (DelayModEnv, val)
>                                          = ((eConfigDelay .~ categorize (Just val)) ecMod, ecVol)
>     examine (ecMod, ecVol) (AttackModEnv, val)
>                                          = ((eConfigAttack .~ categorize (Just val)) ecMod, ecVol)
>     examine (ecMod, ecVol) (HoldModEnv, val)
>                                          = ((eConfigHold .~ categorize (Just val)) ecMod, ecVol)
>     examine (ecMod, ecVol) (DecayModEnv, val)
>                                          = ((eConfigDecay .~ categorize (Just val)) ecMod, ecVol)
>     examine (ecMod, ecVol) (ReleaseModEnv, val)
>                                          = ((eConfigRelease .~ categorize (Just val)) ecMod, ecVol)
>
>     examine (ecMod, ecVol) (DelayVolEnv, val)
>                                          = (ecMod, (eConfigDelay .~ categorize (Just val)) ecVol)
>     examine (ecMod, ecVol) (AttackVolEnv, val)
>                                          = (ecMod, (eConfigAttack .~ categorize (Just val)) ecVol)
>     examine (ecMod, ecVol) (HoldVolEnv, val)
>                                          = (ecMod, (eConfigHold .~ categorize (Just val)) ecVol)
>     examine (ecMod, ecVol) (DecayVolEnv, val)
>                                          = (ecMod, (eConfigDecay .~ categorize (Just val)) ecVol)
>     examine (ecMod, ecVol) (ReleaseVolEnv, val)
>                                          = (ecMod, (eConfigRelease .~ categorize (Just val)) ecVol)
>
>     examine (ecMod, ecVol) _             = (ecMod, ecVol)
>
>     upd                :: VB.Vector GenData → GenEnum → Maybe Int → VB.Vector GenData
>     upd is ge val_                       = 
>       let
>         ix                               = fromEnum ge
>         val                              = fromMaybe 0 val_
>         genData                          = is VB.! ix
>
>         inrange                          = maybe True probe (genData ^. (gStatic . gClip))
>           where
>             probe      :: (Int, Int) → Bool
>             probe clip                   = inRange clip val
>         (good, wild)                     = if inrange
>                                              then (IntSet.insert val (genData ^. gGoodValues), genData ^. gWildValues)
>                                              else (genData ^. gGoodValues, IntSet.insert val (genData ^. gWildValues))
>         genData'                         = (  (gOccur +~ 1)
>                                             . (gAccum +~ val)
>                                             . (gGoodValues .~ good)
>                                             . (gWildValues .~ wild)) genData
>       in
>         VB.update is $ VB.singleton (ix, genData')
>
>     shredGen           :: VB.Vector GenData → F.Generator → VB.Vector GenData
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
> addEnvMaps             :: Map EConfig Int → Map EConfig Int → Map EConfig Int
> addEnvMaps                               = Map.unionWith (+)
>
> addGenSums             :: GenSum → GenSum → GenSum
> addGenSums (GenSum typ tag gd1 _ mod1 vol1) (GenSum _ _ gd2 _ mod2 vol2)
>                                          =
>   GenSum
>      typ 
>      tag
>      (VB.zipWith addGenDatas gd1 gd2)
>      VB.empty -- WOX
>      (addEnvMaps mod1 mod2)
>      (addEnvMaps vol1 vol2)

The End
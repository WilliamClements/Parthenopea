> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

ShredFile
William Clements
May 28, 2026

> module Eng.ShredFile (
>         rollupGenSums, shredFile ) where
>
> import qualified Codec.SoundFont         as F
> import Control.Lens hiding ( element, ix )
> import Data.Array.Unboxed
> import qualified Data.Bifunctor          as BF
> import Data.IntMap.Strict ( IntMap )
> import qualified Data.IntMap.Strict      as IntMap
> import Data.IntSet ( IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import qualified Data.Text               as Text
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec

Getting down to business ==============================================================================================
   
From one SoundFont file we produce one GenSum, to collect interesting data about Generators.

Later on, a rollup GenSum will be produced - i. e.,per-file GenSums added together. Similarly, GenSums 
for Zones are rolled up to Instrument GenSums, and Instrument GenSums are rolled up to File GenSums.

> shredFile              :: SFFileBoot → IO GenSum
> shredFile sffile                         = do
>   let vInstGenSum                        = VB.fromList ((IntMap.elems . IntMap.mapWithKey shredInst) owners)
>   return $ rollupGenSums GSFileLevel ftag vInstGenSum
>   where
>     ftag                                 = "f " ++ sffile.zFilename
>
>     shredInst          :: Int → IntSet → GenSum
>     shredInst kinst bags                 = rollupGenSums GSInstLevel itag vZoneGenSum
>       where
>         itag           :: String         = "i " ++ Text.unpack (Text.pack $ fixName $ F.instName (loadInst kinst))
> 
>         vZoneGenSum                      = VB.fromList $ map shredSlate slates
>
>         shredSlate     :: GenSlate → GenSum
>         shredSlate slate                 =
>           let
>             ztag       :: String         = "z " ++ Text.unpack (Text.pack $ fixName $ F.sampleName (loadShdr ksample))
>
>             modMap, volMap
>                        :: Map EConfig Int
>             (modMap, volMap)             = (mark Map.empty modEC, mark Map.empty volEC)
>                                              where (modEC, volEC) = VB.foldl' examineIf (initEC, initEC) slate
>
>             ksample    :: Int            = round $ (slate VB.! fromEnum SampleIndex) ^. gAccum
>           in
>             makeGenSum GSZoneLevel ztag slate VB.empty 1 modMap volMap
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
>
>                 gens                     = VB.generate (jgen - igen) (loadGen . (+ igen))
>           in
>             map shredBag (IntSet.toList bags)
>
>         slates                           = 
>           let
>             firstZone  :: GenSlate       = head slates_
>             globalZone :: Bool           = isEmptyGenData $ firstZone VB.! fromEnum SampleIndex
>
>             replace    :: GenData → GenData → GenData
>             replace gz oz
>               | not gzE && ozE           = gz
>               | otherwise                = oz
>               where
>                 gzE                      = isEmptyGenData gz
>                 ozE                      = isEmptyGenData oz
>           in
>             if globalZone
>               then map (VB.zipWith replace firstZone) (tail slates_)
>               else slates_        
>
>     loadInst kinst                       = sffile.zFileArrays.ssInsts ! fromIntegral kinst
>     loadBag kbag                         = sffile.zFileArrays.ssIBags ! fromIntegral kbag
>     loadShdr kshdr                       = sffile.zFileArrays.ssShdrs ! fromIntegral kshdr
>     loadGen kgen                         = sffile.zFileArrays.ssIGens ! fromIntegral kgen
>
>     owners             :: IntMap IntSet
>     owners                               = foldl' erect IntMap.empty [ii..(ij-1)]
>       where
>         (wi, wj)                         = bounds sffile.zFileArrays.ssInsts
>         (ii, ij)       :: (Int, Int)     = BF.bimap fromIntegral fromIntegral (wi, wj)
>
>         erect m kinst                    =
>           let
>             iinst, jinst
>                        :: F.Inst
>             iinst                        = loadInst kinst
>             jinst                        = loadInst (kinst + 1)
>
>             ibag, jbag :: Int
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
>                                               else examine acc (genData ^. gGen, genData ^. gAccum)
>
>     examine            :: (EConfig, EConfig) → (GenEnum, Double) → (EConfig, EConfig)
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
>     upd is ge valMaybe                   = VB.update is $ VB.singleton (ix, genData')
>       where
>         ix                               = fromEnum ge
>         genData                          = is VB.! ix
>         spec                             = specVector VB.! ix
>         staticClip_                      = spec ^. gClip
>         staticClip                       = fromJust staticClip_
>
>         val_                             = fromJust valMaybe
>         val
>           | isNothing valMaybe           = spec ^. gDefault
>           | isNothing staticClip_        = val_
>           | otherwise                    = clip staticClip val_
>         dVal           :: Double         = fromIntegral val
>
>         wild_                            = genData ^. gWildValues
>         wild
>            | isNothing valMaybe          = wild_
>            | val == val_                 = wild_
>            | otherwise                   = IntSet.insert val_ wild_
>
>         genData'                         = (  (gOccur +~ 1)
>                                             . (gAccum +~ fromIntegral val)
>                                             . (gAccumSquares +~ (dVal * dVal))
>                                             . (gWildValues .~ wild)) genData
>
>     shredGen           :: VB.Vector GenData → F.Generator → VB.Vector GenData
>         
>     -- 0..4
>     shredGen is (F.StartAddressOffset val)
>                                          = upd is StartAddressOffset (Just val)
>     shredGen is (F.EndAddressOffset val)
>                                          = upd is EndAddressOffset (Just val)
>     shredGen is (F.LoopStartAddressOffset val)
>                                          = upd is LoopStartAddressOffset (Just val)
>     shredGen is (F.LoopEndAddressOffset val)
>                                          = upd is LoopEndAddressOffset (Just val)
>     shredGen is (F.StartAddressCoarseOffset val)
>                                          = upd is StartAddressCoarseOffset (Just val)
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
>     shredGen is (F.EndAddressCoarseOffset val)
>                                          = upd is EndAddressCoarseOffset (Just val)
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
>         -- 25..32
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
>     shredGen is (F.KeyToModEnvHold val)
>                                          = upd is KeyToModEnvHold (Just val)
>     shredGen is (F.KeyToModEnvDecay val)
>                                          = upd is KeyToModEnvDecay (Just val)
>         
>         -- 33..41
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
>     shredGen is (F.LoopStartAddressCoarseOffset val)
>                                          = upd is LoopStartAddressCoarseOffset (Just val)
>     shredGen is (F.Key val)
>                                          = upd is Key ((Just . fromIntegral) val)
>     shredGen is (F.Vel val)
>                                          = upd is Vel ((Just . fromIntegral) val)
>     shredGen is (F.InitAtten val)
>                                          = upd is InitAtten (Just val)
>         
>         -- 50..54
>     shredGen is (F.LoopEndAddressCoarseOffset val)
>                                          = upd is LoopEndAddressCoarseOffset (Just val)
>     shredGen is (F.CoarseTune val)
>                                          = upd is CoarseTune (Just val)
>     shredGen is (F.FineTune val)
>                                          = upd is FineTune (Just val)
>     shredGen is (F.SampleIndex val)
>                                          = upd is SampleIndex ((Just . fromIntegral) val)
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
> rollupGenSums          :: GenSumLevel → String → VB.Vector GenSum → GenSum
> rollupGenSums toLevel tagRollup vGenSum  =
>   let
>     addGenSums         :: GenSum → GenSum → GenSum
>     addGenSums (GenSum level tagAdd slate1 _ nz1 mod1 vol1) (GenSum _ _ slate2 _ nz2 mod2 vol2)
>                                          =
>       GenSum
>         level 
>         tagAdd
>         (VB.zipWith addGenDatas slate1 slate2)
>         VB.empty
>         (nz1 + nz2)
>         (Map.unionWith (+) mod1 mod2)
>         (Map.unionWith (+) vol1 vol2)
>
>     sublevel           :: GenSumLevel    = toEnum (fromEnum toLevel - 1)
>     chadd              :: GenSum → GenSum → GenSum
>     chadd gensum1 gensum2
>       | gensum1 ^. gsLevel /= sublevel || gensum2 ^. gsLevel /= sublevel
>                                          = error "rollupGenSums: bad bookkeeping"
>       | otherwise                        = addGenSums gensum1 gensum2
>   in
>     (    (gsLevel .~ toLevel)
>     .      (gsTag .~ tagRollup)
>     .  (gsSubSums .~ vGenSum)) $ VB.foldl' chadd (VB.head vGenSum) (VB.tail vGenSum)

The End
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
> import qualified Data.Bifunctor          as BF
> import Control.Lens hiding ( element, ix )
> import qualified Control.Monad           as CM
> import Data.Array.Unboxed
> import Data.IntMap.Strict ( IntMap )
> import qualified Data.IntMap.Strict      as IntMap
> import Data.IntSet ( IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.List
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import Data.Ord
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec
> import qualified System.FilePattern.Directory
>                                          as FP
  
The Generator types are numbered 0 to 60. =============================================================================

The per-file diagnostic data includes:
1. vector of 61 per-generator infos - each with
   a. static data from the spec (clip and default)
   b. occurrence count
   c. all values encountered, partitioned by out-of-range
2. envelope statistics summed up over all instruments in the file

We list out both sets of data for each file, then list out the rollup sets.

> data StaticData where
>   StaticData :: {_gClip :: Maybe (Int, Int)
>                , _gDefault :: Int} → StaticData
>   deriving (Eq, Show)
>
> data GenData                             =
>   GenData {
>     _gId               :: GenEnum
>   , _gStatic           :: StaticData
>   , _gOccur            :: Int
>   , _gAccum            :: Int
>   , _gAccumWith        :: Int
>   , _gValues           :: IntSet
>   , _gWild             :: IntSet}
>   deriving (Eq, Show)
> makeGenData            :: GenEnum → GenData
> makeGenData ge                           =
>   let
>     (mclip, def)                         = allClipVector VB.! fromEnum ge
>   in
>     GenData ge (StaticData mclip def) 0 0 0 IntSet.empty IntSet.empty
>
> data GenSum                              =
>   GenSum {
>     _gsFilename        :: FilePath
>   , _gsGenData         :: VB.Vector GenData
>   , _gsModEnvMap       :: Map EConfig Int
>   , _gsVolEnvMap       :: Map EConfig Int}
>   deriving (Eq, Show)
> makeGenSum             :: FilePath → VB.Vector GenData → Map EConfig Int → Map EConfig Int → GenSum
> makeGenSum                               = GenSum
>
> makeLenses ''StaticData
> makeLenses ''GenData
> makeLenses ''GenSum
  
Executive =============================================================================================================

> batchProcessor         :: IO ()
> batchProcessor                           = do
>   timeThen                               ← getZonedTime
>   putStrLn $ show timeThen ++ "\n"
>   sf2s                                   ← FP.getDirectoryFilesIgnoreSlow "." ["*.sf2"] []
>
>   proceed sf2s
>
>   timeNow                                ← getZonedTime
>   putStrLn $ "\n" ++ show timeNow
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
> openInvestigation      :: VB.Vector SFFileBoot → IO (VB.Vector String)
> openInvestigation vFilesBoot             = do
>   vGenSum                                ← CM.mapM shredFile vFilesBoot
>   let gensOutput                         = VB.concatMap showFile vGenSum
>   let vRollup                            = VB.foldl' addGenSums (VB.head vGenSum) (VB.tail vGenSum)
>   let rollupOutput                       = showFile vRollup VB.++ showStats vRollup
>   return $ gensOutput VB.++ rollupOutput
>   where
>     showFile           :: GenSum → VB.Vector String
>     showFile gensum                      = VB.concat [
>       VB.fromList ["", gensum ^. gsFilename]
>       , VB.map show (gensum ^. gsGenData)
>       , showInstData (gensum ^. gsModEnvMap)
>       , showInstData (gensum ^. gsVolEnvMap)]
>     showInstData       :: Map EConfig Int → VB.Vector String
>     showInstData envMap
>       | Map.null envMap                  = VB.empty
>       | otherwise                        = VB.fromList (map (uncurry showMe) arrivals)
>         where
>           arrivals     :: [(Int, EConfig)]    
>           arrivals                       = sortBy (comparing Down) (Map.foldrWithKey shuffle [] envMap)
>                                               where shuffle ec count acc = (count, ec) : acc
>           showMe       :: Int → EConfig → String
>           showMe count ec                = unwords [show count, show ec]
>
>     showStats          :: GenSum → VB.Vector String
>     showStats gensum                     = VB.map showOneGen valueBearing
>       where
>         showOneGen ge                    = showTwoStats ((gensum ^. gsGenData) VB.! fromEnum ge)
>         showTwoStats genData             =
>           if IntSet.null (genData ^. gWild)
>              then s1
>              else unwords [s1, "   (", s2, ")"]
>           where
>             s1                           = unwords [show (genData ^. gId), showStat (genData ^. gValues)]
>             s2                           = showStat ((genData ^. gValues) `IntSet.union` (genData ^. gWild))
>         showStat is                      = if IntSet.null is then [] else unwords [show m, "+-", show s]
>           where
>             (m, s)                       = getStats (IntSet.toList is)
>             
> shredFile              :: SFFileBoot → IO GenSum
> shredFile sffile                         =
>   return $ makeGenSum sffile.zFilename v2 modMap volMap
>   where
>     loadInst kinst                       = sffile.zFileArrays.ssInsts ! fromIntegral kinst
>     loadBag kbag                         = sffile.zFileArrays.ssIBags ! fromIntegral kbag
>     loadGen kgen                         = sffile.zFileArrays.ssIGens ! fromIntegral kgen
>
>     owners             :: IntMap IntSet
>     owners                               = foldl' erect IntMap.empty [ii..(ij-1)]
>       where
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
>     populate           :: IntSet → Map EConfig Int → Map EConfig Int
>     populate bags m                      =
>       let
>         result, result'
>                        :: [(Bool, EConfig)]
>         result                           = map examineGens (IntSet.toList bags)
>
>         gz             :: EConfig
>         (gz, result')                    = if (fst . head) result
>                                             then ((snd . head) result, tail result)
>                                             else (makeEConfig Nothing Nothing Nothing Nothing Nothing, result)
>
>         mark           :: Map EConfig Int → (Bool, EConfig) → Map EConfig Int
>         mark mc (_, ec)                  = Map.insertWith (+) (addEConfigs gz ec) 1 mc
>       in
>         foldl' mark m result'
>
>     v0, v1, v2         :: VB.Vector GenData
>     v0                                   = VB.generate 61 (makeGenData . toEnum)
>     v1                                   = foldl' shredGen v0 sffile.zFileArrays.ssIGens
>     v2                                   = v1 -- WOX  VB.generate 61 (makeGenData . toEnum)
>
>     wi, wj             :: Word
>     ii, ij             :: Int
>     (wi, wj)                             = bounds sffile.zFileArrays.ssInsts
>     (ii, ij)                             = BF.bimap fromIntegral fromIntegral (wi, wj)
>
>     modMap, volMap     :: Map EConfig Int
>     modMap                               = IntMap.foldr populate Map.empty owners
>     volMap                               = IntMap.foldr populate Map.empty owners
>
>     examineGens        :: Int → (Bool, EConfig)
>     examineGens ibag                           =
>       let
>         igen, jgen     :: Int
>         igen                             = fromIntegral $ F.genNdx (loadBag ibag)
>         jgen                             = fromIntegral $ F.genNdx (loadBag (ibag + 1))
>
>         gens                             = VB.generate (jgen - igen) (loadGen . (+ igen))
>
>       in
>         VB.foldl' examine (False, makeEConfig Nothing Nothing Nothing Nothing Nothing) gens
>
>     examine            :: (Bool, EConfig) → F.Generator → (Bool, EConfig)
>     examine (_, ec) (F.SampleIndex _)
>                                          = (True, ec)
>     examine (b, ec) (F.DelayModEnv val)
>                                          = (b, (eConfigDelay .~ categorize (Just val)) ec)
>     examine (b, ec) (F.AttackModEnv val)
>                                          = (b, (eConfigAttack .~ categorize (Just val)) ec)
>     examine (b, ec) (F.HoldModEnv val)
>                                          = (b, (eConfigHold .~ categorize (Just val)) ec)      
>     examine (b, ec) (F.DecayModEnv val)
>                                          = (b, (eConfigDecay .~ categorize (Just val)) ec)
>     examine (b, ec) (F.ReleaseModEnv val)
>                                          = (b, (eConfigRelease .~ categorize (Just val)) ec)
>     examine (b, ec) _                    = (b, ec)
>
>     upd                :: VB.Vector GenData → GenEnum → Maybe Int → VB.Vector GenData
>     upd is ge val_                       = 
>       let
>         ix                               = fromEnum ge
>         val                              = fromMaybe 0 val_
>         genData                          = is VB.! ix
>         inrange                          = maybe True probe (genData ^. (gStatic . gClip))
>           where
>             probe      :: (Int, Int) → Bool
>             probe clip                   = inRange clip val
>         (values, wild)                   = if inrange
>                                              then (IntSet.insert val (genData ^. gValues), genData ^. gWild)
>                                              else (genData ^. gValues, IntSet.insert val (genData ^. gWild))
>         genData'                         = (  (gOccur +~ 1)
>                                             . (gAccum +~ val)
>                                             . (gValues .~ values)
>                                             . (gWild .~ wild)) genData
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
> addGenDatas            :: GenData → GenData → GenData
> addGenDatas gd1 gd2
>                                          =
>   GenData
>     (gd1 ^. gId)
>     (StaticData (gd1 ^. (gStatic . gClip)) (gd1 ^. (gStatic . gDefault)))
>     ((gd1 ^. gOccur) + (gd2 ^. gOccur))
>     ((gd1 ^. gAccum) + (gd2 ^. gAccum))
>     ((gd1 ^. gAccumWith) + (gd2 ^. gAccumWith))
>     ((gd1 ^. gValues) `IntSet.union` (gd2 ^. gValues))
>     ((gd1 ^. gWild) `IntSet.union` (gd2 ^. gWild))
>
> addInstDatas           :: Map EConfig Int → Map EConfig Int → Map EConfig Int
> addInstDatas                             = Map.unionWith (+)
>
> addGenSums             :: GenSum → GenSum → GenSum
> addGenSums (GenSum _ gd1 mod1 vol1) (GenSum _ gd2 mod2 vol2)
>                                          =
>   GenSum
>      "<rollup>"
>      (VB.zipWith addGenDatas gd1 gd2)
>      (addInstDatas mod1 mod2)
>      (addInstDatas vol1 vol2)
>
> getStats               :: [Int] → (Double, Double)
> getStats vals                            = (mean dubs, stdDevP dubs)
>   where
>     dubs               :: [Double]       = map fromIntegral vals
>
>     mean               :: Fractional a => [a] → a
>     mean xs                              = sum xs / realToFrac (length xs)
>     stdDevP            :: Floating a => [a] → a
>     stdDevP xs                           = sqrt (sum (map (\x -> (x - mean xs) ** 2) xs) / realToFrac (length xs))

The End
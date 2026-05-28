> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
June 16, 2025

> module Eng.Command (
>         batchProcessor ) where
>
> import qualified Codec.SoundFont         as F
> import Control.Lens hiding ( element, ix )
> import qualified Control.Monad           as CM
> import qualified Data.IntSet             as IntSet
> import Data.List
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict         as Map
> import Data.Ord
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec
> import Eng.ShredFile
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
>       vGenSum                            ← CM.mapM shredFile vFilesBoot
>       fData                              ← showResults vGenSum
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
> showResults       :: VB.Vector GenSum → IO (VB.Vector String)
> showResults vGenSum                      = do
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
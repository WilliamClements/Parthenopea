> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
May 18, 2026

> module Eng.Command (
>         batchProcessor ) where
>
> import qualified Codec.SoundFont         as F
> import qualified Control.Monad           as CM
> import Data.Time ( getZonedTime )
> import qualified Data.Vector.Strict      as VB
> import Eng.SFSpec
> import Eng.ShowResults
> import Eng.ShredFile
> import qualified System.FilePattern.Directory
>                                          as FP

Executive =============================================================================================================

> batchProcessor         :: IO ()
> batchProcessor                           = do
>   timeThen                               ← getZonedTime
>   putStrLn $ show timeThen ++ "\n"
>   sf2s                                   ← FP.getDirectoryFilesIgnoreSlow "." ["*.sf2"] []
>
>   if null sf2s
>     then putStrLn "No .sf2 files found."
>     else proceed sf2s
>
>   timeNow                                ← getZonedTime
>   putStrLn $ "\n" ++ show timeNow
>   putStrLn "\nThe End"
>
> proceed                :: [FilePath] → IO ()
> proceed sf2s                             = do
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   let vFilesBoot                         = VB.fromList extraction
>   vGenSum                                ← CM.mapM shredFile vFilesBoot
>   fData                                  ← showResults vGenSum
>   mapM_ putStrLn fData
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

The End
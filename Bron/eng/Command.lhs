> {-# LANGUAGE UnicodeSyntax #-}

Command
William Clements
May 18, 2026

> module Eng.Command (
>         batchProcessor ) where
>
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
>     else proceedWith sf2s
>
>   timeNow                                ← getZonedTime
>   putStrLn $ "\n" ++ show timeNow
>   putStrLn "\nThe End"
>
> proceedWith            :: [FilePath] → IO ()
> proceedWith sf2s                         = do
>   extraction                             ← CM.zipWithM openSoundFontFile [0..] sf2s
>   putStrLn $ unwords [show (length extraction), "extracted\n"]
>   let vFilesBoot                         = VB.fromList extraction
>   vGenSum                                ← CM.mapM shredFile vFilesBoot
>   showResults $ rollupGenSums GSRollLevel "<summary>" vGenSum

The End
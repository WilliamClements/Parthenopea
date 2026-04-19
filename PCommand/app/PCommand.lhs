> {-# LANGUAGE UnicodeSyntax #-}
>
> module Main where

This implements the main for PCommand.
Midi files found in current directory will be rendered. 
Jingle-list will be rendered also if named in the plus argument.

> import App.Rosters
> import Data.Map.Strict ( Map )
> import qualified Data.Map.Strict         as Map
> import Parthenopea.Music.Siren
> import Parthenopea.SoundFont.Command
> import Parthenopea.SoundFont.Directives
> import System.Environment
>
> main                   :: IO ()
> main                                     = do
>   args                                   ← getArgs
>   either doNormal doError (massage args)
>   where
>     dives                                =
>       defDirectives
>         {   client = "PCommand"
>           , dReportVerbosity = allOn}
>
>     jingleDB           :: Map String [Song]
>     jingleDB                             = makeJingleDB dives
>
>     doNormal                             = batchProcessor dives
>     doError                              = putStrLn
>
>     massage            :: [String] → Either [Song] String
>     massage args                         =
>       case length args of
>         0              → Left []
>         1              → if head (head args) == '+'
>                            then
>                              let
>                                mSongs    = head args `Map.lookup` jingleDB
>                              in
>                                case mSongs of
>                                  Nothing                 → Right $ badJingleList $ head args
>                                  Just songs              → Left songs
>                            else Right badArgs
>         _              → Right badArgs
>
>     badArgs                              =
>       "if any command line arguments are given, there must be exactly one and it must begin with plus (+) character"
>     badJingleList arg                    =
>       unwords [arg, "not found in", show (Map.keys jingleDB)]

> module Main where

The selected jingle-list will be rendered

> import Parthenopea.SoundFont.Command
> import Parthenopea.SoundFont.Directives
> import App.Rosters
>
> main                   :: IO ()
> main                                     = do
>   batchProcessor dives (selectedJingleList dives)
>   where
>     dives                                =
>       defDirectives
>         {   client = "PCommand"
>           , dReportVerbosity = allOn}

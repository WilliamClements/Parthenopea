> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE UnicodeSyntax #-}

ScanReport
William Clements
October 5, 2025

> module Parthenopea.SoundFont.ScanReport ( writeScanReport ) where
>
> import qualified Control.Monad           as CM
> import Data.List ( sortOn )
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Ord ( Down(Down) )
> import qualified Data.Vector.Strict      as VB
> import Debug.Trace ( traceIO )
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.SFSpec
  
executive =============================================================================================================

> writeScanReport        :: Directives → Rational → VB.Vector SFFileBoot → ResultDispositions → IO ()
> writeScanReport dives verbosity bootFiles rd
>                                          = do
>   CM.when diagnosticsEnabled             (traceIO $ unwords [fName, show rd])
>
>   -- output all selections to the report file
>   let esSampleSummary                    = summarize rd.preSampleDispos ++ [EndOfLine]
>   let esInstSummary                      = summarize rd.preInstDispos ++ [EndOfLine]
>   let esPreZoneSummary                   = summarize rd.preZoneDispos ++ [EndOfLine]
>   let esSampleScan                       = procMap rd.preSampleDispos ++ [EndOfLine]
>   let esInstScan                         = procMap rd.preInstDispos ++ [EndOfLine]
>   let esPreZoneScan                      = procMap rd.preZoneDispos ++ [EndOfLine]
>   let esTail                             = [EndOfLine, EndOfLine]
>
>   writeReportBySections
>     dives
>     reportScanName
>     ([esSampleSummary, esInstSummary, esPreZoneSummary]
>      ++ if verbosity < (1/3) then [] else [esSampleScan, esInstScan, esPreZoneScan]
>      ++ [esTail])
>   where
>     fName                                = "writeScanReport"
>
>     summarize          :: ∀ r . (SFKeyType r) ⇒ Map r [Scan] → [Emission]
>     summarize sm                         =
>       let
>         hs                               = sortOn (Down . snd) $ Map.toList $ Map.foldr histoFold Map.empty sm
>
>         histoFold ss mfold               = foldr (foldfun . getTriple) mfold ss
>           where foldfun dispo            = Map.insertWith (+) dispo 1
>             
>
>         emitHisto      :: ((Disposition, Impact, String), Int) → [Emission]
>         emitHisto ((dispo, impact, function), count)
>                                          =
>           [  emitShowL   count        16
>            , emitShowL   dispo        24
>            , emitShowL   impact       32
>            , ToFieldL    function     52
>            , EndOfLine]
>       in
>         concatMap emitHisto hs
>         
>     procMap sm                           = concat $ Map.mapWithKey procKey sm
>
>     procKey k ssIn                       = if null ssOut
>                                              then []
>                                              else prolog ++ [EndOfLine] ++ concatMap procScan ssIn ++ [EndOfLine]
>       where
>         ssOut                            = filter (\s → s.sDisposition `notElem` calcElideSet verbosity) ssIn
>         sffileBoot                       = bootFiles VB.! wfile k
>
>         prolog                           = 
>           [  Unblocked (show k)
>            , Blanks 5
>            , Unblocked sffileBoot.zFilename
>            , Blanks 5]
>           ++ kname k sffileBoot
>
>     procScan scan                    =
>       [  emitShowL scan.sDisposition 24
>        , emitShowL scan.sImpact      32
>        , ToFieldL scan.sFunction     52
>        , Unblocked scan.sClue
>        , EndOfLine]

The End
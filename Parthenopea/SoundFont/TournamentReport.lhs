> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE UnicodeSyntax #-}

TournamentReport
William Clements
October 5, 2025

> module Parthenopea.SoundFont.TournamentReport
>        ( printChoices
>        , showPerGM
>        , writeTournamentReport
>        ) where
>
> import qualified Control.Monad           as CM
> import Data.List
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Debug.Trace
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
>
> writeTournamentReport  :: Directives
>                           → VB.Vector SFFileBoot
>                           → (Map InstrumentName [PerGMScored], Map PercussionSound [PerGMScored])
>                           → IO ()
> writeTournamentReport dives vBootFiles (pContI, pContP)
>                        = do
>   CM.when diagnosticsEnabled             (traceIO $ unwords [fName, show $ (length pContI, length pContP)])
>   -- output all selections to the report file
>   let legend           =
>            emitComment     [   Blanks 66
>                             , ToFieldL "overall" 15
>                             , ToFieldL "hints" spacing
>                             , ToFieldL "stereo" spacing
>                             , ToFieldL "24-bit" spacing
>                             , ToFieldL "resoln" spacing
>                             , ToFieldL "confrm" spacing
>                             , ToFieldL "fuzzy" spacing
>                             , ToFieldR "raw fuzz" 14]
>         ++ emitNextComment (Blanks 81 : showWeights spacing)
>         where
>           spacing                        = 7
>   let esI              = concatMap (uncurry dumpContestants) (Map.toList pContI)
>   let esP              = concatMap (uncurry dumpContestants) (Map.toList pContP)
>   let eol              = singleton EndOfLine
>
>   writeReportBySections dives reportTournamentName [esFiles, legend, esI, eol, esFiles, legend, esP]
>
>   where
>     fName                                = "writeTournamentReport"
>
>     esFiles                              =
>       let
>         emitF sffile                     = [emitShowL sffile.zWordFBoot 7, emitShowL sffile.zFilename 54, EndOfLine]
>       in
>         VB.foldr (++) [] (VB.map emitF vBootFiles)

emit standard output text detailing what choices we made for rendering GM items =======================================

> printChoices           :: (Map InstrumentName GMChoices, Map PercussionSound GMChoices)
>                           → [InstrumentName]
>                           → [PercussionSound]
>                           → ([(Bool, [Emission])], [(Bool, [Emission])])
> printChoices (zI, zP) is ps              = (map (extract zI) is, map (extract zP) ps)
>   where
>     extract            :: ∀ a. (Ord a) ⇒ Map a GMChoices → a → (Bool, [Emission])
>     extract choices kind                 = (found, ems)
>                                              where GMChoices found _ ems = choices Map.! kind
>
> dumpContestants        :: ∀ a. (Ord a, Show a, SFScorable a) ⇒ a → [PerGMScored] → [Emission]
> dumpContestants kind contestants         = prolog ++ ex ++ epilog
>   where
>     prolog, ex, epilog :: [Emission]
>
>     prolog                               = emitLine [emitShowL kind 50]
>     ex                                   = concatMap dumpContestant contestants
>     epilog                               = emitLine []
>
> dumpContestant         :: PerGMScored → [Emission]
> dumpContestant scored                    = ex
>   where
>     showAkr            :: Double         = roundBy 10 scored.pAgainstKindResult
>     (showEmp, n)                         = showEmpiricals scored.pArtifactGrade.pEmpiricals
> 
>     ex = emitLine [ Blanks 4, emitShowL      scored.pPerGMKey.pgkwFile       8
>                             , ToFieldR       scored.szI                      22
>                   , Blanks 4, ToFieldR      (fromMaybe "" scored.mszP)       22
>                   , Blanks 4, emitShowL      scored.pArtifactGrade.pScore    15
>                             , ToFieldL       showEmp                         n
>                             , emitShowR      showAkr                         8]
>
> showEmpiricals         :: [Double] → (String, Int)
> showEmpiricals dubs                      = (concatMap fun dubs, (spacing + 1) * length dubs)
>   where
>     fun                :: Double → String
>     fun x                                = fillFieldL spacing (show $ roundBy 10 x)
>     spacing                              = 7

The End
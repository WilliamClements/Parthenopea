> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE UnicodeSyntax #-}

TournamentReport
William Clements
October 5, 2025

> module Parthenopea.SoundFont.TournamentReport ( printChoices, showPerGM, writeTournamentReport ) where
>
> import Data.List
> import Data.Map ( Map )
> import qualified Data.Map                as Map
> import Data.Maybe
> import qualified Data.Vector.Strict      as VB
> import Euterpea.Music
> import Parthenopea.Repro.Emission
> import Parthenopea.SoundFont.Runtime
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
>
> writeTournamentReport  :: Directives
>                           → VB.Vector SFFileBoot
>                           → Map InstrumentName [PerGMScored]
>                           → Map PercussionSound [PerGMScored]
>                           → IO ()
> writeTournamentReport dives vBootFiles pContI pContP
>                        = do
>   -- output all selections to the report file
>   let legend           =
>           emitComment     [   ToFieldL "hints" spacing
>                             , ToFieldL "stereo" spacing
>                             , ToFieldL "24-bit" spacing
>                             , ToFieldL "resolution" spacing
>                             , ToFieldL "conformant" spacing
>                             , ToFieldL "fuzzy" spacing]
>        ++ emitNextComment (showWeights spacing )
>         where
>           spacing                        = 16
>   let esI              = concatMap dumpContestants (Map.toList pContI)
>   let esP              = concatMap dumpContestants (Map.toList pContP)
>   let eol              = singleton EndOfLine
>
>   writeReportBySections dives reportTournamentName [esFiles, legend, esI, eol, esFiles, legend, esP]
>
>   where
>     esFiles                              =
>       let
>         emitF sffile                     = [emitShowL sffile.zWordFBoot 7, emitShowL sffile.zFilename 54, EndOfLine]
>       in
>         VB.foldr (++) [] (VB.map emitF vBootFiles)

emit standard output text detailing what choices we made for rendering GM items =======================================

> printChoices           :: SFRuntime
>                           → [InstrumentName]
>                           → [PercussionSound]
>                           → ([(Bool, [Emission])], [(Bool, [Emission])])
> printChoices runt is ps                  = (map (extract runt.zChoicesI) is, map (extract runt.zChoicesP) ps)
>   where
>     extract            :: ∀ a. (Ord a) ⇒ Map a (Bool, Maybe PerGMKey, [Emission]) → a → (Bool, [Emission])
>     extract choices kind                 = (found, ems)
>                                              where (found, _, ems) = choices Map.! kind
>
> showPerGM              :: PerGMScored → [Emission]
> showPerGM scored                         =
>   [emitShowL scored.pPerGMKey.pgkwFile 4] ++ [ToFieldL scored.szI 22] ++ showmZ
>   where
>     showmZ                               = maybe [] showZ scored.mszP
>     showZ name                           = [Unblocked name]
>
> dumpContestants        :: ∀ a. (Ord a, Show a, SFScorable a) ⇒ (a, [PerGMScored]) → [Emission]
> dumpContestants (kind, contestants)      = prolog ++ ex ++ epilog
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
> showEmpiricals dubs                      = (concatMap fun dubs, 7 * length dubs)
>   where
>     fun                :: Double → String
>     fun x                                = fillFieldL 6 (show $ roundBy 10 x)

The End
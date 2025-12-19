> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

SFSpec
William Clements
April 16, 2023

> module Parthenopea.SoundFont.Directives where
>
> import Data.Ratio ( (%) )
> import Parthenopea.SoundFont.Utility

configuration ("Directives") ==========================================================================================

> data ReportVerbosity                     =
>   ReportVerbosity {
>     dForRanges         :: Rational
>   , dForScan           :: Rational
>   , dForTournament     :: Rational} deriving (Eq, Show)
> okReportVerbosity      :: ReportVerbosity → Bool
> okReportVerbosity rv                     = 
>      inCanonicalRange rv.dForRanges
>   && inCanonicalRange rv.dForScan
>   && inCanonicalRange rv.dForTournament
>   where
>     inCanonicalRange                     = inARange (0::Rational, 1::Rational)
> allOn, allOff          :: ReportVerbosity
> allOn                                    =
>   ReportVerbosity 1 1 1
> allOff                                   =
>   ReportVerbosity 0 0 0
>
> data SynthSwitches                       =
>   SynthSwitches {
>     useEnvelopes       :: Bool
>   , usePassages        :: Bool
>   , useNoteBending     :: Bool
>   , usePitchCorrection :: Bool
>   , useAttenuation     :: Bool
>   , useLoopSwitching   :: Bool
>   , useReverb          :: Bool
>   , useChorus          :: Bool
>   , usePan             :: Bool
>   , useDCBlock         :: Bool
>   , noStereoNoPan      :: Bool
>   , normalizingOutput  :: Bool} deriving (Eq, Show)
> defSynthSwitches       :: SynthSwitches
> defSynthSwitches                         =
>   SynthSwitches
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>     True
>
> data Directives                          =
>   Directives {
>     client             :: String
>
>   , crossInstrumentPairing
>   , doAbsorption       :: Bool
>   , fixBadNames        :: Bool
>   , hackWildJumps      :: Bool
>   , hackWildMidiValues :: Bool
>   , multipleCompetes   :: Bool
>   , narrowRosterForBoot
>                        :: Bool
>   , narrowRosterForRuntime
>                        :: Bool
>   , parallelPairing    :: Bool
>   , switchBadStereoZonesToMono
>                        :: Bool
>
>   , proConRatio        :: Rational
>   , absorbThreshold    :: Rational
>
>   , synthSwitches      :: SynthSwitches
>
>   , dReportVerbosity   :: ReportVerbosity}
>   deriving (Eq, Show)

Client to specify Directives overrides, especially "client". ==========================================================

> defDirectives          :: Directives
> defDirectives                            =
>   baseDives
>     {   client                           = "sandbox"
>       , crossInstrumentPairing           = False
>       , hackWildJumps                    = True
>       , hackWildMidiValues               = True
>       , parallelPairing                  = False
>       , synthSwitches                    = baseDives.synthSwitches{useNoteBending = False}
>       , dReportVerbosity                 = allOn}

Override here only if this is a Parthenopea library sandbox. ==========================================================
For example:
        , narrowRosterForBoot = False
        , dReportVerbosity = allOff

>   where
>     baseDives                            =
>       Directives

Edit below if changing "default defaults" =============================================================================

>         ""
>         True
>         True
>         True
>         False
>         False
>         True
>         True
>         True
>         True
>         False
>         (3/4)
>         (4/5)
>         defSynthSwitches
>         allOff
>
> okDirectives           :: Directives → Bool
> okDirectives dives                       =
>   okReportVerbosity dives.dReportVerbosity
>   && inARange (1 % 10, 10 % 1) dives.proConRatio
>   && inARange (1 % 10,      1) dives.absorbThreshold -- lower threshold results in more absorptions permitted

Remarks on directives 16-Dec-2025:

1. crossInstrumentPairing and parallelPairing are intended to be on by default, as signified by their "default
   default" settings. Turned off because they do not _fully_ work yet. Design is good.
2. hackWildJumps and hackWildMidiValues are intended to be off by default. But it is difficult to guarantee no
   glitches would result, and anyway these cases typically work out OK if you eat the error.
3. dReportVerbosity is intended to be allOff by default for performance. Setting it to allOn for diagnostic value.
4. As you see, synthSwitches are intended to be all on, and they are. Occasions for overriding are troubleshooting
   experiments.

The End
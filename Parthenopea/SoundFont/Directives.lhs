> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

SFSpec
William Clements
April 16, 2023

> module Parthenopea.SoundFont.Directives (
>          allOn
>        , allOff
>        , defDirectives
>        , Directives(..)
>        , okDirectives
>        , ReportVerbosity(..)
>        , SynthSwitches(..) ) where
>
> import Data.Ratio ( (%) )
> import Parthenopea.SoundFont.Utility
  
configuration ("Directives") ==========================================================================================

> data ReportVerbosity                     =
>   ReportVerbosity {
>     dForPassage        :: Rational
>   , dForRanges         :: Rational
>   , dForScan           :: Rational
>   , dForTournament     :: Rational} deriving (Eq, Show)
> okReportVerbosity      :: ReportVerbosity → Bool
> okReportVerbosity rv                     = 
>      inCanonicalRange rv.dForPassage
>   && inCanonicalRange rv.dForRanges
>   && inCanonicalRange rv.dForScan
>   && inCanonicalRange rv.dForTournament
>   where
>     inCanonicalRange                     = inARange (0::Rational, 1::Rational)
> allOn, allOff          :: ReportVerbosity
> allOn                                    =
>   ReportVerbosity 1 1 1 1
> allOff                                   =
>   ReportVerbosity 0 0 0 0
>
> data SynthSwitches                       =
>   SynthSwitches {
>     useEnvelopes       :: !Bool
>   , usePassages        :: !Bool
>   , useInflections     :: !Bool
>   , useModulators      :: !Bool
>   , useDefModulators   :: !Bool
>   , useNoteBending     :: !Bool
>   , usePitchCorrection :: !Bool
>   , useAttenuation     :: !Bool
>   , useLoopSwitching   :: !Bool
>   , useReverb          :: !Bool
>   , useChorus          :: !Bool
>   , usePan             :: !Bool
>   , useDCBlock         :: !Bool
>   , useLFO             :: !Bool
>   , chorusRate         :: !Double
>   , chorusDepth        :: !Double
>   , noStereoNoPan      :: !Bool
>   , normalizingOutput  :: !Bool}
>   deriving (Eq, Show)

useLFO
-- False to suppress all uses of the low frequency oscillator

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
>     True
>     True
>     5.0
>     0.0001
>     True
>     True
>
> data Directives                          =
>   Directives {
>     client             :: !String
>
>   , crossInstrumentPairing
>   , doAbsorption       :: !Bool
>   , fixBadNames        :: !Bool
>   , hackWildJumps      :: !Bool
>   , hackWildMidiValues :: !Bool
>   , multipleCompetes   :: !Bool
>   , narrowRosterForBoot
>                        :: !Bool
>   , narrowRosterForRuntime
>                        :: !Bool
>   , parallelPairing    :: !Bool
>   , linklessPairing    :: !Bool
>   , switchBadStereoZonesToMono
>                        :: !Bool
>   , skipGlissandi      :: !Bool
>
>   , proConRatio        :: Rational
>   , absorbThreshold    :: Rational
>
>   , synthSwitches      :: !SynthSwitches
>
>   , dReportVerbosity   :: ReportVerbosity}
>   deriving (Eq, Show)

Client to specify Directives overrides, especially "client". ==========================================================

> defDirectives          :: Directives
> defDirectives                            =
>   baseDives
>     {   client                           = "sandbox"
>       , hackWildJumps                    = True
>       , hackWildMidiValues               = True
>       , synthSwitches                    = baseDives.synthSwitches{ useNoteBending = False }
>       , dReportVerbosity                 = allOn}

Override here only if this is a Parthenopea library sandbox. ==========================================================

>   where
>     baseDives                            =
>       Directives
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
>         True
>         False
>         False
>         (3/4)
>         (4/5)
>         defSynthSwitches
>         allOff
>
> okDirectives           :: Directives → Bool
> okDirectives Directives{ .. }
>                                          = 
>   okReportVerbosity dReportVerbosity
>   && okSwitches synthSwitches
>   && inARange (1 % 10, 10 % 1) proConRatio     -- higher ratio attenuates the con component
>   && inARange (1 % 10,      1) absorbThreshold -- lower threshold results in more absorptions permitted
>
> okSwitches             :: SynthSwitches → Bool
> okSwitches SynthSwitches{ .. }
>                                          =
>   (useModulators || not useDefModulators)
>   && inARange (0.1, 100) chorusRate
>   && inARange (0.00001, 1.1) chorusDepth

Remarks on directives and defaults 18-Mar-2026:

1. hackWildJumps and hackWildMidiValues are intended to be off by default. But we trade off in favor of a higher
   success rate. Typical glitches that are thus masked are usually not interesting. However, the harmful side of the
   tradeoff is loss in diagnostic clarity when chasing actual bugs.
2. dReportVerbosity is intended to be allOff by default for performance. Setting it to allOn for diagnostic value.
3. synthSwitches are intended to be all on. Currently opting out of note bending. Troubleshooting experiments are
   the usual reason for overriding something in synthSwitches.

The End
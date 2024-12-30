> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DerivingStrategies #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module SettingsDefs where
>
> import Control.Monad.IO.Class
> import Database.Persist.MongoDB
> import Database.Persist.TH
> import Language.Haskell.TH.Syntax
>
> let mongoSettings = mkPersistSettings (ConT ''MongoContext)
>   in
>     share [mkPersist mongoSettings] [persistLowerCase|
>       ControlSettings
>         qqDoRender                       Bool                        default = True
>         qqDiagnosticsEnabled             Bool                        default = False
>         qqReportTourney                  Bool                        default = True
>         qqSkipGlissandi                  Bool                        default = False
>         qqReplacePerCent                 Double                      default = 0
>         deriving Show
>
>       SynthSettings
>         qqUsePitchCorrection             Bool                        default = True
>         qqUseAttenuation                 Bool                        default = True
>         qqUseEnvelopes                   Bool                        default = True
>         qqUseLoopSwitching               Bool                        default = True
>         qqUseEffectReverb                Bool                        default = True
>         qqUseEffectChorus                Bool                        default = True
>         qqUseEffectPan                   Bool                        default = True
>         qqUseEffectDCBlock               Bool                        default = True
>         qqNoStereoNoPan                  Bool                        default = True
>         qqNormalizingOutput              Bool                        default = True
>         deriving Show
>
>       SoundFontSettings
>         qqAllowStereoCrossovers          Bool                        default = False
>         qqAllowOverlappingRanges         Bool                        default = True
>         qqAllowOutOfRange                Bool                        default = True
>         qqMultipleCompetes               Bool                        default = True
>         qqCombinePartials                Bool                        default = False
>         qqCanDevolveToMono               Bool                        default = True
>         deriving Show
>
>       ModulationSettings
>         qqUseModulators                  Bool                        default = True
>         qqChorusAllPerCent               Double                      default = 0
>         qqReverbAllPerCent               Double                      default = 0
>         qqUseDefaultMods                 Bool                        default = True
>         qqUseLFO                         Bool                        default = True
>         qqChorusRate                     Double                      default = 5.0
>         qqChorusDepth                    Double                      default = 0.001
>         qqCascadeConfig                  Int                         default = 0               
>         deriving Show
>
>       ScoringSettings
>         qqWeighHints                     Double                      default = 10
>         qqWeighStereo                    Double                      default = 2
>         qqWeigh24Bit                     Double                      default = 0
>         qqWeighResolution                Double                      default = 3/2
>         qqWeighConformance               Double                      default = 3
>         qqWeighFuzziness                 Double                      default = 3
>
>         qqNarrowInstrumentScope          Bool                        default = True
>         qqConRatio                       Double                      default = 3/4
>         qqSampleSizeMin                  Word                        default = 0 -- 48
>         qqInferStereo                    Bool                        default = False
>         qqRequiredZoneLinkage            Double                      default = 0
>
>         qqFFThresholdPossible            Double                      default = 50
>         qqFFThresholdStands              Double                      default = 150
>         qqFFThresholdConfirmed           Double                      default = 250
>         deriving Show
>
>       DiscreteSettings
>         qqBulgeDiv                       Double                      default = 20
>         qqDropoffRate                    Double                      default = 240
>         qqReverseSignal                  Bool                        default = True
>         qqDisableConvo                   Bool                        default = False
>         qqDisableMultiply                Bool                        default = False
>         qqUseFastFourier                 Bool                        default = True
>         qqCorrectDCOffset                Bool                        default = False
>         qqChopSignal                     Bool                        default = False
>         deriving Show
>
>       Config
>         name                            String
>         control                         ControlSettingsId
>         synth                           SynthSettingsId
>         soundfont                       SoundFontSettingsId
>         modulation                      ModulationSettingsId
>         scoring                         ScoringSettingsId
>         discrete                        DiscreteSettingsId 
>     |]

The End
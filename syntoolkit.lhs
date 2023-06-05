> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> 

Port (C++ to Haskell) of STK (called here Syntoolkit)

> module Syntoolkit where
>
> import Data.Array
> import Data.Word
> import Euterpea (InstrumentName(BrassSection))
   
> class Filter a where
>   ftick                :: StkFrames → Word64 → a → StkFrames 
>
> class Generator a where
>   gtick                :: StkFrames → Word64 → a → StkFrames 
>
> data ADSR =
>   ADSR
>   {
>       aTarget          :: Double
>     , aValue           :: Double
>     , aAttackRate      :: Double
>     , aDecayRate       :: Double
>     , aReleaseRate     :: Double
>     , aReleaseTime     :: Double
>     , aSustainLevel    :: Double
>   } deriving (Show, Eq, Ord)
>
> newADSR = ADSR 0 0 0.001 0.001 0.005 (-1) 0.5
>
> data EnvState =
>   ATTACK | DECAY | SUSTAIN | RELEASE | IDLE
>
> data Asymp =
>   Asymp
>   {
>     bValue             :: Double
>     , bTarget          :: Double
>     , bState           :: Int
>     , bFactor          :: Double
>     , bConstant        :: Double
>   } deriving (Show, Eq, Ord)
>
> newAsymp = Asymp 0.0 0.0 0 (exp( -1.0 / ( 0.3 * stkSampleRate ) )) 0.0
>
> stkSampleRate = 44100.0
>
> sampleRateChanged      :: Double → Double → Asymp → Asymp
> sampleRateChanged newR oldR asymp =
>   let
>     tau = (-1.0) / log (bFactor asymp) * oldR
>   in asymp { bFactor = exp( -1.0 / ( tau * newR ) )}
>
> keyOn, keyOff      :: Asymp → Asymp
> keyOn asymp = asymp { bTarget = 1.0 }
> keyOff asymp = asymp { bTarget = 0.0 }
>
> data BandedWG =
>   BandedWG
>   {
>       cDPluck          :: Bool
>     , cBowTable        :: BowTable
>     , cADSR            :: ADSR
>     , cFrequency       :: Double
>     , cPreset          :: Int
>     , cBowPosition     :: Int
>     , cBaseGain        :: Double
>     , cIntegrationConstant
>                        :: Double
>     , cTrackVelocity   :: Bool
>     , cBowVelocity     :: Double
>     , cBowTarget       :: Double
>     , cStrikeAmp       :: Double
>   } deriving (Show, Eq, Ord)
>
> newBandedWG = BandedWG True
>                        newBowTable {dSlope = 3.0}
>                        newADSR {- adsr_.setAllTimes( 0.02, 0.005, 0.9, 0.01 ); -}
>                        220.0
>                        0
>                        0
>                        0.999
>                        0
>                        False
>                        0
>                        0
>                        0
>
> data BowTable =
>   BowTable
>   {
>       dOffset          :: Double
>     , dSlope           :: Double
>     , dMinOutput       :: Double
>     , dMaxOutput       :: Double
>   } deriving (Show, Eq, Ord)
>
> newBowTable = BowTable 0.0 0.1 0.01 0.98
>
> data BeeThree =
>   BeeThree
>   {
>       eADSR            :: [ADSR]
>     , eWaves           :: Array Word FileLoop
>     , eVibrato         :: SineWave
>     -- WOX , eTwoZero         :: TwoZero
>     , eNOperators      :: Word64
>     , eBaseFrequency   :: Double
>     , eRatios          :: [Double]
>     , eGains           :: [Double]
>     , eModDepth        :: Double
>     , eControl1        :: Double
>     , eControl2        :: Double
>     , eFMGains         :: Array Word Double
>     , eFMSusLevels     :: Array Word Double
>     , eFMAttTimes      :: Array Word Double
>   } deriving (Show, Eq, Ord)
>
> newBeeThree = BeeThree initADSRs
>                        initWaves
>                        initVibrato
>                        -- WOX initTwoZero
>                        0
>                        0
>                        initRatios
>                        initGains
>                        0
>                        0
>                        0
>                        initFMGains
>                        initFMSusLevels
>                        initFMAttTimes
>   where
>     initADSRs = []
>     initWaves = array (0,4)
>                 [   (0, newFileLoop2 (stkRawWavePath ++ "sinewave.raw") True)
>                   , (1, newFileLoop2 (stkRawWavePath ++ "sinewave.raw") True)
>                   , (2, newFileLoop2 (stkRawWavePath ++ "sinewave.raw") True)
>                   , (3, newFileLoop2 (stkRawWavePath ++ "fwavblnk.raw") True)]
>     initVibrato = newSineWave
>     -- WOX initTwoZero = newTwoZero
>     initRatios = [0.999, 1.997, 3.006, 6.009]
>     initGains = [95, 95, 99, 95]
>     initFMGains = array (0,3) []
>     initFMSusLevels = array (0,3) []
>     initFMAttTimes = array (0,3) []
>
> stkRawWavePath = "../../rawwaves/"                                          
>
> data FileLoop =
>   FileLoop
>   {
>       fFirstFrame      :: StkFrames
>     , fPhaseOffset     :: Double
>   } deriving (Show, Eq, Ord)
>
> newFileLoop = FileLoop newStkFrames 0
> newFileLoop2           :: String → Bool → FileLoop
> newFileLoop2 filename raw = newFileLoop
>
> data StkFrames =
>   StkFrames
>   {
>       gData            :: [Double]
>     , gDataRate        :: Double
>     , gNFrames         :: Word64 
>     , gNChannels       :: Word
>     , gSize            :: Word64
>     , gBufferSize      :: Word64
>   } deriving (Show, Eq, Ord)
>
> newStkFrames = StkFrames [0]
>                          0
>                          0
>                          0
>                          0
>                          0
>
> globTable              :: StkFrames
> globTable = newStkFrames
>
> data SineWave =
>   SineWave
>   {
>       hTime            :: Double
>     , hRate            :: Double
>     , hPhaseOffset     :: Double
>     , hIIndex          :: Word64
>     , hAlpha           :: Double
>   } deriving (Show, Eq, Ord)
>
> newSineWave = SineWave 0 1 0 0 0
>
> data FilterData =
>   FilterData
>   {
>       jGain            :: Double
>     , jChannelsIn      :: Word64
>     , jLastFrame       :: StkFrames
>     , jB               :: [Double]
>     , jA               :: [Double]
>     , jOutputs         :: StkFrames
>     , jInputs          :: StkFrames
>   } deriving (Show, Eq, Ord)
>
> newFilterData = FilterData 0
>                            0
>                            newStkFrames
>                            [0,0,0]
>                            []
>                            newStkFrames
>                            newStkFrames
>
> data BiQuad =
>   BiQuad
>   {
>       kK               :: Double
>     , kKSqr            :: Double
>     , kDenom           :: Double
>     , kFilterData      :: FilterData
>   } deriving (Show, Eq, Ord)
>
> newBiQuad = BiQuad 0 0 1 newFilterData
>
> data Blit =
>   Blit
>   {
>       lNHarmonics      :: Word64
>     , lM               :: Word64
>     , lRate            :: Double
>     , lPhase           :: Double
>     , lP               :: Double
>   } deriving (Show, Eq, Ord)
>
> newBlit freq = Blit 0 0 0 0 0
>
> data BlitSaw =
>   BlitSaw
>   {
>       mNHarmonics      :: Word64
>     , mM               :: Word64
>     , mRate            :: Double
>     , mPhase           :: Double
>     , mP               :: Double
>     , mC2              :: Double
>     , mA               :: Double
>     , mState           :: Double
>   } deriving (Show, Eq, Ord)
>
> newBlitSaw freq = BlitSaw 0 0 0 0 0 0 0 0
>
> data BlitSquare =
>   BlitSquare
>   {
>       nNHarmonics      :: Word64
>     , nM               :: Word64
>     , nRate            :: Double
>     , nPhase           :: Double
>     , nP               :: Double
>     , nA               :: Double
>     , nLastBlitOutput  :: Double
>     , nDCBState        :: Double
>   } deriving (Show, Eq, Ord)
>
> newBlitSquare freq = BlitSquare 0 0 0 0 0 0 0 0
>
> data BlowBotl =
>   BlowBotl
>   {
>       oJetTable        :: JetTable
>     , oResonator       :: BiQuad
>     , oDCBlock         :: PoleZero
>     , oNoise           :: Noise
>     , oADSR            :: ADSR
>     , oVibrato         :: SineWave
>     , oMaxPressure     :: Double
>     , oNoiseGain       :: Double
>     , oVibratoGain     :: Double
>     , oOutputGain      :: Double
>   } deriving (Show, Eq, Ord)
>
> newBlowBotl = BlowBotl newJetTable
>                        newBiQuad
>                        newPoleZero
>                        newNoise
>                        newADSR
>                        newSineWave
>                        0
>                        20
>                        0
>                        0
>
> data BlowHole =
>   BlowHole
>   {
>       pDelays          :: Array Word DelayL
>     , pReedTable       :: ReedTable
>     , pFilter          :: OneZero
>     , pTonehole        :: PoleZero
>     , pVent            :: PoleZero
>     , pEnvelope        :: Envelope
>     , pNoise           :: Noise
>     , pVibrato         :: SineWave
>     , pScatter         :: Double
>     , pTHCoeff         :: Double
>     , pRHGain          :: Double
>     , pOutputGain      :: Double
>     , pNoiseGain       :: Double
>     , pVibratoGain     :: Double
>   } deriving (Show, Eq, Ord)
>
> data JetTable =
>   JetTable
>   {
>   } deriving (Show, Eq, Ord)
>
> newJetTable = JetTable
>   
> data Noise =
>   Noise
>   {
>   } deriving (Show, Eq, Ord)
>
> newNoise = Noise
>
> data Envelope =
>   Envelope
>   {
>       qValue           :: Double
>     , qTarget          :: Double
>     , qRate            :: Double
>     , qState           :: Int
>   } deriving (Show, Eq, Ord)
>
> newEnvelope = Envelope 0
>                        0
>                        0
>                        0
>
> data PoleZero =
>   PoleZero
>   {
>   } deriving (Show, Eq, Ord)
>
> newPoleZero = PoleZero
>
> data ReedTable =
>   ReedTable
>   {
>       rOffset          :: Double
>     , rSlope           :: Double
>   } deriving (Show, Eq, Ord)
>
> newReedTable = ReedTable 0
>                          0
>
> data DelayL =
>   DelayL
>   {
>       sInPoint         :: Word64
>     , sOutPoint        :: Word64
>     , sDelay           :: Double
>     , sAlpha           :: Double
>     , sOmAlpha         :: Double
>     , sNextOutput      :: Double
>     , sDoNextOut       :: Bool
>   } deriving (Show, Eq, Ord)
>
> newDelayL delay = DelayL 0
>                          0
>                          delay
>                          0
>                          0
>                          0
>                          True
>
> data OneZero =
>   OneZero
>   {
>   } deriving (Show, Eq, Ord)
>
> newOneZero = OneZero
>
> data Bowed =
>   Bowed
>   {
>       tNeckDelay       :: DelayL
>     , tBridgeDelay     :: DelayL
>     , tBowTable        :: BowTable
>     , tStringFilter    :: OnePole
>     , tBodyFilters     :: Array Word BiQuad
>     , tVibrato         :: SineWave
>     , tADSR            :: ADSR
>     , tBowDown         :: Bool
>     , tMaxVelocity     :: Double
>     , tBaseDelay       :: Double
>     , tVibratoGain     :: Double
>     , tBetaRatio       :: Double
>   } deriving (Show, Eq, Ord)
>
> data OnePole =
>   OnePole
>   {
>   } deriving (Show, Eq, Ord)
>
> data Brass =
>   Brass
>   {
>       uDelayLine       :: DelayL
>     , uLipFilter       :: BiQuad
>     , uDCBlock         :: PoleZero
>     , uADSR            :: ADSR
>     , uVibrato         :: SineWave
>     , uLipTarget       :: Double
>     , uSlideTarget     :: Double
>     , uVibratoGain     :: Double
>     , uMaxPressure     :: Double
>   } deriving (Show, Eq, Ord)
>
> data Chorus =
>   Chorus
>   {
>       vDelayLine       :: Array Word DelayL
>     , vMods            :: Array Word SineWave
>     , vBaseLength      :: Double
>     , vModDepth        :: Double
>   } deriving (Show, Eq, Ord)
>
> data Clarinet =
>   Clarinet
>   {
>       wDelayLine       :: DelayL
>     , wReedTable       :: ReedTable
>     , wFilter          :: OneZero
>     , wEnvelope        :: Envelope
>     , wNoise           :: Noise
>     , wVibrato         :: SineWave
>     , wOutputGain      :: Double
>     , wNoiseGain       :: Double
>     , wVibratoGain     :: Double
>   } deriving (Show, Eq, Ord)
>
> {-
> newClarinet lowestFrequency =
>               Clarinet newDelayL $ 1 + toInteger (0.5 * stkSampleRate / lowestFrequency)
>                        newReedTable
>                        newOneZero
>                        newEnvelope
>                        newNoise
>                        newSineWave
>                        1.0
>                        0.2
>                        0.1
> -}
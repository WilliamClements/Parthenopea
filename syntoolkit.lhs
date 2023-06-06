> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
> {-# HLINT ignore "Use head" #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE UnicodeSyntax #-}
> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> 

Port (C++ to Haskell) of STK (called here Syntoolkit)

> module Syntoolkit where
>
> import Data.Array
> import Data.Int
> import Data.Word
> import System.Random
   
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
> data FM =
>   FM
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
> newBeeThree = FM       initADSRs
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
>     , gNFrames         :: Word 
>     , gNChannels       :: Word
>     , gSize            :: Word64
>     , gBufferSize      :: Word64
>   } deriving (Show, Eq, Ord)
>
> newStkFrames = StkFrames []
>                          0
>                          0
>                          0
>                          0
>                          0
>
> newStkFrames1          :: StkFrames → [Double] → StkFrames
> newStkFrames1 stkFrames theData = stkFrames {gData = theData}
>
> newStkFrames2          :: Word → Word → StkFrames
> newStkFrames2 numFrames numChannels =
>   let
>     size, bufferSize    :: Word64
>     size = fromIntegral $ numFrames * numChannels
>     bufferSize = size
>     dataRate           :: Double
>     dataRate = stkSampleRate
>   in
>     StkFrames [] dataRate numFrames numChannels size bufferSize
>
> -- WOX globTable              :: StkFrames
> -- WOX globTable = newStkFrames2
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
>     , oNoise           :: Noise Int
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
>                        (newNoise 1171)
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
>     , pNoise           :: Noise Int
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
> instance Generator (Noise a) where
>   gtick                :: StkFrames → Word64 → Noise a → StkFrames
>   gtick frames chan noise = frames'
>     where
>       data' = map newSample [0..(fromIntegral (gNFrames frames))]
>       -- sample = (gData stkFrames) !! fromIntegral w
>       frames'       :: StkFrames
>       frames' = frames {gData = data'}
>       newSample        :: Int → Double
>       newSample i = 2.0 * fst (random (ggStdGen noise))
>
> newtype GeneratorData =
>   GeneratorData
>   {
>     ooLastFrame        :: StkFrames
>   } deriving (Show, Eq, Ord)
>
> data Noise a =
>   Noise
>   {
>       ggStdGen           :: StdGen
>     , ggGeneratorData    :: GeneratorData
>   } deriving Show
>
> instance Eq a => Eq (Noise a) where a == b = True
> instance Ord a => Ord (Noise a) where compare n1 n2 = EQ
>
> newNoise               :: Int → Noise a
> newNoise seed = Noise (mkStdGen seed) (GeneratorData newStkFrames)
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
>     , wNoise           :: Noise Int
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
>
> data Delay =
>   Delay
>   {
>       xInPoint         :: Word64
>     , xOutPoint        :: Word64
>     , xDelay           :: Word64
>   } deriving (Show, Eq, Ord)
>
> newDelay delay maxDelay = Delay 0 0 delay
>
> data DelayA = 
>   DelayA
>   {
>       yInPoint         :: Word64
>     , yOutPoint        :: Word64
>     , yDelay           :: Double
>     , yAlpha           :: Double
>     , yCoeff           :: Double
>     , yAPInput         :: Double
>     , yNextOutput      :: Double
>     , yNextOut         :: Bool
>   } deriving (Show, Eq, Ord)
>
> data Drummer =
>   Drummer
>   {
>       zWaves           :: Array Word FileWvIn
>     , zFilters         :: Array Word OnePole
>     , zSoundOrder      :: [Int]
>     , zSoundNumber     :: [Int]
>     , zNSounding       :: Int
>   } deriving (Show, Eq, Ord)
>
> data FileWvIn =
>   FileWvIn
>   {
>       aaFile           :: FileRead
>     , aaFinished       :: Bool
>     , aaInterpolate    :: Bool
>     , aaInt2FloatScaling
>                        :: Bool
>     , aaChunking       :: Bool
>     , aaTime           :: Double
>     , aaRate           :: Double
>     , aaFileSize       :: Word64
>     , aaChunkThreshold :: Word64
>     , aaChunkSize      :: Word64
>     , aaChunkPointer   :: Int64
>   } deriving (Show, Eq, Ord)
>
> data FileRead =
>   FileRead
>   {
>       bbFd             :: File
>     , bbByteSwap       :: Bool
>     , bbWavFile        :: Bool
>     , bbFileSize       :: Word64
>     , bbDataOffset     :: Word64
>     , bbChannels       :: Word
>     , bbDataType       :: StkFormat
>     , bbFileRate       :: Double
>   } deriving (Show, Eq, Ord)
>
> type StkFormat = Word64
>
> data Echo =
>   Echo
>   {
>       ccDelayLine      :: Delay
>     , ccLength         :: Word64
>   } deriving (Show, Eq, Ord)
>
> data FileWrite =
>   FileWrite
>   {
>       ddFd             :: File
>     , ddFileType       :: FileType
>     , ddDataType       :: StkFormat
>     , ddChannels       :: Word64
>     , ddFrameCounter   :: Word64
>     , ddByteSwap       :: Bool
>   } deriving (Show, Eq, Ord)
>
> type File = Word64
> type FileType = Word64
>
> data FileWvOut =
>   FileWvOut
>   {
>       eeFile           :: FileWrite
>     , eeBufferFrames   :: Word64
>     , eeBufferIndex    :: Word64
>     , eeIData          :: Int64
>   } deriving (Show, Eq, Ord)
>
> data Fir =
>   Fir
>   {
>   } deriving (Show, Eq, Ord)
>
> data Flute =
>   Flute
>   {
>       ffJetDelay       :: DelayL
>     , ffBoreDelay      :: DelayL
>     , ffJetTable       :: JetTable
>     , ffFilter         :: OnePole
>     , ffDCBlock        :: PoleZero
>     , ffNoise          :: Noise Int
>     , ffADSR           :: ADSR
>     , ffVibrato        :: SineWave
>     , ffLastFrequency  :: Double
>     , ffMaxPressure    :: Double
>     , ffJetReflection  :: Double
>     , ffEndReflection  :: Double
>     , ffNoiseGain      :: Double
>     , ffVibratoGain    :: Double
>     , ffOutputGain     :: Double
>     , ffJetRatio       :: Double
>   } deriving (Show, Eq, Ord)
>
> data FMVoices =
>   FMVoices
>   {
>       ggCurrentVowel   :: Int
>     , ggTilt           :: Array Word Double
>     , ggMods           :: Array Word Double
>   } deriving (Show, Eq, Ord)
>
> data FormSweep =
>   FormSweep
>   {
>       hhDirty          :: Bool
>     , hhFrequency      :: Double
>     , hhRadius         :: Double
>     , hhStartFrequency :: Double
>     , hhStartRadius    :: Double
>     , hhStartGain      :: Double
>     , hhTargetFrequency
>                        :: Double
>     , hhTargetRadius   :: Double
>     , hhTargetGain     :: Double
>     , hhDeltaFrequency :: Double
>     , hhDeltaRadius    :: Double
>     , hhDeltaGain      :: Double
>     , hhSweepState     :: Double
>     , hhSweepRate      :: Double
>   } deriving (Show, Eq, Ord)
>
> data FreeVerb =
>   FreeVerb
>   {
>       iiG              :: Double
>     , iiGain           :: Double
>     , iiRoomSizeMem    :: Double
>     , iiRoomSize       :: Double
>     , iiDampMem        :: Double
>     , iiDamp           :: Double
>     , iiWet1           :: Double
>     , iiWet2           :: Double
>     , iiDry            :: Double
>     , iiWidth          :: Double
>     , iiFrozenMode     :: Bool
>     , iiCombDelayL     :: Array Word Delay
>     , iiCombDelayR     :: Array Word Delay
>     , iiCombLPL        :: Array Word OnePole
>     , iiCombLPR        :: Array Word OnePole
>     , iiAllPassDelayL  :: Array Word Delay
>     , iiAllPassDelayR  :: Array Word Delay
>   } deriving (Show, Eq, Ord)
>
> data GrainState =
>       GRAIN_STOPPED
>     | GRAIN_FADEIN
>     | GRAIN_SUSTAIN
>     | GRAIN_FADEOUT deriving (Show, Eq, Ord)
>
> data Grain =
>   Grain
>   {
>       jjScaler         :: Double
>     , jjRate           :: Double
>     , jjAttackCount    :: Word64
>     , jjSustainCount   :: Word64
>     , jjDecayCount     :: Word64
>     , jjDelayCount     :: Word64
>     , jjCounter        :: Word64
>     , jjPointer        :: Double
>     , jjStartPointer   :: Word64
>     , jjRepeats        :: Word64
>     , jjState          :: GrainState
>   } deriving (Show, Eq, Ord)
>
> data Granulate =
>   Granulate
>   {
>       kkData           :: StkFrames
>     , kkGrains         :: [Grain]
>     , kkNoise          :: Noise Int
>     , kkGPointer       :: Double
>     , kkGDuration      :: Word64
>     , kkGRampPercent   :: Word64
>     , kkGDelay         :: Word64
>     , kkGStretch       :: Word64
>     , kkStretchCounter :: Word64
>     , kkGOffset        :: Int64
>     , kkGRandomFactor  :: Double
>     , kkGain           :: Double
>   } deriving (Show, Eq, Ord)
>
> data Guitar =
>   Guitar
>   {
>       mmStrings        :: Array Word Twang
>     , mmStringState    :: Array Word Int
>     , mmDecayCounter   :: Array Word Word64
>     , mmFilePointer    :: Array Word Word64
>     , mmPluckGains     :: Array Word Double
>     , mmPickFilter     :: OnePole
>     , mmCouplingFilter :: OnePole
>     , mmCouplingGain   :: Double
>     , mmExcitation     :: StkFrames
>     , mmLastFrame      :: StkFrames
>   } deriving (Show, Eq, Ord)
>
> data Twang =
>   Twang
>   {
>       lldelayLine      :: DelayA
>     , llcombDelay      :: DelayL
>     , llLoopFilter     :: Fir
>     , llLastOutput     :: Double
>     , llFrequency      :: Double
>     , llLoopGain       :: Double
>     , llPluckPosition  :: Double
>   } deriving (Show, Eq, Ord)
>
> data JCRev =
>   JCRev
>   {
>       nnAllpassDelays  :: Array Word Delay
>     , nnCombDelays     :: Array Word Delay
>     , nnCombFilters    :: Array Word OnePole
>     , nnOutLeftDelay   :: Delay
>     , nnOutRightDelay  :: Delay
>     , nnAllpassCoefficient
>                        :: Double
>     , nnCombCoefficient:: Array Word Double
>   } deriving (Show, Eq, Ord)

sample programs ===========================================================================

> noiseMain              :: IO ()
> noiseMain = do
>   -- output               :: StkFrames
>   let output :: StkFrames = newStkFrames2 20 1
>   let noise = newNoise 737
>   let output' = gtick output 0 noise
>   mapM_ (doOutput output') [0..19]
>   where
>     doOutput           :: StkFrames → Int → IO ()
>     doOutput output' i = putStr $ show $ gData output' !! i
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

SFSpec
William Clements
April 16, 2023

> module Parthenopea.SoundFont.SFSpec where
>
> import qualified Codec.SoundFont         as F
> import Data.Array.Unboxed
> import qualified Data.Audio              as A
> import Data.Char
> import Data.Int ( Int8, Int16 )
> import Data.IntSet (IntSet)
> import qualified Data.IntSet             as IntSet
> import Data.List
> import Data.Ratio ( (%) )
> import Data.Time
> import Euterpea.IO.MIDI.GeneralMidi ( )
> import Parthenopea.Repro.Emission
> import Parthenopea.Repro.Smashing
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Utility
  
implementing SoundFont spec ===========================================================================================

> type SampleIndex                         = Word
> type InstIndex                           = Int
> type BagIndex                            = Word
> type Fuzz                                = Double
>
> data SampleType =
>   SampleTypeMono
>   | SampleTypeRight
>   | SampleTypeLeft
>   | SampleTypeLinked
>   | SampleTypeOggVorbis
>   | SampleTypeRomMono
>   | SampleTypeRomRight
>   | SampleTypeRomLeft
>   | SampleTypeRomLinked deriving (Eq, Show)
>
> toSampleType           :: Word → SampleType
> toSampleType hex                         = deJust "toMaybeSampleType" (toMaybeSampleType hex)
>
> toMaybeSampleType      :: Word → Maybe SampleType
> toMaybeSampleType n                      =
>   case n of
>     0x0                                  → Just SampleTypeMono
>     0x1                                  → Just SampleTypeMono
>     0x2                                  → Just SampleTypeRight
>     0x4                                  → Just SampleTypeLeft
>     0x8                                  → Just SampleTypeLinked
>     0x10                                 → Just SampleTypeOggVorbis
>     0x8001                               → Just SampleTypeRomMono
>     0x8002                               → Just SampleTypeRomRight
>     0x8004                               → Just SampleTypeRomLeft
>     0x8008                               → Just SampleTypeRomLinked
>     _                                    → Nothing
>
> fromSampleType             :: SampleType → Word
> fromSampleType stype =
>   case stype of
>     SampleTypeMono                       → 0x1
>     SampleTypeRight                      → 0x2
>     SampleTypeLeft                       → 0x4
>     SampleTypeLinked                     → 0x8
>     SampleTypeOggVorbis                  → 0x10
>     SampleTypeRomMono                    → 0x8001
>     SampleTypeRomRight                   → 0x8002
>     SampleTypeRomLeft                    → 0x8004
>     SampleTypeRomLinked                  → 0x8008
>
> data PerGMKey                            =
>   PerGMKey {
>     pgkwFile           :: !Int
>   , pgkwInst           :: !Word
>   , pgkwBag            :: !(Maybe Word)}
>   deriving (Eq, Ord, Show)
> stdPerGMKey            :: Int → Int → PerGMKey
> stdPerGMKey wFile wInst                  =
>   PerGMKey
>     wFile
>     (fromIntegral wInst)
>     Nothing
>
> data ChangeNameItem                      = FixBadName deriving Eq
>
> data ChangeName a                        =
>   ChangeName {
>     cnSource           :: a
>   , cnChanges          :: [ChangeNameItem]
>   , cnName             :: String}
>
> data PreSampleKey                        =
>   PreSampleKey {
>     pskwFile           :: !Int
>   , pskwSampleIndex    :: !Word}
>   deriving (Eq, Ord, Show)
> type PreSample                           = ChangeName F.Shdr
>
>
> data PreZoneKey                          =
>   PreZoneKey {
>     pzkwFile           :: !Int
>   , pzkwInst           :: !Word
>   , pzkwBag            :: !Word
>   , pzkwSampleIndex    :: !Word}
>   deriving (Eq, Ord, Show)
>
> data FileArrays                          = 
>   FileArrays {
>     ssInsts            :: Array Word F.Inst
>   , ssIBags            :: Array Word F.Bag
>   , ssIGens            :: Array Word F.Generator
>   , ssIMods            :: Array Word F.Mod
>   , ssShdrs            :: Array Word F.Shdr}
>
> effPSShdr              :: PreSample → F.Shdr
> effPSShdr ps                             = ps.cnSource{F.sampleName = ps.cnName}
> isLeftPS               :: PreSample → Bool
> isLeftPS ps                              = Just SampleTypeLeft == toMaybeSampleType (effPSShdr ps).sampleType
> isRightPS              :: PreSample → Bool
> isRightPS ps                             = Just SampleTypeRight == toMaybeSampleType (effPSShdr ps).sampleType
>
> data PerInstrument                       =
>   PerInstrument {
>     piChanges          :: ChangeName F.Inst
>   , pOwned             :: IntSet
>   , pCrossing          :: IntSet
>   , pSmashing          :: Smashing Word}
> allBixen, ownedOnly    :: PerInstrument → IntSet
> allBixen perI                            = perI.pOwned `IntSet.union` perI.pCrossing
> ownedOnly perI                           = perI.pOwned
> instance Show PerInstrument where
>   show perI                              = unwords ["PerInstrument", show (perI.pOwned, perI.pCrossing)]
>
> data SampleArrays                        = 
>   SampleArrays {
>     ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
> type AgainstKindResult                   = Double

bootstrapping =========================================================================================================

> writeReportBySections  :: Directives → FilePath → [[Emission]] → IO ()
> writeReportBySections dives fp eSections   = do
>   tsStarted                              ← getZonedTime
>   let prolog                             = [Unblocked (show tsStarted), EndOfLine, EndOfLine]
>   let epilog                             = [EndOfLine, Unblocked $ show dives]
>   writeFileBySections fp ([prolog] ++ eSections ++ [epilog, theEnd])
>
>   putStr $ reapEmissions [Unblocked $ unwords ["wrote", fp], EndOfLine]
>
> howClose               :: ∀ j . (Eq j) ⇒ [j] → [j] → Rational
> howClose js0 js1
>   | null js0 || null js1                 = 0
>   | otherwise                            = genericLength commonPrefix % genericLength js1
>   where
>     commonPrefix                         = takeWhile (uncurry (==)) (zip js0 js1)
>
> goodChar               :: Char → Bool
> goodChar cN                              = isAscii cN && not (isControl cN)
>
> goodName               :: String → Bool
> goodName name                            = not (null name) && all goodChar name
>
> fixName                :: String → String
> fixName name
>   | null name                            = "<noname>"
>   | otherwise                            = map (\cN → if goodChar cN then cN else '_') name

Returning rarely-changed but otherwise hard-coded names; e.g. Tournament Report.

> reportPassageName, reportRangesName, reportScanName, reportTournamentName
>                        :: FilePath
> reportPassageName                        = "Passage.report"
> reportRangesName                         = "Ranges.report"
> reportScanName                           = "Scan.report"
> reportTournamentName                     = "Tournament.report"

The End
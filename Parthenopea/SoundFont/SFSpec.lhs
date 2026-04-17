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
> import qualified Data.Map.Lazy           as Lazy
> import Data.Map.Strict ( Map )
> import qualified Data.Map.Strict         as Map
> import Data.Ratio ( (%) )
> import Data.Time
> import Euterpea.IO.MIDI.GeneralMidi ( )
> import Euterpea.Music
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
>     0x0                    → Just SampleTypeMono
>     0x1                    → Just SampleTypeMono
>     0x2                    → Just SampleTypeRight
>     0x4                    → Just SampleTypeLeft
>     0x8                    → Just SampleTypeLinked
>     0x10                   → Just SampleTypeOggVorbis
>     0x8001                 → Just SampleTypeRomMono
>     0x8002                 → Just SampleTypeRomRight
>     0x8004                 → Just SampleTypeRomLeft
>     0x8008                 → Just SampleTypeRomLinked
>     _                      → Nothing
>
> fromSampleType             :: SampleType → Word
> fromSampleType stype =
>   case stype of
>     SampleTypeMono         → 0x1
>     SampleTypeRight        → 0x2
>     SampleTypeLeft         → 0x4
>     SampleTypeLinked       → 0x8
>     SampleTypeOggVorbis    → 0x10
>     SampleTypeRomMono      → 0x8001
>     SampleTypeRomRight     → 0x8002
>     SampleTypeRomLeft      → 0x8004
>     SampleTypeRomLinked    → 0x8008
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
> data Disposition                         =
>   Accepted | Modified | Violated | Rescued | Dropped | NoChange
>   deriving (Eq, Ord, Show)
>
> data Impact                              =
>   ToProgram | ToCache | Winner
>      | NoZones | BadZones
>      | BadName
>      | BadSampleRate | BadSampleType | BadSampleLimits
>      | BadAppliedLimits | BadStereoPartner | RomBased | BadGMRange 
>      | Paired | Orphaned
>      | Absorbing | Absorbed | NoAbsorption    
>      | Unrecognized | Narrow | NoPercZones
>      | Captured | Adopted | AdoptedAsMono | GlobalZone
>   deriving (Eq, Ord, Show)
>
> virginrd               :: ResultDispositions
> virginrd                                 = ResultDispositions 
>                                              Lazy.empty 
>                                              Lazy.empty 
>                                              Lazy.empty
>
> data Scan                                =
>   Scan {
>     sDisposition       :: !Disposition
>   , sImpact            :: !Impact
>   , sFunction          :: !String
>   , sClue              :: !String}
>   deriving (Eq, Show)
>
> data ResultDispositions                  =
>   ResultDispositions {
>     preSampleDispos    :: Lazy.Map PreSampleKey     [Scan]
>   , preInstDispos      :: Lazy.Map PerGMKey         [Scan]
>   , preZoneDispos      :: Lazy.Map PreZoneKey       [Scan]}
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
> data FFMatches =
>   FFMatches {
>     ffInput            :: !String
>   , ffInst             :: Map InstrumentName Fuzz
>   , ffPerc             :: Map PercussionSound Fuzz} deriving Show
>
> data SampleArrays                        = 
>   SampleArrays {
>     ssData             :: A.SampleData Int16
>   , ssM24              :: Maybe (A.SampleData Int8)}
> data Matches                             =
>   Matches {
>     mSMatches          :: Lazy.Map PreSampleKey FFMatches
>   , mIMatches          :: Lazy.Map PerGMKey FFMatches}
> defMatches             :: Matches
> defMatches                               = Matches Lazy.empty Lazy.empty
> combineMatches         :: Matches → Matches → Matches
> combineMatches m1 m2                     =
>   m1{  mSMatches                         = Lazy.union m1.mSMatches    m2.mSMatches
>      , mIMatches                         = Lazy.union m1.mIMatches    m2.mIMatches}
>
> type AgainstKindResult                   = Double
> 
> data ArtifactGrade =
>   ArtifactGrade {
>     pScore             :: !Int
>   , pEmpiricals        :: [Double]}
>   deriving (Show)
>
> weighHints             :: Rational
> weighStereo            :: Rational
> weigh24Bit             :: Rational
> weighResolution        :: Rational
> weighConformance       :: Rational
> weighFuzziness         :: Rational
>
> ssWeights              :: [Double]
> ssWeights                                = [ fromRational weighHints
>                                            , fromRational weighStereo
>                                            , fromRational weigh24Bit
>                                            , fromRational weighResolution
>                                            , fromRational weighConformance
>                                            , fromRational weighFuzziness ]
> showWeights            :: Int → [Emission]
> showWeights spacing                      = concatMap (\weight → [emitShowL weight spacing]) ssWeights
>
> weighHints                               = 10
> weighStereo                              = 2
> weigh24Bit                               = 0
> weighResolution                          = 3/2
> weighConformance                         = 3
> weighFuzziness                           = 3

bootstrapping =========================================================================================================

> getTriple              :: Scan → (Disposition, Impact, String)
> getTriple s                              = (s.sDisposition, s.sImpact, s.sFunction)
>
> noClue                 :: String
> noClue                                   = ""
>
> calcElideSet           :: Rational → [Disposition]
> deadset, rescueset     :: [Disposition]    -- a given list is filtered down to the three Dispositions
>                                            -- then we count "membership" per distinct Impact
>                                            -- ...dead if any counts are odd
> deadset                                  = [Violated, Dropped, Rescued]
>                                            -- the following only optionally appear in scan report
> calcElideSet dive                        = if dive < (1/2)
>                                              then [Accepted, Modified, NoChange]
>                                              else []
> rescueset                                = [Rescued]
>
> surveyDispositions     :: [Disposition] → [Scan] → Map Impact Int
> surveyDispositions dns                   = foldr survFold Map.empty
>   where
>     survFold           ::  Scan → Map Impact Int → Map Impact Int
>     survFold s m
>       | s.sDisposition `elem` dns        = Map.insertWith (+) s.sImpact 1 m
>       | otherwise                        = m
>
> dead                   :: [Scan] → Bool
> dead ss                                  =
>   surveyDispositions [Violated, Dropped] ss /= surveyDispositions [Rescued] ss
>
> hasImpact              :: Impact → [Scan] → Bool
> hasImpact impact                         = any (\s → s.sImpact == impact)
>
> wasRescued             :: Impact → [Scan] → Bool
> wasRescued impact                        = any (\s → s.sDisposition `elem` rescueset && s.sImpact == impact)
>
> badButMaybeFix         :: ∀ a. (Show a) ⇒ Bool → Impact → String → a → a → [Scan]
> badButMaybeFix doFix imp fName bad good  =
>   if doFix
>     then [viol, resc]
>     else [viol]
>   where
>     viol                                 = Scan Violated imp fName (show bad)
>     resc                                 = Scan Rescued imp fName (show good)
>
> instance Show ResultDispositions where
>   show rd                                =
>     unwords [  "ResultDispositions"
>              , show (length rd.preSampleDispos, length rd.preInstDispos, length rd.preZoneDispos)]
>
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
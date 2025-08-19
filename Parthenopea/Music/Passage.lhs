> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Passage
William Clements
August 15, 2025

> module Parthenopea.Music.Passage where
>
> import Data.List
> import Data.Maybe
> import qualified Data.Vector             as VB
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren

Notation-Driven Dynamics ==============================================================================================

_Marking_                = composer's passage-shape directive, sequence
_Overall                 = 
   chamge rate = onset time and dur *versus* start and end velocity

> type SelfIndex                           = Int
> type EventIndex                          = Int
>
> data Marking                             =
>     Mark StdLoudness
>   | ReMark StdLoudness
>   | Rest1
>   | Span1
>   | SpanN Int
>   deriving (Eq, Show)
> expandMarkings         :: [Marking] → [Marking]
> expandMarkings                           = concatMap expand
>   where
>     expand (SpanN n)                     = replicate n Span1
>     expand mark                          = [mark]
>
> data Overall                             =
>   Overall {
>     oStartT            :: Double
>   , oEndT              :: Double
>   , oStartV            :: Double
>   , oEndV              :: Double}
>   deriving (Eq, Show)
> makeOverall            :: Double → Double → MEvent → MEvent → Maybe Overall
> makeOverall startV endV startEv endEv    = Just $ Overall startT endT startV endV
>   where
>     startT                               = fromRational startEv.eTime
>     endT                                 = fromRational endEv.eTime + fromRational endEv.eDur
> changeRate             :: Overall → Double
> changeRate over                          = (over.oEndV - over.oStartV) / (over.oEndT - over.oStartT)
> velocity               :: Double → Overall → Double
> velocity tIn over                        = over.oStartV + tIn * changeRate over
> twoVelos               :: Double → Double → Overall → VB.Vector Double
> twoVelos onset delta over                = VB.fromList
>                                            [ velocity onset           over
>                                            , velocity (onset + delta) over]
> fourVelos              :: Double → Double → Overall → Overall → VB.Vector Double
> fourVelos onset delta over0 over1        = VB.fromList
>                                            [ velocity onset           over0
>                                            , velocity (onset + delta / 2) over0
>                                            , velocity (onset + delta / 2) over1
>                                            , velocity (onset + delta) over1]
>
> data MekNote                             =
>   MekNote {
>     mSelfIndex         :: SelfIndex
>   , mPrimitive         :: Primitive Pitch
>   , mMarking           :: Marking
>   , mEvent             :: Maybe MEvent
>   , mOverall           :: Maybe Overall
>   , mParams            :: VB.Vector Double}
>   deriving (Eq, Show)
> inflection             :: MekNote → Bool
> inflection mek                           = case mek.mMarking of
>                                              ReMark _              → False
>                                              _                     → True
> changeParams           :: MekNote → VB.Vector Double → (SelfIndex, MekNote)
> changeParams mek dubs                    = (mek.mSelfIndex, mek{mParams = dubs})
> getMarkLoudness        :: MekNote → Maybe StdLoudness
> getMarkLoudness mek                      =
>   case mek.mMarking of
>     Mark x                               → Just x
>     ReMark x                             → Just x
>     _                                    → Nothing
> getMarkNode            :: MekNote → VB.Vector SelfIndex
> getMarkNode mek                          =
>   case mek.mMarking of
>     Mark _                               → VB.singleton mek.mSelfIndex
>     ReMark _                             → VB.singleton mek.mSelfIndex
>     _                                    → VB.empty
> makeMekNote            :: Int → Primitive Pitch → Marking → MekNote
> makeMekNote six prim marking
>   | restProblem                          = error $ unwords ["Non-corresponding rests"]
>   | otherwise                            =
>     MekNote six prim marking Nothing Nothing VB.empty
>   where
>     restProblem                          =
>       case prim of
>         Rest _                           → marking /= Rest1
>         _                                → False
> mekWidth               :: MekNote → Int
> mekWidth mek                             =
>   case mek.mPrimitive of
>     Note _ _                             → 1
>     Rest _                               → 0
>
> passage                :: BandPart → [Marking] → Music Pitch → Music1
> passage bp markings ma
>   | not enableDynamics                   = toMusic1 ma
>   | null markings                        = error $ unwords ["empty markings"]
>   | otherwise                            = removeZeros $ passageImpl bp (expandMarkings markings) (removeZeros ma)
>
> passageImpl            :: BandPart → [Marking] → Music Pitch → Music1
> passageImpl bp markings ma
>   | traceIf trace_IP False               = undefined
>   | otherwise                            = VB.foldl' final (rest 0) enriched
>   where
>     fName__                              = "passageImpl"
>     trace_IP                             = unwords [fName__, show markings]
>
>     -- reconstruct notes with added dynamics metadata
>     final              :: Music1 → MekNote → Music1 
>     final music mek                      =
>       music :+: case mek.mPrimitive of
>                   Note durI pitchI       → note durI (pitchI, [Dynamics fName__, (Params . VB.toList) mek.mParams])
>                   Rest durI              → rest durI
>
>     -- flatten ma to zippable list of the notes and rests
>     prims              :: [Primitive Pitch]
>     prims                                =
>       let
>         pFun           :: Primitive Pitch → [Primitive Pitch]
>         pFun (Note pP dP)                = [Note pP dP]
>         pFun (Rest dP)                   = [Rest dP]
>       in
>         mFold pFun (++) undefined undefined ma
>
>     -- capture performance events from ma
>     eTable                               = VB.fromList $ fst $ musicToMEvents (bandPartContext bp) (toMusic1 ma)
>
>     -- evolve enriched note/rest (MekNote) list
>     rawMeks, withEvents, withOveralls, enriched
>                        :: VB.Vector MekNote
>    
>     rawMeks                               =
>       profess
>         -- assertion
>         (not (null prims) && (length prims == length markings))
>         (unwords ["bad lengths; prims, markings", show $ length prims, show $ length markings])
>         (VB.fromList $ zipWith3 makeMekNote [0..] prims markings)
>
>     withEvents                           = fst $ foldl' implant (VB.empty, 0) rawMeks
>       where
>         implant (meks, soFar) mek        = (meks VB.++ VB.singleton mek{mEvent = setEvent}, soFar + mekWidth mek)
>           where
>             setEvent                     = case mek.mPrimitive of
>                                              Note _ _              → Just (eTable VB.! soFar)
>                                              Rest _                → Nothing
>
>     (nodePairs, nodePairGroups)          = formNodeGroups
>       where
>         formNodeGroups :: (VB.Vector (SelfIndex, SelfIndex), VB.Vector (VB.Vector (SelfIndex, Int)))
>         formNodeGroups                   = (nps, npgs)
>           where
>             nodes                        = VB.foldl' append VB.empty withEvents
>                                              where append sis mek = sis VB.++ getMarkNode mek
>             nps                          = VB.zip nodes (VB.tail nodes)
>             npgs                         = VB.fromList $ VB.groupBy inflected nps
>                                              where inflected (_, si1) (_, _) = inflection (withEvents VB.! si1)
>
>     withOveralls                         = withEvents `VB.update` lode
>       where
>         lode                             = VB.concatMap computeOverall nodePairs
>
>         computeOverall (si0, si1)        = VB.singleton (si0, mek0{mOverall = makeOverall loud0 loud1 ev0 ev1})
>           where
>             fName                        = "computeOverall"
>
>             mek0                         = withEvents VB.! si0
>             mek1                         = withEvents VB.! si1
>
>             ev0                          = deJust (unwords [fName, "0"]) mek0.mEvent
>             ev1                          = deJust (unwords [fName, "1"]) mek1.mEvent
>
>             loud0                        = (loudness . fromJust . getMarkLoudness) mek0
>             loud1                        = (loudness . fromJust . getMarkLoudness) mek1
>
>     enriched                             = withOveralls `VB.update` lode
>       where
>         lode                             = VB.concatMap enrich nodePairGroups
>         fence                            = VB.length withOveralls - 1
>
>         enrich nodePairGroup             =
>           let
>             richen (si0, si1)            = VB.map infuse lmeks
>               where
>                 lmeks                    = VB.slice si0 (si1 - si0 + 1 - fencePost) withOveralls
>                                              where fencePost = if si1 == fence then 0 else 1
>                 infuse mek               =
>                   let
>                     fName                = "infuse"
>
>                     si                   = mek.mSelfIndex
>                     isInflection         =
>                       length nodePairGroup > 2 && isJust (VB.find inflex (VB.slice 1 fence nodePairGroup))
>                       where inflex nodePair = si == fst nodePair
>                     ev                   = deJust fName mek.mEvent
>                     onset                = fromRational ev.eTime
>                     delta                = fromRational ev.eDur
>                     over0                = deJust fName (VB.head lmeks).mOverall
>                     over1                =
>                       case find (\np → snd np == mek.mSelfIndex) nodePairs of
>                         Just np          → fromJust (withOveralls VB.! fst np).mOverall
>                         Nothing          → error $ unwords [fName, "inflection has no previous node !?"]
>                   in
>                     if isInflection
>                       then changeParams mek (fourVelos onset delta over0 over1)
>                       else changeParams mek (twoVelos onset delta over0)
>            in
>              VB.concatMap richen nodePairGroup
>
> data Answers                             =
>   Answers {
>     aNoteBend          :: VB.Vector Double
>   , a2Velos            :: VB.Vector Double
>   , a4Velos            :: VB.Vector Double} deriving (Eq, Show)
> setFromAnswers         :: Answers → VB.Vector Double
> setFromAnswers answers                   = answers.aNoteBend VB.++ answers.a2Velos VB.++ answers.a4Velos
> getAnswers             :: VB.Vector Double → Answers
> getAnswers vParams                       = 
>    case length vParams of
>      0 → Answers VB.empty VB.empty VB.empty
>      1 → Answers vParams VB.empty VB.empty
>      2 → Answers VB.empty vParams VB.empty
>      3 → Answers (VB.slice 0 1 vParams) (VB.slice 1 2 vParams) VB.empty
>      4 → Answers VB.empty VB.empty vParams
>      5 → Answers (VB.slice 0 1 vParams) VB.empty (VB.slice 1 4 vParams)
>      _ → error "invalid params length"
> addNoteBend, add2Velos, add4Velos
>                        :: VB.Vector Double → VB.Vector Double → VB.Vector Double
> addNoteBend nb vParams                   = setFromAnswers (getAnswers vParams){aNoteBend = nb}
> add2Velos vs vParams                     = setFromAnswers (getAnswers vParams){a2Velos = vs}
> add4Velos vs vParams                     = setFromAnswers (getAnswers vParams){a4Velos = vs}

Configurable parameters ===============================================================================================

> enableDynamics         :: Bool

Edit the following ====================================================================================================

> enableDynamics                           = True

The End
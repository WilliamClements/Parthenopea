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

> module Parthenopea.Music.Passage(
>          add2Velos
>        , add4Velos
>        , addNoteBend
>        , Answers(..)
>        , expandMarkings
>        , formNodeGroups
>        , getAnswers
>        , makeMekNote
>        , Marking(..)
>        , MekNote(..)
>        , passage) where
>
> import Data.List ( foldl' )
> import qualified Data.Vector             as VB
> import Euterpea.IO.MIDI.MEvent ( MEvent(eDur, eTime), musicToMEvents )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren ( bandPartContext, BandPart(..), stdVelocity)
> import Parthenopea.Repro.Modulation ( clip )
> import Parthenopea.SoundFont.SFSpec ( Velocity )

Notation-Driven Dynamics ==============================================================================================

_Marking_                = composer's passage-shape directive, sequence
_Overall                 = 
   chamge rate = onset time and dur *versus* start and end velocity

> type SelfIndex                           = Int
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
> velocity tIn over                        = clip (0, 128) (over.oStartV + tIn * changeRate over)
> twoVelos               :: Double → Double → Overall → VB.Vector Double
> twoVelos onset delta over                = VB.fromList
>                                            [ velocity onset               over
>                                            , velocity (onset + delta)     over]
> fourVelos              :: Double → Double → Overall → Overall → VB.Vector Double
> fourVelos onset delta over0 over1        = VB.fromList
>                                            [ velocity onset               over0
>                                            , velocity (onset + delta / 2) over0
>                                            , velocity (onset + delta / 2) over1
>                                            , velocity (onset + delta)     over1]
>
> data MekNote                             =
>   MekNote {
>     mSelfIndex         :: SelfIndex
>   , mPrimitive         :: Primitive Pitch
>   , mMarking           :: Marking
>   , mEvent             :: Maybe MEvent
>   , mOverall           :: Maybe Overall
>   , mParams            :: Either Velocity (VB.Vector Double)}
>   deriving (Eq, Show)
> inflection             :: MekNote → Bool
> inflection mek                           =
>   case mek.mMarking of
>     ReMark _                             → False
>     _                                    → True
> changeParams           :: MekNote → Either Velocity (VB.Vector Double) → (SelfIndex, MekNote)
> changeParams mek val                     = (mek.mSelfIndex, mek{mParams = val})
> getMarkVelocity        :: MekNote → Velocity
> getMarkVelocity mek                      =
>   case mek.mMarking of
>     Mark x                               → stdVelocity x
>     ReMark x                             → stdVelocity x
>     _                                    → error "no mark velocity"
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
>     MekNote six prim marking Nothing Nothing (Right VB.empty)
>   where
>     restProblem                          =
>       case prim of
>         Rest _                           → marking /= Rest1
>         _                                → False
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
>     fName_                               = "passageImpl"
>     trace_IP                             = unwords [fName_, show markings]
>
>     -- reconstruct notes with added dynamics metadata
>     final              :: Music1 → MekNote → Music1 
>     final music mek                      =
>       music :+: case mek.mPrimitive of
>                   Note durI pitchI       → note durI (pitchI, Dynamics fName_ : makeNas mek.mParams)
>                   Rest durI              → rest durI
>       where
>         makeNas        :: Either Velocity (VB.Vector Double) → [NoteAttribute]
>         makeNas (Left homeVolume)        = [Volume homeVolume]
>         makeNas (Right directive)        = [(Volume . average) directive, (Params . VB.toList) directive]
>         
>         average        :: VB.Vector Double → Velocity
>         average directive                = round $ VB.sum directive / (fromIntegral . length) directive
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
>         implant (meks, soFar) mek        = (meks VB.++ VB.singleton mek{mEvent = setEvent}, soFar + mekWidth)
>           where
>             setEvent                     = 
>               case mek.mPrimitive of
>                 Note _ _                 → Just (eTable VB.! soFar)
>                 Rest _                   → Nothing
>             mekWidth                     = 
>               case mek.mPrimitive of
>                 Note _ _                 → 1
>                 Rest _                   → 0
>
>     (nodePairs, nodeGroups)              = formNodeGroups withEvents
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
>             loud0                        = (fromIntegral . getMarkVelocity) mek0
>             loud1                        = (fromIntegral . getMarkVelocity) mek1
>
>     enriched                             = withOveralls `VB.update` lode
>       where
>         lode                             = VB.concatMap enrich nodeGroups
>         mekFence                         = VB.length withOveralls - 1
>
>         enrich         :: VB.Vector SelfIndex → VB.Vector (SelfIndex, MekNote)
>         enrich nodeGroup
>           | gLen == 0                    = error "no nodes in node group"
>           | gLen == 1                    = VB.fromList [(mek0.mSelfIndex, mek0{mParams = (Left . getMarkVelocity) mek0})]
>           | otherwise                    = VB.concatMap richen groupNodePairs
>           where
>             gLen                         = length nodeGroup
>             mek0                         = withOveralls VB.! (nodeGroup VB.! 0)
>             groupNodePairs               = VB.zip nodeGroup (VB.tail nodeGroup)
>
>             richen (si0, si1)
>               | traceNow trace_R False   = undefined
>               | otherwise                = VB.map infuse segMeks
>               where
>                 fName                    = "richen"
>                 trace_R                  = unwords [fName, show (si0, si1)]
> 
>                 segMeks                  = VB.slice si0 (si1 - si0 + 1 - fencePost) withOveralls
>                                              where fencePost = if si1 == mekFence then 0 else 1
>                 infuse mek               =
>                   let
>                     ev                   = deJust fName mek.mEvent
>                     onset                = fromRational ev.eTime
>                     delta                = fromRational ev.eDur
>                     over0                = deJust (unwords [fName, "0", show si0]) (withOveralls VB.! si0).mOverall
>                     over1                = -- WOX deJust (unwords [fName, "1", show si1]) (withOveralls VB.! si1).mOverall
>                       case VB.find (\np → snd np == mek.mSelfIndex) groupNodePairs of
>                         Just np          → deJust (unwords [fName, "-1", show np]) (withOveralls VB.! fst np).mOverall
>                         Nothing          → error $ unwords [fName, "inflection has no previous node !?"]
>
>                     isInflection         = si0 == mek.mSelfIndex && si0 /= VB.head nodeGroup && si0 /= VB.last nodeGroup
>                   in
>                     if isInflection
>                       then changeParams mek (Right (fourVelos onset delta over1 over0))
>                       else changeParams mek (Right (twoVelos onset delta over0))
>
> formNodeGroups         :: VB.Vector MekNote
>                           → (VB.Vector (SelfIndex, SelfIndex), VB.Vector (VB.Vector SelfIndex))
> formNodeGroups meks                      = (nps, nhs)
>   where
>     nodes                                =
>       let
>         append sis mek                   = sis VB.++ getMarkNode mek
>       in
>         VB.foldl' append VB.empty meks
>     nps                                  = VB.zip nodes (VB.tail nodes)
>     nhs                                  = VB.fromList $ VB.groupBy inflected nodes
>       where
>         inflected _ si                   = inflection (meks VB.! si)
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
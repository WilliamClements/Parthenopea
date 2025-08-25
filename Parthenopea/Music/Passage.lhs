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
> import Parthenopea.Repro.Modulation ( clip, epsilon )
> import Parthenopea.SoundFont.SFSpec ( Velocity )

Notation-Driven Dynamics ==============================================================================================

_Marking_                = composer's passage-shape directive, sequence
_Overall                 = 
   change rate = onset time and dur *versus* start and end velocity

> type SelfIndex                           = Int
>
> data Marking                             =
>     Mark StdLoudness
>   | ReMark StdLoudness
>   | Rest1
>   | Span1
>   | SpanN Int
>   deriving (Eq, Show)
> expandMarkings         :: [Marking] → VB.Vector Marking
> expandMarkings ms                        = VB.fromList $ concatMap expand ms
>   where
>     expand (SpanN n)                     = replicate n Span1
>     expand x                             = [x]
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
> velocity               :: Double → Overall → Double
> velocity tIn over
>   | tIn < (over.oStartT - epsilon) || tIn > (over.oEndT + epsilon)
>                                          = error $ unwords ["velocity: input tIn out of range", show (tIn, over)]
>   | otherwise                            = clip (0, 128) (over.oStartV + (tIn - over.oStartT) * changeRate)
>   where
>     changeRate                           = (over.oEndV - over.oStartV) / (over.oEndT - over.oStartT)
> twoVelos               :: Double → Double → Overall → Either Velocity (VB.Vector Double)
> twoVelos onset delta over                = branchVelos $ VB.fromList
>                                            [ velocity onset               over
>                                            , velocity (onset + delta)     over]
> fourVelos              :: Double → Double → Overall → Overall → Either Velocity (VB.Vector Double)
> fourVelos onset delta over0 over1        = branchVelos $ VB.fromList
>                                            [ velocity onset  over0
>                                            , velocity (onset + delta / 2) over0
>                                            , velocity (onset + delta / 2) over1
>                                            , velocity (onset + delta)     over1]
> branchVelos            :: VB.Vector Double → Either Velocity (VB.Vector Double)
> branchVelos vIn                          =
>   if all (nearlyEqual keyParam) vIn
>     then (Left . round) keyParam
>     else Right vIn
>   where
>     keyParam                             = vIn VB.! 0
>     nearlyEqual x y                      = abs (y - x) < epsilon
>
> data MekNote                             =
>   MekNote {
>     mSelfIndex         :: SelfIndex
>   , mPrimitive         :: Primitive Pitch
>   , mMarking           :: Marking
>   , mEvent             :: Maybe MEvent
>   , mOverall           :: Maybe Overall
>   , mParams            :: Maybe (Either Velocity (VB.Vector Double))}
>   deriving (Eq, Show)
> makeMekNote            :: SelfIndex → Primitive Pitch → Marking → MekNote
> makeMekNote six prim marking
>   | restProblem                          = error $ unwords ["Non-corresponding rests"]
>   | otherwise                            =
>     MekNote six prim marking Nothing Nothing Nothing
>   where
>     restProblem                          =
>       case prim of
>         Rest _                           → marking /= Rest1
>         _                                → False
> changeParams           :: MekNote → Either Velocity (VB.Vector Double) → (SelfIndex, MekNote)
> changeParams mek val                     = (mek.mSelfIndex, mek{mParams = Just val})
> getMarkVelocity        :: MekNote → Velocity
> getMarkVelocity mek                      =
>   case mek.mMarking of
>     Mark x                               → stdVelocity x
>     ReMark x                             → stdVelocity x
>     _                                    → error "no mark velocity"
> passage                :: BandPart → [Marking] → Music Pitch → Music1
> passage bp markings ma
>   | not enableDynamics                   = toMusic1 ma
>   | null markings                        = error $ unwords ["empty markings"]
>   | otherwise                            = removeZeros $ passageImpl bp (expandMarkings markings) (removeZeros ma)
>
> passageImpl            :: BandPart → VB.Vector Marking → Music Pitch → Music1
> passageImpl bp markings ma
>   | traceIf trace_IP False               = undefined
>   | otherwise                            = VB.foldl' final (rest 0) enriched
>   where
>     fName_                               = "passageImpl"
>     trace_IP                             = unwords [fName_, show markings, show (VB.map mEvent withEvents)]
>
>     -- reconstruct notes with added dynamics metadata
>     final              :: Music1 → MekNote → Music1 
>     final music mek
>       | traceIf trace_F False            = undefined
>       | otherwise                        =
>       music :+: case mek.mPrimitive of
>                   Note durI pitchI       →
>                     note durI (pitchI, Dynamics fName_ : (makeNas . deJust fName) mek.mParams)
>                   Rest durI              → rest durI
>       where
>         fName                            = "final"
>         trace_F                          = unwords [fName, show mek.mSelfIndex, show mek.mParams]
>
>         makeNas        :: Either Velocity (VB.Vector Double) → [NoteAttribute]
>         makeNas (Left homeVolume)        = [Volume homeVolume]
>         makeNas (Right directive)        =
>           if VB.null directive
>             then error $ unwords [fName, "illegally null mParams directive"]
>             else [(Volume . average) directive, (Params . VB.toList) directive]
>         
>         average        :: VB.Vector Double → Velocity
>         average directive                = round $ VB.sum directive / (fromIntegral . VB.length) directive
>
>     -- evolve enriched note/rest (MekNote) list
>     rawMeks, withEvents, withOveralls, seeded, enriched
>                        :: VB.Vector MekNote
>    
>     rawMeks                               =
>       profess
>         (nPrims /= 0 && (nPrims == nMarks))
>         (unwords ["bad lengths; prims, markings", show (nPrims, nMarks)])
>         (VB.zipWith3 makeMekNote selfIndices prims markings)
>       where
>         nPrims                           = VB.length prims
>         nMarks                           = VB.length markings
>
>         selfIndices                      = VB.generate nPrims id
>
>         prims          :: VB.Vector (Primitive Pitch)
>         prims                            =
>           let
>             pFun       :: Primitive Pitch → VB.Vector (Primitive Pitch)
>             pFun (Note pP dP)            = VB.singleton (Note pP dP)
>             pFun (Rest dP)               = VB.singleton (Rest dP)
>           in
>             mFold pFun (VB.++) undefined undefined ma
>
>     withEvents                           = fst $ foldl' implant (VB.empty, 0) rawMeks
>       where
>         eTable                           = VB.fromList $ fst $ musicToMEvents (bandPartContext bp) (toMusic1 ma)
>
>         implant (meks, soFar) mek        = (meks VB.++ VB.singleton mek{mEvent = setEvent}, soFar + mekWidth)
>           where
>             (setEvent, mekWidth)         = 
>               case mek.mPrimitive of
>                 Note _ _                 → (Just (eTable VB.! soFar),    1)
>                 Rest _                   → (Nothing,                     0)
>
>     (nodePairs, nodeGroups)              = formNodeGroups withEvents
>
>     withOveralls                         = withEvents `VB.update` lode
>       where
>         lode                             =
>           VB.concatMap (uncurry computeOverall) nodePairs VB.++ computeOverall lastSi lastSi
>         lastSi                           = snd $ VB.last nodePairs
>
>         computeOverall si0 si1
>           | traceIf trace_CO False       = undefined
>           | otherwise                    = VB.singleton (si0, mek0{mOverall = makeOverall loud0 loud1 ev0 ev1})
>           where
>             fName                        = "computeOverall"
>             trace_CO                     = unwords [fName, show ((si0, loud0, ev0), (si1, loud1, ev1))]
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
>     seeded                               = withOveralls `VB.update` lode
>       where
>         lode                             = VB.concatMap enseed nodeGroups
>         mekFence                         = VB.length withOveralls - 1
>
>         enseed         :: VB.Vector SelfIndex → VB.Vector (SelfIndex, MekNote)
>         enseed nodeGroup
>           | gLen == 0                    = error "no nodes in node group"
>           | gLen == 1                    = seedOne $ withOveralls VB.! (nodeGroup VB.! 0)
>           | otherwise                    =
>           VB.concatMap (uncurry seeden) (VB.zip nodeGroup (VB.tail nodeGroup))
>                        VB.++ seeden (VB.last nodeGroup) (VB.last nodeGroup)
>           where
>             gLen                         = VB.length nodeGroup
>
>             seedOne mekArg               = VB.singleton $ changeParams mekArg (Left $ getMarkVelocity mekArg)
>
>             seeden si0 si1
>               | traceIf trace_S False    = undefined
>               | otherwise                = VB.map infuse seedMeks
>               where
>                 fName                    = "seeden"
>                 trace_S                  = unwords [fName, show (si0, si1), show over]
>
>                 seedMeks                 = VB.slice si0 (si1 - si0 + 1 - fencePost) withOveralls
>                                              where fencePost = if si1 == mekFence then 0 else 1
>
>                 infuse mek               =
>                   if enableInflections && si0 == mek.mSelfIndex && si0 /= VB.head nodeGroup
>                     then changeParams mek $ fourVelos onset delta overPrev over
>                     else changeParams mek $ twoVelos onset delta over
>                   where
>                     ev                   = deJust fName mek.mEvent
>                     onset                = fromRational ev.eTime
>                     delta                = fromRational ev.eDur
>
>                 over                     = deJust (unwords [fName, show si0]) (withOveralls VB.! si0).mOverall
>                 overPrev                 =
>                   case VB.find (\np → snd np == si0) nodePairs of
>                     Just np              → deJust (unwords [fName, show np]) (withOveralls VB.! fst np).mOverall
>                     Nothing              → error $ unwords [fName, "inflection has no previous node !?"]
>
>     enriched                             = seeded `VB.update` lode
>       where
>         lode                             = fst $ VB.foldl' enrich (VB.empty, bp.bpHomeVelocity) seeded
>
>         enrich         :: (VB.Vector (SelfIndex, MekNote), Velocity)
>                           → MekNote
>                           → (VB.Vector (SelfIndex, MekNote), Velocity)
>         enrich (updates, velo) mek       =
>           let
>             velo'                        =
>               case mek.mMarking of
>                 Mark x                   → stdVelocity x
>                 ReMark x                 → stdVelocity x
>                 _                        → velo
>             updateOne                    =
>               case mek.mParams of
>                 Nothing                  → VB.singleton $ changeParams mek (Left velo')
>                 _                        → VB.empty
>           in
>             (updates VB.++ updateOne, velo')
>
> formNodeGroups         :: VB.Vector MekNote
>                           → (VB.Vector (SelfIndex, SelfIndex), VB.Vector (VB.Vector SelfIndex))
> formNodeGroups meks                      = (nodePairs, nodeGroups)
>   where
>     nodes                                =
>       let
>         getMarkNode mek                  =
>           case mek.mMarking of
>             Mark _                       → VB.singleton mek.mSelfIndex
>             ReMark _                     → VB.singleton mek.mSelfIndex
>             _                            → VB.empty
>
>         append sis mek                   = sis VB.++ getMarkNode mek
>       in
>         VB.foldl' append VB.empty meks
>     nodePairs                            = VB.zip nodes (VB.tail nodes)
>     nodeGroups                           = VB.fromList $ VB.groupBy inflected nodes
>       where
>         inflected _ si                   =
>           case (meks VB.! si).mMarking of
>             ReMark _                     → False
>             _                            → True
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
> enableInflections      :: Bool    

Edit the following ====================================================================================================

> enableDynamics                           = True
> enableInflections                        = True

The End
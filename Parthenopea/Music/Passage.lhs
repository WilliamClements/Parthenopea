> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE NumericUnderscores  #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
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
> makeOverall            :: Double → Double → MEvent → MEvent → Overall
> makeOverall startV endV startEv endEv    = Overall startT endT startV endV
>   where
>     startT                               = fromRational startEv.eTime
>     endT                                 = fromRational $ endEv.eTime + endEv.eDur
> changeRate             :: Overall → Double
> changeRate over                          = (over.oEndV - over.oStartV) / (over.oEndT - over.oStartT)
> velocity               :: Double → Overall → Double
> velocity tIn over                        = over.oStartV + tIn * changeRate over
> twoVelos               :: MEvent → Overall → [Double]
> twoVelos ev over                         = [ velocity onset           over
>                                            , velocity (onset + delta) over]
>   where
>     onset                                = fromRational ev.eTime
>     delta                                = fromRational ev.eDur
> fourVelos              :: MEvent → Overall → Overall → [Double]
> fourVelos ev over0 over1                 = [ velocity onset           over0
>                                            , velocity (onset + delta / 2) over0
>                                            , velocity (onset + delta / 2) over1
>                                            , velocity (onset + delta) over1]
>   where
>     onset                                = fromRational ev.eTime
>     delta                                = fromRational ev.eDur
>
> data MekNote                             =
>   MekNote {
>     mSelfIndex         :: SelfIndex
>   , mPrimitive         :: Primitive Pitch
>   , mMarking           :: Marking
>   , mEventIndex        :: EventIndex
>
>   , mEndIndex          :: SelfIndex
>   , mOverall           :: Maybe Overall
>   , mParams            :: [Double]}
>   deriving (Eq, Show)
> inflection             :: MekNote → Bool
> inflection mek                           = case mek.mMarking of
>                                              ReMark _              → False
>                                              _                     → True
> changeParams           :: MekNote → [Double] → (SelfIndex, MekNote)
> changeParams mek dubs                    = (mek.mSelfIndex, mek{mParams = dubs})
> getMarkLoudness        :: MekNote → Maybe StdLoudness
> getMarkLoudness mek                      =
>   case mek.mMarking of
>     Mark x                               → Just x
>     ReMark x                             → Just x
>     _                                    → Nothing
> getMarkNode            :: MekNote → [SelfIndex]
> getMarkNode mek                          =
>   case mek.mMarking of
>     Mark _                               → [mek.mSelfIndex]
>     ReMark _                             → [mek.mSelfIndex]
>     _                                    → []
> makeMekNote            :: Int → Primitive Pitch → Marking → MekNote
> makeMekNote six prim marking
>   | restProblem                          = error $ unwords ["Non-corresponding rests"]
>   | otherwise                            =
>     MekNote six prim marking  0 0 Nothing []
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
> foldMeks               :: ∀ a . (MekNote → a → (MekNote, a)) → a → VB.Vector MekNote → (VB.Vector MekNote, a)
> foldMeks userFun initA meksIn            = foldl' mekFoldFun (meksIn, initA) meksIn
>   where
>     mekFoldFun         :: (VB.Vector MekNote, a) → MekNote → (VB.Vector MekNote, a)
>     mekFoldFun (meksFold, itemA) mek     =
>       let
>         (mek', item')                    = userFun mek itemA
>       in
>         (meksFold VB.// [(mek'.mSelfIndex, mek')], item')
>
> passage                :: BandPart → [Marking] → Music Pitch → Music1
> passage bp markings ma
>   | not enableDynamics                   = toMusic1 ma
>   | null markings                        = error $ unwords ["empty markings"]
>   | otherwise                            = removeZeros $ passageImpl bp (expandMarkings markings) (removeZeros ma)
>
> passageImpl            :: BandPart → [Marking] → Music Pitch → Music1
> passageImpl bp markings ma
>   | traceNow trace_IP False              = undefined
>   | otherwise                            = foldl' final (rest 0) enriched
>   where
>     fName__                              = "passageImpl"
>     trace_IP                             = unwords [fName__, show markings]
>
>     -- reconstruct notes with added dynamics metadata
>     final              :: Music1 → MekNote → Music1 
>     final music mek                      =
>       music :+: case mek.mPrimitive of
>                   Note durI pitchI       → note durI (pitchI, [Dynamics fName__, Params mek.mParams])
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
>     coIndexed, withOveralls, enriched
>                        :: VB.Vector MekNote     
>     coIndexed                            = fst $ foldMeks implant 0 unIndexed
>       where
>         unIndexed      :: VB.Vector MekNote     
>         unIndexed                        =
>           profess
>                (not (null prims) && (length prims == length markings))
>                (unwords ["bad lengths; prims, markings", show $ length prims, show $ length markings])
>                (VB.fromList $ zipWith3 makeMekNote [0..] prims markings)
>         implant mek soFar                = (mek{mEventIndex = soFar}, soFar + mekWidth mek)
>
>     nodes              :: [SelfIndex]
>     nodes                                = foldl' append [] coIndexed
>       where
>         append sis mek                   = sis ++ getMarkNode mek
>
>     nodePairs          :: [(SelfIndex, SelfIndex)]
>     nodePairs                            = zip nodes (tail nodes)
>
>     nodePairGroups     :: [[(SelfIndex, SelfIndex)]]
>     nodePairGroups                       = groupBy inflected nodePairs
>       where
>         inflected (_, si1) (_, _)        = inflection (coIndexed VB.! si1)                
>             
>     withOveralls                         = coIndexed VB.// lode
>       where
>         lode                             = concatMap computeOverall nodePairs
>
>         computeOverall (si0, si1)        = [(si0, mek0{mOverall = Just over})]
>           where
>             over                         = makeOverall loud0 loud1 ev0 ev1
>
>             mek0                         = coIndexed VB.! si0
>             mek1                         = coIndexed VB.! si1
>
>             ev0                          = eTable VB.! mek0.mEventIndex
>             ev1                          = eTable VB.! mek1.mEventIndex
>
>             loud0                        = (loudness . fromJust . getMarkLoudness) mek0
>             loud1                        = (loudness . fromJust . getMarkLoudness) mek1
>
>     enriched                             = withOveralls VB.// lode
>       where
>         lode                             = concatMap enrich nodePairGroups
>         fence                            = VB.length withOveralls - 1
>
>         enrich                           = concatMap richen
>         richen (si0, si1)                = map infuse lmeks
>           where
>             lmeks                        = map (withOveralls VB.!) [si0 .. si1 - fencePost]
>             fencePost                    = if si1 == fence then 0 else 1
>             infuse mek                   =
>               let
>                 fName                    = "infuse"
>
>                 ev                       = eTable VB.! mek.mEventIndex
>                 over0                    = fromJust (head lmeks).mOverall
>
>                 tween                    = changeParams mek (fourVelos ev over0 over1)
>                   where
>                     over1                = case find (\np → snd np == mek.mSelfIndex) nodePairs of
>                                              Just np               → fromJust (withOveralls VB.! fst np).mOverall
>                                              Nothing               → error $ unwords [fName, "inflection has no previous node !?"]
>
>                 tweak                    = changeParams mek (twoVelos ev over0)
>               in
>                 if inflection mek then tween else tweak

The End
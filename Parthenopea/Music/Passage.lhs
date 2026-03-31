> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Passage
William Clements
August 15, 2025

> module Parthenopea.Music.Passage(
>        enrichPassage
>        , expandMarkings
>        , formNodeGroups
>        , makeMekNote
>        , Marking(..)
>        , MekNote(..)
>        , passage) where
>
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.MIDI.MEvent ( MEvent(eDur, eTime), musicToMEvents )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren ( bandPartContext, BandPart(..), stdVelocity)
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Utility
  
Notation-Driven Dynamics ==============================================================================================
           When Passage is used, each note in a group is assigned a volume range versus time. Underlying synthesizer
           interprets that to vary output volume accordingly -- across the whole phrase.

_Marking_                = composer's passage-shape directive, sequence
_Overall_                = 
   change rate = onset time and dur *versus* start and end velocity

> type SelfIndex                           = Int
>
> data Marking                             =
>   Mark StdLoudness
>   | Inflect StdLoudness
>   | Span1
>   | SpanN Int
>   | Tail StdLoudness
>   deriving (Eq, Show)
> expandMarkings         :: [Marking] → VB.Vector Marking
> expandMarkings ms                        = VB.fromList $ concatMap expand ms
>   where
>     expand (SpanN n)                     = replicate n Span1
>     expand x                             = [x]
>
> data Overall                             =
>   Overall {
>     oChangeRate        :: !Double
>   , oStartT            :: !Double
>   , oStartV            :: !Double}
>   deriving Show
> makeOverall            :: Double → Double → MEvent → MEvent → Overall
> makeOverall startV endV startEv endEv    = Overall changeRate startT startV
>   where
>     startT                               = fromRational startEv.eTime
>     endT                                 = fromRational endEv.eTime + fromRational endEv.eDur
>     changeRate                           = (endV - startV) / (endT - startT)
> velocity               :: Double → Overall → Double
> velocity tIn over                        =
>   profess
>     (tIn >= (over.oStartT - epsilon))
>     (unwords ["velocity: input tIn out of range", show (tIn, over)])
>     (clip (0, 128) (over.oStartV + (tIn - over.oStartT) * over.oChangeRate))
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
> branchVelos meksIn                       =
>   if VB.all (nearlyEqual keyParam) meksIn
>     then (Left . round) keyParam
>     else Right meksIn
>   where
>     keyParam                             = meksIn VB.! 0
>     nearlyEqual x y                      = abs (y - x) < epsilon

      M.E.K. = Music Education for Kids ===============================================================================

> data MekNote                             =
>   MekNote {
>     mSelfIndex         :: SelfIndex
>   , mPrimitive         :: Primitive Pitch
>   , mMarking           :: Marking
>   , mEvent             :: Maybe MEvent
>   , mOverall           :: Maybe Overall
>   , mParams            :: Maybe (Either Velocity (VB.Vector Double))}
>   deriving Show
> makeMekNote            :: SelfIndex → Primitive Pitch → Marking → Maybe MEvent → MekNote
> makeMekNote six prim marking mev         =
>   MekNote 
>      six 
>      prim 
>      marking 
>      mev 
>      Nothing Nothing
> getMarkVelocity        :: MekNote → Velocity
> getMarkVelocity mek                      =
>   case mek.mMarking of
>     Mark x                               → stdVelocity x
>     Inflect x                            → stdVelocity x
>     Tail x                               → stdVelocity x
>     _                                    → error "no mark velocity"
> passage                :: Directives → BandPart → [Marking] → Music Pitch → Music1
> passage dives bp markings ma
>   | not dives.synthSwitches.usePassages  = toMusic1 ma
>   | null markings                        = error $ unwords ["empty markings"]
>   | otherwise                            = (removeZeros . passageToMusic) meksIn
>   where
>     meksIn                               = enrichPassage dives bp (expandMarkings markings) (removeZeros ma)
>
> passageToMusic         :: VB.Vector MekNote → Music1
> passageToMusic meksIn                    = foldl' eachMek (rest 0) meksIn
>   where

Enfix note attributes to send "passage" input to the runtime synthesizer ==============================================

>     eachMek            :: Music1 → MekNote → Music1 
>     eachMek music mek                    =
>       music :+: case mek.mPrimitive of
>                   Note durI pitchI       → mangleNote durI pitchI
>                   Rest durI              → rest durI
>       where
>         fName                            = "eachMek"
>
>         mangleNote dM pM                 = note dM (pM, Dynamics fName : (makeNAs . deJust fName) mek.mParams)
>
>         makeNAs (Left homeVolume)        = [Volume homeVolume]
>         makeNAs (Right sweeps)           =
>           profess
>             (not $ VB.null sweeps)
>             (unwords [fName, "illegally null sweeps"])
>             [(Volume . average) sweeps, (Params . VB.toList) sweeps]
>         
>         average sweeps                   = round $ VB.sum sweeps / (fromIntegral . VB.length) sweeps

Construct a vector of MekNotes, enriching them ========================================================================
      
> enrichPassage          :: Directives → BandPart → VB.Vector Marking → Music Pitch → VB.Vector MekNote
> enrichPassage dives bp markings musicIn_ 
>   | traceIf trace_EP False               = undefined
>   | otherwise                            = enriched
>   where
>     fName                                = "enrichPassage"
>     trace_EP                             = unwords [fName, show mekFence, show markings]
>
>     enriched, rawMeks  :: VB.Vector MekNote
>
>     -- stepwise evolution of enriched note/rest (MekNote) vector via these functions
>     -- each function produces an update vector with elements that need to be changed
>     wearOveralls, sewSeeds, enfill
>                        :: VB.Vector MekNote → VB.Vector MekNote
>
>     rawMeks                              = makeMeks    
>     mekFence                             = VB.length rawMeks - 1
>     (nodePairs, nodeGroups)              = formNodeGroups rawMeks
>     musicIn                              = removeZeros musicIn_
>
>     enriched                             = notracer "enriched" $
>       let
>         doUpdate       :: (VB.Vector MekNote → VB.Vector MekNote) → VB.Vector MekNote → VB.Vector MekNote
>         doUpdate updateIf meksIn         = VB.update meksIn $ VB.map enTag $ updateIf (notracer "meksIn" meksIn)
>                                              where enTag mek = (mek.mSelfIndex, mek)
>       in
>         (doUpdate enfill . doUpdate sewSeeds . doUpdate wearOveralls) rawMeks

Initialize the MekNote vector ========================================================================================

      quick-zip together the available heterogeneous data per primitive (notes only, no rests)
      1. (index)
      2. Music Primitive
      3. composer's velocity Marking
      4. Maybe MEvent from "performing" the music snippet

      later stages will compute and add following data to each M.E.K. note
      1. transform having type Overall
      2. directive having type Maybe (Either Velocity (VB.Vector Double))}

>     makeMeks                             =
>       profess
>         (nPrims > 0 && (nPrims == nMarks) && (not $ null evs))
>         (unwords ["bad length(s) in; prims, markings, evs"
>                 , "\nprims=", show $ length prims, show prims
>                 , "\nmarks=", show $ VB.length markings, show markings
>                 , "\nevs=", show $ length evs, show evs])
>         (VB.zipWith4 makeMekNote
>                        selfIndices
>                        prims
>                        markings
>                        evs)
>       where
>         nPrims                           = VB.length prims
>         nMarks                           = VB.length markings
>
>         selfIndices                      = VB.generate nPrims id
>
>         prims                            =
>           let
>             pFun (Note pP dP)            = VB.singleton (Note pP dP)
>             pFun (Rest _)                = VB.empty
>             noChords _ _                 = error "no chords in Passage input"
>           in
>             mFold pFun (VB.++) noChords undefined musicIn
>
>         eTable                           =
>           VB.fromList $ fst $ musicToMEvents (bandPartContext bp) (toMusic1 (removeZeros musicIn))
>
>         evs                              = 
>           let
>             slotIn     :: (VB.Vector (Maybe MEvent), Int) → Primitive Pitch → (VB.Vector (Maybe MEvent), Int)
>             slotIn (pvec, soFar) prim    = (pvec VB.++ curEvents, soFar + mekWidth)
>               where
>                 (curEvents, mekWidth)    = 
>                   case prim of
>                     Note _ _             → (VB.singleton (Just $ eTable VB.! soFar),    1)
>                     Rest _               → (VB.empty,                                   0)
>           in
>             fst $ VB.foldl' slotIn (VB.empty, 0) prims

wearOveralls ==========================================================================================================
      compute transforms (Overalls) to thread the velocity changes through
      irrespective of grouping, it folds on nodePairs
      the Overall is consumed in fourVelos or twoVelos - core dynamics functions

>     wearOveralls meksIn                  =
>       VB.concatMap (uncurry computeOverall) nodePairs VB.++ computeOverall lastSi lastSi
>       where
>         lastSi                           = if null nodePairs
>                                              then mekFence
>                                              else snd $ VB.last nodePairs
>
>         computeOverall si0 si1           =
>           VB.singleton mek0{mOverall = Just $ makeOverall loud0 loud1 ev0 ev1}
>           where
>             fName                        = "computeOverall"
>
>             mek0                         = meksIn VB.! si0
>             mek1                         = meksIn VB.! si1
>
>             ev0                          = deJust (unwords [fName, "ev0"]) mek0.mEvent
>             ev1                          = deJust (unwords [fName, "ev1"]) mek1.mEvent
>
>             loud0                        = (fromIntegral . getMarkVelocity) mek0
>             loud1                        = (fromIntegral . getMarkVelocity) mek1

sewSeeds ==============================================================================================================
      infuse M.E.K. notes' mParams, which has the type Maybe (Either Velocity (Vector Double))
      folds on nodeGroups

>     sewSeeds meksIn                      = VB.concat (map enseed nodeGroups)
>       where
>         enseed         :: VB.Vector SelfIndex → VB.Vector MekNote
>         enseed nodeGroup
>           | gLen == 0                    = error $ unwords [fName, "no nodes in node group"]
>           | gLen == 1                    = seedOne $ meksIn VB.! (nodeGroup VB.! 0)
>           | otherwise                    =
>           VB.concatMap (uncurry seeden) (VB.zip nodeGroup (VB.tail nodeGroup))
>                        VB.++ seeden (VB.last nodeGroup) (VB.last nodeGroup)
>           where
>             gLen                         = VB.length nodeGroup  
>             seedOne mekArg               = VB.singleton $ mekArg{mParams = (Just . Left) (getMarkVelocity mekArg)}
>
>             seeden si0_ si1_             = VB.map infuse seedMeks
>               where
>                 si0                      = notracer "si0" si0_
>                 si1                      = notracer "si1" si1_
>
>                 fName                    = "seeden"
>
>                 seedMeks                 = VB.slice si0 (si1 - si0 + 1 - fencePost) meksIn
>                                              where fencePost = if si1 == mekFence then 0 else 1
>
>                 infuse mek               =
>                   if dives.synthSwitches.useInflections && si0 == mek.mSelfIndex && si0 /= VB.head nodeGroup
>                     then mek{mParams = Just (fourVelos onset delta overPrev over)}
>                     else mek{mParams = Just (twoVelos onset delta over)}
>                   where
>                     ev                   = deJust fName mek.mEvent
>                     onset                = fromRational ev.eTime
>                     delta                = fromRational ev.eDur
>
>                 over                     = deJust (unwords [fName, show si0]) (meksIn VB.! si0).mOverall
>                 overPrev                 =
>                   case VB.find (\np → snd np == si0) nodePairs of
>                     Just np              → deJust (unwords [fName, show np]) (meksIn VB.! fst np).mOverall
>                     Nothing              → error $ unwords [fName, "inflection has no previous node !?"]

enfill ================================================================================================================
      fills in any as yet undetermined mParams with simple (Left) Velocity
      folds on MekNotes

>     enfill meksIn                        = fst $ VB.foldl' fillOne (VB.empty, bp.bpHomeVelocity) meksIn
>       where
>         fillOne        :: (VB.Vector MekNote, Velocity)
>                           → MekNote
>                           → (VB.Vector MekNote, Velocity)
>         fillOne (updates, velo) mek      =
>           let
>             velo'                        =
>               case mek.mMarking of
>                 Mark x                   → stdVelocity x
>                 Inflect x                → stdVelocity x
>                 _                        → velo
>             updateOne                    =
>               case mek.mParams of
>                 Nothing                  → VB.singleton $ mek{mParams = (Just . Left) velo'}
>                 _                        → VB.empty
>           in
>             (updates VB.++ updateOne, velo')

formNodeGroups ========================================================================================================
      model the inflection/non-inflection sequence
      each value in nodePairs is the SelfIndex pair identifying consecutive nodes of source vector

> formNodeGroups         :: VB.Vector MekNote
>                           → (VB.Vector (SelfIndex, SelfIndex), [VB.Vector SelfIndex])
> formNodeGroups meks                      = (nodePairs, nodeGroups)
>   where
>     getMarkNode mek                      =
>       case mek.mMarking of
>         Mark _                           → VB.singleton mek.mSelfIndex
>         Inflect _                        → VB.singleton mek.mSelfIndex
>         _                                → VB.empty
>     nodes                                =
>       let
>         append sis mek                   = sis VB.++ getMarkNode mek
>       in
>         VB.foldl' append VB.empty meks
>     nodePairs                            = VB.zip nodes (VB.tail nodes)
>     nodeGroups                           =
>       let
>         inflected _ si                   =
>           case (meks VB.! si).mMarking of
>             Mark _                       → False
>             _                            → True
>       in
>          VB.groupBy inflected nodes

The End
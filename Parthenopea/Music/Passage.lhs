> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}

Passage
William Clements
August 15, 2025

> module Parthenopea.Music.Passage(
>        chartPassage
>        , enrichPassage
>        , expandMarkings
>        , Marking(..)
>        , MekNote(..)
>        , npassage
>        , passage) where
>
> import Data.Foldable
> import qualified Data.Vector.Strict      as VB
> import Euterpea.IO.Audio.Types
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Music.Siren
> import Parthenopea.Repro.Chart
> import Parthenopea.Repro.Envelopes
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Utility
  
Notation-Driven Dynamics ==============================================================================================
           When Passage is used, each note in a group is assigned, in effect, a function from time to volume.
           Underlying synthesizer, in effect, runs that function to compute the output volume.

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
> expandMarkings ms                        = VB.fromList $ tracer "expanded" $ concatMap expand ms
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
>     endT                                 = fromRational (endEv.eTime + endEv.eDur)
>     changeRate                           = (endV - startV) / (endT - startT)
> velocity               :: Double → Overall → Double
> velocity tIn over                        =
>   profess
>     (tIn > (over.oStartT - epsilon))
>     (unwords ["velocity: input tIn out of range", show (tIn, over)])
>     (clip (0, 128) (over.oStartV + (tIn - over.oStartT) * over.oChangeRate))
> twoVelos               :: Double → Double → Overall → Either Velocity (VB.Vector Double)
> twoVelos onset delta over                = branchVelos $ VB.fromList
>                                            [ velocity onset               (notracer "ZZover" over)
>                                            , velocity (onset + delta)     over]
> fourVelos              :: Double → Double → Overall → Overall → Either Velocity (VB.Vector Double)
> fourVelos onset delta over0 over1        = branchVelos $ VB.fromList
>                                            [ velocity onset  over0
>                                            , velocity (onset + delta / 2) over0
>                                            , velocity (onset + delta / 2) over1
>                                            , velocity (onset + delta)     over1]
> branchVelos            :: VB.Vector Double → Either Velocity (VB.Vector Double)
> branchVelos params                       =
>   if VB.all (nearlyEqual keyParam) (notracer "HHHparams" params)
>     then (Left . round) keyParam
>     else Right params
>   where
>     keyParam                             = params VB.! 0
>     nearlyEqual x y                      = abs (y - x) < epsilon

      M.E.K. = Music Education for Kids of all ages ===================================================================

> data MekNote                             =
>   MekNote {
>     mSelfIndex         :: SelfIndex
>   , mPrimitive         :: Primitive Pitch
>   , mMarking           :: Marking
>   , mEvent             :: MEvent
>   , mOverall           :: Maybe Overall
>   , mParams            :: Maybe (Either Velocity (VB.Vector Double))}
>   deriving Show
> makeMekNote            :: SelfIndex → Primitive Pitch → Marking → MEvent → MekNote
> makeMekNote six prim marking ev          =
>   MekNote 
>      six 
>      prim 
>      marking 
>      ev 
>      Nothing Nothing
>
> getMarkingLoudness     :: MekNote → StdLoudness
> getMarkingLoudness mek                   =
>   case mek.mMarking of
>     Mark x                               → x
>     Inflect x                            → x
>     Tail x                               → x
>     _                                    → error $ unwords ["expected Marking with loudness, not", show mek.mMarking]
>
> npassage               :: Directives → String → BandPart → [Marking] → Music Pitch → Music1
> npassage dives pname bp markings ma       
>   | not dives.synthSwitches.usePassages  = toMusic1 ma
>   | null markings                        = error $ unwords ["empty markings"]
>   | otherwise                            = (removeZeros . enrichedToMusic pname) enriched
>   where
>     enriched                             = enrichPassage dives bp (expandMarkings markings) (removeZeros ma)
>
> passage                :: Directives → BandPart → [Marking] → Music Pitch → Music1
> passage dives                            = npassage dives "<passage>"
>
> enrichedToMusic        :: String → VB.Vector MekNote → Music1
> enrichedToMusic pname enriched           = phrase [Dyn (StdLoudness startLoudness)] music
>   where
>     fName                                = "enrichedToMusic"
>
>     startLoudness                        = getMarkingLoudness $ enriched VB.! 0
>     music                                = foldl' enFix (rest 0) enriched

Insert note attributes to define the later input to runtime synthesizer ===============================================

>     enFix              :: Music1 → MekNote → Music1 
>     enFix music mek                      = music :+: case mek.mPrimitive of
>                                              Note durI pitchI       → mangleNote durI pitchI
>                                              Rest durI              → error $ restsError durI 
>       where
>         mangleNote dM pM                 = note dM (pM, Dynamics pname : (makeNAs . deJust fName) mek.mParams)
>
>         makeNAs (Left homeVolume)        = [Volume homeVolume]
>         makeNAs (Right sweeps)           =
>           profess
>             (not $ VB.null sweeps)
>             (unwords [fName, "illegally null sweeps"])
>             [(Volume . average) sweeps, (Params . VB.toList) sweeps]
>         
>         average sweeps                   = round $ VB.sum sweeps / (fromIntegral . VB.length) sweeps
>
>         restsError d                     = unwords [fName, "no rests in MekNotes", show d]

Construct a vector of MekNotes, enriching them ========================================================================
      
> enrichPassage          :: Directives → BandPart → VB.Vector Marking → Music Pitch → VB.Vector MekNote
> enrichPassage dives bp markings musicIn  = enriched
>   where
>     fName                                = "enrichPassage"
>
>     enriched, rawMeks  :: VB.Vector MekNote
>
>     -- stepwise evolution of enriched note (MekNote) vector via these functions
>     -- each function produces an update vector with elements that are to be changed
>     wearOveralls, sewSeeds, enfill
>                        :: VB.Vector MekNote → VB.Vector MekNote
>
>     rawMeks                              = makeMeks    
>     (nodeMates, nodeGroups)              = formNodeGroups rawMeks
>
>     enriched                             =
>       let
>         doUpdate       :: (VB.Vector MekNote → VB.Vector MekNote) → VB.Vector MekNote → VB.Vector MekNote
>         doUpdate updateIf meksIn         = VB.update meksIn $ VB.map enTag $ updateIf meksIn
>                                              where enTag mek = (mek.mSelfIndex, mek)
>       in
>         (doUpdate enfill . doUpdate sewSeeds . doUpdate wearOveralls) rawMeks

Initialize the MekNote vector ========================================================================================

      quick-zip together available data per primitive (notes only, no rests)
      1. (self index)
      2. Music Primitive
      3. composer's velocity Marking
      4. MEvent from "performing" particular note

      later stages will compute and add following data to each M.E.K. note
      5. time to velocity transform = Overall
      6. two ways of driving velocity = Maybe (Either Velocity (VB.Vector Double))

>     makeMeks                             =
>       profess
>         (nPrims > 0 && (nPrims == nMarks) && not (null evs))
>         (unwords [fName, "bad length(s) in; prims, markings, evs"
>                 , "\nprims=", show $ length prims, show prims
>                 , "\nmarks=", show $ VB.length markings, show markings
>                 , "\nevs=", show $ length evs, show evs])
>         (notracer "[MekNote]" $ VB.zipWith4 makeMekNote
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
>
>             noChords _ _                 =
>               error $ unwords [fName, "no chords allowed in passage phrase", show musicIn]
>
>             control (Tempo _) v          = v                                        
>             control _ _                  =
>               error $ unwords [fName, "no controls allowed in passage phrase, other than Tempo", show musicIn]
>           in
>             mFold pFun (VB.++) noChords control musicIn
>
>         eTable                           =
>           VB.fromList $ fst $ musicToMEvents (bandPartContext bp) ((removeZeros . toMusic1) musicIn)
>
>         evs                              = 
>           let
>             slotIn     :: (VB.Vector MEvent, SelfIndex) → Primitive Pitch → (VB.Vector MEvent, SelfIndex)
>             slotIn (pvec, soFar) prim    = (pvec VB.++ curEvents, soFar + mekWidth)
>               where
>                 (curEvents, mekWidth)    = 
>                   case prim of
>                     Note _ _             → (VB.singleton (eTable VB.! soFar),           1)
>                     Rest _               → (VB.empty,                                   0)
>           in
>             fst $ VB.foldl' slotIn (VB.empty, 0) prims

wearOveralls ==========================================================================================================
      compute transforms (Overalls) to thread the velocity changes through, irrespective of grouping
      the Overall is consumed in fourVelos or twoVelos - core dynamics functions
      (maps over nodeMates)

>     wearOveralls meksIn                  =
>       VB.concatMap (uncurry computeOverall) nodeMates -- WOX VB.++ computeOverall lastSi lastLastSi
>       where
>         computeOverall si0 si1
>           | traceNow trace_CO False      = undefined
>           | otherwise                    = 
>           VB.singleton mek0{mOverall = Just $ makeOverall loud0 loud1 ev0 ev1}
>           where
>             trace_CO                     = unwords [fName, "Overall", show (si0, si1), show (loud0, loud1)]
>
>             mek0                         = meksIn VB.! si0
>             mek1                         = meksIn VB.! si1
>
>             ev0                          = mek0.mEvent
>             ev1                          = mek1.mEvent
>
>             loud0, loud1
>                        :: Double
>             loud0                        = (fromIntegral . stdVelocity . getMarkingLoudness) mek0
>             loud1                        = (fromIntegral . stdVelocity . getMarkingLoudness) mek1

sewSeeds ==============================================================================================================
      infuse M.E.K. with a value of type Maybe (Either Velocity (Vector Double))
      (maps on nodeGroups)

>     sewSeeds meksIn                      = VB.concat (map seedGroup nodeGroups)
>       where
>         seedGroup         :: VB.Vector SelfIndex → VB.Vector MekNote
>         seedGroup nodeGroup
>           | gLen == 0                    = error $ unwords [fName, "no nodes in node group"]
>           | gLen == 1                    = VB.empty
>           | otherwise                    =
>             VB.concatMap (uncurry seedSlice) (VB.zip nodeGroup (VB.tail nodeGroup))
>           where
>             gLen                         = VB.length nodeGroup  
>
>             seedSlice  :: SelfIndex → SelfIndex → VB.Vector MekNote
>             seedSlice si0 si1            = VB.map infuse sliceMeks
>               where
>                 fName                    = "seedSlice"
>
>                 sliceMeks                = VB.slice si0 (si1 - si0 + 1) meksIn
>
>                 infuse mek               =
>                   if dives.synthSwitches.useInflections && si0 == mek.mSelfIndex && si0 /= VB.head nodeGroup
>                     then mek{mParams = Just (fourVelos onset delta overPrev over)}
>                     else mek{mParams = Just (twoVelos onset delta over)}
>                   where
>                     ev                   = mek.mEvent
>                     onset, delta
>                        :: Double
>                     onset                = fromRational ev.eTime
>                     delta                = fromRational ev.eDur
>
>                 over                     = deJust (unwords [fName, show si0]) (meksIn VB.! si0).mOverall
>                 overPrev                 =
>                   case VB.find (\np → snd np == si0) nodeMates of
>                     Just np              → deJust (unwords [fName, show np]) (meksIn VB.! fst np).mOverall
>                     Nothing              → error $ unwords [fName, "inflection has no previous node !?"]

enfill ================================================================================================================
      fills in any as yet undetermined mParams with simple (Left) Velocity
      (folds on MekNotes)

>     enfill meksIn                        = fst $ VB.foldl' fillOne (VB.empty, bp.bpHomeVelocity) meksIn
>       where
>         fillOne (updates, _) mek         =
>           let
>             velo       :: Velocity       = stdVelocity $ getMarkingLoudness mek
>             updateOne  :: VB.Vector MekNote
>                                          =
>               case mek.mParams of
>                 Nothing                  → VB.singleton $ mek{mParams = (Just . Left) velo}
>                 _                        → VB.empty
>           in
>             (updates VB.++ updateOne, velo)

formNodeGroups ========================================================================================================
      model the topology of given MekNote sequence, using SelfIndex to identify nodes
        nodeMates  = all the consecutive pairs from given           -- (vector of integer pairs)
        nodeGroups = given grouped by inflection status             -- (list of integer vectors)

> formNodeGroups         :: VB.Vector MekNote → (VB.Vector (SelfIndex, SelfIndex), [VB.Vector SelfIndex])
> formNodeGroups meks                      = (nodeMates, nodeGroups)
>   where
>     getSelfIndex mek                     =
>       case mek.mMarking of
>         Mark _                           → VB.singleton mek.mSelfIndex
>         Inflect _                        → VB.singleton mek.mSelfIndex
>         Tail _                           → VB.singleton mek.mSelfIndex
>         _                                → VB.empty
>     nodes                                =
>       let
>         append sis mek                   = sis VB.++ getSelfIndex mek
>       in
>         VB.foldl' append VB.empty meks
>     nodeMates                            = VB.zip nodes (VB.tail nodes)
>     nodeGroups                           =
>       let
>         inflected _ si                   =
>           case (meks VB.! si).mMarking of
>             Mark _                       → False
>             _                            → True
>       in
>          VB.groupBy inflected nodes
>
> chartPassage           :: VB.Vector MekNote → [Section]
> chartPassage meksIn                      = zipWith Section chartColors (VB.toList $ VB.map chartNote meksIn)
>   where
>     chartNote          :: MekNote → [(Double, Double)]
>     chartNote mek                        =
>       let
>         startT, secs   :: Double
>         startT                           = fromRational mek.mEvent.eTime
>         secs                             = fromRational mek.mEvent.eDur
>         ctrSF          :: CtrSF () Double
>         ctrSF                            = doSweepingEnvelope secs (deJust "no mParams!?!" mek.mParams)
>       in
>         graphSF secs ctrRate startT ctrSF

The End
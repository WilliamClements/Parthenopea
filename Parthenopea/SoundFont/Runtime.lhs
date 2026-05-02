> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE OverloadedRecordDot #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

Runtime
William Clements
February 1, 2025

> module Parthenopea.SoundFont.Runtime (
>          prepareRuntime
>        , SFFileBoot(..)
>        , SFFileRuntime(..)
>        , SFRuntime(..) ) where
>
> import qualified Data.Bifunctor          as BF
> import Data.Foldable
> import Data.IntMap        ( IntMap )
> import qualified Data.IntMap             as IntMap
> import Data.IntSet (IntSet )
> import qualified Data.IntSet             as IntSet
> import Data.Map.Strict ( Map )
> import qualified Data.Map.Strict         as Map
> import Data.Maybe
> import qualified Data.Set                as Set
> import qualified Data.Vector.Strict      as VB
> import qualified Data.Vector.Unboxed     as VU
> import Euterpea.IO.Audio.Basics ( outA )
> import Euterpea.IO.Audio.Render ( Instr )
> import Euterpea.IO.Audio.Types ( AudRate, Stereo, Clock, Signal )
> import Euterpea.Music
> import Parthenopea.Debug
> import Parthenopea.Repro.Smashing
> import Parthenopea.Repro.Synthesizer ( eutSynthesize )
> import Parthenopea.Repro.Zone
> import Parthenopea.SoundFont.Directives
> import Parthenopea.SoundFont.Scoring
> import Parthenopea.SoundFont.SFSpec
> import Parthenopea.SoundFont.Utility
  
> data SFRuntime                           =
>   SFRuntime {
>     zDirectives        :: Directives
>   , zRoster            :: ([InstrumentName], [PercussionSound])
>   , zWinners           :: (Map InstrumentName GMChoices, Map PercussionSound GMChoices)
>   , zRuntimeFiles      :: IntMap SFFileRuntime         {- [FileIndex → SFFileRuntime]   -}
>   , zInstrumentMap     :: [(InstrumentName, Instr (Stereo AudRate))]}
> instance Show SFRuntime where
>   show runt                 =
>     unwords ["SFRuntime", show (length runt.zRuntimeFiles, length runt.zInstrumentMap)]
> data SFFileBoot                          =
>   SFFileBoot {
>     zWordFBoot         :: !Int
>   , zFilename          :: FilePath
>   , zFileArrays        :: FileArrays
>   , zPreZonesBoot      :: IntMap PreZone
>   , zSquirrelSample    :: SampleArrays}
>
> data SFFileRuntime                       =
>   SFFileRuntime {
>     zWordFRuntime      :: !Int
>   , zPerInstrument     :: IntMap PerInstrument
>   , zPreZonesRuntime   :: IntMap PreZone
>   , ks                 :: SampleArrays}
> instance Show SFFileRuntime where
>   show sffile                            =
>     unwords [  "SFFileRuntime"
>              , show sffile.zPerInstrument]

cache SoundFont data that is only needed for Runtime ==================================================================

> prepareRuntime     :: Directives
>                       → ([InstrumentName], [PercussionSound]) 
>                       → VB.Vector SFFileBoot
>                       → Map PerGMKey PerInstrument
>                       → (Map InstrumentName GMChoices, Map PercussionSound GMChoices)
>                       → IO SFRuntime
> prepareRuntime dives rost vFilesBoot perIs choices
>                                          = do
>   let runtimeFiles                       = IntMap.fromList $ mapMaybe supply (VB.toList vFilesBoot)
>   let prerunt                            =
>         SFRuntime
>           dives
>           rost
>           choices
>           runtimeFiles
>           []
>   instrumentMap                          ← prepareInstruments prerunt choices
>   return prerunt{zInstrumentMap = instrumentMap}
>   where
  
"actions" =============================================================================================================
           map FileIndex into [InstIndex] so we can tell:
             1. which SoundFont files are actually needed
             2. what instruments need to be preserved from each file

>     actions            :: IntMap IntSet                {- [FileIndex → [InstIndex]]     -}
>     actions                              =
>       let
>         extract m                        = Set.fromList $ foldl' buildUp [] m
>           where
>             buildUp s (GMChoices _ mpergm _)
>                                          = maybe s append mpergm
>                                              where append pergm = pergm : s
>         selectI m pergm                  =
>           IntMap.insertWith IntSet.union pergm.pgkwFile ((IntSet.singleton . fromIntegral) pergm.pgkwInst) m
>       in
>         foldl' selectI IntMap.empty (extract (fst choices) `Set.union` extract (snd choices))
  
"supply" ==============================================================================================================
           transfer data from SFFileBoot collection to SFFileRuntime collection
           the former needs lots of space and the latter benefits from data compactness

>     supply             :: SFFileBoot → Maybe (Int, SFFileRuntime)
>     supply sffile                        = sffile.zWordFBoot `IntMap.lookup` actions >>= runtimeFile
>       where
>         runtimeFile    :: IntSet → Maybe (Int, SFFileRuntime)
>         runtimeFile insts                =    
>           let
>             getPerI inst                 =
>               perIs Map.! stdPerGMKey sffile.zWordFBoot inst
>
>             newPerI                      = IntMap.fromSet getPerI insts
>             pzdb                         = 
>               if doCopyPzdb
>                 then IntSet.foldl'       doSave   IntMap.empty         bixen
>                 else IntMap.foldlWithKey doUpdate sffile.zPreZonesBoot sffile.zPreZonesBoot
>               where
>                 bixen                    = IntSet.foldl' lump IntSet.empty insts
>                                              where lump m i = IntSet.union m (allBixen (newPerI IntMap.! i))
>
>                 resolve pz               = pz{pzRecon = Just $ resolvePreZone dives pz}
>
>                 doSave m bix             =
>                   let
>                     pz                   = sffile.zPreZonesBoot IntMap.! bix
>                   in
>                     IntMap.insert bix (resolve pz) m
>
>                 doUpdate m bix pz        =
>                   let
>                     keep _               = if bix `IntSet.member` bixen
>                                              then (Just . resolve) pz
>                                              else Nothing
>                   in
>                     IntMap.update keep bix m
>           in
>             Just ( sffile.zWordFBoot
>                  , SFFileRuntime 
>                      sffile.zWordFBoot
>                      newPerI
>                      pzdb
>                      sffile.zSquirrelSample)

define signal functions and instrument maps to support rendering ======================================================

> prepareInstruments     :: SFRuntime
>                           → (Map InstrumentName GMChoices, Map PercussionSound GMChoices)
>                           → IO [(InstrumentName, Instr (Stereo AudRate))]
> prepareInstruments runt choices          = do
>     return $ (Percussion, assignPercussion)                                                               : imap
>   where
>     (zI, zP)                             = BF.bimap middle middle choices
>                                              where middle = Map.mapMaybe (\gmr → gmr.gmPerGMKey)
>     imap                                 = Map.foldrWithKey imapFolder [] zI
>     pmap                                 = Map.foldrWithKey pmapFolder [] zP
>
>     imapFolder kind _ target             = (kind, assignInstrument (zI Map.! kind))                       : target
>     pmapFolder kind _ target             = (kind, (pergm.pgkwFile, pergm.pgkwInst))                       : target
>                                              where pergm = zP Map.! kind
>
>     assignInstrument   :: ∀ p . Clock p ⇒ PerGMKey → Instr (Stereo p)
>     assignInstrument pergm durI pch vol params
>                                          =
>       proc _ → do
>         (zL, zR)                         ← instrumentSF runt pergm durI pch vol params ⤙ ()
>         outA                             ⤙ (zL, zR)
>
>     assignPercussion   :: ∀ p . Clock p ⇒ Instr (Stereo p)
>     assignPercussion pDur pch vol ps     = assignInstrument pergm pDur pch vol ps
>       where
>         pergm                            = stdPerGMKey wF (fromIntegral wI)
>         (wF, wI)                         =
>           case lookup kind pmap of
>             Nothing    → error $ unwords ["Percussion does not have", show kind, "in the supplied pmap."]
>             Just x     → x
>         kind           :: PercussionSound
>         kind                             =
>           let
>             percNum                      = pch - 35
>             spread                       = fromEnum OpenTriangle - fromEnum AcousticBassDrum + 1
>           in
>             profess
>               (inZRange percNum spread)
>               (unwords ["assignPercussion illegal pch", show pch])
>               (toEnum percNum)
>
> instrumentSF           :: ∀ p . Clock p ⇒
>                           SFRuntime
>                           → PerGMKey
>                           → Dur
>                           → AbsPitch
>                           → Volume
>                           → [Double]
>                           → Signal p () (Double, Double)
> instrumentSF runt pergm durI pchIn volIn ps
>   | traceIf trace_ISF False              = undefined
>   | otherwise                            = eutSynthesize
>                                              sw
>                                              (reconX, mreconX)
>                                              noon
>                                              (VB.fromList ps)
>                                              reconX.rSampleRate
>                                              durI
>                                              sffile.ks 
>   where
>     fName_                               = "instrumentSF"
>     trace_ISF                            =
>       unwords [fName_, show (pergm.pgkwFile, pergm.pgkwInst), show (pchIn, volIn)]
>
>     dives                                = runt.zDirectives
>     sw                                   = dives.synthSwitches
>
>     sffile                               = runt.zRuntimeFiles IntMap.! pergm.pgkwFile
>     perI                                 = sffile.zPerInstrument IntMap.! fromIntegral pergm.pgkwInst
>
>     noon                                 = carefulNoteOn dives.hackWildMidiValues volIn pchIn
>
>     (reconX, mreconX)                    =
>       case eyeOnTheFly of
>         Left pz                          → (receiveRecon sw pz noon, Nothing)
>         Right (pzL, pzR)                 → (reconL, Just $ copyRoot reconL reconR)
>           where
>             reconL                       = receiveRecon sw pzL noon
>             reconR                       = receiveRecon sw pzR noon
>
>             copyRoot r1 r2               = r2{rRootKey                    = r1.rRootKey
>                                              , rPitchCorrection           = r1.rPitchCorrection}

zone selection for rendering ==========================================================================================

>     eyeOnTheFly        :: Either PreZone (PreZone, PreZone)
>     eyeOnTheFly
>       | traceIf trace_EOTF False         = undefined
>       | null smashSpaces                 = error $ unwords [fName, show smashup, "has no subspaces"]
>       | spL < 0 || cntL <= 0 || spR < 0 || cntR <= 0
>                                          = error $ unwords [fName, show noon, "cell contains nonsense"
>                                                                  , show ((spL, cntL), (spR, cntR))
>                                                                  , show smashStats]
>       | isNothing foundL || isNothing foundR
>                                          = error $ unwords [fName, "spL, spR"
>                                                                  , show (spL, spR)
>                                                                  , "not both present in"
>                                                                  , show (allBixen perI)] 
>       | pzL.pzRecon == pzR.pzRecon       = Left pzL
>       | otherwise                        = Right (pzL, pzR)
>       where
>         fName                            = unwords [fName_, "eyeOnTheFly"]
>         trace_EOTF                       = unwords [fName, show (spL, spR), smashTag]
>
>         smashup@Smashing{ .. }
>                                          = perI.pSmashing
>         (index1, index2)                 = noonAsCoords noon
>
>         ((spL, cntL), (spR, cntR))       = if dives.hackWildJumps
>                                              then useTwoLookupCells
>                                              else useOneGetLeafCells
>           where
>             useTwoLookupCells            = (lookupCell index1 smashup, lookupCell index2 smashup)
>             useOneGetLeafCells           =
>               let
>                 vu                       = getLeafCells index1 smashup
>                 lvu                      = VU.length vu
>               in
>                 profess
>                   (lvu == 2)
>                   (unwords [fName, "Leaf dimension", show lvu, "not two!?!"])
>                   (vu VU.! 0, vu VU.! 1)
>         foundL                           = fromIntegral spL `IntMap.lookup` sffile.zPreZonesRuntime
>         foundR                           = fromIntegral spR `IntMap.lookup` sffile.zPreZonesRuntime
>
>         pzL                              = deJust fName foundL
>         pzR                              = deJust fName foundR

flag to choose algorithm when preparing pzdb for Runtime use ==========================================================
    True : copy all used pzs from old to new
    False : do a deletion fold to erase unused pzs

    Which is better? I did not measure anything, but recommend deploying the more space efficient method, especially
    lower thunk bloat! (OTOH since I am using strict maps that part may be a wash.) I guessed deletion would be better.
    Also keep in mind that the used portion will be generally much smaller than the unused. 

> doCopyPzdb                               = False

The End
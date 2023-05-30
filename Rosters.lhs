> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UnicodeSyntax #-}
>

Rosters support ===========================================================================

> module Main where
>
> import Baking
> import Cecil
> import Control.Monad (foldM)
> import Covers
> import qualified Data.Map as Map
> import Debug.Trace ( traceIO, traceM )
> import Euterpea.IO.MIDI ( play )
> import Euterpea.Music
> import Fanfare
> import Parthenopea
> import SoundFont
> import SunPyg

> main                   :: IO ()
> main = doSoundFont soundFontDatabaseOrig bjingles

organize exposed music ====================================================================

> ajingles, bjingles
>  , cjingles, djingles  :: [(String, Music (Pitch, [NoteAttribute]))]
>
> ajingles =
>    [("theFanfare"      , aggrandize (theFanfare 4))
>    , ("slot"           , aggrandize (slot 4))
>    , ("alice"          , aggrandize alice)
>    , ("bob"            , aggrandize (bob 4))
>    , ("copper"         , aggrandize (copper 2))
>    , ("gold"           , aggrandize gold)
>    , ("silver"         , aggrandize silver)]
> bjingles =
>    [("getCITM"         , aggrandize getCITM)
>    , ("bake"           , bakedJingle 345)
>    , ("bill"           , aggrandize (bill 4))
>    , ("roger"          , aggrandize roger)]
> cjingles =
>    [("cecil"           , aggrandize cecil)
>    , ("abby"           , aggrandize abby)
>    , ("wj"             , aggrandize wj)
>    , ("shelby"         , aggrandize shelby)
>    , ("weHateHer"      , aggrandize weHateHer)]
> djingles =
>    [("waypostpurple"   , aggrandize waypostpurple)
>    , ("whelpNarp"      , aggrandize whelpNarp)
>    , ("snake"          , aggrandize snake)
>    , ("pendingtonArnt" , aggrandize (pendingtonArnt 2))
>    , ("ssailor"        , aggrandize ssailor)]
> zjingles =
>    [("basicLick"       , aggrandize basicLick)
>    , ("sunPyg"         , aggrandize sunPyg)]
> sj =
>    [("sunPyg"          , aggrandize sunPyg)]

organize instruments from multiple SoundFont files ========================================

> lofiInst, hiDefInst, essentialsInst, dSoundFontV4Inst
>                        :: [(String, ([Hints], InstrumentName))]                                
> lofiPerc, hiDefPerc, essentialsPerc, dSoundFontV4Perc
>                        :: [(String, [(String, ([Hints], PercussionSound))])]                                
>
> littleSoundFontDatabase =
>   [
>     ("editDSoundFontV4.sf2",      ([],    (dSoundFontV4Inst, dSoundFontV4Perc)))
>     , ("editKorg_X5_Drums.sf2",   ([],            (korgInst, korgPerc)))
>   ]
>
> soundFontDatabaseOrig =
>   [
>       ("editLofi.sf2",            ([DHigh],       (lofiInst, lofiPerc)))
>     , ("editArachno.sf2",         ([],         (arachnoInst, arachnoPerc)))
>     , ("editHiDef.sf2",           ([DHigh],      (hiDefInst, hiDefPerc)))
>     , ("editKorg_X5_Drums.sf2",   ([],            (korgInst, korgPerc)))
>     , ("editDSoundFontV4.sf2",    ([],    (dSoundFontV4Inst, dSoundFontV4Perc)))
>     , ("editEssentials.sf2",      ([DLow],  (essentialsInst, essentialsPerc)))
>   ]
>
> lofiInst =
>   [
>       ("Lofi Casio Synth 1",      ([],  AcousticGrandPiano))
>   ]
> lofiPerc =
>   [
>   ]
>
> arachnoInst =
>   [
>        ("ContraBass5",            ([], Contrabass))
>      , ("Orchestral Harp1",       ([], ElectricGrandPiano))
>      , ("Charang0",               ([], OverdrivenGuitar))
>      , ("Cello",                  ([], Cello))
>   ]
> arachnoPerc =
>   [
>   ]
>
> hiDefInst =
>   [
>       ("*Choir Aahs 2",           ([],  ChoirAahs))
>     , ("*Slow Violin",            ([],  Viola))
>     , ("'59 Les Paul",            ([],  ElectricGuitarClean))
>     , ("Accordion",               ([],  Accordion))
>     , ("Bagpipe Drone",           ([],  Bagpipe))
>     , ("Bassoon",                 ([],  Bassoon))
>     , ("Cello 2",                 ([],  Cello))
>     , ("ChurOrg2",                ([],  ChurchOrgan))
>     , ("CP Layer 2 Left",         ([],  RhodesPiano))
>     , ("Don's Piano V2",          ([],  BrightAcousticPiano))
>     , ("Elec Bass1",              ([],  ElectricBassFingered))
>     , ("Elec Bass2",              ([],  ElectricBassPicked))
>     , ("Elec Gtr 11",             ([],  ElectricGuitarJazz))
>     , ("Elec Org 4",              ([],  RockOrgan))
>     , ("Flute 2",                 ([],  Flute))
>     , ("Group 1",                 ([],  VoiceOohs))
>     , ("Hard Nylon Guitar",       ([],  AcousticGuitarNylon))
>     , ("Harmonica",               ([],  Harmonica))
>     , ("Harp 2",                  ([],  OrchestralHarp))
>     , ("HonkyTonk1",              ([],  HonkyTonkPiano))
>     , ("Marimba0",                ([],  Marimba))
>     , ("Oboe",                    ([],  Oboe))
>     , ("Piano 1",                 ([],  AcousticGrandPiano))
>     , ("Piccolo 2",               ([],  Piccolo))
>     , ("Sax Sect",                ([],  AltoSax))
>     , ("Sax Sect",                ([],  TenorSax))
>     , ("sitar",                   ([],  Sitar))
>     , ("Slap Bass10",             ([],  SlapBass1))
>     , ("Slap Bass2",              ([],  SlapBass2))
>     {- , ("SoftStringAsp",           ([],  StringEnsemble1)) -}
>     , ("Steel Guitar",            ([],  AcousticGuitarSteel))
>     , ("Syn Bass 1",              ([],  SynthBass1))
>     , ("Syn Bass 2",              ([],  SynthBass2))
>     , ("Synth Strings 2",         ([],  SynthStrings1))
>     , ("Synth Strings 3",         ([],  SynthStrings2))
>     , ("Trumpet 2",               ([],  Trumpet))
>     , ("Vibraphone 2",            ([],  Vibraphone))
>     , ("Violin 11",               ([],  Violin))
>   ]
> hiDefPerc =
>   [
>       ("*POP Drums",               [  ("Maracas",              ([], Maracas)) 
>                                     , ("Ride Cymbal 1",        ([], RideCymbal1))
>                                     , ("Ride Cymbal 2",        ([], RideCymbal2))])
>
>     , ("Don's XG Std Kit",         [  ("Agogo High",           ([], HighAgogo))
>                                     , ("Agogo Low",            ([], LowAgogo))
>                                     , ("Cuica Mute",           ([], MuteCuica))
>                                     , ("Cuica Open",           ([], OpenCuica))])
>
>     -- WOX, ("Drum_Kit_K&S_Room",        [  ("Drum_Snare4",          ([], AcousticSnare))])
>
>     , ("drm: Rock Toms",           [  ("drm-rocktom1m",        ([], HighTom))
>                                     , ("drm-rocktom2m",        ([], HiMidTom))
>                                     , ("drm-rocktom3m",        ([], LowTom))])
>
>     , ("GS Bass Drum 2",           [  ("analog kickl",         ([], BassDrum1))])
>
>     , ("SGM rhythm(cym1)",         [  ("Bongo Hi Op(L)",       ([], HiBongo))
>                                     , ("Bongo Lo Op(L)",       ([], LowBongo))
>                                     , ("Tambourine",           ([], Tambourine))])
>
>     , ("XG Percussion E",          [  ("Cabasa(L)",            ([], Cabasa))
>                                     , ("Crash Cymbal 1",       ([], CrashCymbal1))
>                                     , ("Crash Cymbal 2",       ([], CrashCymbal2))
>                                     , ("Hi-Hat Closed(L)",     ([], ClosedHiHat))
>                                     , ("Hi-Hat Half-Open(L)",  ([], OpenHiHat))
>                                     , ("Splash Cymbal",        ([], SplashCymbal))
>                                     , ("Vibra Slap",           ([], Vibraslap))])
>
>     , ("XG Room Kit",              [  ("DR_SD3_DR_S_038_D_1",  ([], AcousticSnare))])
>   ]
>
> korgInst =
>   [
>   ]
> korgPerc =
>   [
>       ("Brush",                    [  ("Jazz Tom Mid",         ([], LowMidTom))])
>
>     , ("CymbalsACO",               [  ("China Crash",          ([], ChineseCymbal))])
>
>     , ("Electro",                  [  ("Electro Snare",        ([], ElectricSnare))])
>
>     , ("HiHatTECH",                [  ("Hi-Hat Pedal Ana",     ([], PedalHiHat))])
>
>     , ("PercussionACOS",           [  ("Agogo Hi",             ([], HighAgogo))
>                                     , ("Agogo Lo",             ([], LowAgogo))
>                                     , ("Clave",                ([], Claves))
>                                     , ("Conga Low",            ([], LowConga))
>                                     , ("Conga Hi",             ([], OpenHiConga))
>                                     , ("Conga Slap",           ([], MuteHiConga))
>                                     , ("Guiro Long",           ([], LongGuiro))
>                                     , ("Guiro Short",          ([], ShortGuiro))
>                                     , ("Tambourine",           ([], Tambourine))
>                                     , ("Triangle Closed",      ([], MuteTriangle))
>                                     , ("Triangle Open",        ([], OpenTriangle))])
>
>     , ("Room",                     [  ("Room Snare",           ([], ElectricSnare))
>                                     , ("Standard Snare 38",    ([], AcousticSnare))])
>
>     , ("Standard",                 [  ("Clap",                 ([], HandClap))
>                                     , ("Cowbell",              ([], Cowbell))
>                                     , ("Ride Bell",            ([], RideBell))
>                                     , ("Rim Shot Stan",        ([], SideStick))
>                                     , ("Standard Tom Floor",   ([], LowFloorTom))
>                                     , ("Standard Tom Lo",      ([], AcousticBassDrum))
>                                     , ("Standard Tom Mid Hig", ([], HiMidTom))
>                                     , ("Standard Tom High",    ([], HighFloorTom))
>                                     , ("Timbale Hi",           ([], HighTimbale))
>                                     , ("Timbale Low",          ([], LowTimbale))
>                                     , ("Whistle Long",         ([], LongWhistle))
>                                     , ("Whistle Short",        ([], ShortWhistle))])
>   ]
>
> dSoundFontV4Inst =
>   [
>       ("+++Gtr Harmonics",        ([],  GuitarHarmonics))
>     , ("60's Organ 1",            ([],  RockOrgan))
>     , ("AAViolin P",              ([],  Violin))
>     , ("Accordion1",              ([],  Accordion))
>     , ("Agogo",                   ([],  Agogo))
>     , ("Applause0",               ([],  Applause))
>     , ("Banjo",                   ([],  Banjo))
>     , ("Bass31",                  ([],  SlapBass1))
>     , ("Baritone BV",             ([],  BaritoneSax))
>     , ("Bird",                    ([],  BirdTweet))
>     , ("Bottle Blow",             ([],  BlownBottle))
>     , ("Brass",                   ([],  BrassSection))
>     , ("Breath Noise0",           ([],  BreathNoise))
>     , ("Bright Piano",            ([],  HonkyTonkPiano))
>     , ("Bright Spatial Grand",    ([],  BrightAcousticPiano))
>     , ("Campana",                 ([],  TubularBells))
>     , ("Celesta",                 ([],  Celesta))
>     , ("Church Organ",            ([],  ChurchOrgan))
>     , ("Clarinet",                ([],  Clarinet))
>     , ("Classical Guitar 1",      ([],  AcousticGuitarSteel))
>     , ("Clavinet1",               ([],  Clavinet))
>     , ("Dulcimer-Hammered",       ([],  Dulcimer))
>     , ("Eddie's English Horn",    ([],  EnglishHorn))
>     , ("Finger Bass",             ([],  ElectricBassFingered))
>     , ("French Horns",            ([],  FrenchHorn))
>     , ("Funk Gt.",                ([],  AcousticGuitarNylon))
>     , ("German 8 Harpsichord",    ([],  Harpsichord))
>     , ("Glockenspiel 1",          ([],  Glockenspiel))
>     , ("Grand Piano",             ([],  AcousticGrandPiano))
>     , ("Guitar Fret Noise",       ([],  GuitarFretNoise))
>     , ("Gunshot",                 ([],  Gunshot))
>     , ("Helicopter",              ([],  Helicopter))
>     , ("hrp:Harp",                ([],  OrchestralHarp))
>     , ("Iowa Alto Sax",           ([],  AltoSax))
>     , ("Iowa Arco Bass-ff",       ([],  Contrabass))
>     , ("iowa bassoon",            ([],  Bassoon))
>     , ("Iowa Cello-mf",           ([],  Cello))
>     , ("Iowa Marimba",            ([],  Marimba))
>     , ("Iowa Oboe",               ([],  Oboe))
>     , ("IowaTrumpet",             ([],  Trumpet))
>     , ("Iowa Viola-mf",           ([],  Viola))
>     , ("Iowa Woodblock",          ([],  Woodblock))
>     , ("Iowa Xylophone",          ([],  Xylophone))
>     , ("Ixox Flute 1",            ([],  Flute))
>     , ("Kalimba",                 ([],  Kalimba))
>     , ("Koto",                    ([],  Koto))
>     , ("Layered Aahs",            ([],  ChoirAahs))
>     , ("Music Box",               ([],  MusicBox))
>     , ("Ocarina",                 ([],  Ocarina))
>     , ("Ottos Fretless",          ([],  FretlessBass))
>     , ("Pan Flute",               ([],  PanFlute))
>     , ("Piccolo",                 ([],  Piccolo))
>     , ("Picked Bass",             ([],  ElectricBassPicked))
>     , ("Pizzicato",               ([],  PizzicatoStrings))
>     , ("Recorder",                ([],  Recorder))
>     , ("Reed Organ",              ([],  ReedOrgan))
>     , ("Rhodes",                  ([],  RhodesPiano))
>     , ("Seashore",                ([],  Seashore))
>     , ("Shakuhachi",              ([],  Shakuhachi))
>     , ("Shamisen",                ([],  Shamisen))
>     , ("Shenai",                  ([],  Shanai))
>     , ("Sitar",                   ([],  Sitar))
>     , ("soprano sax",             ([],  SopranoSax))
>     , ("Spatial Grand Piano",     ([],  ElectricGrandPiano))
>     , ("Steel Drum",              ([],  SteelDrums))
>     , ("Strat Marshall",          ([],  DistortionGuitar))
>     , ("Stream",                  ([],  Pad2Warm))
>     , ("Synth Bass 3 Rubber",     ([],  SynthBass1))
>     , ("Synth Bass 3",            ([],  SynthBass2))
>     , ("Synth Drum",              ([],  SynthDrum))
>     , ("Synth Brass 1",           ([],  SynthBrass1))
>     , ("Synth Brass 2",           ([],  SynthBrass2))
>     , ("Synth Strings 1",         ([],  SynthStrings1))
>     , ("Synth Strings 2",         ([],  SynthStrings2))
>     , ("Taiko Drum",              ([],  TaikoDrum))
>     , ("TELEPHONE",               ([],  TelephoneRing))
>     , ("Tenor Both Xfade",        ([],  TenorSax))
>     , ("Timpani All",             ([],  Timpani))
>     , ("Tremolo Strings",         ([],  StringEnsemble1))
>     , ("Trombone1",               ([],  Trombone))
>     , ("Tuba",                    ([],  Tuba))
>     , ("Ukelele",                 ([],  Pad3Polysynth))
>     , ("vibraphone",              ([],  Vibraphone))
>     , ("Vocal Oooh",              ([],  VoiceOohs))
>     , ("Whistle",                 ([],  Whistle))
>   ]
> dSoundFontV4Perc =
>   [
>       ("Drumkit Basic 1",         [  ("Agogo High",           ([], HighAgogo))
>                                    , ("Agogo Low",            ([], LowAgogo))
>                                    , ("Iowa High WoodblockL", ([], HiWoodBlock))
>                                    , ("Iowa Low WoodblockL",  ([], LowWoodBlock))
>                                    , ("Iowa Splash CymbalL",  ([], SplashCymbal))
>                                    , ("VibraslapL",           ([], Vibraslap))])
>
>     , ("Iowa Castanets",          [  ("Iowa CastanetL",       ([], Maracas))])
>
>     , ("OSDK closedhihat",        [  ("OSDK chh19L",          ([], ClosedHiHat))])
> 
>     , ("OSDK crash1",             [  ("OSDK crash11L",        ([], CrashCymbal1))])
>
>     , ("OSDK crash2",             [  ("OSDK crash1L",         ([], CrashCymbal2))])
> 
>     , ("OSDK kickdrum",           [  ("OSDK kick1L",          ([], BassDrum1))])
>
>     , ("OSDK openhihat",          [  ("OSDK hohh1L",          ([], OpenHiHat))])
>
>     , ("OSDK Reverse Cymbal",     [  ("OSDK ride-rev11L",     ([], RideCymbal1))])
>
>     , ("OSDK ride1",              [  ("OSDK ride-mid-in1L",   ([], RideCymbal2))])
>
>     , ("OSDK snaredrum1",         [  ("OSDK snare-bottom1L",  ([], AcousticSnare))])
>
>     , ("OSDK tom1",               [  ("OSDK large-tom1L",     ([], LowFloorTom))])
>
>     , ("OSDK tom3",               [  ("OSDK medium-tom1L",    ([], LowTom))])
>
>     , ("OSDK tom6-room",          [  ("OSDK small-tom1-1L",   ([], HighTom))])
>
>     , ("Standard 12",             [  ("Cabasa",               ([], Cabasa))
>                                    , ("M Bongo Tone",         ([], HiBongo))
>                                    , ("Low Tumba",            ([], LowBongo))
>                                    , ("Cuica Mute",           ([], MuteCuica))
>                                    , ("Cuica Open",           ([], OpenCuica))])
>   ]
>
> essentialsInst =
>   [
>       ("Alto Sax XSwitch",        ([],  AltoSax))
>     , ("B3-1 Slow Rotor",         ([],  ReedOrgan))
>     , ("Bassoon",                 ([],  Bassoon))
>     , ("Cello",                   ([],  Cello))
>     , ("ChGrand01v1",             ([],  AcousticGrandPiano))
>     , ("Clarinet",                ([],  Clarinet))
>     , ("Clean Guitar",            ([],  ElectricGuitarClean))
>     , ("DX7 Rhodes",              ([],  RhodesPiano))
>     , ("F",                       ([],  Violin))
>     , ("Flute",                   ([],  Flute))
>     , ("Group 162",               ([],  Viola))
>     , ("Jazz Guitar",             ([],  ElectricGuitarJazz))
>     , ("MagiCs 5Strg Banjo",      ([],  Banjo))
>     , ("Nylon Guitar 1",          ([],  AcousticGuitarNylon))
>     , ("Oboe",                    ([],  Oboe))
>     , ("Palm Muted Guitar",       ([],  ElectricGuitarMuted))
>     , ("Piccolo",                 ([],  Piccolo))
>     , ("Pipe Organ",              ([],  ChurchOrgan))
>     , ("Spanish",                 ([],  AcousticGuitarSteel))
>     , ("String Ensembles",        ([],  StringEnsemble1))
>     , ("Strings Pan",             ([],  StringEnsemble2))
>     , ("Tenor Both Xfade",        ([],  TenorSax))
>     , ("Timpani 1 JN",            ([],  Timpani))
>     , ("Trombone",                ([],  Trombone))
>     , ("Trumpet",                 ([],  Trumpet))
>     , ("Tuba",                    ([],  Tuba))
>     , ("Upright-Piano-1",         ([],  BrightAcousticPiano))
>     -- , ("Violin3",            Violin)
>   ]
> essentialsPerc =
>   []
>

a few playthings ... get it? ==============================================================

> playJingle             :: () → (String, Music (Pitch, [NoteAttribute])) → IO ()
> playJingle _ (s, m) =
>    do
>       traceM ( show s ++ " " ++ show (durS (dur m)) ++ " seconds" )
>       playDM Nothing m
>       
> playJingles            :: [(String, Music (Pitch, [NoteAttribute]))] → IO ()
> playJingles jingles =
>    foldM playJingle () (cycle jingles)
>
> playSnippet            :: () → Int → IO ()
> playSnippet () i =
>    let inst :: InstrumentName
>        inst = toEnum (i `mod` fromEnum Gunshot)
>    in do
>       traceIO ("InstrumentName = " ++ show inst)
>       play $ instrument inst pSnippet02
>
> playSnippets           :: IO ()
> playSnippets = 
>    foldM playSnippet () [0..]
> gUnit :: Music Pitch
> gUnit = addDur qn [f 4, a 4, b 4, a 4, f 4, a 4, b 4, a 4
>                  , e 4, a 4, b 4, a 4, e 4, a 4, b 4, a 4]
>
> gUnitAtVolume          :: Volume → Music (Pitch, Volume)
> gUnitAtVolume vol = addVolume vol gUnit
>
> nylon :: Music (Pitch, Volume)
> nylon =
>   removeZeros
>   $ tempo 1
>   $ transpose 0
>   $ keysig A Major
>   $ instrument Glockenspiel
>     (line [gUnitAtVolume  40, rest hn, gUnitAtVolume  60, rest hn, gUnitAtVolume 80, rest hn
>          , gUnitAtVolume 100, rest hn, gUnitAtVolume 120])
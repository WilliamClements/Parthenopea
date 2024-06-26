> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE UnicodeSyntax #-}

SunPyg
William Clements
March 20, 2023

> module SunPyg (sunPyg) where
>
> import Euterpea.IO.MIDI.Play
> import Euterpea.Music
> import Parthenopea
> import Percussion
  
SunPyg ================================================================================================================

> sunPygTempo                              = 1
> sunPygTranspose                          = 3
>
> sunPygLead_                              = makePitched Violin               sunPygTranspose      0        110
> sunPygChoir_                             = makePitched ChoirAahs            sunPygTranspose      0        110
> sunPygBass_                              = makePitched ElectricBassFingered sunPygTranspose      0         80
> sunPygPerc_                              = makeNonPitched                                                  75
>
> sunPyg dynMap =
>   removeZeros
>   $ aggrandize
>   $ tempo sunPygTempo
>   $ chord [percMusic, leadMusic, vocalMusic, bassMusic]
>
>   where
>
>    sunPygLead                            = replace sunPygLead_ dynMap
>    sunPygChoir                           = replace sunPygChoir_ dynMap
>    sunPygBass                            = replace sunPygBass_ dynMap
>    sunPygPerc                            = replace sunPygPerc_ dynMap
>   
>    percLine = line [m000_004, m005_008, m009_012, m013_016, m017_020, m021_024, m025_028
>                   , m029_032, m033_036, m037_040, m041_044, m045_048, m049_052, m053_056
>                   , m057_060, m061_064, m065_068, m069_072, m073_076, m077_080, m081_084
>                   , m085_088, m089_092, m093_096, m097_100, m101_104, m105_108, m109_112
>                   , m113_116, m117_120, m121_124, m125_128, m129_132, m133_136, m137_140
>                   , m141_144, m145_148, m149_152, m153_156, m157_160, m161_164, m165_168
>                   , m169_172, m173_176, m177_180, m181_184, m185_188, m189_192, m193_196
>                   , m197_200, m201_204, m205_207]
>    leadLine = line [g000_004, g005_008, g009_012, g013_016, g017_020, g021_024, g025_028
>                   , g029_032, g033_036, g037_040, g041_044, g045_048, g049_052, g053_056
>                   , g057_060, g061_064, g065_068, g069_072, g073_076, g077_080, g081_084
>                   , g085_088, g089_092, g093_096, g097_100, g101_104, g105_108, g109_112
>                   , g113_116, g117_120, g121_124, g125_128, g129_132, g133_136, g137_140
>                   , g141_144, g145_148, g149_152, g153_156, g157_160, g161_164, g165_168
>                   , g169_172, g173_176, g177_180, g181_184, g185_188, g189_192, g193_196
>                   , g197_200, g201_204, g205_207]
>   
>    vocalLine = line [vocalTacit, vocalActive]
>    vocalTacit = rest 190
>    vocalActive = line [v190, v192, v194, v196, v198, v200, v202, v204, v206]
>    vocalsOnly =
>      keysig C Lydian
>      $ bandPart sunPygChoir vocalActive
>
>    bassLine = line [rest wn, times 38 (line [c 2 wn, d 2 wn])
>                            , times  8 (line [a 2 wn, d 2 wn])
>                            , times 58 (line [c 2 wn, d 2 wn])]
>   
>    percMusic = bandPart sunPygPerc percLine
>   
>    leadMusic = 
>      keysig C Lydian
>      $ bandPart sunPygLead leadLine
>   
>    vocalMusic = 
>      keysig C Lydian
>      $ bandPart sunPygChoir vocalLine
>   
>    bassMusic =
>      keysig C Lydian
>      $ bandPart sunPygBass bassLine
>   
>    m000 = line [rest dhn, tempo (5/4) (line [percHTsn, percHTen, percLTsn, percLTsn])]
>    m001 = line [percBDen, percOHHen, percCHHen, chord [percCHHen, percBDen]
>               , chord [percCHHen, percLTen], percCHHen, percCHHen
>               , chord [percOHHen, percBDen]]
>    m002 = line [percCHHen, chord [percCHHen, percBDen], chord [percCHHsn, percBDsn]
>               , percCHHsn, percCHHen, chord [percCHHsn, percLTsn], percCHHsn, percCHHen
>               , percCHHsn, percCHHsn, chord [percCHHen, percCHHen]]
>    m003 = line [chord [percCHHen, percBDen], percCHHen, rest en
>               , chord [percCHHsn, percBDsn], percBDsn, chord [percCHHen, percLTen]
>               , percCHHen, rest en, chord [percOHHen, percBDen]]
>    m004 = line [percCHHen, chord [percCHHen, percBDen], chord [percCHHen, percBDen]
>               , percCHHen, chord [percCHHen, percBDen], percCHHen
>               , percCHHsn, percCHHsn, chord [percCHHen, percBDen]]
>   
>    g000 = line [rest dhn, tempo (5/4) (line [fs 5 sn, fs 5 sn, c 5 sn, e 5 en])]
>    g001 = line [rest en, fs 5 en, g 5 en, a 5 en, fs 5 qn, e 5 en, e 5 en]
>    g002 = line [ e 5 en,  d 5 en, d 5 en, d 6 en,  a 5 hn]
>    g003 = line [ a 5 en,  g 5 en, tempo (3/2) (line [g 5 en, fs 5 en, e 5 en])
>               ,  e 5 en,  d 5 en, tempo (3/2) (line [d 5 en,  b 4 en, b 4 en])]
>    g004 = line [tempo (3/2) (line [b 4 en, a 4 en, g 4 en])
>               ,  g 4 en, g 4 en, grace (-2) (a 4 hn)]
>
>    m000_004 = line [m000, m001, m002, m003, m004]
>    g000_004 = line [g000, g001, g002, g003, g004]
>
>    m005_008 = line [m005, m006, m007, m008]
>    g005_008 = line [g005, g006, g007, g008]
>
>    m005 = line [chord [percCHHen, percBDen], percm ClosedHiHat [sn, sn, sn, sn]
>               , chord [percCHHsn, percBDsn], chord [percCHHsn, percBDsn]
>               , chord [percCHHen, percLTen], percCHHen, percCHHen
>               , chord [percOHHen, percBDen]]
>    m006 = line [chord [percCHHen, percBDen], chord [percCHHen, percBDen], percBDen
>               , chord [percCHHen, percBDen], percLTen, percCHHen, rest en
>               , chord [percCHHen, percBDen]]
>    m007 = line [chord [percCHHen, percBDen], percCHHen, percCHHsn, percCHHsn
>               , chord [percCHHsn, percBDsn], percBDsn, chord [percCHHen, percLTen]
>               , percOHHen, percCHHen, chord [percCHHen, percBDen]]
>    m008 = line [chord [percCHHsn, percBDsn], percCHHsn, percCHHen, percCHHsn, percCHHsn
>               , chord [percCHHen, percBDen], chord [percCHHen, percLTen], percOHHen
>               , percCHHen, chord [percCHHen, percBDen]]
>
>    g005 = line [chord [e 4 en, a 4 en], chord [c 4 en, c 5 en], c 5 en, d 5 en, c 5 en
>               , c 5 en, tempo (3/2) (line [c 5 en, d 5 en, chord [g 4 en, c 5 en]])]
>    g006 = line [grace (-2) (e 5 qn), tempo (3/2) (line [e 5 qn, e 5 en])
>               , tempo (3/2) (line [e 5 en, rest en, g 5 en, g 5 qn, g 5 en])]
>    g007 = line [tempo (3/2) (line [g 5 en, e 5 en, e 5 en, e 5 sn, fs 5 sn, g 5 sn])
>               , g 5 en, tempo (3/2) (line [g 5 sn, fs 5 sn, e 5 sn]), fs 5 en
>               , tempo (3/2) (line [fs 5 sn, e 5 sn, d 5 sn]), e 5 sn, d 5 sn]
>    g008 = line [d 5 hn, descent sunPygLead (D, 5) dqn, grace (-1) (d 6 en)]
>   
>    m009_012 = line [m009, m010, m011, m012]
>    g009_012 = line [g009, g010, g011, g012]
>   
>    m009 = line [chord [percCHHen, percBDen], percOHHen, rest en, chord [percCHHen, percBDen]
>               , chord [percCHHen, percLTen], percOHHen, percCHHsn, percCHHsn
>               , chord [percOHHen, percBDen]]
>    m010 = line [rest en, chord [percOHHen, percBDen], chord [percCHHsn, percBDsn], percCHHsn
>               , chord [percOHHen, percBDen], chord [percCHHsn, percLTsn], percCHHsn
>               , percOHHen, percCHHsn, percCHHsn, percCHHen]
>    m011 = line [chord [percCHHsn, percBDsn], percCHHsn, percOHHen, percCHHsn, percCHHsn
>               , chord [percOHHen, percBDen], chord [percCHHsn, percLTsn], percCHHsn
>               , percOHHen, percCHHsn, percCHHsn, chord [percOHHen, percBDen]]
>    m012 = line [chord [percCHHsn, percBDsn], percCHHsn, chord [percOHHen, percBDen]
>               , percm ClosedHiHat [sn, sn, sn], percBDsn, percLTen, percOHHen, percCHHsn
>               , percHTsn, tempo (3/2) (percm HighTom [sn, sn, sn])]
>   
>    g009 = line [tempo (3/2)
>                       (line [d 6 dqn, chord [c 5 en, d 6 en]
>                            , tempo (5/4) (line [e 6 sn, d 6 sn, e 6 sn, d 6 sn, b 5 sn])])
>               , b 5 sn, b 6 sn, b 5 sn, a 5 sn, b 5 qn]
>    g010 = line [tempo (3/2) (line [b 5 en, b 5 sn]), a 5 en
>               , tempo (3/2) (line [a 5 en, a 5 en, a 5 sn, b 5 sn]), c 6 en
>               , chord [b 5 tn, e 5 tn], a 5 tn, rest sn, rest qn]
>    g011 = line [tempo (3/2) (line [rest en, b 5 en, c 6 en]), d 6 en
>               , tempo (3/2) (line [d 6 sn, c 6 sn, b 5 sn]), c 6 en, b 5 sn
>               , grace (-4) (e 6 (sn+qn))]
>    g012 = line [e 6 sn, d 6 sn, d 6 sn, d 6 dqn, rest sn, fs 6 sn, d 6 sn, rest sn, fs 6 sn
>               , tempo (3/2) (line [fs 6 sn, e 6 sn, d 6 sn])]
>
>    m013_016 = line [m013, m014, m015, m016]
>    g013_016 = line [g013, g014, g015, g016]
>
>    m013 = line [chord [percCCen, percHTen, percBDen], percOHHen, percCHHen
>               , chord [percOHHen, percBDen], chord [percCHHen, percLTen], percOHHen
>               , percCHHen, chord [percOHHen, percBDen]]
>    m014 = line [chord [percCHHen, percBDen], chord [percCHHen, percHTen], percCHHsn, percCHHsn
>               , chord [percOHHen, percBDen], chord [percCHHsn, percBDsn], percCHHsn, percOHHen
>               , percCHHen, chord [percOHHen, percHTen, percBDen]]
>    m015 = line [chord [percCHHen, percBDen], percOHHen, percCHHen
>               , chord [percCHHsn, percBDsn], percBDsn, chord [percCHHen, percLTen], percOHHen
>               , chord [percCHHen, percBDen], chord [percCHHsn, percBDsn], percLTsn]
>    m016 = line [chord [percCHHsn, percBDsn], percLTsn, chord [percCHHen, percBDen]
>               , percCHHen, chord [percCHHsn, percBDsn], percBDsn
>               , chord [percCHHen, percLTen], percCHHen, chord [percCHHsn, percHTsn], percHTsn
>               , chord [percCHHsn, percSDsn], percSDsn]
>
>    g013 = line [tempo (5/4) (line [e 6 en, e 6 sn, d 6 sn, b 5 sn]), d 6 sn, b 5 sn, a 5 en
>               , tempo (3/2) (line [a 5 en, a 5 en, a 5 en, c 6 en, b 5 en, a 5 en])]
>    g014 = line [tempo (7/4)
>                 (line [grace (-2) (a 5 en), a 5 en, g 5 en, fs 5 en, grace 2 (c 5 dqn)])
>               , c 5 qn, tempo (3/2) (line [a 4 en, c 5 en, c 5 en])]
>    g015 = line [grace (-2) (d 5 den), g 5 sn, fs 5 sn, g 5 sn, a 5 en, rest sn, fs 5 en
>               , fs 5 tn, d 5 tn, tempo (3/2) (line [d 5 sn, c 5 sn, a 4 en, c 5 en])]
>    g016 = line [tempo (3/2) (line [c 5 en, c 5 sn, a 4 sn, c 5 sn, a 4 sn])
>               , grace 1 (g 4 en), tempo (3/2) (line [g 4 sn, fs 4 sn, d 4 sn]), c 4 en
>               , d 4 en, d 4 en, grace (-1) (g 4 en)]
>
>    m017_020 = line [m017, m018, m019, m020]
>    g017_020 = line [g017, g018, g019, g020]
>   
>    m017 = line [chord [perc CrashCymbal1 qn, line [percBDen, rest en]], percOHHen, percCHHen
>               , chord [percOHHen, percBDen], chord [percCHHen, percLTen], percCHHsn
>               , percCHHsn, chord [percOHHen, percBDen]]
>    m018 = line [percCHHen, chord [percOHHen, percBDen], chord [percCHHen, percBDen]
>               , chord [percOHHen, percBDen], chord [percCHHen, percLTen], percOHHen
>               , percCHHsn, percBDsn, chord [percOHHen, percBDen]]
>    m019 = line [chord [percCHHen, percBDen], percOHHen, percCHHen, percOHHen
>               , chord [percCHHen, percLTen], percOHHen, percCHHen, percOHHen]
>    m020 = line [chord [percCHHen, percLTen], chord [percOHHen, percBDen]
>               , chord [percCHHen, percBDen], chord [percOHHen, percLTen], percCHHen
>               , chord [percOHHsn, percBDsn], percBDsn, percCHHsn, percBDsn, percOHHen]
>    g017 = line [rest sn, fs 4 sn, g 4 (en+hn), e 4 en, grace (-2) (fs 4 en)]
>    g018 = line [rest sn, d 4 den, e 4 den, grace (-2) (fs 4 den), e 4 en, fs 4 en
>               , grace (-1) (g 4 en)]
>    g019 = line [rest sn, fs 4 sn, g 4 dhn, tempo (3/2) (line [c 5 tn, e 5 tn, d 5 tn])
>               , b 4 sn]
>    g020 = line [tempo (3/2) (line [b 4 en, b 4 en, a 4 en])
>               , tempo (3/2) (line [b 4 en, b 4 en, grace (-2) (d 5 en)])
>               , tempo (3/2) (line [b 4 en, a 4 en, b 4 en]), a 4 en]
>
>    m021_024 = line [m021, m022, m023, m024]
>    g021_024 = line [g021, g022, g023, g024]
>
>    m021 = line [chord [percCHHen, percBDen], percOHHen, percCHHen
>               , chord [percOHHen, percBDen], chord [percCHHen, percLTen], percOHHen
>               , percCHHen, chord [percOHHen, percBDen]]
>    m022 = line [percm ClosedHiHat [en, en, sn, sn], percOHHen, chord [percCHHen, percLTen]
>               , percOHHen, percCHHen, chord [percOHHen, percBDen]]
>    m023 = line [percCHHsn, percCHHsn, percOHHen, percCHHsn, percCHHsn
>               , chord [percCHHen, percBDen], chord [percCHHen, percLTen], percOHHen
>               , percCHHsn, percCHHsn, percBDsn, percBDsn]
>    m024 = line [chord [percCHHen, percLTen], chord [percOHHen, percBDen]
>               , chord [percCHHen, percBDen], chord [percCHHen, line [percLTsn, percBDsn]]
>               , chord [percCHHen, line [percBDsn, percBDsn]], chord [percCHHen, percBDen]
>               , chord [percCHHen, percLTen], chord [percCHHen, line [percLTsn, percLTsn]]]
>
>    g021 = line [tempo (3/2) (line [g 4 en, g 4 en, a 4 en, g 4 en, g 4 en, fs 4 en])
>               , tempo (3/2) (line [fs 4 en, fs 4 en, e 4 en])
>               , tempo (3/2)
>            (line [e 4 en, tempo (3/2) (line [fs 4 sn, e 4 sn, d 4 sn]), e 4 sn, d 4 sn])
>               , b 3 sn, a 3 sn]
>    g022 = line [a 3 qn, a 3 sn, g 3 sn, g 3 en, g 3 sn, g 3 sn, fs 3 en, fs 3 en
>               , tempo (3/2) (line [fs 3 sn, g 3 sn, fs 3 sn])]
>    g023 = line [tempo (3/2) (line [e 3 qn, e 3 en]), e 3 qn, grace (-5) (d 4 en), d 4 en
>               , d 4 en, d 4 sn, b 3 sn]
>    g024 = line [d 4 (hn+en), e 3 en, rest en
>               , tempo (3/2) (line [grace (-4) (e 5 en), e 5 sn])]
>   
>    m025_028 = line [m025, m026, m027, m028]
>    g025_028 = line [g025, g026, g027, g028]
>   
>    m025 = line [chord [percCCen, percBDen], percm RideCymbal1 [sn, sn, en]
>               , chord [percRCen, percBDen], chord [percRCsn, percLTsn]
>               , percm RideCymbal1 [sn, en, sn, sn], chord [percRCen, percBDen]]
>    m026 = line [chord [percRCsn, percBDsn], percm RideCymbal1 [sn, en, sn, sn]
>               , chord [percRCsn, percBDsn], percBDsn, chord [percRCsn, percLTsn]
>               , percm RideCymbal1 [sn, en, sn, sn], chord [percRCen, percBDen]]
>    m027 = m026
>    m028 = line [chord [percRCsn, percBDsn], percm RideCymbal1 [sn, en, sn, sn]
>               , chord [percCHHsn, percBDsn], percBDsn
>               , chord [percRCsn, percLTsn], chord [percRCsn, percBDsn], percRCen
>               , chord [percRCsn, percBDsn], percRCsn, chord [percRCen, percLTen]]
>   
>    g025 = line [c 5 en, fs 4 en, g 4 sn, fs 4 sn, rest sn
>               , tempo (3/2) (line [fs 4 tn, g 4 tn, d 4 tn])
>               , tempo (3/2) (line [fs 4 en, fs 4 sn]), e 4 sn
>               , tempo (3/2) (line [d 4 tn, d 4 tn, d 4 tn])
>               , tempo (3/2) (line [d 4 sn, b 3 sn, c 4 sn, b 3 sn, a 3 sn, a 3 sn])]
>    g026 = line [ascent sunPygLead (E, 3) dhn, tempo (5/4) (line [rest en, fs 4 en, fs 4 sn])]
>    g027 = line [tempo (3/2) (line [chord [d 4 sn, g 4 sn, c 5 sn], fs 4 en]), fs 4 sn
>               , g 4 sn, tempo (3/2) (line [g 4 sn, fs 4 sn, d 4 sn, fs 4 en, fs 4 sn])
>               , tempo (3/4) (line [tempo (3/2) (line [e 4 en, fs 4 sn]), e 4 sn, fs 4 sn
>                            , e 4 tn, fs 4 tn, e 4 tn, d 4 tn])]
>    g028 = line [b 3 en, chord [e 3 en, a 3 en], tempo (3/2) (line [g 3 en, fs 3 qn])
>               , tempo (3/2) (line [e 3 qn, e 3 en])
>               , tempo (3/2) (line [e 3 sn, fs 3 sn, g 3 sn]), g 3 sn, a 3 sn]
>   
>    m029_032 = line [m029, m030, m031, m032]
>    g029_032 = line [g029, g030, g031, g032]
>   
>    m029 = line [chord [percRCsn, percBDsn], percm RideCymbal1 [sn, en, sn, sn]
>               , chord [percRCsn, percBDsn], percBDsn
>               , chord [percRCsn, percLTsn], chord [percRCsn, percBDsn], percRCen
>               , percRCsn, percRCsn, chord [percRCen, percBDen]]
>    m030 = line [percCHHsn, percCHHsn, chord [percCHHen, percBDen], percCHHsn, percCHHsn
>               , chord [percCHHsn, percBDsn], percBDsn
>               , chord [percCHHsn, percLTsn], percm ClosedHiHat [sn, en, sn, sn]
>               , percLTen]
>    m031 = line [chord [percRCen, percBDen], percm RideCymbal1 [en, sn, sn]
>               , chord [percRCsn, percBDsn], percBDsn, chord [percRCsn, percLTsn]
>               , percm RideCymbal1 [sn, en, sn], chord [percRCsn, percBDsn]
>               , percRCsn, percBDsn]
>    m032 = line [chord [percRCsn, percBDsn], chord [percRCen, percBDen]
>               , chord [percRCsn, percBDsn], chord [percRCen, percBDen], percRCen
>               , chord [percRCen, percLTen], chord [percRCsn, percBDsn], percBDsn
>               , chord [percRCsn, percLTsn], percBDsn, chord [percCCen, percBDen]]
>   
>    g029 = line [a 3 sn, g 3 en, g 3 en, a 3 sn, b 3 en
>               , a 3 en, a 3 en, a 3 sn, a 3 sn, b 3 en]
>    g030 = line [b 3 en, b 3 sn, c 4 sn, b 3 en
>               , tempo (3/2) (line [grace (-1) (c 4 en), c 4 sn, c 4 en, c 4 en, c 4 en])
>               , tempo (3/2) (line [grace (-2) (d 4 en), b 3 sn, b 3 sn, b 3 en])]
>    g031 = line [c 4 sn, grace 1 (b 3 (den + en)), rest qn, b 3 en, b 3 en, g 4 en]
>    g032 = line [g 4 en, fs 4 en, descent sunPygLead (Fs, 4) hn, rest qn]
>   
>    m033_036 = line [m033, m034, m035, m036]
>    g033_036 = line [g033, g034, g035, g036]
>   
>    m033 = line [chord [percRCen, percBDen], percm RideCymbal1 [sn, sn, sn, sn]
>               , chord [percRCsn, percBDsn], chord [percRCsn, percBDsn]
>               , chord [percRCen, percLTen], percRCen, chord [percRCsn, percBDsn]
>               , percRCsn, chord [percRCsn, percBDsn], percBDsn]
>    m034 = line [chord [percCCen, percLTen, percBDen], chord [percCCsn, percBDsn]
>               , chord [percCCsn, percBDsn], chord [percCCen, percBDen]
>               , chord [percCCen, percBDen], chord [percCCen, percLTen], percCCen
>               , percCCen, chord [percCCen, percBDen]]
>    m035 = line [chord [percCCsn, percBDsn], percm LowTom [sn, sn, sn]
>               , chord [percRCen, percLTen], chord [percRCen, percBDen]
>               , chord [percRCen, percLTen], percm RideCymbal1 [sn, sn, en]
>               , chord [percRCsn, percBDsn], chord [percRCsn, percBDsn]]
>    m036 = line [percLTsn, percLTsn, chord [percRCsn, percBDsn], percBDsn
>               , percLTsn, percLTsn, chord [percRCsn, percBDsn], percBDsn
>               , percBDsn, percBDsn, chord [percRCsn, percLTsn], percLTsn
>               , chord [percRCsn, percBDsn], percBDsn
>               , chord [percRCsn, percLTsn], percLTsn]
>   
>    g033 = line [rest en, grace 4 (b 4 en), fs 4 en, tempo (3/2) (line [g 4 sn, d 4 en])
>               , tempo (3/2) (line [g 4 en, g 4 sn, fs 4 sn, d 4 en, fs 4 en, fs 4 sn
>               , e 4 sn, d 4 en])]
>    g034 = line [d 4 tn, e 4 tn, fs 4 sn, fs 4 en
>               , tempo (3/2) (line [fs 4 sn, d 4 sn, g 4 en, tempo (3/2) (line [g 4 sn, fs 4 sn, e 4 sn])])
>               , tempo (3/2) (line [tempo (5/2) (line [fs 4 en, fs 4 sn, e 4 sn, d 4 sn]), e 4 qn])
>               , e 4 en, grace (-6) (fs 4 en)]
>    g035 = line [fs 4 en, e 4 en, e 4 en, grace (-2) (a 4 sn), fs 4 sn
>               , tempo (3/2) (line [fs 4 en, fs 4 sn, fs 4 sn, g 4 sn, fs 4 sn])
>               , tempo (5/4) (line [e 4 en, fs 4 sn, e 4 sn, d 4 sn])]
>    g036 = line [e 4 en, ascent sunPygLead (D, 4) dqn, rest dqn, tempo (3/2) (line [e 5 en, e 5 sn])]
>   
>    m037_040 = line [m037, m038, m039, m040]
>    g037_040 = line [g037, g038, g039, g040]
>   
>    m037 = line [chord [percCCen, percBDen], percm RideCymbal1 [sn, sn, en]
>               , chord [percRCen, percBDen], chord [percRCen, percLTen]
>               , percm RideCymbal1 [sn, sn, en], chord [percRCen, percBDen]]
>    m038 = line [percRCsn, percRCsn, chord [percRCen, percBDen], chord [percRCsn, percBDsn]
>               , percRCsn, percRCen, chord [percRCsn, percLTsn], percRCsn, percRCen
>               , chord [percRCsn, percBDsn], percRCsn, chord [percRCen, percBDen]]
>    m039 = line [chord [percRCen, percBDen], chord [percRCsn, percLTsn, percBDsn], percLTsn
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [percRCsn, percLTsn, percBDsn], percBDsn, chord [percRCen, percBDen]
>               , chord [percCCen, percBDen], chord [percCCsn, percLTsn, percBDsn], percBDsn]
>    m040 = line [percCCen, chord [percCCsn, percBDsn], percBDsn
>               , chord [percCCen, percLTen, percBDen], chord [percCCsn, percBDsn], percBDsn
>               , chord [percRCen, percLTen, percBDen], percLTsn, percLTsn
>               , percBDsn, chord [percRCsn, percLTsn, percBDsn], percLTsn
>               , chord [percCCsn, percLTsn, percBDsn]]
>   
>    g037 = line [grace 2 (c 5 en), fs 4 sn, g 4 sn
>               , tempo (3/2) (line [g 4 sn, fs 4 sn, d 4 sn, fs 4 en, fs 4 sn])
>               , tempo (3/2) (line [e 4 en, e 4 en, fs 4 en, fs 4 en
>               , tempo (3/2) (line [e 4 sn, fs 4 sn, e 4 sn]), d 4 en])]
>    g038 = line [tempo (3/2) (line [d 4 en, d 4 sn, d 4 en, grace (-6) (b 3 sn)]), a 3 tn, b 3 tn, a 3 tn, b 3 tn
>               , a 3 sn, g 3 den, fs 3 en, tempo (3/2) (line [fs 3 en, e 3 en, fs 3 en])]
>    g039 = line [tempo (3/2) (line [e 3 en, a 3 sn, b 3 sn, a 3 sn, b 3 sn, a 3 en, e 3 en, fs 3 en])
>               , tempo (3/2) (line [g 3 en, a 3 sn, b 3 sn, a 3 sn, b 3 sn, a 3 en, a 3 sn, b 3 en, c 4 sn])]
>    g040 = line [tempo (3/2) (line [a 3 en, b 3 sn]), a 3 en, a 3 sn, a 3 sn, a 3 sn, grace (-1) (c 4 den), b 3 en
>               , b 3 tn, c 4 tn, b 3 tn, c 4 tn, a 3 en]
>   
>    m041_044 = line [m041, m042, m043, m044]
>    g041_044 = line [g041, g042, g043      ]
>   
>    m041 = line [percCCen, chord [percCCen, percBDen], chord [percCCen, percBDen], percCCen
>               , chord [percRCen, percLTen], percRCen, percRCen, chord [percRCen, percBDen]]
>    m042 = line [chord [percRCen, percBDen], percRCsn, chord [percRCsn, percBDsn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [percCCen, percLTen], percCCsn, chord [percCCsn, percBDsn], perc LowTom qn]
>    m043 = line [chord [percCCen, percBDen], percRCen, percRCen, percRCsn, percBDsn
>               , chord [percRCen, percLTen], percRCen, percRCsn, percBDsn, percHTsn, percHTsn]
>    m044 = line [chord [percCCen, percBDen], chord [percCCen, percBDen], percCCsn, percBDsn
>               , chord [percCCsn, percLTsn], percHTsn, chord [percCCsn, percHTsn], percHTsn
>               , chord [percCCsn, percBDsn], chord [percCCsn, percBDsn]
>               , tempo (3/2) (line [percCCen, percLTsn, chord[percCCen, percBDen], percLTsn])]
>   
>    g041 = line [grace 3 (a 3 qn), tempo (3/2) (line [a 3 en, b 3 en, d 4 en])
>               , d 4 qn, e 4 sn, d 4 sn, a 3 en]
>    g042 = line [a 3 sn, b 3 sn, d 4 sn, d 4 sn, d 4 en, e 4 sn, d 4 sn
>               , a 3 sn, b 3 sn, d 4 sn, grace (-2) (fs 4 sn), rest sn, d 4 sn, e 4 en]
>    g043 =
>      line [
>        tempo (11/8)
>          (line [
>             tempo (3/2) (line [chord [d 4 qn, d 5 qn], d 4 en, grace (-2) (e 4 qn), d 4 en])
>           , tempo (3/2) (line [grace (-4) (fs 4 qn), d 4 en, e 4 qn, d 4 en])
>           , tempo (3/2) (line [d 4 qn, d 4 en, e 4 qn, d 4 en, fs 4 qn, d 4 en, g 4 qn, d 4 en])
>           , tempo (3/2) (line [g 4 qn, d 4 en, g 4 qn, d 4 en, a 4 qn, d 4 en])])]
>   
>    m045_048 = line [m045, m046, m047, m048]
>    g045_048 = line [g045, g046, g047, g048]
>   
>    m045 = line [chord [percRCen, percBDen], percRCsn, chord [percRCsn, percBDsn]
>               , percRCen, chord [percRCen, percBDen], chord [percRCen, percLTen], percRCen
>               , percRCsn, times 3 (chord [percHTsn, percLTsn])]
>    m046 = line [chord [percCCen, percHTen, percBDen], percRCen, percRCen, percBDsn, percRCsn
>               , chord [perc RideCymbal1 den, perc LowTom den], chord [percRCsn, percBDsn]
>               , percRCen, chord [percRCen, percBDen]]
>    m047 = line [chord [percRCen, percBDen], percm RideCymbal1 [sn, sn, sn]
>               , chord [percRCsn, percBDsn], percRCsn, chord [percRCsn, percBDsn]
>               , chord [percRCen, percLTen], percRCen, percRCen, chord [percRCen, percBDen]]
>    m048 = line [percLTsn, chord [perc CrashCymbal1 den, perc BassDrum1 den]
>               , chord [percCCen, percBDen], percLTsn, percLTsn, percCCsn, percBDsn, percLTen
>               , chord [percCCen, percLTen], percBDen]
>   
>    g045 =
>      line [
>        tempo (11/8)
>          (line [
>             tempo (3/2) (line [a 4 qn, d 4 en, b 4 qn, d 4 en, b 4 qn, d 4 en])
>           , c 5 en, tempo (3/2) (line [c 5 sn, b 4 sn, a 4 sn])
>           , tempo (5/4) (line [b 4 sn, c 5 sn, b 4 sn, a 4 sn, b 4 sn]), a 4 sn, g 4 sn])]
>    g046 = line [tempo (5/4) (line [fs 4 sn, e 4 sn, grace (-2) (fs 4 sn), e 4 sn, a 3 sn])
>               , d 4 en, b 3 sn, a 3 (sn + dqn), a 3 en]
>    g047 = line [e 4 den, b 4 (sn + qn + den), d 4 sn, d 4 sn, d 4 sn, a 4 en]
>    g048 = line [rest sn, grace (-4) (e 5 (den + dqn))
>               , tempo (5/3) (addDur en [b 4, e 4, a 3, e 5, a 4])]
>   
>    m049_052 = line [m049, m050, m051, m052]
>    g049_052 = line [g049, g050, g051, g052]
>   
>    m049 = line [times 4 (chord [percRCen, percBDen]), chord [percRCen, percLTen]
>               , percm RideCymbal1 [en, sn, sn], chord [percRCen, percBDen]]
>    m050 = line [chord [percRCen, percBDen], percRCen, chord [percRCen, percLTen]
>               , chord [percRCsn, percBDsn], chord [percRCsn, percBDsn]
>               , chord [percRCsn, percLTsn], chord [percRCen, percBDen], percRCsn
>               , chord [percRCen, percBDen], chord [percRCen, percLTen]]
>    m051 = line [chord [percRCen, percBDen], percRCsn, percRCsn, percRCen
>               , chord [percRCen, percBDen], times 2 (chord [percRCen, percBDen]), percLTen
>               , chord [percRCsn, percBDsn], percBDsn]
>    m052 = line [chord [percRCen, percHTen], percRCsn, chord [percOHHsn, percBDsn], percCHHsn
>               , chord [percRCen, percBDen], percRCsn, chord [percRCen, percBDen]
>               , percm LowTom [sn, sn, sn], chord [perc RideCymbal1 den, perc BassDrum1 den]]
>   
>    g049 = line [d 4 sn, b 4 sn, e 4 sn, a 3 sn, b 4 en, e 4 en
>               , a 3 en, e 5 en, g 4 sn, d 4 sn, g 4 en]
>    g050 = line [a 4 en, d 5 en, e 5 en, fs 5 en, grace (-2) (g 5 en)
>               , addDur tn [fs 5, g 5, fs 5, e 5]
>               , tempo (3/2) (line [fs 5 en, fs 5 sn, e 5 sn, d 5 sn, e 5 sn])]
>    g051 = line [b 5 en, fs 5 en, tempo (3/2) (line [fs 5 sn, g 5 sn, fs 5 sn]), fs 5 en
>               , rest qn, b 5 en, fs 5 en]
>    g052 = line [fs 5 sn, g 5 sn, fs 5 qn, bf 5 sn, g 5 sn
>               , tempo (3/2) (line [fs 5 en, fs 5 sn, chord [b 4 sn, a 5 sn], g 5 sn, fs 5 sn])
>               , fs 5 sn, tempo (3/2) (line [bf 5 sn, g 5 sn, fs 5sn]), a 5 sn]
>   
>    m053_056 = line [m053, m054, m055, m056]
>    g053_056 = line [g053, g054, g055, g056]
>   
>    m053 = line [chord [percRCen, percBDen], percm RideCymbal1 [sn, sn, en]
>               , chord [percRCen, percBDen], chord [percRCen, percLTen], percRCsn, percBDsn
>               , percRCen, chord [percRCen, percBDen]]
>    m054 = line [percRCen, chord [percRCen, percLTen], chord [percRCsn, percLTsn]
>               , chord [percRCen, percBDen], chord [percRCsn, percBDsn]
>               , chord [percRCsn, percBDsn], percLTen, chord [percRCsn, percBDsn]
>               , perc RideCymbal1 den, percBDsn]
>    m055 = line [chord [perc RideCymbal1 den, perc LowTom den], chord [percRCsn, percBDsn]
>               , times 2 (chord [percRCen, percBDen]), chord [percRCen, percLTen]
>               , perc LowTom den, chord [percRCsn, percBDsn], chord [percRCen, percBDen]]
>    m056 = line [chord [perc RideCymbal1 den, perc BassDrum1 den], chord [percRCsn, percBDsn]
>               , rest sn, chord [percRCsn, percBDsn], chord [percRCen, percBDen]
>               , chord [percRCsn, percBDsn], chord [percRCen, percBDen]
>               , chord [percRCsn, percBDsn]
>               , tempo (3/2) (line [rest en, chord [percRCen, percBDen], chord [percRCen, percBDen]])]
>   
>    g053 =
>      line [
>        tempo (3/2)
>           (line [
>             addDur sn [a 5, g 5, fs 5, g 5, bf 5, g 5], fs 5 en
>           , tempo (5/4) (line [a 5 en, a 5 sn, g 5 sn, fs 5 sn])])
>        , g 5 sn, a 5 sn, tempo (3/2) (line [chord [e 5 sn, a 5 sn], g 5 sn, fs 5 sn])
>        , tempo (3/2)
>             (line [
>               chord [e 5 en, g 5 en], chord [e 5 sn, g 5 sn]
>             , fs 5 sn, e 5 sn, fs 5 sn])]
>    g054 = line [grace (-3) (bf 5 en), a 5 en, g 5 en, g 5 sn, fs 5 sn, fs 5 sn
>               , tempo (3/2) (line [fs 5 tn, g 5 tn, fs 5 tn])
>               , chord [d 5 sn, fs 5 sn], chord [a 3 den, d 5 den, fs 5 (sn + qn)]]
>    g055 = line [fs 5 dqn, fs 5 sn, g 5 sn, chord [c 6 sn, e 6 sn], b 5 sn, c 6 sn
>               ,  b 5 sn, c 6 sn, b 5 sn, tempo (3/2) (line [b 5 sn, c 6 sn, b 5 sn])]
>    g056 = line [grace (-2) (d 6 en), chord [a 5 sn, d 6 sn], grace (-2) (e 6 sn)
>               , e 6 qn, tempo (3/2) (line [addDur sn [e 6, fs 6, e 6, d 6, e 6, fs 6]])
>               , f 6 sn, fs 6 sn, tempo (3/2) (line [f 6 en, b 5 sn])]
>   
>    m057_060 = line [m057, m058, m059, m060]
>    g057_060 = line [g057, g058, g059, g060]
>   
>    m057 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [perc CrashCymbal1 qn, perc LowTom qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]]
>    m058 = line [rest en, chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , chord [percCCsn, percBDsn], percBDsn, chord [percCCen, percLTen]
>               , chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , tempo (3/2) (line [percm HighTom [sn, sn, sn]])]
>    m059 = line [chord [percCCen, percHTen, percBDen], percCCen, chord [percCCen, percBDen]
>               , chord [percCCen, percBDen]
>               , chord [percm CrashCymbal1 [en, sn, den, sn, sn]
>                      , percm LowTom [dden, tn], perc HighTom (tn+en), percBDsn, percBDen]]
>    m060 = line [tempo (3/2) (line [chord [percCCsn, percHTsn, percBDsn], percHTsn, percLTsn])
>               , rest sn, chord [percCCsn, percBDsn]
>               , tempo (3/2) (line [percHTsn, percHTsn, percLTsn]), chord [percCCen, percBDen]
>               , times 3 (line [chord [percCCsn, percLTsn], percBDsn])
>               , tempo (3/2) (line [percm HighTom [sn, sn, sn]])]
>   
>    g057 = line [d 6 dqn, c 6 en, rest en, tempo (3/2) (line [a 5 en, a 5 sn]), fs 5 sn
>               , a 5 sn, tempo (3/2) (line [a 5 sn, g 5 sn, fs 5 sn])]
>    g058 = line [tempo (3/2) (line [g 5 sn, fs 5 sn, e 5 sn]), fs 5 sn, e 5 den
>               , tempo (3/2) (line [f 5 sn, e 5 sn, d 5 sn])
>               , tempo (3/2) (line [d 5 sn, b 4 sn, b 4 sn, a 4 en, d 5 tn, b 4 tn]), a 4 qn]
>    g059 = line [a 4 qn, tempo (3/2) (line [a 3 en, d 4 sn]), a 4 en, g 4 en
>               , tempo (3/2) (line [c 5 sn, g 4 sn, d 4 sn, g 4 en, g 4 en, g 4 en])]
>    g060 = line [addDur sn [g 4, fs 4, d 5, d 4, g 4, fs 4, d 5], d 4 en, grace (-2) (a 4 sn)
>               , g 4 en, tempo (3/2) (line [g 4 en, g 4 en, g 4 sn, fs 4 sn])]
>   
>    m061_064 = line [m061, m062, m063, m064]
>    g061_064 = line [g061, g062, g063, g064]
>   
>    m061 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>               , percRCen, chord [percRCen, percBDen], chord [percRCen, percLTen], percRCen
>               , percCCen, chord [percCCen, percBDen]]
>    m062 = line [percCCsn, chord [percm CrashCymbal1 [sn, en, sn, sn], percm BassDrum1 [sn, en, sn, sn]]
>               , percBDen, percm LowTom [den, sn, en, sn, sn]]
>    m063 = line [chord [percCCen, percBDen], chord [percRCen, percBDen], percRCen
>               , chord [percRCsn, percBDsn], percBDsn, chord [perc SplashCymbal en, percLTen]
>               , percm SplashCymbal [en, en], chord [perc SplashCymbal en, percBDen]]
>    m064 = line [chord [perc SplashCymbal en, percLTen], perc SplashCymbal en
>               , tempo (3/2) (line [perc SplashCymbal en, percLTen, chord [perc SplashCymbal en, percBDen]])
>               , tempo (3/2) (line [chord [perc SplashCymbal en, percBDen], percLTen
>                                  , chord [perc SplashCymbal en, percBDen]])
>               , tempo (3/2) (line [chord [perc SplashCymbal en, percLTen]
>                            , times 2 (chord [perc SplashCymbal en, percLTen, percBDen])])]
>   
>    g061 = line [addDur tn [e 4, fs 4, e 4, d 4], tempo (3/2) (line [d 4 en, d 4 en, d 4 en]), e 4 en
>               , tempo (5/4) (line [c 4 en, tempo (3/4) (line [d 4 en, d 4 sn, c 4 en, c 4 sn])])]
>    g062 = line [tempo (3/2) (line [b 3 sn, c 4 sn, b 3 sn, a 3 sn, e 3 qn, e 3 sn]), e 3 (en + hn)]
>    g063 = line [rest en, a 5 en, tempo (3/2) (line [a 5 qn, a 5 sn, b 4 sn])
>               , addDur sn [d 6, e 5, e 5, e 5], chord [c 6 tn, e 6 tn], e 5 tn, c 6 tn, e 5 tn
>               , tempo (3/2) (line [e 5 sn, e 5 sn, e 5 sn])]
>    g064 = line [chord [b 4 sn, e 5 sn], chord [b 4 sn, e 5 sn], grace (-1) (c 6 en)
>               , tempo (3/2) (line [a 5 en, a 5 en, a 5 en, c 6 qn, a 5 en])
>               , tempo (3/2) (line [a 5 en, a 5 en, tempo (3/2) (line [b 4 sn, chord [b 4 sn, e 5 sn], e 5 sn])])]
>   
>    m065_068 = line [m065, m066, m067, m068]
>    g065_068 = line [g065, g066, g067, g068]
>   
>    m065 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], percCCen, chord [percRCen, percBDen]
>               , chord [percRCen, percLTen], percRCsn, percLTsn
>               , chord [percRCsn, percLTsn], percLTsn, percBDsn, chord [percRCsn, percLTsn]]
>    m066 = line [times 3 (line [percm BassDrum1 [sn, sn, sn], chord [percRCsn, percLTsn]])
>               , times 2 (line [chord [percRCsn, percHTsn, percBDsn], percHTsn])]
>    m067 = line [chord [percCCen, percHTen, percBDen], percRCen, percRCen, chord [percRCen, percBDen]
>               , percLTen, percm RideCymbal1 [en, en, en]]
>    m068 = line [times 3 (chord [percHTsn, percLTsn]), chord [percRCsn, percLTsn, percBDsn]
>               , percRCsn, chord [percHTsn, percLTsn], chord [perc RideCymbal1 tn, perc HighTom tn]
>               , perc LowTom tn, perc HighTom tn, perc LowTom tn
>               , chord [percRCsn, percLTsn], percBDsn, chord [percRCsn, percLTsn], percLTsn
>               , chord [percRCsn, percHTsn], percBDsn, chord [percRCsn, percHTsn], chord [percRCsn, percBDsn]]
>   
>    g065 =
>      line [
>          tempo (3/2) (line [e 5 en, e 5 sn, a 5 sn, e 5 sn, e 5 sn, e 5 en, e 5 sn, e 5 sn
>                   , b 4 sn, e 5 den, b 5 sn]), b 5 en
>        , tempo (3/2) (line [a 5 en, tempo (3/4) (line [c 6 sn, b 5 sn, a 5 sn])])]
>    g066 = line [grace (-4) (d 6 qn), d 6 sn, c 6 en, e 5 tn, b 4 tn, e 5 sn, e 5 tn, e 5 tn
>               , chord [e 5 en, a 5 en], tempo (3/2) (line [g 5 en, g 5 sn]), g 5 sn, fs 5 sn]
>    g067 = line [tempo (3/2) (line [fs 5 sn, grace (-2) (a 5 en)]), a 5 sn, a 5 sn
>               , tempo (3/2) (line [a 5 sn, fs 5 sn, e 5 sn]), e 5 sn, fs 5 sn, g 5 en, fs 5 en
>               , addDur sn [e 5, b 4, e 5, a 5]]
>    g068 = line [tempo (3/2) (line [addDur sn [a 5, g 5, fs 5, e 5, e 5, fs 5], g 5 en, fs 5 qn])
>               , e 5 en, c 6 qn, a 6 en]
>
>    m069_072 = line [m069, m070, m071, m072]
>    g069_072 = line [g069, g070, g071, g072]
>   
>    m069 = line [chord [percCCen, percBDen], percBDen, percCCen, chord [percCCsn, percBDsn], percBDsn
>               , chord [perc CrashCymbal1 qn, perc LowTom qn], percCCen, percCCen]
>    m070 = line [chord [percCCen, percBDen], tempo (3/2) (line [percHTsn, percLTsn, percHTsn])
>               , perc SplashCymbal tn, percm LowTom [tn, tn, tn], percCCen
>               , percm CrashCymbal1 [en, sn, sn], percm SplashCymbal [en, sn]
>               , perc HighTom tn, perc LowTom tn]
>    m071 = line [chord [percRCen, percHTen, percBDen], percRCen, percRCen, chord [percRCen, percBDen]
>               , tempo (3/2) (line [percLTen, chord [percRCen, percBDen], chord [percRCen, percBDen]])
>               , tempo (3/2) (line [percHTen, percCCen, percHTsn, percHTsn])]
>    m072 =
>     line [
>         chord [line [percRCen, percRCsn, percRCsn], line [percBDen, percBDsn, percBDsn]]
>       , tempo (3/2)
>           (line [chord [percRCen, percHTen], chord [percRCen, percBDen]
>                , tempo (3/2) (line [chord [percRCsn, percBDsn], percLTsn, percLTsn])])
>       , chord [percRCsn, percLTsn], percLTsn, percLTsn
>       , tempo 3 (line [chord [percLTsn, percBDsn], percHTsn, percHTsn])
>       , chord [percRCsn, percHTsn, percBDsn], percBDsn, percm HighTom [tn, tn, tn]
>       , chord [perc RideCymbal1 tn, perc BassDrum1 tn]]
>   
>    g069 = line [a 6 tn, fs 6 tn, e 6 sn, e 6 tn, fs 6 tn, g 6 en, fs 6 en, e 6 sn
>               , tempo (3/2) (line [e 6 en, e 6 en, e 6 en])
>               , grace (-1) (e 6 sn), a 3 sn, e 6 sn, fs 6 sn]
>    g070 = line [e 6 en, e 6 en, tempo (3/2) (line [e 6 sn, fs 6 sn, e 6 sn]), descent sunPygLead (D, 6) dqn
>               , tempo (7/4) (line [a 5 sn, g 5 sn, fs 5 sn, e 5 en, e 5 sn, fs 5 sn])]
>    g071 = line [g 5 sn, fs 5 en, fs 5 sn, g 5 sn, fs 5 sn, grace (-3) (a 5 den), g 5 en, g 5 sn
>               , b 5 en, tempo (3/2) (line [chord [b 4 sn, b 5 sn], a 5 sn, g 5 sn])]
>    g072 = line [grace (-5) (a 5 sn), b 5 sn, a 5 en
>               , tempo (3/2) (line [a 5 en, a 5 en, a 5 sn, b 5 sn])
>               , addDur tn [c 6, b 5, a 5, b 5], tempo (3/2) (line [d 6 en, d 6 sn])
>               , tempo (3/2) (line [b 5 en, a 5 sn])
>               , tempo (5/4) (line [b 5 sn, c 6 sn, b 5 sn, a 5 sn, b 5 sn])
>               , tempo (3/2) (line [grace (-2) (a 5 en), a 5 sn])
>               , tempo (3/2) (line [a 5 sn, g 5 sn, e 5 sn, e 5 tn, g 6 tn, e 5 sn, d 5 sn])
>               , chord [b 4 sn, a 5 sn], e 5 sn, d 5 sn, c 5 sn, chord [d 5 tn, fs 5 tn]
>               , c 5 tn, a 4 sn, tempo (3/2) (line [c 5 en, c 5 sn])]
>   
>    m073_076 = line [m073, m074, m075, m076]
>    g073_076 = line [      g074, g075, g076]
>   
>    m073 = line [perc RideCymbal1 qn, chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>               , percRCen, chord [percRCen, percBDen]]
>    m074 = line [tempo (3/2) (line [chord [percRCen, percBDen], percLTen, percLTen])
>               , tempo (3/2) (line [chord [percRCen, percBDen], chord [percRCen, percLTen]
>               , chord [percRCen, percLTen]]), perc BassDrum1 hn]
>    m075 = line [chord [perc RideCymbal1 qn, perc BassDrum1 qn], percRCen
>               , chord [percRCen, percBDen], chord [percRCen, percLTen], percRCen
>               , chord [percRCen, percBDen], chord [percRCsn, percBDsn], percm LowTom [tn, tn]]
>    m076 = line [tempo (3/2) (line [percLTen, times 2 (chord [percRCen, percLTen, percBDen])])
>               , tempo (3/2) (line [percLTsn, percLTsn, percLTen, chord [percRCen, percBDen]])
>               , tempo (3/2) (line [chord [percRCen, percBDen], percm LowTom [sn, sn, en]])
>               , tempo (3/2) (line [chord [percRCen, percLTen, percBDen]
>                                  , chord [percRCen, percLTen], percLTsn, percLTsn])]
>   
>    g074 = line [a 4 (dhn + en), e 5 en]
>    g075 = line [e 5 sn, d 5 sn, c 5 en, c 5 en, d 5 en, e 5 en
>               , tempo (3/2) (line [e 5 sn, d 5 sn, c 5 sn]), d 5 en, b 6 en]
>    g076 = line [tempo (3/2) (line [d 5 en, d 5 en, g 5 sn, fs 5 sn]), e 5 sn, d 5 sn, g 5 en
>               , fs 5 en, tempo (3/2) (line [fs 5 sn, e 5 sn, d 5 sn]), d 5 en, d 5 en]
>   
>    m077_080 = line [m077, m078, m079, m080]
>    g077_080 = line [g077, g078, g079, g080]
>   
>    m077 =
>      line [
>        tempo (3/2)
>          (line [percLTen, chord [percRCen, percLTen, percBDen], chord [percRCen, percLTen]
>               , chord [perc RideCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>               , chord [percRCen, percBDen]])
>        , chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn], percCCen
>        , chord [percCCen, percBDen]]
>    m078 = line [chord [percCCen, percBDen], percCCen, percCCen, chord [percCCen, percBDen]
>               , chord [percCCen, percLTen], chord [percCCen, percBDen], perc RideCymbal1 qn]
>    m079 = line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [perc RideCymbal1 qn, perc LowTom qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen]]
>    m080 = line [chord [perc RideCymbal1 qn, perc LowTom qn]
>               , tempo (3/2) (line [chord [percRCen, percBDen], percLTen, percLTen
>                                  , times 3 (line [chord [perc SplashCymbal en, percLTen, percBDen], percLTen])])]
>   
>    g077 = tempo (3/2) (line [d 5 qn, e 5 en, d 5 en, c 5 qn, d 5 en, c 5 en, a 4 dqn, a 4 en])
>    g078 = line [grace (-1) (g 4 en), fs 4 (en + hn), descent sunPygLead (D, 6) qn]
>    g079 = line [rest en, fs 5 en, tempo (3/2) (line [addDur en [c 5, c 5, a 4, c 5, a 4, c 5, a 5, g 5, g 5]])]
>    g080 = line [tempo (3/2) (line [addDur en [d 5, c 5, a 4, g 4, g 4, g 4, a 4, g 4, a 4, c 5], a 4 qn])]
>   
>    m081_084 = line [m081, m082, m083, m084]
>    g081_084 = line [g081, g082, g083, g084]
>   
>    m081 =
>      t32 [percLTen, chord [perc SplashCymbal en, percBDen], percLTen, chord [perc SplashCymbal en, percBDen]
>         , chord [perc SplashCymbal en, percBDen], chord [perc SplashCymbal en, percLTen, percBDen]
>         , chord [perc SplashCymbal en, percBDen], chord [perc SplashCymbal en, percLTen, percBDen]
>         , chord [perc SplashCymbal en, percBDen], chord [perc SplashCymbal en, percBDen], percLTen
>         , chord [perc SplashCymbal en, percBDen]]
>    m082 =
>      t32 [times 4 (line [percLTen, chord [perc SplashCymbal en, percBDen]]), percLTen, percm BassDrum1 [en, en, en]]
>    m083 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [perc RideCymbal1 qn, perc LowTom qn], chord[percRCen, percBDen], chord [percRCen, percLTen]]
>    m084 = line [percLTen, chord [percRCen, percBDen], chord [perc RideCymbal1 den, perc BassDrum1 den], percBDsn
>               , chord [perc RideCymbal1 qn, perc LowTom qn], chord [perc RideCymbal1 qn, perc LowTom qn]]
>   
>    g081 = t32 [addDur en [d 5, c 5, a 4, g 4, g 4, g 4, a 4, g 4, a 4, c 5], a 4 qn]
>    g082 = t32 [a 4 qn, d 4 en, d 4 en, c 6 en, a 5 qn, a 5 en, g 5 en, e 5 en, a 5 en, g 5 en]
>    g083 = t32 [g 5 en, e 5 en, d 5 en, c 5 en, e 5 en, d 5 qn, c 5 en, a 4 en, g 4 en, c 5 en, c 5 en]
>    g084 = line [t32 [a 4 sn, g 4 sn, e 4 sn], g 4 qn, t32 [g 4 sn, e 4 en], e 4 den, fs 4 sn
>               , t32 [e 4 sn, d 4 sn, c 4 sn], d 4 sn, c 4 sn]
>   
>    m085_088 = line [m085, m086, m087, m088]
>    g085_088 = line [g085, g086, g087, g088]
>
>    m085 = line [chord [perc RideCymbal1 qn, perc BassDrum1 qn], percRCen, chord [percRCen, percBDen]
>               , chord [percRCen, percLTen], percRCen, chord [percRCen, percBDen], chord [percRCen, percBDen]]
>    m086 = line [chord [percRCen, percLTen], chord [percRCen, percBDen], chord [percRCsn, percLTsn]
>               , percBDsn, percRCsn, percBDsn, percLTen, chord [percRCsn, percBDsn], percBDsn
>               , percLTsn, percLTsn, chord [percRCsn, percLTsn], percLTsn]
>    m087 = line [chord [percRCen, percBDen], percLTsn, percLTsn, rest sn, percLTsn
>               , chord [percRCsn, percBDsn], percBDsn
>               , times 3 (chord [percRCen, percBDen]), chord [percRCsn, percLTsn], percBDsn]
>    m088 = line [perc RideCymbal1 den, chord [percRCsn, percBDsn], percRCen, chord [percCCen, percBDen]
>               , percLTsn, times 2 (chord [perc RideCymbal1 den, perc BassDrum1 den])
>               , chord [perc CrashCymbal1 sn, perc BassDrum1 sn]]
>   
>    g085 = line [a 3 sn, g 3 sn, g 3 en, a 3 en, g 3 en, a 3 en, a 3 en, g 3 sn, a 3 den]
>    g086 = line [a 3 hn, a 3 en, rest en, t32 [rest sn, fs 6 sn, grace (-2) (a 5 en), b 5 en]]
>    g087 = t32 [addDur en [a 5, g 5, e 5, d 5, e 5, d 5, c 5, a 4, g 4, a 4, g 4, e 4]]
>    g088 = line [t32 [d 4 en, c 4 en, d 4 en, c 4 en, a 3 en, g 3 en, g 3 en, g 3 qn], a 3 en
>               , t32 [grace (-2) (a 3 sn), g 3 sn, e 3 sn]]
>   
>    m089_092 = line [m089, m090, m091, m092]
>    g089_092 = line [g089, g090, g091, g092]
>   
>    m089 = line [chord [percRCen, percBDen], percRCen, percRCen, chord [percRCen, percBDen]
>               , chord [percRCen, percLTen], percRCen, chord[percRCen, percBDen], percLTen]
>    m090 = line [chord [percCCen, percBDen], percRCen, t32 [percLTsn, percLTsn, percLTsn]
>               , chord [percCCsn, percBDsn], percLTsn, chord [percCCen, percLTen]
>               , t32 [percLTsn, percLTsn, percBDsn, percm LowTom [sn, sn, sn, sn, sn, sn]]]
>    m091 = line [chord [percRCen, percBDen], percRCen, percRCen, chord [percRCen, percBDen]
>               , chord [percCCen, percBDen], percRCen, chord [percRCen, percBDen]
>               , chord [percRCen, percBDen]]
>    m092 = line [chord [percRCen, percLTen], percBDen, percBDen, chord [percRCen, percLTen]
>               , percLTsn, percLTsn, chord [percLTsn, percBDsn], percLTsn, chord [percLTsn, percBDsn]
>               , percLTsn, chord [percLTsn, percBDsn], percLTsn]
>   
>    g089 = line [tempo (7/4) (line [g 3 sn, fs 3 en, fs 3 en, t32 [g 3 sn, fs 3 sn, e 3 sn]]), e 3 en
>               , e 3 en, e 3 en, grace (-1) (c 4 en), b 3 qn]
>    g090 = line [b 3 dqn, t32 [b 3 sn, c 4 sn, b 3 sn], a 3 en, a 3 en, c 4 sn, d 4 den]
>    g091 = line [b 3 (dhn + en), t32 [b 3 sn, c 4 sn, b 3 sn]]
>    g092 = line [a 3 en, a 3 qn, grace (-2) (d 4 en), d 4 sn, c 4 en, c 4 sn
>               , t32 [c 4 sn, d 4 sn, c 4 sn], a 3 en]
>   
>    m093_096 = line [m093, m094, m095, m096]
>    g093_096 = line [g093, g094, g095, g096]
>   
>    m093 = line [chord [percCCen, percBDen], chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , percRCen
>               , tempo (7/4) (line [percm LowTom [en, en, en, en]
>                                  , chord [percm RideCymbal1 [qn, en], percm BassDrum1 [qn, en]]])]
>    m094 =
>      line [
>        percLTen, chord [perc SplashCymbal sn, percBDsn], chord [perc SplashCymbal sn, percBDsn]
>        , tempo (4/3)
>            (line [perc SplashCymbal den, percBDsn, chord [perc SplashCymbal den, perc LowTom den]
>                 , percBDsn, chord [perc SplashCymbal den, perc LowTom den], percBDsn
>                 , chord [perc SplashCymbal den, perc LowTom den], percBDsn])]
>    m095 = line [chord [percCCen, percLTen, percBDen], percRCen, chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [percCCen, percLTen], percRCen, rest en
>               , chord [percCCen, percBDen]]
>    m096 = line [t32 [rest en, chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]]
>               , chord [perc RideCymbal1 den, perc LowTom den], chord [percRCsn, percBDsn]
>               , rest en, chord [percRCen, percBDen], rest en, percLTsn, percLTsn]
>   
>    g093 = line [t32 [a 3 sn, b 3 sn, c 4 sn], a 3 en, a 3 en, grace (-3) (d 4 hn)
>               , t32 [grace (-2) (e 4 sn), d 4 sn, b 3 sn]]
>    g094 = line [b 3 dqn, t32 [b 3 sn, grace (-1) (c 4 sn), b 3 sn, a 3 en, a 3 sn], a 3 dqn]
>    g095 = line [a 3 dqn, b 4 en, d 5 dqn, e 5 en]
>    g096 = line [d 6 (qn + sn), a 4 den, t32 [b 4 en, b 4 en, c 5 sn, grace 1 (b 4 sn)], b 4 qn]
>   
>    m097_100 = line [m097,       m099, m100]
>    g097_100 = line [g097, g098, g099, g100]
>   
>    m097 =
>      line [
>        tempo (6/5)
>              (line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn], perc RideCymbal1 qn
>             , chord [perc RideCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>             , chord [perc RideCymbal1 qn, perc LowTom qn]
>             , perc LowTom qn, rest qn])
>         , chord [percRCsn, percLTsn, percBDsn], percLTsn, percLTen, rest en
>         , chord [percRCsn, percLTsn, percBDsn], chord [percRCsn, percLTsn]
>         , chord [perc RideCymbal1 qn, perc LowTom qn]  
>           ]
>    m099 = line [t32 [chord [percRCen, percLTen, percBDen], chord [perc RideCymbal1 qn, perc LowTom qn]]
>               , percRCen, chord [percRCen, percBDen], chord [perc RideCymbal1 qn, perc LowTom qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]]
>    m100 =
>      t32 [chord [perc RideCymbal1 qn, perc BassDrum1 qn], chord [perc RideCymbal1 qn, perc LowTom qn]
>         , chord [perc RideCymbal1 qn, perc LowTom qn], chord [perc RideCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>         , chord [perc RideCymbal1 qn, perc BassDrum1 qn], chord [perc RideCymbal1 qn, perc BassDrum1 qn]]
>   
>    g097 = line [b 4 dqn, a 4 qn, e 6 en, rest sn, c 6 den]
>    g098 = line [descent sunPygLead (C, 6) en, d 4 sn, d 4 sn, g 4 sn, g 4 sn, g 4 en, g 4 en, a 4 en, a 4 en
>               , t32 [b 4 en, c 5 sn]]
>    g099 = line [tempo (5/4) (line [addDur en [b 4, a 4, b 4, d 5, a 4]]), a 4 sn, b 4 den
>               , grace (-1) (c 5 en), b 4 en]
>    g100 =
>      line [
>          tempo (5/4) (line [a 4 en, a 4 en, a 5 en, e 5 sn, fs 5 sn, e 5 sn, d 5 sn])
>        , d 5 qn, descent sunPygLead (D, 6) qn]
>   
>    m101_104 = line [m101, m102, m103, m104]
>    g101_104 = line [g101, g102, g103, g104]
>   
>    m101 = line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn], rest en
>               , percBDsn, percBDsn, chord [perc ClosedHiHat qn, perc LowTom qn, perc BassDrum1 qn]
>               , chord [percCHHen, percBDen], chord [percCHHen, percBDen]]
>    m102 = line [percCHHen, chord [percCHHen, percBDen], chord [percCHHen, percBDen], percLTsn
>               , chord [percLTsn, percBDsn], chord [percCHHsn, percLTsn], chord [percLTsn, percBDsn]
>               , percLTsn, percLTsn, chord [percCHHsn, percLTsn, percBDsn]
>               , chord [perc LowTom den, perc BassDrum1 den]]
>    m103 = line [chord [percCHHen, percLTen, percBDen], chord [percCHHsn, percBDsn], percLTsn
>               , percLTsn, chord [percCHHsn, percBDsn], percLTen
>               , chord [percCHHsn, percHTsn, percBDsn], percLTsn, percLTsn, percLTsn
>               , chord [percOHHen, percLTen, percBDen], chord [percOHHsn, percLTsn, percBDsn], percLTsn]
>    m104 =
>      line [
>        percCHHsn, percBDsn, chord [percCCsn, percLTsn], percLTsn, percLTsn
>      , chord [perc CrashCymbal1 en, perc LowTom en, perc BassDrum1 en], percLTsn
>      , percLTsn, percLTsn, percHTsn, percLTsn, percLTsn, percHTsn, percLTsn, percLTsn]
>   
>    g101 = line [d 5 en, grace (-4) (b 4 en), d 5 en
>               , t32 [chord [b 4 sn, d 6 sn], e 6 sn, d 6 (sn + qn), d 6 en], d 6 en, d 6 en]
>    g102 = line [t32 [d 6 en, d 6 qn, d 6 en, b 5 sn], d 6 qn, d 6 en, b 5 sn, d 6 en, d 6 sn]
>    g103 = line [d 6 en, b 5 sn, d 6 sn, d 6 sn, b 5 sn, d 6 en, d 6 en, d 6 en, d 6 sn
>               , d 6 sn, addDur tn [d 6, e 6, d 6, b 6]]
>    g104 =
>      line [
>          t32 [d 6 en, d 6 sn], e 6 sn, g 6 sn, t32 [c 6 tn, e 6 tn, fs 6 tn]
>         , chord [fs 6 sn, a 6 sn], t32 [e 6 en, grace (-2) (fs 6 den), d 6 sn], d 6 en
>         , d 6 tn, b 5 tn, a 5 sn, e 6 tn, b 5 tn, a 5 sn]
>   
>    m105_108 = line [m105, m106, m107, m108]
>    g105_108 = line [g105, g106, g107, g108]
>   
>    m105 = line [chord [percCCen, percBDen], percRCsn, percBDsn, chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [percCCen, percLTen], percCCen
>               , chord [percCCen, percBDen], chord [percCCen, percBDen]]
>    m106 = line [t32 [percm LowTom [sn, sn, en, en]], times 3 (chord [percCCen, percBDen])
>               , chord [percCCen, percLTen, percBDen]
>               , tempo (5/4) (line [percm LowTom [sn, sn, sn, sn, sn]])]
>    m107 = line [t32 [times 3 (chord [percCCen, percBDen])]
>               , tempo (5/4) (line [percm LowTom [sn, sn, sn, sn, sn]]), chord [percCCen, percBDen]
>               , chord [percCCen, percBDen], chord [perc CrashCymbal1 qn, perc BassDrum1 qn]]
>    m108 = line [tempo (5/4) (line [percm LowTom [sn, sn, sn, sn, sn]])
>               , t32 [times 3 (chord [percCCen, percBDen])], chord [percCCen, percLTen]
>               , percLTsn, percBDsn, percLTsn, percLTsn, t32 [percLTsn, percLTsn, percLTsn]]
>   
>    g105 = line [a 5 qn, a 5 en, t32 [grace (-2) (a 5 en), a 5 sn], d 6 sn, c 6 sn, a 5 en
>               , addDur sn [d 6, c 6, d 6, c 6]]
>    g106 = line [c 6 dqn, chord [e 5 en, e 6 en], t32 [c 6 en, a 5 qn], a 5 qn]
>    g107 = line [a 5 dqn, fs 5 sn, g 5 sn, a 5 sn, a 5 sn
>               , t32 [addDur tn [e 6, c 6, d 6, c 6, b 5, c 6]]
>               , a 5 sn, a 5 sn, a 5 sn, g 5 sn]
>    g108 = line [fs 5 en, fs 5 sn, e 5 sn, t32 [e 5 en, e 5 en, fs 5 en], fs 5 en, d 6 en
>               , chord [e 5 en, d 5 en], fs 5 sn, g 5 sn]
>   
>    m109_112 = line [m109, m110, m111, m112]
>    g109_112 = line [g109, g110, g111, g112]
>   
>    m109 = line [t32 [times 3 (chord [percCCen, percBDen])], times 2 (chord [percCCen, percBDen])
>               , tempo (5/4) (line [percm LowTom [sn, sn, sn, sn, sn]])
>               , t32 [times 3 (chord [percOHHen, percBDen])]]
>    m110 = line [chord [percCHHen, percBDen], chord [percCCsn, percBDsn], chord [percCCsn, percBDsn]
>               , t32 [perc LowTom qn, chord [percCCen, percBDen]], chord [percLTsn, percBDsn]
>               , percBDsn, percBDen, chord [percCCen, percBDen], percBDsn, percLTsn]
>    m111 = line [perc LowTom qn, chord [percCCen, percBDen], chord [percCCsn, percBDsn]
>               , chord [percCCsn, percBDsn], chord [perc CrashCymbal1 den, perc BassDrum1 den]
>               , chord [percCCsn, percBDsn], perc LowTom qn]
>    m112 = line [chord [percm CrashCymbal1 [sn, sn, en, sn, den], percm BassDrum1 [sn, sn, en, sn, den]]
>               , t32 [times 3 (chord [percCCen, percBDen])], chord [perc CrashCymbal1 qn, perc BassDrum1 qn]]
>   
>    g109 = line [t32 [e 5 en, e 5 sn, e 5 sn, g 5 sn, e 5 sn], c 4 den, b 6 sn, b 4 en
>               , t32 [c 5 sn, b 4 sn, a 4 sn], a 4 en, a 4 en]
>    g110 = line [d 4 en, a 4 sn, a 4 sn, c 5 sn, e 5 sn, t32 [c 6 en, a 5 sn, a 5 qn, a 5 en]
>               , c 6 sn, e 6 sn, t32 [c 6 en, a 5 sn]]
>    g111 = line [a 5 en, a 5 en, a 5 sn, a 5 sn, c 6 sn, e 6 sn, t32 [c 6 en, a 5 sn, a 5 en, a 5 sn]
>               , c 6 sn, e 6 sn, t32 [c 6 en, a 5 sn]]
>    g112 = line [a 5 en, a 5 en, a 5 sn, a 5 sn, c 6 sn, e 6 sn, c 6 sn, a 5 sn
>               , t32 [a 5 en, a 5 sn], c 6 sn, e 6 sn, c 6 en]
>   
>    m113_116 = line [m113, m114, m115, m116]
>    g113_116 = line [g113, g114, g115, g116]
>   
>    m113 =
>      line [
>          rest en
>        , t32 [chord [percCCen, percBDen], rest en, chord [percCCen, percBDen]]
>        , chord [percCCen, percBDen]
>        , t32 [
>            t32 [times 3 (chord [percCCen, percBDen])]
>          , tempo (5/4) (line [percLTsn, percLTsn, percLTsn, percLTsn, perc LowTom tn, perc LowTom tn])
>          , tempo (7/8) (line [chord [percCCsn, percBDsn]
>                               , chord [percCCsn, percBDsn]
>                               , percm LowTom [tn, tn, tn]])]]      
>    m114 =
>      line [
>          percLTsn, chord [percCCsn, percBDsn], percLTen, chord [percCCsn, percBDsn], percBDsn
>        , t32 [percm LowTom [sn, sn, sn, sn, sn, sn]
>             , chord [percCCsn, percLTsn, percBDsn], percLTsn, percLTsn]
>        , perc LowTom den, chord [percCCsn, percLTsn]]
>    m115 = line [chord [percCCsn, percLTsn, percBDsn], percm LowTom [sn, tn, tn, tn, tn]
>               , chord [percCCsn, percBDsn], chord [perc CrashCymbal1 den, perc BassDrum1 den]
>               , chord [percCCsn, percBDsn], chord [percCCsn, percBDsn], percLTen
>               , chord [percCCsn, percBDsn], chord [percCCsn, percBDsn], percLTen]
>    m116 = line [percLTen, chord [percCCsn, percBDsn], chord [percCCsn, percBDsn], perc LowTom den
>               , chord [percCCsn, percBDsn], chord [perc CrashCymbal1 den, perc BassDrum1 den]
>               , chord [percCCsn, percBDsn], chord [perc CrashCymbal1 qn, perc BassDrum1 qn]]
>   
>    g113 = line [c 6 sn, a 5 sn, a 5 sn, a 5 sn, c 6 sn, a 5 sn, grace (-2) (a 5 sn), a 5 sn
>               , a 5 sn, c 6 sn, a 5 sn, c 6 sn
>               , tempo (7/4) (line [e 6 sn, c 6 en, a 5 en, a 5 en])]
>    g114 = line [t32 [fs 6 qn, d 6 en], e 6 en, chord [e 5 en, g 6 en]
>               , t32 [grace (-2) (fs 6 sn), e 6 sn, c 6 sn]
>               , tempo (5/4) (line [fs 6 sn, fs 6 tn, e 6 tn, c 6 tn])
>               , t32 [d 6 sn, b 4 sn, d 6 sn], c 6 sn, grace (-4) (fs 6 sn)]
>    g115 = line [g 4 sn, d 6 sn, c 6 sn, d 6 sn, t32 [c 6 qn, a 5 en, chord [d 4 sn, a 5 sn], g 4 sn, g 5 sn]
>               , a 5 en, a 5 sn, g 5 en, g 5 sn]
>    g116 = line [fs 5 sn, g 5 sn, fs 5 sn, e 5 den, e 5 sn, fs 5 sn, e 5 dqn, fs 5 sn, e 5 sn]
>   
>    m117_120 = line [m117, m118, m119, m120]
>    g117_120 = line [g117, g118, g119, g120]
>   
>    m117 = line [chord [percCCsn, percBDsn], chord [percCCsn, percBDsn], perc LowTom (en + den)
>               , chord [percCCsn, percBDsn]
>               , t32 [chord [percCCsn, percLTsn, percBDsn]
>                    , percm LowTom [sn, sn, sn, sn, sn, sn, sn, sn, sn, sn, sn]]]
>    m118 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>               , chord [percCCsn, percBDsn], chord [percCCsn, percBDsn]
>               , chord [perc CrashCymbal1 qn, perc LowTom qn], chord [percCCen, percBDen]
>               , chord [percCCen, percLTen, percBDen]]
>    m119 = line [chord [percCCen, percLTen], chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , t32 [chord [percCCsn, percLTsn], percLTsn, percLTsn]
>               , tempo (7/4) (line [percm LowTom [en, en, en, en, en, en, en]])]
>    m120 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [perc CrashCymbal1 qn, perc LowTom qn]
>               , chord [percCCen, percLTen, percBDen], chord [percCCen, percLTen, percBDen]]
>   
>    g117 = line [e 5 qn, tempo (5/4) (line [fs 5 en, fs 5 sn, e 5 sn, rest sn])
>               , f 5 sn, f 5 sn, e 5 sn, g 5 sn, f 5 sn, e 5 sn, chord [g 4 en, g 5 en]]
>    g118 = line [fs 5 en, fs 5 sn, fs 5 sn, e 5 en, grace (-2) (a 5 en)
>               , fs 5 sn, fs 5 sn, e 5 sn, grace (-2) (fs 5 sn), g 4 tn, g 5 tn, e 5 den]
>    g119 = line [e 5 sn, e 5 sn, d 5 sn, ascent sunPygLead (D, 5) (sn + dhn)]
>    g120 = line [fs 5 en, fs 5 sn, g 5 sn, a 5 en, a 5 en
>               , t32 [addDur sn [a 5, fs 5, g 5, fs 5, g 5, fs 5]], e 5 sn, b 5 sn
>               , t32 [grace (-2) (fs 5 sn), e 5 sn, d 5 sn]]
>   
>    m121_124 = line [m121, m122, m123, m124]
>    g121_124 = line [g121, g122, g123, g124]
>   
>    m121 = line [t32 [percLTen, percLTen, percLTen], percLTsn, percLTsn, percLTsn, percLTsn
>               , tempo (5/4) (line [chord [percCCen, percLTen]
>                                  , chord [percCCsn, percLTsn]
>                                  , chord [percCCen, percLTen]])
>               , t32 [chord [percCCen, percLTen], chord [percCCen, percLTen], chord [percCCen, percLTen]]]
>    m122 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [perc CrashCymbal1 qn, perc LowTom qn]
>               , chord [percCCen, percBDen], chord [percCCen, percBDen]]
>    m123 =
>      chord [
>        percm ClosedHiHat [qn, qn, qn, qn]
>        , tempo (5/4) (percm LowTom [en, en, en, en, en, en, en, en, en, en])]
>        
>    m124 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [perc CrashCymbal1 qn, perc LowTom qn]
>               , chord [percRCen, percBDen], chord [percRCsn, percBDsn], percBDsn]
>   
>    g121 = line [e 5 sn, e 5 tn, e 5 tn, t32 [d 5 sn, rest sn, d 5 sn], a 4 sn, d 5 sn, b 4 sn, a 4 sn
>               , t32 [chord [b 3 en, a 4 en], e 4 sn, a 4 sn, g 3 sn, a 3 sn, g 3 sn, e 3 sn, e 4 sn]
>               , e 4 en]
>    g122 = line [rest en, e 4 en, e 4 sn, d 4 sn, e 4 qn, chord [e 4 sn, g 4 sn], chord [d 4 sn, g 4 sn]
>               , chord [e 4 sn, a 4 sn], chord [d 4 sn, g 4 sn], chord [e 4 en, a 4 en]]
>    g123 = line [fs 4 en, chord [fs 3 qn, fs 4 qn], a 6 en, descent sunPygLead (A, 6) qn, rest en, e 4 en]
>    g124 = line [chord [a 3 qn, fs 4 qn, b 4 qn, fs 5 qn], t32 [fs 5 qn, fs 5 en]
>               , fs 5 sn, g 5 sn, a 5 qn, t32 [a 5 sn, g 5 sn, fs 5 sn]]
>   
>    m125_128 = line [m125, m126, m127, m128]
>    g125_128 = line [g125, g126, g127, g128]
>   
>    m125 = line [chord [percRCen, percLTen], chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [percRCsn, percLTsn], percBDsn, chord [percRCen, percBDen]
>               , chord [percRCsn, percBDsn], chord [percRCsn, percBDsn], chord [percRCen, percBDen]
>               , percLTsn, chord [percRCsn, percBDsn]]
>    m126 = line [percLTsn, chord [percRCen, percBDen], percLTsn
>               , t32 [chord [percRCen, percBDen], percLTen, t32 [percLTsn, percLTsn, percLTsn]]
>               , t32 [chord [percRCen, percBDen], chord [percRCen, percBDen], chord [percRCen, percBDen]]
>               , percCCsn, percLTsn, percLTsn, percLTsn]
>    m127 = line [percLTsn, percLTsn, percHTsn, percHTsn, chord [percCCen, percBDen]
>               , chord [percCCsn, percBDsn], percBDsn
>               , t32 [percm LowTom [sn, sn, sn, sn, sn, sn, sn, sn, sn, sn, sn, sn]]]
>    m128 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>               , chord [percCCsn, percBDsn], percBDsn, chord [percCCen, percLTen], percCCsn
>               , percBDsn, chord [percCCen, percLTen], percCCsn, percBDsn]
>   
>    g125 = line [g 5 en, t32 [g 5 sn, a 5 en], a 5 en, g 5 en, addDur tn [g 5, a 5, g 5, fs 5]
>               , t32 [g 5 en, fs 5 en, g 5 sn, fs 5 sn, e 5 sn, fs 5 sn, e 5 sn]]
>    g126 = line [fs 5 sn, e 5 sn, d 5 tn, e 5 tn, d 5 tn, e 5 tn, d 5 tn, b 4 dsn
>               , t32 [a 4 sn, b 4 sn, a 4 sn, b 5 en, d 6 sn], c 6 en, b 5 en, b 5 sn, f 5 sn]
>    g127 = line [b 5 sn, g 5 sn, fs 5 dqn, b 6 en, e 6 en, tempo (5/4) (line [e 6 den, b 5 en])]
>    g128 = line [e 6 en, t32 [e 6 sn, d 6 sn, b 5 sn], chord [g 4 sn, d 6 sn], d 6 sn
>               , t32 [d 6 sn, c 6 sn, b 6 sn]
>               , tempo (5/4) (line [e 6 sn, e 6 en, d 6 sn, b 5 sn, d 6 en, d 6 sn, c 6 sn, b 5 sn])]
>   
>    m129_132 = line [m129, m130, m131, m132]
>    g129_132 = line [g129, g130, g131, g132]
>   
>    m129 = line [chord [percCCsn, percLTsn], percBDsn, t32 [percHTsn, percHTsn, percHTsn]
>               , chord [perc ClosedHiHat qn,  perc LowTom qn,  perc BassDrum1 qn]
>               , chord [perc ClosedHiHat den, perc LowTom den, perc BassDrum1 den], percLTsn
>               , chord [perc ClosedHiHat sn,  perc LowTom sn,  perc BassDrum1 sn]
>               , perc ClosedHiHat den]
>    m130 = line [t32 [chord [percCCsn, percBDsn], percHTsn, percLTen, percLTsn, percLTsn]
>               , t32 [chord [percCHHen, percBDen], chord [perc CrashCymbal1 qn, perc BassDrum1 qn]]
>               , perc LowTom den, chord [percCCsn, percBDsn]
>               , chord [perc CrashCymbal1 den, perc BassDrum1 den], chord [percCCsn, percBDsn]]
>    m131 =
>      line [
>          perc LowTom qn
>        , tempo (5/4)
>            (line [chord [percm CrashCymbal1 [en, den, en, den]
>                        , percm BassDrum1    [en, den, en, den]]])
>        , perc LowTom qn]
>    m132 = line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn], percCCen
>               , chord [percCCsn, percBDsn], chord [percCCsn, percBDsn]
>               , chord [percCCen, percLTen], chord [percCCen, percBDen]
>               , chord [percCCen, percBDen], percCCsn, percBDsn]
>   
>    g129 = line [e 6 sn, d 6 sn, c 6 en, d 6 en, c 6 sn, b 5 sn
>               , t32 [a 5 en, chord [b 4 sn, fs 5 sn]], chord [b 4 sn, fs 5 sn, b 5 sn]
>               , chord [e 5 sn, b 5 sn], b 5 sn, a 5 sn, chord [g 4 sn, fs 5 sn], e 5 sn]
>    g130 = line [chord [g 4 qn, d 5 qn], e 5 qn, grace (-2) (fs 5 en), a 5 en, a 5 en, a 5 en]
>    g131 = line [t32 [a 5 en, a 5 sn, a 5 en, b 5 sn], a 5 hn, e 6 en, e 6 en]
>    g132 = line [e 6 en, t32 [e 6 sn, d 6 sn, b 5 sn, d 6 en, d 6 sn, grace 1 (b 5 en), a 5 sn]
>               , c 6 sn, b 5 den, t32 [b 5 en, a 5 sn, fs 5 en, fs 5 sn]]
>   
>    m133_136 = line [m133, m134, m135, m136]
>    g133_136 = line [g133, g134, g135, g136]
>   
>    m133 = line [perc LowTom qn, chord [percCCsn, percBDsn]
>               , chord [perc CrashCymbal1 den, perc BassDrum1 den], chord [percCCsn, percBDsn]
>               , chord [perc CrashCymbal1 den, perc BassDrum1 den], rest qn]
>    m134 = line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn], percCCen
>               , chord [percCCen, percBDen], chord [percCCen, percLTen], percCCen
>               , chord [percCCen, percLTen], chord [percCCen, percBDen]]
>    m135 = line [percBDen, percLTen, percLTen, perc BassDrum1 tn, perc LowTom tn
>               , perc BassDrum1 tn, perc LowTom tn, percBDsn, perc LowTom den
>               , perc BassDrum1 tn, perc LowTom tn, perc BassDrum1 tn, perc LowTom tn, percLTen]
>    m136 = line [perc CrashCymbal1 qn, percCCen, percBDsn, percBDsn
>               , chord [perc CrashCymbal1 qn, perc LowTom qn], percCCen, chord [percCCen, percBDen]]
>   
>    g133 = line [t32 [e 5 en, b 5 sn, e 5 sn, chord [b 4 en, d 4 en], b 5 en, e 5 sn]
>               , chord [g 4 en, b 4 en, d 5 en], d 5 en, a 4 en, chord [b 4 en, e 5 en, a 4 en]
>               , a 4 en]
>    g134 = line [t32 [b 4 qn, d 5 en, d 5 en, b 4 en, d 5 en]
>               , t32 [d 5 en, b 4 en, d 5 en, d 5 en, rest en, d 5 en]]
>    g135 = line [t32 [grace (-2) (fs 5 en), e 5 en, chord [a 3 en, d 5 en]], d 5 den, a 5 sn
>               , t32 [a 5 en, grace (-3) (d 5 en), f 5 en, f 5 en, d 5 en, f 5 en]]
>    g136 = line [t32 [grace (-2) (g 5 en), d 5 en, c 5 en, d 5 en, grace (-1) (f 5 en), grace (-1) (f 5 en)]
>               , t32 [d 5 sn, c 5 sn, a 4 sn, d 5 en, c 5 sn], c 5 sn, a 4 sn, g 4 en]
>   
>    m137_140 = line [m137, m138, m139, m140]
>    g137_140 = line [g137, g138, g139, g140]
>   
>    m137 = line [chord [percCCen, percLTen], percCCen, percCCen
>               , t32 [percLTsn, percLTsn, percBDsn, percBDsn, chord [percHTen, percLTen]]
>               , chord [percHTen, percLTen], perc LowTom qn]
>    m138 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], percCCen, percCCsn, percBDsn
>               , chord [percCCen, percLTen], percCCen, chord [percCCen, percBDen]
>               , chord [percCCen, percBDen]]
>    m139 = line [percCCen, chord [percCCen, percBDen], percLTsn
>               , chord [perc CrashCymbal1 den, perc BassDrum1 en], percBDsn, percBDsn, percLTsn
>               , percCCsn, rest sn, chord [percCCsn, percBDsn], percLTsn, chord [percLTsn, percBDsn]]
>    m140 = line [chord [percRCen, percBDen], chord [percRCen, percBDen], percRCen
>               , chord [percRCsn, percBDsn], percBDsn, chord [percRCen, percLTen]
>               , chord [percRCen, percBDen], percRCen, chord [percRCsn, percBDsn]
>               , chord [percRCsn, percBDsn]]
>   
>    g137 = line [t32 [f 4 en, d 4 en, c 4 en], c 4 den, rest sn
>               , t32 [grace (-2) (d 5 qn), f 5 en, f 5 en, d 5 en, f 5 en]]
>    g138 = line [t32 [g 5 qn, fs 5 en], grace (-3) (g 5 sn), t32 [g 5 sn, fs 5 sn, e 5 sn]
>               , d 5 sn, t32 [c 5 en, d 5 en, f 5 en, f 5 en, d 5 en, c 5 sn, a 4 sn]]
>    g139 = line [t32 [c 5 sn, chord [a 4 en, d 5 en]], chord [a 4 sn, d 5 sn], c 6 sn
>               , t32 [fs 5 en, d 5 sn], d 5 (en + den), e 6 sn
>               , grace (-1) (g 6 en), descent sunPygLead (G, 6) en]
>    g140 = line [t32 [c 6 sn, d 6 sn, c 6 sn], a 5 sn, f 5 sn
>               , t32 [grace 5 (g 5 en), chord [f 5 sn, d 6 sn]], d 5 sn, c 5 sn, f 5 den
>               , chord [a 5 (sn + qn), c 6 (sn + qn)]]
>   
>    m141_144 = line [m141, m142, m143, m144]
>    g141_144 = line [g141, g142, g143, g144]
>   
>    m141 = line [percRCen, times 3 (line [chord [percRCsn, percLTsn], percBDsn])
>               , t32 [times 3 (line [chord [percRCen, percLTen], percBDen])]]
>    m142 =
>      t32 [
>        chord [perc RideCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percLTen], percBDen
>      , chord [perc RideCymbal1 qn, perc LowTom qn]
>      , chord [perc RideCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen], percLTen
>      , chord [perc RideCymbal1 qn, perc HighTom qn]]
>    m143 =
>      line [
>          t32 [times 2 (line [chord [percRCen, percBDen], percLTen])
>        , chord [percRCen, percBDen], percBDen]
>        , tempo (7/4) (line [percLTen, percLTen, percBDen, percLTen, percBDen, percLTen, percBDen])]
>    m144 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn], percBDen
>        , chord [perc CrashCymbal1 qn, perc LowTom qn], chord [percRCen, percBDen]
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [perc CrashCymbal1 qn, perc LowTom qn], chord [percCCen, percBDen]]
>   
>    g141 = line [rest en, b 6 en, g 6 qn, f 6 en, a 5 tn, g 5 tn, f 5 en, g 5 sn, f 5 sn, b 4 sn]
>    g142 =
>      line [
>          tempo (5/4) (
>            line [addDur sn [c 5, a 4, f 5, c 5,d 5], rest sn, d 5 sn, c 5 sn, a 4 sn, g 4 sn])
>        , addDur tn [es 4, fs 4, es 4, fs 4, es 4, e 4, d 4, c 4], ascent sunPygLead (D, 4) qn]
>           
>    g143 =
>      line [
>        tempo (5/4) (
>          line [grace (-3) (c 4 qn), a 5 qn, c 6 qn, d 6 qn
>              , t32 [g 6 en, g 6 en, d 6 en]])]
>    g144 = line [grace (-2) (d 6 en), d 6 sn, d 6 sn, d 6 qn, grace (-3) (g 6 dqn)
>               , grace (-2) (fs 6 en)]
>   
>    m145_148 = line [m144, m144, m144, m144]
>    g145_148 = line [g145, g146, g147, g148]
>   
>    g145 = line [t32 [fs 6 en, d 6 en, c 6 en], a 5 sn, grace (-2) (a 5 den), g 5 dqn, a 5 en]
>    g146 = line [rest en, chord [grace (-1) (c 6 en), grace (-2) (g 6 en)]
>               , t32 [chord [c 6 en, g 6 en], rest en, grace (-1) (f 6 en)]
>               , f 6 sn, d 6 sn, c 6 sn, a 5 sn
>               , t32 [grace (-2) (a 5 qn), a 5 sn, chord [a 5 sn, d 6 sn, f 6 sn]]]
>    g147 = line [chord [c 6 en, f 6 en], d 6 sn, c 6 sn, t32 [d 6 en, ds 6 en, d 6 en]
>               , t32 [grace 1 (c 6 en), e 5 en, e 6 en], c 6 sn, b 5 den]
>    g148 = line [t32 [c 6 qn, e 5 en, d 6 sn, chord [c 6 sn, f 6 sn], chord [cs 6 sn, f 6 sn]]
>               , grace (-2) (a 5 en), g 5 dqn, a 5 sn, chord [d 4 sn, g 4 sn]]
>   
>    m149_152 = line [m144, m150, m151, m152]
>    g149_152 = line [g149, g150, g151, g152]
>   
>    m150 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [percCCen, percLTen], percBDen, percCCen
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], percBDen, chord [percCCen, percLTen]
>        , percBDen, percCCen]
>    m151 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [percCCen, percLTen], percBDen, percCCen
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], percBDen
>        , chord [percCCen, percLTen], chord [percCCen, percBDen], chord [percCCen, percBDen]]
>
>    m152 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [percCCen, percLTen], percBDen, percCCen
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>        , chord [percCCen, percBDen]]
>   
>    g149 = line [chord [d 4 en, g 4 en], chord [grace 2 (d 6 en), fs 6 en], chord [d 6 en, g 6 en]
>               , chord [c 6 en, e 6 en], t32 [d 6 qn, a 5 en]
>               , tempo (5/4) (line [g 6 sn, d 6 sn, e 6 sn, d 6 en])]
>    g150 = line [tempo (5/4) (line [chord [a 5 sn, c 6 sn], d 6 sn, c 6 sn, chord [a 5 sn, d 6 sn], d 6 sn])
>               , t32 [a 5 sn, g 5 sn, f 5 sn], g 5 sn, g 5 tn, f 5 tn
>               , t32 [d 5 sn, grace (-3) (f 5 sn), d 5 sn, c 5 sn, e 5 sn, c 5 sn]
>               , c 4 sn, d 4 sn, d 4 tn, c 4 tn, a 4 tn, g 4 tn]
>    g151 = line [a 4 sn, g 4 en, f 4 sn, t32 [f 4 sn, d 4 sn, c 4 sn], d 4 en, d 4 en
>               , descent sunPygLead (D, 6) en, t32 [rest en, grace (-2) (d 4 en), df 4 en]]
>    g152 = line [c 4 qn, t32 [d 4 qn, d 4 en], e 4 qn, g 4 qn]
>   
>    m153_156 = line [m153, m154, m155, m156]
>    g153_156 = line [g153, g154, g155, g156]
>   
>    m153 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>        , chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>        , chord [percRCen, percBDen], chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>        , chord [percRCen, percBDen], chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>        , chord [percRCen, percBDen]]
>    m154 =
>      t32 [
>          chord [perc RideCymbal1 qn, perc LowTom qn], chord [percRCen, percBDen]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>        , chord [percRCen, percLTen], percBDen, percBDen
>        , chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>        , chord [percCCen, percBDen]]
>    m155 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc LowTom qn], chord [percCCen, percBDen]
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [percCCsn, percLTsn, percBDsn], percm LowTom [sn, sn, sn, sn, sn]
>        , times 3 (chord [percCCen, percLTen, percBDen])]
>    m156 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [percCCen, percLTen], percBDen, percCCen
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], times 4 (chord [percCCen, percBDen])]
>   
>    g153 = line [chord [e 4 qn, a 4 qn], c 5 qn, d 5 qn, t32 [a 5 en, c 6 en, b 5 en]]
>    g154 = line [t32 [a 5 en, g 5 en, a 5 en], addDur sn [c 6, b 5, a 5, g 5]
>               , t32 [a 5 en, b 5 en, c 6 en, d 6 en, e 6 en, e 6 en]]
>    g155 = line [t32 [fs 6 en, fs 6 en, t32 [e 6 sn, fs 6 sn, e 6 sn], d 6 en, e 6 en, e 6 en]
>               , t32 [d 6 sn, e 5 sn, c 6 sn], d 6 en
>               , t32 [c 6 en, b 5 en, b 5 sn, c 6 tn, b 5 tn]]
>    g156 = line [t32 [a 5 en, g 5 sn], a 5 dsn, g 5 tn, t32 [a 5 qn, a 5 en]
>                    , t32 [b 5 en, a 5 en, b 5 en, d 6 qn, d 6 en]]
>   
>    m157_160 = line [m157, m158, m159, m160]
>    g157_160 = line [g157, g158, g159, g156]
>   
>    m157 = line [t32 [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]]
>               , chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>               , chord [perc CrashCymbal1 qn, perc BassDrum1 qn] 
>               , chord [perc CrashCymbal1 qn, perc LowTom qn]]
>    m158 = line [chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>               , chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , chord [perc CrashCymbal1 qn, perc BassDrum1 qn]]
>    m159 = line [chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], perc LowTom qn, perc HighTom qn]
>    m160 =
>      t32 [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>         , chord [perc CrashCymbal1 qn, perc LowTom qn], chord [percCCen, percBDen]
>         , chord [line [percCCen, percCCen], perc BassDrum1 qn], percBDsn, percBDsn
>         , percLTen, perc LowTom qn]
>   
>    g157 = line [descent sunPygLead (D, 6) qn, t32 [rest qn, e 5 en, g 5 en, a 5 en, rest en]
>               , grace (-1) (f 6 qn)]
>    g158 = line [fs 6 en, rest en, grace (-2) (e 6 dqn), rest en, d 6 qn]
>    g159 = line [fs 6 en, a 6 en, rest hn, grace (-1) (f 6 qn)]
>    g160 = line [fs 6 en, rest en, grace (-2) (e 6 dqn), rest en, t32 [e 6 en, d 6 en, b 5 en]]
>   
>    m161_164 = line [m161, m162, m163, m164]
>    g161_164 = line [g161, g162, g163, g164]
>   
>    m161 =
>      line [
>          t32 [
>              chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>            , chord [percCCen, percBDen], chord [percCCen, percLTen]
>            , perc LowTom qn]
>        , chord [percCCen, percLTen, percBDen], chord [percCCsn, percBDsn]
>        , chord [percCCsn, percBDsn], perc LowTom qn]
>    m162 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [percCCen, percLTen], percCCen, chord [percCCen, percBDen]
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCCen, percBDen]
>        , chord [perc CrashCymbal1 qn, perc LowTom qn], chord [percCCen, percBDen]]
>    m163 =
>      line [
>          chord [percLTen, percBDen], percOHHen, percCHHsn, percBDsn, percCHHen
>        , t32 [chord [percCHHsn, percLTsn], chord [percLTsn, percBDsn], chord [percCHHsn, percLTsn]
>             , chord [percLTsn, percBDsn], percLTsn, chord [percLTsn, percBDsn]]
>        , chord [perc CrashCymbal1 qn, perc LowTom qn]]
>    m164 = line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>               , t32 [chord [perc RideCymbal1 qn, perc BassDrum1 qn], percBDen]
>               , t32 [chord [percRCen, percBDen], percRCen, chord [percRCen, percBDen]]
>               , t32 [chord [percRCen, percLTen], chord [percCHHen, percBDen], chord [percCHHen, percBDen]]]
>   
>    g161 = line [t32 [grace (-2) (b 5 sn), a 5 den, g 5 dqn, a 5 en], a 5 en, rest den
>               , a 6 sn, g 6 en]
>    g162 = line [g 6 en, a 6 en, grace (-2) (a 6 sn), g 6 (den+en), a 6 en
>               , grace (-2) (a 6 qn)]
>    g163 = line [t32 [g 6 en, a 5 sn], chord [a 5 en, d 6 en], t32 [d 6 en, f 5 en, d 5 sn, f 5 sn]
>               , addDur tn [g 5, f 5, e 5, d 5], f 5 sn, t32 [rest tn, a 4 tn, b 3 tn]
>               , chord [b 4 tn, f 5 tn], d 5 tn, c 5 tn, a 4 tn, c 5 dsn, c 5 tn]
>    g164 = line [a 4 sn, g 4 tn, f 4 tn, g 4 tn, f 4 tn, g 4 tn, f 4 tn
>               , t32 [c 4 en, d 4 en, d 4 en], d 4 sn, c 4 sn, d 4 sn, d 4 (sn + den), rest sn]
>   
>    m165_168 = line [m165, m166, m167, m168]
>    g165_168 = line [g165, g166, g167, g168]
>   
>    m165 =
>      t32 [
>          chord [percRCsn, percLTsn], chord [percRCsn, percBDsn], percLTsn, percBDsn
>        , chord [percRCsn, percLTsn], percBDsn, chord [percRCen, percLTen], percBDen
>        , chord [percRCen, percBDen], chord [percRCen, percBDen], percRCsn, percRCsn
>        , chord [percRCsn, percLTsn], percBDsn, chord [percRCen, percBDen]
>        , chord [percRCsn, percBDsn], percRCsn, percRCen]
>    m166 = 
>      t32 [
>          chord [percRCen, percBDen], chord [percRCsn, percLTsn], chord [percRCsn, percBDsn]
>        , chord [percRCsn, percLTsn], percBDsn, chord [perc RideCymbal1 qn, perc LowTom qn]
>        , chord [percRCen, percBDen], chord [percRCen, percLTen, percBDen]
>        , chord [percCHHen, percBDen], chord [percRCen, percBDen]
>        , chord [percRCen, percLTen, percBDen], chord [percRCen, percBDen]
>        , chord [percRCen, percBDen]]
>    m167 =
>      t32 [
>        t32 [times 6 (line [chord [percRCen, percBDen], percHTen, percLTen])]]
>    m168 =
>      t32 [
>          chord [percCCen, percBDen], chord [percRCen, percBDen], percRCen
>        , chord [percRCen, percLTen], percRCen, chord [percRCen, percBDen]
>        , chord [percRCen, percBDen], percRCen, chord [percRCen, percBDen]
>        , chord [percRCen, percLTen], chord [percRCen, percLTen]
>        , chord [percRCsn, percLTsn], percHTsn]
>
>    g165 =
>      t32 [
>          addDur sn [e 3, fs 3, e 3, fs 3, e 3, fs 3], a 3 qn, a 3 en, a 3 qn, a 3 en
>        , a 3 qn, e 3 en] 
>    g166 =
>      t32 [
>          c 4 en
>        , addDur sn [d 4, a 3, d 4, a 3, d 4, c 4, d 4, c 4, d 4, c 4, d 4, c 4, d 4, c 4
>                   , d 4, c 4, d 4, c 4, d 4, c 4, d 4, c 4]]
>    g167 =
>      t32 [
>          d 4 sn, a 3 sn, d 4 sn, a 3 sn, chord [d 4 sn, g 4 sn], a 3 sn
>        , chord [d 4 sn, g 4 sn], a 3 sn, d 4 sn, a 3 sn, chord [d 4 sn, g 4 sn], a 3 sn
>        , chord [d 4 en, g 4 en], a 3 sn, d 4 tn, rest tn, g 3 sn, d 4 sn
>        , chord [a 3 qn :+: c 4 en, d 4 qn :+: d 4 en]]
>    g168 =
>      t32 [
>          chord [d 4 sn, g 3 sn], a 3 sn, chord [a 3 sn, c 4 sn], chord [d 4 sn, g 3 sn]
>        , a 3 sn, d 4 sn, g 4 sn, d 4 sn, chord [d 4 sn, g 3 sn], d 4 den, chord [d 4 en, g 3 en]
>        , chord [d 4 en, c 5 en], chord [d 4 en, g 3 en], g 4 sn, d 4 sn, g 4 sn
>        , d 4 sn, d 4 en]
>   
>    m169_172 = line [m169, m170, m171, m172]
>    g169_172 = line [g169, g170, g171, g172]
>   
>    m169 =
>      t32[
>         chord [percRCsn, percHTsn, percBDsn], percHTsn, percBDsn, percHTsn
>       , chord [percCHHsn, percBDsn], percHTsn, chord [percRCsn, percBDsn], percLTsn, percBDsn
>       , percHTsn, chord [percRCsn, percBDsn], percHTsn
>       , percBDen, percHTen, chord [percRCsn, percHTsn], percLTen, chord [percRCsn, percBDsn]
>       , rest en, chord [percRCen, percBDen]]
>    m170 = line [t32 [chord [perc RideCymbal1 qn, perc BassDrum1 qn], percHTen]
>               , chord [percRCen, percHTen, percBDen], chord [percRCsn, percBDsn]
>               , chord [percRCsn, percBDsn], percLTen, percLTen, chord [percRCen, percBDen]
>               , chord [percRCen, percBDen]]
>    m171 =
>      line [percLTen, percLTsn, percLTsn
>          , tempo (5/4)
>              (line [
>                 percm LowTom [sn, sn, sn, sn]
>               , times 3 (chord [percRCsn, percLTsn, percBDsn]), percLTsn, percLTsn, percLTsn])
>          , percLTsn, percLTsn, chord [percRCen, percBDen]]
>    m172 = line [chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , rest sn, chord [percCCen, percBDen], chord [percCCsn, percBDsn]
>               , percLTen, perc LowTom (en + den), chord [percCCsn, percLTsn, percBDsn]]
>   
>    g169 =
>      t32 [
>          chord [d 4 en, a 4 en, d 5 en], chord [d 4 en, g 4 en], d 4 en
>        , d 4 sn, rest sn, chord [e 3 sn, d 4 sn], g 4 sn, a 4 en
>        , g 4 qn, grace (-2) (b 4 en), a 4 qn, b 4 en]
>    g170 = line [t32 [a 4 qn, grace (-2) (b 4 en)], c 5 qn
>               , t32 [e 6 en, addDur sn [d 4, a 3, d 4, a 3, d 4, a 3, d 4, g 4, d 4, g 4]]]
>    g171 = line [d 4 dsn, a 4 tn, d 4 en, t32 [d 4 en, g 4 sn, g 4 sn, d 4 en]
>               , d 4 sn, b 3 sn, b 4 dqn]
>    g172 =
>      t32 [
>          rest en, d 4 en, chord [a 4 en, a 6 en]
>        , addDur sn [g 4, d 4, g 4, d 4, g 4, d 4, g 4, d 4, g 4, d 4, g 4, a 4]
>        , c 5 sn, g 4 sn, chord [b 4 sn, e 5 sn], chord [g 4 sn, e 5 sn], g 4 sn, g 4 sn]
>   
>    m173_176 = line [m173, m174, m175, m176]
>    g173_176 = line [g173, g174, g175, g176]
>   
>    m173 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], perc LowTom qn
>               , tempo (5/4) (line [chord [percCCen, percBDen], chord [perc CrashCymbal1 den, perc BassDrum1 den]])
>               , perc LowTom qn]
>    m174 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn] 
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]]
>    m175 = line [percLTsn, percLTsn
>               , t32 [chord [percRCsn, percLTsn, percBDsn], percLTsn, percLTsn]
>               , t32 [chord [percRCsn, percLTsn, percBDsn], percLTsn, percLTsn]
>               , t32 [percLTsn, percLTsn, chord [percRCsn, percLTsn, percBDsn]]
>               , t32 [chord [percRCsn, percHTsn, percBDsn], percHTsn, percHTsn]
>               , t32 [percHTsn, percHTsn, percHTsn]
>               , t32 [percLTsn, percLTsn, percLTsn, percBDsn, percBDsn, percBDsn]]
>    m176 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [perc CrashCymbal1 qn, perc LowTom qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]]
>   
>    g173 = line [chord [a 3 sn, g 4 sn], chord [c 4 sn, c 5 sn], a 3 en
>               , t32 [d 4 en, c 4 en, a 3 dqn, a 3 en, g 3 en, d 4 en, fs 3 en]]
>    g174 = line [fs 3 hn, t32 [g 3 qn, fs 3 en, g 3 en, fs 3 en, g 3 en]]
>    g175 = line [a 3 en, a 3 sn, a 3 (sn + qn), g 5 en, chord [a 4 en, d 4 en, fs 4 en]
>               , rest en, chord [g 4 en, c 5 en, e 5 en]]
>    g176 = line [chord [a 4 qn, d 5 qn, fs 5 qn]
>               , t32 [chord [a 4 en, d 5 en, fs 5 en], rest en, e 3 en]
>               , chord [grace (-1) (a 4 qn), grace (-1) (d 5 qn), grace (-1) (g 5 qn)]
>               , chord [grace (-1) (a 4 en), grace (-1) (d 5 en), grace (-1) (g 5 en)]
>               , chord [a 4 en, d 5 en, fs 5 en]]
>   
>    m177_180 = line [m177, m178, m179, m180]
>    g177_180 = line [g177, g178, g179, g180]
>   
>    m177 = line [chord [perc CrashCymbal1 qn, perc LowTom qn], chord [percCCen, percLTen]
>               , chord [percCCen, percLTen, percBDen], percBDen, chord[percCCen, percLTen]
>               , chord[percCCen, percLTen], chord [percCCen, percLTen, percBDen]]
>    m178 = line [chord [percCCen, percBDen], percBDen, chord [percCCen, percBDen]
>               , percLTsn, percBDsn, chord [percCCen, percLTen, percBDen], percBDen
>               , chord [percRCen, percLTen], percBDsn, percCCsn]
>    m179 = line [percLTsn, percLTsn, chord [percCCen, percBDen], percLTen, percLTsn
>               , chord [percCCsn, percBDsn], percCCsn, percBDsn
>               , chord [percCCen, percLTen, percBDen], percLTen, percLTsn, percLTsn]
>    m180 = line [chord [percCCsn, percLTsn, percBDsn], percLTsn
>               , chord [percCCen, percLTen, percBDen], chord [percRCen, percBDen]
>               , chord [percCCsn, percBDsn], percBDsn, chord [percCCen, percLTen, percBDen]
>               , chord [percRCen, percBDen], chord [percCCen, percBDen]
>               , chord [percCCen, percLTen, percBDen]]
>   
>    g177 = line [chord [a 4 en, d 5 en, fs 5 en], a 4 en, chord [d 6 en, fs 6 en]
>               , chord [d 6 en, fs 6 en], chord [d 6 en, fs 6 en]
>               , chord [b 4 en, d 6 en, fs 6 en], chord [a 4 en, c 4 en, e 5 en]
>               , chord [a 4 en, c 4 en, e 5 en]]
>    g178 =
>      line [
>        chord [grace (-2) (a 4 qn), grace (-2) (d 5 qn), grace (-2) (fs 5 qn)]
>      , chord [line [chord [b 4 qn, c 5 qn, fs 5 qn], chord [g 4 qn, c 5 qn, e 5 sn]]
>             , line [rest en, a 3 qn, a 3 en]]
>      , chord [g 4 en, c 5 en, e 5 en], chord [d 4 en, g 4 en, c 5 en]]
>    g179 = line [chord [d 4 hn, g 4 hn, c 5 hn], rest en, chord [d 4 en, g 4 en, c 5 en]
>               , d 4 sn, chord [d 4 den, g 4 den, c 5 den]]
>    g180 = line [t32 [chord [a 4 qn, d 5 qn, fs 4 qn], e 4 en], e 4 en, d 4 en
>               , t32 [chord [a 4 qn, d 4 qn, g 4 qn], b 4 en]
>               , chord [a 4 den, d 5 den, g 5 den, d 6 den]
>               , chord [a 4 sn, d 5 sn, fs 5 sn, d 6 sn]]
>   
>    m181_184 = line [m181, m182, m183, m184]
>    g181_184 = line [g181, g182, g183, g184]
>   
>    m181 = line [t32 [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>                    , percLTen, percBDen, chord [percCCsn, percBDsn]
>                    , chord [percCCen, percBDen], percLTsn]
>               , perc BassDrum1 den, perc LowTom sn, chord [percLTen, percBDen]
>               , chord [percLTen, percBDen]]
>    m182 = line [chord [perc CrashCymbal1 den, perc BassDrum1 den], chord [percCCsn, percBDsn]
>               , rest en, chord [percCCen, percBDen]
>               , chord [percCCsn, percLTsn], percBDsn, chord [percCHHsn, percLTsn]
>               , chord [percCHHsn, percLTsn], chord [percCHHsn, percBDsn]
>               , chord [percLTsn, percBDsn], chord [percCHHsn, percLTsn], percLTsn]
>    m183 = line [chord [percOHHen, percLTen, percBDen], chord [percCHHsn, percLTsn], percLTsn
>               , chord [percOHHen, percBDen], chord [percCCen, percOHHen, percBDen]
>               , chord [percCCsn, percBDsn], percHTsn, percHTsn, percHTsn
>               , chord [percHTsn, percBDsn], percHTsn, percHTsn, percHTsn]
>    m184 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [percRCen, percLTen], chord [percRCen, percBDen]
>               , chord [percRCen, percLTen], chord [percRCen, percBDen]]
>   
>    g181 = line [t32 [chord [a 4 qn, d 5 qn, fs 5 qn, d 6 qn], chord [a 4 en, d 5 en]]
>               , chord [a 4 (qn + den), d 5 (qn + den)], e 3 sn, a 6 sn, rest sn
>               , a 4 en]
>    g182 = line [t32 [chord [b 4 qn, e 5 qn, g 5 qn], chord [a 4 en, d 5 en, fs 5 en]]
>               , t32 [chord [b 4 qn, e 5 qn, g 5 qn], chord [a 4 en, d 5 en, fs 5 en]]
>               , chord [b 4 qn, e 5 qn, g 5 qn], chord [grace (-1) (d 5 en), grace (-1) (fs 5 en)]
>               , chord [grace 2 (a 4 en), grace 2 (c 5 en), grace 2 (e 5 en)]]
>    g183 = line [chord [a 4 hn, c 5 hn, e 5 hn], chord [a 4 en, d 5 en, fs 5 en], rest en
>               , chord [a 4 en, d 5 en, fs 5 en], chord [g 4 en, c 5 en, e 5 en]]
>    g184 = line [chord [a 4 qn, d 5 qn, g 5 qn], t32 [b 6 en, rest qn]
>               , chord [grace (-1) (a 4 qn), grace (-1) (d 5 qn), grace (-1) (g 5 qn), grace (-1) (b 5 qn)]
>               , chord [grace (-1) (a 4 en), grace (-1) (d 5 en), grace (-1) (g 5 en), grace (-1) (b 5 en)]
>               , chord [a 4 en, d 5 en, a 5 en]]
>   
>    m185_188 = line [m185, m186, m187, m188]
>    g185_188 = line [g185, g186, g187, g188]
>   
>    m185 = line [percLTen, chord [percRCen, percBDen], chord [percRCen, percBDen], percLTen
>               , chord [percRCen, percBDen], chord [percRCen, percBDen], percLTen, percBDen]
>    m186 = line [percBDen, percLTen, t32 [rest en, chord [perc RideCymbal1 qn, perc BassDrum1 qn]]
>               , rest qn, chord [perc RideCymbal1 qn, perc BassDrum1 qn]]
>    m187 = line [rest en, percBDsn, percBDsn, percBDen, percBDen, rest sn
>               , chord [perc RideCymbal1 den, perc BassDrum1 den]
>               , t32 [times 3 (chord [percRCen, percBDen])]]
>    m188 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>               , chord [percCHHen, percBDen], percBDen
>               , chord [perc CrashCymbal1 den, perc LowTom den], percBDsn, percLTsn, percBDsn
>               , chord [percCCen, percLTen]]
>   
>    g185 = line [rest en, chord [grace (-7) (a 5 en), grace (-7) (c 6 en), grace (-7) (b 6 en)]
>               , chord [a 5 en, c 6 en, fs 6 en], chord [a 5 qn, c 6 qn, fs 6 qn]
>               , chord [descent sunPygLead (A, 5) en
>                      , descent sunPygLead (C, 6) en
>                      , descent sunPygLead (Fs, 6) en]
>               , t32 [rest qn, chord [a 3 en, a 4 en]]]
>    g186 = line [t32 [chord [a 3 en, g 4 en], c 5 en, chord [a 4 en, d 5 en, fs 5 en]]
>               , chord [a 4 en, d 5 en, fs 5 en], chord [g 4 en, c 5 en, e 5 en]
>               , chord [g 4 qn, c 5 qn, e 5 qn]
>               , t32 [times 2 (chord [g 4 en, c 5 en, e 5 en, b 5 en]), rest en]]
>    g187 =
>      line [
>          t32 [
>              c 5 sn, chord [c 5 sn, d 5 sn], chord [c 5 qn, d 5 qn]
>            , chord [a 4 sn, d 5 sn], chord [a 4 sn, d 5 sn], chord [g 4 en, c 5 en], fs 4 en
>            , fs 4 sn, g 4 sn, fs 4 sn]
>        , a 3 en, t32 [g 4 en, fs 4 en, d 4 en]]
>    g188 = line [t32 [a 3 qn, rest en], chord [g 4 en, c 5 en, e 5 en]
>               , chord [a 4 dqn, d 5 dqn, fs 5 dqn], chord [b 4 sn, e 5 sn, g 5 sn]
>               , chord [a 4 sn, d 5 sn, fs 5 sn], chord [b 4 en, e 5 en, g 5 en]]
>   
>    m189_192 = line [m189, m190, m191, m192]
>    g189_192 = line [g189, g190, g191, g192]
>   
>    m189 = line [percLTsn, percBDsn, chord [percCCsn, percBDsn], percLTsn, percLTsn, percLTsn
>               , chord [percCCsn, percLTsn, percBDsn], percHTsn
>               , t32 [percHTsn, percHTsn, percHTsn], percLTsn, percBDsn
>               , t32 [percLTsn, percLTsn, percLTsn], percLTsn, chord [percCCsn, percLTsn, percBDsn]]
>    m190 = line [chord [percCCen, percBDen], percLTsn, chord [percCCsn, percBDsn], rest en
>               , chord [percCCsn, percBDsn], chord [percCCsn, percBDsn], percLTsn, percBDsn
>               , chord [percCCen, percLTen], chord [perc CrashCymbal1 qn, perc BassDrum1 qn]]
>    m191 = line [times 4 (chord [percCCen, percBDen])
>               , t32 [times 3 (chord [perc RideCymbal1 qn, perc LowTom qn, perc BassDrum1 qn])]]
>    m192 = line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>               , chord [percCCen, percBDen], chord [percCCen, percBDen]
>               , chord [perc CrashCymbal1 qn, perc LowTom qn], percRCen
>               , chord [percCCen, percLTen, percBDen]]
>   
>    v190 = line [rest dwn, t32 [a 4 qn, b 4 qn, d 5 qn]]
>    v192 = line [a 4 dqn, b 4 sn, a 4 sn, g 4 (hn + dhn), e 4 qn]
>   
>    g189 = line [t32 [descent sunPygLead (A, 6) qn, chord [d 6 en, g 6 en]], chord [g 4 en, c 5 en, e 5 en]
>               , chord [grace (-2) (a 6 qn), grace (-2) (d 5 qn), grace (-2) (fs 5 qn)]
>               , chord [a 3 en, a 4 en, d 5 en]
>               , t32 [chord [a 3 qn, a 4 qn, d 5 qn, fs 5 qn], chord [a 3 en, a 4 en, d 5 en, fs 5 en]]]
>    g190 = line [times 4 (chord [g 4 sn, c 5 sn, e 5 sn]), chord [a 4 sn, d 5 sn], g 4 sn, a 3 en
>               , times 2 (chord [g 4 en, c 5 en, e 5 en])
>               , chord [grace (-2) (a 4 en), grace (-2) (d 5 en), grace (-2) (fs 5 en)]
>               , chord [grace 2 (g 4 sn), grace 2 (c 5 sn), grace 2 (e 5 sn)], a 3 sn]
>    g191 = line [chord [grace (-2) (a 4 qn), grace (-2) (d 5 qn), grace (-2) (fs 5 qn)]
>               , t32 [grace (-2) (d 5 qn), b 6 en]
>               , t32 [fs 5 qn, g 5 qn, a 5 qn]]
>    g192 = line [fs 5 dqn, g 5 sn, fs 5 sn, d 5 hn]
>   
>    m193_196 = line [m193, m194, m195, m196]
>    g193_196 = line [g193, g194, g195, g196]
>   
>    m193 = line [percCCen, chord [percCCen, percBDen], chord [percCCen, percBDen], percBDsn
>               , percBDsn, chord [percRCen, percLTen], percBDen, chord [percCCen, percBDen]
>               , percBDen]
>    m194 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], rest en
>               , chord [percRCsn, percBDsn], percBDsn, chord [percRCen, percLTen], percRCen
>               , chord [percRCen, percBDen], percLTsn, percLTsn]
>    m195 = line [percLTsn, chord [percRCsn, percLTsn, percBDsn], rest sn
>               , chord [percRCsn, percBDsn], chord [percRCsn, percBDsn]
>               , chord [percCCsn, percLTsn], percBDsn, percBDsn
>               , chord [percCCsn, percLTsn], percBDsn, chord [percCCsn, percLTsn], percBDsn
>               , t32 [chord [percCCsn, percLTsn], percm LowTom [sn, sn, sn, sn, sn]]]
>    m196 =
>      t32 [
>          chord [perc RideCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc CrashCymbal1 qn, perc LowTom qn]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn]]
>   
>    v194 = line [e 5 dqn, d 5 en, d 5 dwn]
>    v196 = line [t32 [a 4 qn, b 4 qn, d 5 qn, a 4 qn, b 4 qn, d 5 qn], a 4 en, b 4 sn, a 4 sn
>               , g 4 hn, fs 4 en, g 4 en]
>   
>    g193 = line [e 5 (hn + en), d 5 en, d 5 qn]
>    g194 = line [chord [descent sunPygLead (Fs, 5) dqn
>                      , descent sunPygLead (A, 5) dqn
>                      , descent sunPygLead (D, 6) dqn]
>               , chord [fs 5 en, a 5 en, d 6 en], chord [fs 5 hn, a 5 hn, d 6 hn]]
>    g195 = line [t32 [e 5 sn, g 4 sn, e 5 sn], c 5 en, fs 4 sn, g 4 sn, fs 4 en, e 4 qn, rest qn]
>    g196 =
>      t32 [fs 5 qn, g 5 qn, a 5 qn, fs 5 qn, g 5 qn, a 5 qn]
>   
>    m197_200 = line [m197, m198, m199, m200]
>    g197_200 = line [g197, g198, g190, g200]
>   
>    m197 = line [chord [perc CrashCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , chord [percRCen, percLTen], percRCen
>               , chord [percCCen, percBDen], chord [percCCen, percBDen]]
>    m198 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percCHHen, percBDen]
>               , chord [percCHHen, percBDen], chord [percCHHen, percLTen], percCHHen
>               , chord [percOHHen, percBDen], chord [percOHHen, percBDen]]
>    m199 = line [percLTsn, percLTsn, chord [percCCen, percBDen], rest en, percBDsn, percBDsn
>               , chord [percCCsn, percLTsn], chord [percCCen, percBDen], percBDsn
>               , percLTsn, percBDsn, chord [percCCen, percLTen]]
>    m200 =
>      t32 [
>          chord [perc CrashCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc CrashCymbal1 qn, perc LowTom qn]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn]
>        , chord [perc RideCymbal1 qn, perc BassDrum1 qn]]
>   
>    v198 = line [e 5 dqn, d 5 en, d 5 (hn + dhn), rest qn]
>    v200 = line [t32 [a 4 qn, b 4 qn, d 5 qn, a 4 qn, b 4 qn, d 5 qn], a 4 en, b 4 sn, a 4 sn
>               , g 4 hn, e 4 qn]
>    g197 = line [a 4 en, b 4 sn, a 4 sn, g 4 hn, fs 4 en, g 4 en]
>    g198 = line [chord [c 5 dqn, e 5 dqn], chord [fs 4 en, a 4 en], chord [fs 4 dqn, a 4 dqn], rest en]
>    g199 = line [a 6 en, a 6 sn, a 6 sn, a 6 en, t32 [b 6 sn, a 6 sn, g 6 sn]
>               , tempo (5/4) (line [a 6 en, a 6 den]), a 5 en, rest en]
>    g200 =
>      t32 [fs 5 qn, g 5 qn, a 5 qn, fs 5 qn, g 5 qn, a 5 qn]
>   
>    m201_204 = line [m201, m202, m203, m204]
>    g201_204 = line [g201, g202, g203, g204]
>   
>    m201 = line [chord [perc RideCymbal1 qn, perc LowTom qn, perc BassDrum1 qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]
>               , perc CrashCymbal1 qn
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]]
>    m202 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [percRCen, percLTen], percRCen
>               , chord [percRCen, percBDen], chord [percCCen, percBDen]]
>    m203 = line [t32 [rest en, percLTen, percLTen, chord [perc RideCymbal1 qn, perc BassDrum1 qn], percLTen]
>               , t32 [percLTen, chord [percRCen, percBDen], percLTen]
>               , t32 [percLTen, chord [percRCen, percBDen], percLTen]]
>    m204 =
>      t32 [
>          times 3 (chord [perc RideCymbal1 qn, perc BassDrum1 qn])
>        , chord [perc CrashCymbal1 qn, perc LowTom qn]
>        , times 2 (chord [perc RideCymbal1 qn, perc BassDrum1 qn])]
>   
>    v202 = line [descent sunPygChoir (Fs, 5) dqn, d 5 en, d 5 (wn + qn), rest qn]
>    v204 = line [t32 [a 4 qn, b 4 qn, d 5 qn, a 4 qn, b 4 qn, d 5 qn], a 4 en, b 4 sn, a 4 sn
>               , g 4 hn, fs 4 en, g 4 en]
>   
>    g201 = line [fs 5 en, g 5 sn, fs 5 sn, e 5 hn, d 5 qn]
>    g202 = line [chord [descent sunPygLead (Fs, 5) dqn
>                      , descent sunPygLead (A, 5) dqn
>                      , descent sunPygLead (D, 6) dqn]
>               , chord [d 5 en, fs 5 en, a 5 en], chord [d 5 dqn, fs 5 dqn, a 5 dqn], a 3 en]
>    g203 = line [t32 [e 6 en, chord [g 4 en, c 5 en], fs 4 en], fs 4 sn, g 4 sn, fs 4 sn, g 4 sn
>               , e 4 qn, d 4 qn]
>    g204 =
>      t32 [fs 5 qn, g 5 qn, a 5 qn, fs 5 qn, g 5 qn, a 5 qn]
>   
>    m205_207 = line [m205, m206, m207]
>    g205_207 = line [g205, g206, g207]
>   
>    m205 = line [chord [perc RideCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [percCCen, percLTen], percBDen
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]]
>    m206 = line [chord [perc CrashCymbal1 qn, perc BassDrum1 qn], chord [percRCen, percBDen]
>               , chord [percRCen, percBDen], chord [perc RideCymbal1 qn, perc LowTom qn]
>               , chord [percRCen, percBDen], chord [percRCen, percBDen]]
>    m207 = line [percm LowTom [sn, sn, sn, sn, sn], percm HighTom [sn, sn, sn], perc BassDrum1 hn]
>   
>    v206 = line [e 4 dqn, d 4 en, d 4 dwn]
>   
>    g205 = line [fs 5 en, g 5 sn, a 5 sn, e 5 hn, d 5 en, e 5 en]
>    g206 = line [grace (-1) (c 5 dqn), a 4 en, a 4 (hn + dhn), c 5 qn]
>    g207 = line []
>   
>    shortPig = 
>      removeZeros
>      $ tempo 1
>      $ addVolume 55 m001''
>   
>    m001' = line [percBDen, percOHHen, percOHHen, chord [percOHHen, percBDen]
>                , chord [percOHHen, percLTen], percOHHen, percOHHen
>                , chord [percOHHen, percBDen]]
>   
>    m001'' = line [percBDqn, percCHHqn, percCHHqn, chord [percCHHqn, percBDqn]
>                , chord [percCHHqn, percLTqn], percCHHqn, percCHHqn
>                , chord [percCHHqn, percBDqn]]

The End
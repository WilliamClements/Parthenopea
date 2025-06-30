# Parthenopea
Haskell library extending *Euterpea 2* music educational package. (See https://www.euterpea.com/ and obtain its textbook ***Haskell School of Music***.)

Parthenopea, using Euterpea and its own SoundFont-specific technology, provides **offline** rendering of songs specified in (user-supplied) MIDI files or created programmatically in Euterpea.

Can also be used to survey and compare thousands of Instruments found in (user-supplied) SoundFont files. Doesn't necessarily require direct use of Euterpea library.

# Building it
Some difficulty here. User needs to know *cabal* (e.g. *cabal.project*) to set up multiple local packages. Also how to debug broken dependency versioning. https://github.com/georgefst/Euterpea2 is a fork of Euterpea intended to help with the latter problem.

# Batch processor
Can initiate batch processing by typing *pCommand* in REPL (i.e. *ghci*). Later on, if you scale up to many/large SoundFont files (I've tested with 500 SoundFonts taking up 46 GB), you should build a standalone executable (*PCommand*) for this purpose, for better performance.

The batch processor opens all MIDI files (\*.mid) and SoundFont files (\*.sf2) found in the current directory. If both types are present, the batch processor will mine the SoundFont files to "convert" the MIDI files to WAV files you can listen to and share.

# Rendering
Parthenopea is opiniated. It picks only the most suitable SoundFont Instruments to realize each MIDI (GM) instrument. The criteria used, and all the scores, are provided in a textual *Tournament Report*.

Note that the result obtained when mixing Instruments from *unrelated* SoundFont files won't sound cohesive. But Parthenopea is not first or foremost a production tool! In the spirit of discovery that Euterpea is known for, I think it is fascinating to listen to my tunes orchestrated varying which SoundFonts to use per rendering run.

# Synthesis
Because of SoundFont (see https://www.synthfont.com/SFSPEC21.PDF), Parthenopea implements **wavetable** synthesis. It is **offline** synthesis because it is non-interactive, utilizing "rendering". The Parthenopea synth is written using Signal Functions ala Euterpea. 

The most difficult aspects to successfully implement were:
1. Creating a (sweepable) low-pass filter **with resonance**. This ended up being a State Variable filter. See https://ccrma.stanford.edu/~jos/svf/svf.pdf .
2. Envelope processing (important for note articulation other than that present in the Sample itself). SoundFont authors tend to use bizarre parameter values which I think are supported by their "synths of choice".
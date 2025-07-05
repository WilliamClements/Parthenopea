# Parthenopea
Haskell library extending *Euterpea 2* music educational package. To Euterpea's song composition capabilities, Parthenopea adds a finishing touch -- experimentative orchestration.

There exists in the wild a vast *trove* of SoundFont files yielding a dizzying variety of sampled Instruments. Granted, the Euterpea user already can connect a synth and manually map Instruments from multiple SoundFont files. But Parthenopea **automatically** makes the highest quality Instrument choices from among the files. This can be a good way to experiment.

To survey, compare, and apply Instruments from ***current*** *trove* doesn't require user to *directly* interface with Euterpea.

For more on Euterpea, see https://www.euterpea.com/ and obtain its textbook ***Haskell School of Music***.

## Building it
Some difficulty here. User needs to know *cabal* (e.g. *cabal.project*) to set up for building multiple local packages in a dependency chain. Also how to debug broken dependency versioning. https://github.com/georgefst/Euterpea2 is a fork of Euterpea intended to work around some build blockers.

## Batch processor
Parthenopea's *batch processor* opens all MIDI files (\*.mid) and SoundFont files (\*.sf2) found in the current directory. If both types are present, it proceeds to mine for best Instruments and "converts" the MIDI files to WAV files -- which you can listen to and share.

You can initiate batch processing by typing *pCommand* in the REPL (i.e. *ghci*). Later on, if you scale up to many/large SoundFont files (I've tested it with 500 SoundFont at 46 GB!), you should build a standalone executable (*PCommand*) for this purpose, for better performance.

## Rendering
Parthenopea is opiniated. As mentioned above, it tries to pick the most suitable SoundFont Instruments to realize each MIDI (GM) instrument. It *scores* Instruments via a combination of empirical stats. All the stats and scores are recorded and provided in a textual *Tournament Report*.

Note that the result obtained when mixing Instruments from *unrelated* SoundFont files won't sound cohesive. But it can be interesting anyway to listen to your tunes *varying* which SoundFont files to draw from.

# Design highlights

## Synthesizer category
Parthenopea implements a **wavetable** type **offline**  synthesizer. ***wavetable*** because Parthenopea works with SoundFont only. ***offline*** because non-interactive, requiring slow rendering steps.

The Parthenopea synth is written using Signal Functions ala Euterpea. 

## (Sweepable) low-pass filter **with resonance**
In Euterpea, neither *filterLowPass* nor *filterLowPassBW* provide resonance as a part of input/response. So I developed a Signal Function that is a State Variable filter (SVF). See https://karmafx.net/docs/karmafx_digitalfilters.pdf and https://ccrma.stanford.edu/~jos/svf/svf.pdf .

## Envelopes
To induce note articulation effects, the SoundFont file Authors specify a set of sound envelope parameter values. (See https://www.synthfont.com/SFSPEC21.PDF).  Difficulties arise from ambiguities in the spec and non-conformance in the artifacts (files). The actual amplitude scaling is accomplished via Euterpea's *envLineSeg* Signal Function. In the interpretation of parameters, we endeavor to:
1. produce a reasonable note
2. honor Author design intent

## Permissive scanning
File scanning must be "fault tolerant" to avoid disqualifying too many otherwise useful SoundFont files and Instruments. This is apparent in the behavior of tools like https://www.polyphone.io/ when opening those flawed files. In Parthenopea, the glitches found are documented in a *Scan Report*.

## Instrument consolidation
A note quality advantage can be attained by Authors via sampling the **natural** Instrument over many small ranges of Pitch and/or Velocity. Some authors, presumably for convenience, spread these many zones over multiple SoundFont instruments, each with a manageable zone count. 

The synthesizer will lose that note quality boost **bigtime** if just one *partial* Instrument is mapped to the given MIDI GM Instrument. So Parthenopea, prior to synthesis, will logically combine partial Instruments into one that has all the zones. 

## Array lookup for zone selection
Nominally, a SoundFont note synthesizer must **search** through an Instrument's zones for the one whose Pitch and Velocity ranges contain the two incoming note parameters. Parthenopea optimizes that by forming an array per Instrument, so the search is reduced to an array lookup. 
# Parthenopea
Haskell library extending *Euterpea 2* music educational package. To Euterpea's song composition capabilities, Parthenopea adds a finishing touch -- experimentative orchestration.

There exists in the wild a vast trove of SoundFont files yielding a dizzying variety of sampled Instruments. Granted, the Euterpea user already can connect a synth and manually map Instruments. But Parthenopea automatically makes the highest quality Instrument choices from *the trove*.

To survey, compare, and apply Instruments from *the trove* doesn't require user to *directly* interface with Euterpea.

For more on Euterpea, see https://www.euterpea.com/ and obtain its textbook ***Haskell School of Music***.

## Building it
Some difficulty here. User needs to know *cabal* (e.g. *cabal.project*) to set up multiple local packages. Also how to debug broken dependency versioning. https://github.com/georgefst/Euterpea2 is a fork of Euterpea intended to work around some build blockers.

## Batch processor
Parthenopea's batch processor opens all MIDI files (\*.mid) and SoundFont files (\*.sf2) found in the current directory. If both types are present, it proceeds to mine for Instruments and "converts" the MIDI files to WAV files -- which you can listen to and share.

You can initiate batch processing by typing *pCommand* in the REPL (i.e. *ghci*). Later on, if you scale up to many/large SoundFont files (I've tested it with 500 SoundFont files taking up 46 GB), you should build a standalone executable (*PCommand*) for this purpose, for better performance.

## Rendering
Parthenopea is opiniated. As mentioned above, it tries to pick the most suitable SoundFont Instruments to realize each MIDI (GM) instrument. It *scores* Instruments via a combination of empirical stats. All the stats and scores are recorded and provided in a textual *Tournament Report*.

Note that the result obtained when mixing Instruments from *unrelated* SoundFont files won't sound cohesive. But it is always interesting to listen to your tunes varying which SoundFont files to draw from.

# Design highlights

## Synthesizer category
Parthenopea implements a **wavetable** type **offline**  synthesizer. ***wavetable*** because Parthenopea works with SoundFont only. ***offline*** because non-interactive, requiring slow rendering steps.

The Parthenopea synth is written using Signal Functions ala Euterpea. 

## (Sweepable) low-pass filter **with resonance**
In Euterpea, neither *filterLowPass* nor *filterLowPassBW* have resonance as a part of input/response, so I developed a Signal Function that works as a State Variable filter. See https://karmafx.net/docs/karmafx_digitalfilters.pdf and https://ccrma.stanford.edu/~jos/svf/svf.pdf .

## Envelopes
To induce note articulation effects, the SoundFont file Authors specify a set of sound envelope parameter values, . (See https://www.synthfont.com/SFSPEC21.PDF).  Difficulties arise from ambiguities in the spec and non-conformance in the artifacts (files). The actual amplitude scaling is accomplished via Euterpea's *envLineSeg* Signal Function. For the interpretation of parameters, we endeavor to:
1. produce a reasonable note
2. honor Author design intent

## Permissive scanning
File scanning must be "fault tolerant" to avoid disqualifying too many otherwise useful SoundFont files and Instruments. This is apparent in the behavior of tools like https://www.polyphone.io/ when opening those files. In Parthenopea, the glitches found are documented in a *Scan Report*.
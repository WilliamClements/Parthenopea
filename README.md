# Parthenopea
Haskell project built on and extending *Euterpea 2* music educational package. Parthenopea adds experimentative orchestration to Euterpea.

The Euterpea user already can connect a synth to select and manually map Instruments to MIDI/GM from their SoundFont file set. However, using heuristics, Parthenopea chooses and maps "for you" the highest quality Instruments.

For more on Euterpea, see https://www.euterpea.com/ and obtain its textbook ***Haskell School of Music***. But Parthenopea doesn't require user to *directly* interface with Euterpea to survey, compare, and apply Instruments from their SoundFont *treasure trove*.

## Building it
Some difficulty here. User needs to know how (e.g. *cabal.project*) to set up local packages in a dependency chain. Also how to debug broken version constraints. https://github.com/georgefst/Euterpea2 is a fork of Euterpea intended to work around some build blockers.

## Batch processor
Parthenopea's *batch processor* opens all MIDI files (\*.mid) and SoundFont files (\*.sf2) found in the current directory. If both types are present, it proceeds to mine for best Instruments and "converts" the MIDI files to music files -- which you can then listen to and share.

Type *pCommand* into the REPL (i.e. *ghci*). Later on, scaling up to many and large SoundFont files, build and use a standalone executable (e.g. *PCommand*) for better performance.  It has been tested with ***500*** SoundFont files totalling ***46 GB*** :astonished: !

## Rendering
Parthenopea is opiniated. As mentioned above, it tries to pick the most suitable SoundFont Instruments to realize each MIDI (GM) instrument. It *scores* Instruments via a combination of empirical stats. All the stats and scores are recorded and provided in a textual *Tournament Report*.

Note that the result obtained when mixing Instruments from *unrelated* SoundFont files won't sound cohesive. But it can be interesting anyway to listen to your tunes *varying* which SoundFont files to draw from.

# Design highlights

## Synthesizer category
Parthenopea implements a **wavetable** type **offline**  synthesizer. ***Wavetable*** because Parthenopea works with SoundFont only. ***Offline*** because non-interactive, requiring slow rendering steps.

The Parthenopea synth is written using Signal Functions ala Euterpea. 

## (Sweepable) low-pass filter **with resonance**
In Euterpea, *filterLowPass* and *filterLowPassBW* Signal Functions are sweepable, but neither provides *resonance* (Q). To address this lack, a Signal Function that is a State Variable filter (SVF) was developed. See https://karmafx.net/docs/karmafx_digitalfilters.pdf and https://ccrma.stanford.edu/~jos/svf/svf.pdf .

## Envelopes
To shape notes for a given Instrument, SoundFont file Authors can specify envelope parameter values. (See https://www.synthfont.com/SFSPEC21.PDF).  Implementation difficulties arose from ambiguities in the spec, non-conformance in the artifacts (files), and, unlike online playing, the lack of a precise key-release onset time.

The runtime amplitude control is via Euterpea's *envLineSeg* Signal Function. Parthenopea first *interpretes* the envelope parameters, endeavoring to:
1. produce a reasonable note
2. (try to) honor Author design intent

## Permissive scanning
File scanning must be "fault tolerant" to avoid disqualifying too many otherwise useful SoundFont files and Instruments. This is apparent in the behavior of tools like https://www.polyphone.io/ when opening those flawed files. In Parthenopea, the glitches encountered and worked around are documented in a *Scan Report*.

## Instrument consolidation
A note quality advantage is achieved by Authors via sampling the **natural** Instrument over many small ranges of Pitch and/or Velocity. Some authors, presumably for convenience, spread these many zones over multiple SoundFont Instruments, each with a manageable zone count. 

Synthesizers would lose that note quality boost **bigtime** if just one *partial* SoundFont Instrument is mapped to given MIDI GM Instrument. So Parthenopea, prior to synthesis, logically combines partial Instruments into one that has all the zones. 

## Array lookup for zone selection
Nominally, SoundFont note synthesizer must **search** through an Instrument's zones for the one whose Pitch and Velocity ranges contain the two incoming note parameters. Parthenopea improves that by computing a cache per Instrument, such that the search is reduced to an array lookup.

## Performance
Don't ask :slightly_frowning_face: ! The bottlenecks are:
1. parsing SoundFont files (https://hackage.haskell.org/package/HCodecs-0.5.2/docs/Codec-SoundFont.html)
2. rendering - executing Signal Function tree

Heavy batch runs have clocked in at ten hours.
# Parthenopea
Haskell project; adds orchestration assistance to Euterpea computer music. Parthenopea chooses and maps "for you" MIDI GMs to the highest quality Instruments.

For more on music educational software and Euterpea, see https://www.euterpea.com/ and obtain its textbook ***Haskell School of Music***. 

## Building it
Some potential difficulty here. User will need to:
1. Set up local (inplace) packages in a dependency chain -- e.g., use *cabal.project*.
2. Debug broken version constraints.

https://github.com/georgefst/Euterpea2 is a fork of Euterpea intended to work around some build blockers.

## Batch processor
Parthenopea's *batch processor* is a command. Here's how it works. Batch opens all MIDI files (\*.mid or \*.midi) and SoundFont files (\*.sf2) found in the current directory. If both types are present, it proceeds to mine for best Instruments and produce music files per midi. User need not *directly* interface with Euterpea to survey, compare, or apply SoundFont Instruments.

In source root directory, do *cabal repl* and type *pCommand*. Better performance with a standalone executable. Capacity has been tested up to ***500*** SoundFont files totalling ***46 GB*** :astonished: !

## Rendering
Parthenopea is opiniated. It tries to pick most suitable SoundFont Instruments to realize each GM. All the stats and scores driving these *tournaments* get printed out. Take a look at *Tournament Report* in ***Sample Reports***.

Note that mixing Instruments from *unrelated* SoundFont files tends to sound off when played. But interesting anyway to listen to your tunes *varying* Instrument mappings.

# Design highlights

## Synthesizer category
Implements a **wavetable** type **offline**  synthesizer. ***Wavetable*** because it is integrated with SoundFont only. ***Offline*** because rendering is not in real time.

The Parthenopea synth is written using Signal Functions ala Euterpea. 

## (Sweepable) low-pass filter **with resonance**
In Euterpea, *filterLowPass* and *filterLowPassBW* Signal Functions are sweepable, but neither provides *resonance* (Q). To address this lack, a Signal Function, a State Variable filter (SVF), was developed. See https://karmafx.net/docs/karmafx_digitalfilters.pdf and https://ccrma.stanford.edu/~jos/svf/svf.pdf .

## Envelopes
SoundFont file Authors, one way or another, specify envelope parameter values for shaping notes. Parthenopea *interprets/modifies* these parameters, then feeds them into Euterpea's *envLineSeg* Signal Function for timed amplitude control. Priorities for the *interpretation* logic are:
1. produce a reasonable note
2. (try to) honor Author design intent

## Permissive scanning
Files taken from the wild have many flaws that could trip us up. Example: unprintable characters in Sample or Instrument names. Tools like https://www.polyphone.io/ still function fine when opening those flawed files. In Parthenopea, glitches encountered and worked around are documented in a *Scan Report*.

## Instrument consolidation
Sampling the **natural** Instrument over many small ranges of Pitch and/or Velocity can give better playback quality. But the Authors often spread these many Zones over multiple SoundFont Instruments, each with a manageable Zone count. Naive implementations, mapping one to one, give bad results. :worried: So. prior to synthesis, *partial* Instruments are in effect combined into one that has all the Zones. 

## Zone selection
Don't like to **search**, at note synthesis time, through an Instrument's Zones to isolate one whose Pitch and Velocity ranges peg the two incoming note parameters. Improved with computed winners' cache -- search is thus reduced to an array lookup.

## Performance
Don't ask :slightly_frowning_face: ! The bottlenecks are:
1. parsing SoundFont files (https://hackage.haskell.org/package/HCodecs-0.5.2/docs/Codec-SoundFont.html)
2. rendering - evaluating Signal Function tree

Heavy batch runs have clocked in at ten hours.
# Parthenopea

Computer music project centered around Euterpea. The new *batch processor* automates orchestration in a sense - applying found SoundFont Instruments to songs and batch-producing sharable music files.

For more on music educational software and Euterpea, see <https://www.euterpea.com> and obtain its textbook ***Haskell School of Music***.

## Usage

All of Euterpea is available to the Haskell programming user for creating or transcribing, and playing, compositions. New *Passages* capability adds better control over dynamics within a song.

For the "production" end, download *PCommand* Windows 64-bit executable from Releases. Invoke it in an empty folder to start cold. It will prompt you to supply Midi files and SoundFonts to run a true *batch* job.

Have fun and be creative :musical_note: with interesting sounds for your instruments. Study sampled sound content via batch processor reports.

## Installation

1. install <https://www.haskell.org/ghcup/> to obtain *cabal* and *ghc*.
2. clone this repository into a primary folder
3. clone, beside that, <https://github.com/georgefst/Euterpea2> (a fork of Euterpea avoiding build blockers).
4. in primary folder, edit provided cabal.project to put in above path to Euterpea2 source
5. "cabal build" to make PCommand (along with Parthenopea lib)

## Batch processor semantics

1. takes input files from, and sends output files to, the runtime start directory
2. some Euterpea-coded sample tunes are provided -- Beethoven's string quartet Opus 131 in C Sharp Minor, the opening fugue, showcases the use of *Passages* dynamics annotation.
3. input:
   - SoundFont files (\*.sf2)
   - MIDI files (\*.mid and/or \*.midi) to be rendered
   - your **Euterpean** compositions
4. output:
   - music files (\*.wav currently supported)
   - report files

Listen to your new song files with initially generated Instrument mappings. Iterate and improve choices.

## Rendering tunes

First of all, do collect **many** SoundFont files. The variety is breathtaking. And PCommand's capacity has been tested up to ***500*** SoundFont files totalling ***46 GB*** :astonished: !

The batch processor conducts *tournaments* to curate the most suitable mined Instruments. Stats and scores driving the result are printed out. Take a look at *Tournament Report* in ***Sample Reports***.

Note that mixing *unrelated* SoundFont Instruments can sound **off** when played. But interesting anyway to listen to your tunes *varying* Instrument mappings.

## Design highlights

### Synthesizer category

Implements a **wavetable** type **offline**  synthesizer. ***Wavetable*** because all SoundFont instruments are sample-based. ***Offline*** because rendering is not in real time.

The *batch processor* synth is written using Signal Functions ala Euterpea.

### (Sweepable) low-pass filter **with resonance**

In Euterpea, *filterLowPass* and *filterLowPassBW* Signal Functions are sweepable, but neither provides *resonance* (Q). To address this lack, a new Signal Function was developed -- namely, a State Variable filter (SVF). See <https://karmafx.net/docs/karmafx_digitalfilters.pdf> and <https://ccrma.stanford.edu/~jos/svf/svf.pdf>. Reference *procSVF* in the code.

### Envelopes

SoundFont file Authors, one way or another, specify envelope parameter values for shaping notes. *batchProcessor* ***interprets/modifies*** the parameters going into Euterpea's *envLineSeg* Signal Function for timed amplitude control. Priorities for the *interpretation* logic are:

1. produce a reasonable note
2. (try to) honor Author design intent

(See *refineEnvelope* in the code)

### Permissive scanning

Files taken from the wild have many flaws that tripped us up at first. Examples:

1. unprintable characters in Sample or Instrument names -- renames the bad identifiers.
2. unusable settings like zero Sample Rate :upside_down_face: -- disqualifies affected Instruments

Tools like <https://www.polyphone.io/> also function fine when opening those flawed files. In *batchProcessor*, glitches encountered and addressed are documented in a *Scan Report*.

### Instrument consolidation

Sampling a **natural** Instrument over many small ranges of Pitch and/or Velocity yields nice spacious sound. But Authors often split Zones among distinct SoundFont Instruments. Naive implementations, mapping Instruments one to one, loses the advantage :worried: if Instrument is distributed. So prior to synthesis, *partial* Instruments are in effect combined into one that has all the Zones. :relieved:

### Zone selection

Don't like to **search**, at note synthesis time, through an Instrument's Zones to isolate one whose Pitch and Velocity ranges peg the two incoming note parameters. Lazily computes winners' cache instead -- search is thus reduced to an array lookup.

(See *Smashing* Module in the code).

### Performance

Don't ask :roll_eyes: ! The bottlenecks are:

1. parsing SoundFont files (<https://hackage.haskell.org/package/HCodecs-0.5.2/docs/Codec-SoundFont.html>)
2. rendering - evaluating Signal Functions modeling the audio clock rate

Heavy batch runs have clocked in at ten hours.

### Why the names?

*Euterpe* is the name of a Muse from Greek mythology. *Parthenope* was a Siren, the daughter of a Muse.

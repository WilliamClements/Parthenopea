# Parthenopea
Multi-faceted entry in the field of music education software. My personal open source project.

Along with its "parent", Euterpea 2, Parthenopea connects music study/composition/play with Haskell functional language programming. (See https://www.euterpea.com/ - obtain the Euterpea textbook ***Haskell School of Music***.)

Overall, the purpose is fun and learning about music and sound.

Parthenopea's strength is experimenting with orchestration. Listen to your compositions played with MIDI Instruments extracted from abundantly available SoundFont files such as "DSoundFontV4.sf2"

Suggested workflow:
1. start in Euterpea in Read-Eval-Print Loop (REPL) *interpreted* mode
2. develop tunes algorithmically, or simply specify all the notes - this is a code-writing step
3. listen to the tunes using *play* function and iterate 
4. run Parthenopea's *compiled* command-line executable in a folder populated with SoundFont files

The software renders each tune to a WAV file, having automatically chosen realizations like "Iowa Violin 3" for MIDI InstrumentNames like *Violin*.

Disclaimer: a mix of Instruments from **multiple** SoundFont files has a result that often sounds less cohesive than you would like. You can fix that as you iterate to final mappings.

Technological notes:
1. Parthenopea employs its own offline SoundFont synth that is native Haskell, Arrows and Signal Functions-based, SoundFont, offline, synthesizer.
2. During execution, Parthenopea emits tabular info about the SoundFont file scanning process and the instrumentation "tournament", in addition to helpful standard output.
3. The command-line utility PConsole has been tested against 48 GB of SoundFont files in one shot, with robust success.
# Setup
Section to be added.

# Parthenopea
Multi-faceted entry in the field of music education software. My personal open source project.

Along with its "parent", Euterpea 2, Parthenopea connects music study/composition/play with Haskell functional language programming. (See https://www.euterpea.com/ - obtain the Euterpea textbook ***Haskell School of Music***.)

Parthenopea's strength, as a Euterpea extension, is orchestration. Listen to your compositions played with a variety of SoundFont/MIDI Instruments.

Suggested workflow:
1. start in Euterpea in Read-Eval-Print Loop (REPL) *interpreted* mode
2. develop tunes algorithmically, or simply specify all the notes - this is a code-writing step
3. listen to the tunes using *play* function and iterate 
4. run Parthenopea's *compiled* command-line executable in a folder populated with SoundFont files
5. listen to the generated WAV files and iterate

Disclaimer: a mix of Instruments from **multiple** unrelated SoundFont files has a result that often sounds less cohesive than you would like. You can fix that as you iterate to final mappings.

Technological notes:
1. Parthenopea employs its own offline SoundFont wavetable synth -- native Haskell, Arrows and Signal Functions-based.
2. During execution, Parthenopea emits tabular info about the SoundFont file scanning process and the instrumentation "tournament".
3. The command-line utility PConsole has been successfully tested against 48 GB of SoundFont files at one shot.
# Setup
1. install GHC and cabal.
2. git clone Parthenopea repo.
3. start command shell.
4. change working directory to PConsole.
5. issue "cabal build"
6. issue "cabal run PConsole"

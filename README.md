# Parthenopea
Multi-faceted entry in the field of music education software. My personal open source project.

Working together with its "runtime", Euterpea 2, Parthenopea connects music study/composition/play with Haskell functional language programming. (See https://www.euterpea.com/ and obtain the Euterpea textbook ***Haskell School of Music***.)

Parthenopea's strength, as a Euterpea extension, is orchestration. Listen to your compositions played with a variety of SoundFont/MIDI Instruments.

Suggested workflow:
1. start in Euterpea in Read-Eval-Print Loop (REPL) *interpreted* mode
2. develop tunes algorithmically, or simply specify all the notes
3. listen to the tunes using *play* function and iterate 
4. your new code could then be placed in Parthenopea/PConsole/Tunes
5. run Parthenopea's *compiled* command-line executable, PConsole, in a folder populated with SoundFont files
6. listen to the generated WAV files of your tunes and iterate

Disclaimer: a mix of Instruments from **unrelated** SoundFont files often sounds less cohesive than you would like. You can fix that as you iterate to final instrument mappings.

Technological notes:
1. Parthenopea employs its own offline SoundFont wavetable synth -- native Haskell, Arrows, and Signal Functions-based.
2. During execution, Parthenopea emits tabular info about the SoundFont file scanning process and the instrumentation "tournament".
3. The command-line utility PConsole has been successfully tested against 48 GB of SoundFont files at one shot.
# Setup
1. install GHC and cabal.
2. git clone Parthenopea repo.
3. start command shell.
4. change working directory from Parthenopea to Parthenopea/PConsole.
5. issue "cabal build"
6. issue "cabal run PConsole"

However, there is currently need to tweak the build of Euterpea2.  Use git clone on https://github.com/georgefst/Euterpea2 , which has the fix built in, to result in the following folder structure:
~/h (for example)
~/h/Parthenopea
~/h/Parthenopea/PConsole
~/h/eut/v2/Euterpea2

Note: ~/h/Parthenopea/cabal.project and ~/h/Parthenopea/PConsole/cabal.project expect above (relative) structure. If you want the HSoM library, I suggest cloning it in 
~/h/eut/hsom/HSoM/



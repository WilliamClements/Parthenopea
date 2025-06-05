# Parthenopea
Haskell library, companion to the great Euterpea 2 - see https://www.euterpea.com/ and buy the book at https://www.amazon.com/Haskell-School-Music-Signals-Symphonies/dp/1108416756 .

Euterpea connects music study/composition/play with Haskell functional language programming.

Parthenopea adds an orchestration capability. It has a native Haskell, Arrows and Signal Functions-based, SoundFont, offline, synthesizer.

Overall, the purpose is fun and learning about music and sound.

But picture this workflow: start in Euterpea in Read-Eval-Print Loop (REPL) interpreted mode. Write Haskell code to do algorithmic composition, or to specify all the notes of a piece, with MIDI. Use the play function with a synthesizer to listen and iterate.

Now move in the production direction with the Parthenopea console app (PConsole) compiled normally. Experiment with Instruments from SoundFont files, and listen to the generated WAV files using Audacity or whatever. 
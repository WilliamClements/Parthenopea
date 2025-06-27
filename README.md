# Parthenopea
Haskell library for working with music. MIDI-based.

Parthenopea interfaces with Euterpea 2. (See https://www.euterpea.com/ and obtain the Euterpea textbook ***Haskell School of Music***.)

Warning: to build successfully, user needs to know *cabal* and be able to troubleshoot dependencies. https://github.com/georgefst/Euterpea2 is a fork of Euterpea intended to facilitate building.

To use just *pCommand*, the batch processor, build your own simple project with following *Main.lhs*:
```
> module Main where
>
> import Parthenopea.SoundFont.Command
>
> main                   :: IO ()
> main                                     = do
>   pCommand
```

Of course, you can fire up GHCi on Parthenopea, and simply type *pCommand*. But that will not perform very well with large or many SoundFont files.

# Implementation notes
The *pCommand* opens MIDI files and SoundFont files found in the current directory. If both types are present, the batch processor will "convert" the MIDI files to WAV files for your listening pleasure. The MIDI "instruments" will be realized using SoundFont Instruments.

Parthenopea has deep support for SoundFont, including synthesis (naturally). Note that a mix of Instruments from **unrelated** SoundFont files won't sound cohesive. But this is all for play, not production!

The synthesizer is written using Signal Functions.

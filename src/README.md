# Source Directory

This directory contains all libraries generated by Quoptimize.
The Quoptimize library has general utilities one might release to Hackage.
The QuoptimizeExe library contains specialized executable code.
For example, Quoptimize would contain all CLI integration code.
This code is important, but should not be client facing.
The Cabal settings declare QuoptimizeExe to be an interal library.

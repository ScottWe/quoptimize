# Executable Directory

The directory contains the Main functions for all Quoptimize executables.
Note that each executable has at least a Main.hs file and a CmdLn.hs file.
The CmdLn.hs file is used for integrating the CmdArg library.
This is because CmdArg requires extra compiler directives.
By separating these files, the directives are limited to CmdArg code.

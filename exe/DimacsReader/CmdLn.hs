-- | Command-line parser for the DIMACS Reader.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module DimacsReader.CmdLn
  ( DimacsReader(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import QuoptimizeExe.CmdLnFlags
  ( def
  , srcFlags
  )
import QuoptimizeExe.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data DimacsReader = DimacsReader { src :: String
                                 } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

dimacsReader :: DimacsReader
dimacsReader = DimacsReader { src = srcFlags def }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO DimacsReader
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "DIMACS Reader"
          desc  = "A command-line interface to test the DIMACS parser."
          ctors = [dimacsReader]

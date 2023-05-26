-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module QuoptimizeExe.CmdLnFlags
  ( def
  , outFlags
  , srcFlags
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import System.Console.CmdArgs
  ( Data
  , Typeable
  , (&=)
  , def
  , typ
  , help
  , typFile
  )

-------------------------------------------------------------------------------
-- * Input/Output Flags.

-- | Returns the flags for the --src argument. The default value is taken as an
-- argument, since flags are impure.
srcFlags :: String -> String
srcFlags x = x &= help "Input source (defaults to stdin)."
               &= typFile

-- | Returns the flags for the --out argument. The default value is taken as an
-- argument, since flags are impure.
outFlags :: String -> String
outFlags x = x &= help "Output destination (defaults to stdout)."
               &= typFile

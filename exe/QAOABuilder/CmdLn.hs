-- | Command-line parser for the QAOA Circuit Builder.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QAOABuilder.CmdLn
  ( QAOABuilder(..)
  , getCmdArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import QuoptimizeExe.CmdLnFlags
  ( def
  , outFlags
  )
import QuoptimizeExe.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data QAOABuilder = QAOABuilder { out :: String
                               } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

qaoaBuilder :: QAOABuilder
qaoaBuilder = QAOABuilder { out = outFlags def }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getCmdArgs :: IO QAOABuilder
getCmdArgs = parseCmdLnArgs title desc ctors
    where title = "QAOA Circuit Builder"
          desc  = "A command-line interface to generate QAOA Quipper circuits."
          ctors = [qaoaBuilder]

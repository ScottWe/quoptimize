-- | Command-line interface for Lattice Surgery Compiler pre-processing.

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import QAOABuilder.CmdLn
  ( QAOABuilder(..)
  , getCmdArgs
  )

-------------------------------------------------------------------------------
-- * Entry Point.

main :: IO ()
main = do
    args <- getCmdArgs
    putStr $ out args ++ "\n"

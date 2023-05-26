-- | Command-line interface for QAOA circuit generation.

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

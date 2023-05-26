-- | Command-line interface to test the DIMACS parser.

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import Quoptimize.CNF.Parser (parseDimacs)
import Quoptimize.CNF.Format
  ( disjunctions
  , fromDimacsFile
  , literalCount
  , maxClauseLen
  )
import QuoptimizeExe.IOUtils
  ( readSrc
  , setLocalToUtf8
  )
import DimacsReader.CmdLn
  ( DimacsReader(..)
  , getCmdArgs
  )

-------------------------------------------------------------------------------
-- * Entry Point.

main :: IO ()
main = do
    setLocalToUtf8
    args             <- getCmdArgs
    (contents, file) <- readSrc $ src args
    case parseDimacs file contents of
        Left err     -> putStrLn err
        Right dimacs -> case fromDimacsFile dimacs of
            Left err  -> putStrLn $ show err
            Right cnf -> do
                putStrLn $ "Literals: " ++ (show $ literalCount cnf)
                putStrLn $ "k-CNF: " ++ (show $ maxClauseLen cnf)
                putStrLn $ "Disjuncts: " ++ (show $ length $ disjunctions cnf)

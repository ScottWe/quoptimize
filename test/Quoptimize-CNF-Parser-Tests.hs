module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Quoptimize.CNF.Language
import Quoptimize.CNF.Parser

-----------------------------------------------------------------------------------------
-- Positive Tests

pos1 = fromJust $ toPosInt 1
pos2 = fromJust $ toPosInt 2
pos3 = fromJust $ toPosInt 3
neg3 = fromJust $ toNegInt (-3)

cnf1 = [PosLit pos1]
cnf2 = [PosLit pos1, PosLit pos2, NegLit neg3]
cnf3 = [PosLit pos2, PosLit pos3]

test_input_1 = "p cnf 1 1\n" ++
               "1 0"

test1 = TestCase (assertEqual "parseDimacs handles the simplest valid program."
                              (Right res :: Either String DimacsFile)
                              (parseDimacs "fp" test_input_1))
    where res = DimacsCNF pos1 pos1 [cnf1]

test_input_2 = "p cnf 3 2\n" ++
               "1 2 -3 0\n" ++
               "2 3 0"

test2 = TestCase (assertEqual "parseDimacs handles a non-trivial case."
                              (Right res :: Either String DimacsFile)
                              (parseDimacs "fp" test_input_2))
    where res = DimacsCNF pos3 pos2 [cnf2, cnf3]

test_input_3 = "c this is a comment\n" ++
               "c this is also a comment\n" ++
               "p cnf 3 2 \n" ++
               "c hello world\n\n\n" ++
               "1 2 -3 0 2 3 0\n\n\n\n\n"

test3 = TestCase (assertEqual "parseDimacs handles edge cases in syntax."
                              (Right res :: Either String DimacsFile)
                              (parseDimacs "fp" test_input_3))
    where res = DimacsCNF pos3 pos2 [cnf2, cnf3]

-----------------------------------------------------------------------------------------
-- Negative Tests

isError :: Either String DimacsFile -> Bool
isError (Left _)  = True
isError (Right _) = False

test_input_4 = "p cnf 0 1\n" ++
               "1 0"

test4 = TestCase (assertBool "parseDimacs detects bad sizes (1/4)."
                             (isError $ parseDimacs "fp" test_input_4))

test_input_5 = "p cnf 1 0\n" ++
               "1 0"

test5 = TestCase (assertBool "parseDimacs detects bad sizes (2/4)."
                             (isError $ parseDimacs "fp" test_input_5))

test_input_6 = "p cnf -1 1\n" ++
               "1 0" 

test6 = TestCase (assertBool "parseDimacs detects bad sizes (3/4)."
                             (isError $ parseDimacs "fp" test_input_6))

test_input_7 = "p cnf 1 -1\n" ++
               "1 0" 

test7 = TestCase (assertBool "parseDimacs detects bad sizes (4/4)."
                             (isError $ parseDimacs "fp" test_input_7))

test_input_8 = "p cnf\n" ++
               "1 1\n" ++
               "1 0\n"

test8 = TestCase (assertBool "parseDimacs detects premature newline in header."
                             (isError $ parseDimacs "fp" test_input_8))

test_input_9 = "p cnf 1 1\n" ++
               "1"

test9 = TestCase (assertBool "parseDimacs detects missing end-of-clause symbol."
                             (isError $ parseDimacs "fp" test_input_9))

test_input_10 = "p cnf 1 1\n" ++
                "0 0\n"

test10 = TestCase (assertBool "parseDimacs detects zeros as literals."
                              (isError $ parseDimacs "fp" test_input_9))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [ TestLabel "Trivial" test1
                                    , TestLabel "General" test2
                                    , TestLabel "Valid_Edge_Cases" test3
                                    , TestLabel "BadSize_1" test4
                                    , TestLabel "BadSize_2" test5
                                    , TestLabel "BadSize_3" test6
                                    , TestLabel "BadSize_4" test7
                                    , TestLabel "Bad_Header" test8
                                    , TestLabel "Bad_EndOfClause" test9
                                    , TestLabel "Bad_ZeroLit" test10
                                    ]

main = defaultMain tests

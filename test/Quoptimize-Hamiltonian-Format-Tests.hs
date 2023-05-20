module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Quoptimize.Hamiltonian.Format

-----------------------------------------------------------------------------------------
-- zeroQDiagHam (on failure)

test1 = TestCase (assertEqual "zeroQDiagHam requires positive values (1/2)."
                              (Nothing :: Maybe QDiagHam)
                              (zeroQDiagHam 0))

test2 = TestCase (assertEqual "zeroQDiagHam requires positive values (2/2)."
                              (Nothing :: Maybe QDiagHam)
                              (zeroQDiagHam (-1)))

-----------------------------------------------------------------------------------------
-- zeroQDiagHam (on success)

zero1 = fromJust $ zeroQDiagHam 1
zero2 = fromJust $ zeroQDiagHam 2
zero3 = fromJust $ zeroQDiagHam 3

test3 = TestCase (assertEqual "zeroQDiagHam sets dimensions correctly (1/3)."
                              1
                              (dim zero1))

test4 = TestCase (assertEqual "zeroQDiagHam sets dimensions correctly (2/3)."
                              2
                              (dim zero2))

test5 = TestCase (assertEqual "zeroQDiagHam sets dimensions correctly (3/3)."
                              3
                              (dim zero3))

test6 = TestCase (assertEqual "zeroQDiagHam leaves trace zero (1/3)."
                              0
                              (trace zero1))

test7 = TestCase (assertEqual "zeroQDiagHam leaves trace zero (2/3)."
                              0
                              (trace zero2))

test8 = TestCase (assertEqual "zeroQDiagHam leaves trace zero (3/3)."
                              0
                              (trace zero3))

-----------------------------------------------------------------------------------------
-- Default Trace

test9 = TestCase (assertEqual "trace defaults to zero for 1x1."
                              (Just 0 :: Maybe Rational)
                              (getComp 0 zero1))

test10 = TestCase (assertEqual "trace defaults to zero for 2x2 (1/2)."
                               (Just 0 :: Maybe Rational)
                               (getComp 0 zero2))

test11 = TestCase (assertEqual "trace defaults to zero for 2x2 (2/2)."
                               (Just 0 :: Maybe Rational)
                               (getComp 1 zero2))

test12 = TestCase (assertEqual "trace defaults to zero for 3x3 (1/3)."
                               (Just 0 :: Maybe Rational)
                               (getComp 0 zero3))

test13 = TestCase (assertEqual "trace defaults to zero for 3x3 (2/3)."
                               (Just 0 :: Maybe Rational)
                               (getComp 1 zero3))

test14 = TestCase (assertEqual "trace defaults to zero for 3x3 (3/3)."
                               (Just 0 :: Maybe Rational)
                               (getComp 2 zero3))

-----------------------------------------------------------------------------------------
-- Sequence of setComp Calls

h1x1_1 = fromJust $ setComp 0 5 zero1
h2x2_1 = fromJust $ setComp 1 3 zero2
h3x3_1 = fromJust $ setComp 0 5 zero3
h3x3_2 = fromJust $ setComp 2 3 h3x3_1
h3x3_3 = fromJust $ setComp 0 3 h3x3_2

test15 = TestCase (assertEqual "setComp preserves dimension (1/5)."
                               1
                               (dim h1x1_1))

test16 = TestCase (assertEqual "setComp preserves dimension (2/5)."
                               2
                               (dim h2x2_1))

test17 = TestCase (assertEqual "setComp preserves dimension (3/5)."
                               3
                               (dim h3x3_1))

test18 = TestCase (assertEqual "setComp preserves dimension (4/5)."
                               3
                               (dim h3x3_2))

test19 = TestCase (assertEqual "setComp preserves dimension (5/5)."
                               3
                               (dim h3x3_3))

test20 = TestCase (assertEqual "setComp updates trace correctly (1/5)."
                               5
                               (trace h1x1_1))

test21 = TestCase (assertEqual "setComp updates trace correctly (2/5)."
                               3
                               (trace h2x2_1))

test22 = TestCase (assertEqual "setComp updates trace correctly (3/5)."
                               5
                               (trace h3x3_1))

test23 = TestCase (assertEqual "setComp updates trace correctly (4/5)."
                               8
                               (trace h3x3_2))

test24 = TestCase (assertEqual "setComp updates trace correctly (5/5)."
                               6
                               (trace h3x3_3))

-----------------------------------------------------------------------------------------
-- getComp after setComp

test25 = TestCase (assertEqual "setComp sets elements correctly in 1x1."
                               (Just 5 :: Maybe Rational)
                               (getComp 0 h1x1_1))

test26 = TestCase (assertEqual "setComp sets elements correctly in 2x2 (1/2)."
                               (Just 0 :: Maybe Rational)
                               (getComp 0 h2x2_1))

test27 = TestCase (assertEqual "setComp sets elements correctly in 2x2 (2/2)."
                               (Just 3 :: Maybe Rational)
                               (getComp 1 h2x2_1))

test28 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (1/9)."
                               (Just 5 :: Maybe Rational)
                               (getComp 0 h3x3_1))

test29 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (2/9)."
                               (Just 0 :: Maybe Rational)
                               (getComp 1 h3x3_1))

test30 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (3/9)."
                               (Just 0 :: Maybe Rational)
                               (getComp 2 h3x3_1))

test31 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (4/9)."
                               (Just 5 :: Maybe Rational)
                               (getComp 0 h3x3_2))

test32 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (5/9)."
                               (Just 0 :: Maybe Rational)
                               (getComp 1 h3x3_2))

test33 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (6/9)."
                               (Just 3 :: Maybe Rational)
                               (getComp 2 h3x3_2))

test34 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (7/9)."
                               (Just 3 :: Maybe Rational)
                               (getComp 0 h3x3_3))

test35 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (8/9)."
                               (Just 0 :: Maybe Rational)
                               (getComp 1 h3x3_3))

test36 = TestCase (assertEqual "setComp sets elements correctly in 3x3 (9/9)."
                               (Just 3 :: Maybe Rational)
                               (getComp 2 h3x3_3))

-----------------------------------------------------------------------------------------
-- getComp (failure)

test37 = TestCase (assertEqual "getComp rejects bounds to low."
                               (Nothing :: Maybe Rational)
                               (getComp (-1) h3x3_3))

test38 = TestCase (assertEqual "getComp rejects bounds to high."
                               (Nothing :: Maybe Rational)
                               (getComp 5 h3x3_3))

-----------------------------------------------------------------------------------------
-- setComp (failure)

test39 = TestCase (assertEqual "setComp rejects bounds to low."
                               (Nothing :: Maybe QDiagHam)
                               (setComp (-1) 0 h3x3_3))

test40 = TestCase (assertEqual "setComp rejects bounds to high."
                               (Nothing :: Maybe QDiagHam)
                               (setComp 5 0 h3x3_3))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [ TestLabel "zeroQDiagHam_OOB_1" test1
                                    , TestLabel "zeroQDiagHam_OOB_2" test2
                                    , TestLabel "zeroQDiagHam_Dim_1" test3
                                    , TestLabel "zeroQDiagHam_Dim_2" test4
                                    , TestLabel "zeroQDiagHam_Dim_3" test5
                                    , TestLabel "zeroQDiagHam_Trace_1" test6
                                    , TestLabel "zeroQDiagHam_Trace_2" test7
                                    , TestLabel "zeroQDiagHam_Trace_3" test8
                                    , TestLabel "zeroQDiagHam_Diag_1x1" test9
                                    , TestLabel "zeroQDiagHam_Diag_2x2_1" test10
                                    , TestLabel "zeroQDiagHam_Diag_2x2_2" test11
                                    , TestLabel "zeroQDiagHam_Diag_3x3_1" test12
                                    , TestLabel "zeroQDiagHam_Diag_3x3_2" test13
                                    , TestLabel "zeroQDiagHam_Diag_3x3_3" test14
                                    , TestLabel "setComp_Dim_1" test15
                                    , TestLabel "setComp_Dim_2" test16
                                    , TestLabel "setComp_Dim_3" test17
                                    , TestLabel "setComp_Dim_4" test18
                                    , TestLabel "setComp_Dim_5" test19
                                    , TestLabel "setComp_Trace_1" test20
                                    , TestLabel "setComp_Trace_2" test21
                                    , TestLabel "setComp_Trace_3" test22
                                    , TestLabel "setComp_Trace_4" test23
                                    , TestLabel "setComp_Trace_5" test24
                                    , TestLabel "getComp_1x1" test25
                                    , TestLabel "getComp_2x2_1" test26
                                    , TestLabel "getComp_2x2_2" test27
                                    , TestLabel "getComp_3x3_1" test28
                                    , TestLabel "getComp_3x3_2" test29
                                    , TestLabel "getComp_3x3_3" test30
                                    , TestLabel "getComp_3x3_4" test31
                                    , TestLabel "getComp_3x3_5" test32
                                    , TestLabel "getComp_3x3_6" test33
                                    , TestLabel "getComp_3x3_7" test34
                                    , TestLabel "getComp_3x3_8" test35
                                    , TestLabel "getComp_3x3_9" test36
                                    , TestLabel "getComp_Under" test37
                                    , TestLabel "getComp_Over" test38
                                    , TestLabel "setComp_Under" test39
                                    , TestLabel "setComp_Over" test40
                                    ]

main = defaultMain tests

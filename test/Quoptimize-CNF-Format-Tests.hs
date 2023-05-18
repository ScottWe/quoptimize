module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Quoptimize.CNF.Format

-----------------------------------------------------------------------------------------
-- omitPolarity

test1 =  TestCase (assertEqual "omitPolarity supports negative strings."
                              "Hello"
                              (omitPolarity $ Negative "Hello"))

test2 = TestCase (assertEqual "omitPolarity supports negative naturals."
                              5
                              (omitPolarity $ Negative 5))

test3 = TestCase (assertEqual "omitPolarity supports positive strings."
                              "World"
                              (omitPolarity $ Positive "World"))

test4 = TestCase (assertEqual "omitPolarity supports positive naturals."
                              10
                              (omitPolarity $ Positive 10))

-----------------------------------------------------------------------------------------
-- emptyCnfSat (on failure)

test5 =  TestCase (assertEqual "emptyCnfSat requires positive values (1/2)."
                              (Nothing :: Maybe CNFSAT)
                              (emptyCnfSat 0))

test6 =  TestCase (assertEqual "emptyCnfSat requires positive values (2/2)."
                              (Nothing :: Maybe CNFSAT)
                              (emptyCnfSat (-1)))

-----------------------------------------------------------------------------------------
-- emptyCnfSat (on success)

empty1 = fromJust $ emptyCnfSat 1
empty2 = fromJust $ emptyCnfSat 2
empty3 = fromJust $ emptyCnfSat 5

test7 = TestCase (assertEqual "emptyCnfSat sets literal count correctly (1/3)."
                              1
                              (literalCount empty1))

test8 = TestCase (assertEqual "emptyCnfSat sets literal count correctly (2/3)."
                              2
                              (literalCount empty2))

test9 = TestCase (assertEqual "emptyCnfSat sets literal count correctly (3/3)."
                              5
                              (literalCount empty3))

test10 = TestCase (assertEqual "emptyCnfSat leaves disjunctions empty (1/3)."
                               []
                               (disjunctions empty1))

test11 = TestCase (assertEqual "emptyCnfSat leaves disjunctions empty (2/3)."
                               []
                               (disjunctions empty2))

test12 = TestCase (assertEqual "emptyCnfSat leaves disjunctions empty (3/3)."
                               []
                               (disjunctions empty3))

-----------------------------------------------------------------------------------------
-- addDisjunction (on success)

disjunct1 = [Positive 1, Positive 4]
disjunct2 = [Negative 0, Negative 2]
disjunct3 = [Positive 0, Negative 1, Positive 2]

cnf1 = fromJust $ addDisjunction disjunct1 empty3
cnf2 = fromJust $ addDisjunction [] cnf1
cnf3 = fromJust $ addDisjunction disjunct2 cnf2
cnf4 = fromJust $ addDisjunction [] cnf3
cnf5 = fromJust $ addDisjunction disjunct3 cnf4

test13 = TestCase (assertEqual "addDisjunction ignores empty clauses (1/2)."
                               cnf1
                               cnf2)

test14 = TestCase (assertEqual "addDisjunction ignores empty clauses (2/2)."
                               cnf3
                               cnf4)

test15 = TestCase (assertEqual "addDisjunction adds non-empty clauses (1/3)."
                               [disjunct1]
                               (disjunctions cnf1))

test16 = TestCase (assertEqual "addDisjunction adds non-empty clauses (2/3)."
                               [disjunct2, disjunct1]
                               (disjunctions cnf3))

test17 = TestCase (assertEqual "addDisjunction adds non-empty clauses (3/3)."
                               [disjunct3, disjunct2, disjunct1]
                               (disjunctions cnf5))

test18 = TestCase (assertEqual "addDisjunction preserves literal count (1/3)."
                               (literalCount empty3)
                               (literalCount cnf1))

test19 = TestCase (assertEqual "addDisjunction preserves literal count (2/3)."
                               (literalCount cnf2)
                               (literalCount cnf3))

test20 = TestCase (assertEqual "addDisjunction preserves literal count (3/3)."
                               (literalCount cnf4)
                               (literalCount cnf5))

-----------------------------------------------------------------------------------------
-- addDisjunction (on failure)

test21 = TestCase (assertEqual "addDisjunction rejects out-of-bounds literals (1/4)."
                               (Nothing :: Maybe CNFSAT)
                               (addDisjunction d cnf5))
    where d = [Positive 0, Negative 7, Positive 1]

test22 = TestCase (assertEqual "addDisjunction rejects out-of-bounds literals (2/4)."
                               (Nothing :: Maybe CNFSAT)
                               (addDisjunction d cnf5))
    where d = [Positive 0, Negative 3, Positive 10, Negative 0, Positive 1]

test23 = TestCase (assertEqual "addDisjunction rejects out-of-bounds literals (3/4)."
                               (Nothing :: Maybe CNFSAT)
                               (addDisjunction d cnf5))
    where d = [Positive 0, Negative 3, Negative (-7), Positive 1]

test24 = TestCase (assertEqual "addDisjunction rejects out-of-bounds literals (4/4)."
                               (Nothing :: Maybe CNFSAT)
                               (addDisjunction d empty3))
    where d = [Positive 0, Negative 3, Negative (-7), Positive 1]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [ TestLabel "omitPolarity_Neg_1" test1
                                    , TestLabel "omitPolarity_Neg_2" test2
                                    , TestLabel "omitPolarity_Pos_1" test3
                                    , TestLabel "omitPolarity_Pos_2" test4
                                    , TestLabel "emptyCnfSat_OOB_1" test5
                                    , TestLabel "emptyCnfSat_OOB_2" test6
                                    , TestLabel "emptyCnfSat_LitCT_1" test7
                                    , TestLabel "emptyCnfSat_LitCt_2" test8
                                    , TestLabel "emptyCnfSat_LitCt_3" test9
                                    , TestLabel "emptyCnfSat_Disjuncts_1" test10
                                    , TestLabel "emptyCnfSat_Disjuncts_2" test11
                                    , TestLabel "emptyCnfSat_Disjuncts_3" test12
                                    , TestLabel "addDisjunction_Empty_1" test13
                                    , TestLabel "addDisjunction_Empty_2" test14
                                    , TestLabel "addDisjunction_NonEmpty_1" test15
                                    , TestLabel "addDisjunction_NonEmpty_2" test16
                                    , TestLabel "addDisjunction_NonEmpty_3" test17
                                    , TestLabel "addDisjunction_LitCt_1" test18
                                    , TestLabel "addDisjunction_LitCt_2" test19
                                    , TestLabel "addDisjunction_LitCt_3" test20
                                    , TestLabel "addDisjunction_OOB_1" test21
                                    , TestLabel "addDisjunction_OOB_2" test22
                                    , TestLabel "addDisjunction_OOB_3" test23
                                    , TestLabel "addDisjunction_OOB_4" test24
                                    ]

main = defaultMain tests

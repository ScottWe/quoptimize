module Main where

import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Quoptimize.CNF.Language

-----------------------------------------------------------------------------------------
-- PosInt (in-bounds)

test1 = TestCase (assertEqual "PosInt works for positive integers (1/3)."
                              5
                              (fromPosInt $ fromJust $ toPosInt 5))

test2 = TestCase (assertEqual "PosInt works for positive integers (2/3)."
                              10
                              (fromPosInt $ fromJust $ toPosInt 10))

test3 = TestCase (assertEqual "PosInt works for positive integers (3/3)."
                              538
                              (fromPosInt $ fromJust $ toPosInt 538))

-----------------------------------------------------------------------------------------
-- PosInt (out-of-bounds)

test4 = TestCase (assertEqual "PosInt rejects negative integers (1/3)."
                              Nothing
                              (toPosInt (-5)))

test5 = TestCase (assertEqual "PosInt rejects negative integers (2/3)."
                              Nothing
                              (toPosInt (-10)))

test6 = TestCase (assertEqual "PosInt rejects negative integers (3/3)."
                              Nothing
                              (toPosInt (-538)))

-----------------------------------------------------------------------------------------
-- NegInt (in-bounds)

test7 = TestCase (assertEqual "NegInt works for negative integers (1/3)."
                              (-5)
                              (fromNegInt $ fromJust $ toNegInt (-5)))

test8 = TestCase (assertEqual "NegInt works for negative integers (2/3)."
                              (-10)
                              (fromNegInt $ fromJust $ toNegInt (-10)))

test9 = TestCase (assertEqual "NegInt works for negative integers (3/3)."
                              (-538)
                              (fromNegInt $ fromJust $ toNegInt (-538)))

-----------------------------------------------------------------------------------------
-- NegInt (out-of-bounds)

test10 = TestCase (assertEqual "NegInt rejects positive integers (1/3)."
                               Nothing
                               (toNegInt 5))

test11 = TestCase (assertEqual "NegInt rejects positive integers (2/3)."
                               Nothing
                               (toNegInt 10))

test12 = TestCase (assertEqual "NegInt rejects positive integers (3/3)."
                               Nothing
                               (toNegInt 538))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [ TestLabel "PosInt_InBounds_1" test1
                                    , TestLabel "PosInt_InBounds_2" test2
                                    , TestLabel "PosInt_InBounds_3" test3
                                    , TestLabel "PosInt_OOB_1" test4
                                    , TestLabel "PosInt_OOB_2" test5
                                    , TestLabel "PosInt_OOB_3" test6
                                    , TestLabel "NegInt_InBounds_1" test7
                                    , TestLabel "NegInt_InBounds_2" test8
                                    , TestLabel "NegInt_InBounds_3" test9
                                    , TestLabel "NegInt_OOB_1" test10
                                    , TestLabel "NegInt_OOB_2" test11
                                    , TestLabel "NegInt_OOB_3" test12
                                    ]

main = defaultMain tests

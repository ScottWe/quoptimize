module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quoptimize.Hamiltonian.Index

-----------------------------------------------------------------------------------------
-- Tests length zero solutions.

index_xxxx = nullBinaryIndex

test1 = TestCase (assertEqual "Null solutions have length zero."
                              0
                              (getBinaryLength index_xxxx))

test2 = TestCase (assertEqual "Null solutions evaluate to zero."
                              0
                              (evaluateIndex index_xxxx))

-----------------------------------------------------------------------------------------
-- Tests length one solutions.

index_xxx0 = addVar index_xxxx False
index_xxx1 = addVar index_xxxx True

test3 = TestCase (assertEqual "One var solutions have length one (1/2)."
                              1
                              (getBinaryLength index_xxx0))

test4 = TestCase (assertEqual "One var solutions have length one (2/2)."
                              1
                              (getBinaryLength index_xxx1))

test5 = TestCase (assertEqual "One var solutions evaluate correctly (1/2)."
                              0
                              (evaluateIndex index_xxx0))

test6 = TestCase (assertEqual "One var solutions evaluate correctly (2/2)."
                              1
                              (evaluateIndex index_xxx1))

-----------------------------------------------------------------------------------------
-- Tests length two solutions.

index_xx00 = addVar index_xxx0 False
index_xx01 = addVar index_xxx0 True
index_xx10 = addVar index_xxx1 False
index_xx11 = addVar index_xxx1 True

test7 = TestCase (assertEqual "Two var solutions have length two (1/4)."
                              2
                              (getBinaryLength index_xx00))

test8 = TestCase (assertEqual "Two var solutions have length two (2/4)."
                              2
                              (getBinaryLength index_xx01))

test9 = TestCase (assertEqual "Two var solutions have length two (3/4)."
                              2
                              (getBinaryLength index_xx10))

test10 = TestCase (assertEqual "Two var solutions have length two (4/4)."
                               2
                               (getBinaryLength index_xx11))

test11 = TestCase (assertEqual "Two var solutions evaluate correctly (1/4)."
                               0
                               (evaluateIndex index_xx00))

test12 = TestCase (assertEqual "Two var solutions evaluate correctly (2/4)."
                               1
                               (evaluateIndex index_xx01))

test13 = TestCase (assertEqual "Two var solutions evaluate correctly (3/4)."
                               2
                               (evaluateIndex index_xx10))

test14 = TestCase (assertEqual "Two var solutions evaluate correctly (4/4)."
                               3
                               (evaluateIndex index_xx11))

-----------------------------------------------------------------------------------------
-- Tests length three solutions.

index_x000 = addVar index_xx00 False
index_x001 = addVar index_xx00 True
index_x010 = addVar index_xx01 False
index_x011 = addVar index_xx01 True
index_x100 = addVar index_xx10 False
index_x101 = addVar index_xx10 True
index_x110 = addVar index_xx11 False
index_x111 = addVar index_xx11 True

test15 = TestCase (assertEqual "Three var solutions have length three (1/8)."
                               3
                               (getBinaryLength index_x000))

test16 = TestCase (assertEqual "Three var solutions have length three (2/8)."
                               3
                               (getBinaryLength index_x001))

test17 = TestCase (assertEqual "Three var solutions have length three (3/8)."
                               3
                               (getBinaryLength index_x010))

test18 = TestCase (assertEqual "Three var solutions have length three (4/8)."
                               3
                               (getBinaryLength index_x011))

test19 = TestCase (assertEqual "Three var solutions have length three (5/8)."
                               3
                               (getBinaryLength index_x100))

test20 = TestCase (assertEqual "Three var solutions have length three (6/8)."
                               3
                               (getBinaryLength index_x101))

test21 = TestCase (assertEqual "Three var solutions have length three (7/8)."
                               3
                               (getBinaryLength index_x110))

test22 = TestCase (assertEqual "Three var solutions have length three (8/8)."
                               3
                               (getBinaryLength index_x111))

test23 = TestCase (assertEqual "Three var solutions evaluate correctly (1/8)."
                               0
                               (evaluateIndex index_x000))

test24 = TestCase (assertEqual "Three var solutions evaluate correctly (2/8)."
                               1
                               (evaluateIndex index_x001))

test25 = TestCase (assertEqual "Three var solutions evaluate correctly (3/8)."
                               2
                               (evaluateIndex index_x010))

test26 = TestCase (assertEqual "Three var solutions evaluate correctly (4/8)."
                               3
                               (evaluateIndex index_x011))

test27 = TestCase (assertEqual "Three var solutions evaluate correctly (5/8)."
                               4
                               (evaluateIndex index_x100))

test28 = TestCase (assertEqual "Three var solutions evaluate correctly (6/8)."
                               5
                               (evaluateIndex index_x101))

test29 = TestCase (assertEqual "Three var solutions evaluate correctly (7/8)."
                               6
                               (evaluateIndex index_x110))

test30 = TestCase (assertEqual "Three var solutions evaluate correctly (8/8)."
                               7
                               (evaluateIndex index_x111))

-----------------------------------------------------------------------------------------
-- Tests length four solutions.

index_0000 = addVar index_x000 False
index_0001 = addVar index_x000 True
index_0010 = addVar index_x001 False
index_0011 = addVar index_x001 True
index_0100 = addVar index_x010 False
index_0101 = addVar index_x010 True
index_0110 = addVar index_x011 False
index_0111 = addVar index_x011 True
index_1000 = addVar index_x100 False
index_1001 = addVar index_x100 True
index_1010 = addVar index_x101 False
index_1011 = addVar index_x101 True
index_1100 = addVar index_x110 False
index_1101 = addVar index_x110 True
index_1110 = addVar index_x111 False
index_1111 = addVar index_x111 True

test31 = TestCase (assertEqual "Four var solutions have length four (1/16)."
                               4
                               (getBinaryLength index_0000))

test32 = TestCase (assertEqual "Four var solutions have length four (2/16)."
                               4
                               (getBinaryLength index_0001))

test33 = TestCase (assertEqual "Four var solutions have length four (3/16)."
                               4
                               (getBinaryLength index_0010))

test34 = TestCase (assertEqual "Four var solutions have length four (4/16)."
                               4
                               (getBinaryLength index_0011))

test35 = TestCase (assertEqual "Four var solutions have length four (5/16)."
                               4
                               (getBinaryLength index_0100))

test36 = TestCase (assertEqual "Four var solutions have length four (6/16)."
                               4
                               (getBinaryLength index_0101))

test37 = TestCase (assertEqual "Four var solutions have length four (7/16)."
                               4
                               (getBinaryLength index_0110))

test38 = TestCase (assertEqual "Four var solutions have length four (8/16)."
                               4
                               (getBinaryLength index_0111))

test39 = TestCase (assertEqual "Four var solutions have length four (9/16)."
                               4
                               (getBinaryLength index_1000))

test40 = TestCase (assertEqual "Four var solutions have length four (10/16)."
                               4
                               (getBinaryLength index_1001))

test41 = TestCase (assertEqual "Four var solutions have length four (11/16)."
                               4
                               (getBinaryLength index_1010))

test42 = TestCase (assertEqual "Four var solutions have length four (12/16)."
                               4
                               (getBinaryLength index_1011))

test43 = TestCase (assertEqual "Four var solutions have length four (13/16)."
                               4
                               (getBinaryLength index_1100))

test44 = TestCase (assertEqual "Four var solutions have length four (14/16)."
                               4
                               (getBinaryLength index_1101))

test45 = TestCase (assertEqual "Four var solutions have length four (15/16)."
                               4
                               (getBinaryLength index_1110))

test46 = TestCase (assertEqual "Four var solutions have length four (16/16)."
                               4
                               (getBinaryLength index_1111))

test47 = TestCase (assertEqual "Four var solutions evaluate correctly (1/16)."
                               0
                               (evaluateIndex index_0000))

test48 = TestCase (assertEqual "Four var solutions evaluate correctly (2/16)."
                               1
                               (evaluateIndex index_0001))

test49 = TestCase (assertEqual "Four var solutions evaluate correctly (3/16)."
                               2
                               (evaluateIndex index_0010))

test50 = TestCase (assertEqual "Four var solutions evaluate correctly (4/16)."
                               3
                               (evaluateIndex index_0011))

test51 = TestCase (assertEqual "Four var solutions evaluate correctly (5/16)."
                               4
                               (evaluateIndex index_0100))

test52 = TestCase (assertEqual "Four var solutions evaluate correctly (6/16)."
                               5
                               (evaluateIndex index_0101))

test53 = TestCase (assertEqual "Four var solutions evaluate correctly (7/16)."
                               6
                               (evaluateIndex index_0110))

test54 = TestCase (assertEqual "Four var solutions evaluate correctly (8/16)."
                               7
                               (evaluateIndex index_0111))

test55 = TestCase (assertEqual "Four var solutions evaluate correctly (9/16)."
                               8
                               (evaluateIndex index_1000))

test56 = TestCase (assertEqual "Four var solutions evaluate correctly (10/16)."
                               9
                               (evaluateIndex index_1001))

test57 = TestCase (assertEqual "Four var solutions evaluate correctly (11/16)."
                               10
                               (evaluateIndex index_1010))

test58 = TestCase (assertEqual "Four var solutions evaluate correctly (12/16)."
                               11
                               (evaluateIndex index_1011))

test59 = TestCase (assertEqual "Four var solutions evaluate correctly (13/16)."
                               12
                               (evaluateIndex index_1100))

test60 = TestCase (assertEqual "Four var solutions evaluate correctly (14/16)."
                               13
                               (evaluateIndex index_1101))

test61 = TestCase (assertEqual "Four var solutions evaluate correctly (15/16)."
                               14
                               (evaluateIndex index_1110))

test62 = TestCase (assertEqual "Four var solutions evaluate correctly (16/16)."
                               15
                               (evaluateIndex index_1111))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [ TestLabel "snull_len" test1
                                    , TestLabel "snull_val" test2
                                    , TestLabel "s0_len" test3
                                    , TestLabel "s1_len" test4
                                    , TestLabel "s0_val" test5
                                    , TestLabel "s1_val" test6
                                    , TestLabel "s00_len" test7
                                    , TestLabel "s01_len" test8
                                    , TestLabel "s10_len" test9
                                    , TestLabel "s11_len" test10
                                    , TestLabel "s00_val" test11
                                    , TestLabel "s01_val" test12
                                    , TestLabel "s10_val" test13
                                    , TestLabel "s11_val" test14
                                    , TestLabel "s000_len" test15
                                    , TestLabel "s001_len" test16
                                    , TestLabel "s010_len" test17
                                    , TestLabel "s011_len" test18
                                    , TestLabel "s100_len" test19
                                    , TestLabel "s101_len" test20
                                    , TestLabel "s110_len" test21
                                    , TestLabel "s111_len" test22
                                    , TestLabel "s000_var" test23
                                    , TestLabel "s001_var" test24
                                    , TestLabel "s010_var" test25
                                    , TestLabel "s011_var" test26
                                    , TestLabel "s100_var" test27
                                    , TestLabel "s101_var" test28
                                    , TestLabel "s110_var" test29
                                    , TestLabel "s111_var" test30
                                    , TestLabel "s0000_len" test31
                                    , TestLabel "s0001_len" test32
                                    , TestLabel "s0010_len" test33
                                    , TestLabel "s0011_len" test34
                                    , TestLabel "s0100_len" test35
                                    , TestLabel "s0101_len" test36
                                    , TestLabel "s0110_len" test37
                                    , TestLabel "s0111_len" test38
                                    , TestLabel "s1000_len" test39
                                    , TestLabel "s1001_len" test40
                                    , TestLabel "s1010_len" test41
                                    , TestLabel "s1011_len" test42
                                    , TestLabel "s1100_len" test43
                                    , TestLabel "s1101_len" test44
                                    , TestLabel "s1110_len" test45
                                    , TestLabel "s1111_len" test46
                                    , TestLabel "s0000_var" test47
                                    , TestLabel "s0001_var" test48
                                    , TestLabel "s0010_var" test49
                                    , TestLabel "s0011_var" test50
                                    , TestLabel "s0100_var" test51
                                    , TestLabel "s0101_var" test52
                                    , TestLabel "s0110_var" test53
                                    , TestLabel "s0111_var" test54
                                    , TestLabel "s1000_var" test55
                                    , TestLabel "s1001_var" test56
                                    , TestLabel "s1010_var" test57
                                    , TestLabel "s1011_var" test58
                                    , TestLabel "s1100_var" test59
                                    , TestLabel "s1101_var" test60
                                    , TestLabel "s1110_var" test61
                                    , TestLabel "s1111_var" test62
                                    ]

main = defaultMain tests

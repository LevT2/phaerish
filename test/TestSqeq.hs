module TestSqeq (testSqeq) where

import SqSolution (SqSolution (..))
import Sqeq (solveSquare)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testSqeq :: TestTree
testSqeq =
  testGroup
    "test solveSquare"
    [ testCase "constant - infinity" $
        solveSquare 0 0 0 @?= Inf,
      testCase "constant - none" $
        solveSquare 0 0 12 @?= None,
      testCase "linear - one root" $
        solveSquare 0 2 1 @?= OneRoot (-1 / 2),
      testCase "quadratic - none" $
        solveSquare 1 1 12 @?= None,
      testCase "quadratic - one root" $
        solveSquare 1 2 1 @?= OneRoot ((-2) / 2 * 1),
      testCase "quadratic - two roots" $
        solveSquare 1 2 (-1) @?= TwoRoots (sqrt 2 - 1) (-1 - sqrt 2)
    ]

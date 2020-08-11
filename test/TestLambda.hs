module TestLambda (testLambda) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import LExpr (LExpr(..))

main :: IO ()
main = defaultMain testLambda

testLambda :: TestTree
testLambda = testGroup "test LExpr" [mvptests, extendedtests]

mvptests :: TestTree
mvptests =
  testGroup
    "basic LExpr cases"
    [ testCase "variable" $
        show (Variable "x") @?= "x",
      testCase "application" $
        show (Apply (Variable "x") (Variable "y")) @?= "(x y)",
      testCase "abstraction" $
        show (Lambda "x" (Variable "e")) @?= "λx.e"
    ]

prettyTestCase :: String -> LExpr -> TestTree
prettyTestCase prettyOut expression =
  testCase prettyOut $
    show expression @?= prettyOut

extendedtests :: TestTree
extendedtests =
  testGroup
    "all examples"
    [ prettyTestCase "(λx.x) y" $
        Apply (Lambda "x" (Variable "x")) (Variable "y")
        ,
      prettyTestCase "λx.(y z)" $
        Lambda "x" (Apply (Variable "y") (Variable "z"))
        ,
      prettyTestCase "(a b c)" $
        Apply (Apply (Variable "a") (Variable "b")) (Variable "c")
        ,
      prettyTestCase "(λxy.z)" $
        Lambda "x" (Lambda "y" (Variable "z"))
    ]

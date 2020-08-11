module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import TestSqeq (testSqeq)
import TestLambda (testLambda)

main :: IO ()
main = defaultMain $
    testGroup "all" [testSqeq, testLambda]


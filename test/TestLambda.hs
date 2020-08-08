import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type VarName = String

data LExpr
  = Variable VarName -- variable
  | Apply LExpr LExpr -- function application
  | Lambda VarName LExpr -- Lambda abstraction
  deriving (Eq)

instance (Show LExpr) where
  show (Variable a) = a
  show (Apply (Lambda x e1) e2) = "(" ++ show (Lambda x e1) ++ ") " ++ show e2
  show (Apply (Apply e1 e2) e3) = "(" ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
  show (Apply e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lambda x (Lambda y e)) = "(λ" ++ x ++ y ++ "." ++ show e ++ ")"
  show (Lambda x e) = "λ" ++ x ++ "." ++ show e

main :: IO ()
main = defaultMain alltests

alltests :: TestTree
alltests = testGroup "all" [mvptests, extendedtests]

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

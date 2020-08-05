import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data LExpr
  = Variable String -- variable
  | Apply LExpr LExpr -- function application
  | Lambda String LExpr -- Lambda abstraction
  deriving (Eq)

show' :: LExpr -> String
show' (Variable a) = a
show' (Apply e1 e2) = "(" ++ show' e1 ++ " " ++ show' e2 ++ ")"
show' (Lambda x e) = "(\\" ++ x ++ " -> " ++ show' e ++ ")"

instance Show LExpr where
  show = show'

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
      testCase "application" $
        show (Lambda "x" (Variable "e")) @?= "(\\x -> e)"
    ]

extendedtests :: TestTree
extendedtests =
  testGroup
    "all examples"
    [ testCase "..." $
        show (Apply (Lambda "x" (Variable "x")) (Variable "y")) @?= "((\\x -> x) y)",
      testCase "..." $
        show (Lambda "x" (Apply (Variable "y") (Variable "z"))) @?= "(\\x -> (y z))",
      testCase "..." $
        show (Lambda "x" (Lambda "y" (Variable "z"))) @?= "(\\x -> (\\y -> z))",
      testCase "..." $
        show (Apply (Apply (Variable "a") (Variable "b")) (Variable "c")) @?= "((a b) c)"   
    ]


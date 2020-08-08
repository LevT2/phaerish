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
  show (Apply (Lambda x y ) e)  = "(" ++ show (Lambda x y )  ++ ") " ++ show e
  show (Apply (Apply e1 e2) e3) = "(" ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")" 
  show (Apply e1 e2)  = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lambda x (Lambda y z)) = "(\\" ++ x ++ y ++ "->" ++ show z ++ ")"                     
  show (Lambda y z)  = "\\" ++ y ++ "->" ++ show z 

λ = "\\"
dot = "->"

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
        show (Lambda "x" (Variable "e")) @?= "\\x->e"
    ]

extendedtests :: TestTree
extendedtests =
  testGroup
    "all examples"
    [ testCase "(λx->x) y" $
        show (Apply (Lambda "x" (Variable "x")) (Variable "y")) @?= "(\\x->x) y"
        ,
      testCase "(λx->y z)" $
        show (Lambda "x" (Apply (Variable "y") (Variable "z"))) @?= "\\x->(y z)"
        ,
      testCase "(a b c)" $
        show (Apply (Apply (Variable "a") (Variable "b")) (Variable "c")) @?= "(a b c)"
        ,
      testCase "(λxy->z)" $
        show (Lambda "x" (Lambda "y" (Variable "z"))) @?= "(" ++ λ ++ "xy" ++ dot ++ "z)"        
    ]

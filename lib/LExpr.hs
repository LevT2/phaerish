module LExpr where

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



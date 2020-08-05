module LExpr (LExpr(..)) where

data LExpr
    = Variable String         -- variable
    | Apply LExpr LExpr       -- function application
    | Lambda String LExpr     -- Lambda abstraction
    deriving (Eq) 


show' :: LExpr -> String
show' (Variable a) = a
show' (Apply e1 e2) = "(" ++ show' e1 ++ " " ++ show' e2 ++ ")"                   
show' (Lambda x e) = "(λ " ++ x ++ ". " ++ show' e ++ ")"    

instance Show LExpr where
  show = show'


module Sqeq (solveSquare, SqSolution(..), showSqSolution) where

data SqSolution = Inf | OneRoot Double | TwoRoots Double Double | None
  deriving (Eq, Show)

showSqSolution :: SqSolution -> String
showSqSolution sqs = 
  case sqs of
    None             ->  "No results"
    OneRoot a        ->  "Single root is :" ++ Prelude.show a
    TwoRoots a b     ->  "Two roots are :" ++ Prelude.show a ++ ", " ++ Prelude.show b
    Inf              ->  "Infinite results" 

-- | Solve equation of kind @a x² + b x + c = 0@
solveSquare :: Double -> Double -> Double -> SqSolution
solveSquare a b c
  | a == 0, b == 0, c == 0  = Inf
  | a == 0, b == 0          = None
  | d < 0                   = None
  | a == 0                  = OneRoot (- c / b)
  | d == 0                  = OneRoot (-b / 2 * a)
  | d > 0                   = TwoRoots ((- b + sqrt d) / (2 * a)) ((- b - sqrt d) / (2 * a))
  where
    d = b * b - 4 * a * c
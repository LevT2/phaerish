module SqSolution (SqSolution(..), showSqSolution) where

data SqSolution = Inf | OneRoot Double | TwoRoots Double Double | None
  deriving (Eq, Show)

showSqSolution :: SqSolution -> String
showSqSolution sqs = 
  case sqs of
    None             ->  "No results"
    OneRoot a        ->  "Single root is :" ++ Prelude.show a
    TwoRoots a b     ->  "Two roots are :" ++ Prelude.show a ++ ", " ++ Prelude.show b
    Inf              ->  "Infinite results" 
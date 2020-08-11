module Sqeq (solveSquare) where
  
import           SqSolution          (SqSolution(..))

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
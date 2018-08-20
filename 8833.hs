-- Solve the 8833 puzzle

-- "With two 8's, two 3's, the basic arithmetic operations (+, -, *, /)
-- and parentheses, make the number 24. For example, I can make
-- 16 = 8 * (8 - 3 - 3)."

-- Define a simple RPN calculator, then use it to filter all
-- permutations of the four digits and three binary operations for
-- those that evaluate to the desired result.

import Data.List

-- In RPN, each token can be a number, an operator, or an Error token
data RPN a = N a | Plus | Minus | Times | Divide | Error
  deriving (Show, Eq)

allOps = [Plus, Minus, Times, Divide]

-- Lift a value to an RPN number
toRPN :: a -> RPN a
toRPN x = N x

-- applyTop: Apply the first RPN operation from the list of inputs.
-- Return the resulting stack.
--
-- The Divide and Minus cases look 'backwards' because the inputs are
-- pushed onto the stack in the order that they are processed.
--
-- An operator is left on the stack when there are too few arguments
-- for it (it matches the final pattern).  Numbers also match the
-- final pattern (they evaluate to themselves).
applyTop :: (Fractional a, Eq a) => [RPN a] -> RPN a -> [RPN a]
applyTop ((N n):(N m):rest) Plus   = N (n + m):rest
applyTop ((N n):(N m):rest) Minus  = N (m - n):rest
applyTop ((N n):(N m):rest) Times  = N (n * m):rest
applyTop ((N 0):(N m):rest) Divide = Error:rest
applyTop ((N n):(N m):rest) Divide = N (m / n):rest
applyTop stack token = token:stack

-- Fully evaluate a given RPN input
evalRPN :: (Fractional a, Eq a) => [RPN a] -> [RPN a]
evalRPN input = foldl' applyTop [] input

-- Find all RPN sequences evaluating to a given result
solvePuzzle result numbers =
  filter (\x -> (evalRPN x) == [toRPN result]) expressions
  where
    opList = sequence $ replicate (length numbers - 1) allOps
    numbersLifted = map toRPN numbers
    symbols = [numbersLifted ++ ops | ops <- opList]
    expressions = nub (concatMap permutations symbols)

main = print $ solvePuzzle 24 (map toRational [8,8,3,3])

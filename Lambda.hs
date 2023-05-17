module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars = 
    \s -> case s of
        (Variable x) -> [x]
        (Function x e) -> delete x (free_vars e)
        (Application e1 e2) -> union (free_vars e1) (free_vars e2)

-- TODO 1.2. reduce a redex
-- reduce e1 x e2 : inlocuieste x din e1 cu e2
reduce :: Expr -> String -> Expr -> Expr
reduce = 
    \e1 x e2 -> case e1 of
        (Variable y) -> if x == y then e2 else e1
        (Function y e) -> if x == y then e1  else Function y (reduce e x e2)
        (Application e3 e4) -> Application (reduce e3 x e2) (reduce e4 x e2)

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN = undefined

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN = undefined

reduceAllN :: Expr -> [Expr]
reduceAllN = undefined

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA = undefined

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA = undefined

reduceAllA :: Expr -> [Expr]
reduceAllA = undefined

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined

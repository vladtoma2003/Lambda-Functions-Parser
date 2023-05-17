{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars =
    \s -> case s of
        (Variable x) -> [x]
        (Function x e) -> delete x (free_vars e)
        (Application e1 e2) -> free_vars e1 `union` free_vars e2

-- TODO 1.2. reduce a redex
-- reduce e1 x e2 : inlocuieste x din e1 cu e2
-- body[x/param] : inlocuieste x din body cu param
-- e1 x e2 -> e1[x/e2] -> \x.e1 e2
reduce :: Expr -> String -> Expr -> Expr
reduce =
    -- \x.y e2
    \e1 x e2 ->
    case e1 of
        (Variable y) ->
            if x == y then e2 else e1
    -- \x.(\y.e) e2 -> (\y.e)[x/e2] -> \y.e[x/e2] -> \x.\y.e[x/e2]; \x.e[x/e2]
        (Function y e) ->
            if x == y then e1 else Function y (reduce e x e2)
    -- \x.(e3 e4) e2 -> (e3 e4)[x/e2] -> e3[x/e2] e4[x/e2] -> \x.(e3[x/e2] e4[x/e2]) e2 -> \x.(e3[x/e2] e4[x/e2]) e2
    -- trebuie inlocuit y din e3 cu a
        (Application e3 e4) ->
            Application (reduce e3 x e2) (reduce e4 x e2)

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

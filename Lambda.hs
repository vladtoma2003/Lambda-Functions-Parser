{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
module Lambda where

import Expr
import Data.List
import Language.Haskell.TH (Info(VarI))

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
reduce (Variable y) x e2 =
            if x == y then e2 else Variable y
    -- \x.(\y.e) e2 -> (\y.e)[x/e2] -> \y.e[x/e2] -> \x.\y.e[x/e2]; \x.e[x/e2]

reduce (Function y e) x e2 =
            if x == y then (Function y e) 
            else if(notElem y (free_vars e2)) then
                    Function y (reduce e x e2)
                else
                    reduce (Function z (reduce e y (Variable z))) x e2
                    where z = head (concatMap (\n -> sequence (take n (tails ['a'..'z']))) [1..] 
                                \\ (union (free_vars (Function y e)) (free_vars e2)))
                                -- generez o lista infinita de valori libere si iau capul acesteia
                                -- pentru a inlocui variabilele libere (pt capturarea variabilelor).

    -- \x.(e3 e4) e2 -> (e3 e4)[x/e2] -> e3[x/e2] e4[x/e2] -> \x.(e3[x/e2] e4[x/e2]) e2 -> \x.(e3[x/e2] e4[x/e2]) e2
    -- trebuie inlocuite variabilele din e1 care sunt libere in e2 cu unele noi, nefolosite
reduce (Application e3 e4) x e2 =
            Application (reduce e3 x e2) (reduce e4 x e2)


-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation (de la stanga la dreapta)
stepN :: Expr -> Expr
stepN = 
    \s -> case s of
        (Variable x) -> Variable x
        (Function x e) -> Function x e
        (Application e1 e2) -> case e1 of
            (Variable x) -> Application e1 e2
            (Function x e) -> reduce e x e2
            (Application e3 e4) -> Application (stepN e1) e2

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

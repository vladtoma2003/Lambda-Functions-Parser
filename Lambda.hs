{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}
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
        (Macro x) -> [x]

-- TODO 1.2. reduce a redex
-- reduce e1 x e2 : inlocuieste x din e1 cu e2
-- body[x/param] : inlocuieste x din body cu param
-- e1 x e2 -> e1[x/e2] -> \x.e1 e2
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable y) x e2 =
            if x == y then e2 else Variable y
    -- \x.(\y.e) e2 -> (\y.e)[x/e2] -> \y.e[x/e2] -> \x.\y.e[x/e2]; \x.e[x/e2]

reduce (Function y e) x e2
  | x == y = (Function y e)
  | (notElem y (free_vars e2)) = Function y (reduce e x e2)
  | otherwise = reduce (Function z (reduce e y (Variable z))) x e2
  where
      z = head
            (concatMap (\ n -> sequence (take n (tails ['a' .. 'z']))) [1 .. ]
               \\ (union (free_vars (Function y e)) (free_vars e2)))
                                -- generez o lista infinita de valori libere si iau capul acesteia
                                -- pentru a inlocui variabilele libere (pt capturarea variabilelor).

    -- \x.(e3 e4) e2 -> (e3 e4)[x/e2] -> e3[x/e2] e4[x/e2] -> \x.(e3[x/e2] e4[x/e2]) e2 -> \x.(e3[x/e2] e4[x/e2]) e2
    -- trebuie inlocuite variabilele din e1 care sunt libere in e2 cu unele noi, nefolosite
reduce (Application e3 e4) x e2 =
            Application (reduce e3 x e2) (reduce e4 x e2)

reduce (Macro y) x e2 = Macro y

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation (de la stanga la dreapta)
stepN :: Expr -> Expr
stepN (Variable x) = Variable x
stepN (Function x e) = Function x (stepN e) 
stepN expr
    | Application (Function x e1) e2 <- expr = reduce e1 x e2
    | Application e1 e2 <- expr = 
        if stepN e1 == e1 then Application e1 (stepN e2) 
        else Application (stepN e1) e2
stepN (Macro x) = Macro x

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN expr 
    | stepN expr == expr = expr
    | otherwise = reduceN (stepN expr)

reduceAllN :: Expr -> [Expr]
reduceAllN expr
    | stepN expr == expr = [expr]
    | otherwise = expr : reduceAllN (stepN expr)

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Variable x) = Variable x
stepA (Function x e) = Function x (stepA e)
stepA expr
    | Application (Function x e1) e2 <- expr = case e2 of
        Variable y -> reduce e1 x e2
        Function y e3 -> reduce e1 x e2
        Application e3 e4 -> Application (Function x e1) (stepA e2)
    | Application e1 e2 <- expr = if stepA e1 == e1 then Application e1 (stepA e2)
        else Application (stepA e1) e2
stepA (Macro x) = Macro x

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expr
    | stepA expr == expr = expr
    | otherwise = reduceA (stepA expr)

reduceAllA :: Expr -> [Expr]
reduceAllA expr
    | stepA expr == expr = [expr]
    | otherwise = expr : reduceAllA (stepA expr)

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros l e =
    case e of
        (Macro x) -> case lookup x l of
            Just e1 -> evalMacros l e1
            Nothing -> Macro x
        (Variable x) -> Variable x
        (Function x e1) -> Function x (evalMacros l e1)
        (Application e1 e2) -> Application (evalMacros l e1) (evalMacros l e2)
    
    

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy [] = []
evalCode strategy ((Evaluate x):xs) = strategy (evalExpr strategy x) : evalCode strategy xs
evalCode strategy ((Assign x e):xs) = evalCode (\e1 -> if e1 == Macro x then e else strategy e1) xs

evalExpr :: (Expr -> Expr) -> Expr -> Expr
evalExpr strategy e = 
    case e of 
        (Variable x) -> strategy (Variable x)
        (Function x e1) -> Function x (evalExpr strategy e1)
        (Application e1 e2) -> Application (evalExpr strategy e1) (evalExpr strategy e2)
        (Macro x) -> strategy (Macro x)

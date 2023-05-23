module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char (isAlphaNum, isAlpha)
import Data.Data (isAlgType)
import GHC.Base (Alternative((<|>)))
import GHC.Exts.Heap.Closures (GenClosure(var, APClosure))
import GHC.Read (paren)

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)

    -- (>>=) = 
    mp >>= f =
        Parser $ \s ->
            case parse mp s of
                Nothing -> Nothing
                Just (x, s) -> parse (f x) s

instance Applicative Parser where
    pf <*> px = do
        f <- pf
        f <$> px
    pure = return

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr cv = 
    case parse initialParser cv of
        Nothing -> error "parse error"
        Just (e, _) -> e

initialParser :: Parser Expr
initialParser = applicationParser <|> atomParser

atomParser :: Parser Expr
atomParser = functionParser <|> variableParser <|> parenParser

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p1) <|> (Parser p2) = Parser (\s -> case p1 s of
                                Nothing -> p2 s
                                ok -> ok)

charParser :: Char -> Parser Char
charParser c = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just (c, xs) else Nothing)

predicateParser :: (Char -> Bool) -> Parser Char -- aplica o functie 
predicateParser p = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing)

starParser :: Parser a -> Parser [a] -- asta
starParser p = plusParser p <|> return []  -- <|> alternativa

plusParser :: Parser a -> Parser [a]
plusParser p =
    do  x <- p
        xs <- starParser p
        return (x:xs)

varParser :: Parser String -- asta
varParser =
    do  x <- predicateParser isAlpha
        xs <- many (predicateParser isAlphaNum)
        return (x:xs)

variableParser :: Parser Expr
variableParser =
    do  x <- varParser
        return (Variable x)

functionParser :: Parser Expr
functionParser =
    do  charParser '\\'
        x <- varParser
        charParser '.'
        e <- initialParser
        return (Function x e)

applicationParser :: Parser Expr
applicationParser =
    do  e1 <- atomParser
        rest <- many (charParser ' ' *> atomParser)
        return (foldl Application e1 rest)
    
parenParser :: Parser Expr
parenParser =
    do  charParser '('
        e <- applicationParser
        charParser ')'
        return e

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = undefined

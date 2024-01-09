module Parser (tokenize, isNegativeNumber, parseExpr, parseExpr', parseAddSub, parseAddSub', parseMulDiv, parseMulDiv', parsePower, parsePower', parseFactor )where

import Expr
import Data.Char

data Token = TNumber Float | TOperator Char | TParen Char deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize = tokenize' False
  where
    tokenize' :: Bool -> String -> [Token]
    tokenize' _ [] = []
    tokenize' isNegation (c:cs)
      | isDigit c || c == '.' = let (number, rest) = span (\x -> isDigit x || x == '.') (c:cs)
                                in TNumber (read number) : tokenize' False rest
      | elem c "+*/^()" = TOperator c : tokenize' (c == '(') cs
      | c == '-' = handleNegative isNegation cs
      | isSpace c = tokenize' isNegation cs
      | otherwise = error $ "Invalid character in expression: " ++ [c]

    handleNegative :: Bool -> String -> [Token]
    handleNegative isNegation cs
      | isNegation && not (null cs) && (isDigit (head cs) || head cs == '.') = 
          let (number, rest) = span (\x -> isDigit x || x == '.') cs
          in TNumber (read ('-' : number)) : tokenize' False rest
      | otherwise = TOperator '-' : tokenize' False cs



isNegativeNumber :: String -> Bool
isNegativeNumber (c:cs) = c == '-' && (not (null cs) && (isDigit (head cs) || head cs == '.'))
isNegativeNumber _ = False

parseExpr :: String -> Expr
parseExpr xs = parseExpr' $ tokenize xs

parseExpr' :: [Token] -> Expr
parseExpr' tokens = 
    let (expr, []) = parseAddSub tokens
    in expr

parseAddSub :: [Token] -> (Expr, [Token])
parseAddSub tokens = 
    let (term, rest) = parseMulDiv tokens
    in parseAddSub' term rest

parseAddSub' :: Expr -> [Token] -> (Expr, [Token])
parseAddSub' leftExpr (TOperator op:rest) | elem op "+-" =
    let (rightExpr, rest') = parseMulDiv rest
        newExpr = case op of
            '+' -> Add leftExpr rightExpr
            '-' -> Subtract leftExpr rightExpr
    in parseAddSub' newExpr rest'
parseAddSub' leftExpr rest = (leftExpr, rest)


parseMulDiv :: [Token] -> (Expr, [Token])
parseMulDiv tokens = 
    let (factor, rest) = parsePower tokens
    in parseMulDiv' factor rest

parseMulDiv' :: Expr -> [Token] -> (Expr, [Token])
parseMulDiv' leftExpr (TOperator op:rest) | elem op "*/" =
    let (rightExpr, rest') = parsePower rest
        newExpr = case op of
            '*' -> Multiply leftExpr rightExpr
            '/' -> Divide leftExpr rightExpr
    in parseMulDiv' newExpr rest'
parseMulDiv' leftExpr rest = (leftExpr, rest)

parsePower :: [Token] -> (Expr, [Token])
parsePower tokens =
    let (base, rest) = parseFactor tokens
    in parsePower' base rest

parsePower' :: Expr -> [Token] -> (Expr, [Token])
parsePower' leftExpr (TOperator '^':rest) =
    let (rightExpr, rest') = parseFactor rest
        newExpr = Power leftExpr rightExpr
    in parsePower' newExpr rest'
parsePower' leftExpr rest = (leftExpr, rest)

parseFactor :: [Token] -> (Expr, [Token])
parseFactor ((TNumber n):rest) = (Number n, rest)
parseFactor (TOperator '(':rest) =
    let (expr, rest') = parseAddSub rest
    in case rest' of
        (TOperator ')':rest'') -> (expr, rest'')
        _ -> error "Missing closing parenthesis"
parseFactor _ = error "Invalid syntax"

module Main (main, formatResult)where

import System.Environment (getArgs)
import Parser (parseExpr)
import Evaluator (evaluate)
import Numeric (showFFloat)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [expression] -> print . formatResult . evaluate $ parseExpr expression
        _            -> putStrLn "Error"

formatResult :: Float -> String
formatResult n = showFFloat (Just 2) n ""

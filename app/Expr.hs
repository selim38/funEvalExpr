module Expr 
    ( Expr(..)
    ) where
        
data Expr = Number Float
          | Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Power Expr Expr
          deriving (Show)

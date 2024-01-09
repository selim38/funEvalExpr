module Evaluator (evaluate) where

import Expr

evaluate :: Expr -> Float
evaluate (Number n) = n
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Subtract a b) = evaluate a - evaluate b
evaluate (Multiply a b) = evaluate a * evaluate b
evaluate (Divide a b) = evaluate a / evaluate b
evaluate (Power a b) = evaluate a ** evaluate b

module Semantic where

data Node
  = IntNode Int
  | UnOp String Node
  | BinOp String Node Node
  deriving (Show)

evaluate :: Node -> Int
evaluate (IntNode n) = n
evaluate (UnOp "+" a) = evaluate a
evaluate (UnOp "-" a) = -evaluate a
evaluate (BinOp "+" a b) = evaluate a + evaluate b
evaluate (BinOp "-" a b) = evaluate a - evaluate b
evaluate (BinOp "*" a b) = evaluate a * evaluate b
evaluate (BinOp "/" a (IntNode 0)) = error "[Semantic] Division by zero"
evaluate (BinOp "/" a b) = evaluate a `div` evaluate b
evaluate (BinOp "**" a b)
  | evaluate b < 0 = error "[Semantic] Negative exponent not supported"
  | otherwise = evaluate a ^ evaluate b

module Semantic where

-- import Control.Monad (liftM2)

-- data NodeKind = BinOp | UnOp | IntVal deriving (Eq, Show)

-- type NodeValue = Either String Int

-- data Node = Node
--   { kind :: NodeKind,
--     value :: NodeValue,
--     children :: [Node]
--   }
--   deriving (Eq, Show)

-- sumValue :: NodeValue -> NodeValue -> NodeValue
-- sumValue = liftM2 (+)

-- subtractValue :: NodeValue -> NodeValue -> NodeValue
-- subtractValue eitherA eitherB = do
--   a <- eitherA
--   b <- eitherB
--   return (a - b)

-- negValue :: NodeValue -> NodeValue
-- negValue eitherA = do
--   a <- eitherA
--   return (-a)

-- evaluateBinOp :: Node -> NodeValue
-- evaluateBinOp node
--   | value node == Left "+" = evaluate leftChild `sumValue` evaluate rightChild
--   | value node == Left "-" = evaluate leftChild `subtractValue` evaluate rightChild
--   | otherwise = error "[Semantic] invalid BinOp node"
--   where
--     leftChild = head (children node)
--     rightChild = last (children node)

-- evaluateUnOp :: Node -> NodeValue
-- evaluateUnOp node
--   | value node == Left "+" = evaluate child
--   | value node == Left "-" = negValue $ evaluate child
--   | otherwise = error "[Semantic] invalid UnOp node"
--   where
--     child = head (children node)

-- evaluateIntVal :: Node -> NodeValue
-- evaluateIntVal = value

-- evaluate :: Node -> NodeValue
-- evaluate node
--   | kind node == BinOp = evaluateBinOp node
--   | kind node == UnOp = evaluateUnOp node
--   | kind node == IntVal = evaluateIntVal node

data Node
  = IntNode Int
  | UnOp String [Node]
  | BinOp String [Node]
  deriving (Show)

evaluate :: Node -> Int
evaluate (IntNode n) = n
evaluate (UnOp "+" [a]) = evaluate a
evaluate (UnOp "-" [a]) = -evaluate a
evaluate (BinOp "+" [a, b]) = evaluate a + evaluate b
evaluate (BinOp "-" [a, b]) = evaluate a - evaluate b
evaluate (BinOp "*" [a, b]) = evaluate a * evaluate b
evaluate (BinOp "/" [a, IntNode 0]) = error "[Semantic] Division by zero"
evaluate (BinOp "/" [a, b]) = evaluate a `div` evaluate b
evaluate (BinOp "**" [a, b])
  | evaluate b < 0 = error "[Semantic] Negative exponent not supported"
  | otherwise = evaluate a ^ evaluate b
evaluate (BinOp "**" [a, b]) = evaluate a ^ evaluate b

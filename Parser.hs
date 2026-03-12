module Parser
  ( run,
  )
where

import Data.Bits (Bits (xor))
import Lexer (Lexer (..), getNext)
import Semantic
import Token (Kind (..), Token (..), Value (..))

evalNextKind :: Lexer -> Token.Kind
evalNextKind lex = Token.kind (next lex)

evalNextValue :: Lexer -> Token.Value
evalNextValue lex = Token.value (next lex)

evalValueInt :: Token.Value -> Int
evalValueInt (Right val) = val

run :: String -> Node
run source =
  -- error $ show $ parseExpression initialLexer
  -- parseExpression initialLexer
  -- leftRotate $ parseExpression initialLexer
  -- error $ show $ reverseTree (parseExpression initialLexer)
  -- error $ show $ rotateN (treeDepth (parseExpression initialLexer) - 1) (parseExpression initialLexer)
  rotateN (treeDepth (parseExpression initialLexer) - 1) (parseExpression initialLexer)
  where
    -- error $ show $ (parseExpression initialLexer)

    -- error $ show $ rotateN (treeDepth (parseExpression initialLexer)) (parseExpression initialLexer)

    -- error $ show $ leftRotate (parseExpression initialLexer)

    invalidLexer = Lexer source (-1) (Token Token.EOF (Left "\0")) -- EOF represents initial non existent token for passing to evalNext to get actual first token
    initialLexer = getNext invalidLexer

treeDepth :: Node -> Int
treeDepth (IntNode _) = 1
treeDepth (UnOp _ children) = 1 + maxChildDepth children
treeDepth (BinOp _ children) = 1 + maxChildDepth children

-- Helper to handle the list of children safely
maxChildDepth :: [Node] -> Int
maxChildDepth [] = 0
maxChildDepth xs = maximum (map treeDepth xs)

rotateN :: Int -> Node -> Node
rotateN n node
  | n <= 0 = node
  | otherwise = rotateN (n - 1) (leftRotate node)

leftRotate :: Node -> Node
leftRotate (BinOp opP [lP, BinOp opC [lC, rC]]) =
  BinOp opC [BinOp opP [lP, lC], rC]
leftRotate n = n

reverseTree :: Node -> Node
reverseTree (IntNode val) = IntNode val
reverseTree (UnOp op children) =
  UnOp op (reverse (map reverseTree children))
reverseTree (BinOp op children) =
  BinOp op (reverse (map reverseTree children))

parseExpression :: Lexer -> Node
parseExpression lex
  | nextLexKind == Token.PLUS =
      Semantic.BinOp "+" [leftNode, rightNode]
  | nextLexKind == Token.MINUS =
      Semantic.BinOp "-" [leftNode, rightNode]
  | otherwise = leftNode
  where
    nextLex = getNext lex
    nextLexKind = evalNextKind nextLex
    leftNode = parseTerm lex
    rightNode = parseExpression $ getNext nextLex

parseTerm :: Lexer -> Node
parseTerm lex
  | nextLexKind == Token.MULT =
      Semantic.BinOp "*" [leftNode, rightNode]
  | nextLexKind == Token.DIV =
      Semantic.BinOp "/" [leftNode, rightNode]
  | otherwise = leftNode
  where
    nextLex = getNext lex
    nextLexKind = evalNextKind nextLex
    leftNode = parseFactor lex
    rightNode = parseExpression $ getNext nextLex

parseFactor :: Lexer -> Node
parseFactor lex
  | nextKind == Token.INT = Semantic.IntNode valueInt
  | nextKind == Token.PLUS = Semantic.UnOp "+" [rightNode]
  | nextKind == Token.MINUS = Semantic.UnOp "-" [rightNode]
  -- | nextKind == Token.OPEN_PAR = Semantic.UnOp "-" [rightNode]
  | otherwise = error $ "[Parser] [Factor] expected INT at position " ++ show (Lexer.position lex) ++ ", got " ++ show (evalNextKind lex)
  where
    nextKind = evalNextKind lex
    valueInt = evalValueInt $ evalNextValue lex
    nextLex = getNext lex
    rightNode = parseFactor $ getNext nextLex

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
  error $ show $ parseExpression initialLexer
  -- parseExpression initialLexer
  where
    invalidLexer = Lexer source (-1) (Token Token.EOF (Left "\0")) -- EOF represents initial non existent token for passing to evalNext to get actual first token
    initialLexer = getNext invalidLexer

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
    rightNode = parseTerm $ getNext nextLex

parseFactor :: Lexer -> Node
parseFactor lex
  | nextKind == Token.INT = Semantic.IntNode valueInt
  -- | nextKind == Token.PLUS = Semantic.UnOp "+" [rightNode]
  -- | nextKind == Token.MINUS = Semantic.UnOp "-" [rightNode]
  -- | nextKind == Token.OPEN_PAR = Semantic.UnOp "-" [rightNode]
  | otherwise = error $ "[Parser] [Factor] expected INT at position " ++ show (Lexer.position lex) ++ ", got " ++ show (evalNextKind lex)
  where
    nextKind = evalNextKind lex
    valueInt = evalValueInt $ evalNextValue lex
    nextLex = getNext lex
    -- rightNode = parseFactor $ getNext nextLex

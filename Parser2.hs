module Parser2
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
run source
  | evalNextKind finalLex == Token.EOF = node
  | otherwise = error $ "[Parser] Unexpected token at end of input: " ++ show (evalNextKind finalLex)
  where
    invalidLexer = Lexer source (-1) (Token Token.EOF (Left "\0")) -- EOF represents initial non existent token for passing to evalNext to get actual first token
    initialLexer = getNext invalidLexer
    (finalLex, node) = parseExpression initialLexer

parseExpression :: Lexer -> (Lexer, Node)
parseExpression lex = parseExpressionLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseTerm lex

parseExpressionLoop :: Lexer -> Node -> (Lexer, Node)
parseExpressionLoop lex leftNode
  | nextKind == Token.PLUS = parseExpressionLoop nextLex (Semantic.BinOp "+" [leftNode, rightNode])
  | nextKind == Token.MINUS = parseExpressionLoop nextLex (Semantic.BinOp "-" [leftNode, rightNode])
  | otherwise = (lex, leftNode)
  where
    nextKind = evalNextKind lex
    (nextLex, rightNode) = parseTerm (getNext lex)

parseTerm :: Lexer -> (Lexer, Node)
parseTerm lex = parseTermLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseFactor lex

parseTermLoop :: Lexer -> Node -> (Lexer, Node)
parseTermLoop lex leftNode
  | nextKind == Token.MULT = parseTermLoop nextLex (Semantic.BinOp "*" [leftNode, rightNode])
  | nextKind == Token.DIV = parseTermLoop nextLex (Semantic.BinOp "/" [leftNode, rightNode])
  | otherwise = (lex, leftNode)
  where
    nextKind = evalNextKind lex
    (nextLex, rightNode) = parseFactor (getNext lex)

parseFactor :: Lexer -> (Lexer, Node)
parseFactor lex
  | nextKind == Token.PLUS = (nextLex, Semantic.UnOp "+" [rightNode])
  | nextKind == Token.MINUS = (nextLex, Semantic.UnOp "-" [rightNode])
  | nextKind == Token.OPEN_PAR =
      if evalNextKind lexAfterOpen == Token.CLOSE_PAR
        then (getNext lexAfterOpen, exprNode) -- Consume CLOSE_PAR
        else error $ "[Parser] Expected CLOSE_PAR at position: " ++ show (Lexer.position lexAfterOpen)
  | nextKind == Token.INT =
      (getNext lex, Semantic.IntNode (evalValueInt (evalNextValue lex)))
  | otherwise =
      error $ "[Parser] Expected INT, got: " ++ show nextKind ++ ", at position: " ++ show (Lexer.position lex)
  where
    nextKind = evalNextKind lex
    (nextLex, rightNode) = parseFactor (getNext lex)
    (lexAfterOpen, exprNode) = parseExpression (getNext lex)
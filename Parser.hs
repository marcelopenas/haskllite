module Parser
  ( run,
  )
where

import Data.Bits (Bits (xor))
import Lexer (Lexer (..), getNext, next)
import Semantic
import Token

run :: String -> Node
run source
  | next finalLex == Token.EOF = node
  | otherwise = error $ "[Parser] Unexpected token at end of input: " ++ show (next finalLex)
  where
    invalidLexer = Lexer source (-1) Token.EOF -- EOF represents initial non existent token for passing to evalNext to get actual first token
    initialLexer = getNext invalidLexer
    (finalLex, node) = parseExpression initialLexer

parseExpression :: Lexer -> (Lexer, Node)
parseExpression lex = case nextToken of
  Token.PLUS -> expected
  Token.MINUS -> expected
  _ -> parseExpressionLoop nextLex leftNode
  where
    nextToken = next lex
    (nextLex, leftNode) = parseTerm lex
    expected = error $ "[Parser] Expected INT, got: " ++ show nextToken ++ ", at position: " ++ show (Lexer.position lex)

parseExpressionLoop :: Lexer -> Node -> (Lexer, Node)
parseExpressionLoop lex leftNode = case nextToken of
  Token.PLUS -> parseExpressionLoop nextLex (Semantic.BinOp "+" leftNode rightNode) -- FIXME call parseExpression
  Token.MINUS -> parseExpressionLoop nextLex (Semantic.BinOp "-" leftNode rightNode) -- FIXME call parseExpression
  _ -> (lex, leftNode)
  where
    nextToken = next lex
    (nextLex, rightNode) = parseTerm (getNext lex)

parseTerm :: Lexer -> (Lexer, Node)
parseTerm lex = parseTermLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseFactor lex

parseTermLoop :: Lexer -> Node -> (Lexer, Node)
parseTermLoop lex leftNode = case nextToken of
  Token.MULT -> parseTermLoop nextLex (Semantic.BinOp "*" leftNode rightNode) -- FIXME call parseTerm
  Token.DIV -> parseTermLoop nextLex (Semantic.BinOp "/" leftNode rightNode) -- FIXME call parseTerm
  _ -> (lex, leftNode)
  where
    nextToken = next lex
    (nextLex, rightNode) = parseFactor (getNext lex)

parseFactor :: Lexer -> (Lexer, Node)
parseFactor lex = case nextToken of
  Token.PLUS -> (nextLex, Semantic.UnOp "+" rightNode)
  Token.MINUS -> (nextLex, Semantic.UnOp "-" rightNode)
  Token.OPEN_PAR ->
    if next lexAfterOpen == Token.CLOSE_PAR
      then (getNext lexAfterOpen, exprNode) -- Consume CLOSE_PAR
      else error $ "[Parser] Expected CLOSE_PAR at position: " ++ show (Lexer.position lexAfterOpen)
  (Token.INT val) -> (getNext lex, Semantic.IntNode val)
  _ -> error $ "[Parser] Expected INT, got: " ++ show nextToken ++ ", at position: " ++ show (Lexer.position lex)
  where
    nextToken = next lex
    (nextLex, rightNode) = parseFactor (getNext lex)
    (lexAfterOpen, exprNode) = parseExpression (getNext lex)

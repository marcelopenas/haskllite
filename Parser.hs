module Parser
  ( run,
  )
where

import Data.Bits (Bits (xor))
import Lexer (Lexer (..), getNext)
import Semantic
import Token

type Parser a = (Lexer, Token) -> ((Lexer, Token), a)

run :: String -> Node
run source
  -- = error $ show $ getNext $ fst $ getNext $ fst $ getNext $ fst $ getNext invalidLexer
  | finalToken == Token.EOF = node
  | otherwise = error $ "[Parser] Unexpected token at end of input: " ++ show finalToken
  where
    invalidLexer = Lexer source (-1)
    (initialLexer, next) = getNext invalidLexer
    ((finalLex, finalToken), node) = parseExpression (initialLexer, next)

parseExpression :: Parser Node
parseExpression (Lexer source position, next) = case nextToken of
  Token.PLUS -> expected
  Token.MINUS -> expected
  _ -> parseExpressionLoop nextLex leftNode
  where
    nextToken = next
    (nextLex, leftNode) = parseTerm (lex, next)
    expected = error $ "[Parser] Expected INT, got: " ++ show nextToken ++ ", at position: " ++ show position
    lex = Lexer source position

parseExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseExpressionLoop (Lexer source position, next) leftNode = case nextToken of
  Token.PLUS -> parseExpressionLoop nextLex (Semantic.BinOp "+" leftNode rightNode)
  Token.MINUS -> parseExpressionLoop nextLex (Semantic.BinOp "-" leftNode rightNode)
  _ -> ((lex, next), leftNode)
  where
    nextToken = next
    (nextLex, rightNode) = parseTerm (getNext lex)
    lex = Lexer source position

parseTerm :: Parser Node
parseTerm (lex, next) = parseTermLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseFactor (lex, next)

parseTermLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseTermLoop (Lexer source position, next) leftNode = case nextToken of
  Token.MULT -> parseTermLoop nextLex (Semantic.BinOp "*" leftNode rightNode)
  Token.DIV -> parseTermLoop nextLex (Semantic.BinOp "/" leftNode rightNode)
  _ -> ((lex, next), leftNode)
  where
    nextToken = next
    (nextLex, rightNode) = parseFactor (getNext lex)
    lex = Lexer source position

parseFactor :: Parser Node
parseFactor (Lexer source position, next) = case nextToken of
  Token.PLUS -> (nextLex, Semantic.UnOp "+" rightNode)
  Token.MINUS -> (nextLex, Semantic.UnOp "-" rightNode)
  Token.OPEN_PAR ->
    if nextAfterOpen == Token.CLOSE_PAR
      then (getNext lexAfterOpen, exprNode) -- Consume CLOSE_PAR
      else error $ "[Parser] Expected CLOSE_PAR at position: " ++ show positionAfterOpen
  (Token.INT val) -> (getNext lex, Semantic.IntNode val)
  _ -> error $ "[Parser] Expected INT, got: " ++ show nextToken ++ ", at position: " ++ show position
  where
    nextToken = next
    (nextLex, rightNode) = parseFactor (getNext lex)
    ((Lexer sourceAfterOpen positionAfterOpen, nextAfterOpen), exprNode) = parseExpression (getNext lex)
    lexAfterOpen = Lexer sourceAfterOpen positionAfterOpen
    lex = Lexer source position

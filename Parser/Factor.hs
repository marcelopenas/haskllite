module Parser.Factor (parseFactor) where

import Lexer (Lexer (..), getNext)
import {-# SOURCE #-} Parser.Expression (parseExpression)
import Parser.Parser (Parser)
import Semantic
import Token

parseFactor :: Parser Node
parseFactor (Lexer source position, token) = case token of
  Token.PLUS -> (nextLex, Semantic.UnOp "+" rightNode)
  Token.MINUS -> (nextLex, Semantic.UnOp "-" rightNode)
  Token.OPEN_PAR ->
    if nextAfterOpen == Token.CLOSE_PAR
      then (getNext lexAfterOpen, exprNode) -- Consume CLOSE_PAR
      else error $ "[Parser] Expected CLOSE_PAR at position: " ++ show positionAfterOpen
  (Token.INT val) -> (getNext lex, Semantic.IntNode val)
  (Token.IDENTIFIER name) -> (getNext lex, Semantic.Identifier name)
  _ -> error $ "[Parser] Expected INT, got: " ++ show token ++ ", at position: " ++ show position
  where
    (nextLex, rightNode) = parseFactor (getNext lex)
    ((Lexer sourceAfterOpen positionAfterOpen, nextAfterOpen), exprNode) = parseExpression (getNext lex)
    lexAfterOpen = Lexer sourceAfterOpen positionAfterOpen
    lex = Lexer source position

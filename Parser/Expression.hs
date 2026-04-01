module Parser.Expression (parseExpression) where

import Lexer (Lexer (..), getNext)
import Parser.Parser (Parser)
import Parser.Term (parseTerm)
import Semantic
import Token

parseExpression :: Parser Node
parseExpression (Lexer source position, token) = case token of
  Token.PLUS -> expected
  Token.MINUS -> expected
  _ -> parseExpressionLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseTerm (lex, token)
    expected = error $ "[Parser] Expected INT, got: " ++ show token ++ ", at position: " ++ show position
    lex = Lexer source position

parseExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseExpressionLoop (Lexer source position, token) leftNode = case token of
  Token.PLUS -> parseExpressionLoop nextLex (Semantic.BinOp "+" leftNode rightNode)
  Token.MINUS -> parseExpressionLoop nextLex (Semantic.BinOp "-" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseTerm (getNext lex)
    lex = Lexer source position

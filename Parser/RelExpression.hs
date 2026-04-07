module Parser.RelExpression (parseRelExpression) where

import Lexer (Lexer (..), getNext)
import Parser.Parser (Parser)
import Parser.Expression (parseExpression)
import Semantic
import Token

parseRelExpression :: Parser Node
parseRelExpression (Lexer source position, token) = case token of
  Token.EQUAL -> expected
  Token.GREATER -> expected
  Token.LESSER -> expected
  _ -> parseRelExpressionLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseExpression (lex, token)
    expected = error $ "[Parser] Expected RelExpression, got: " ++ show token ++ ", at position: " ++ show position
    lex = Lexer source position

parseRelExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseRelExpressionLoop (Lexer source position, token) leftNode = case token of
  Token.EQUAL -> parseRelExpressionLoop nextLex (Semantic.BinOp "==" leftNode rightNode)
  Token.GREATER -> parseRelExpressionLoop nextLex (Semantic.BinOp ">" leftNode rightNode)
  Token.LESSER -> parseRelExpressionLoop nextLex (Semantic.BinOp "<" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseExpression (getNext lex)
    lex = Lexer source position

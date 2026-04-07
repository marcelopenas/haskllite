module Parser.BoolExpression (parseBoolExpression) where

import Lexer (Lexer (..), getNext)
import Parser.Parser (Parser)
import Parser.BoolTerm (parseBoolTerm)
import Semantic
import Token

parseBoolExpression :: Parser Node
parseBoolExpression (Lexer source position, token) = case token of
  Token.OR -> expected
  _ -> parseBoolExpressionLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseBoolTerm (lex, token)
    expected = error $ "[Parser] Expected BoolExpression, got: " ++ show token ++ ", at position: " ++ show position
    lex = Lexer source position

parseBoolExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseBoolExpressionLoop (Lexer source position, token) leftNode = case token of
  Token.OR -> parseBoolExpressionLoop nextLex (Semantic.BinOp "||" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseBoolTerm (getNext lex)
    lex = Lexer source position

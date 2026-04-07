module Parser.BoolTerm (parseBoolTerm) where

import Lexer (Lexer (..), getNext)
import Parser.Parser (Parser)
import Parser.RelExpression (parseRelExpression)
import Semantic
import Token

parseBoolTerm :: Parser Node
parseBoolTerm (Lexer source position, token) = case token of
  Token.AND -> expected
  _ -> parseBoolTermLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseRelExpression (lex, token)
    expected = error $ "[Parser] Expected BoolTerm, got: " ++ show token ++ ", at position: " ++ show position
    lex = Lexer source position

parseBoolTermLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseBoolTermLoop (Lexer source position, token) leftNode = case token of
  Token.AND -> parseBoolTermLoop nextLex (Semantic.BinOp "&&" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseRelExpression (getNext lex)
    lex = Lexer source position

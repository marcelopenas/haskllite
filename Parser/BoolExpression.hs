module Parser.BoolExpression (parseBoolExpression) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), getNext)
import Node (Node (BinOp))
import Parser.BoolTerm (parseBoolTerm)
import Parser.Parser (Parser)
import Token (Token (OR))

parseBoolExpression :: Parser Node
parseBoolExpression lexState@(lex, token) = case token of
  OR -> compilerParserError lexState "Expected BoolExpression"
  _ -> parseBoolExpressionLoop `uncurry` parseBoolTerm lexState

parseBoolExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseBoolExpressionLoop lexState@(lex, token) leftNode = case token of
  OR -> parseBoolExpressionLoop nextLex (BinOp "||" leftNode rightNode)
  _ -> (lexState, leftNode)
  where
    (nextLex, rightNode) = parseBoolTerm (getNext lex)
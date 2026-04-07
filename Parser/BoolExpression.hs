module Parser.BoolExpression (parseBoolExpression) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), getNext)
import Parser.BoolTerm (parseBoolTerm)
import Parser.Parser (Parser)
import Semantic (Node (BinOp))
import Token (Token (OR))

parseBoolExpression :: Parser Node
parseBoolExpression lexerState = case token of
  OR -> compilerParserError lexerState "Expected BoolExpression"
  _ -> parseBoolExpressionLoop nextLex leftNode
  where
    (_, token) = lexerState
    (nextLex, leftNode) = parseBoolTerm lexerState

parseBoolExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseBoolExpressionLoop (lex, token) leftNode = case token of
  OR -> parseBoolExpressionLoop nextLex (BinOp "||" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseBoolTerm (getNext lex)

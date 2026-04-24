module Parser.BoolTerm (parseBoolTerm) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Node (Node (BinOp))
import Parser.Parser (Parser)
import Parser.RelExpression (parseRelExpression)
import Token (Token (AND))

parseBoolTerm :: Parser Node
parseBoolTerm lexState@(lex, token) = case token of
  AND -> compilerParserError lexState "Expected BoolTerm"
  _ -> parseBoolTermLoop `uncurry` parseRelExpression lexState

parseBoolTermLoop :: LexerState -> Node -> (LexerState, Node)
parseBoolTermLoop lexState@(lex, token) node = case token of
  AND -> parseBoolTermLoop expressionLex (BinOp "&&" node rightNode)
  _ -> (lexState, node)
  where
    (expressionLex, rightNode) = parseRelExpression (getNext lex)

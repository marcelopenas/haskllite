module Parser.BoolTerm (parseBoolTerm) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Parser.Parser (Parser)
import Parser.RelExpression (parseRelExpression)
import Semantic (Node (BinOp))
import Token (Token (AND))

parseBoolTerm :: Parser Node
parseBoolTerm (lex, token) = case token of
  AND -> compilerParserError lexerState "Expected BoolTerm"
  _ -> parseBoolTermLoop nextLex leftNode
  where
    lexerState = (lex, token)
    (nextLex, leftNode) = parseRelExpression lexerState

parseBoolTermLoop :: LexerState -> Node -> (LexerState, Node)
parseBoolTermLoop (lex, token) leftNode = case token of
  AND -> parseBoolTermLoop nextLex (BinOp "&&" leftNode rightNode)
  _ -> (lexerState, leftNode)
  where
    lexerState = (lex, token)
    (nextLex, rightNode) = parseRelExpression (getNext lex)

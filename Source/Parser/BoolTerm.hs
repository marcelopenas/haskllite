module Source.Parser.BoolTerm (parseBoolTerm) where

import Source.Parser.Parser (Parser)
import Source.Parser.RelExpression (parseRelExpression)
import Source.CompilerError (compilerParserError)
import Source.Lexer (Lexer (..), LexerState, getNext)
import Source.Node (Node (BinOp))
import Source.Token (Token (AND))

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

module Parser.Expression (parseExpression) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Node (Node (BinOp))
import Parser.Parser (Parser)
import Parser.Term (parseTerm)
import Token (Token (MINUS, PLUS))

parseExpression :: Parser Node
parseExpression lexerState@(lex, token) = case token of
  PLUS -> compilerParserError lexerState "Expected INT"
  MINUS -> compilerParserError lexerState "Expected INT"
  _ -> parseExpressionLoop `uncurry` parseTerm lexerState

parseExpressionLoop :: LexerState -> Node -> (LexerState, Node)
parseExpressionLoop lexerState@(lex, token) node = case token of
  PLUS -> parseExpressionLoop termLex (BinOp "+" node rightNode)
  MINUS -> parseExpressionLoop termLex (BinOp "-" node rightNode)
  _ -> (lexerState, node)
  where
    (termLex, rightNode) = parseTerm $ getNext lex

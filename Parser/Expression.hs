module Parser.Expression (parseExpression) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Parser.Parser (Parser)
import Parser.Term (parseTerm)
import Semantic (Node (BinOp))
import Token (Token (MINUS, PLUS))

parseExpression :: Parser Node
parseExpression (lex, token) = case token of
  PLUS -> compilerParserError lexerState "Expected INT"
  MINUS -> compilerParserError lexerState "Expected INT"
  _ -> parseExpressionLoop nextLex leftNode
  where
    lexerState = (lex, token)
    (nextLex, leftNode) = parseTerm lexerState

parseExpressionLoop :: LexerState -> Node -> (LexerState, Node)
parseExpressionLoop (lex, token) leftNode = case token of
  PLUS -> parseExpressionLoop nextLex (BinOp "+" leftNode rightNode)
  MINUS -> parseExpressionLoop nextLex (BinOp "-" leftNode rightNode)
  _ -> (lexerState, leftNode)
  where
    lexerState = (lex, token)
    (nextLex, rightNode) = parseTerm $ getNext lex

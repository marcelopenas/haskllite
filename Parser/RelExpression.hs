module Parser.RelExpression (parseRelExpression) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Parser.Expression (parseExpression)
import Parser.Parser (Parser)
import Semantic (Node (BinOp))
import Token (Token (EQUAL, GREATER, LESSER))

parseRelExpression :: Parser Node
parseRelExpression (lex, token) = case token of
  EQUAL -> compilerParserError lexerState "Expected RelExpression"
  GREATER -> compilerParserError lexerState "Expected RelExpression"
  LESSER -> compilerParserError lexerState "Expected RelExpression"
  _ -> parseRelExpressionLoop nextLex leftNode
  where
    lexerState = (lex, token)
    (nextLex, leftNode) = parseExpression lexerState

parseRelExpressionLoop :: LexerState -> Node -> (LexerState, Node)
parseRelExpressionLoop (lex, token) leftNode = case token of
  EQUAL -> parseRelExpressionLoop nextLex (BinOp "==" leftNode rightNode)
  GREATER -> parseRelExpressionLoop nextLex (BinOp ">" leftNode rightNode)
  LESSER -> parseRelExpressionLoop nextLex (BinOp "<" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseExpression (getNext lex)

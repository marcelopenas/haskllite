module Parser.RelExpression (parseRelExpression) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Parser.Expression (parseExpression)
import Parser.Parser (Parser)
import Semantic (Node (BinOp))
import Token (Token (EQUAL, GREATER, LESSER))

parseRelExpression :: Parser Node
parseRelExpression lexState@(lex, token) = case token of
  EQUAL -> compilerParserError lexState "Expected RelExpression"
  GREATER -> compilerParserError lexState "Expected RelExpression"
  LESSER -> compilerParserError lexState "Expected RelExpression"
  _ -> parseRelExpressionLoop `uncurry` parseExpression lexState

parseRelExpressionLoop :: LexerState -> Node -> (LexerState, Node)
parseRelExpressionLoop lexState@(lex, token) leftNode = case token of
  EQUAL -> parseRelExpressionLoop expressionLex (BinOp "==" leftNode rightNode)
  GREATER -> parseRelExpressionLoop expressionLex (BinOp ">" leftNode rightNode)
  LESSER -> parseRelExpressionLoop expressionLex (BinOp "<" leftNode rightNode)
  _ -> (lexState, leftNode)
  where
    (expressionLex, rightNode) = parseExpression (getNext lex)

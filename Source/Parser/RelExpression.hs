module Source.Parser.RelExpression (parseRelExpression) where

import Source.CompilerError (compilerParserError)
import Source.Lexer (Lexer (..), LexerState, getNext)
import Source.Node (Node (BinOp))
import Source.Parser.Expression (parseExpression)
import Source.Parser.Parser (Parser)
import Source.Token (Token (EQUAL, GREATER, LESSER))

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

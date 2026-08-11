module Source.Parser.Expression (parseExpression) where

import Source.Parser.Parser (Parser)
import Source.Parser.Term (parseTerm)
import Source.Lexer (Lexer (..), LexerState, getNext)
import Source.Node (Node (BinOp))
import Source.CompilerError (compilerParserError)
import Source.Token (Token (MINUS, PLUS))

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

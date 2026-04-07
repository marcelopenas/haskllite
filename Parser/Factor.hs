module Parser.Factor (parseFactor) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser)
import Semantic
import Token

parseFactor :: Parser Node
parseFactor (Lexer source position, token) = case token of
  Token.PLUS -> (nextLex, Semantic.UnOp "+" rightNode)
  Token.MINUS -> (nextLex, Semantic.UnOp "-" rightNode)
  Token.NOT -> (nextLex, Semantic.UnOp "!" rightNode)
  Token.SCAN -> (parseFactorScan $ getNext lex, Semantic.Scan)
  Token.OPEN_PAR ->
    if nextAfterOpen == Token.CLOSE_PAR
      then (getNext lexAfterOpen, exprNode) -- Consume CLOSE_PAR
      else error $ "[Parser] Expected CLOSE_PAR at position: " ++ show positionAfterOpen
  (Token.INT val) -> (getNext lex, Semantic.IntNode val)
  (Token.IDENTIFIER name) -> (getNext lex, Semantic.Identifier name)
  _ -> error $ "[Parser] Expected INT, got: " ++ show token ++ ", at position: " ++ show position
  where
    (nextLex, rightNode) = parseFactor (getNext lex)
    ((Lexer sourceAfterOpen positionAfterOpen, nextAfterOpen), exprNode) = parseBoolExpression (getNext lex)
    lexAfterOpen = Lexer sourceAfterOpen positionAfterOpen
    lex = Lexer source position

parseFactorScan :: LexerState -> LexerState
parseFactorScan (lex, token) = case token of
  Token.OPEN_PAR -> parseFactorClose nextScanner
  _ -> compilerParserError scanner "Expected open par"
  where
    scanner = (lex, token)
    (Lexer source position) = lex
    nextScanner = getNext lex

parseFactorClose :: LexerState -> LexerState
parseFactorClose (lex, token) = case token of
  Token.CLOSE_PAR -> nextScanner
  _ -> compilerParserError scanner "Expected close par"
  where
    scanner = (lex, token)
    (Lexer source position) = lex
    nextScanner = getNext lex

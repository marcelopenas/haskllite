module Parser.Factor (parseFactor) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser)
import Semantic
import Token

parseFactor :: Parser Node
parseFactor (lex, token) = case token of
  PLUS -> (afterLex, UnOp "+" rightNode)
  MINUS -> (afterLex, UnOp "-" rightNode)
  NOT -> (afterLex, UnOp "!" rightNode)
  SCAN -> (parseFactorScan $ getNext lex, Scan)
  OPEN_PAR ->
    if nextAfterOpen == CLOSE_PAR
      then (getNext lexAfterOpen, exprNode) -- Consume CLOSE_PAR
      else compilerParserError (lexAfterOpen, nextAfterOpen) "Expected CLOSE_PAR"
  INT val -> (nextLex, IntNode val)
  BOOLEAN val -> (nextLex, BoolNode val)
  STR val -> (nextLex, StringNode val)
  IDENTIFIER name -> (nextLex, Identifier name)
  _ -> compilerParserError (lex, token) "Expected INT | BOOL | STR | IDENT"
  where
    nextLex = getNext lex
    (afterLex, rightNode) = parseFactor nextLex
    ((lexAfterOpen, nextAfterOpen), exprNode) = parseBoolExpression (getNext lex)

parseFactorScan :: LexerState -> LexerState
parseFactorScan (lex, token) = case token of
  OPEN_PAR -> parseFactorClose nextScanner
  _ -> compilerParserError scanner "Expected open par"
  where
    scanner = (lex, token)
    (Lexer source position) = lex
    nextScanner = getNext lex

parseFactorClose :: LexerState -> LexerState
parseFactorClose (lex, token) = case token of
  CLOSE_PAR -> nextScanner
  _ -> compilerParserError scanner "Expected close par"
  where
    scanner = (lex, token)
    (Lexer source position) = lex
    nextScanner = getNext lex

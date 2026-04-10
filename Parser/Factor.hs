module Parser.Factor (parseFactor) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser)
import Semantic
import Token

parseFactor :: Parser Node
parseFactor lexState@(lex, token) = case token of
  PLUS -> (factorLexState, UnOp "+" factorNode)
  MINUS -> (factorLexState, UnOp "-" factorNode)
  NOT -> (factorLexState, UnOp "!" factorNode)
  SCAN -> (parseFactorScan $ getNext lex, Scan)
  OPEN_PAR ->
    if expressionToken == CLOSE_PAR
      then (getNext expressionLex, expressionNode) -- Consume CLOSE_PAR
      else compilerParserError (expressionLex, expressionToken) "Expected CLOSE_PAR"
  INT val -> (nextLex, IntNode val)
  BOOLEAN val -> (nextLex, BoolNode val)
  STR val -> (nextLex, StringNode val)
  IDENTIFIER name -> (nextLex, Identifier name)
  _ -> compilerParserError lexState "Expected INT | BOOL | STR | IDENT"
  where
    nextLex = getNext lex
    (factorLexState, factorNode) = parseFactor nextLex
    ((expressionLex, expressionToken), expressionNode) = parseBoolExpression (getNext lex)

parseFactorScan :: LexerState -> LexerState
parseFactorScan lexState@(lex, token) = case token of
  OPEN_PAR -> parseFactorClose $ getNext lex
  _ -> compilerParserError lexState "Expected open par"

parseFactorClose :: LexerState -> LexerState
parseFactorClose lexState@(lex, token) = case token of
  CLOSE_PAR -> getNext lex
  _ -> compilerParserError lexState "Expected close par"

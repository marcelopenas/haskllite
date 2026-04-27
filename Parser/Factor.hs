module Parser.Factor (parseFactor) where

import CompilerError (compilerParserError)
import Distribution.Fields.LexerMonad (LexState)
import Lexer (Lexer (..), LexerState, getNext)
import Node (Node (..))
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
  OPEN_PAR -> case getNext lex of
    (typeLex, TYPE typeName) ->
      let castLexState = parseFactorClose $ getNext typeLex
          (afterCastLexState, castNode) = parseFactor castLexState
       in (afterCastLexState, CastNode castNode typeName)
    _ ->
      if expressionToken == CLOSE_PAR
        then (nextExpressionState, expressionNode) -- Consume CLOSE_PAR
        else compilerParserError (expressionLex, expressionToken) "Expected CLOSE_PAR"
  INT val -> (nextLexState, IntNode val)
  FLOAT val -> (nextLexState, FloatNode val)
  BOOLEAN val -> (nextLexState, BoolNode val)
  STR val -> (nextLexState, StringNode val)
  IDENTIFIER name -> parseIdentifierFactor name nextLexState
  IF ->
    let (afterIfLexState, afterIfNode) = parseBoolExpression $ getNext lex
        (trueLexState, trueNode) = parseFactorIf afterIfLexState
        (falseLexState, falseNode) = parseFactorElse trueLexState
     in (falseLexState, If afterIfNode trueNode falseNode)
  _ -> compilerParserError lexState "Expected INT | BOOL | STR | IDENT"
  where
    nextLexState = getNext lex
    (factorLexState, factorNode) = parseFactor nextLexState
    ((expressionLex, expressionToken), expressionNode) = parseBoolExpression (getNext lex)
    nextExpressionState@(nextExpressionLex, nextExpressionToken) = getNext expressionLex

parseFactorScan :: LexerState -> LexerState
parseFactorScan lexState@(lex, token) = case token of
  OPEN_PAR -> parseFactorClose $ getNext lex
  _ -> compilerParserError lexState "Expected open par"

parseFactorClose :: LexerState -> LexerState
parseFactorClose lexState@(lex, token) = case token of
  CLOSE_PAR -> getNext lex
  _ -> compilerParserError lexState "Expected close par"

parseFactorClosePass :: (LexerState, Node) -> (LexerState, Node)
parseFactorClosePass (lexState@(lex, token), node) = case token of
  CLOSE_PAR -> (getNext lex, node)
  _ -> compilerParserError lexState "Expected close par"

parseFactorIf :: Parser Node
parseFactorIf lexState@(lex, token) = parseFactorCloseBra $ parseFactor $ parseFactorOpenBra lexState

parseFactorOpenBra :: LexerState -> LexerState
parseFactorOpenBra lexState@(lex, token) = case token of
  OPEN_BRA -> getNext lex
  _ -> compilerParserError lexState "Expected open bra"

parseFactorCloseBra :: (LexerState, Node) -> (LexerState, Node)
parseFactorCloseBra (lexState@(lex, token), node) = case token of
  CLOSE_BRA -> (getNext lex, node)
  _ -> compilerParserError lexState "Expected close bra"

parseFactorElse :: Parser Node
parseFactorElse lexState@(lex, token) = case token of
  ELSE -> parseFactorIf $ getNext lex
  _ -> compilerParserError lexState "expected else in if expression"

parseIdentifierFactor :: String -> Parser Node
parseIdentifierFactor name lexState@(lex, token) = case token of
  OPEN_PAR -> parseCallFactor name lexState
  _ -> (lexState, Identifier name)

parseCallFactor :: String -> Parser Node
parseCallFactor name lexState@(lex, token) = case token of
  OPEN_PAR ->
    let afterOpen = getNext lex
        (afterArgs, args) = parseCallArgsFactor afterOpen
        (afterClose, _) = expectCloseParFactor afterArgs
     in (afterClose, FuncCall name args)
  _ -> compilerParserError lexState "Expected open paren for function call"

parseCallArgsFactor :: Parser [Node]
parseCallArgsFactor lexState@(lex, token) = case token of
  CLOSE_PAR -> (lexState, [])
  _ ->
    let (afterFirst, first) = parseBoolExpression lexState
        (afterRest, rest) = parseCallArgsRestFactor afterFirst
     in (afterRest, first : rest)

parseCallArgsRestFactor :: Parser [Node]
parseCallArgsRestFactor lexState@(lex, token) = case token of
  COMMA ->
    let afterComma = getNext lex
        (afterExpr, expr) = parseBoolExpression afterComma
        (afterRest, rest) = parseCallArgsRestFactor afterExpr
     in (afterRest, expr : rest)
  _ -> (lexState, [])

expectCloseParFactor :: Parser ()
expectCloseParFactor lexState@(lex, token) = case token of
  CLOSE_PAR -> (getNext lex, ())
  _ -> compilerParserError lexState "Expected close paren"
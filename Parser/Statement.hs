module Parser.Statement (parseStatement) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Parser.Block (parseBlock)
import Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser)
import Semantic (Node (Assignment, If, NoOp, Print, While))
import Token (Token (ASSIGN, CLOSE_PAR, ELSE, END, IDENTIFIER, IF, LET, OPEN_PAR, PRINT, WHILE))

parseStatement :: Parser Node
parseStatement lexState@(lex, token) = case token of
  END -> (nextLexState, NoOp)
  LET -> parseStatementLet nextLexState
  IDENTIFIER name -> parseStatementAssign name False nextLexState
  PRINT -> (parseStatementEnd openParLexerState, Print openParNode)
  WHILE -> (statementLexState, While openParNode statementNode)
  IF ->
    let (afterStatementLexState, afterAfterNode) = parseStatementElse statementLexState
     in (afterStatementLexState, If openParNode statementNode afterAfterNode)
  _ -> parseBlock lexState
  where
    nextLexState = getNext lex
    (openParLexerState, openParNode) = parseBoolExpression $ parseStatementOpenPar nextLexState
    (statementLexState, statementNode) = parseStatement openParLexerState

parseStatementOpenPar :: LexerState -> LexerState
parseStatementOpenPar lexState@(lex, token) = case token of
  OPEN_PAR -> lexState
  _ -> compilerParserError lexState "Expected open parenthesis"

parseStatementElse :: Parser Node
parseStatementElse lexState@(lex, token) = case token of
  ELSE -> parseStatement $ getNext lex
  _ -> (lexState, NoOp)

parseStatementLet :: Parser Node
parseStatementLet lexState@(lex, token) = case token of
  IDENTIFIER name -> parseStatementAssign name True $ getNext lex
  _ -> compilerParserError lexState "Expected identifier"

parseStatementAssign :: String -> Bool -> Parser Node
parseStatementAssign name immutable lexState@(lex, token) = case token of
  ASSIGN -> (parseStatementEnd expressionLexState, Assignment name newNode immutable)
  _ -> compilerParserError lexState "Expected assignment"
  where
    (expressionLexState, newNode) = parseBoolExpression $ getNext lex

parseStatementEnd :: (Lexer, Token) -> (Lexer, Token)
parseStatementEnd lexState@(lex, token) = case token of
  END -> getNext lex
  _ -> compilerParserError lexState "No END in statement"

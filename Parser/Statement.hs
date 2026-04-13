module Parser.Statement (parseStatement) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Parser.Block (parseBlock)
import Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser)
import Semantic (Node (Assignment, If, NoOp, Print, VarDec, While))
import Token (Token (ASSIGN, CLOSE_PAR, ELSE, END, IDENTIFIER, IF, LET, MUT, OPEN_PAR, PRINT, TYPE, TYPE_ASSIGN, WHILE), VarType)

parseStatement :: Parser Node
parseStatement lexState@(lex, token) = case token of
  END -> (nextLexState, NoOp)
  LET -> parseStatementLet nextLexState
  IDENTIFIER name -> parseStatementAssign name nextLexState
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
  MUT -> parseStatementMut $ getNext lex
  IDENTIFIER name -> parseStatementDeclareTypeAssign name True $ getNext lex
  _ -> compilerParserError lexState "Expected identifier"

parseStatementMut :: Parser Node
parseStatementMut lexState@(lex, token) = case token of
  IDENTIFIER name -> parseStatementDeclareTypeAssign name False $ getNext lex
  _ -> compilerParserError lexState "Expected identifier"
  where
    (expressionLexState, newNode) = parseBoolExpression $ getNext lex

parseStatementAssign :: String -> Parser Node
parseStatementAssign name lexState@(lex, token) = case token of
  ASSIGN -> (parseStatementEnd expressionLexState, Assignment name newNode)
  _ -> compilerParserError lexState "Expected assignment"
  where
    (expressionLexState, newNode) = parseBoolExpression $ getNext lex

parseStatementDeclareTypeAssign :: String -> Bool -> Parser Node
parseStatementDeclareTypeAssign name immutable lexState@(lex, token) = case token of
  TYPE_ASSIGN -> (expressionLexState, newNode)
  _ -> compilerParserError lexState "Expected :" -- to accept no :, call parseStatementDeclare with a new type as cast
  where
    (expressionLexState, newNode) = parseStatementDeclareType name immutable $ getNext lex

parseStatementDeclareType :: String -> Bool -> Parser Node
parseStatementDeclareType name immutable lexState@(lex, token) = case token of
  TYPE varType ->
    let (expressionLexState, newNode) = parseStatementDeclare name immutable varType $ getNext lex
     in (parseStatementEnd expressionLexState, newNode)
  _ -> compilerParserError lexState "Expected type"

parseStatementDeclare :: String -> Bool -> VarType -> Parser Node
parseStatementDeclare name immutable varType lexState@(lex, token) = case token of
  ASSIGN -> (expressionLexState, VarDec name newNode immutable varType)
  _ -> compilerParserError lexState "Expected declaration"
  where
    (expressionLexState, newNode) = parseBoolExpression $ getNext lex

parseStatementEnd :: (Lexer, Token) -> (Lexer, Token)
parseStatementEnd lexState@(lex, token) = case token of
  END -> getNext lex
  _ -> compilerParserError lexState "No END in statement"

module Parser.Statement (parseStatement) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Node (Node (Assignment, For, If, NoOp, Print, VarDec, While))
import {-# SOURCE #-} Parser.Block (parseBlock)
import Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser)
import Token (Token (ASSIGN, CLOSE_PAR, ELSE, END, FOR, IDENTIFIER, IF, LET, MUT, OPEN_PAR, PRINT, TYPE, TYPE_ASSIGN, WHILE), VarType (I32T))

parseStatement :: Parser Node
parseStatement lexState@(lex, token) = case token of
  END -> (nextLexState, NoOp)
  LET -> parseStatementLet nextLexState
  IDENTIFIER name -> parseStatementAssign name nextLexState
  PRINT -> (parseStatementEnd openParLexerState, Print openParNode)
  WHILE -> (statementLexState, While openParNode statementNode)
  FOR ->
    let (assignmentLexState, assignmentNode) = parseStatementEndP $ parseStatementInitFor $ parseStatementOpenParP nextLexState -- custom "parseStatementLet"" to not use type default to int and DecVal
        (conditionLexState, conditionNode) = parseStatementEndP $ parseBoolExpression assignmentLexState
        (updateLexState, updateNode) = parseStatementCloseParP $ parseStatementIdentifierFor conditionLexState -- custom "parseStatement" to not use ; in end
        (expressionLexState, expressionNode) = parseBlock updateLexState
     in (expressionLexState, For assignmentNode conditionNode updateNode expressionNode)
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

parseStatementOpenParP :: LexerState -> LexerState
parseStatementOpenParP lexState@(lex, token) = case token of
  OPEN_PAR -> getNext lex
  _ -> compilerParserError lexState "Expected open parenthesis"

parseStatementCloseParP :: (LexerState, Node) -> (LexerState, Node)
parseStatementCloseParP (lexState@(lex, token), node) = case token of
  CLOSE_PAR -> (getNext lex, node)
  _ -> compilerParserError lexState "Expected close parenthesis"

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
  _
    | immutable -> compilerParserError lexState "No assignment to immutable variable"
    | otherwise -> (lexState, VarDec name NoOp immutable varType)
  where
    (expressionLexState, newNode) = parseBoolExpression $ getNext lex

parseStatementEnd :: (Lexer, Token) -> (Lexer, Token)
parseStatementEnd lexState@(lex, token) = case token of
  END -> getNext lex
  _ -> compilerParserError lexState "No END in statement"

parseStatementEndP :: (LexerState, Node) -> (LexerState, Node)
parseStatementEndP (lexState@(lex, token), node) = case token of
  END -> (getNext lex, node)
  _ -> compilerParserError lexState "No END in statement"

-- * For

parseStatementInitFor :: Parser Node
parseStatementInitFor lexState@(lex, token) = case token of
  MUT -> parseStatementMut $ getNext lex
  IDENTIFIER name -> parseStatementDeclare name False I32T $ getNext lex -- Initial var is auto mut and i32
  _ -> compilerParserError lexState "Expected identifier"

parseStatementIdentifierFor :: Parser Node
parseStatementIdentifierFor lexState@(lex, token) = case token of
  IDENTIFIER name -> parseStatementAssignFor name (getNext lex)

parseStatementAssignFor :: String -> Parser Node
parseStatementAssignFor name lexState@(lex, token) = case token of
  ASSIGN -> (expressionLexState, Assignment name newNode)
  _ -> compilerParserError lexState "Expected assignment"
  where
    (expressionLexState, newNode) = parseBoolExpression $ getNext lex
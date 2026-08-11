module Source.Parser.Statement (parseStatement) where

import Source.CompilerError (compilerParserError)
import Source.Lexer (Lexer (..), LexerState, getNext)
import Source.Node (Node (Assignment, For, FuncCall, If, NoOp, Print, Return, UnityNode, VarDec, While))
import {-# SOURCE #-} Source.Parser.Block (parseBlock)
import Source.Parser.BoolExpression (parseBoolExpression)
import Source.Parser.Parser (Parser)
import Source.Token (Token (ASSIGN, CLOSE_PAR, COMMA, ELSE, END, EOF, FOR, IDENTIFIER, IF, LET, MUT, OPEN_PAR, PRINT, RETURN, TYPE, TYPE_ASSIGN, WHILE), VarType (I32T))

parseStatement :: Parser Node
parseStatement lexState@(lex, token) = case token of
  END -> (nextLexState, NoOp)
  LET -> parseStatementLet nextLexState
  IDENTIFIER name -> parseIdentifier name nextLexState
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
  RETURN -> case token' of
    OPEN_PAR -> case token'' of
      CLOSE_PAR -> (getNext lex'', Return UnityNode)
      _ -> (returnLexerState, Return returnNode)
    END -> compilerParserError nextLexState "Expected return value or ()"
    EOF -> compilerParserError nextLexState "Expected return value or ()"
    _ -> (returnLexerState, Return returnNode)
    where
      (lex', token') = getNext lex
      (lex'', token'') = getNext lex'
      (returnLexerState, returnNode) = parseBoolExpression nextLexState
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

expectClosePar :: Parser ()
expectClosePar lexState@(lex, token) = case token of
  CLOSE_PAR -> (getNext lex, ())
  _ -> compilerParserError lexState "Expected close paren"

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

parseIdentifier :: String -> Parser Node
parseIdentifier name lexState@(lex, token) = case token of
  OPEN_PAR -> parseCall name lexState
  _ -> parseStatementAssign name lexState

parseCall :: String -> Parser Node
parseCall name lexState@(lex, token) = case token of
  OPEN_PAR ->
    let afterOpen = getNext lex
        (afterArgs, args) = parseCallArgs afterOpen
        (afterClose, _) = expectClosePar afterArgs
     in (afterClose, FuncCall name args)
  _ -> compilerParserError lexState "Expected open paren for function call"

parseCallArgs :: Parser [Node]
parseCallArgs lexState@(lex, token) = case token of
  CLOSE_PAR -> (lexState, [])
  _ ->
    let (afterFirst, first) = parseBoolExpression lexState
        (afterRest, rest) = parseCallArgsRest afterFirst
     in (afterRest, first : rest)

parseCallArgsRest :: Parser [Node]
parseCallArgsRest lexState@(lex, token) = case token of
  COMMA ->
    let afterComma = getNext lex
        (afterExpr, expr) = parseBoolExpression afterComma
        (afterRest, rest) = parseCallArgsRest afterExpr
     in (afterRest, expr : rest)
  _ -> (lexState, [])
module Parser.Statement (parseStatement) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Parser.Block (parseBlock)
import Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser)
import Semantic (Node (Assignment, If, NoOp, Print, While))
import Token (Token (ASSIGN, CLOSE_PAR, ELSE, END, IDENTIFIER, IF, LET, OPEN_PAR, PRINT, WHILE))

parseStatement :: Parser Node
parseStatement (lex, token) = case token of
  END -> (nextLexerState, NoOp)
  LET -> parseStatementLet nextLexerState
  IDENTIFIER name -> parseStatementAssign name False nextLexerState
  PRINT -> (parseStatementEnd newLexerState, Print newNode)
  WHILE -> (afterLexerState, While newNode afterNode)
  IF -> (afterAfterLexerState, If newNode afterNode afterAfterNode)
  _ -> parseBlock lexerState
  where
    lexerState = (lex, token)
    nextLexerState = getNext lex
    (newLexerState, newNode) = parseBoolExpression $ parseStatementOpenPar nextLexerState
    (afterLexerState, afterNode) = parseStatement newLexerState
    (afterAfterLexerState, afterAfterNode) = parseStatementElse afterLexerState

parseStatementOpenPar :: LexerState -> LexerState
parseStatementOpenPar (lex, token) = case token of
  OPEN_PAR -> lexerState
  _ -> compilerParserError lexerState "Expected open parenthesis"
  where
    lexerState = (lex, token)

parseStatementElse :: Parser Node
parseStatementElse (lex, token) = case token of
  ELSE -> (nextScanner, nextNode)
  _ -> ((lex, token), NoOp)
  where
    (nextScanner, nextNode) = parseStatement $ getNext lex

parseStatementLet :: Parser Node
parseStatementLet (lex, token) = case token of
  IDENTIFIER name -> parseStatementAssign name True nextLexerState
  _ -> compilerParserError lexerState "Expected identifier"
  where
    lexerState = (lex, token)
    nextLexerState = getNext lex

parseStatementAssign :: String -> Bool -> Parser Node
parseStatementAssign name immutable (lex, token) = case token of
  ASSIGN -> (parseStatementEnd newLexerState, Assignment name newNode immutable)
  _ -> compilerParserError lexerState "Expected assignment"
  where
    lexerState = (lex, token)
    (newLexerState, newNode) = parseBoolExpression $ getNext lex

parseStatementEnd :: (Lexer, Token) -> (Lexer, Token)
parseStatementEnd (lex, token) = case token of
  END -> getNext lex
  _ -> compilerParserError lexerState "No END in statement"
  where
    lexerState = (lex, token)

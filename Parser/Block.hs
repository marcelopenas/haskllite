module Parser.Block (parseBlock) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Node (Node (Block))
import Parser.Parser (Parser)
import Parser.Statement (parseStatement)
import Token (Token (CLOSE_BRA, EOF, OPEN_BRA))

parseBlock :: Parser Node
parseBlock lexState@(lex, token) = case token of
  OPEN_BRA -> (statementLexState, Block statementNode)
  _ -> compilerParserError lexState "Expected open bracket on block"
  where
    (statementLexState, statementNode) = parseBlockStatement $ getNext lex

parseBlockStatement :: Parser [Node]
parseBlockStatement lexState@(lex, token) = case token of
  CLOSE_BRA -> (getNext lex, [])
  _ -> parseBlockCloseBra $ parseBlockStatementLoop (lexState, [])

parseBlockStatementLoop :: (LexerState, [Node]) -> (LexerState, [Node])
parseBlockStatementLoop (lexState@(lex, token), nodes) = case token of
  CLOSE_BRA -> (lexState, nodes)
  _ -> parseBlockStatementLoop (nextLexState, nextNode : nodes)
  where
    (nextLexState, nextNode) = parseStatement lexState

parseBlockCloseBra :: (LexerState, [Node]) -> (LexerState, [Node])
parseBlockCloseBra (lexState@(lex, token), nodes) = case token of
  CLOSE_BRA -> (getNext lex, nodes)
  _ -> compilerParserError lexState "Expected close bracket on block"

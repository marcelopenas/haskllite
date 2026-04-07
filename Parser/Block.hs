module Parser.Block (parseBlock) where

import CompilerError (compilerParserError)
import Lexer (Lexer (..), LexerState, getNext)
import Parser.Parser (Parser)
import Parser.Statement (parseStatement)
import Semantic (Node (Block))
import Token (Token (CLOSE_BRA, EOF, OPEN_BRA))

parseBlock :: Parser Node
parseBlock (lex, token) = case token of
  OPEN_BRA -> (statementLexerState, Block statementNode)
  _ -> compilerParserError lexerState "Expected open bracket on block"
  where
    lexerState = (lex, token)
    (statementLexerState, statementNode) = parseBlockStatement $ getNext lex

parseBlockStatement :: Parser [Node]
parseBlockStatement (lex, token) = case token of
  CLOSE_BRA -> (getNext lex, [])
  _ -> parseBlockCloseBra $ parseBlockStatementLoop (lexerState, [])
  where
    lexerState = (lex, token)

parseBlockStatementLoop :: (LexerState, [Node]) -> (LexerState, [Node])
parseBlockStatementLoop ((lex, token), nodes) = case token of
  CLOSE_BRA -> (lexerState, nodes)
  _ -> parseBlockStatementLoop (nextLexerState, nextNode : nodes)
  where
    lexerState = (lex, token)
    (nextLexerState, nextNode) = parseStatement lexerState

parseBlockCloseBra :: (LexerState, [Node]) -> (LexerState, [Node])
parseBlockCloseBra (lexerState, nodes) = case token of
  CLOSE_BRA -> (getNext lex, nodes)
  _ -> compilerParserError lexerState "Expected close bracket on block"
  where
    (lex, token) = lexerState

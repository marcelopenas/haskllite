module Parser.Block where

import CompilerError (compilerError)
import CompilerError qualified
import Data.Graph (Tree (Node))
import Lexer (Lexer (..), getNext)
import Parser.Parser (Parser, Scanner)
import Parser.Statement (parseStatement)
import Semantic (Node)
import Semantic qualified as Node
import Token (Token (CLOSE_BRA, EOF, OPEN_BRA))

parseBlock :: Parser Node
parseBlock (lex, token) = case token of
  Token.OPEN_BRA -> (statementScanner, Node.Block statementNode)
  _ -> compilerError (lex, token) CompilerError.ParserError "expected open bracket on block"
  where
    (statementScanner, statementNode) = parseBlockStatement $ getNext lex

parseBlockStatement :: Parser [Node]
parseBlockStatement (lex, token) = case token of
  Token.CLOSE_BRA -> (getNext lex, [])
  _ -> parseBlockCloseBra $ parseBlockStatementLoop (scanner, [])
  where
    scanner = (lex, token)

parseBlockStatementLoop :: (Scanner, [Node]) -> (Scanner, [Node])
parseBlockStatementLoop ((lex, token), nodes) = case token of
  Token.CLOSE_BRA -> (scanner, nodes)
  _ -> parseBlockStatementLoop ((nextLex, nextToken), nextNode : nodes)
  where
    scanner = (lex, token)
    ((nextLex, nextToken), nextNode) = parseStatement scanner

parseBlockCloseBra :: (Scanner, [Node]) -> (Scanner, [Node])
parseBlockCloseBra (scanner, nodes) = case token of
  Token.CLOSE_BRA -> (getNext lex, nodes)
  _ -> compilerError scanner CompilerError.ParserError "expected close bracket on block"
  where
    (lex, token) = scanner
    (Lexer _ position) = lex

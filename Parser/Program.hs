module Parser.Program (parseProgram) where

import Lexer (Lexer (..), getNext)
import Parser.Parser (Parser)
import Parser.Statement (parseStatement)
import Semantic (Node (Block))
import Token (Token (EOF))

parseProgram :: Parser Node
parseProgram lexerState =
  (lastLexerState, Block nodes)
  where
    (lastLexerState, nodes) = parseProgramLoop [] lexerState

parseProgramLoop :: [Node] -> Parser [Node]
parseProgramLoop statements (lex, token) = case token of
  EOF -> ((lex, token), statements)
  _ -> (newLexerState, newStatements)
  where
    (nextLexerState, node) = parseStatement (lex, token)
    (newLexerState, newStatements) = parseProgramLoop (node : statements) nextLexerState

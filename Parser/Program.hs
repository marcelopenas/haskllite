module Parser.Program (parseProgram) where

import Parser.Parser (Parser)
import Parser.Statement (parseStatement)
import Semantic (Node (Block))
import Token (Token (EOF))

parseProgram :: Parser Node
parseProgram lexState =
  (lastLexState, Block nodes)
  where
    (lastLexState, nodes) = parseProgramLoop [] lexState

parseProgramLoop :: [Node] -> Parser [Node]
parseProgramLoop statements lexState@(lex, token) = case token of
  EOF -> (lexState, statements)
  _ -> parseProgramLoop (node : statements) nextLexState
  where
    (nextLexState, node) = parseStatement lexState

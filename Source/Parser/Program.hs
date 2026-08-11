module Source.Parser.Program (parseProgram) where

import Source.Parser.Function (parseFunction)
import Source.Parser.Parser (Parser)
import Source.Parser.Statement (parseStatement)
import Source.Node (Node (Block))
import Source.Token (Token (EOF, FN))

parseProgram :: Parser Node
parseProgram lexState =
  (lastLexState, Block nodes)
  where
    (lastLexState, nodes) = parseProgramLoop [] lexState

parseProgramLoop :: [Node] -> Parser [Node]
parseProgramLoop statements lexState@(lex, token) = case token of
  EOF -> (lexState, statements)
  FN ->
    let (nextLexState, node) = parseFunction lexState
     in parseProgramLoop (node : statements) nextLexState
  _ ->
    let (nextLexState, node) = parseStatement lexState
     in parseProgramLoop (node : statements) nextLexState

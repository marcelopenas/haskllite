module Parser.Program where

import Lexer (Lexer (..), getNext)
import Semantic ( Node(Block) )
import Token ( Token(EOF) )
import Parser.Parser ( Parser )
import Parser.Statement (parseStatement)

parseProgram :: Parser Node
parseProgram scanner =
  ((lastLex, lastToken), Semantic.Block nodes)
  where
    ((lastLex, lastToken), nodes) = parseProgramLoop [] scanner

parseProgramLoop :: [Node] -> Parser [Node]
parseProgramLoop statements (lex, token) = case token of
  Token.EOF -> ((lex, token), statements)
  _ -> ((newLex, newToken), newStatements)
  where
    ((nextLex, nextToken), node) = parseStatement (lex, token)
    nextStatements = node : statements
    ((newLex, newToken), newStatements) = parseProgramLoop nextStatements (nextLex, nextToken)

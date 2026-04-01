module Parser.Run (run) where

import Lexer (Lexer (Lexer), getNext)
import Parser.Parser ()
import Parser.Program (parseProgram)
import Semantic (Node)
import Token (Token (EOF))

run :: String -> Node
run source
  | finalToken == Token.EOF = node
  | otherwise = error $ "[Parser] Unexpected token at end of input: " ++ show finalToken
  where
    invalidLexer = Lexer source (-1)
    (initialLexer, next) = getNext invalidLexer
    ((finalLex, finalToken), node) = parseProgram (initialLexer, next)

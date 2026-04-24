module Parser.Run (run) where

import CompilerError (compilerParserError)
import Lexer (Lexer (Lexer), getNext)
import Node (Node)
import Parser.Program (parseProgram)
import Token (Token (EOF))

run :: String -> Node
run source
  | finalToken == Token.EOF = node
  | otherwise = compilerParserError finalLexState "Unexpected token at end of input"
  where
    (finalLexState@(_, finalToken), node) = parseProgram $ getNext $ Lexer source (-1) -- Invalid lexer, will result in fist position

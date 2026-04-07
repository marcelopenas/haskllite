module Parser.Run (run) where

import CompilerError (compilerParserError)
import Lexer (Lexer (Lexer), getNext)
import Parser.Program (parseProgram)
import Semantic (Node)
import Token (Token (EOF))

run :: String -> Node
run source
  | finalToken == Token.EOF = node
  | otherwise = let finalLexerState = (finalLex, finalToken) in compilerParserError finalLexerState "Unexpected token at end of input"
  where
    ((finalLex, finalToken), node) = parseProgram $ getNext $ Lexer source (-1) -- Invalid lexer, will result in fist position

module Parser.Run (run) where

import CompilerError (compilerParserError)
import Lexer (Lexer (Lexer), getNext)
import Node (Node (Block, FuncCall))
import Parser.Program (parseProgram)
import Token (Token (EOF))

run :: String -> Node
run source
  | finalToken == EOF = case node of
      Block statements -> Block (FuncCall "main" [] : statements)
      _ -> error "Unexpected top-level node, expected a block, this will only happen if the parser is broken"
  | otherwise = compilerParserError finalLexState "Unexpected token at end of input"
  where
    (finalLexState@(_, finalToken), node) = parseProgram $ getNext $ Lexer source (-1) -- Invalid lexer, will result in fist position

module Source.Parser.Run (run) where

import Source.CompilerError (compilerParserError)
import Source.Lexer (Lexer (Lexer), getNext)
import Source.Node (Node (Block, FuncCall))
import Source.Parser.Program (parseProgram)
import Source.Token (Token (EOF))

run :: String -> Node
run source
  | finalToken == EOF = case node of
      Block statements -> Block (FuncCall "main" [] : statements)
      _ -> error "Unexpected top-level node, expected a block, this will only happen if the parser is broken"
  | otherwise = compilerParserError finalLexState "Unexpected token at end of input"
  where
    (finalLexState@(_, finalToken), node) = parseProgram $ getNext $ Lexer source (-1) -- Invalid lexer, will result in fist position

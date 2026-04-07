module CompilerError (compilerParserError, compilerSemanticError, compilerLexerError) where

import {-# SOURCE #-} Lexer (Lexer (..), LexerState)
import Token (Token)

compilerParserError :: LexerState -> String -> a
compilerParserError (Lexer source position, token) message =
  error $ "[Parser] " ++ message ++ ", got: " ++ show token ++ " at: " ++ show position

compilerLexerError :: (Int, Char) -> String -> a
compilerLexerError (nextPos, nextChar) message =
  error $ "[Lexer] " ++ message ++ ", got " ++ show nextChar ++ " at: " ++ show nextPos

compilerSemanticError :: String -> a
compilerSemanticError message =
  error $ "[Semantic] " ++ message

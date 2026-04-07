module CompilerError (compilerError, CompilerError(..)) where

import Lexer (Lexer (..), LexerState)
import Token (Token)

data CompilerError = ParserError | LexerError

compilerError :: LexerState -> CompilerError -> String -> a
compilerError (Lexer source position, token) errorType message =
  error $ errorTypeMessage ++ message ++ ", got: " ++ show token ++ " at: " ++ show position
  where
    errorTypeMessage = case errorType of
      ParserError -> "[Parser] "
      LexerError -> "[Lexer] "

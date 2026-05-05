module CompilerError (compilerParserError, compilerSemanticError, compilerLexerError) where

import GHC.Stack (HasCallStack)
import {-# SOURCE #-} Lexer (Lexer (..), LexerState)
import Token (Token)

getCurrentLinePart :: Int -> [Char] -> [String]
getCurrentLinePart lineStart source = (lines (drop lineStart source) ++ [""])

getCurrentLine :: [String] -> String
getCurrentLine [] = error "[CompilerError] unable to get lines on error"
getCurrentLine (line:lines) = line

calculatePosition :: String -> Int -> (Int, Int, String)
calculatePosition source offset = (max 1 lineNum, max 1 colNum, currentLine)
  where
    prefix = take offset source
    lineNum = length (lines prefix)
    lineStart = case span (/= '\n') (reverse prefix) of
      (_, '\n' : rest) -> length rest + 1
      _ -> 0
    currentLine = getCurrentLine $ getCurrentLinePart lineStart source
    colNum = offset - lineStart + 1

formatSourceError :: String -> Int -> String
formatSourceError source offset =
  "Line "
    ++ show line
    ++ ", Column "
    ++ show col
    ++ ":\n"
    ++ "  | "
    ++ lineText
    ++ "\n"
    ++ "  | "
    ++ pointer
  where
    (line, col, lineText) = calculatePosition source offset
    pointer = replicate (col - 1) ' ' ++ "^"

compilerParserError :: (HasCallStack) => LexerState -> String -> a
compilerParserError (Lexer source position, token) message =
  error $
    "[Parser] " ++ "Unexpected token: " ++ show token ++ "\n" ++ message ++ "\n" ++ formatSourceError source position

compilerLexerError :: (HasCallStack) => String -> (Int, Char) -> String -> a
compilerLexerError source (nextPos, nextChar) message =
  error $
    "[Lexer] " ++ message ++ "\n" ++ formatSourceError source nextPos

compilerSemanticError :: (HasCallStack) => String -> a
compilerSemanticError message =
  error $ "[Semantic] " ++ message

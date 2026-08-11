module Source.CompilerError (compilerParserError, compilerSemanticError, compilerLexerError) where

import GHC.Stack (HasCallStack)
import {-# SOURCE #-} Source.Lexer (Lexer (..), LexerState)
import Source.Token (Token)

italic      = "\x1b[3m"
bold      = "\x1b[1m"
underline = "\x1b[4m"
reset     = "\x1b[0m"
red       = "\x1b[31m"
green     = "\x1b[32m"
yellow    = "\x1b[33m"
blue      = "\x1b[34m"
cyan      = "\x1b[36m"
redBold   = "\x1b[1;31m"
greenBold = "\x1b[1;32m"
blueBold  = "\x1b[1;34m"

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
    ++ blueBold ++ show line ++ reset
    ++ ", Column "
    ++ blueBold ++ show col ++ reset
    ++ ":\n"
    ++ cyan ++ "  | " ++ reset
    ++ lineText
    ++ "\n"
    ++ cyan ++ "  | " ++ reset
    ++ cyan ++ pointer ++ reset
  where
    (line, col, lineText) = calculatePosition source offset
    pointer = replicate (col - 1) ' ' ++ "^"

compilerParserError :: (HasCallStack) => LexerState -> String -> a
compilerParserError (Lexer source position, token) message =
  error $
    redBold ++ "[Parser]" ++ italic ++ " Unexpected token: " ++ reset ++ show token ++ "\n" ++ italic ++ message ++ reset ++ "\n" ++ formatSourceError source position

compilerLexerError :: (HasCallStack) => String -> (Int, Char) -> String -> a
compilerLexerError source (nextPos, nextChar) message =
  error $
    redBold ++ "[Lexer]" ++ italic ++ " " ++ message ++ reset ++"\n" ++ formatSourceError source nextPos

compilerSemanticError :: (HasCallStack) => String -> a
compilerSemanticError message =
  error $ redBold ++ "[Semantic]" ++ italic ++ " " ++ message ++ reset

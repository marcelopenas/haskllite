module Lexer (Lexer (..), LexerState, getNext) where

import CompilerError (compilerLexerError)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Token (Token (..), VarType (..))

type Source = String

type Position = Int

data Lexer = Lexer Source Position deriving (Show)

type LexerState = (Lexer, Token)

isIdentifier :: Char -> Bool
isIdentifier c = isAlphaNum c || c == '_' || c == '!'

insInt :: Char -> Bool
insInt = isDigit

getNextLex :: Lexer -> Lexer
getNextLex (Lexer source position) = Lexer source (position + 1)

getPrevLex :: Lexer -> Lexer
getPrevLex (Lexer source position) = Lexer source (position - 1)

getCharLex :: Lexer -> Char
getCharLex (Lexer source position)
  | position < length source = source !! position
  | otherwise = compilerLexerError (position, source !! (position - 1)) "getChar beyond source"

emptyBuilder :: String
emptyBuilder = ""

getNext :: Lexer -> (Lexer, Token)
getNext currentLex@(Lexer source position)
  | length source <= nextPos = (nextLex, EOF)
  | isSpace nextChar = getNext nextLex -- If space or \n continue
  | isAlpha nextChar = case identifierLexState of
      (newLex, IDENTIFIER "println!") -> (newLex, PRINT)
      (newLex, IDENTIFIER "let") -> (newLex, LET)
      (newLex, IDENTIFIER "mut") -> (newLex, MUT)
      (newLex, IDENTIFIER "if") -> (newLex, IF)
      (newLex, IDENTIFIER "while") -> (newLex, WHILE)
      (newLex, IDENTIFIER "else") -> (newLex, ELSE)
      (newLex, IDENTIFIER "scanln!") -> (newLex, SCAN)
      (newLex, IDENTIFIER "true") -> (newLex, BOOLEAN True)
      (newLex, IDENTIFIER "false") -> (newLex, BOOLEAN False)
      (newLex, IDENTIFIER "str") -> (newLex, TYPE StrT)
      (newLex, IDENTIFIER "i32") -> (newLex, TYPE I32T)
      (newLex, IDENTIFIER "bool") -> (newLex, TYPE BooleanT)
      _ -> identifierLexState
  | isDigit nextChar = getNextParse nextLex (INT 0) insInt emptyBuilder
  | otherwise = case nextChar of
      '+' -> (nextLex, PLUS)
      '-' -> (nextLex, MINUS)
      '^' -> (nextLex, XOR)
      '!' -> (nextLex, NOT)
      '/' -> (nextLex, DIV)
      '(' -> (nextLex, OPEN_PAR)
      ')' -> (nextLex, CLOSE_PAR)
      '{' -> (nextLex, OPEN_BRA)
      '}' -> (nextLex, CLOSE_BRA)
      '>' -> (nextLex, GREATER)
      '<' -> (nextLex, LESSER)
      ';' -> (nextLex, END)
      ':' -> (nextLex, TYPE_ASSIGN)
      '*' -> case nextNextChar of
        '*' -> (nextNextLex, POWER)
        _ -> (nextLex, MULT)
      '=' -> case nextNextChar of
        '=' -> (nextNextLex, EQUAL)
        _ -> (nextLex, ASSIGN)
      '&' -> case nextNextChar of
        '&' -> (nextNextLex, AND)
        _ -> compilerLexerError (nextPos, nextChar) "Invalid token at position"
      '|' -> case nextNextChar of
        '|' -> (nextNextLex, OR)
        _ -> compilerLexerError (nextPos, nextChar) "Invalid token at position"
      '\"' -> getNextParseString nextLex emptyBuilder
      _ -> compilerLexerError (nextPos, nextChar) "Invalid token at position"
  where
    nextLex = getNextLex currentLex
    nextChar = getCharLex nextLex
    (Lexer _ nextPos) = nextLex
    nextNextLex = getNextLex nextLex
    nextNextChar = getCharLex nextNextLex
    (Lexer _ nextNextPos) = nextNextLex
    identifierLexState = getNextParse nextLex (IDENTIFIER emptyBuilder) isIdentifier emptyBuilder

getNextParseString :: Lexer -> String -> LexerState
getNextParseString currentLex@(Lexer source position) building = case nextChar of
  '\"' -> (nextLex, STR $ reverse building)
  _
    | (position + 1) < length source -> getNextParseString nextLex (nextChar : building)
    | otherwise -> compilerLexerError (position, source !! (position - 1)) "EOL on string" -- FIXME this never happens, why???
  where
    nextLex = getNextLex currentLex
    nextChar = getCharLex nextLex

getNextParse :: Lexer -> Token -> (Char -> Bool) -> String -> LexerState
getNextParse currentLex@(Lexer source position) token isIdentifier building = case token of
  IDENTIFIER _
    | position >= length source -> (currentLex, IDENTIFIER building)
    | isIdentifier currentChar -> getNextParse nextLex (IDENTIFIER "") isIdentifier (building ++ [currentChar])
    | otherwise -> (prevLex, IDENTIFIER building)
  INT _
    | position >= length source -> (currentLex, INT (read building :: Int))
    | isIdentifier currentChar -> getNextParse nextLex (INT 0) isIdentifier (building ++ [currentChar])
    | otherwise -> (prevLex, INT (read building :: Int))
  where
    nextLex = getNextLex currentLex
    prevLex = getPrevLex currentLex
    currentChar = getCharLex currentLex

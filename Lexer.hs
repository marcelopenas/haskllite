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

getNext :: Lexer -> (Lexer, Token)
getNext (Lexer source position)
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
    nextPos = position + 1
    nextChar = source !! nextPos
    nextLex = Lexer source nextPos

    emptyBuilder = ""

    nextNextPos = nextPos + 1
    nextNextChar = source !! nextNextPos
    nextNextLex = Lexer source nextNextPos

    identifierLexState = getNextParse nextLex (IDENTIFIER emptyBuilder) isIdentifier emptyBuilder

getNextParseString :: Lexer -> String -> LexerState
getNextParseString (Lexer source position) building = case nextChar of
  '\"' -> (nextLex, STR $ reverse building)
  _ -> getNextParseString nextLex (nextChar : building)
  where
    nextChar = source !! nextPos
    nextPos = position + 1
    nextLex = Lexer source nextPos

getNextParse :: Lexer -> Token -> (Char -> Bool) -> String -> LexerState
getNextParse (Lexer source position) token isIdentifier building = case token of
  IDENTIFIER _
    | position >= length source -> (currentLex, IDENTIFIER building)
    | isIdentifier currentChar -> getNextParse nextLex (IDENTIFIER "") isIdentifier (building ++ [currentChar])
    | otherwise -> (prevLex, IDENTIFIER building)
  INT _
    | position >= length source -> (currentLex, INT (read building :: Int))
    | isIdentifier currentChar -> getNextParse nextLex (INT 0) isIdentifier (building ++ [currentChar])
    | otherwise -> (prevLex, INT (read building :: Int))
  where
    currentLex = Lexer source position
    nextLex = Lexer source (position + 1)
    prevLex = Lexer source (position - 1)

    currentChar = source !! position

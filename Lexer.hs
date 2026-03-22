module Lexer
  ( Lexer (..),
    getNext,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Token

type Source = String

type Position = Int

data Lexer = Lexer Source Position deriving (Show)

type LexerState = (Lexer, Token)

getNext :: Lexer -> (Lexer, Token)
getNext (Lexer source position)
  | length source <= nextPos = newLexerState Token.EOF
  | nextCharEq '+' = newLexerState Token.PLUS
  | nextCharEq '-' = newLexerState Token.MINUS
  | nextCharEq '^' = newLexerState Token.XOR
  | nextCharEq '*' = getNextParseStar newLexPos
  | nextCharEq '/' = newLexerState Token.DIV
  | nextCharEq '(' = newLexerState Token.OPEN_PAR
  | nextCharEq ')' = newLexerState Token.CLOSE_PAR
  | nextCharEq '=' = newLexerState Token.ASSIGN
  | nextCharEq ';' = newLexerState Token.END
  | isAlpha nextChar = case getNextParseIdentifier newLexPos emptyBuilder of
      (Lexer _ newPos, Token.IDENTIFIER "println!") -> (newLex newPos, Token.PRINT)
      lexWithIdentifier -> lexWithIdentifier
  | isDigit nextChar = getNextParseInt newLexPos emptyBuilder
  | isSpace nextChar = getNext newLexPos -- If space or \n continue
  | otherwise = error $ "[Lexer] invalid token at position " ++ show nextPos ++ "got " ++ show nextChar
  where
    nextPos = position + 1
    nextChar = source !! nextPos

    newLex = Lexer source
    newLexPos = newLex nextPos
    newLexerState :: Token -> LexerState
    newLexerState t = (newLexPos, t)

    nextCharEq :: Char -> Bool
    nextCharEq c = nextChar == c

    emptyBuilder = ""

getNextParseStar :: Lexer -> LexerState
getNextParseStar (Lexer source position) = case nextChar of
  '*' -> (newLex nextPos, Token.POWER)
  _ -> (newLex position, Token.MULT)
  where
    nextPos = position + 1
    nextChar = source !! nextPos
    newLex = Lexer source

{-
For functions that look for len > 1 tokens:
Receives current pos and works from there, returns: end of token +1 = pos
On base: position - 1, since the loop preemptively adds 1, when its over it will be on the next char, but finished
-}

getNextParseIdentifier :: Lexer -> String -> LexerState
getNextParseIdentifier (Lexer source position) buildingIdentifier
  | position >= length source =
      (Lexer source position, Token.IDENTIFIER buildingIdentifier)
  | isAlphaNumUnderscore currentChar =
      getNextParseIdentifier
        (Lexer source (position + 1))
        (buildingIdentifier ++ [currentChar])
  | otherwise =
      (Lexer source (position - 1), Token.IDENTIFIER buildingIdentifier)
  where
    isAlphaNumUnderscore c = isAlphaNum c || c == '_'
    currentChar = source !! position

getNextParseInt :: Lexer -> String -> LexerState
getNextParseInt (Lexer source position) buildingInt
  | position >= length source =
      (Lexer source position, Token.INT (read buildingInt :: Int))
  | isDigit currentChar =
      getNextParseInt
        (Lexer source (position + 1))
        (buildingInt ++ [currentChar])
  | otherwise =
      (Lexer source (position - 1), Token.INT (read buildingInt :: Int))
  where
    currentChar = source !! position

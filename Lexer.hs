module Lexer
  ( Lexer (..),
    getNext,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Token

data Lexer = Lexer
  { source :: String,
    position :: Int,
    next :: Token
  }
  deriving (Show)

getNext :: Lexer -> Lexer
getNext (Lexer source position next)
  | length source <= nextPos = newLexPos Token.EOF
  | nextCharEq '+' = newLexPos Token.PLUS
  | nextCharEq '-' = newLexPos Token.MINUS
  | nextCharEq '^' = newLexPos Token.XOR
  | nextCharEq '*' = getNextParseStar newLexNext
  | nextCharEq '/' = newLexPos Token.DIV
  | nextCharEq '(' = newLexPos Token.OPEN_PAR
  | nextCharEq ')' = newLexPos Token.CLOSE_PAR
  | nextCharEq '=' = newLexPos Token.ASSIGN
  | nextCharEq ';' = newLexPos Token.END
  | isAlpha nextChar = case getNextParseIdentifier newLexNext emptyBuilder of
      Lexer _ newPos (Token.IDENTIFIER "println!") -> newLex newPos Token.PRINT
      lexWithIdentifier -> lexWithIdentifier
  | isDigit nextChar = getNextParseInt newLexNext emptyBuilder
  | isSpace nextChar = getNext newLexNext -- If space or \n proceed to next position
  | otherwise = error $ "[Lexer] invalid token at position " ++ show position ++ "got " ++ show nextChar
  where
    nextPos = position + 1
    nextChar = source !! nextPos
    newLex = Lexer source
    newLexPos = Lexer source nextPos
    newLexNext = newLexPos next
    nextCharEq c = nextChar == c
    emptyBuilder = ""

getNextParseStar :: Lexer -> Lexer
getNextParseStar (Lexer source position next) = case nextChar of
  '*' -> newLex Token.POWER
  _ -> currentLex Token.MULT
  where
    nextPos = position + 1
    nextChar = source !! nextPos
    newLex = Lexer source nextPos
    currentLex = Lexer source position

-- Receives current pos and works from there, returns: end of identifier +1 = pos
getNextParseIdentifier :: Lexer -> String -> Lexer
getNextParseIdentifier (Lexer source position next) buildingIdentifier
  | position >= length source =
      Lexer source position (Token.IDENTIFIER (read buildingIdentifier))
  | isAlphaNumUnderscore currentChar =
      getNextParseIdentifier
        (Lexer source (position + 1) (Token.IDENTIFIER "\0")) -- "\0" represents building int token, since it will be replaced by the actual int value when the int is finished
        (buildingIdentifier ++ [currentChar])
  | otherwise =
      -- position - 1, since the loop preemptively adds 1, when its over it will be on the next char, but finished
      Lexer source (position - 1) (Token.IDENTIFIER (read buildingIdentifier))
  where
    isAlphaNumUnderscore c = isAlphaNum c || c == '_'
    currentChar = source !! position

-- Receives current pos and works from there, returns: end of int +1 = pos
getNextParseInt :: Lexer -> String -> Lexer
getNextParseInt (Lexer source position next) buildingInt
  | position >= length source =
      Lexer source position (Token.INT (read buildingInt))
  | isDigit currentChar =
      getNextParseInt
        (Lexer source (position + 1) (Token.INT 0)) -- 0 represents building int token, since it will be replaced by the actual int value when the int is finished
        (buildingInt ++ [currentChar])
  | otherwise =
      -- position - 1, since the loop preemptively adds 1, when its over it will be on the next char, but finished
      Lexer source (position - 1) (Token.INT (read buildingInt))
  where
    currentChar = source !! position

module Lexer
  ( Lexer (..),
    getNext,
  )
where

import Data.Char (isDigit)
import Token (Token (..))
import Token qualified

data Lexer = Lexer
  { source :: String,
    position :: Int,
    next :: Token
  }
  deriving (Show)

getNext :: Lexer -> Lexer
getNext (Lexer source position next)
  | length source <= nextPos = newLex Token.EOF
  | nextChar == '\0' = newLex Token.EOF
  | nextChar == '+' = newLex Token.PLUS
  | nextChar == '-' = newLex Token.MINUS
  | nextChar == '^' = newLex Token.XOR
  | nextChar == '*' = getNextParseStar (newLex next)
  | nextChar == '/' = newLex Token.DIV
  | nextChar == '(' = newLex Token.OPEN_PAR
  | nextChar == ')' = newLex Token.CLOSE_PAR
  | isDigit nextChar =
      getNextParseInt (newLex next) ""
  | nextChar == ' ' = getNext (newLex next) -- If space proceed to next position
  | otherwise = error $ "[Lexer] invalid token at position " ++ show position ++ "got " ++ show nextChar
  where
    nextPos = position + 1
    nextChar = source !! nextPos
    newLex = Lexer source nextPos

getNextParseStar :: Lexer -> Lexer
getNextParseStar (Lexer source position next)
  -- | nextChar == '*' = newLex Token.POWER
  | otherwise = currentLex Token.MULT
  where
    nextPos = position + 1
    nextChar = source !! nextPos
    newLex = Lexer source nextPos
    currentLex = Lexer source position

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

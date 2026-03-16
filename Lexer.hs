module Lexer
  ( Lexer (..),
    getNext,
  )
where

import Data.Char (isDigit, isSpace)
import Token (Token (..))

data Lexer = Lexer
  { source :: String,
    position :: Int,
    next :: Token
  }
  deriving (Show)

getNext :: Lexer -> Lexer
getNext (Lexer source position next)
  | length source <= nextPos = newLex Token.EOF
  | nextCharEq '\0' = newLex Token.EOF
  | nextCharEq '+' = newLex Token.PLUS
  | nextCharEq '-' = newLex Token.MINUS
  | nextCharEq '^' = newLex Token.XOR
  | nextCharEq '*' = getNextParseStar (newLex next)
  | nextCharEq '/' = newLex Token.DIV
  | nextCharEq '(' = newLex Token.OPEN_PAR
  | nextCharEq ')' = newLex Token.CLOSE_PAR
  | isDigit nextChar =
      getNextParseInt (newLex next) ""
  | isSpace nextChar = getNext (newLex next) -- If space proceed to next position
  | otherwise = error $ "[Lexer] invalid token at position " ++ show position ++ "got " ++ show nextChar
  where
    nextPos = position + 1
    nextChar = source !! nextPos
    newLex = Lexer source nextPos
    nextCharEq c = nextChar == c

getNextParseStar :: Lexer -> Lexer
getNextParseStar (Lexer source position next) = case nextChar of
  '*' -> newLex Token.POWER
  _ -> currentLex Token.MULT
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

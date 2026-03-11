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
  | length source <= nextPos = newLex $ Token Token.EOF (Left "\0")
  | nextChar == '\0' = newLex $ Token Token.EOF (Left "\0")
  | nextChar == '+' = newLex $ Token Token.PLUS (Left "+")
  | nextChar == '-' = newLex $ Token Token.MINUS (Left "-")
  | nextChar == '^' = newLex $ Token Token.XOR (Left "^")
  | nextChar == '*' = newLex $ Token Token.MULT (Left "*")
  | nextChar == '/' = newLex $ Token Token.DIV (Left "/")
  | nextChar == '(' = newLex $ Token Token.OPEN_PAR (Left "(")
  | nextChar == ')' = newLex $ Token Token.CLOSE_PAR (Left ")")
  | isDigit nextChar =
      getNextParseInt (newLex next) ""
  | nextChar == ' ' = getNext (newLex next) -- If space proceed to next position
  | otherwise = error $ "[Lexer] invalid token at position " ++ show position ++ "got " ++ show nextChar
  where
    nextPos = position + 1
    nextChar = source !! nextPos
    newLex = Lexer source nextPos

-- Receives current pos and works from there, returns: end of int +1 = pos
getNextParseInt :: Lexer -> String -> Lexer
getNextParseInt (Lexer source position next) buildingInt
  | position >= length source =
      Lexer source position (Token Token.INT (Right $ read buildingInt))
  | isDigit currentChar =
      getNextParseInt
        ( Lexer source (position + 1) (Token Token.INT (Left (buildingInt ++ [currentChar])))
        )
        (buildingInt ++ [currentChar])
  | otherwise =
      -- position - 1, since the loop preemptively adds 1, when its over it will be on the next char, but finished
      Lexer source (position - 1) (Token Token.INT (Right $ read buildingInt))
  where
    currentChar = source !! position

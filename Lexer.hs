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
  } deriving (Show)

getNext :: Lexer -> Lexer
getNext (Lexer source position next)
  | length source <= nextPos = Lexer source nextPos (Token Token.EOF (Left "\0"))
  | nextChar == '\0' = Lexer source nextPos (Token Token.EOF (Left "\0"))
  | nextChar == '+' = Lexer source nextPos (Token Token.PLUS (Left "+"))
  | nextChar == '-' = Lexer source nextPos (Token Token.MINUS (Left "-"))
  | nextChar == '^' = Lexer source nextPos (Token Token.XOR (Left "^"))
  | nextChar == '*' = Lexer source nextPos (Token Token.MULT (Left "*"))
  | nextChar == '/' = Lexer source nextPos (Token Token.DIV (Left "/"))
  | isDigit nextChar =
      getNextParseInt (Lexer source nextPos next) ""
  | nextChar == ' ' = getNext (Lexer source nextPos next) -- If space proceed to next position
  | otherwise = error "[Lexer] invalid token"
  where
    nextChar = source !! (position + 1)
    nextPos = position + 1

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

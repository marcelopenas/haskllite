module Lexer
  ( Lexer (..),
    selectNext,
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

selectNext :: Lexer -> Lexer
selectNext (Lexer source position next)
  | length source <= nextPos = Lexer source nextPos (Token Token.EOF (Left "\0"))
  | nextChar == '\0' = Lexer source nextPos (Token Token.EOF (Left "\0"))
  | nextChar == '+' = Lexer source nextPos (Token Token.PLUS (Left "+"))
  | nextChar == '-' = Lexer source nextPos (Token Token.MINUS (Left "-"))
  | isDigit nextChar =
      selectNextParseInt (Lexer source nextPos next) ""
  | nextChar == ' ' = selectNext (Lexer source nextPos next) -- If space proceed to next position
  | otherwise = error "Error: invalid token"
  where
    nextChar = source !! (position + 1)
    nextPos = position + 1

-- Receives current pos and works from there, returns: end of int +1 = pos
-- FIXME should be a way to not need to check if string ended twice, but number parsing would probably need to not be in this helper function
selectNextParseInt :: Lexer -> String -> Lexer
selectNextParseInt (Lexer source position next) buildingInt
  | position >= length source =
      Lexer source position (Token Token.INT (Right $ read buildingInt))
  | isDigit currentChar =
      selectNextParseInt
        ( Lexer source (position + 1) (Token Token.INT (Left (buildingInt ++ [currentChar])))
        )
        (buildingInt ++ [currentChar])
  | otherwise =
      Lexer source (position - 1) (Token Token.INT (Right $ read buildingInt))
      -- position - 1, since the loop preemptively adds 1, when its over it will be on the next char, but finished
  where
    currentChar = source !! position

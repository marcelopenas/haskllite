module Token
  ( Token (..),
  )
where

data Token
  = INT Int
  | PLUS
  | MINUS
  | XOR
  | MULT
  | DIV
  -- | POWER
  | OPEN_PAR
  | CLOSE_PAR
  | EOF
  deriving (Show, Eq)
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
  | FACT
  | OPEN_PAR
  | CLOSE_PAR
  | EOF
  deriving (Show, Eq)
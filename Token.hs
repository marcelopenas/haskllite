module Token
  ( Token (..),
  )
where

type Name = String

type Immutable = Bool

data Token
  = INT Int
  | PLUS
  | MINUS
  | XOR
  | MULT
  | DIV
  | POWER
  | OPEN_PAR
  | CLOSE_PAR
  | ASSIGN
  | END
  | PRINT
  | LET
  | IDENTIFIER Name
  | EOF
  deriving (Show, Eq)
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
  | AND
  | OR
  | NOT
  | EQUAL
  | GREATER
  | LESSER
  | OPEN_PAR
  | CLOSE_PAR
  | OPEN_BRA
  | CLOSE_BRA
  | IF
  | WHILE
  | ELSE
  | SCAN
  | ASSIGN
  | END
  | PRINT
  | LET
  | IDENTIFIER Name
  | EOF
  deriving (Show, Eq)
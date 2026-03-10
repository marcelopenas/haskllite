module Token
  ( Token (..),
    Kind (..),
    Value (..),
  )
where

data Kind
  = INT
  | PLUS
  | MINUS
  | XOR
  | MULT
  | DIV
  | POWER
  | OPEN_PAR
  | CLOSE_PAR
  | EOF
  deriving (Show, Eq, Enum, Bounded)

type Value = Either String Int

data Token
  = Token
  { kind :: Kind,
    value :: Value
  }
  deriving (Show)

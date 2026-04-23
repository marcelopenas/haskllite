module Token
  ( Token (..),
    VarType (..),
  )
where

type Name = String

type Immutable = Bool

data VarType
  = I32T
  | F64T
  | BooleanT
  | StrT
  | UnityT
  deriving (Show, Eq)

data Token
  = INT Int
  | FLOAT Float
  | COMMA
  | PERIOD
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
  | FOR
  | ELSE
  | SCAN
  | ASSIGN
  | END
  | PRINT
  | FN
  | ARROW
  | RETURN
  | LET
  | MUT
  | TYPE VarType
  | TYPE_ASSIGN
  | BOOLEAN Bool
  | STR String
  | IDENTIFIER Name
  | EOF
  deriving (Show, Eq)
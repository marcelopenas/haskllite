module Parser
  ( run,
  )
where

import Lexer (Lexer (..), selectNext)
import Token (Kind (..), Token (..), Value (..))

getNextKind :: Lexer -> Token.Kind
getNextKind lex = kind (next lex)

getNextValue :: Lexer -> Token.Value
getNextValue lex = value (next lex)

getValueInt :: Token.Value -> Int
getValueInt (Right val) = val
getValueInt _ = error "Error: NaN" -- Should only occur if token 'kind' is INT and 'value' is of type string

run :: String -> Int
run source =
  -- Is EOF represents initial non existent token to pass to getNext to acquire first actual token
  parseExpression (selectNext $ Lexer source (-1) (Token Token.EOF (Left "\0")))

parseExpression :: Lexer -> Int -- Receives INITIAL lexer
parseExpression lex
  | getNextKind lex == Token.INT = snd (parseExpressionInt (selectNext lex) (getValueInt $ getNextValue lex))
  | getNextKind lex == Token.PLUS = error "Error: must not start with operand +"
  | getNextKind lex == Token.MINUS = error "Error: must not start with operand -"
  | getNextKind lex == Token.EOF = 0
  | otherwise = error "Error: invalid token"

-- TODO recursion between these two

parseExpressionInt :: Lexer -> Int -> (Lexer, Int) -- When value was Int and now needs to be operand
parseExpressionInt lex val
  | getNextKind lex == Token.EOF = (lex, val)
  | getNextKind lex == Token.PLUS = parseExpressionOperand (selectNext lex) val Token.PLUS
  | getNextKind lex == Token.MINUS = parseExpressionOperand (selectNext lex) val Token.MINUS
  | getNextKind lex == Token.INT = error "Error: expected operand + or -"
  | otherwise = error "Error: No token matched"

parseExpressionOperand :: Lexer -> Int -> Token.Kind -> (Lexer, Int) -- When value was Int and now needs to be operand
parseExpressionOperand lex val operand
  | operand == Token.PLUS =
      (selectNext lex, val + snd (parseExpressionInt lex val))
  | operand == Token.MINUS =
      (selectNext lex, val - getValueInt (getNextValue lex))
  | otherwise = error "Error: must have integer after operand"

module Parser
  ( run,
  )
where

import Lexer (Lexer (..), selectNext)
import Token (Kind (..), Token (..), Value (..))

-- TODO change names to better reflect fuctions, selectNext -> getActualNextValNewLex, getNext -> evalueateNextValueDontChangeLex

getNextKind :: Lexer -> Token.Kind
getNextKind lex = kind (next lex)

getNextValue :: Lexer -> Token.Value
getNextValue lex = value (next lex)

getValueInt :: Token.Value -> Int
getValueInt (Right val) = val
getValueInt _ = error "[Lexer] Token of type INT presenting NaN value" -- Should only occur if token 'kind' is INT and 'value' is of type string, eg lexer made a mistake

run :: String -> Int
run source =
  snd $ parseExpression (selectNext $ Lexer source (-1) (Token Token.EOF (Left "\0")))

-- EOF represents initial non existent token for passing to getNext to get actual first token

parseExpression :: Lexer -> (Lexer, Int)
parseExpression lex
  | getNextKind lex == Token.INT = parseExpression' (selectNext lex) (getValueInt $ getNextValue lex)
  | getNextKind lex == Token.PLUS = error "[Parser] expected INT"
  | getNextKind lex == Token.MINUS = error "[Parser] expected INT"
  | getNextKind lex == Token.EOF = error "[Parser] expected INT"
  | otherwise = error "[Parser] invalid token"

parseExpression' :: Lexer -> Int -> (Lexer, Int)
parseExpression' lex val
  | getNextKind lex == Token.EOF = (lex, val)
  | getNextKind lex == Token.PLUS =
      if getNextKind (selectNext lex) == Token.INT
        then
          parseExpression' (selectNext (selectNext lex)) (val + getValueInt (getNextValue (selectNext lex)))
        else error "[Parser] expected INT after +"
  | getNextKind lex == Token.MINUS =
      if getNextKind (selectNext lex) == Token.INT
        then
          parseExpression' (selectNext (selectNext lex)) (val - getValueInt (getNextValue (selectNext lex)))
        else error "[Parser] expected INT after -"
  | getNextKind lex == Token.INT = error "[Parser] expected OPERAND + or -"
  | otherwise = error "[Parser] invalid token"

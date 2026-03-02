module Parser
  ( run,
  )
where

import Lexer (Lexer (..), getNext)
import Token (Kind (..), Token (..), Value (..))
import Data.Bits (Bits(xor))

evaluateNextKind :: Lexer -> Token.Kind
evaluateNextKind lex = kind (next lex)

evaluateNextValue :: Lexer -> Token.Value
evaluateNextValue lex = value (next lex)

evaluateValueInt :: Token.Value -> Int
evaluateValueInt (Right val) = val
evaluateValueInt _ = error "[Lexer] Token of type INT presenting NaN value" -- Should only occur if token 'kind' is INT and 'value' is of type string, eg lexer made a mistake

-- EOF represents initial non existent token for passing to evaluateNext to get actual first token
run :: String -> Int
run source =
  snd $ parseExpression (getNext $ Lexer source (-1) (Token Token.EOF (Left "\0")))

parseExpression :: Lexer -> (Lexer, Int)
parseExpression lex
  | evaluateNextKind lex == Token.INT = parseExpression' (getNext lex) (evaluateValueInt $ evaluateNextValue lex)
  | evaluateNextKind lex == Token.PLUS = error "[Parser] expected INT"
  | evaluateNextKind lex == Token.MINUS = error "[Parser] expected INT"
  | evaluateNextKind lex == Token.XOR = error "[Parser] expected INT"
  | evaluateNextKind lex == Token.EOF = error "[Parser] expected INT"
  | otherwise = error "[Parser] invalid token"

parseExpression' :: Lexer -> Int -> (Lexer, Int)
parseExpression' lex val
  | evaluateNextKind lex == Token.EOF = (lex, val)
  | evaluateNextKind lex == Token.PLUS =
      if evaluateNextKind (getNext lex) == Token.INT
        then
          parseExpression' (getNext (getNext lex)) (val + evaluateValueInt (evaluateNextValue (getNext lex)))
        else error "[Parser] expected INT after +"
  | evaluateNextKind lex == Token.MINUS =
      if evaluateNextKind (getNext lex) == Token.INT
        then
          parseExpression' (getNext (getNext lex)) (val - evaluateValueInt (evaluateNextValue (getNext lex)))
        else error "[Parser] expected INT after -"
  | evaluateNextKind lex == Token.XOR =
      if evaluateNextKind (getNext lex) == Token.INT
        then
          parseExpression' (getNext (getNext lex)) (val `xor` evaluateValueInt (evaluateNextValue (getNext lex)))
        else error "[Parser] expected INT after ^"
  | evaluateNextKind lex == Token.INT = error "[Parser] expected OPERAND + or -"
  | otherwise = error "[Parser] invalid token"

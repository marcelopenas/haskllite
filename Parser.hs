module Parser
  ( run,
  )
where

import Data.Bits (Bits (xor))
import Lexer (Lexer (..), getNext)
import Token (Kind (..), Token (..), Value (..))

evalNextKind :: Lexer -> Token.Kind
evalNextKind lex = kind (next lex)

evalNextValue :: Lexer -> Token.Value
evalNextValue lex = value (next lex)

evalValueInt :: Token.Value -> Int
evalValueInt (Right val) = val
evalValueInt _ = error "[Lexer] Token of type INT presenting NaN value" -- If token 'kind' is INT and 'value' is of type string, lexer made a mistake

-- EOF represents initial non existent token for passing to evalNext to get actual first token
run :: String -> Int
run source =
  snd $ parseExpression (getNext $ Lexer source (-1) (Token Token.EOF (Left "\0")))

parseExpression :: Lexer -> (Lexer, Int)
parseExpression lex
  | evalNextKind lex == Token.PLUS = error "[Parser] expected INT"
  | evalNextKind lex == Token.MINUS = error "[Parser] expected INT"
  | evalNextKind lex == Token.XOR = error "[Parser] expected INT"
  | evalNextKind lex == Token.EOF = error "[Parser] expected INT"
  | otherwise = parseTerm lex

parseExpression' :: Lexer -> Int -> (Lexer, Int)
parseExpression' lex acc
  | evalNextKind lex == Token.PLUS =
      (getNext lex, acc + result)
  | evalNextKind lex == Token.MINUS =
      (getNext lex, acc - result)
  | evalNextKind lex == Token.XOR =
      (getNext lex, acc `xor` result)
  | evalNextKind lex == Token.INT = error "[Parser] expected OPERAND (+, -, ^)"
  | otherwise = (lex, acc)
  where
    result = snd (parseExpression (getNext lex))

parseTerm :: Lexer -> (Lexer, Int)
parseTerm lex
  | evalNextKind lex == Token.MULT = error "[Parser] expected INT"
  | evalNextKind lex == Token.DIV = error "[Parser] expected INT"
  | evalNextKind lex == Token.INT = parseTerm' (getNext lex) intVal
  | otherwise = error "[Parser] invalid token"
  where
    intVal = evalValueInt $ evalNextValue lex

parseTerm' :: Lexer -> Int -> (Lexer, Int)
parseTerm' lex acc
  | evalNextKind lex == Token.MULT =
      (getNext lex, acc * result)
  | evalNextKind lex == Token.DIV =
      (getNext lex, acc `div` result)
  | evalNextKind lex == Token.INT = error "[Parser] expected OPERAND (*, /)"
  | otherwise = parseExpression' lex acc
  where
    result = snd (parseExpression (getNext lex))

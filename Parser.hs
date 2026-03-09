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
  snd (parseExpression initialLexer initialVal)
  where
    invalidLexer = Lexer source (-1) (Token Token.EOF (Left "\0"))
    (initialLexer, initialVal) = parseInt $ getNext invalidLexer

parseExpression :: Lexer -> Int -> (Lexer, Int)
parseExpression lex acc
  | evalNextKind lex == Token.EOF = (lex, acc)
  | otherwise =
      uncurry parseTerm (parseTermOperand lex acc)

parseExpressionOperand :: Lexer -> Int -> (Lexer, Int)
parseExpressionOperand lex acc
  | evalNextKind lex == Token.PLUS = (nextLex, acc + nexVal)
  | evalNextKind lex == Token.MINUS = (nextLex, acc - nexVal)
  | evalNextKind lex == Token.XOR = (nextLex, acc `xor` nexVal)
  | evalNextKind lex == Token.INT = error "[Parser] expected OPERAND (+, -, ^)"
  | otherwise = (lex, acc)
  where
    (nextLex, nexVal) = parseInt $ getNext lex

parseTerm :: Lexer -> Int -> (Lexer, Int)
parseTerm lex acc
  -- | evalNextKind lex == Token.EOF = (lex, acc)
  | evalNextKind lex == Token.INT = parseTermOperand (getNext lex) intVal
  | otherwise = parseExpressionOperand lex acc
  -- | otherwise = (lex, acc)
  -- | otherwise = error "[Parser] expected INT"
  -- | otherwise = parseTermOperand (getNext lex) intVal
  where
    intVal = evalValueInt $ evalNextValue lex

parseTermOperand :: Lexer -> Int -> (Lexer, Int)
parseTermOperand lex acc
  | evalNextKind lex == Token.MULT =
      (nextLex, acc * nextVal)
  | evalNextKind lex == Token.DIV =
      (nextLex, acc `div` nextVal)
  | evalNextKind lex == Token.INT = error "[Parser] expected OPERAND (*, /)"
  -- | otherwise = parseTerm lex acc
  | otherwise = (lex, acc)
  -- | otherwise = parseTermOperand lex acc
  where
    (nextLex, nextVal) = parseTerm (getNext lex) acc

parseInt :: Lexer -> (Lexer, Int)
parseInt lex
  | evalNextKind lex == Token.INT = (nextLex, intVal)
  | otherwise = error "[Parser] expected INT"
  where
    nextLex = getNext lex
    intVal = evalValueInt $ evalNextValue lex

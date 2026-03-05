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
run :: String -> (Lexer, Int)
run source =
  parseExpression initialLexer initialVal
  where
    invalidLexer = Lexer source (-1) (Token Token.INT (Right 42))
    (initialLexer, initialVal) = parseExpressionInt $ getNext invalidLexer

parseExpression :: Lexer -> Int -> (Lexer, Int)
parseExpression lex val
  | evalNextKind lex == Token.EOF = (lex, val)
  | otherwise =
      -- \| otherwise = do
      -- let newLex = getNext lex
      -- let (newLex, newVal) = parseExpressionOperand lex val
      -- (lex, val)
      -- (newLex, newVal)
      -- parseExpression lex newVal
      uncurry parseExpression (parseExpressionOperand lex val)

parseExpressionInt :: Lexer -> (Lexer, Int)
parseExpressionInt lex
  | evalNextKind lex == Token.INT = (nextLex, intVal)
  | otherwise = error "[Parser] expected INT"
  where
    nextLex = getNext lex
    intVal = evalValueInt $ evalNextValue lex

parseExpressionOperand :: Lexer -> Int -> (Lexer, Int)
parseExpressionOperand lex acc
  | evalNextKind lex == Token.PLUS =
      do
        -- (lex, acc)
        let (nextLex, nexVal) = parseExpressionInt $ getNext lex
        (nextLex, acc + nexVal)
  | evalNextKind lex == Token.MINUS =
      do
        let (nextLex, nexVal) = parseExpressionInt $ getNext lex
        (nextLex, acc - nexVal)
  -- (getNext lex, acc - result)
  | evalNextKind lex == Token.XOR =
      (getNext lex, acc `xor` result)
  | evalNextKind lex == Token.INT = error "[Parser] expected OPERAND (+, -, ^)"
  | otherwise = (lex, acc)
  where
    result = snd $ parseExpressionInt (getNext lex)

-- result = snd (parseExpressionOperand ((getNext lex), acc))

-- parseTerm :: Lexer -> (Lexer, Int)
-- parseTerm lex
--   | evalNextKind lex == Token.MULT = error "[Parser] expected INT"
--   | evalNextKind lex == Token.DIV = error "[Parser] expected INT"
--   | evalNextKind lex == Token.INT = parseTerm' (getNext lex) intVal
--   | otherwise = error "[Parser] invalid token"
--   where
--     intVal = evalValueInt $ evalNextValue lex

-- parseTerm' :: Lexer -> Int -> (Lexer, Int)
-- parseTerm' lex acc
--   | evalNextKind lex == Token.MULT =
--       (getNext lex, acc * result)
--   | evalNextKind lex == Token.DIV =
--       (getNext lex, acc `div` result)
--   | evalNextKind lex == Token.INT = error "[Parser] expected OPERAND (*, /)"
--   | otherwise = parseExpression' lex acc
--   where
--     result = snd (parseExpression (getNext lex))

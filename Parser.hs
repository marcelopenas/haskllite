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
    initialLexer = getNext invalidLexer
    initialVal = 0
    -- (initialLexer, initialVal) = parseInt $ getNext invalidLexer

parseExpression :: Lexer -> Int -> (Lexer, Int)
parseExpression lex acc
  | evalNextKind lex == Token.EOF = (lex, acc)
  | evalNextKind lex == Token.PLUS = error "[Parser] [Expression] expected TERM, not +"
  | evalNextKind lex == Token.MINUS = error "[Parser] [Expression] expected TERM, not -"
  | otherwise =
      uncurry parseExpression $ uncurry parseExpressionOperand (parseTerm lex acc)

parseExpressionOperand :: Lexer -> Int -> (Lexer, Int)
parseExpressionOperand lex acc
  | evalNextKind lex == Token.PLUS = (nextLex, acc + nextVal)
  | evalNextKind lex == Token.MINUS = (nextLex, acc - nextVal)
  | evalNextKind lex == Token.XOR = (nextLex, acc `xor` nextVal)
  | evalNextKind lex == Token.INT = error "[Parser] [Expression] expected OPERAND (+, -, ^)"
  | otherwise = parseExpression lex acc
  where
    (nextLex, nextVal) = parseTerm (getNext lex) acc

parseTerm :: Lexer -> Int -> (Lexer, Int)
parseTerm lex acc
  | evalNextKind lex == Token.MULT = error "[Parser] [Term] expected INT, not *"
  | evalNextKind lex == Token.DIV = error "[Parser] [Term] expected INT, not /"
  | otherwise = parseTermOperand (getNext nextLex) nextVal
  where
    (nextLex, nextVal) = parseFactor lex acc

parseTermOperand :: Lexer -> Int -> (Lexer, Int)
parseTermOperand lex acc
  | evalNextKind lex == Token.MULT =
      (nextLex, acc * nextVal)
  | evalNextKind lex == Token.DIV =
      (nextLex, acc `div` nextVal)
  | evalNextKind lex == Token.INT = error "[Parser] [Term] expected OPERAND (*, /)"
  | otherwise = (lex, acc)
  where
    (nextLex, nextVal) = parseTerm (getNext lex) acc

parseFactor :: Lexer -> Int -> (Lexer, Int)
parseFactor lex acc
  | evalNextKind lex == Token.INT = (getNext lex, intVal)
  | evalNextKind lex == Token.PLUS = (nextLex, acc + nextVal)
  | evalNextKind lex == Token.MINUS = (nextLex, acc - nextVal)
  | evalNextKind lex == Token.OPEN_PAR = uncurry parseFactorClose (parseExpression lex acc)
  | otherwise = error "[Parser] [Factor] expected INT"
  where
    intVal = evalValueInt $ evalNextValue lex
    (nextLex, nextVal) = parseFactor (getNext lex) intVal

parseFactorClose  :: Lexer -> Int -> (Lexer, Int)
parseFactorClose lex acc
  | evalNextKind lex == Token.CLOSE_PAR = (getNext lex, acc)
  | otherwise = error "[Parser] [Factor] expected )"

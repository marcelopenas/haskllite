module Parser
  ( run,
  )
where

import Data.Bits (Bits (xor))
import Lexer (Lexer (..), getNext)
import Semantic
import Token (Kind (..), Token (..), Value (..))

evalNextKind :: Lexer -> Token.Kind
evalNextKind lex = Token.kind (next lex)

evalNextValue :: Lexer -> Token.Value
evalNextValue lex = Token.value (next lex)

evalValueInt :: Token.Value -> Int
evalValueInt (Right val) = val
evalValueInt _ = error "[Lexer] Token of type INT presenting NaN value" -- If token 'kind' is INT and 'value' is of type string, lexer made a mistake

run :: String -> Node
run source =
  parseExpression initialLexer
  -- leftRotate $ parseExpression initialLexer
  -- error $ show $ reverseTree (parseExpression initialLexer)
  -- rotateN (treeDepth (parseExpression initialLexer)) (parseExpression initialLexer)
  -- error $ show $ leftRotate (parseExpression initialLexer)
  where
    invalidLexer = Lexer source (-1) (Token Token.EOF (Left "\0")) -- EOF represents initial non existent token for passing to evalNext to get actual first token
    initialLexer = getNext invalidLexer

treeDepth :: Node -> Int
treeDepth (IntNode _) = 1
treeDepth (UnOp _ children) = 1 + maxChildDepth children
treeDepth (BinOp _ children) = 1 + maxChildDepth children

-- Helper to handle the list of children safely
maxChildDepth :: [Node] -> Int
maxChildDepth [] = 0
maxChildDepth xs = maximum (map treeDepth xs)

rotateN :: Int -> Node -> Node
rotateN n node
  | n <= 0    = node
  | otherwise = rotateN (n - 1) (leftRotate node)

leftRotate :: Node -> Node
leftRotate (BinOp opP [lP, BinOp opC [lC, rC]]) = 
    BinOp opC [BinOp opP [lP, lC], rC]
leftRotate n = n

reverseTree :: Node -> Node
reverseTree (IntNode val) = IntNode val
reverseTree (UnOp op children) = 
    UnOp op (reverse (map reverseTree children))
reverseTree (BinOp op children) = 
    BinOp op (reverse (map reverseTree children))

-- parseExpression :: Lexer -> Node
-- parseExpression lex
--   | evalNextKind nextLex == Token.PLUS =
--       Semantic.BinOp "+" [leftNode, parseExpression $ getNext nextLex]
--   | evalNextKind nextLex == Token.MINUS =
--       Semantic.BinOp "-" [leftNode, parseExpression $ getNext nextLex]
--   | otherwise = parseTerm lex
--   where
--     nextLex = getNext lex
--     leftNode = parseTerm lex

parseExpression :: Lexer -> Node
parseExpression lex = do
  let leftNode = parseTerm lex
  let nextLex = getNext lex
  case evalNextKind nextLex of
    Token.PLUS -> Semantic.BinOp "+" [leftNode, parseExpression $ getNext nextLex]
    Token.MINUS -> Semantic.BinOp "-" [leftNode, parseExpression $ getNext nextLex]
    _ -> leftNode

parseTerm :: Lexer -> Node
parseTerm lex = do
  let leftNode = parseFactor lex
  let nextLex = getNext lex
  case evalNextKind nextLex of
    Token.MULT -> Semantic.BinOp "*" [leftNode, parseTerm $ getNext nextLex]
    Token.DIV -> Semantic.BinOp "/" [leftNode, parseTerm $ getNext nextLex]
    _ -> leftNode

parseFactor :: Lexer -> Node
parseFactor lex = do
  case evalNextKind lex of
    Token.INT -> Semantic.IntNode (evalValueInt $ evalNextValue lex)
    _ -> error "[Parser] [Factor] expected INT"

-- * EXPRESSION

-- parseExpression :: Lexer -> Node
-- parseExpression lex
--   | evalNextKind lex == Token.PLUS =
--       Semantic.BinOp "+" [parseExpression $ getNext lex]
--   | evalNextKind lex == Token.MINUS =
--       Semantic.BinOp "-" [parseExpression $ getNext lex]
--   | evalNextKind lex == Token.INT =
--       Semantic.IntNode (evalValueInt $ evalNextValue lex)
--   | otherwise = error "[Parser] [Expression] expected INT, + or -"

-- parseExpression :: Lexer -> Node
-- parseExpression lex
--   | x <- parseExpression lex,
--     evalNextKind lex == Token.PLUS =
--       Node BinOp (Left "+") [x, parseExpression $ getNext lex]
--   | x <- parseExpression $ getNext lex,
--     evalNextKind lex == Token.INT =
--       Node IntVal (Right (evalValueInt $ evalNextValue lex)) [x, parseExpression $ getNext lex]
--   | otherwise =
--       Node IntVal (Right (evalValueInt $ evalNextValue lex)) []


-- parseExpression :: Lexer -> Node -> (Lexer, Node)
-- parseExpression lex acc
--   | evalNextKind lex == Token.EOF = (lex, acc)
--   | evalNextKind lex == Token.PLUS = error "[Parser] [Expression] expected TERM, not +"
--   | evalNextKind lex == Token.MINUS = error "[Parser] [Expression] expected TERM, not -"
--   | otherwise =
--       uncurry parseExpression $ parseExpressionOperand nextLex nextVal
--   where
--     (nextLex, nextVal) = parseTerm lex acc

-- parseExpressionOperand :: Lexer -> Node -> (Lexer, Node)
-- parseExpressionOperand lex acc
--   | evalNextKind lex == Token.PLUS = (getNext lex, Node BinOp (Left "+") [])
--   | evalNextKind lex == Token.MINUS = (getNext lex, Node BinOp (Left "-") [])
--   | evalNextKind lex == Token.INT = error "[Parser] [Expression] expected OPERAND (+, -)"
--   | otherwise = (lex, acc)

-- * TERM

-- parseTerm :: Lexer -> Int -> (Lexer, Int)
-- parseTerm lex acc
--   | evalNextKind lex == Token.MULT = error "[Parser] [Term] expected INT, not *"
--   | evalNextKind lex == Token.DIV = error "[Parser] [Term] expected INT, not /"
--   | otherwise = uncurry parseTerm $ parseTermOperand nextLex nextVal
--   -- | otherwise = uncurry parseTermOperand (parseFactor lex acc)
--   where
--     (nextLex, nextVal) = parseFactor lex acc

-- parseTermOperand :: Lexer -> Int -> (Lexer, Int)
-- parseTermOperand lex acc
--   | evalNextKind lex == Token.MULT =
--       (nextLex, acc * nextVal)
--   | evalNextKind lex == Token.DIV =
--       (nextLex, acc `div` nextVal)
--   | evalNextKind lex == Token.INT = error "[Parser] [Term] expected OPERAND (*, /)"
--   | otherwise = (lex, acc)
--   where
--     (nextLex, nextVal) = parseTerm (getNext lex) acc

-- * FACTOR

-- parseFactor :: Lexer -> Int -> (Lexer, Int)
-- parseFactor lex acc
--   | evalNextKind lex == Token.INT = (getNext lex, intVal)
--   -- | evalNextKind lex == Token.INT = (getNext nextLex, nextVal)
--   -- | evalNextKind lex == Token.PLUS = (nextLex, acc + nextVal)
--   -- | evalNextKind lex == Token.MINUS = (nextLex, acc - nextVal)
--   -- | evalNextKind lex == Token.OPEN_PAR = uncurry parseFactorClose (parseExpression lex acc)
--   | otherwise = error "[Parser] [Factor] expected INT"
--   where
--     intVal = evalValueInt $ evalNextValue lex
--     (nextLex, nextVal) = parseFactor (getNext lex) intVal

-- parseFactorClose :: Lexer -> Int -> (Lexer, Int)
-- parseFactorClose lex acc
--   | evalNextKind lex == Token.CLOSE_PAR = (getNext lex, acc)
--   | otherwise = error "[Parser] [Factor] expected )"

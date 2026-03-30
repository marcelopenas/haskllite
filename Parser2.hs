module Parser2
  ( run,
  )
where

import Data.Bits (Bits (xor))
import Lexer (Lexer (..), getNext, next)
import Semantic
import Token (Token (..))

run :: String -> Node
run source
  | next finalLex == Token.EOF = node
  | otherwise = error $ "[Parser] Unexpected token at end of input: " ++ show (next finalLex)
  where
    invalidLexer = Lexer source (-1) Token.EOF -- EOF represents initial non existent token for passing to evalNext to get actual first token
    initialLexer = getNext invalidLexer
    (finalLex, node) = parseExpression initialLexer

parseExpression :: Lexer -> (Lexer, Node)
parseExpression lex =
  let (nextLex, leftNode) = parseTerm lex
   in parseExpressionLoop nextLex leftNode

parseExpressionLoop :: Lexer -> Node -> (Lexer, Node)
parseExpressionLoop lex leftNode
  | nextKind == Token.PLUS =
      let (afterRight, rightNode) = parseTerm (getNext lex)
       in parseExpressionLoop afterRight (Semantic.BinOp "+" [leftNode, rightNode])
  | nextKind == Token.MINUS =
      let (afterRight, rightNode) = parseTerm (getNext lex)
       in parseExpressionLoop afterRight (Semantic.BinOp "-" [leftNode, rightNode])
  | otherwise = (lex, leftNode)
  where
    nextKind = next lex

parseTerm :: Lexer -> (Lexer, Node)
parseTerm lex =
  let (nextLex, leftNode) = parseUnary lex
   in parseTermLoop nextLex leftNode

parseTermLoop :: Lexer -> Node -> (Lexer, Node)
parseTermLoop lex leftNode
  | nextKind == Token.MULT =
      let (afterRight, rightNode) = parseUnary (getNext lex)
       in parseTermLoop afterRight (Semantic.BinOp "*" [leftNode, rightNode])
  | nextKind == Token.DIV =
      let (afterRight, rightNode) = parseUnary (getNext lex)
       in parseTermLoop afterRight (Semantic.BinOp "/" [leftNode, rightNode])
  | otherwise = (lex, leftNode)
  where
    nextKind = next lex

parseUnary :: Lexer -> (Lexer, Node)
parseUnary lex
  | nextKind == Token.PLUS =
      let (after, node) = parseUnary (getNext lex)
       in (after, Semantic.UnOp "+" [node])
  | nextKind == Token.MINUS =
      let (after, node) = parseUnary (getNext lex)
       in (after, Semantic.UnOp "-" [node])
  | otherwise = parsePower lex
  where
    nextKind = next lex

parsePower :: Lexer -> (Lexer, Node)
parsePower lex =
  let (afterLeft, leftNode) = parseFactorial lex
  in if next afterLeft == Token.POWER -- Assuming Token.POWER is '^'
     then let (afterRight, rightNode) = parsePower (getNext afterLeft)
          in (afterRight, Semantic.BinOp "**" [leftNode, rightNode])
     else (afterLeft, leftNode)

parseFactorial :: Lexer -> (Lexer, Node)
parseFactorial lex =
  let (nextLex, node) = parsePrimary lex
   in parseFactorialLoop nextLex node

parseFactorialLoop :: Lexer -> Node -> (Lexer, Node)
parseFactorialLoop lex leftNode
  | next lex == Token.FACT =
      parseFactorialLoop (getNext lex) (Semantic.UnOp "!" [leftNode])
  | otherwise = (lex, leftNode)

parsePrimary :: Lexer -> (Lexer, Node)
parsePrimary lex
  | nextKind == Token.OPEN_PAR =
      let (afterExpr, exprNode) = parseExpression (getNext lex)
       in if next afterExpr == Token.CLOSE_PAR
            then (getNext afterExpr, exprNode)
            else error $ "[Parser] Expected CLOSE_PAR at position: " ++ show (Lexer.position afterExpr)
  | Token.INT val <- nextKind = (getNext lex, Semantic.IntNode val)
  | otherwise =
      error $ "[Parser] Expected INT, got: " ++ show nextKind ++ ", at position: " ++ show (Lexer.position lex)
  where
    nextKind = next lex
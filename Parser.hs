module Parser
  ( run,
  )
where

import Data.Bits (Bits (xor))
import Lexer (Lexer (..), getNext)
import Semantic
import Token

type Scanner = (Lexer, Token)

type Parser a = Scanner -> (Scanner, a)

run :: String -> Node
run source
  | finalToken == Token.EOF = node
  | otherwise = error $ "[Parser] Unexpected token at end of input: " ++ show finalToken
  where
    invalidLexer = Lexer source (-1)
    (initialLexer, next) = getNext invalidLexer
    ((finalLex, finalToken), node) = parseProgram (initialLexer, next)

-- * Program

parseProgram :: Parser Node
parseProgram scanner =
  ((lastLex, lastToken), Semantic.Block nodes)
  where
    ((lastLex, lastToken), nodes) = parseProgramLoop [] scanner

parseProgramLoop :: [Node] -> Parser [Node]
parseProgramLoop statements (lex, token) = case token of
  Token.EOF -> ((lex, token), statements)
  _ -> ((newLex, newToken), newStatements)
  where
    ((nextLex, nextToken), node) = parseStatement (lex, token)
    nextStatements = node : statements
    ((newLex, newToken), newStatements) = parseProgramLoop nextStatements (nextLex, nextToken)

-- * Statement

parseStatement :: Parser Node
parseStatement (Lexer source position, token) = case token of
  Token.END -> ((nextLex, nextToken), Semantic.NoOp)
  Token.LET -> parseStatementLet (nextLex, nextToken)
  Token.IDENTIFIER name -> parseStatementAssign name False (nextLex, nextToken)
  Token.PRINT -> (parseStatementEnd (newLex, newToken), Semantic.Print newNode)
  _ -> error $ "[Parser] invalid statement: " ++ show token ++ " at: " ++ show position
  where
    lex = Lexer source position
    (nextLex, nextToken) = getNext lex
    ((newLex, newToken), newNode) = parseExpression (nextLex, nextToken)

parseStatementLet :: Parser Node
parseStatementLet (Lexer source position, token) = case token of
  Token.IDENTIFIER name -> parseStatementAssign name True (nextLex, nextToken)
  _ -> error $ "[Parser] expected identifier, got: " ++ show token ++ " at: " ++ show position
  where
    lex = Lexer source position
    (nextLex, nextToken) = getNext lex


parseStatementAssign :: String -> Bool -> Parser Node
parseStatementAssign name immutable (Lexer source position, token) = case token of
  Token.ASSIGN -> (parseStatementEnd (newLex, newToken), Semantic.Assignment name newNode immutable)
  _ -> error $ "[Parser] expected assignment at: " ++ show position ++ ", got: " ++ show token
  where
    lex = Lexer source position
    (nextLex, nextToken) = getNext lex
    ((newLex, newToken), newNode) = parseExpression (nextLex, nextToken)

parseStatementEnd :: (Lexer, Token) -> (Lexer, Token)
parseStatementEnd (Lexer source position, token) = case token of
  Token.END -> getNext lex
  _ -> error $ "[Parser] no END in statement at:" ++ show position
  where
    lex = Lexer source position

-- * Expression

parseExpression :: Parser Node
parseExpression (Lexer source position, token) = case token of
  Token.PLUS -> expected
  Token.MINUS -> expected
  _ -> parseExpressionLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseTerm (lex, token)
    expected = error $ "[Parser] Expected INT, got: " ++ show token ++ ", at position: " ++ show position
    lex = Lexer source position

parseExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseExpressionLoop (Lexer source position, token) leftNode = case token of
  Token.PLUS -> parseExpressionLoop nextLex (Semantic.BinOp "+" leftNode rightNode)
  Token.MINUS -> parseExpressionLoop nextLex (Semantic.BinOp "-" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseTerm (getNext lex)
    lex = Lexer source position

-- * Term

parseTerm :: Parser Node
parseTerm scanner = parseTermLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseFactor scanner

parseTermLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseTermLoop (Lexer source position, token) leftNode = case token of
  Token.MULT -> parseTermLoop nextLex (Semantic.BinOp "*" leftNode rightNode)
  Token.DIV -> parseTermLoop nextLex (Semantic.BinOp "/" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseFactor (getNext lex)
    lex = Lexer source position

-- * Factor

parseFactor :: Parser Node
parseFactor (Lexer source position, token) = case token of
  Token.PLUS -> (nextLex, Semantic.UnOp "+" rightNode)
  Token.MINUS -> (nextLex, Semantic.UnOp "-" rightNode)
  Token.OPEN_PAR ->
    if nextAfterOpen == Token.CLOSE_PAR
      then (getNext lexAfterOpen, exprNode) -- Consume CLOSE_PAR
      else error $ "[Parser] Expected CLOSE_PAR at position: " ++ show positionAfterOpen
  (Token.INT val) -> (getNext lex, Semantic.IntNode val)
  (Token.IDENTIFIER name) -> (getNext lex, Semantic.Identifier name)
  _ -> error $ "[Parser] Expected INT, got: " ++ show token ++ ", at position: " ++ show position
  where
    (nextLex, rightNode) = parseFactor (getNext lex)
    ((Lexer sourceAfterOpen positionAfterOpen, nextAfterOpen), exprNode) = parseExpression (getNext lex)
    lexAfterOpen = Lexer sourceAfterOpen positionAfterOpen
    lex = Lexer source position

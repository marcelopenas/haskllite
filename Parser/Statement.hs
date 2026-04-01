module Parser.Statement (parseStatement) where

import Lexer (Lexer (..), getNext)
import Parser.Expression (parseExpression)
import Parser.Parser (Parser)
import Semantic (Node (Assignment, NoOp, Print))
import Token (Token (ASSIGN, END, IDENTIFIER, LET, PRINT))

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
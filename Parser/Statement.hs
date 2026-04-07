module Parser.Statement (parseStatement) where

import CompilerError (CompilerError (ParserError), compilerError)
import Lexer (Lexer (..), getNext)
import {-# SOURCE #-} Parser.Block (parseBlock)
import Parser.BoolExpression (parseBoolExpression)
import Parser.Parser (Parser, Scanner)
import Semantic (Node (Assignment, If, NoOp, Print, While))
import Token (Token (ASSIGN, CLOSE_PAR, ELSE, END, IDENTIFIER, IF, LET, OPEN_PAR, PRINT, WHILE))

parseStatement :: Parser Node
parseStatement (lex, token) = case token of
  Token.END -> ((nextLex, nextToken), Semantic.NoOp)
  Token.LET -> parseStatementLet (nextLex, nextToken)
  Token.IDENTIFIER name -> parseStatementAssign name False (nextLex, nextToken)
  Token.PRINT -> (parseStatementEnd (newLex, newToken), Semantic.Print newNode) -- TODO check for ()
  Token.WHILE -> ((afterLex, afterToken), Semantic.While newNode afterNode) -- TODO check for ()
  Token.IF -> ((afterAfterLex, afterAfterToken), Semantic.If newNode afterNode afterAfterNode) -- TODO check for ()
  _ -> parseBlock (lex, token)
  where
    (Lexer _ position) = lex
    (nextLex, nextToken) = getNext lex
    ((newLex, newToken), newNode) = parseBoolExpression $ parseStatementOpenPar (nextLex, nextToken)
    ((afterLex, afterToken), afterNode) = parseStatement (newLex, newToken)
    ((afterAfterLex, afterAfterToken), afterAfterNode) = parseStatementElse (afterLex, afterToken)

parseStatementOpenPar :: Scanner -> Scanner
parseStatementOpenPar (lex, token) = case token of
  Token.OPEN_PAR -> scanner
  _ -> compilerError scanner CompilerError.ParserError "expected open parenthesis"
  where
    scanner = (lex, token)

-- parseStatementClosePar :: (Scanner, Node) -> (Scanner, Node)
-- parseStatementClosePar ((lex, token), node) = case token of
--   Token.CLOSE_PAR -> (scanner, node)
--   _ -> compilerError scanner CompilerError.ParserError "expected close parenthesis"
--   where
--     scanner = (lex, token)

parseStatementElse :: Parser Node
parseStatementElse (lex, token) = case token of
  Token.ELSE -> ((nextLex, nextToken), nextNode) -- TODO check for ()
  _ -> ((lex, token), Semantic.NoOp)
  where
    ((nextLex, nextToken), nextNode) = parseStatement $ getNext lex

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
    ((newLex, newToken), newNode) = parseBoolExpression (nextLex, nextToken)

parseStatementEnd :: (Lexer, Token) -> (Lexer, Token)
parseStatementEnd (Lexer source position, token) = case token of
  Token.END -> getNext lex
  _ -> error $ "[Parser] no END in statement at:" ++ show position
  where
    lex = Lexer source position
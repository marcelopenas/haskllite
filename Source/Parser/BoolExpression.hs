module Source.Parser.BoolExpression (parseBoolExpression) where

import Source.Parser.BoolTerm (parseBoolTerm)
import Source.Parser.Parser (Parser)
import Source.CompilerError (compilerParserError)
import Source.Lexer (Lexer (..), getNext)
import Source.Node (Node (BinOp))
import Source.Token (Token (OR))

parseBoolExpression :: Parser Node
parseBoolExpression lexState@(lex, token) = case token of
  OR -> compilerParserError lexState "Expected BoolExpression"
  _ -> parseBoolExpressionLoop `uncurry` parseBoolTerm lexState

parseBoolExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)
parseBoolExpressionLoop lexState@(lex, token) leftNode = case token of
  OR -> parseBoolExpressionLoop nextLex (BinOp "||" leftNode rightNode)
  _ -> (lexState, leftNode)
  where
    (nextLex, rightNode) = parseBoolTerm (getNext lex)
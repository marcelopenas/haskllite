module Parser.Term (parseTerm) where

import Lexer (Lexer (..), getNext)
import Parser.Factor (parseFactor)
import Parser.Parser (Parser)
import Semantic
import Token

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

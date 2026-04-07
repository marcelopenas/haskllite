module Parser.Term (parseTerm) where

import Lexer (Lexer (..), LexerState, getNext)
import Parser.Factor (parseFactor)
import Parser.Parser (Parser)
import Semantic (Node (BinOp))
import Token (Token (DIV, MULT))

parseTerm :: Parser Node
parseTerm scanner = parseTermLoop nextLex leftNode
  where
    (nextLex, leftNode) = parseFactor scanner

parseTermLoop :: LexerState -> Node -> (LexerState, Node)
parseTermLoop (lex, token) leftNode = case token of
  MULT -> parseTermLoop nextLex (BinOp "*" leftNode rightNode)
  DIV -> parseTermLoop nextLex (BinOp "/" leftNode rightNode)
  _ -> ((lex, token), leftNode)
  where
    (nextLex, rightNode) = parseFactor $ getNext lex

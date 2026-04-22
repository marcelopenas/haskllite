module Parser.Term (parseTerm) where

import Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Parser.Factor (parseFactor)
import Parser.Parser (Parser)
import Semantic (Node (BinOp))
import Token (Token (DIV, MULT))

parseTerm :: Parser Node
parseTerm scanner = parseTermLoop `uncurry` parseFactor scanner

parseTermLoop :: LexerState -> Node -> (LexerState, Node)
parseTermLoop lexState@(lex, token) leftNode = case token of
  MULT -> parseTermLoop factorLex (BinOp "*" leftNode rightNode)
  DIV -> parseTermLoop factorLex (BinOp "/" leftNode rightNode)
  _ -> (lexState, leftNode)
  where
    (factorLex, rightNode) = parseFactor $ getNext lex

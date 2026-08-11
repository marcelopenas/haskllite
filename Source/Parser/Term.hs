module Source.Parser.Term (parseTerm) where

import Source.Lexer (Lexer (..), LexerState, getNext)
import {-# SOURCE #-} Source.Parser.Factor (parseFactor)
import Source.Parser.Parser (Parser)
import Source.Node (Node (BinOp))
import Source.Token (Token (DIV, MULT))

parseTerm :: Parser Node
parseTerm scanner = parseTermLoop `uncurry` parseFactor scanner

parseTermLoop :: LexerState -> Node -> (LexerState, Node)
parseTermLoop lexState@(lex, token) leftNode = case token of
  MULT -> parseTermLoop factorLex (BinOp "*" leftNode rightNode)
  DIV -> parseTermLoop factorLex (BinOp "/" leftNode rightNode)
  _ -> (lexState, leftNode)
  where
    (factorLex, rightNode) = parseFactor $ getNext lex

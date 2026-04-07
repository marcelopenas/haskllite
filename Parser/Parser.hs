module Parser.Parser (Parser) where

import Lexer (Lexer, LexerState)
import Token (Token)

type Parser a = LexerState -> (LexerState, a)

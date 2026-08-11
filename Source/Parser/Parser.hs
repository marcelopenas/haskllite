module Source.Parser.Parser (Parser) where

import Source.Lexer (Lexer, LexerState)
import Source.Token (Token)

type Parser a = LexerState -> (LexerState, a)

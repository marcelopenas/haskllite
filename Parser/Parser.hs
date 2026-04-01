module Parser.Parser (Parser) where

import Lexer (Lexer)
import Token (Token)

type Scanner = (Lexer, Token)

type Parser a = Scanner -> (Scanner, a)

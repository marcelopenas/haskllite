module Parser.Parser (Parser, Scanner) where

import Lexer (Lexer)
import Token (Token)

type Scanner = (Lexer, Token)

type Parser a = Scanner -> (Scanner, a)

module Parser.Expression (parseExpression) where

import Lexer (LexerState)
import Parser.Parser (Parser)
import Semantic (Node)

parseExpression :: Parser Node

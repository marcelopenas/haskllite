module Parser.Expression (parseExpression) where

import Lexer (Lexer (..),)
import Parser.Parser (Parser)
import Semantic ( Node )
import Token ( Token )

parseExpression :: Parser Node
parseExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)

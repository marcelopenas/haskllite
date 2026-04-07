module Parser.BoolExpression (parseBoolExpression) where

import Lexer (Lexer (..),)
import Parser.Parser (Parser)
import Semantic ( Node )
import Token ( Token )

parseBoolExpression :: Parser Node
parseBoolExpressionLoop :: (Lexer, Token) -> Node -> ((Lexer, Token), Node)

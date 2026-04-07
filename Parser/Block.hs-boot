module Parser.Block where

import Parser.Parser (Parser, Scanner)
import Semantic (Node (Block))
import Token (Token)

parseBlock :: Parser Node
parseBlockCloseBra :: (Scanner, [Node]) -> (Scanner, [Node])

module Parser.Factor (parseFactor) where

import Parser.Parser (Parser)
import Node (Node)
import GHC.Stack (HasCallStack)

parseFactor :: Parser Node
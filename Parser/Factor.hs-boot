module Parser.Factor (parseFactor) where

import Parser.Parser (Parser)
import Semantic (Node)
import GHC.Stack (HasCallStack)

parseFactor :: Parser Node
module Lexer
  ( Lexer (..),
    LexerState,
    getNext,
  )
where

import Token (Token)

type Source = String

type Position = Int

data Lexer = Lexer Source Position

type LexerState = (Lexer, Token)

getNext :: Lexer -> (Lexer, Token)

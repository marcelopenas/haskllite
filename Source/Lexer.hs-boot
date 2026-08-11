module Source.Lexer
  ( Lexer (..),
    LexerState,
    getNext,
  )
where

import Source.Token (Token)

type Source = String

type Position = Int

data Lexer = Lexer Source Position

type LexerState = (Lexer, Token)

getNext :: Lexer -> (Lexer, Token)

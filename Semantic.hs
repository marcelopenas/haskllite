{-# LANGUAGE BangPatterns #-}

module Semantic where

import Control.Monad (foldM)
import Data.Bits (Bits (xor))
import SymbolTable (SymbolTable, getSymbol, setSymbol)

data Node
  = IntNode Int
  | UnOp String Node
  | BinOp String Node Node
  | Identifier String
  | Print Node
  | Assignment String Node
  | Block [Node]
  | NoOp
  deriving (Show)

evaluate :: Node -> SymbolTable -> Int
evaluate (IntNode n) st = n
evaluate (UnOp "+" a) st = evaluate a st
evaluate (UnOp "-" a) st = -evaluate a st
evaluate (BinOp "+" a b) st = evaluate a st + evaluate b st
evaluate (BinOp "-" a b) st = evaluate a st - evaluate b st
evaluate (BinOp "^" a b) st = evaluate a st `xor` evaluate b st
evaluate (BinOp "*" a b) st = evaluate a st * evaluate b st
evaluate (BinOp "/" a (IntNode 0)) st = error "[Semantic] Division by zero"
evaluate (BinOp "/" a b) st = evaluate a st `div` evaluate b st
evaluate (BinOp "**" a b) st
  | evaluate b st < 0 = error "[Semantic] Negative exponent not supported"
  | otherwise = evaluate a st ^ evaluate b st
evaluate (Identifier name) st = getSymbol name st

execute :: Node -> SymbolTable -> IO SymbolTable
execute (Print node) st = do
  print (evaluate node st)
  return st
execute (Assignment name expr) st = do
  let !value = evaluate expr st
  let !st' = setSymbol (name, value) st
  return st'
execute (Block nodes) st = do
  foldM (flip execute) st (reverse nodes) -- Nodes will be right to left, thus reverse nodes
execute NoOp st = return st

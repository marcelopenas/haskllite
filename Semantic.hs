{-# LANGUAGE BangPatterns #-}

module Semantic where

import Control.Monad (foldM)
import Data.Bits
import GHC.IO (unsafePerformIO)
import SymbolTable (SymbolTable, getSymbol, setSymbol)

data Node
  = IntNode Int
  | UnOp String Node
  | BinOp String Node Node
  | Identifier String
  | Print Node
  | Scan
  | If Node Node Node
  | While Node Node
  | Assignment String Node Bool -- Name Expression Immutable
  | Block [Node]
  | NoOp
  deriving (Show)

evaluate :: Node -> SymbolTable -> Int
evaluate Scan st = unsafePerformIO (readLn :: IO Int)
evaluate (IntNode n) st = n
evaluate (UnOp "+" a) st = evaluate a st
evaluate (UnOp "-" a) st = -evaluate a st
evaluate (UnOp "!" a) st = fromEnum $ not $ toEnum $ evaluate a st
evaluate (BinOp "+" a b) st = evaluate a st + evaluate b st
evaluate (BinOp "-" a b) st = evaluate a st - evaluate b st
evaluate (BinOp "^" a b) st = evaluate a st `xor` evaluate b st
evaluate (BinOp "*" a b) st = evaluate a st * evaluate b st
evaluate (BinOp "/" a (IntNode 0)) st = error "[Semantic] Division by zero"
evaluate (BinOp "/" a b) st = evaluate a st `div` evaluate b st
evaluate (BinOp "**" a b) st
  | evaluate b st < 0 = error "[Semantic] Negative exponent not supported"
  | otherwise = evaluate a st ^ evaluate b st
evaluate (BinOp "==" a b) st = fromEnum $ evaluate a st == evaluate b st
evaluate (BinOp ">" a b) st = fromEnum $ evaluate a st > evaluate b st
evaluate (BinOp "<" a b) st = fromEnum $ evaluate a st < evaluate b st
evaluate (BinOp "&&" a b) st
  | abs (evaluate a st) .&. abs (evaluate b st) >= 1 = 1
  | otherwise = 0
evaluate (BinOp "||" a b) st
  | abs (evaluate a st) .|. abs (evaluate b st) >= 1 = 1
  | otherwise = 0
evaluate (Identifier name) st = getSymbol name st -- Return content

execute :: Node -> SymbolTable -> IO SymbolTable
execute (Print node) st = do
  print (evaluate node st)
  return st
execute (Assignment name expr immutable) st = do
  -- Get value
  let !value = evaluate expr st
  -- Assign
  let !st' = setSymbol (name, (value, immutable)) st
  return st'
execute (Block nodes) st = do
  foldM (flip execute) st (reverse nodes) -- Nodes will be right to left, thus reverse nodes
execute (If evalNode ifNode elseNode) st = do
  if evaluate evalNode st == 1 then execute ifNode st else execute elseNode st
execute (While evalNode node) st = do
  let !value = evaluate evalNode st
  if value == 1
    then do
      !nextSt <- execute node st
      execute (While evalNode node) nextSt
    else
      return st
execute NoOp st = return st

{-# LANGUAGE BangPatterns #-}

module Semantic (evaluate, execute, Node (..)) where

import CompilerError (compilerSemanticError)
import Control.Monad (foldM)
import Data.Bits (Bits (xor, (.&.), (.|.)))
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
evaluate (UnOp op a) st = case op of
  "+" -> a'
  "-" -> -a'
  "!" -> fromEnum $ not $ toEnum a'
  where
    a' = evaluate a st
evaluate (BinOp op a b) st = case op of
  "+" -> a' + b'
  "-" -> a' - b'
  "^" -> a' `xor` b'
  "*" -> a' * b'
  "**" -> if b' > 0 then a' ^ b' else compilerSemanticError "Negative exponent not supported"
  "/" -> if b' > 0 then a' `div` b' else compilerSemanticError "Division by zero"
  "==" -> fromEnum $ a' == b'
  ">" -> fromEnum $ a' > b'
  "<" -> fromEnum $ a' < b'
  "&&" -> if abs a' .&. abs b' >= 1 then 1 else 0
  "||" -> if abs a' .|. abs b' >= 1 then 1 else 0
  where
    a' = evaluate a st
    b' = evaluate b st
evaluate (Identifier name) st = getSymbol name st

execute :: Node -> SymbolTable -> IO SymbolTable
execute (Print node) st = do
  print (evaluate node st)
  return st
execute (Assignment name expr immutable) st = do
  let !value = evaluate expr st
  let !st' = setSymbol (name, (value, immutable)) st
  return st'
execute (Block nodes) st = do
  foldM (flip execute) st (reverse nodes) -- Nodes will be right to left, thus reverse nodes
execute (If evalNode ifNode elseNode) st = do
  let !value = evaluate evalNode st
  if value == 1
    then do
      execute ifNode st
    else do
      execute elseNode st
execute (While evalNode node) st = do
  let !value = evaluate evalNode st
  if value == 1
    then do
      !nextSt <- execute node st
      execute (While evalNode node) nextSt
    else
      return st
execute NoOp st = return st

{-# LANGUAGE BangPatterns #-}

module Semantic (evaluate, execute, Node (..)) where

import CompilerError (compilerSemanticError)
import Control.Monad (foldM)
import Data.Bits (Bits (xor, (.&.), (.|.)))
import Data.Char (intToDigit)
import GHC.IO (unsafePerformIO)
import SymbolTable (Content (..), SymbolTable, createVariable, getSymbol, setSymbol)
import Token (VarType)

data Node
  = IntNode Int
  | BoolNode Bool
  | StringNode String
  | UnOp String Node
  | BinOp String Node Node
  | Identifier String
  | Print Node
  | Scan
  | If Node Node Node
  | While Node Node
  | Assignment String Node -- Name Expression Immutable
  | VarDec String Node Bool VarType -- -- Name Expression Immutable Type
  | Block [Node]
  | NoOp
  deriving (Show)

evaluate :: Node -> SymbolTable -> Content
evaluate Scan st = IntContent $ unsafePerformIO (readLn :: IO Int) -- TODO cast other types depending on :
evaluate (IntNode n) st = IntContent n
evaluate (BoolNode n) st = BoolContent n
evaluate (StringNode n) st = StringContent n
evaluate (UnOp op a) st = case (op, a') of
  ("+", IntContent a') -> IntContent a'
  ("+", _) -> compilerSemanticError "Invalid operator UnOp + for non i32"
  ("-", IntContent a') -> IntContent $ -a'
  ("-", _) -> compilerSemanticError "Invalid operator UnOp - for non i32"
  ("!", IntContent a') -> BoolContent $ not $ toEnum a' -- ? Implicit cast?
  ("!", BoolContent a') -> BoolContent $ not a'
  ("!", _) -> compilerSemanticError "Invalid operator UnOp ! for non i32 | bool"
  where
    a' = evaluate a st
evaluate (BinOp op a b) st = case (op, a', b') of
  ("+", IntContent a', IntContent b') -> IntContent $ a' + b'
  ("+", StringContent a', StringContent b') -> StringContent $ a' ++ b'
  ("+", StringContent a', IntContent b') -> StringContent $ reverse $ intToDigit b' : a'
  ("+", IntContent a', StringContent b') -> StringContent $ intToDigit a' : b'
  ("+", StringContent a', BoolContent True) -> StringContent $ a' ++ "true"
  ("+", StringContent a', BoolContent False) -> StringContent $ a' ++ "false"
  ("+", BoolContent True, StringContent b') -> StringContent $ "true" ++ b'
  ("+", BoolContent False, StringContent b') -> StringContent $ "false" ++ b'
  ("+", _, _) -> compilerSemanticError "Invalid operator BinOp + for non i32 | str"
  ("-", IntContent a', IntContent b') -> IntContent $ a' - b'
  ("-", _, _) -> compilerSemanticError "Invalid operator BinOp - for non i32"
  ("^", IntContent a', IntContent b') -> IntContent $ a' `xor` b'
  ("^", BoolContent a', BoolContent b') -> BoolContent $ a' `xor` b'
  ("^", _', _) -> compilerSemanticError "Invalid operator BinOp - for non i32 | bool"
  ("*", IntContent a', IntContent b') -> IntContent $ a' * b'
  ("*", _, _) -> compilerSemanticError "Invalid operator BinOp * for non i32"
  ("**", IntContent a', IntContent b')
    | b' <= 0 -> compilerSemanticError "Negative exponent not supported"
    | otherwise -> IntContent $ a' ^ b'
  ("**", _, _) -> compilerSemanticError "Invalid operator BinOp * for non i32"
  ("/", IntContent a', IntContent b')
    | b' == 0 -> compilerSemanticError "Division by zero"
    | otherwise -> IntContent $ a' `div` b'
  ("/", _, _) -> compilerSemanticError "Invalid operator BinOp * for non i32"
  ("==", IntContent a', IntContent b') -> BoolContent $ a' == b'
  ("==", BoolContent a', BoolContent b') -> BoolContent $ a' == b'
  ("==", StringContent a', StringContent b') -> BoolContent $ a' == b'
  ("==", _, _) -> compilerSemanticError "Invalid operator BinOp == for non CMP"
  (">", IntContent a', IntContent b') -> BoolContent $ a' > b'
  (">", BoolContent a', BoolContent b') -> BoolContent $ a' > b'
  (">", StringContent a', StringContent b') -> BoolContent $ a' > b'
  (">", _, _) -> compilerSemanticError "Invalid operator BinOp > for non CMP"
  ("<", IntContent a', IntContent b') -> BoolContent $ a' < b'
  ("<", BoolContent a', BoolContent b') -> BoolContent $ a' < b'
  ("<", StringContent a', StringContent b') -> BoolContent $ a' < b'
  ("<", _, _) -> compilerSemanticError "Invalid operator BinOp < for non CMP"
  ("&&", IntContent a', IntContent b') -> BoolContent $ abs a' .&. abs b' >= 1
  ("&&", BoolContent a', BoolContent b') -> BoolContent $ a' && b'
  ("&&", _, _) -> compilerSemanticError "Invalid operator BinOp && for non i32 | bool"
  ("||", IntContent a', IntContent b') -> BoolContent $ abs a' .|. abs b' >= 1
  ("||", BoolContent a', BoolContent b') -> BoolContent $ a' || b'
  ("||", _, _) -> compilerSemanticError "Invalid operator BinOp || for non i32 | bool"
  where
    a' = evaluate a st
    b' = evaluate b st
evaluate (Identifier name) st = getSymbol name st
evaluate NoOp st = NullContent -- FIXME this should not be here, it is to fix a empty statement calling eval on noOp

execute :: Node -> SymbolTable -> IO SymbolTable
execute (Print node) st = do
  let node' = evaluate node st
  print node'
  return st
execute (VarDec name expr immutable varType) st = do
  let !value = evaluate expr st
  let !st' = createVariable (name, value, immutable, varType) st
  return st'
execute (Assignment name expr) st = do
  let !value = evaluate expr st
  let !st' = setSymbol (name, value) st
  return st'
execute (Block nodes) st = do
  !st' <- foldM (flip execute) st (reverse nodes) -- Nodes will be right to left, thus reverse nodes
  return st'
execute (If evalNode ifNode elseNode) st = do
  let !value = evaluate evalNode st
  if isTruthy value
    then do
      execute ifNode st
    else do
      execute elseNode st
execute (While evalNode node) st = do
  let !value = evaluate evalNode st
  if value == BoolContent True
    then do
      !nextSt <- execute node st
      execute (While evalNode node) nextSt
    else
      return st
execute NoOp st = return st

isTruthy :: Content -> Bool
isTruthy value = case value of
  BoolContent true -> True
  IntContent 1 -> True
  StringContent s
    | length s > 1 -> True
    | otherwise -> False
  _ -> False

{-# LANGUAGE BangPatterns #-}

module Semantic (evaluate, execute) where

import CompilerError (compilerSemanticError)
import Control.Monad (foldM)
import Data.Bits (Bits (xor, (.&.), (.|.)))
import Data.Char (intToDigit)
import FunctionTable (FunctionTable)
import GHC.IO (unsafePerformIO)
import Node (Node (..))
import SymbolTable (SymbolTable, Variable (..), createVariable, getSymbol, popScope, pushScope, setSymbol)
import Token (VarType (BooleanT, F64T, I32T, StrT))

evaluate :: Node -> (SymbolTable, FunctionTable) -> Variable
evaluate Scan (st, ft) = IntContent $ unsafePerformIO (readLn :: IO Int) -- TODO change to StringContent
evaluate (IntNode n) (st, ft) = IntContent n
evaluate (FloatNode n) (st, ft) = FloatContent n
evaluate (BoolNode n) (st, ft) = BoolContent n
evaluate (StringNode n) (st, ft) = StringContent n
evaluate (CastNode n F64T) (st, ft) = case n' of
  IntContent n -> FloatContent (fromIntegral n :: Float)
  FloatContent n -> FloatContent n
  _ -> compilerSemanticError "Invalid cast to f64"
  where
    n' = evaluate n (st, ft)
evaluate (CastNode n I32T) (st, ft) = case n' of
  FloatContent n -> IntContent (round n :: Int) -- TODO Should be truncate
  IntContent n -> IntContent n
  _ -> compilerSemanticError "Invalid cast to i32"
  where
    n' = evaluate n (st, ft)
evaluate (CastNode n StrT) (st, ft) = case n' of
  FloatContent n -> StringContent (show n)
  IntContent n -> StringContent (show n)
  StringContent n -> StringContent n
  BoolContent n -> StringContent (show n)
  _ -> compilerSemanticError "Invalid cast to string"
  where
    n' = evaluate n (st, ft)
evaluate (CastNode n BooleanT) (st, ft) = case n' of
  FloatContent n -> BoolContent (n /= 0)
  IntContent n -> BoolContent (n /= 0)
  StringContent n -> BoolContent (n /= "")
  BoolContent n -> BoolContent n
  _ -> compilerSemanticError "Invalid cast to boolean"
  where
    n' = evaluate n (st, ft)
evaluate (UnOp op a) (st, ft) = case (op, a') of
  ("+", IntContent a') -> IntContent a'
  ("+", _) -> compilerSemanticError "Invalid operator UnOp + for non i32"
  ("-", IntContent a') -> IntContent $ -a'
  ("-", _) -> compilerSemanticError "Invalid operator UnOp - for non i32"
  ("!", IntContent a') -> IntContent $ fromEnum $ not $ toEnum a'
  ("!", BoolContent a') -> BoolContent $ not a'
  ("!", _) -> compilerSemanticError "Invalid operator UnOp ! for non i32 | bool"
  where
    a' = evaluate a (st, ft)
evaluate (BinOp op a b) (st, ft) = case (op, a', b') of
  ("+", IntContent a', IntContent b') -> IntContent $ a' + b'
  ("+", IntContent a', FloatContent b') -> FloatContent $ fromIntegral a' + b'
  ("+", FloatContent a', IntContent b') -> FloatContent $ a' + fromIntegral b'
  ("+", FloatContent a', FloatContent b') -> FloatContent $ a' + b'
  ("+", StringContent a', StringContent b') -> StringContent $ a' ++ b'
  ("+", StringContent a', IntContent b') -> StringContent $ reverse $ intToDigit b' : reverse a'
  ("+", IntContent a', StringContent b') -> StringContent $ intToDigit a' : b'
  ("+", StringContent a', BoolContent True) -> StringContent $ a' ++ "true"
  ("+", StringContent a', BoolContent False) -> StringContent $ a' ++ "false"
  ("+", BoolContent True, StringContent b') -> StringContent $ "true" ++ b'
  ("+", BoolContent False, StringContent b') -> StringContent $ "false" ++ b'
  ("+", _, _) -> compilerSemanticError "Invalid operator BinOp + for non i32 | str"
  ("-", IntContent a', IntContent b') -> IntContent $ a' - b'
  ("-", IntContent a', FloatContent b') -> FloatContent $ fromIntegral a' - b'
  ("-", FloatContent a', IntContent b') -> FloatContent $ a' - fromIntegral b'
  ("-", FloatContent a', FloatContent b') -> FloatContent $ a' - b'
  ("-", _, _) -> compilerSemanticError "Invalid operator BinOp - for non i32"
  ("^", IntContent a', IntContent b') -> IntContent $ a' `xor` b'
  ("^", BoolContent a', BoolContent b') -> BoolContent $ a' `xor` b'
  ("^", _', _) -> compilerSemanticError "Invalid operator BinOp - for non i32 | bool"
  ("*", IntContent a', IntContent b') -> IntContent $ a' * b'
  ("*", IntContent a', FloatContent b') -> FloatContent $ fromIntegral a' * b'
  ("*", FloatContent a', IntContent b') -> FloatContent $ a' * fromIntegral b'
  ("*", FloatContent a', FloatContent b') -> FloatContent $ a' * b'
  ("*", _, _) -> compilerSemanticError "Invalid operator BinOp * for non i32"
  ("**", IntContent a', IntContent b')
    | b' <= 0 -> compilerSemanticError "Negative exponent not supported"
    | otherwise -> IntContent $ a' ^ b'
  ("**", _, _) -> compilerSemanticError "Invalid operator BinOp * for non i32"
  ("/", IntContent a', IntContent b')
    | b' == 0 -> compilerSemanticError "Division by zero"
    | otherwise -> IntContent $ a' `div` b'
  ("/", IntContent a', FloatContent b')
    | b' == 0 -> compilerSemanticError "Division by zero"
    | otherwise -> FloatContent $ fromIntegral a' / b'
  ("/", FloatContent a', IntContent b')
    | b' == 0 -> compilerSemanticError "Division by zero"
    | otherwise -> FloatContent $ a' / fromIntegral b'
  ("/", FloatContent a', FloatContent b')
    | b' == 0 -> compilerSemanticError "Division by zero"
    | otherwise -> FloatContent $ a' / b'
  ("/", _, _) -> compilerSemanticError "Invalid operator BinOp * for non i32"
  ("==", IntContent a', IntContent b') -> BoolContent $ a' == b'
  ("==", BoolContent a', BoolContent b') -> BoolContent $ a' == b'
  ("==", StringContent a', StringContent b') -> BoolContent $ a' == b'
  ("==", _, _) -> compilerSemanticError "Invalid operator BinOp == for non CMP"
  (">", IntContent a', IntContent b') -> BoolContent $ a' > b'
  (">", IntContent a', FloatContent b') -> BoolContent $ fromIntegral a' > b'
  (">", FloatContent a', IntContent b') -> BoolContent $ a' > fromIntegral b'
  (">", FloatContent a', FloatContent b') -> BoolContent $ a' > b'
  (">", BoolContent a', BoolContent b') -> BoolContent $ a' > b'
  (">", StringContent a', StringContent b') -> BoolContent $ a' > b'
  (">", _, _) -> compilerSemanticError "Invalid operator BinOp > for non CMP"
  ("<", IntContent a', IntContent b') -> BoolContent $ a' < b'
  ("<", IntContent a', FloatContent b') -> BoolContent $ fromIntegral a' < b'
  ("<", FloatContent a', IntContent b') -> BoolContent $ a' < fromIntegral b'
  ("<", FloatContent a', FloatContent b') -> BoolContent $ a' < b'
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
    a' = evaluate a (st, ft)
    b' = evaluate b (st, ft)
evaluate (Identifier name) (st, ft) = getSymbol name st
evaluate (If evalNode ifNode elseNode) (st, ft) =
  if isTrue (evaluate evalNode (st, ft))
    then evaluate ifNode (st, ft)
    else evaluate elseNode (st, ft)
evaluate NoOp (st, ft) = NullContent -- FIXME this should not be here, it is to fix a empty statement calling eval on noOp

execute :: Node -> (SymbolTable, FunctionTable) -> IO (SymbolTable, FunctionTable)
execute (Print node) (st, ft) = do
  let node' = evaluate node (st, ft)
  print node'
  return (st, ft)
execute (VarDec name expr immutable varType) (st, ft) = do
  let !value = evaluate expr (st, ft)
  let !st' = createVariable (name, value, immutable, varType) st
  return (st', ft)
execute (Assignment name expr) (st, ft) = do
  let !value = evaluate expr (st, ft)
  let !st' = setSymbol (name, value) st
  return (st', ft)
execute (Block nodes) (st, ft) = do
  let sst = pushScope st
  (st', ft') <- foldM (flip execute) (sst, ft) (reverse nodes) -- Nodes will be right to left, thus reverse nodes
  let ust = popScope st'
  return (ust, ft')
execute (If evalNode ifNode elseNode) (st, ft) = do
  let !value = evaluate evalNode (st, ft)
  if isTrue value
    then do
      execute ifNode (st, ft)
    else do
      execute elseNode (st, ft)
execute (While evalNode node) (st, ft) = do
  let !value = evaluate evalNode (st, ft)
  if isTrue value
    then do
      !nextSt <- execute node (st, ft)
      execute (While evalNode node) nextSt
    else
      return (st, ft)
execute (For assignment condition update expression) (st, ft) = do
  -- ?should assignment be  passed and executed multiple times, or updated directly?
  (st', ft') <- execute assignment (st, ft)
  let !continue = evaluate condition (st', ft')
  if isTrue continue
    then do
      !exprSt <- execute expression (st', ft')
      !updateSt <- execute update exprSt
      !nextSt <- execute (For NoOp condition update expression) updateSt -- No op so that it executes init once
      return nextSt
    else
      return (st', ft')
execute NoOp (st, ft) = return (st, ft)

isTrue :: Variable -> Bool
isTrue value = case value of
  BoolContent True -> True
  BoolContent False -> False
  _ -> compilerSemanticError "Invalid type for condition, expected boolean value"

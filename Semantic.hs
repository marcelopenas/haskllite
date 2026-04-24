{-# LANGUAGE BangPatterns #-}

module Semantic (evaluate, execute, Node (..)) where

import CompilerError (compilerSemanticError)
import Control.Monad (foldM)
import Data.Bits (Bits (xor, (.&.), (.|.)))
import Data.Char (intToDigit)
import GHC.IO (unsafePerformIO)
import SymbolTable (SymbolTable, Variable (..), createVariable, getSymbol, popScope, pushScope, setSymbol)
import Token (VarType (BooleanT, F64T, I32T, StrT))

data Node
  = IntNode Int
  | FloatNode Float
  | BoolNode Bool
  | StringNode String
  | CastNode Node VarType
  | UnOp String Node
  | BinOp String Node Node
  | Identifier String
  | Print Node
  | Scan
  | If Node Node Node -- Condition Expression If Node Else Node
  | While Node Node -- Condition Expression
  | For Node Node Node Node -- Assignment Condition Update Expression
  | Assignment String Node -- Name Expression
  | VarDec String Node Bool VarType -- -- Name Expression Immutable Type
  | Block [Node]
  | FuncDec String VarType [(String, VarType)] Node -- name returnType [(arg, argType)] Block
  | FuncCall String [Node] -- name [Expression]
  | Return Node -- Expression
  | NoOp
  deriving (Show)

evaluate :: Node -> SymbolTable -> Variable
evaluate Scan st = IntContent $ unsafePerformIO (readLn :: IO Int) -- TODO change to StringContent
evaluate (IntNode n) st = IntContent n
evaluate (FloatNode n) st = FloatContent n
evaluate (BoolNode n) st = BoolContent n
evaluate (StringNode n) st = StringContent n
evaluate (CastNode n F64T) st = case n' of
  IntContent n -> FloatContent (fromIntegral n :: Float)
  FloatContent n -> FloatContent n
  _ -> compilerSemanticError "Invalid cast to f64"
  where
    n' = evaluate n st
evaluate (CastNode n I32T) st = case n' of
  FloatContent n -> IntContent (round n :: Int) -- Should be truncate
  IntContent n -> IntContent n
  _ -> compilerSemanticError "Invalid cast to i32"
  where
    n' = evaluate n st
evaluate (CastNode n StrT) st = case n' of
  FloatContent n -> StringContent (show n)
  IntContent n -> StringContent (show n)
  StringContent n -> StringContent n
  BoolContent n -> StringContent (show n)
  _ -> compilerSemanticError "Invalid cast to string"
  where
    n' = evaluate n st
evaluate (CastNode n BooleanT) st = case n' of
  FloatContent n -> BoolContent (n /= 0)
  IntContent n -> BoolContent (n /= 0)
  StringContent n -> BoolContent (n /= "")
  BoolContent n -> BoolContent n
  _ -> compilerSemanticError "Invalid cast to boolean"
  where
    n' = evaluate n st
evaluate (UnOp op a) st = case (op, a') of
  ("+", IntContent a') -> IntContent a'
  ("+", _) -> compilerSemanticError "Invalid operator UnOp + for non i32"
  ("-", IntContent a') -> IntContent $ -a'
  ("-", _) -> compilerSemanticError "Invalid operator UnOp - for non i32"
  ("!", IntContent a') -> IntContent $ fromEnum $ not $ toEnum a'
  ("!", BoolContent a') -> BoolContent $ not a'
  ("!", _) -> compilerSemanticError "Invalid operator UnOp ! for non i32 | bool"
  where
    a' = evaluate a st
evaluate (BinOp op a b) st = case (op, a', b') of
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
    a' = evaluate a st
    b' = evaluate b st
evaluate (Identifier name) st = getSymbol name st
evaluate (If evalNode ifNode elseNode) st =
  if isTrue (evaluate evalNode st)
    then evaluate ifNode st
    else evaluate elseNode st
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
  let sst = pushScope st
  !st' <- foldM (flip execute) sst (reverse nodes) -- Nodes will be right to left, thus reverse nodes
  let ust = popScope st'
  return ust
execute (If evalNode ifNode elseNode) st = do
  let !value = evaluate evalNode st
  if isTrue value
    then do
      execute ifNode st
    else do
      execute elseNode st
execute (While evalNode node) st = do
  let !value = evaluate evalNode st
  if isTrue value
    then do
      !nextSt <- execute node st
      execute (While evalNode node) nextSt
    else
      return st
execute (For assignment condition update expression) st = do
  -- ?should assignment be  passed and executed multiple times, or updated directly?
  !st' <- execute assignment st
  let !continue = evaluate condition st'
  if isTrue continue
    then do
      !exprSt <- execute expression st'
      !updateSt <- execute update exprSt
      !nextSt <- execute (For NoOp condition update expression) updateSt -- No op so that it executes init once
      return nextSt
    else
      return st'
execute NoOp st = return st

isTrue :: Variable -> Bool
isTrue value = case value of
  BoolContent True -> True
  BoolContent False -> False
  _ -> compilerSemanticError "Invalid type for condition, expected boolean value"

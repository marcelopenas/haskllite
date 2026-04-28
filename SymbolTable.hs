{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module SymbolTable
  ( SymbolTable (..),
    getSymbol,
    setSymbol,
    createVariable,
    newSymbolTable,
    getOffset,
    pushScope,
    popScope,
    typeMatch,
    Variable (..),
  )
where

import CompilerError (compilerSemanticError)
import Control.DeepSeq (NFData)
import Data.Data (Data (toConstr))
import Data.List (any, elemIndex, find, nub)
import GHC.Generics (Generic)
import Token (VarType (BooleanT, F64T, I32T, StrT, UnityT))

data Variable
  = IntContent Int
  | FloatContent Float
  | StringContent String
  | BoolContent Bool
  | NullContent
  deriving (Data, Eq, Generic, NFData)

instance Show Variable where
  show :: Variable -> String
  show (IntContent s) = show s
  show (FloatContent s) = show s
  show (StringContent s) = s
  show (BoolContent True) = "true"
  show (BoolContent False) = "false"
  show NullContent = ""

type Immutable = Bool

type Symbol = (String, Variable, Immutable, VarType)

type Frame = [Symbol]

data SymbolTable
  = EmptyST
  | Scope Frame SymbolTable
  deriving (Show, Eq, Generic, NFData)

lookup4 :: (Eq a) => a -> [(a, b, c, d)] -> Maybe (a, b, c, d)
lookup4 key = find (\(k, _, _, _) -> k == key)

getSymbol :: String -> SymbolTable -> Variable
getSymbol name EmptyST = compilerSemanticError $ "Undefined variable: " ++ name
getSymbol name (Scope frame st) =
  case lookup4 name frame of
    Nothing -> getSymbol name st
    Just (_, content, _, _) -> content

existsInCurrentFrame :: String -> [Symbol] -> Bool
existsInCurrentFrame name = any (\(n, _, _, _) -> n == name)

createVariable :: Symbol -> SymbolTable -> SymbolTable
createVariable _ EmptyST = error "Scope [symbol] EmptyST"
createVariable symbol@(name, content, _, varType) (Scope frame parent)
  | existsInCurrentFrame name frame = compilerSemanticError $ "Variable already declared in current scope: " ++ name
  | typeMatch varType content = Scope (symbol : frame) parent -- ?should create error when a var is declared with same name on prev scopes?
  | otherwise = compilerSemanticError $ "Invalid type on variable declaration" ++ " Expected " ++ show varType ++ " but got " ++ show (toConstr content) ++ " on variable: " ++ name

typeMatch :: VarType -> Variable -> Bool
typeMatch I32T (IntContent _) = True
typeMatch F64T (FloatContent _) = True
typeMatch StrT (StringContent _) = True
typeMatch BooleanT (BoolContent _) = True
typeMatch _ NullContent = True -- to support var declaration without assignment, and to support returning null on void functions
-- typeMatch UnityT NullContent = True
typeMatch _ _ = False

setSymbol :: (String, Variable) -> SymbolTable -> SymbolTable
setSymbol (name, content) EmptyST = compilerSemanticError $ "Undefined variable: " ++ name
setSymbol (name, content) (Scope frame st) =
  case lookup4 name frame of
    Just (name, _, immutable, varType)
      | immutable -> compilerSemanticError $ "Cannot modify immutable variable: " ++ name
      | typeMatch varType content -> Scope ((name, content, immutable, varType) : filter (\(nn, _, _, _) -> nn /= name) frame) st
      | otherwise -> compilerSemanticError $ "Type mismatch: cannot assign " ++ show (toConstr content) ++ " to " ++ show varType
    Nothing -> Scope frame (setSymbol (name, content) st)

getAllSymbols :: SymbolTable -> [Symbol]
getAllSymbols EmptyST = []
getAllSymbols (Scope frame st) = frame ++ getAllSymbols st

getOffset :: String -> SymbolTable -> Int
getOffset name st =
  case elemIndex name uniqueNames of
    Just idx -> (idx + 1) * (-4)
    Nothing -> compilerSemanticError $ "Undefined variable: " ++ name
  where
    uniqueNames = reverse (nub [n | (n, _, _, _) <- getAllSymbols st])

newSymbolTable :: SymbolTable
newSymbolTable = pushScope EmptyST

pushScope :: SymbolTable -> SymbolTable
pushScope = Scope []

popScope :: SymbolTable -> SymbolTable
popScope (Scope _ st) = st
popScope EmptyST = compilerSemanticError "Cannot pop scope from an empty symbol table"

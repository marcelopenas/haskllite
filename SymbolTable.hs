module SymbolTable
  ( SymbolTable (..),
    getSymbol,
    setSymbol,
    createVariable,
    newSymbolTable,
    Content (..),
  )
where

import CompilerError (compilerSemanticError)
import Data.List (find)
import Token (VarType)

data Content = IntContent Int | StringContent String | BoolContent Bool deriving (Eq)

instance Show Content where
  show :: Content -> String
  show (IntContent s) = show s
  show (StringContent s) = s
  show (BoolContent True) = "True"
  show (BoolContent False) = "False"

type Variable = Content

type Immutable = Bool

type Symbol = (String, Variable, Immutable, VarType)

type SymbolTable = [Symbol]

lookup4 :: (Eq a) => a -> [(a, b, c, d)] -> Maybe (a, b, c, d)
lookup4 key = find (\(k, _, _, _) -> k == key)

getSymbol :: String -> SymbolTable -> Content
getSymbol name table = case lookup4 name table of
  Just (_, content, _, _) -> content
  Nothing -> compilerSemanticError $ "Undefined variable: " ++ name

createVariable :: Symbol -> SymbolTable -> SymbolTable
createVariable symbol@(name, variable, immutable, varType) table = case lookup4 name table of
  Just (name, content, immutable, varType) -> compilerSemanticError $ "tried creating new variable with conflicting names: " ++ show name
  _ -> symbol : table

setSymbol :: String -> Content -> SymbolTable -> SymbolTable
setSymbol name content table = case lookup4 name table of
  Just (_, _, True, _) -> compilerSemanticError $ "tried redefining immutable variable: " ++ show name
  Just (_, _, immutable, varType) -> (name, content, immutable, varType) : table
  _ -> compilerSemanticError $ "tried assigning value to undeclared variable: " ++ show name

newSymbolTable :: SymbolTable
newSymbolTable = []
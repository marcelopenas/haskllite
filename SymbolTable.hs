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

data Content = IntContent Int | StringContent String | BoolContent Bool deriving (Eq)

instance Show Content where
  show :: Content -> String
  show (IntContent s) = show s
  show (StringContent s) = s
  show (BoolContent True) = "True"
  show (BoolContent False) = "False"

type Variable = (Content, Immutable) -- FIXME Mutability should be in Symbol

type Immutable = Bool

type Symbol = (String, Variable)

type SymbolTable = [Symbol]

getSymbol :: String -> SymbolTable -> Content
getSymbol name table = case lookup name table of
  Just value -> fst value
  Nothing -> compilerSemanticError $ "Undefined variable: " ++ name

createVariable :: Symbol -> SymbolTable -> SymbolTable
createVariable symbol@(name, variable) table = case lookup name table of
  Just (content, immutable) -> compilerSemanticError $ "tried creating new variable with conflicting names: " ++ show name
  _ -> symbol : table

setSymbol :: Symbol -> SymbolTable -> SymbolTable
setSymbol symbol@(name, variable) table = case lookup name table of
  Just (_, True) -> compilerSemanticError $ "tried redefining immutable variable: " ++ show name
  Just (_, _) -> symbol : table
  _ -> compilerSemanticError $ "tried assigning value to undeclared variable: " ++ show name

newSymbolTable :: SymbolTable
newSymbolTable = []
module SymbolTable
  ( SymbolTable (..),
    getSymbol,
    setSymbol,
    newSymbolTable,
  )
where

type Variable = Int

type Symbol = (String, Variable)

type SymbolTable = [Symbol]

getSymbol :: String -> SymbolTable -> Variable
getSymbol name table = case lookup name table of
  Just value -> value
  Nothing -> error $ "[Semantic] Undefined variable: " ++ name

setSymbol :: Symbol -> SymbolTable -> SymbolTable
setSymbol symbol table = symbol : table

newSymbolTable :: SymbolTable
newSymbolTable = []
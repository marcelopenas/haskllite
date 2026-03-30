module SymbolTable
  ( SymbolTable (..),
    getSymbol,
    setSymbol,
    newSymbolTable,
  )
where

type Content = Int

type Variable = (Content, Immutable)

type Immutable = Bool

type Symbol = (String, Variable)

type SymbolTable = [Symbol]

getSymbol :: String -> SymbolTable -> Content
getSymbol name table = case lookup name table of
  Just value -> fst value
  Nothing -> error $ "[Semantic] Undefined variable: " ++ name

setSymbol :: Symbol -> SymbolTable -> SymbolTable
setSymbol symbol table = case lookup name table of
  Just (_, True) -> error $ "[Semantic] tried redefining immutable variable: " ++ show name
  _ -> symbol : table
  where
    (name, variable) = symbol

newSymbolTable :: SymbolTable
newSymbolTable = []
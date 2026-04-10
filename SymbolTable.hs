module SymbolTable
  ( SymbolTable (..),
    getSymbol,
    setSymbol,
    newSymbolTable,
    Content (..),
  )
where

data Content = IntContent Int | StringContent String | BoolContent Bool deriving (Eq)

instance Show Content where
  show :: Content -> String
  show (IntContent s) = show s
  show (StringContent s) = s
  show (BoolContent True) = "True"
  show (BoolContent False) = "False"

type Variable = (Content, Immutable)

type Immutable = Bool

type Symbol = (String, Variable)

type SymbolTable = [Symbol]

getSymbol :: String -> SymbolTable -> Content
getSymbol name table = case lookup name table of
  Just value -> fst value
  Nothing -> error $ "[Semantic] Undefined variable: " ++ name

setSymbol :: Symbol -> SymbolTable -> SymbolTable
setSymbol symbol@(name, variable) table = case lookup name table of
  Just (_, True) -> error $ "[Semantic] tried redefining immutable variable: " ++ show name
  _ -> symbol : table

newSymbolTable :: SymbolTable
newSymbolTable = []
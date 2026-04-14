module SymbolTable
  ( SymbolTable (..),
    getSymbol,
    setSymbol,
    createVariable,
    newSymbolTable,
    Variable (..),
  )
where

import CompilerError (compilerSemanticError)
import Data.Data (Data (toConstr))
import Data.List (find)
import Token (VarType (BooleanT, I32T, StrT))

data Variable
  = IntContent Int
  | StringContent String
  | BoolContent Bool
  | NullContent
  deriving (Data, Eq)

instance Show Variable where
  show :: Variable -> String
  show (IntContent s) = show s
  show (StringContent s) = s
  show (BoolContent True) = "true"
  show (BoolContent False) = "false"
  show NullContent = ""

type Immutable = Bool

type Symbol = (String, Variable, Immutable, VarType)

type SymbolTable = [Symbol]

lookup4 :: (Eq a) => a -> [(a, b, c, d)] -> Maybe (a, b, c, d)
lookup4 key = find (\(k, _, _, _) -> k == key)

getSymbol :: String -> SymbolTable -> Variable
getSymbol name table = case lookup4 name table of
  Just (_, content, _, _) -> content
  Nothing -> compilerSemanticError $ "Undefined variable: " ++ name

createVariable :: Symbol -> SymbolTable -> SymbolTable
createVariable symbol@(name, variable, immutable, varType) table = case lookup4 name table of
  Just (name, content, immutable, varType) -> compilerSemanticError $ "tried creating new variable with conflicting names: " ++ show name
  _
    | typeMatch varType variable -> symbol : table
    | otherwise -> compilerSemanticError $ "tried assigning variable " ++ show name ++ " to invalid type, is: " ++ show varType ++ " tried: " ++ show (toConstr variable)

typeMatch :: VarType -> Variable -> Bool
typeMatch I32T (IntContent _) = True
typeMatch StrT (StringContent _) = True
typeMatch BooleanT (BoolContent _) = True
typeMatch _ NullContent = True
typeMatch _ _ = False

setSymbol :: (String, Variable) -> SymbolTable -> SymbolTable
setSymbol (name, content) table = case lookup4 name table of
  Just (_, _, True, _) -> compilerSemanticError $ "tried redefining immutable variable: " ++ show name
  Just (name, _, immutable, varType) ->
    if typeMatch varType content
      then (name, content, immutable, varType) : table
      else compilerSemanticError $ "could not assign type: " ++ show (toConstr content) ++ " to: " ++ show varType
  _ -> compilerSemanticError $ "tried assigning value to undeclared variable: " ++ show name

newSymbolTable :: SymbolTable
newSymbolTable = []
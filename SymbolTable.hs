module SymbolTable
  ( SymbolTable (..),
    getSymbol,
    setSymbol,
    createVariable,
    newSymbolTable,
    getOffset,
    Variable (..),
  )
where

import CompilerError (compilerSemanticError)
import Data.Data (Data (toConstr))
import Data.List (any, elemIndex, find, nub)
import Token (VarType (BooleanT, F64T, I32T, StrT))

data Variable
  = IntContent Int
  | FloatContent Float
  | StringContent String
  | BoolContent Bool
  | NullContent
  deriving (Data, Eq)

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
  deriving (Show, Eq)

lookup4 :: (Eq a) => a -> [(a, b, c, d)] -> Maybe (a, b, c, d)
lookup4 key = find (\(k, _, _, _) -> k == key)

existsSymbol :: String -> SymbolTable -> Bool
existsSymbol name st = case st of
  EmptyST -> False
  Scope frame st' -> any (\(n, _, _, _) -> n == name) frame || existsSymbol name st'

getSymbol :: String -> SymbolTable -> Variable
getSymbol name st = case st of
  EmptyST -> compilerSemanticError $ "Undefined variable: " ++ name
  Scope frame st' ->
    case lookup4 name frame of
      Just (_, content, _, _) -> content
      Nothing -> getSymbol name st'

createVariable :: Symbol -> SymbolTable -> SymbolTable
createVariable symbol@(name, _, _, _) st =
  if existsSymbol name st
    then compilerSemanticError $ "Variable already exists: " ++ name
    else case st of
      EmptyST -> Scope [symbol] EmptyST
      Scope frame st' -> Scope (symbol : frame) st'

typeMatch :: VarType -> Variable -> Bool
typeMatch I32T (IntContent _) = True
typeMatch F64T (FloatContent _) = True
typeMatch StrT (StringContent _) = True
typeMatch BooleanT (BoolContent _) = True
typeMatch _ NullContent = True
typeMatch _ _ = False

setSymbol :: (String, Variable) -> SymbolTable -> SymbolTable
setSymbol (name, content) st = case st of
  EmptyST -> compilerSemanticError $ "Undefined variable: " ++ name
  Scope frame st' ->
    case lookup4 name frame of
      Just (n, oldContent, immutable, varType) ->
        if immutable
          then compilerSemanticError $ "Cannot modify immutable variable: " ++ name
          else
            if typeMatch varType content
              then Scope ((n, content, immutable, varType) : filter (\(nn, _, _, _) -> nn /= name) frame) st'
              else compilerSemanticError $ "Type mismatch: cannot assign " ++ show (toConstr content) ++ " to " ++ show varType
      Nothing -> Scope frame (setSymbol (name, content) st')

getAllSymbols :: SymbolTable -> [Symbol]
getAllSymbols st = case st of
  EmptyST -> []
  Scope frame st' -> frame ++ getAllSymbols st'

getOffset :: String -> SymbolTable -> Int
getOffset name st =
  case elemIndex name uniqueNames of
    Just idx -> (idx + 1) * (-4)
    Nothing -> compilerSemanticError $ "Undefined variable: " ++ name
  where
    uniqueNames = reverse (nub [n | (n, _, _, _) <- getAllSymbols st])

newSymbolTable :: SymbolTable
newSymbolTable = EmptyST

pushScope :: SymbolTable -> SymbolTable
pushScope st = case st of
  Scope frame st' -> Scope [] (Scope frame st') -- st' = Inner ST
  EmptyST -> compilerSemanticError "Cant pop scope to nothing"

popScope :: SymbolTable -> SymbolTable
popScope st = case st of
  Scope _ st' -> st' -- st' =  Inner ST
  EmptyST -> compilerSemanticError "Cant pop scope to nothing"

module FuncTable
  ( FuncTable (..),
    getFunc,
    createFunc,
    newFuncTable,
    Func (..),
  )
where

import CompilerError (compilerSemanticError)
import Data.List (find)
import Semantic (Node)
import Token (VarType)

type Arg = (String, VarType)

type Func = (String, [Arg], VarType, Node) -- Name returnType [(arg, argType)] block

type FuncTable = [Func]

lookup4 :: (Eq a) => a -> [(a, b, c, d)] -> Maybe (a, b, c, d)
lookup4 key = find (\(k, _, _, _) -> k == key)

getFunc :: String -> FuncTable -> Func
getFunc name table = case lookup4 name table of
  Just func -> func
  Nothing -> compilerSemanticError $ "Undefined func: " ++ name

createFunc :: Func -> FuncTable -> FuncTable
createFunc func@(name, _, _, _) table = case lookup4 name table of
  Just (name, _, _, _) -> compilerSemanticError $ "tried creating new func with conflicting names: " ++ show name
  _ -> func : table

newFuncTable :: FuncTable
newFuncTable = []

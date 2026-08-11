module Source.FunctionTable
  ( FunctionTable (..),
    getFunc,
    createFunc,
    newFunctionTable,
    Func (..),
  )
where

import Data.List (find)
import Source.CompilerError (compilerSemanticError)
import Source.Node (Node)
import Source.Token (VarType)

type Arg = (String, VarType)

type Func = (String, [Arg], VarType, Node) -- Name returnType [(arg, argType)] block

type FunctionTable = [Func]

lookup4 :: (Eq a) => a -> [(a, b, c, d)] -> Maybe (a, b, c, d)
lookup4 key = find (\(k, _, _, _) -> k == key)

getFunc :: String -> FunctionTable -> Func
getFunc name table = case lookup4 name table of
  Just func -> func
  Nothing -> compilerSemanticError $ "Undefined func: " ++ name

createFunc :: Func -> FunctionTable -> FunctionTable
createFunc func@(name, _, _, _) table = case lookup4 name table of
  Just (name, _, _, _) -> compilerSemanticError $ "tried creating new func with conflicting names: " ++ show name
  _ -> func : table

newFunctionTable :: FunctionTable
newFunctionTable = []

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Source.Node (Node (..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Source.Token (VarType)

data Node
  = IntNode Int
  | FloatNode Float
  | BoolNode Bool
  | StringNode !String
  | UnityNode
  | CastNode Node VarType
  | UnOp String Node
  | BinOp String Node Node
  | Identifier String
  | Print Node
  | Scan
  | If Node Node Node -- Condition Expression If Node Else Node
  | While Node Node -- Condition Expression
  | For Node Node Node Node -- Assignment Condition Update Expression
  | Assignment String Node -- Name Expression
  | VarDec String Node Bool VarType -- -- Name Expression Immutable Type
  | Block [Node]
  | FuncDec String VarType [(String, VarType)] Node -- name returnType [(arg, argType)] Block
  | FuncCall String [Node] -- name [Expression]
  | Return Node -- Expression
  | Struct String [Node] -- name [varDec]
  | StructAccess Node String -- Node:should_be_Identifier fieldName
  | StructAssign Node String Node -- Node:should_be_Identifier fieldName valueExpr
  | NoOp
  deriving (Show, Generic, NFData)
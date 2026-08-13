{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Source.SemanticASM (generate) where

import Data.List (intercalate)
import Data.Unique (hashUnique, newUnique)
import GHC.IO (unsafePerformIO)
import Source.CompilerError (compilerSemanticError)
import Source.FunctionTable (FunctionTable, createFunc, getFunc)
import Source.Node (Node (..))
import Source.SymbolTable (SymbolTable, Variable (..), createVariable, getOffset, getSymbol, newSymbolTable)
import Source.Token (VarType (BooleanT, F64T, I32T, StrT))

pattern ValidSum :: (Node, Node)
pattern ValidSum <-
  ( \case
      (IntNode _, IntNode _) -> True
      (Identifier _, IntNode _) -> True
      (IntNode _, Identifier _) -> True
      (Identifier _, Identifier _) -> True
      (IntNode _, UnOp {}) -> True
      (UnOp {}, IntNode _) -> True
      (Identifier _, UnOp {}) -> True
      (UnOp {}, Identifier _) -> True
      (UnOp {}, UnOp {}) -> True
      (IntNode _, BinOp {}) -> True
      (BinOp {}, IntNode _) -> True
      (Identifier _, BinOp {}) -> True
      (BinOp {}, Identifier _) -> True
      (BinOp {}, BinOp {}) -> True
      _ -> False ->
      True
    )

pattern ValidEq :: (Node, Node)
pattern ValidEq <-
  ( \case
      (IntNode _, IntNode _) -> True
      (BoolNode _, BoolNode _) -> True
      (Identifier _, IntNode _) -> True
      (IntNode _, Identifier _) -> True
      (Identifier _, Identifier _) -> True
      _ -> False ->
      True
    )

pattern ValidXor :: (Node, Node)
pattern ValidXor <- ValidSum

pattern ValidMul :: (Node, Node)
pattern ValidMul <- ValidSum

pattern ValidDiv :: (Node, Node)
pattern ValidDiv <- ValidSum

pattern ValidRelOp :: (Node, Node)
pattern ValidRelOp <- ValidEq

pattern ValidBoolOp :: (Node, Node)
pattern ValidBoolOp <-
  ( \case
      (BoolNode _, BoolNode _) -> True
      (Identifier _, BoolNode _) -> True
      (BoolNode _, Identifier _) -> True
      (Identifier _, Identifier _) -> True
      _ -> True ->
      True
    )

pattern ValidNeg :: Node
pattern ValidNeg <-
  ( \case
      IntNode _ -> True
      Identifier _ -> True
      BinOp {} -> True
      UnOp _ _ -> True
      _ -> False ->
      True
    )

joinLines :: [String] -> String
joinLines = intercalate "\n"

freshLabel :: String -> String
freshLabel prefix = prefix ++ "_" ++ show (hashUnique $ unsafePerformIO newUnique)

generate :: Node -> (SymbolTable, FunctionTable) -> (SymbolTable, FunctionTable, [String])
generate = generateWithReturnLabel Nothing

generateWithReturnLabel :: Maybe String -> Node -> (SymbolTable, FunctionTable) -> (SymbolTable, FunctionTable, [String])
generateWithReturnLabel retLabel (IntNode val) (st, ft) =
  ( st,
    ft,
    [ "; IntNode",
      "mov eax, " ++ show val
    ]
  )
generateWithReturnLabel _ (FloatNode _) (st, ft) =
  (st, ft, [compilerSemanticError "f64 is not supported in compiler mode yet"])
generateWithReturnLabel _ (BoolNode val) (st, ft) =
  ( st,
    ft,
    [ "; BoolNode",
      "mov eax, " ++ if val then "1" else "0"
    ]
  )
generateWithReturnLabel _ (StringNode _) (st, ft) =
  (st, ft, [compilerSemanticError "str is not supported in compiler mode yet"])
generateWithReturnLabel _ UnityNode (st, ft) =
  ( st,
    ft,
    [ "; UnityNode",
      "mov eax, 0"
    ]
  )
generateWithReturnLabel retLabel (CastNode node targetType) (st, ft) = case targetType of
  I32T ->
    let (st', ft', asmNode) = generateWithReturnLabel retLabel node (st, ft)
     in (st', ft', "; CastNode to i32" : asmNode)
  BooleanT ->
    let (st', ft', asmNode) = generateWithReturnLabel retLabel node (st, ft)
     in ( st',
          ft',
          [ "; CastNode to bool",
            joinLines asmNode,
            "cmp eax, 0",
            "mov ecx, 0",
            "mov eax, 1",
            "cmove eax, ecx"
          ]
        )
  F64T -> (st, ft, [compilerSemanticError "Cast to f64 is not supported in compiler mode yet"])
  StrT -> (st, ft, [compilerSemanticError "Cast to str is not supported in compiler mode yet"])
generateWithReturnLabel _ NoOp (st, ft) = (st, ft, ["; NoOp"])
generateWithReturnLabel retLabel (UnOp op a) (st, ft) =
  case op of
    "+" ->
      let (st', ft', asmA) = generateWithReturnLabel retLabel a (st, ft)
       in (st', ft', ["; UnOp +", joinLines asmA])
    "-" -> case a of
      ValidNeg ->
        let (st', ft', asmA) = generateWithReturnLabel retLabel a (st, ft)
         in ( st',
              ft',
              [ "; UnOp -",
                joinLines asmA,
                "neg eax"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator UnOp - for non i32"])
    "!" ->
      let (st', ft', asmA) = generateWithReturnLabel retLabel a (st, ft)
       in ( st',
            ft',
            [ "; UnOp !",
              joinLines asmA,
              "xor eax, 1"
            ]
          )
    _ -> (st, ft, [compilerSemanticError $ "Unknown unary operator: " ++ op])
generateWithReturnLabel retLabel (BinOp op a b) (st, ft) =
  case op of
    "+" -> case (a, b) of
      ValidSum ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp +",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "add eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp + for non i32"])
    "-" -> case (a, b) of
      ValidSum ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp -",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "sub eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp - for non i32"])
    "*" -> case (a, b) of
      ValidMul ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp *",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "imul eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp * for non i32"])
    "/" -> case (a, b) of
      (IntNode _, IntNode 0) -> (st, ft, [compilerSemanticError "Division by zero literal in BinOp /"])
      ValidDiv ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp /",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "cdq",
                "idiv ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp / for non i32"])
    "**" -> case (a, b) of
      (_, IntNode bVal) | bVal <= 0 -> (st, ft, [compilerSemanticError "Negative exponent not supported"])
      ValidMul ->
        let idPower = freshLabel "pow"
            exitPower = freshLabel "pow_exit"
            (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp ** (Power)",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "mov edx, eax",
                "mov eax, 1",
                "cmp ecx, 0",
                "jle " ++ exitPower,
                idPower ++ ":",
                "imul eax, edx",
                "dec ecx",
                "jnz " ++ idPower,
                exitPower ++ ":"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp ** for non i32"])
    "^" -> case (a, b) of
      ValidXor ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp ^",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "xor eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp ^ for non i32"])
    "==" -> case (a, b) of
      ValidEq ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp ==",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "cmp eax, ecx",
                "mov ecx, 1",
                "mov eax, 0",
                "cmove eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp == for non i32 | bool"])
    "&&" -> case (a, b) of
      ValidBoolOp ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp &&",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "and eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp && for non-boolean"])
    "||" -> case (a, b) of
      ValidBoolOp ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp ||",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "or eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp || for non-boolean"])
    ">" -> case (a, b) of
      ValidRelOp ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp >",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "cmp eax, ecx",
                "mov ecx, 1",
                "mov eax, 0",
                "cmovg eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp > for non i32 | bool"])
    "<" -> case (a, b) of
      ValidRelOp ->
        let (stB, ftB, asmB) = generateWithReturnLabel retLabel b (st, ft)
            (stA, ftA, asmA) = generateWithReturnLabel retLabel a (stB, ftB)
         in ( stA,
              ftA,
              [ "; BinOp <",
                joinLines asmB,
                "push eax",
                joinLines asmA,
                "pop ecx",
                "cmp eax, ecx",
                "mov ecx, 1",
                "mov eax, 0",
                "cmovl eax, ecx"
              ]
            )
      _ -> (st, ft, [compilerSemanticError "Invalid operator BinOp < for non i32 | bool"])
    _ -> (st, ft, [compilerSemanticError $ "Unknown binary operator: " ++ op])
generateWithReturnLabel _ (Identifier name) (st, ft) = (st, ft, asmCode)
  where
    !value = getSymbol name st
    !offset = getOffset name st
    asmCode =
      [ "; Identifier",
        "mov eax, [ebp" ++ show offset ++ "]"
      ]
generateWithReturnLabel retLabel (VarDec name expr immutable varType) (st, ft) = (st'', ft', asmSource')
  where
    st' = createVariable (name, NullContent, immutable, varType) st
    (st'', ft', asmSourceE) = generateWithReturnLabel retLabel expr (st', ft)
    offset = getOffset name st''
    asmSource' =
      [ "; Declaration",
        "sub esp, 4",
        joinLines asmSourceE,
        "mov [ebp" ++ show offset ++ "], eax"
      ]
generateWithReturnLabel retLabel (Assignment name expr) (st, ft) = (st', ft', asmSource')
  where
    _ = getSymbol name st
    (st', ft', asmSourceE) = generateWithReturnLabel retLabel expr (st, ft)
    offset = getOffset name st
    asmSource' =
      [ "; Assignment",
        joinLines asmSourceE,
        "mov [ebp" ++ show offset ++ "], eax"
      ]
generateWithReturnLabel retLabel (Print node) (st, ft) = (st', ft', asmCode)
  where
    (st', ft', nodeAsmCode) = generateWithReturnLabel retLabel node (st, ft)
    asmCode =
      [ "; Print",
        joinLines nodeAsmCode,
        "push eax",
        "push format_out",
        "call printf",
        "add esp, 8"
      ]
generateWithReturnLabel _ Scan (st, ft) = (st, ft, asmCode)
  where
    asmCode =
      [ "; Scanln",
        "push scan_int",
        "push format_in",
        "call scanf",
        "add esp, 8",
        "mov eax, dword [scan_int]"
      ]
generateWithReturnLabel retLabel (Block nodes) (st, ft) = (st', ft', asmCodes)
  where
    (st', ft', asmCodes) =
      foldl
        ( \(stAcc, ftAcc, asmAcc) node ->
            let (stNext, ftNext, asmNext) = generateWithReturnLabel retLabel node (stAcc, ftAcc)
             in (stNext, ftNext, asmAcc ++ asmNext)
        )
        (st, ft, [""])
        (reverse nodes)
generateWithReturnLabel retLabel (While evalNode execNode) (st, ft) =
  ( st',
    ft',
    [ "; While",
      loopLabel ++ ":",
      joinLines evalAsmCode,
      "cmp eax, 0",
      "je " ++ exitLabel,
      joinLines execAsmCode,
      "jmp " ++ loopLabel,
      exitLabel ++ ":"
    ]
  )
  where
    loopLabel = freshLabel "loop"
    exitLabel = freshLabel "exit"
    (evalSt, evalFt, evalAsmCode) = generateWithReturnLabel retLabel evalNode (st, ft)
    (st', ft', execAsmCode) = generateWithReturnLabel retLabel execNode (evalSt, evalFt)
generateWithReturnLabel retLabel (For assignment condition update expression) (st, ft) =
  ( stExpr,
    ftExpr,
    [ "; For",
      joinLines initAsm,
      loopLabel ++ ":",
      joinLines condAsm,
      "cmp eax, 0",
      "je " ++ exitLabel,
      joinLines exprAsm,
      joinLines updateAsm,
      "jmp " ++ loopLabel,
      exitLabel ++ ":"
    ]
  )
  where
    loopLabel = freshLabel "for_loop"
    exitLabel = freshLabel "for_exit"
    (stInit, ftInit, initAsm) = generateWithReturnLabel retLabel assignment (st, ft)
    (stCond, ftCond, condAsm) = generateWithReturnLabel retLabel condition (stInit, ftInit)
    (stExpr, ftExpr, exprAsm) = generateWithReturnLabel retLabel expression (stCond, ftCond)
    (_, _, updateAsm) = generateWithReturnLabel retLabel update (stExpr, ftExpr)
generateWithReturnLabel retLabel (If evalNode ifNode elseNode) (st, ft) =
  ( st'',
    ft'',
    [ "; If-Else",
      joinLines evalAsmCode,
      "cmp eax, 0",
      "je " ++ elseLabel,
      joinLines ifAsmCode,
      "jmp " ++ exitLabel,
      elseLabel ++ ":",
      joinLines elseAsmCode,
      exitLabel ++ ":"
    ]
  )
  where
    elseLabel = freshLabel "else"
    exitLabel = freshLabel "if_exit"
    (stEval, ftEval, evalAsmCode) = generateWithReturnLabel retLabel evalNode (st, ft)
    (stIf, ftIf, ifAsmCode) = generateWithReturnLabel retLabel ifNode (stEval, ftEval)
    (st'', ft'', elseAsmCode) = generateWithReturnLabel retLabel elseNode (stIf, ftIf)
generateWithReturnLabel _ (FuncDec name returnType args block) (st, ft) =
  ( st,
    ft',
    [ "; FuncDec " ++ name,
      "jmp " ++ declEndLabel,
      funcLabel ++ ":",
      "push ebp",
      "mov ebp, esp"
    ]
      ++ paramsAsm
      ++ bodyAsm
      ++ [ returnLabel ++ ":",
           "mov esp, ebp",
           "pop ebp",
           "ret",
           declEndLabel ++ ":"
         ]
  )
  where
    ft' = createFunc (name, args, returnType, block) ft
    funcLabel = "func_" ++ name
    returnLabel = freshLabel ("func_return_" ++ name)
    declEndLabel = freshLabel ("func_decl_end_" ++ name)
    (fnSt, paramsAsm) =
      foldl
        ( \(stAcc, asmAcc) (idx, (argName, argType)) ->
            let stArg = createVariable (argName, NullContent, False, argType) stAcc
                argOffset = getOffset argName stArg
                callOffset = 8 + idx * 4
                asmArg =
                  [ "sub esp, 4",
                    "mov eax, [ebp+" ++ show callOffset ++ "]",
                    "mov [ebp" ++ show argOffset ++ "], eax"
                  ]
             in (stArg, asmAcc ++ asmArg)
        )
        (newSymbolTable, [])
        (zip [0 ..] args)
    (_, _, bodyAsm) = generateWithReturnLabel (Just returnLabel) block (fnSt, ft')
generateWithReturnLabel retLabel (FuncCall name args) (st, ft) =
  if length params /= length args
    then
      ( st,
        ft,
        [ compilerSemanticError $
            "Argument count mismatch in function call to "
              ++ name
              ++ ", expected "
              ++ show (length params)
              ++ " but got "
              ++ show (length args)
        ]
      )
    else
      ( st',
        ft',
        [ "; FuncCall " ++ name
        ]
          ++ pushArgsAsm
          ++ [ "call " ++ funcLabel,
               "add esp, " ++ show (length args * 4)
             ]
      )
  where
    (_, params, _, _) = getFunc name ft
    funcLabel = "func_" ++ name
    (st', ft', pushArgsAsm) =
      foldl
        ( \(stAcc, ftAcc, asmAcc) arg ->
            let (stNext, ftNext, argAsm) = generateWithReturnLabel retLabel arg (stAcc, ftAcc)
             in (stNext, ftNext, asmAcc ++ [joinLines argAsm, "push eax"])
        )
        (st, ft, [])
        (reverse args)
generateWithReturnLabel retLabel (Return expr) (st, ft) =
  case retLabel of
    Nothing ->
      ( st,
        ft,
        [ compilerSemanticError "Return statement outside of function is not supported in compiler mode"
        ]
      )
    Just returnLabel ->
      let (st', ft', exprAsm) = generateWithReturnLabel retLabel expr (st, ft)
       in ( st',
            ft',
            [ "; Return",
              joinLines exprAsm,
              "jmp " ++ returnLabel
            ]
          )
generateWithReturnLabel _ (Struct _ _) (st, ft) =
  (st, ft, [compilerSemanticError "Struct is not supported in compiler mode yet"])
generateWithReturnLabel _ (StructAccess _ _) (st, ft) =
  (st, ft, [compilerSemanticError "Struct field access is not supported in compiler mode yet"])
generateWithReturnLabel _ (StructAssign _ _ _) (st, ft) =
  (st, ft, [compilerSemanticError "Struct field assignment is not supported in compiler mode yet"])

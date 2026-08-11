{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Source.SemanticASM (generate) where

import Control.Monad (foldM)
import Data.List (intercalate)
import Data.Unique (hashUnique, newUnique)
import GHC.IO (unsafePerformIO)
import Source.CompilerError (compilerSemanticError)
import Source.Node (Node (..))
import Source.SymbolTable (SymbolTable, Variable (..), createVariable, getOffset, getSymbol, setSymbol)
import Source.Token (VarType (BooleanT, F64T, I32T, StrT))

pattern ValidSum :: (Node, Node)
pattern ValidSum <-
  ( \case
      (IntNode a, IntNode b) -> True
      (Identifier a, IntNode b) -> True
      (IntNode a, Identifier b) -> True
      (Identifier a, Identifier b) -> True
      -- (IntNode a, _) -> True
      (IntNode a, UnOp {}) -> True
      (UnOp {}, IntNode b) -> True
      (Identifier a, UnOp {}) -> True
      (UnOp {}, Identifier b) -> True
      (UnOp {}, UnOp {}) -> True
      (IntNode a, BinOp {}) -> True
      (BinOp {}, IntNode b) -> True
      (Identifier a, BinOp {}) -> True
      (BinOp {}, Identifier b) -> True
      (BinOp {}, BinOp {}) -> True
      _ -> False ->
      True
    )

pattern ValidEq :: (Node, Node)
pattern ValidEq <-
  ( \case
      (IntNode a, IntNode b) -> True
      (Identifier a, IntNode b) -> True
      (IntNode a, Identifier b) -> True
      (Identifier a, Identifier b) -> True
      _ -> False ->
      True
    )

pattern ValidXor :: (Node, Node)
pattern ValidXor <- ValidSum

pattern ValidMul :: (Node, Node)
pattern ValidMul <- ValidSum

pattern ValidPower :: (Node, Node)
pattern ValidPower <- ValidMul

pattern ValidDiv :: (Node, Node)
pattern ValidDiv <- ValidSum

pattern ValidRelOp :: (Node, Node)
pattern ValidRelOp <- ValidEq

pattern ValidBoolOp :: (Node, Node)
pattern ValidBoolOp <-
  ( \case
      (BoolNode a, BoolNode b) -> True
      (Identifier a, BoolNode b) -> True
      (BoolNode a, Identifier b) -> True
      (Identifier a, Identifier b) -> True
      _ -> True ->
      True
    )

pattern ValidNeg :: Node
pattern ValidNeg <-
  ( \case
      (IntNode a) -> True
      (Identifier a) -> True
      (BinOp {}) -> True
      (UnOp _ _) -> True
      _ -> False ->
      True
    )

joinLines :: [[Char]] -> [Char]
joinLines = intercalate "\n"

generate :: Node -> SymbolTable -> (SymbolTable, [String]) -- TODO add funcTable
generate (IntNode val) st =
  ( st,
    [ "; IntNode",
      "mov eax, " ++ show val
    ]
  )
generate NoOp st = (st, ["; NoOp"])
generate (UnOp op a) st =
  ( st,
    case op of
      "+" ->
        [ "; UnOp +",
          joinLines (snd (generate a st))
        ]
      "-" -> case a of
        ValidNeg ->
          [ "; UnOp -",
            joinLines (snd (generate a st)),
            "neg eax"
          ]
        _ -> compilerSemanticError "Invalid operator UnOp - for non i32 | str"
      "!" ->
        [ "; UnOp !",
          joinLines (snd (generate a st)),
          "xor eax, 1"
        ]
  )
generate (BinOp op a b) st =
  ( st,
    case op of
      "+" -> case (a, b) of
        ValidSum ->
          [ "; BinOp +",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "add eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp + for non i32"
      "-" -> case (a, b) of
        ValidSum ->
          [ "; BinOp -",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "sub eax, ecx"
          ]
      "*" -> case (a, b) of
        ValidMul ->
          [ "; BinOp *",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "imul eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp * for non i32"
      "/" -> case (a, b) of
        (IntNode _, IntNode 0) -> compilerSemanticError "Division by zero literal in BinOp /"
        ValidDiv ->
          [ "; BinOp /",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            -- "xor edx, edx",
            "cdq",
            "idiv ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp / for non i32"
      "**" -> case (a, b) of
        (a, IntNode bVal) | bVal <= 0 -> compilerSemanticError "Negative exponent not supported"
        ValidMul ->
          let idPower = hashUnique $ unsafePerformIO newUnique
           in [ "; BinOp ** (Power)",
                joinLines (snd (generate b st)),
                "push eax",
                joinLines (snd (generate a st)),
                "pop ecx",
                "mov edx, eax",
                "mov eax, 1",
                "cmp ecx, 0",
                "jle exit_pow_" ++ show idPower,
                "pow_loop_" ++ show idPower ++ ":",
                "imul eax, edx",
                "dec ecx",
                "jnz pow_loop_" ++ show idPower,
                "exit_pow_" ++ show idPower ++ ":"
              ]
        _ -> compilerSemanticError "Invalid operator BinOp ** for non i32"
      "^" -> case (a, b) of
        ValidXor ->
          [ "; BinOp ^",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "xor eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp ^"
      "==" -> case (a, b) of
        ValidEq ->
          [ "; BinOp ==",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "cmp eax, ecx",
            "mov ecx, 1",
            "mov eax, 0",
            "cmove eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp == for non i32"
      "&&" -> case (a, b) of
        ValidBoolOp ->
          [ "; BinOp &&",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "and eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp && for non-boolean"
      "||" -> case (a, b) of
        ValidBoolOp ->
          [ "; BinOp ||",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "or eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp || for non-boolean"
      ">" -> case (a, b) of
        ValidRelOp ->
          [ "; BinOp >",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "cmp eax, ecx",
            "mov ecx, 1",
            "mov eax, 0",
            "cmovg eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp > for non i32"
      "<" -> case (a, b) of
        ValidRelOp ->
          [ "; BinOp <",
            joinLines (snd (generate b st)),
            "push eax",
            joinLines (snd (generate a st)),
            "pop ecx",
            "cmp eax, ecx",
            "mov ecx, 1",
            "mov eax, 0",
            "cmovl eax, ecx"
          ]
        _ -> compilerSemanticError "Invalid operator BinOp < for non i32"
      _ -> compilerSemanticError $ "Unknown binary operator: " ++ op
  )
generate (Identifier name) st = (st, asmCode)
  where
    !value = getSymbol name st -- Used for checking if var is declared
    offset = getOffset name st
    asmCode =
      [ "; Identifier",
        "mov eax, [ebp" ++ show offset ++ "]"
      ]
generate (VarDec name expr immutable varType) st = (st'', asmSource')
  where
    st' = createVariable (name, NullContent, immutable, varType) st
    (st'', asmSourceE) = generate expr st'
    offset = getOffset name st''
    asmSource' =
      [ "; Declaration",
        "sub esp, 4",
        joinLines asmSourceE,
        "mov [ebp" ++ show offset ++ "], eax" -- Equivalent to assignment
      ]
generate (Assignment name expr) st = (st', asmSource')
  where
    (st', asmSourceE) = generate expr st
    offset = getOffset name st
    asmSource' =
      [ "; Assignment",
        joinLines asmSourceE,
        "mov [ebp" ++ show offset ++ "], eax"
      ]
generate (Print node) st = (st', asmCode)
  where
    (st', nodeAsmCode) = generate node st
    asmCode =
      [ "; Print",
        joinLines nodeAsmCode,
        "push eax",
        "push format_out",
        "call printf",
        "add esp, 8"
      ]
generate Scan st = (st, asmCode)
  where
    asmCode =
      [ "; Scanln",
        "push scan_int",
        "push format_in",
        "call scanf",
        "add esp, 8",
        "mov eax, dword [scan_int]"
      ]
generate (Block nodes) st = (st', asmCodes)
  where
    (st', asmCodes) =
      foldl
        ( \(stAcc, asmAcc) node ->
            let (stNext, asmNext) = generate node stAcc
             in (stNext, asmAcc ++ asmNext)
        )
        (st, [""])
        (reverse nodes)
generate (While evalNode execNode) st =
  ( st',
    [ "; While",
      "loop_" ++ show identifier ++ ":",
      joinLines evalAsmCode,
      "cmp eax, 0",
      "je exit_" ++ show identifier,
      joinLines execAsmCode,
      "jmp loop_" ++ show identifier,
      "exit_" ++ show identifier ++ ":"
    ]
  )
  where
    identifier = hashUnique $ unsafePerformIO newUnique
    (evalSt, evalAsmCode) = generate evalNode st
    (st', execAsmCode) = generate execNode evalSt
generate (If evalNode ifNode elseNode) st =
  ( st'',
    [ "; If-Else",
      joinLines evalAsmCode,
      "cmp eax, 0",
      "je else_" ++ show identifier,
      joinLines ifAsmCode,
      "jmp exit_" ++ show identifier,
      "else_" ++ show identifier ++ ":",
      joinLines elseAsmCode,
      "exit_" ++ show identifier ++ ":"
    ]
  )
  where
    identifier = hashUnique $ unsafePerformIO newUnique
    (stEval, evalAsmCode) = generate evalNode st
    (stIf, ifAsmCode) = generate ifNode stEval
    (st'', elseAsmCode) = generate elseNode stIf

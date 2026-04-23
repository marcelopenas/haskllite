{-# LANGUAGE BangPatterns #-}

import Parser.Run (run)
import PreProcess (preProcess)
import Semantic (Node (Assignment, BinOp, BoolNode, For, Identifier, IntNode, NoOp, Print, VarDec), execute)
import SemanticASM (generate)
import SymbolTable (newSymbolTable)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeDirectory, takeExtension)
import Token (VarType (I32T))

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "[Main] no source file provided"
    else do
      let filePath = head args
      let flag = tail args
      if takeExtension filePath /= ".rs"
        then error "[Main] must be .rs file"
        else do
          compilerInput <- readFile filePath
          if flag == ["--compiled"]
            then do
              let !(st, asmCode) = generate (run $ preProcess compilerInput ++ "\n") newSymbolTable
              writeFile (takeDirectory filePath ++ "/" ++ (takeBaseName filePath ++ ".asm")) (formatAsmCode $ unlines asmCode)
            else do
              !st <- execute (run $ preProcess compilerInput ++ "\n") newSymbolTable
              return ()

formatAsmCode :: String -> String
formatAsmCode asmCode =
  unlines
    [ "section .data",
      "  format_out: db \"%d\", 10, 0 ; format printf",
      "  format_in: db \"%d\", 0 ; format scanf",
      "  scan_int: dd 0; 32-bits integer",
      "",
      "section .text",
      "",
      "  extern printf",
      "  extern scanf",
      "  global _start",
      "",
      "_start:",
      "  push ebp ; store EBP",
      "  mov ebp, esp ; clear stack",
      "  ; Code start"
    ]
    ++ asmCode
    ++ unlines
      [ "",
        "  ; Code end",
        "  mov esp, ebp ; re-establish stack",
        "  pop ebp",
        "",
        "; exit",
        "  mov eax, 1",
        "  xor ebx, ebx",
        "  int 0x80"
      ]
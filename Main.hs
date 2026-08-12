{-# LANGUAGE BangPatterns #-}

import Control.DeepSeq (deepseq)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeDirectory, takeExtension)
import Source.FunctionTable (newFunctionTable)
import Source.Parser.Run (run)
import Source.PreProcess (preProcess)
import Source.Semantic (execute)
import Source.SemanticASM (generate)
import Source.SymbolTable (newSymbolTable)
import Source.Token (VarType (I32T))
import Source.FormatAsmCode (formatAsmCode)

getFilePath :: [string] -> string
getFilePath [] = error "[Main] no arguments provided"
getFilePath (arg:args) = arg

getFlag :: [string] -> [string]
getFlag [] = error "[Main] no flags provided"
getFlag (arg:args) = args

main :: IO ()
main = do
  args <- getArgs
  if null args
    then error "[Main] no source file provided"
    else do
      let filePath = getFilePath args
      let flags = getFlag args
      if takeExtension filePath /= ".rs"
        then error "[Main] must be .rs file"
        else do
          compilerInput <- readFile filePath
          if flags == ["--compiled"]
            then do
              let !(st, asmCode) = generate (run $ preProcess compilerInput ++ "\n") newSymbolTable
              writeFile (takeDirectory filePath ++ "/" ++ (takeBaseName filePath ++ ".asm")) (formatAsmCode $ unlines asmCode)
            else do
              !sourceScope <- execute (run $ preProcess compilerInput ++ "\n") (newSymbolTable, newFunctionTable)
              sourceScope `deepseq` return ()

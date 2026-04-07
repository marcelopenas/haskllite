import Parser.Run (run)
import PreProcess (preProcess)
import Semantic (execute)
import SymbolTable (newSymbolTable)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "[Main] no source file provided"
    else do
      let filePath = head args
      compilerInput <- readFile filePath
      !table <- execute (run $ preProcess compilerInput ++ "\n") newSymbolTable
      return ()

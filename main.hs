import Lexer (Lexer)
import Parser (run)
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
      execute (run $ preProcess compilerInput) newSymbolTable
      return ()

import Data.Char (isDigit, isSpace)
import Parser (run)
import System.Environment (getArgs)
import Lexer (Lexer)
import Semantic (evaluate)

main :: IO ()
main = do
  args <- getArgs
  let compilerInput = firstElement args -- Gets run arguments
  print $ compilerEntry compilerInput -- If first argument exists run

firstElement :: [element] -> Maybe element -- Gets first element from list if it exists
firstElement [] = Nothing
firstElement (x : _) = Just x

compilerEntry :: Maybe String ->  Int -- Verifies if input is sane
compilerEntry Nothing = error "[Invocation] must pass argument"
compilerEntry (Just compilerInput) = evaluate $ run compilerInput

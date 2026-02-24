import Data.Char (isDigit, isSpace)
import Parser (run)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let compilerInput = firstElement args -- Gets run arguments
  case compilerEntry compilerInput of -- If first argument exists run
    Right result -> print result
    Left err -> error ("Error: " ++ err)

firstElement :: [element] -> Maybe element -- Gets first element from list if it exists
firstElement [] = Nothing
firstElement (x : _) = Just x

compilerEntry :: Maybe String -> Either String Int -- Verifies if input is sane
compilerEntry Nothing = Left "must pass argument"
compilerEntry (Just compilerInput)
  | null compilerInput = Left "input cannot be empty"
  | otherwise = Right (run compilerInput)

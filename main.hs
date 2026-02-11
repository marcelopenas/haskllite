import Data.Char (isDigit, isSpace)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let compilerInput = safeFirstElement args -- Gets run arguments
  case compilerInput of
    Nothing -> error "Error: must pass argument"
    Just compilerInputString -> case compilerEntry compilerInputString of -- If first argument exists run
      Right result -> print result
      Left err -> error ("Error: " ++ err)

safeFirstElement :: [element] -> Maybe element -- Gets first element from list if it exists
safeFirstElement [] = Nothing
safeFirstElement (x : xs) = Just x

compilerEntry :: String -> Either String Int -- Verifies if input is sane
compilerEntry compilerInput
  | null compilerInput = Left "input cannot be empty"
  | otherwise = parser compilerInput

parseNumber :: String -> Either String (Int, String)
parseNumber inputStr =
  let trimmed = dropWhile isSpace inputStr
      (ds, remaining) = span isDigit trimmed
  in if null ds then Left "expected number" else Right (read ds, remaining)

parseOp :: String -> Either String (Char, String)
parseOp inputStr
  | opChar == '+' || opChar == '-' = Right (opChar, afterOp)
  | otherwise = Left "expected operator"
  where
    trimmed = dropWhile isSpace inputStr
    (opChar : afterOp) = trimmed

loopEval :: Int -> String -> Either String Int
loopEval total remainingStr
  | null trimmedRest = Right total
  | otherwise = do
      (op, afterOp) <- parseOp trimmedRest
      (num, remainingAfter) <- parseNumber afterOp
      let total' = if op == '+' then total + num else total - num
      loopEval total' remainingAfter
  where
    trimmedRest = dropWhile isSpace remainingStr

parser :: String -> Either String Int
parser input = do
  (initialValue, remaining) <- parseNumber input
  let remaining' = dropWhile isSpace remaining
  loopEval initialValue remaining'
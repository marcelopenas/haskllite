import Data.Char (isDigit, isSpace)
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
  | otherwise = parser compilerInput

parser :: String -> Either String Int -- Sends input to eval
parser input = do
  (initialValue, remaining) <- parseNumber input
  let remaining' = dropWhile isSpace remaining
  loopEval initialValue remaining'

-- Loops alternating number and operator and if it mismatches will error
loopEval :: Int -> String -> Either String Int -- Iterates through input and send to specific parser
loopEval total remainingStr
  | null trimmedRest = Right total
  | otherwise = do
      (op, afterOp) <- parseOp trimmedRest
      (num, remainingAfter) <- parseNumber afterOp
      let total' = if op == '+' then total + num else total - num
      loopEval total' remainingAfter
  where
    trimmedRest = dropWhile isSpace remainingStr

parseNumber :: String -> Either String (Int, String) -- Receives string and pareses it to number, errors if its the wrong type
parseNumber inputStr
  | null ds = Left "expected number"
  | otherwise = Right (read ds, remaining)
  where trimmed = dropWhile isSpace inputStr
        (ds, remaining) = span isDigit trimmed

parseOp :: String -> Either String (Char, String) -- Receives string and pareses it to operator, errors if its the wrong type
parseOp inputStr
  | opChar == '+' || opChar == '-' = Right (opChar, afterOp)
  | otherwise = Left "expected operator"
  where trimmed = dropWhile isSpace inputStr
        (opChar : afterOp) = trimmed

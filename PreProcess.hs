module PreProcess
  ( preProcess,
  )
where

import Data.List (findIndex, isPrefixOf, stripPrefix, tails)
import Data.Text (Text, pack, replace, unpack)

preProcess :: String -> String
preProcess source = substituteConstants $ removeComments source

removeBetween :: Int -> Int -> String -> String
removeBetween startIdx endIdx str =
  take startIdx str ++ drop endIdx str

-- * Comments

removeComments :: String -> String
removeComments [] = []
-- Match the start of a comment
removeComments ('/' : '/' : xs) = removeComments (dropToNewline xs)
-- Keep the character and move to the next
removeComments (x : xs) = x : removeComments xs

-- Helper to skip everything until a newline
dropToNewline :: String -> String
dropToNewline [] = []
dropToNewline ('\n' : xs) = xs -- Keep the newline or skip it? Usually keep it.
dropToNewline (_ : xs) = dropToNewline xs

-- * Constants

type Name = String

type Value = String

type Constant = (Name, Value)

findIndexString :: String -> String -> Maybe Int
findIndexString search str = findIndex (isPrefixOf search) (tails str)

-- Find and remove

substituteConstants :: String -> String
substituteConstants source = replacedSource
  where
    (constants, modifiedSource) = substituteConstantsLoop newConstantTable source
    replacedSource = replaceConstants constants modifiedSource
    newConstantTable = []

substituteConstantsLoop :: [Constant] -> String -> ([Constant], String)
substituteConstantsLoop constants source = case constIndex of
  Just constIndex -> uncurry substituteConstantsLoop $ getConstantOccurrence constants source constIndex
  Nothing -> (constants, source)
  where
    constIndex = findIndexString "const " source

getConstantOccurrence :: [Constant] -> String -> Int -> ([Constant], String)
getConstantOccurrence constants source constIndex = (constant : constants, modifiedSource)
  where
    identifierIndex = constIndex + 6 -- Accounts for 'const '
    (constName, constValueIndex) = getConstName identifierIndex source ""
    (constValue, constValueEndIndex) = getConstValue (constValueIndex + 3) source "" -- Accounts for 'const `name` = '
    constant = (constName, constValue)

    getConstName :: Int -> String -> String -> (String, Int)
    getConstName currentIndex source buildingString = case currentChar of
      ';' -> error "[Lexer] Incomplete define statement"
      ' ' -> (reverse buildingString, currentIndex)
      _ -> getConstName (currentIndex + 1) source (currentChar : buildingString)
      where
        currentChar = source !! currentIndex
    getConstValue :: Int -> String -> String -> (String, Int)
    getConstValue currentIndex source buildingString = case currentChar of
      ';' -> (reverse buildingString, currentIndex)
      _ -> getConstValue (currentIndex + 1) source (currentChar : buildingString)
      where
        currentChar = source !! currentIndex

    modifiedSource = removeBetween constIndex (constValueEndIndex + 1) source

-- Replace

replaceConstants :: [Constant] -> String -> String
replaceConstants [] source = source
replaceConstants (constant : constants) source = replaceConstants constants replacedSource
  where
    replacedSource = replaceConstantsLoop constant source

replaceConstantsLoop :: Constant -> String -> String
replaceConstantsLoop constant source = replacedSource
  where
    (constName, constValue) = constant
    replacedSource = replaceString constName constValue source

replaceString :: String -> String -> String -> String
replaceString old new source = unpack $ replace (pack old) (pack new) (pack source)
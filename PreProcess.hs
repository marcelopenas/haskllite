module PreProcess
  ( preProcess,
  )
where

import Data.List (findIndex, isPrefixOf, stripPrefix, tails)
import Data.Text (Text, pack, replace, unpack)

preProcess :: String -> String
preProcess source = substituteConstants $ removeComment 0 source

substring :: Int -> Int -> String -> String
substring i j s = take (j - i) (drop i s)

removeBetween :: Int -> Int -> String -> String
removeBetween startIdx endIdx str =
  take startIdx str ++ drop endIdx str

-- * Comments

removeComment :: Int -> String -> String
removeComment pos source
  | pos >= sourceLen = source
  | otherwise = removeComment endPos (removeBetween startPos endPos source)
  where
    findStart :: Int -> String -> Int
    findStart pos' source
      | pos' >= sourceLen = pos'
      | otherwise = case (currentChar, nextChar) of
          ('/', '/') -> pos'
          _ -> findStart (pos' + 1) source
      where
        currentChar = source !! pos'
        nextChar = source !! (pos' + 1)

    findEnd :: Int -> String -> Int
    findEnd pos' source
      | pos' >= sourceLen = pos'
      | otherwise = case currentChar of
          '\n' -> pos' + 1
          _ -> findEnd (pos' + 1) source
      where
        currentChar = source !! pos'

    startPos = findStart pos source
    endPos = findEnd startPos source

    sourceLen = length source

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
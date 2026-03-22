module PreProcess
  ( preProcess,
  )
where

import Data.List (isPrefixOf, stripPrefix)

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

-- TODO substituteConstants
substituteConstants :: String -> String
substituteConstants source = source

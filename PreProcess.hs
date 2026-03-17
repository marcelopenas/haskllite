module PreProcess
  ( preProcess,
  )
where

preProcess :: String -> String
preProcess source = source
-- preProcess = filter (not . isComment)
--   where
--     isComment s = s == "//"

module File
  ( readLines,
  )
where

import Control.Exception

-- Returns either a Left (error message), or
-- Right (lines in file).
readLines :: String -> IO (Either String [String])
readLines filename = do
  content <- (try (readFile filename) :: IO (Either SomeException String))
  return $ case content of
    Left failure -> Left $ displayException failure
    Right result -> Right $ lines result

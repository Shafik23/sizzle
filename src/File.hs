module File
  ( readLines,
    writeLines,
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

-- Returns either a Left (error message), or
-- Right (lines written).
writeLines :: FilePath -> [String] -> IO (Either String Int)
writeLines filename inputLines = do
  let len = length inputLines
  result <- (try (writeFile filename (unlines inputLines)) :: IO (Either SomeException ()))
  return $ case result of
    Left failure -> Left $ displayException failure
    Right _ -> Right len

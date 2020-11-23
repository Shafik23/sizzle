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
  return $ transformEither content lines

-- Returns either a Left (error message), or
-- Right (lines written).
writeLines :: FilePath -> [String] -> IO (Either String Int)
writeLines filename inputLines = do
  let len = length inputLines
  result <- (try (writeFile filename (unlines inputLines)) :: IO (Either SomeException ()))
  return $ transformEither result (const len)

transformEither :: Either SomeException t -> (t -> b) -> Either String b
transformEither (Left failure) _ = Left $ displayException failure
transformEither (Right success) f = Right (f success)

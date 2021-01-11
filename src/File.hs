module File
  ( readLines,
    writeLines,
    FailableIO,
  )
where

import Control.Exception
import Data.String

type FailableIO a = IO (Either String a)

-- Returns either a Left (error message), or
-- Right (lines in file).
readLines :: String -> FailableIO [String]
readLines filename = do
  content <- try (readFile filename)
  return $ transformEither content lines

-- Returns either a Left (error message), or
-- Right (lines written).
writeLines :: FilePath -> [String] -> FailableIO Int
writeLines filename inputLines = do
  let len = length inputLines
  result <- try (writeFile filename (unlines inputLines))
  return $ transformEither result (const len)

-- Takes an Either <exception> <result> and translates it to
-- an Either <Error message> <transformed result>; the second argument
-- is a function that transforms the result, if it was successful.
transformEither :: Either SomeException a -> (a -> b) -> Either String b
transformEither (Left failure) _ = Left $ displayException failure
transformEither (Right success) f = fmap f (Right success)

module EZIO
  ( readLines,
    writeLines,
    tryIO,
    FailableIO,
  )
where

import Control.Exception
import Data.Functor
import Data.String

-- This type gives you either a:
-- Left  <String error message>    OR
-- Right <result of type a>
type FailableIO a = IO (Either String a)

-- Returns either a Left (error message), or
-- Right (lines in file).
readLines :: String -> FailableIO [String]
readLines filename = do
  content <- tryIO (readFile filename)
  return $ fmap lines content

-- Returns either a Left (error message), or
-- Right (lines written).
writeLines :: FilePath -> [String] -> FailableIO Int
writeLines filename inputLines = do
  let len = length inputLines
  result <- tryIO (writeFile filename (unlines inputLines))
  return $ fmap (const len) result

-- Takes an Either <exception> <result> and translates it to
-- an Either <Error message> <transformed result>; the second argument
-- is a function that transforms the result, if it was successful.
transformEither :: (a -> b) -> Either SomeException a -> Either String b
transformEither _ (Left failure) = Left $ displayException failure
transformEither f (Right success) = fmap f (Right success)

tryIO :: IO a -> FailableIO a
tryIO = transform . try
  where
    transform = fmap (transformEither id)
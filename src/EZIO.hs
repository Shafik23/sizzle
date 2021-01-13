module EZIO
  ( readLines,
    writeLines,
    tryIO,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

type EitherT = ExceptT

type FailableIO a = EitherT String IO a

-- Returns either a Left (error message), or
-- Right (lines in file).
readLinesHelper :: String -> FailableIO [String]
readLinesHelper filename = do
  content <- tryIO (readFile filename)
  return (lines content)

readLines :: FilePath -> IO (Either String [String])
readLines = runExceptT . readLinesHelper

-- Returns either a Left (error message), or
-- Right (lines written).
writeLinesHelper :: FilePath -> [String] -> FailableIO Int
writeLinesHelper filename inputLines = do
  let len = length inputLines
  result <- tryIO (writeFile filename (unlines inputLines))
  return len

writeLines :: FilePath -> [String] -> IO (Either String Int)
writeLines filename inputLines = runExceptT $ writeLinesHelper filename inputLines

-- Takes an Either <exception> <result> and translates it to
-- an Either <Error message> <transformed result>; the second argument
-- is a function that transforms the result, if it was successful.
transformEither :: (a -> b) -> Either SomeException a -> Either String b
transformEither _ (Left failure) = Left $ displayException failure
transformEither f (Right success) = fmap f (Right success)

tryIO :: IO a -> FailableIO a
tryIO action = do
  result <- liftIO (try action)
  let resultTransformed = transformEither id result
  except resultTransformed

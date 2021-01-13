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

type FailableIO a = IO (Either String a)

-- Returns either a Left (error message), or
-- Right (lines in file).
readLines :: String -> FailableIO [String]
readLines filename = do
  content <- tryIO (readFile filename)
  return (fmap lines content)

-- Returns either a Left (error message), or
-- Right (lines written).
writeLines :: FilePath -> [String] -> FailableIO Int
writeLines filename inputLines = do
  let len = length inputLines
  result <- tryIO (writeFile filename (unlines inputLines))
  return (fmap (const len) result)

-- Takes an Either <exception> <result> and translates it to
-- an Either <Error message> <transformed result>; the second argument
-- is a function that transforms the result, if it was successful.
transformEither :: (a -> b) -> Either SomeException a -> Either String b
transformEither _ (Left failure) = Left $ displayException failure
transformEither f (Right success) = fmap f (Right success)

tryIOHelper :: IO a -> EitherT String IO a
tryIOHelper action = do
  result <- liftIO (try action)
  let resultTransformed = transformEither id result
  except resultTransformed

tryIO :: IO a -> IO (Either String a)
tryIO = runExceptT . tryIOHelper
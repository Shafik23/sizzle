module Main where

import File
import Lib

main :: IO ()
main = do
  writeLines "foo.txt" ["a", "b", "c"]
  result <- readLines "foo.txt"
  let stuff = either id unlines result
  print stuff
  return ()

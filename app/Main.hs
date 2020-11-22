module Main where

import Sizzle

main :: IO ()
main = do
  writeLines "foo.txt" ["a", "b", "c"]
  result <- readLines "foo.txt"
  let stuff = either id unlines result
  print stuff
  return ()

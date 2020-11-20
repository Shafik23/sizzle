module File
  ( readLines,
  )
where

readLines :: String -> IO [String]
readLines filename = do
  content <- readFile filename
  return (lines content)

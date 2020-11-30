module Sizzle
  ( module File,
    module Series,
    helloSizzle,
  )
where

import File
import Series

helloSizzle :: IO ()
helloSizzle = putStrLn "Hello from Sizzle!"

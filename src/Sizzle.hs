module Sizzle
  ( module File,
    helloSizzle,
  )
where

import File

helloSizzle :: IO ()
helloSizzle = putStrLn "Hello from Sizzle!"

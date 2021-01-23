module Main where

import Control.Monad
import Control.Monad.Trans
import Network.Wai.Middleware.RequestLogger
import Sizzle
import System.Random (newStdGen, randomRs)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get "/" $ text "Welcome to a Sizzle, an amalgamation library to help web development"

  get "/random" $ do
    g <- liftIO newStdGen
    json $ take 20 $ randomRs (1 :: Int, 100) g

  get "/jsonExample" $ do
    json ("Something" :: String)

  get "/header" $ do
    agent <- header "User-Agent"
    maybe (raise "User-Agent header not found!") text agent

  get "/static/:file" $ do
    f <- param "file"
    file $ "static/" ++ f

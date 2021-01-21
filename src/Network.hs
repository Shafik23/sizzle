{-# LANGUAGE LambdaCase #-}

module Network
  ( httpGet,
    httpPost,
    httpPatch,
    httpDelete,
    wget,
  )
where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import EZIO (tryIO, writeLines)
import Network.HTTP.Simple
import Web.Scotty

type Url = String

data Method = GET | POST | PATCH | DEL deriving (Show)

httpHelper :: Method -> Url -> IO T.Text
httpHelper method url = do
  request <- parseRequest $ show method ++ " " ++ url
  response <- httpBS request
  return $ (decodeUtf8 . getResponseBody) response

failableHttp :: Method -> Url -> IO (Either String T.Text)
failableHttp method url = tryIO (httpHelper method url)

httpGet :: Url -> IO (Either String T.Text)
httpGet = failableHttp GET

httpPost :: Url -> IO (Either String T.Text)
httpPost = failableHttp POST

httpPatch :: Url -> IO (Either String T.Text)
httpPatch = failableHttp PATCH

httpDelete :: Url -> IO (Either String T.Text)
httpDelete = failableHttp DEL

wget :: Url -> FilePath -> IO (Either String Int)
wget url filename =
  httpGet url >>= \case
    (Left failure) -> return (Left failure)
    (Right urlData) -> writeLines filename (map T.unpack (T.lines urlData))

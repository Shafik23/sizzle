{-# LANGUAGE LambdaCase #-}

module Network
  ( httpGet,
    httpPost,
    httpPatch,
    httpDelete,
    wget,
  )
where

import Control.Exception
import qualified Data.ByteString.Char8 as B8
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding
import EZIO
import Network.HTTP.Simple

type Url = String

data Method = GET | POST | PATCH | DEL deriving (Show)

httpHelper :: Method -> Url -> IO T.Text
httpHelper method url = do
  request <- parseRequest $ show method ++ " " ++ url
  response <- httpBS request
  return $ (decodeUtf8 . getResponseBody) response

failableHttp :: Method -> Url -> FailableIO T.Text
failableHttp method url = tryIO $ httpHelper method url

httpGet :: Url -> FailableIO T.Text
httpGet = failableHttp GET

httpPost :: Url -> FailableIO T.Text
httpPost = failableHttp POST

httpPatch :: Url -> FailableIO T.Text
httpPatch = failableHttp PATCH

httpDelete :: Url -> FailableIO T.Text
httpDelete = failableHttp DEL

wget :: Url -> FilePath -> FailableIO Int
wget url filename =
  httpGet url >>= \case
    (Left failure) -> return (Left failure)
    (Right urlData) -> writeLines filename (map T.unpack (T.lines urlData))

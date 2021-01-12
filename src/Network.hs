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

httpGet :: Url -> IO T.Text
httpGet = httpHelper GET

httpPost :: Url -> IO T.Text
httpPost = httpHelper POST

httpPatch :: Url -> IO T.Text
httpPatch = httpHelper PATCH

httpDelete :: Url -> IO T.Text
httpDelete = httpHelper DEL

wget :: Url -> FilePath -> FailableIO Int
wget url filename =
  httpGet url >>= \content ->
    writeLines filename (map T.unpack (T.lines content))

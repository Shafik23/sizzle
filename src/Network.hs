module Network
  ( httpGet,
  )
where

import Control.Exception
import qualified Data.ByteString.Char8 as B8
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Simple

type Url = String

data Method = GET | POST | PATCH | DEL deriving (Show)

httpHelper :: Method -> Url -> IO Text
httpHelper method url = do
  request <- parseRequest $ show method ++ " " ++ url
  response <- httpBS request
  return $ (decodeUtf8 . getResponseBody) response

httpGet :: Url -> IO Text
httpGet = httpHelper GET

httpPost :: Url -> IO Text
httpPost = httpHelper POST

httpPatch :: Url -> IO Text
httpPatch = httpHelper PATCH

httpDelete :: Url -> IO Text
httpDelete = httpHelper DEL

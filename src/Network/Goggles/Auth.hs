{-# language OverloadedStrings #-}
module Network.Goggles.Auth where

import Network.HTTP.Types
import Network.HTTP.Types.URI (urlEncode, urlDecode)
import Network.HTTP.Types.Header
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Binary.Builder

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), encode, decode)

import Control.Monad.Catch
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Keys



scopes :: [BS.ByteString]
scopes = [ "https://www.googleapis.com/auth/cloud-platform" ]

baseUrl :: BS.ByteString
baseUrl = "https://accounts.google.com/o/oauth2/v2/auth"

mkAuthHeader :: String -> Header
mkAuthHeader tok = (hAuthorization, BS.pack $ unwords ["Bearer :",  tok])

mkContentTypeHeader :: String -> Header
mkContentTypeHeader ct = (hContentType, BS.pack ct)


mkRequest :: MonadThrow m => String -> StdMethod -> [Header] -> m Request
mkRequest url m heads = do
  reqBase <- parseRequest url
  let mbs = renderStdMethod m
  return $ reqBase { method = mbs, requestHeaders = heads }

  




main :: IO ()
main = do
    -- let settings = managerSetProxy
    --         (proxyEnvironment Nothing)
    --         defaultManagerSettings
    man <- newManager defaultManagerSettings-- settings
    req <- parseRequest "http://httpbin.org/get"
    -- let req = "http://httpbin.org/get"
    --         -- Note that the following settings will be completely ignored.
    --         {
    --           proxy = Nothing -- Just $ Proxy "localhost" 1234
    --         }
    -- httpLbs req man >>= print
    respBS <- httpLbs req man
    print $ responseBody respBS -- (decode $ responseBody respBS :: Maybe BS.ByteString)

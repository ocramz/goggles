{-# language OverloadedStrings #-}
module Network.Goggles.Internal.HTTP where

import Network.HTTP.Client
import Network.HTTP.Types

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Monad.Reader
import Control.Monad.Except

import Data.Aeson

import Network.Goggles.Cloud


runRequest :: Request -> Cloud BS.ByteString
runRequest req = do
    manager <- asks hManager
    cloudIO $ do
        res <- httpLbs req manager
        return $ LBS.toStrict $ responseBody res



get :: String -> RequestHeaders -> BS.ByteString -> Cloud BS.ByteString
get uri heads body = do
  req <- cloudIO $ do
    reqBase <- parseUrlThrow uri
    return $ reqBase {
      method = renderStdMethod GET
    , requestHeaders = heads
    , requestBody = RequestBodyBS body
                   }
  runRequest req

post :: String -> RequestHeaders -> BS.ByteString -> Cloud BS.ByteString
post uri heads body = do
  req <- cloudIO $ do
    reqBase <- parseUrlThrow uri
    return $ reqBase {
      method = renderStdMethod POST
    , requestHeaders = heads
    , requestBody = RequestBodyBS body
                   }
  runRequest req


getJSON :: FromJSON a => String -> RequestHeaders -> Cloud a
getJSON url headers = do
    body <- get url headers ""
    case eitherDecodeStrict body of
        Left e -> throwError $ JSONDecodeError e
        Right r -> return r

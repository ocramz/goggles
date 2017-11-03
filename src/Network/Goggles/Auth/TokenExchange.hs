{-# language OverloadedStrings #-}
module Network.Goggles.Auth.TokenExchange where

import Network.Utils.HTTP (urlEncode)

import Data.Keys (gcpPrivateKeyRSA, gcpClientEmail)

import Network.Mime
import Network.HTTP.Req
import qualified Network.HTTP.Client as H -- (RequestBody(..))
import qualified Network.HTTP.Types as H

import Network.Goggles.Internal.Auth.JWT

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Exception (throwIO)

import qualified Data.Aeson as J
import qualified Data.Text as T
import Data.Aeson (object, (.=))

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8            as B8
-- import           Data.ByteString.Base64.URL (encode)
import           Data.Text.Encoding         (encodeUtf8)
import Crypto.Random.Types


-- some actual exception handling may go in the implementation of handleHttpException
instance MonadHttp IO where
  handleHttpException = throwIO

-- | we need a custom content type (`application/...`) associated with this type of JSON payload
newtype ReqBodyJsonUrlEncoded a = ReqBodyJsonUE a

instance J.ToJSON a => HttpBody (ReqBodyJsonUrlEncoded a) where
  getRequestBody (ReqBodyJsonUE a) = H.RequestBodyLBS (J.encode a)
  getRequestContentType _ = pure "application/x-www-form-urlencoded; charset=utf-8\n"


mainTokenExchange = do
  payload <- mkRequestPayload scopes
  r <- req POST 
    (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token") 
    (ReqBodyJsonUE payload) 
    jsonResponse 
    mempty
  print (responseBody r :: J.Value)




testMain = do
  payload <- mkRequestPayload scopes
  r <- req' POST 
    (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token") 
    (ReqBodyJsonUE payload) 
    mempty
    (\request _ -> pure request)
  print $ H.queryString r
  print $ H.requestHeaders r
  print $ payload -- show $ H.requestBody r
  

scopes :: [T.Text]
scopes = ["https://www.googleapis.com/auth/cloud-platform"]


mkRequestPayload
  :: (MonadIO m, MonadThrow m, MonadRandom m) => [T.Text] -> m J.Value
mkRequestPayload scps = do
  clientEmail <- liftIO $ gcpClientEmail
  privateKey <- liftIO $ gcpPrivateKeyRSA
  case (clientEmail, privateKey) of
    (Just ce, Right pk) -> do
      let serv = ServiceAccount pk ce Nothing
          opts = TokenOptions scps (Just 3600)
      jwt <- encodeBearerJWT serv opts
      return $ object [
                     "assertion" .= B8.unpack jwt
                     , "grant_type" .= urlEncode "urn:ietf:params:oauth:grant-type:jwt-bearer"
                      ]
    (_, Left e) -> error e
    (_, _) -> error "Error!"


testRequestPayload = do
  clientEmail <- gcpClientEmail
  privateKey <- gcpPrivateKeyRSA
  case (clientEmail, privateKey) of
    (Just ce, Right pk) -> do
      let serv = ServiceAccount pk ce Nothing
          opts = TokenOptions scopes (Just 1200)
      jwt <- encodeBearerJWT serv opts
      return $ J.encode $ object [
                     "assertion" .= B8.unpack jwt
                     , "grant_type" .= urlEncode "urn:ietf:params:oauth:grant-type:jwt-bearer"
                      ]      

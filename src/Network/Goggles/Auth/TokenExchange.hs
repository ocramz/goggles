{-# language OverloadedStrings #-}
module Network.Goggles.Auth.TokenExchange where

import Network.Utils.HTTP (urlEncode)

import Data.Keys (gcpPrivateKeyRSA, gcpClientEmail)

import Network.Mime
import Network.HTTP.Req
import Network.HTTP.Client (RequestBody(..))

import Network.Goggles.Internal.Auth.JWT (getSignedJWT, JWTError)

import Control.Exception (throwIO)

import qualified Data.Aeson as J
import Data.Aeson (object, (.=))

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8            as B8



instance MonadHttp IO where
  handleHttpException = throwIO


-- | we need a custom content type
newtype ReqBodyJsonUrlEncoded a = ReqBodyJsonUE a

instance J.ToJSON a => HttpBody (ReqBodyJsonUrlEncoded a) where
  getRequestBody (ReqBodyJsonUE a) = RequestBodyLBS (J.encode a)
  getRequestContentType _ = pure "application/x-www-form-urlencoded"


main = do
  payload <- mkRequestPayload
  r <- req POST -- method
    (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token") 
    (ReqBodyJsonUE payload) 
    jsonResponse 
    mempty       
  print (responseBody r :: J.Value)



scopes = ["https://www.googleapis.com/auth/cloud-platform"]

mkRequestPayload :: IO J.Value
mkRequestPayload = do
  clientEmail <- gcpClientEmail
  privateKey <- gcpPrivateKeyRSA
  case (clientEmail, privateKey) of
    (Just ce, Right pk) -> do
      jwtE <- getSignedJWT ce Nothing scopes Nothing pk
      case jwtE of Right jwt ->
                     return $ object [
                     "assertion" .= B8.unpack jwt
                     , "grant_type" .= urlEncode "urn:ietf:params:oauth:grant-type:jwt-bearer"
                     ]
                   Left e -> error $ show e
    (_, Left e) -> error e

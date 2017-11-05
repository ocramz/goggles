{-# language OverloadedStrings, DeriveGeneric, TypeFamilies #-}
module Network.Goggles.Auth.TokenExchange where

import Data.Monoid ((<>))

import GHC.Generics

import Network.Utils.HTTP (urlEncode)

import Data.Keys (gcpPrivateKeyRSA, gcpClientEmail)

import Network.Mime
import Network.HTTP.Req

import qualified Network.HTTP.Client as H -- (RequestBody(..))
import qualified Network.HTTP.Client.TLS as H (tlsManagerSettings)
import qualified Network.HTTP.Types as H

import Network.Goggles.Internal.Auth.JWT

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Exception (throwIO)
import Data.Typeable

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J

import Data.Aeson (object, (.=), (.:), (.:?))

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8            as B8
-- import           Data.ByteString.Base64.URL (encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import Crypto.Random.Types

import qualified Data.Attoparsec.ByteString.Lazy as A

data TokenExchangeException =
    NotFound !String
  | KeysNotFound !String
  deriving (Show, Typeable)
instance Exception TokenExchangeException
  
  
-- some actual exception handling may go in the implementation of handleHttpException
instance MonadHttp IO where
  handleHttpException = throwIO


data OAuth2Token = OAuth2Token {
    _oaTokenExpirySeconds :: Int
  , _oaTokenString :: T.Text
  , _oaTokenType :: T.Text } deriving (Eq, Show, Generic)

instance J.FromJSON OAuth2Token where
  parseJSON = J.withObject "OAuth2Token" $ \js -> OAuth2Token
    <$> js .: "expires_in"
    <*> js .: "access_token"
    <*> js .: "token_type"

get :: MonadHttp m => OAuth2Token -> T.Text -> m LB.ByteString
get (OAuth2Token _ tok _) endpoint = do
  resp <- req GET
    (https "www.googleapis.com" /: endpoint)
    NoReqBody
    lbsResponse
    (oAuth2Bearer $ T.encodeUtf8 tok)
  return $ responseBody resp

post :: MonadHttp m => OAuth2Token -> T.Text -> LB.ByteString -> m LB.ByteString
post (OAuth2Token _ tok _) endpoint payload = do
  resp <- req POST
    (https "www.googleapis.com" /: endpoint)
    (ReqBodyLbs payload)
    lbsResponse
    (oAuth2Bearer $ T.encodeUtf8 tok)
  return $ responseBody resp
  
  
getOAuth2Token :: (MonadIO m, MonadThrow m, MonadRandom m, MonadHttp m) => m OAuth2Token
getOAuth2Token = do
  payload <- signedRequestPayload scopes
  r <- req POST 
    (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token")
    (ReqBodyLbs payload)
    lbsResponse
    (header "Content-Type" "application/x-www-form-urlencoded; charset=utf-8")
  maybe
    (throwM $ NotFound "")
    pure
    (J.decode (responseBody r) :: Maybe OAuth2Token)

scopes :: [T.Text]
scopes = ["https://www.googleapis.com/auth/cloud-platform"]


signedRequestPayload :: (MonadIO m, MonadThrow m, MonadRandom m) => [T.Text] -> m LB.ByteString
signedRequestPayload scps = do
  clientEmail <- liftIO $ gcpClientEmail
  privateKey <- liftIO $ gcpPrivateKeyRSA
  case (clientEmail, privateKey) of
    (Just ce, Right pk) -> do
      let serv = ServiceAccount pk ce Nothing
          opts = TokenOptions scps
      jwt <- encodeBearerJWT serv opts
      return $ LB.fromStrict $
                   "grant_type="
                <> (B8.pack $ urlEncode "urn:ietf:params:oauth:grant-type:jwt-bearer")
                <> "&assertion="
                <> jwt 
    (_, _) -> throwM $ KeysNotFound "Private key and/or client email not found !"








-- -- Sandbox : -- 

-- testMain2 = do
--   manager <- H.newManager H.tlsManagerSettings
--   payload <- mkRequestPayload scopes
--   -- initialRequest <- H.parseRequest "https://www.googleapis.com/oauth2/v4/token"
--   initialRequest <- H.parseRequest "https://www.googleapis.com/oauth2/v4/tokenFoo"
--   let r = initialRequest {
--         H.method = "POST"
--       , H.requestBody = H.RequestBodyLBS $ payload
--       , H.requestHeaders = [(H.hContentType, B8.pack "application/x-www-form-urlencoded")]
--         }
--   -- print (H.requestBody request)
--   -- let (H.RequestBodyLBS rb) = H.requestBody r
--   -- print rb
--   response <- H.httpLbs r manager
--   print (H.responseBody response)
--   -- print (J.decode (H.responseBody response) :: Maybe J.Value)



-- testMain = do
--   payload <- mkRequestPayload scopes
--   r <- req' POST 
--     (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token") 
--     (ReqBodyJsonUE payload) 
--     mempty
--     (\request _ -> pure request)
--   print $ H.queryString r
--   print $ H.requestHeaders r
--   print $ payload -- show $ H.requestBody r


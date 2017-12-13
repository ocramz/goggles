{-# language OverloadedStrings, DeriveGeneric, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts, MultiParamTypeClasses, DataKinds, FlexibleInstances #-}
module Network.Goggles.Auth.TokenExchange (
    scopes
  , GCP
  , requestTokenGCP
                                          ) where

import Data.Monoid ((<>))

import Network.HTTP.Req
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Crypto.Random.Types as CR

import Network.Goggles.Cloud
import Network.Goggles.Types
import Network.Goggles.Auth.OAuth2
import Network.Goggles.Auth.JWT
import Network.Utils.HTTP (putLbs, getLbs, urlEncode)




-- * The GCP type

data GCP

instance HasCredentials GCP where
  type Credentials GCP = GCPServiceAccount
  type TokenContent GCP = T.Text
  tokenFetch = requestTokenGCP

instance Show (Token GCP) where
  show (Token tok time) = unwords ["GCP Token :", T.unpack tok, "; expires at :", show time]

-- | We can provide a custom http exception handler rather than throwing exceptions with this instance  
instance MonadHttp (Cloud GCP) where
  handleHttpException = throwM





-- * Constants

-- | OAuth2 scopes for the various Google Cloud Platform services.
--
-- Please refer to 
--
-- > https://developers.google.com/identity/protocols/googlescopes
-- 
-- for the full list
scopes :: [T.Text]
scopes = ["https://www.googleapis.com/auth/cloud-platform"]

uriBase :: Url 'Https
uriBase = https "www.googleapis.com"



-- * Google Cloud Storage (GCS)

-- | `getObject b p` retrieves the contents of a GCS object (of full path `p`) in bucket `b`
getObject :: T.Text -> T.Text -> Cloud GCP LbsResponse
getObject b p = do
  tok <- accessToken
  let
    opts = oAuth2Bearer (T.encodeUtf8 tok) <>
           altMedia
    uri = uriBase /: "storage" /: "v1" /: "b" /: b /: "o" /: p
  getLbs uri opts

-- | `getObjectMetadata b p` retrieves the metadata of a GCS object (of full path `p`) in bucket `b`
getObjectMetadata :: T.Text -> T.Text -> Cloud GCP LbsResponse
getObjectMetadata b p = do
  tok <- accessToken
  let
    opts = oAuth2Bearer (T.encodeUtf8 tok)
    uri = uriBase /: "storage" /: "v1" /: "b" /: b /: "o" /: p
  getLbs uri opts

--
-- GET https://www.googleapis.com/storage/v1/b/bucket/o
listObjects :: T.Text -> Cloud GCP LbsResponse
listObjects b = do
  tok <- accessToken
  let
    opts = oAuth2Bearer (T.encodeUtf8 tok)
    uri = uriBase /: "storage" /: "v1" /: "b" /: b /: "o"
  getLbs uri opts

-- | `putObject b p body` uploads a bytestring `body` into a GCS object (of full path `p`) in bucket `b`
putObject :: T.Text -> T.Text -> LB.ByteString -> Cloud GCP LbsResponse
putObject b objName body = do
  tok <- accessToken
  let
    opts = oAuth2Bearer (T.encodeUtf8 tok) <>
           ulMedia <>
           ("name" =: objName)
    uri = uriBase /: "upload" /: "storage" /: "v1" /: "b" /: b /: "o"
  putLbs uri opts body 


  
-- | Constant request parameters required by GCS calls
altMedia, ulMedia :: Option 'Https
altMedia = "alt" =: ("media" :: T.Text)
ulMedia = "uploadType" =: ("media" :: T.Text)

  



-- | Implementation of `tokenFetch`
requestTokenGCP :: Cloud GCP (Token GCP)
requestTokenGCP = do
   saOk <- asks credentials
   let opts = GCPTokenOptions scopes
   t0 <- requestGcpOAuth2Token saOk opts
   tutc <- mkOAuth2TokenUTC (2 :: Int) t0
   return (Token (oauTokenString tutc) (oauTokenExpiry tutc))


-- | Request an OAuth2Token
requestGcpOAuth2Token :: (MonadThrow m, CR.MonadRandom m, MonadHttp m) =>
     GCPServiceAccount -> GCPTokenOptions -> m OAuth2Token
requestGcpOAuth2Token serviceAcct opts = do
  jwt <- T.decodeUtf8 <$> encodeBearerJWT serviceAcct opts
  requestOAuth2Token
      (uriBase /: "oauth2" /: "v4" /: "token")
      [("grant_type", T.pack $ urlEncode "urn:ietf:params:oauth:grant-type:jwt-bearer"),
       ("assertion", jwt)]
      (header "Content-Type" "application/x-www-form-urlencoded; charset=utf-8")

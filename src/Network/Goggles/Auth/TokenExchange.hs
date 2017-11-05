{-# language OverloadedStrings, DeriveGeneric, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
module Network.Goggles.Auth.TokenExchange where

import Data.Monoid ((<>))

import GHC.Generics

import Network.Utils.HTTP (urlEncode)



import Network.Mime
import Network.HTTP.Req

import qualified Network.HTTP.Client as H -- (RequestBody(..))
import qualified Network.HTTP.Client.TLS as H (tlsManagerSettings)
import qualified Network.HTTP.Types as H



import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Exception (throwIO, AsyncException)
import Data.Typeable

import Control.Concurrent.STM

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

import Network.Goggles.Internal.Auth.JWT
import Data.Keys (gcpPrivateKeyRSA, gcpClientEmail)


data TokenExchangeException =
    NotFound !String
  | APICredentialsNotFound !String
  deriving (Show, Typeable)
instance Exception TokenExchangeException


data Handle = Handle {
    hToken :: !(TVar (Maybe OAuth2Token))
  -- , hFetchToken :: !(Cloud OAuth2Token)
                     }


data Cloud m a = Cloud { runCloud :: ReaderT Handle m a }

instance Functor f => Functor (Cloud f) where
  fmap f m = Cloud (f <$> runCloud m)

instance Applicative f => Applicative (Cloud f) where
  pure = liftCloud . pure
  f <*> v = Cloud $ ReaderT (\r -> runReaderT f' r <*> runReaderT v' r) where
    f' = runCloud f
    v' = runCloud v

liftCloud :: m a -> Cloud m a
liftCloud m = Cloud $ ReaderT (const m)    

instance Monad m => Monad (Cloud m) where
    m >>= k = Cloud $ ReaderT $ \r -> do
      a <- runReaderT (runCloud m) r
      runReaderT (runCloud $ k a) r

instance MonadIO m => MonadIO (Cloud m) where
  liftIO = lift . liftIO

instance MonadTrans Cloud where
  lift = liftCloud

instance Monad m => MonadReader Handle (Cloud m) where
  ask = ask
  local = local
  reader = reader

-- instance MonadHttp m => MonadHttp (Cloud m) where


evalCloud h m = runReaderT (runCloud m) h `catch` ( \e ->
  case fromException e of
    Just ex -> throwM (ex :: AsyncException)
    Nothing -> return ()
                                                  )




cacheToken :: MonadIO m => OAuth2Token -> Cloud m OAuth2Token
cacheToken token = do
  tokenTVar <- asks hToken
  liftIO $ atomically $ do
    current <- readTVar tokenTVar
    let newToken = case current of
          Nothing -> token
          Just x  -> if _oaTokenExpirySeconds x > _oaTokenExpirySeconds token then x else token
    writeTVar tokenTVar (Just newToken)
    return newToken

  
 
-- some actual exception handling may go in the implementation of handleHttpException
instance MonadHttp IO where
  handleHttpException = throwIO


data OAuth2Token = OAuth2Token {
    _oaTokenExpirySeconds :: Int
  , _oaTokenString :: T.Text
  , _oaTokenType :: T.Text } deriving (Eq, Generic)
instance Show OAuth2Token where
  show (OAuth2Token expt _ _) = unwords ["OAuth2 token expires in", show expt, "seconds"]

instance J.FromJSON OAuth2Token where
  parseJSON = J.withObject "OAuth2Token" $ \js -> OAuth2Token
    <$> js .: "expires_in"
    <*> js .: "access_token"
    <*> js .: "token_type"

-- | Make an authenticated GET request to googleapis.com 
get :: MonadHttp m => OAuth2Token
    -> T.Text           -- ^ request URI path
    -> m LB.ByteString
get (OAuth2Token _ tok _) endpoint = do
  resp <- req GET
    (https "www.googleapis.com" /: endpoint)
    NoReqBody
    lbsResponse
    (oAuth2Bearer $ T.encodeUtf8 tok)
  return $ responseBody resp

-- | Make an authenticated POST request to googleapis.com 
post :: MonadHttp m => OAuth2Token
  -> T.Text          -- ^ request URI path
  -> LB.ByteString   -- ^ request body
  -> m LB.ByteString
post (OAuth2Token _ tok _) endpoint payload = do
  resp <- req POST
    (https "www.googleapis.com" /: endpoint)
    (ReqBodyLbs payload)
    lbsResponse
    (oAuth2Bearer $ T.encodeUtf8 tok)
  return $ responseBody resp
  
-- | Request an OAuth2Token
requestOAuth2Token :: (MonadIO m, MonadThrow m, MonadRandom m, MonadHttp m) => m OAuth2Token
requestOAuth2Token = do
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
    (_, _) -> throwM $ APICredentialsNotFound "Private key and/or client email not found !"








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


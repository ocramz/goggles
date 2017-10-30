{-# language OverloadedStrings, FlexibleContexts #-}
module Network.Goggles.Internal.Auth.JWT where

import qualified Data.ByteString            as B
import           Data.ByteString.Base64.URL (encode)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.UnixTime              (getUnixTime, utSeconds)
import           Foreign.C.Types

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans (liftIO)

import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA.PKCS15 (signSafer)
import Crypto.PubKey.RSA.Types
import Crypto.Random.Types


import Data.Typeable

data JWTError =
    BadExpirationTime !String
  | CryptoSignError !String deriving (Show, Typeable)
instance Exception JWTError where




-- adapted from http://hackage.haskell.org/package/google-oauth2-jwt to use `cryptonite` rather than OpenSSL
getSignedJWT :: (Integral a, MonadRandom m, MonadIO m) =>
       T.Text           -- ^ email address of the service account.
    -> Maybe T.Text  -- ^ email address of the user for which the application is requesting delegated access.
    -> [T.Text]   -- ^ list of the permissions that the application requests.
    -> Maybe a    -- ^ expiration time (maximum and default value is an hour, 3600).
    -> PrivateKey  -- ^ private key gotten from the PEM string obtained from the Google API Console.
    -> m (Either JWTError B.ByteString)
getSignedJWT iss msub scs mxt pk = do
  let xt = fromIntegral (fromMaybe 3600 mxt)
  if (xt<1 || xt>3600) then (return $ Left $ BadExpirationTime $ unwords ["Bad expiration time", show xt]) else do 
  t <- liftIO getUnixTime
  let i = header <> "." <> toB64 ("{\"iss\":\"" <> iss <> "\","
          <> maybe T.empty (\e -> "\"sub\":\"" <> e <> "\",") msub
          <> "\"scope\":\"" <> T.intercalate " " scs <> "\",\"aud\
          \\":\"https://www.googleapis.com/oauth2/v4/token\",\"ex\
          \p\":" <> toT (utSeconds t + CTime xt) <> ",\"iat\":"
          <> toT (utSeconds t) <> "}")
  signed <- signSafer (Just SHA256) pk i
  case signed of Left e -> return $ Left (CryptoSignError $ unwords ["RSA signing error: ", show e])
                 Right s -> return $ Right $ i <> "." <> encode s
  where
    toT = T.pack . show
    toB64 = encode . encodeUtf8
    header = toB64 "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"

 

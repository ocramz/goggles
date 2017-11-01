{-# language OverloadedStrings, FlexibleContexts #-}
module Network.Goggles.Internal.Auth.JWT where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString.Char8            as B8
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
-- import Crypto.Cipher.Types
import Data.X509 
import Data.X509.Memory (readKeyFileFromMemory)

import Data.PEM (pemParseBS, pemParseLBS)


import Data.Typeable

import Data.Keys



data JWTError =
    BadExpirationTime !String
  | CryptoSignError !String deriving (Show, Typeable)
instance Exception JWTError where



-- | SHA256withRSA (also known as RSASSA-PKCS1-V1_5-SIGN with the SHA-256 hash function)
-- adapted from http://hackage.haskell.org/package/google-oauth2-jwt to use `cryptonite` rather than OpenSSL
-- original : https://hackage.haskell.org/package/google-oauth2-jwt-0.2.2/docs/src/Network-Google-OAuth2-JWT.html#getSignedJWT
getSignedJWT :: (Integral a, MonadRandom m, MonadIO m) =>
       T.Text           -- ^ email address of the service account (ending in .gserviceaccount.com ).
    -> Maybe T.Text  -- ^ email address of the user for which the application is requesting delegated access.
    -> [T.Text]   -- ^ authentication scopes that the application requests.
    -> Maybe a    -- ^ expiration time (maximum and default value is an hour, 3600).
    -> PrivateKey  -- ^ private key gotten from the PEM string obtained from the Google API Console.
    -> m (Either JWTError B.ByteString)
getSignedJWT iss msub scs mxt pk = do
  let xt = fromIntegral (fromMaybe 3600 mxt)
  if (xt<1 || xt>3600) then (return $ Left $ BadExpirationTime $ unwords ["Bad expiration time", show xt]) else
    do 
      t <- liftIO getUnixTime
      let i = header <> "." <> toB64 ("{\"iss\":\"" <> iss <> "\"," <> maybe T.empty (\e -> "\"sub\":\"" <> e <> "\",") msub <> "\"scope\":\"" <> T.intercalate " " scs <> "\",\"aud\\\":\"https://www.googleapis.com/oauth2/v4/token\",\"ex\\p\":" <> toT (utSeconds t + CTime xt) <> ",\"iat\":" <> toT (utSeconds t) <> "}")
      signed <- signSafer (Just SHA256) pk i
      case signed of Left e ->
                       return $ Left (CryptoSignError $ unwords ["RSA signing error: ", show e])
                     Right s -> return $ Right $ i <> "." <> encode s
  where
    toT = T.pack . show    
    header = toB64 "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"
    -- Base64url-encoded header : "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9"

-- | Base64url encoding 
toB64 :: T.Text -> B.ByteString  
toB64 = encode . encodeUtf8




toPEM :: T.Text -> B8.ByteString
toPEM k = b1 <> toB64 k <> b2 where
  b1 = B8.pack "-----BEGIN RSA PRIVATE KEY-----\n"
  b2 = B8.pack "\n-----END RSA PRIVATE KEY-----\n"


-- FIXME : why doesn't pemParseBS work with the supplied key?
pw = do
  Just k <- gcpPrivateKey
  -- pure $ toPEM k
  pure $ pemParseBS $ toPEM k




-- fileReadPrivateKey filepath = do
--     pk <- rights . parseKey . pemParseBS <$> B.readFile filepath
--     case pk of
--         []    -> error "no valid RSA key found"
--         (x:_) -> return x

--     where parseKey (Right pems) = map (fmap (PrivRSA . snd) . KeyRSA.decodePrivate . LB.fromChunks . (:[]) . pemContent) pems
--           parseKey (Left err) = error ("Cannot parse PEM file " ++ show err)


-- -- decodePrivate dat = either (Left . show) parsePrivate $ decodeASN1 BER dat          




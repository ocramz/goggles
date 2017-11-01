{-# language OverloadedStrings, FlexibleContexts #-}
{-# language RecordWildCards #-}
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

import Data.PEM -- (pemParseBS, pemParseLBS)

import Data.ASN1.Types
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.BinaryEncoding


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






pw = do
  Just k <- gcpPrivateKey
  pure $ pemParseBS $ toPEM k where
    toPEM :: T.Text -> B8.ByteString
    toPEM k = b1 <> toB64 k <> b2 where
      b1 = B8.pack "-----BEGIN RSA PRIVATE KEY-----\n"
      b2 = B8.pack "\n-----END RSA PRIVATE KEY-----\n"



decodeRSAPrivKey bs = pemParseBS bs >>= \pems -> case pems of
    []           -> Left ("Private key not found: " ++ B8.unpack bs)
    (_:_:_)      -> Left "Too many private keys"
    [p@PEM { .. }] ->
        case pemName of
            "RSA PRIVATE KEY" -> parseRSA p
            _                 -> Left "Unknown private key type"
  where
    parseRSA  :: PEM -> Either String PrivateKey
    parseRSA PEM {..} =
      case decodeASN1' DER pemContent of
          Left er    -> Left (show er)
          Right [ Start Sequence
                , IntVal _version
                , IntVal public_n
                , IntVal public_e
                , IntVal private_d
                , IntVal private_p
                , IntVal private_q
                , IntVal private_dP
                , IntVal private_dQ
                , IntVal private_qinv
                , End Sequence
                ] -> let public_size = calculateSize public_n
                         private_pub = PublicKey { .. }
                     in Right PrivateKey {..}
          Right _ -> Left ("Invalid ASN1 stream found in PEM. " ++ B8.unpack bs)

calculateSize :: Integer -> Int
calculateSize = go 1
  where
    go i n | 2 ^ (i * 8) > n = i
           | otherwise       = go (i + 1) n



-- fileReadPrivateKey filepath = do
--     pk <- rights . parseKey . pemParseBS <$> B.readFile filepath
--     case pk of
--         []    -> error "no valid RSA key found"
--         (x:_) -> return x

--     where parseKey (Right pems) = map (fmap (PrivRSA . snd) . KeyRSA.decodePrivate . LB.fromChunks . (:[]) . pemContent) pems
--           parseKey (Left err) = error ("Cannot parse PEM file " ++ show err)


-- -- decodePrivate dat = either (Left . show) parsePrivate $ decodeASN1 BER dat          




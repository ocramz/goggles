{-# language OverloadedStrings, FlexibleContexts #-}
{-# language RecordWildCards #-}
module Network.Goggles.Internal.Auth.JWT (encodeBearerJWT, JWTError(..), ServiceAccount(..), TokenOptions(..)) where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString.Char8            as B8
-- import           Data.ByteString.Base64.URL (encode)
import qualified Data.Text                  as T
-- import           Data.Text.Encoding         (encodeUtf8)

import qualified Data.Aeson as J
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as J (Pair)

import           Data.Monoid ((<>))

import           Data.UnixTime (getUnixTime, utSeconds, UnixTime(..))
import           Foreign.C.Types

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans (liftIO)
import Control.Monad.Catch

import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA.PKCS15 (signSafer)
import Crypto.PubKey.RSA.Types
import Crypto.Random.Types
-- import Crypto.Cipher.Types

import Data.Typeable

import Data.ByteArray
import Data.ByteArray.Encoding

-- import Network.HTTP.Types


data JWTError =
    BadExpirationTime !String
  | CryptoSignError !String
  deriving (Show, Typeable)
instance Exception JWTError where



data ServiceAccount = ServiceAccount {
    _servicePrivateKey :: PrivateKey  -- ^ private key gotten from the PEM string obtained from the Google API Console
  , _serviceEmail :: T.Text    -- ^ email address of the service account (ending in .gserviceaccount.com )
  , _serviceAccountUser :: Maybe T.Text -- ^ email address of the user for which the application is requesting delegated access
                                     } deriving (Eq, Show)

data TokenOptions = TokenOptions {
    _tokenOptionsScopes :: [T.Text] -- ^ authentication scopes that the application requests
                                 } deriving (Eq, Show)
  



-- | Returns a bytestring with the signed JWT-encoded token request. 
-- adapted from https://github.com/brendanhay/gogol/blob/master/gogol/src/Network/Google/Auth/ServiceAccount.hs
encodeBearerJWT :: (MonadThrow m, MonadRandom m, MonadIO m) =>
                   ServiceAccount
                -> TokenOptions
                -> m B8.ByteString
encodeBearerJWT s opts = do
    i <- input <$> liftIO getUnixTime
    r <- signSafer (Just SHA256) (_servicePrivateKey s) i
    either failure (pure . concat' i) r
  where
    concat' i x = i <> "." <> signature (base64 x)
    failure e = throwM $ CryptoSignError (show e)
    signature bs = case B8.unsnoc bs of
            Nothing         -> mempty
            Just (bs', x)
                | x == '='  -> bs'
                | otherwise -> bs
    input t = header <> "." <> payload
      where
        header = base64Encode
            [ "alg" .= ("RS256" :: T.Text)
            , "typ" .= ("JWT"   :: T.Text) ]
        payload = base64Encode $
            [ "aud"   .= T.pack "https://www.googleapis.com/oauth2/v4/token" 
            , "scope" .= (T.intercalate " " $ _tokenOptionsScopes opts)
            , "iat"   .= toT (utSeconds t)
            , "exp"   .= toT (CTime 3600 + utSeconds t)
            , "iss"   .= _serviceEmail s
            ] <> maybe [] (\sub -> ["sub" .= sub]) (_serviceAccountUser s)
        toT = T.pack . show


base64Encode :: [J.Pair] -> B8.ByteString
base64Encode = base64 . LB.toStrict . J.encode . J.object

base64 :: ByteArray a => a -> B8.ByteString
base64 = convertToBase Base64URLUnpadded

-- -- textBody = RequestBodyBS . encodeUtf8

maybeDefault :: (b -> Bool) -> b -> Maybe b -> b
maybeDefault q d = maybe d ff where
  ff t | q t = d
       | otherwise = t



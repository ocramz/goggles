{-# language OverloadedStrings #-}
module Data.Keys (parseRSAPrivateKey) where

import Data.Monoid ((<>))

import qualified Data.Text as T
import           Data.Text.Encoding         (encodeUtf8)
-- import qualified Data.Text.IO as T

import Control.Monad.Catch
-- import Control.Monad.IO.Class

import Data.X509 
import Data.X509.Memory (readKeyFileFromMemory)
import Crypto.PubKey.RSA.Types

import Network.Goggles.Control.Exceptions


-- | Parse a chunk of text into an RSA private key. For Google Cloud Platform , this is the private key associated with the user's "service account" (for server-to-server API use)
--
-- > https://console.cloud.google.com/apis/credentials
--
-- Note: do /not/ supply the RSA header and footer or any newlines (they will be inserted by this function).
parseRSAPrivateKey :: MonadThrow m => T.Text -> m PrivateKey
parseRSAPrivateKey k =
  case parseRSAPrivateKey_helper k of [] -> throwM $ NoParsePK "Cannot parse RSA key"
                                      (PrivKeyRSA ok:_) -> return ok
                                      _ -> throwM $ NoRSAKey "Found key is not a RSA private key"


parseRSAPrivateKey_helper :: T.Text -> [PrivKey]
parseRSAPrivateKey_helper = readKeyFileFromMemory . withPEMheaders encodeUtf8 where
  withPEMheaders encf k = b1 <> encf k <> b2 where
      b1 = encf $ T.pack "-----BEGIN RSA PRIVATE KEY-----\n"
      b2 = encf $ T.pack "\n-----END RSA PRIVATE KEY-----\n"

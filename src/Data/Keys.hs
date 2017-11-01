{-# language OverloadedStrings #-}
module Data.Keys
  (gcpUID
  , gcpPrivateKeyRSA
  , gcpServiceAccountSecret
  ) where

import Data.Monoid 

import qualified Data.Text as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A

import qualified Data.Map as M

import Data.Char

import Data.X509 
import Data.X509.Memory (readKeyFileFromMemory)
import Crypto.PubKey.RSA.Types


{- |
Parse a text file containing keys/environment variables of the form <KEY>=<VALUE>.
Obviously this file shouldn't be in source control (e.g. should be mentioned in .gitignore )
-}

fname :: String
fname = "secrets"

parseSecrets :: IO (M.Map T.Text T.Text)
parseSecrets = do
  secs <- T.readFile fname
  case A.parseOnly (A.sepBy kvPair A.endOfLine) secs of
    Left e -> error e
    Right x -> return $ M.fromList x

-- gcpUID, gcpPrivateKey, gcpServiceAccountSecret :: IO (Maybe T.Text)
gcpUID = do
  ma <- parseSecrets
  return $ M.lookup "GCS_CLIENT_EMAIL" ma

gcpServiceAccountSecret = do 
  ma <- parseSecrets
  return $ M.lookup "GCP_SERVICE_ACCOUNT_SECRET" ma
  
  
validName :: A.Parser T.Text
validName = A.takeWhile1 (\c ->
                            isAlphaNum c ||
                            c == '_')

quotedString :: A.Parser T.Text
quotedString = do
  _ <- "\""
  x <- A.takeTill (\c -> c == '\"')
  _ <- "\""
  return x

kvPair :: A.Parser (T.Text, T.Text)
kvPair = do
  k <- validName
  _ <- "="
  v <- quotedString
  return (k, v)



gcpPrivateKeyRSA :: IO (Either String PrivateKey)
gcpPrivateKeyRSA = do 
  ma <- parseSecrets
  case M.lookup "GCS_PRIVATE_KEY" ma of
    Just k ->  
      case parseRSAPrivateKey k of [] -> return $ Left "Cannot parse RSA key"
                                   (PrivKeyRSA ok:_) -> return $ Right ok
                                   _ -> return $ Left "Found key is not a RSA private key"
    Nothing -> return $ Left "No RSA key called GCS_PRIVATE_KEY found"

parseRSAPrivateKey :: T.Text -> [PrivKey]
parseRSAPrivateKey = readKeyFileFromMemory . withPEMheaders encodeUtf8  


  


-- | e.g. withPEMheaders encodeUtf8 
withPEMheaders :: Monoid m => (T.Text -> m) -> T.Text -> m
withPEMheaders encf k = b1 <> encf k <> b2 where
      b1 = encf $ T.pack "-----BEGIN RSA PRIVATE KEY-----\n"
      b2 = encf $ T.pack "\n-----END RSA PRIVATE KEY-----\n"

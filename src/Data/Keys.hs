{-# language OverloadedStrings #-}
module Data.Keys where
 
import Data.Monoid 

import qualified Data.Text as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A

import Control.Exception
import Data.Typeable

import qualified Data.Map as M

import Data.Char

import Data.X509 
import Data.X509.Memory (readKeyFileFromMemory)
import Crypto.PubKey.RSA.Types


{- |
Parse a text file containing keys/environment variables of the form <KEY>=<VALUE>.
Obviously this file shouldn't be in source control (e.g. should be mentioned in .gitignore )
-}


-- | Filename containing the secrets
fname :: String
fname = "secrets"

data GCPKeys = GCPKeys {
    _gcpClientEmail :: T.Text
  , _gcpPrivateKeyRSA :: PrivateKey
  , _gcpServiceAcctSecret :: T.Text
                       } deriving (Eq, Show)


data KeyException = NoPKFound !String
  | NoParsePK !String
  | NoRSAKey !String
  deriving (Eq, Show, Typeable)
instance Exception KeyException

readPrivateKey :: M.Map T.Text a -> Either KeyException a
readPrivateKey ma =
  case M.lookup "GCP_PRIVATE_KEY" ma of
    Nothing -> Left $ NoPKFound "Cannot find RSA key GCP_PRIVATE_KEY"
    Just kt -> Right kt

readClientEmail :: M.Map T.Text a -> Either KeyException a
readClientEmail ma =
  case M.lookup "GCP_CLIENT_EMAIL" ma of
    Nothing -> Left $ NoPKFound "Cannot find client email GCP_CLIENT_EMAIL"
    Just cem -> Right cem

readServiceAcctSecret :: M.Map T.Text a -> Either KeyException a
readServiceAcctSecret ma =
  case M.lookup "GCP_SERVICE_ACCOUNT_SECRET" ma of
    Nothing -> Left $ NoPKFound "Cannot find service account secret GCP_SERVICE_ACCOUNT_SECRET"
    Just cem -> Right cem  

parsePK :: T.Text -> Either KeyException PrivateKey
parsePK k =
  case parseRSAPrivateKey k of [] -> Left $ NoParsePK "Cannot parse RSA key"
                               (PrivKeyRSA ok:_) -> Right ok
                               _ -> Left $ NoRSAKey "Found key is not a RSA private key"

gcpKeysMap :: M.Map T.Text T.Text -> Either KeyException GCPKeys
gcpKeysMap ma = do
    pk <- (readPrivateKey ma >>= parsePK)
    cem <- readClientEmail ma
    sas <- readServiceAcctSecret ma
    pure $ GCPKeys cem pk sas

gcpKeys :: IO (Either KeyException GCPKeys)
gcpKeys = do
  ma <- parseSecrets
  let ks = gcpKeysMap ma
  return ks


-- gcpPrivateKeyRSA :: IO (Either String PrivateKey)
-- gcpPrivateKeyRSA = do 
--   ma <- parseSecrets
--   case M.lookup "GCP_PRIVATE_KEY" ma of
--     Just k ->  
--       case parseRSAPrivateKey k of [] -> return $ Left "Cannot parse RSA key"
--                                    (PrivKeyRSA ok:_) -> return $ Right ok
--                                    _ -> return $ Left "Found key is not a RSA private key"
--     Nothing -> return $ Left "No RSA key called GCP_PRIVATE_KEY found"

parseRSAPrivateKey :: T.Text -> [PrivKey]
parseRSAPrivateKey = readKeyFileFromMemory . withPEMheaders encodeUtf8  where
  withPEMheaders encf k = b1 <> encf k <> b2 where
      b1 = encf $ T.pack "-----BEGIN RSA PRIVATE KEY-----\n"
      b2 = encf $ T.pack "\n-----END RSA PRIVATE KEY-----\n"

  



parseSecrets :: IO (M.Map T.Text T.Text)
parseSecrets = do
  secs <- T.readFile fname
  case A.parseOnly (A.sepBy kvPair A.endOfLine) secs of
    Left e -> error e
    Right x -> return $ M.fromList x  
  
validName :: A.Parser T.Text
validName = A.takeWhile1 (\c ->
                            isAlphaNum c ||
                            c == '_')

kvPair :: A.Parser (T.Text, T.Text)
kvPair = do
  k <- validName
  _ <- "="
  v <- A.takeTill A.isEndOfLine 
  return (k, v)





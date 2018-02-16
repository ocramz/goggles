{-# language OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module Network.Goggles.Auth.GCP.JWT where

import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString.Char8            as B8
import qualified Data.Text                  as T


import qualified Data.Aeson as J
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as J (Pair)

import           Data.Monoid ((<>))

import           Data.UnixTime (getUnixTime, utSeconds, UnixTime(..))
import           Foreign.C.Types

import Control.Monad.Except
import Control.Monad.Trans (liftIO)
import Control.Monad.Catch

import qualified Crypto.Hash.Algorithms as CR
import qualified Crypto.PubKey.RSA.PKCS15 as CR (signSafer) 
import qualified Crypto.Random.Types as CR


import Data.ByteArray
import Data.ByteArray.Encoding

import Network.Goggles.Types
import Network.Goggles.Control.Exceptions



-- | Returns a bytestring with the signed JWT-encoded token request. 
-- adapted from https://github.com/brendanhay/gogol/blob/master/gogol/src/Network/Google/Auth/ServiceAccount.hs
encodeBearerJWT :: (MonadThrow m, CR.MonadRandom m, MonadIO m) =>
                   GCPServiceAccount
                -> GCPTokenOptions
                -> m B8.ByteString
encodeBearerJWT s opts = do
    i <- input <$> liftIO getUnixTime
    r <- CR.signSafer (Just CR.SHA256) (_servicePrivateKey s) i
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

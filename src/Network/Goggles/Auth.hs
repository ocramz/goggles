{-# language OverloadedStrings #-}
module Network.Goggles.Auth where

import Network.HTTP.Types
import Network.HTTP.Types.URI (urlEncode, urlDecode)
import Network.HTTP.Types.Header
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Binary.Builder

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), encode, decode)

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Concurrent.STM

import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Time

import qualified Data.Map as M
import Data.Typeable

import Data.Keys (gcpUID, gcpPrivateKey, gcpServiceAccountSecret)



data Token = Token
    { tokenExpiresAt :: !UTCTime
    , tokenValue     :: !T.Text
    } deriving (Show)

-- data Handle = Handle
--     { hManager :: !Manager
--       -- ^ Shared HTTP manager.
--     , hToken :: !(TVar (Maybe Token))
--       -- ^ Cache for the access token. Use 'accessToken' when within the 'Cloud'
--       -- monad to access the token. That function will automatically refresh it
--       -- when it is about to expire.
--     , hFetchToken :: !(Cloud Token)
--       -- ^ The action which is used to fetch a fresh access token.
--     }

-- newtype Cloud a = Cloud { runCloud :: ReaderT Handle }
                


authScopes :: [String]
authScopes = [ "https://www.googleapis.com/auth/cloud-platform" ]

baseUrl :: BS.ByteString
baseUrl = "https://accounts.google.com/o/oauth2/v2/auth"

mkAuthHeader :: String -> Header
mkAuthHeader tok = (hAuthorization, BS.pack $ unwords ["Bearer",  tok])

mkContentTypeHeader :: String -> Header
mkContentTypeHeader ct = (hContentType, BS.pack ct)




get :: MonadThrow m => String -> RequestHeaders -> BS.ByteString -> m Request
get uri heads body = do
  reqBase <- parseUrlThrow uri
  return $ reqBase {
      method = renderStdMethod GET
    , requestHeaders = heads
    , requestBody = RequestBodyBS body
                   }

post :: MonadThrow m => String -> RequestHeaders -> BS.ByteString -> m Request
post uri heads body = do
  reqBase <- parseUrlThrow uri
  return $ reqBase {
      method = renderStdMethod POST
    , requestHeaders = heads
    , requestBody = RequestBodyBS body
                   }











  


withGCPCredentials :: (T.Text -> T.Text -> IO b) -> IO b
withGCPCredentials f = do
  uid <- gcpUID 
  secret <- gcpPrivateKey
  case (uid, secret) of (Just u, Just s) -> f u s
                        (Nothing, Just _) -> throwM $ GCPAuthException "GCP private key not found"
                        (Just _, Nothing) ->  throwM $ GCPAuthException "GCP user id not found"
                        (Nothing, Nothing) -> throwM $ GCPAuthException "GCP credentials not found"                        

data GCPException = GCPAuthException String deriving (Show, Typeable)
instance Exception GCPException






main :: IO ()
main = do
    -- let settings = managerSetProxy
    --         (proxyEnvironment Nothing)
    --         defaultManagerSettings
    man <- newManager defaultManagerSettings-- settings
    req <- parseRequest "http://httpbin.org/get"
    -- let req = "http://httpbin.org/get"
    --         -- Note that the following settings will be completely ignored.
    --         {
    --           proxy = Nothing -- Just $ Proxy "localhost" 1234
    --         }
    -- httpLbs req man >>= print
    respBS <- httpLbs req man
    print $ responseBody respBS -- (decode $ responseBody respBS :: Maybe BS.ByteString)

{-# language OverloadedStrings, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Network.Goggles.Auth where

import Data.Monoid

import Network.HTTP.Types
import Network.HTTP.Types.URI (urlEncode, urlDecode)
import Network.HTTP.Types.Header
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Binary.Builder

import Data.Aeson -- (FromJSON(..), ToJSON(..), Object(..), object, (.=), encode, decode)

import Control.Exception

import Control.Monad.Reader
import Control.Monad.Catch hiding (catch)
import Control.Monad.Except
import Control.Exception
import Control.Concurrent.STM

import Control.Retry

-- import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Time
import Data.Scientific (toBoundedInteger)

import qualified Data.HashMap.Strict as M
import Data.Typeable

import Data.Keys (gcpUID, gcpPrivateKey, gcpServiceAccountSecret)

import Network.Goggles.Cloud






-- | apply a given retry policy a finite number of times
retryBounded :: Monad m => RetryPolicyM m -> Int -> RetryPolicyM m
retryBounded policy n = policy <> limitRetries n


-- evalCloudRecovering policy = recovering policy 










-- | authentication scopes, raw HTTP connections

authScopes :: [String]
authScopes = [ "https://www.googleapis.com/auth/cloud-platform" ]

baseUrl :: BS.ByteString
baseUrl = "https://accounts.google.com/o/oauth2/v2/auth"

mkAuthHeader :: String -> Header
mkAuthHeader tok = (hAuthorization, BS.pack $ unwords ["Bearer",  tok])

mkContentTypeHeader :: String -> Header
mkContentTypeHeader ct = (hContentType, BS.pack ct)



runRequest :: Request -> Cloud BS.ByteString
runRequest req = do
    manager <- asks hManager
    cloudIO $ do
        res <- httpLbs req manager
        return $ LBS.toStrict $ responseBody res



get :: String -> RequestHeaders -> BS.ByteString -> Cloud BS.ByteString
get uri heads body = do
  req <- cloudIO $ do
    reqBase <- parseUrlThrow uri
    return $ reqBase {
      method = renderStdMethod GET
    , requestHeaders = heads
    , requestBody = RequestBodyBS body
                   }
  runRequest req

post :: String -> RequestHeaders -> BS.ByteString -> Cloud BS.ByteString
post uri heads body = do
  req <- cloudIO $ do
    reqBase <- parseUrlThrow uri
    return $ reqBase {
      method = renderStdMethod POST
    , requestHeaders = heads
    , requestBody = RequestBodyBS body
                   }
  runRequest req


getJSON :: FromJSON a => String -> RequestHeaders -> Cloud a
getJSON url headers = do
    body <- get url headers ""
    case eitherDecodeStrict body of
        Left e -> throwError $ JSONDecodeError e
        Right r -> return r









-- | Create a new 'Handle' with sensible defaults. The defaults are such that
-- the 'Handle' works out of the box when the application is running on an
-- instance in the Google cloud.
createHandle :: IO Handle
createHandle = do
    manager <- newManager tlsManagerSettings
    mkHandle manager defaultMetadataToken


-- | Create a new 'Handle' with your own configuration options.
mkHandle :: Manager -> Cloud Token -> IO Handle
mkHandle manager fetchToken = do
    token <- newTVarIO Nothing
    return $ Handle manager token fetchToken



-- | Fetch the access token for the default service account from the local
-- metadata server. This only works when the code is running in the Google
-- cloud and the instance has a services account attached to it.
defaultMetadataToken :: Cloud Token
defaultMetadataToken = serviceAccountToken "default"


metadataUri :: String
metadataUri = "http://metadata.google.internal"

projectMetadataUri :: String
projectMetadataUri = "/computeMetadata/v1/project"

instanceMetadataUri :: String
instanceMetadataUri = "/computeMetadata/v1/instance"



-- | Like 'getJSON' but for reading from the metadata server.
readJSON :: (FromJSON a) => String -> Cloud a
readJSON key = getJSON (metadataUri ++ key) [("Metadata-Flavor","Google")]



-- | Fetch an access token for the given service account.
serviceAccountToken :: String -> Cloud Token
serviceAccountToken acc = do
    res <- readJSON (instanceMetadataUri ++ "/service-account/" ++ acc ++ "/token")
    case res of
        (Object o) -> case (M.lookup "access_token" o, M.lookup "expires_in" o) of
            (Just (String value), Just (Number expiresIn)) -> do
                case toBoundedInteger expiresIn :: Maybe Int of
                    Nothing -> throwError $ UnknownError "fetchToken: Bad expiration time"
                    Just i -> do
                        now <- cloudIO $ getCurrentTime
                        return $ Token (addUTCTime (fromIntegral i) now) value
            _ -> throwError $ UnknownError "fetchToken: Could not decode response"
        _ -> throwError $ UnknownError "fetchToken: Bad resposnse"






  


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

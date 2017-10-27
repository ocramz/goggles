{-# language OverloadedStrings, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Network.Goggles.Auth where

import Data.Monoid

import Network.HTTP.Types
import Network.HTTP.Types.URI (urlEncode, urlDecode)
import Network.HTTP.Types.Header
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Binary.Builder

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), encode, decode)

import Control.Applicative (Alternative(..), empty, (<|>))
import Control.Monad.Reader
import Control.Monad.Catch hiding (catch)
import Control.Monad.Except
import Control.Exception
import Control.Concurrent.STM

import Control.Retry

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

data Handle = Handle
    { hManager :: !Manager
      -- ^ Shared HTTP manager.
    , hToken :: !(TVar (Maybe Token))
      -- ^ Cache for the access token. Use 'accessToken' when within the 'Cloud'
      -- monad to access the token. That function will automatically refresh it
      -- when it is about to expire.
    , hFetchToken :: !(Cloud Token)
      -- ^ The action which is used to fetch a fresh access token.
    }

data Error
    = UnknownError !T.Text
    | IOError !String
    | DecodeError !String
    | TemporaryIOError !String
    deriving (Eq, Show)
instance Exception Error where

newtype Cloud a = Cloud
    { runCloud :: ReaderT Handle (ExceptT Error IO) a
    } deriving (Functor, Applicative, Monad, MonadIO,
        MonadError Error, MonadReader Handle)

instance Alternative Cloud where
    empty = throwError $ UnknownError "empty"
    a <|> b = catchError a (const b)

-- | Evaluate a 'Cloud' action and return either the 'Error' or the result.
evalCloud :: Handle -> Cloud a -> IO (Either Error a)
evalCloud h m = (runExceptT $ runReaderT (runCloud m) h) `catch`
    (\e -> transformException (UnknownError . T.pack . show) e >>= return . Left)


-- | Transform an synchronous exception into an 'Error'. Async exceptions
-- are left untouched and propagated into the 'IO' monad.
transformException :: (SomeException -> Error) -> SomeException -> IO Error
transformException f e = case fromException e of
    Just async -> throwIO (async :: AsyncException)
    Nothing    -> return $ f e


-- | Run an 'IO' action inside the 'Cloud' monad, catch all synchronous
-- exceptions and transform them into 'Error's.
cloudIO :: IO a -> Cloud a
cloudIO m = do
    res <- liftIO $ (Right <$> m) `catch`
        (\e -> transformException (IOError . show) e >>= return . Left)
    case res of
        Left e -> throwError e
        Right  r -> return r


-- | apply a given retry policy a finite number of times
retryBounded :: Monad m => RetryPolicyM m -> Int -> RetryPolicyM m
retryBounded policy n = policy <> limitRetries n








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

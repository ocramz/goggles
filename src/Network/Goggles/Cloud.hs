{-# language OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Network.Goggles.Cloud where


import Control.Applicative (Alternative(..), empty, (<|>))
import Control.Monad.Reader
import Control.Monad.Catch hiding (catch)
import Control.Monad.Except
import Control.Exception
import Control.Concurrent.STM

import Data.Time

import Network.HTTP.Client

import qualified Data.Text as T

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
    | JSONDecodeError !String
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

{-# language OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# language UndecidableInstances #-}
module Network.Goggles.Cloud (
    WebApiM(..)
  , evalWebApiIO
  , liftWebApiIO
  , HasCredentials(..)
  , Token(..)
  , accessToken
  , refreshToken
  , Handle(..)
  , createHandle
                             ) where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Control.Monad.Trans.Reader as RT (ask, local)
import Crypto.Random.Types (MonadRandom(..))
import Crypto.Random.Entropy (getEntropy)
import Control.Exception (AsyncException, fromException)
import Control.Concurrent.STM
import Data.Time

import Network.Goggles.Control.Exceptions


-- | This class 
class HasCredentials c where
  type Credentials c
  type Options c
  type TokenContent c 
  tokenFetch :: WebApiM c (Token c)

-- | An authentication 'Token' with an expiry date
data Token c = Token {
    tToken :: TokenContent c
  , tTime :: UTCTime
  }

-- | A 'Handle' contains all information necessary to communicating with a cloud API provider:
--
-- * Authentication credentials (e.g. username/password)
-- * Authentication token (used to authenticate every API call)
-- * Options (e.g. GCP authentication scopes)
data Handle c = Handle {
      credentials :: Credentials c
    , token :: TVar (Maybe (Token c))
    , options :: Options c
  }



-- | The main type of the library. It can easily be re-used in libraries that interface with more than one cloud API provider because its type parameter `c` lets us be declare distinct behaviours for each.
newtype WebApiM c a = WebApiM {
  runWebApiM :: ReaderT (Handle c) IO a
  } deriving (Functor, Applicative, Monad)


instance HasCredentials c => Alternative (WebApiM c) where
    empty = throwM $ UnknownError "empty"
    a1 <|> a2 = do
      ra <- try a1
      case ra of
        Right x -> pure x
        Left e -> case (fromException e) :: Maybe CloudException of
          Just _ -> a2
          Nothing -> throwM (UnknownError "Uncaught exception (not a CloudException)")


-- -- | NB : this works similarly to <|> in the Alternative instance; it must discard information on which exception case occurred
-- tryOrElse :: MonadCatch m => m b -> m b -> m b
-- tryOrElse a1 a2 = do
--   ra <- try a1
--   case ra of
--     Right x -> pure x
--     Left e -> case (e :: CloudException) of _ -> a2




-- | Lift an `IO a` action into the 'WebApiM' monad
liftWebApiIO_ :: IO a -> WebApiM c a
liftWebApiIO_ m = WebApiM $ ReaderT (const m)

-- | Lift an `IO a` action into the 'WebApiM' monad, and catch synchronous exceptions, while rethrowing the asynchronous ones to IO
liftWebApiIO :: HasCredentials c => IO a -> WebApiM c a
liftWebApiIO m = do
  liftWebApiIO_ m `catch` \e -> case fromException e of 
    Just asy -> throwM (asy :: AsyncException)
    Nothing -> throwM $ IOError (show e)




 
-- | Evaluate a 'WebApiM' action, given a 'Handle'.
--
-- NB : Assumes all exceptions are handled by `throwM`
evalWebApiIO :: Handle c -> WebApiM c a -> IO a
evalWebApiIO r (WebApiM b) = runReaderT b r `catch` \e -> case (e :: CloudException) of 
  ex -> throwM ex





instance HasCredentials c => MonadIO (WebApiM c) where
  liftIO = liftWebApiIO

instance HasCredentials c => MonadThrow (WebApiM c) where
  throwM = liftIO . throwM

instance HasCredentials c => MonadCatch (WebApiM c) where
  catch (WebApiM (ReaderT m)) c =
    WebApiM $ ReaderT $ \r -> m r `catch` \e -> runReaderT (runWebApiM $ c e) r
  
{- | the whole point of this parametrization is to have a distinct MonadHttp for each API provider/DSP

instance HasCredentials c => MonadHttp (Boo c) where
  handleHttpException = throwM
-}

instance HasCredentials c => MonadRandom (WebApiM c) where
  getRandomBytes = liftIO . getEntropy

instance HasCredentials c => MonadReader (Handle c) (WebApiM c) where
  ask = WebApiM RT.ask
  local f m = WebApiM $ RT.local f (runWebApiM m)




-- | `cacheToken tok hdl` : Overwrite the token TVar `tv` containing a token if `tok` carries a more recent timestamp.
cacheToken ::
  HasCredentials c => Token c -> WebApiM c (Token c)
cacheToken tok = do
  tv <- asks token
  liftWebApiIO $ atomically $ do
    current <- readTVar tv
    let new = case current of
          Nothing -> tok
          Just t -> if tTime t > tTime tok then t else tok
    writeTVar tv (Just new)
    return new

refreshToken :: HasCredentials c => WebApiM c (Token c)
refreshToken = tokenFetch >>= cacheToken



-- | Extract the token content (needed to authenticate subsequent requests). The token will be valid for at least 60 seconds
accessToken :: HasCredentials c => WebApiM c (TokenContent c)
accessToken = do
    tokenTVar <- asks token 
    mbToken <- liftWebApiIO $ atomically $ readTVar tokenTVar
    tToken <$> case mbToken of
        Nothing -> refreshToken 
        Just t -> do
            now <- liftWebApiIO $ getCurrentTime
            if now > addUTCTime (- 60) (tTime t)
                then refreshToken 
                else return t  
  
-- | Create a 'Handle' with an empty token
createHandle :: HasCredentials c => Credentials c -> Options c -> IO (Handle c) 
createHandle sa opts = Handle <$> pure sa <*> newTVarIO Nothing <*> pure opts

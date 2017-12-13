module Network.Goggles (
    GCP
  , scopes
  , Cloud(..)
  , evalCloudIO
  , liftCloudIO
  , HasCredentials(..)
  , Token(..)
  , accessToken
  , refreshToken
  , Handle(..)
  , createHandle  
  -- * Exceptions
  , KeyException(..)
  , JWTError(..)
  , TokenExchangeException(..)
  , CloudException(..)  
  ) where


import Network.Goggles.Control.Exceptions 
import Network.Goggles.Cloud 
import Network.Goggles.Auth.TokenExchange
-- import System.Environment (lookupEnv)








{-| 

-}
module Network.Goggles (
  -- * Google Cloud Storage
    getObject
  , listObjects
  , putObject
  -- ** GCP Authentication scopes
  , scopesDefault
  -- ** Running Cloud programs
  , createHandle    
  , evalCloudIO
  -- *** Executing IO actions within 'Cloud'
  , liftCloudIO
  -- * Types
  , GCP
  , GCPServiceAccount(..)
  , Cloud(..)
  -- ** Authentication
  , HasCredentials(..)
  , Token(..)
  -- , accessToken
  -- , refreshToken
  , Handle(..)
  -- * Private key 
  , parseRSAPrivateKey
  -- * Exceptions
  , KeyException(..)
  , JWTError(..)
  , TokenExchangeException(..)
  , CloudException(..)  
  ) where


import Network.Goggles.Control.Exceptions 
import Network.Goggles.Cloud
import Network.Goggles.Types
import Network.Goggles.Auth.TokenExchange
import Data.Keys 
-- import System.Environment (lookupEnv)








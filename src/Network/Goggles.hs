{-|

-}
module Network.Goggles (
  -- ** Running WebApiM programs
    createHandle    
  , evalWebApiIO
  -- *** Lifting IO programs into 'WebApiM'
  , liftWebApiIO
  -- * Types
  , WebApiM(..)
  -- ** Authentication
  , HasCredentials(..)
  , HasToken(..)
  , Token(..)
  , accessToken
  , refreshToken
  , Handle(..)
  -- * Private key 
  , parseRSAPrivateKey
  -- * OAuth2 related
  , OAuth2Token(..)
  -- * Exceptions
  , KeyException(..)
  , JWTError(..)
  , TokenExchangeException(..)
  , CloudException(..)
  -- * Utilities
  , putLbs, getLbs, urlEncode
  ) where


import Network.Goggles.Control.Exceptions 
import Network.Goggles.Cloud
import Network.Goggles.Auth
-- import Network.Goggles.Types
import Network.Utils.HTTP
import Data.Keys 









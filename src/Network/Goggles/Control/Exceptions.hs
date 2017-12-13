module Network.Goggles.Control.Exceptions (
    KeyException(..)
  , JWTError(..)
  , TokenExchangeException(..)
  , CloudException(..)
  )where

import Control.Exception
import Data.Typeable

-- | Authentication key exceptions
data KeyException =
    NoSecretFound !String
  | NoParsePK !String
  | NoRSAKey !String
  deriving (Eq, Show, Typeable)
instance Exception KeyException

-- | Errors associated with JWT-encoded token request
data JWTError =
    BadExpirationTime !String
  | CryptoSignError !String
  deriving (Show, Typeable)
instance Exception JWTError where


-- | Token exchange exceptions
data TokenExchangeException =
    NotFound !String               -- ^ Something went wrong with the request, token not found
  | APICredentialsNotFound !String -- ^ 
  deriving (Show, Typeable)
instance Exception TokenExchangeException

-- | Cloud API exception
data CloudException =
    UnknownError !String
  | IOError !String
  | TimeoutError !String
  | JsonDecodeError !String
  deriving (Show, Typeable)
instance Exception CloudException



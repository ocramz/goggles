{-|
Module      : Network.Goggles
Description : Main module
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX

== Introduction

This library aims to abstract away (part of) the bookkeeping related to exchanging data with web APIs.

In particular, `goggles` can take care of automatically refreshing a token that has a finite lifetime such that the program never uses an invalid token. The token is furthermore cached such that network usage is reduced to a minimum.



== Usage

@
import Network.Goggles
@

The user might also need to turn on the `OverloadedStrings` language extension.

To begin with, the user provides a type for the remote service she wishes to interface to, along with a couple typeclass instances.

@
data Remote

newtype C = C { apiKey :: 'Text' } deriving Eq   -- API authentication credentials
@

Notice we don't actually need any data constructor associated with the 'Remote' type. In the simplest case it can be a "phantom type", only used as a label for typeclass instances.

This library design allows to be general as possible (many instances are polymorphic in this label, so the user doesn't need to write them), and specific where needed (as we will see with the exception handling mechanism further below.

There are so far two main use cases for `goggles`, corresponding to the complexity of the remote API authentication mechanism.

=== 1. Simple authentication

If calling the remote API only requires a key of some sort (i.e. does not involve a session token), the 'Remote' type should only be extended with a 'HasCredentials' interface:

@
instance HasCredentials Remote where
  type Credentials Remote = C
@


=== 2. Token-based authentication

If the API requires a token as well (this is the case with OAuth2-based implementations), the user must extend the 'HasToken' typeclass as well, by providing two associated types and a method implementation :

@
instance HasToken Remote where
  type Options Remote = ...                  -- any parameters that should be passed to the API call
  type TokenContent Remote = Text            -- must match the type of 'apiKey' defined above
  tokenFetch = ...                           -- create and retrieve the token from the remote API
@

Once this is in place, a valid token can always be retrieved with 'accessToken'. This checks the validity of the locally cached token and performs a 'tokenFetch' only when this is about to expire.

=== Exception handling

Internally, `goggles` uses `req` for HTTP connections, so the user must always provide a 'MonadHttp' instance for her 'Remote' type :

@
instance MonadHttp Remote where
  handleHttpExcepions = ...
@

The actual implementation of `handleHttpExceptions` depends on the actual API semantics, but the user can just use 'throwM' here (imported from the 'Control.Monad.Catch' module of the `exceptions` package).


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









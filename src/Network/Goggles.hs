{-| This module is the entry point to the @goggles@ library, which is a Haskell interface to the cloud services hosted by Google (e.g. storage, compute, mail, etc.: <https://cloud.google.com/>) .

Most Google Cloud Platform (GCP) functionality requires authentication, which must be obtained beforehand from the website either with a free trial or a paid plan.

From now on, we'll assume the user has such credentials and is able to load them alongside this library.

The examples require the following declarations (which in turn mean that the @req@ and @bytestring@ libraries are imported by the user's project):

> {-# language OverloadedStrings #-}
>
> import qualified Data.ByteString.Lazy as LB
> import Network.HTTP.Req (responseBody)
> import Network.Goggles

== /Examples/

This first example, @listBucket@, reads content from a cloud storage bucket:

1. it loads the GCP credentials (username and RSA key),
2. retrieves a token via OAuth2,
3. performs a single call to the Cloud Storage API endpoint that lists the metadata related to the contents of a storage bucket, and
4. returns the raw API data to the user as a lazy ByteString.

> listBucket :: IO LB.ByteString
> listBucket = do
>   let usr = "...iam.gserviceaccount.com"
>       bucket = "<my-gcs-bucket>"
>       key = "<rsa_key>"
>   pvtkey <- parseRSAPrivateKey key
>   let creds = GCPServiceAccount pvtkey usr Nothing ""
>   hdl <- createHandle creds scopesDefault
>   responseBody <$> evalCloudIO hdl (listObjects bucket)


-}
module Network.Goggles (
  -- * API endpoints
  -- ** Google Cloud Storage
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
  , GCPTokenOptions(..)
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








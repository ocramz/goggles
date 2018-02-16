{-|
== /Dependencies/

The examples require the following declarations (which in turn mean that the @req@ and @bytestring@ libraries are imported by the user's project). You will also need the @OverloadedStrings@ language extension :

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
>   responseBody <$> evalWebApiIO hdl (listObjects bucket)


-}
module Network.Goggles (
  -- ** Running WebApiM programs
    createHandle    
  , evalWebApiIO
  -- *** "Lifting" IO programs into 'WebApiM'
  , liftWebApiIO
  -- * Types
  -- , GCP
  -- , GCPServiceAccount(..)
  -- , GCPTokenOptions(..)
  , WebApiM(..)
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
-- import Network.Goggles.Auth.TokenExchange
import Data.Keys 
-- import System.Environment (lookupEnv)








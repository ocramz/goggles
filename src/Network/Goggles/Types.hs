{-# language TypeFamilies, FlexibleInstances #-}
module Network.Goggles.Types (
    GCPServiceAccount(..)
  , GCPTokenOptions(..) ) where

import qualified Data.Text as T
import Crypto.PubKey.RSA.Types




-- | Credentials for Google Cloud Platform
data GCPServiceAccount = GCPServiceAccount {
    _servicePrivateKey :: PrivateKey  -- ^ Private key (i.e. the PEM string obtained from the Google API Console)
  , _serviceEmail :: T.Text    -- ^ Email address of the service account (ending in .gserviceaccount.com )
  , _serviceAccountUser :: Maybe T.Text -- ^ email address of the user for which the application is requesting delegated access (defaults to the service account email if Nothing)
  , _gcpServiceAcctSecret :: T.Text -- ^ Service account secret
                                     } deriving (Eq, Show)



data GCPTokenOptions = GCPTokenOptions {
    _tokenOptionsScopes :: [T.Text] -- ^ authentication scopes that the application requests
                                 } deriving (Eq, Show)

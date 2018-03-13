module Network.Goggles.Auth.GCP
  (
  GCP(..),
  GCPServiceAccount(..),
  GCPTokenOptions(..)  ,
  scopesDefault,
  getObject,
  getObjectMetadata,
  listObjects,
  putObject,
  requestTokenGCP
  )
where

import Network.Goggles.Auth.GCP.TokenExchange 

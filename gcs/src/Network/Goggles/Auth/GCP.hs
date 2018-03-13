module Network.Goggles.Auth.GCP
  (
  GCP(..), scopesDefault,
  getObject,
  getObjectMetadata,
  listObjects,
  putObject,
  requestTokenGCP
  )
where

import Network.Goggles.Auth.GCP.TokenExchange 

module Network.Goggles.Auth.GCP
  (
  GCP(..), scopesDefault,
  getObject,
  getObjectMetadata,
  listObjects,
  putObject
  )
where

import Network.Goggles.Auth.GCP.TokenExchange 

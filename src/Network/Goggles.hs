{-# language DeriveGeneric #-}
module Network.Goggles where


import System.Envy
import GHC.Generics




data Token = Token { tokenStr :: String } deriving (Show, Generic)

instance DefConfig Token where
  defConfig = Token ""

instance FromEnv Token where

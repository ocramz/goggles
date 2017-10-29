{-# language OverloadedStrings, DeriveGeneric #-}
module Network.Goggles.Experimental where

import Control.Exception (throwIO)

import Control.Monad.Reader

import Data.Time

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.ByteString.Char8 as BS

import Data.Aeson
import Lens.Micro
import Lens.Micro.Aeson
import GHC.Generics

import Data.Monoid ((<>))

import Network.HTTP.Req


instance MonadHttp IO where
  handleHttpException = throwIO


main :: IO ()
main = do
  let params =
        "foo" =: ("bar" :: T.Text) <>
        queryFlag "baz"
  response <- req POST (https "httpbin.org" /: "post") (ReqBodyUrlEnc params) jsonResponse mempty
  print (responseBody response :: Value)



data Resp1 = Resp1 { r1origin :: String
                -- , r1data :: String
                } deriving (Show, Generic)
instance ToJSON Resp1 where
instance FromJSON Resp1 where
  parseJSON = withObject "R1" $ \v -> Resp1 <$> v .: "origin" --  <*> v .: "Accept-Encoding"
  

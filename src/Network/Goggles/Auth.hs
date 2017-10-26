{-# language OverloadedStrings #-}
module Network.Goggles.Auth where

import Network.HTTP.Client
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), encode, decode)

import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS






main :: IO ()
main = do
    -- let settings = managerSetProxy
    --         (proxyEnvironment Nothing)
    --         defaultManagerSettings
    man <- newManager defaultManagerSettings-- settings
    let req = "http://httpbin.org/get"
            -- Note that the following settings will be completely ignored.
            { proxy = Nothing -- Just $ Proxy "localhost" 1234
            }
    -- httpLbs req man >>= print
    respBS <- httpLbs req man
    print $ responseBody respBS -- (decode $ responseBody respBS :: Maybe BS.ByteString)

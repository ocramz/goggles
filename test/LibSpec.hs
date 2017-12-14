{-# language OverloadedStrings #-}
module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Req (responseBody)
import Network.Goggles



main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x


t1 = do
  let usr = "...iam.gserviceaccount.com"
      bucket = "<my-gcs-bucket>"
  pvtkey <- parseRSAPrivateKey "<key>"
  let creds = GCPServiceAccount pvtkey usr Nothing ""
  hdl <- createHandle creds scopesDefault
  responseBody <$> evalCloudIO hdl (listObjects bucket)
  
  

{-# language OverloadedStrings, DeriveGeneric #-}
module Network.Goggles.Auth.OAuth2
  (
    requestOAuth2Token
  , OAuth2Token(..)
  , OAuth2TokenUTC(..)
  , mkOAuth2TokenUTC
                                )
  where


import Network.HTTP.Req
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import GHC.Generics
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T (encodeUtf8)
-- import qualified Data.ByteString.Char8            as B8
-- import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson as J
import Data.Aeson ((.:), (.:?))
import Network.Utils.HTTP
import Data.Time

import Network.Goggles.Control.Exceptions



data OAuth2TokenUTC = OAuth2TokenUTC {
    oauTokenExpiry :: UTCTime
  , oauTokenString :: T.Text
  , oauTokenType :: Maybe T.Text
                                     } deriving (Eq, Show)

mkOAuth2TokenUTC :: (MonadIO m, Integral t) => t -> OAuth2Token -> m OAuth2TokenUTC
mkOAuth2TokenUTC delta oa2 = liftIO $
  OAuth2TokenUTC <$>
  tokenExpiryTime delta (oaTokenExpirySeconds oa2) <*>
  pure (oaTokenString oa2) <*>
  pure (oaTokenType oa2)


data OAuth2Token = OAuth2Token {
    oaTokenExpirySeconds :: Int
  , oaTokenString :: T.Text
  , oaTokenType :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance J.FromJSON OAuth2Token where
  parseJSON = J.withObject "OAuth2Token" $ \js -> OAuth2Token
    <$> js .: "expires_in"
    <*> js .: "access_token"
    <*> js .:? "token_type"


-- | send a POST request over HTTPS to a given URI that will return a OAuth2Token
requestOAuth2Token
  :: (MonadHttp m, MonadThrow m) =>
     Url scheme         -- ^ Request URI
  -> [(T.Text, T.Text)] -- ^ parameter list as a list of (key, value) pairs
  -> Option scheme      -- ^ request options (e.g. headers)     
  -> m OAuth2Token
requestOAuth2Token uri args httpOpts = do
  let payload = encodeHttpParametersLB args
  r <- req POST
         uri
         (ReqBodyLbs payload)
         lbsResponse
         httpOpts
  maybe (throwM $ NotFound "Something went wrong with the token request") pure (J.decode (responseBody r) :: Maybe OAuth2Token)




-- | Returns the UTCTime (absolute) related to a delay in seconds from the time this function is executed. We need this helper function because the "expires_in" field in the OAuth2 response means "seconds from now".
tokenExpiryTime ::
  (Integral t, Integral t1) =>
     t          -- ^ Correction for system delays (e.g. processing and network time). Positive
  -> t1         -- ^ "seconds from now" parameter. Positive 
  -> IO UTCTime 
tokenExpiryTime delta s = do
  tnow <- getCurrentTime
  let sd = fromIntegral s
      sdelta = fromIntegral delta
  return $ addUTCTime (sd - sdelta) tnow

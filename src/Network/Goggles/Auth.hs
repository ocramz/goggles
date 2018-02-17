{-# language OverloadedStrings, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# language RankNTypes #-}
module Network.Goggles.Auth
  (
    requestOAuth2Token
  , OAuth2Token(..)
  , OAuth2TokenUTC(..)
  , mkOAuth2TokenUTC
                                )
where

import Network.Goggles.Auth.OAuth2


{-# language OverloadedStrings, FlexibleContexts #-}
module Network.Utils.HTTP where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.ByteString.Lazy as LB
import Data.Char ( isAscii, isAlphaNum, digitToInt )
import Network.HTTP.Req

import Network.Goggles.Cloud

-- | Create an authenticated 'GET' call
getLbsWithToken :: (HasCredentials c, HasToken c, MonadHttp (WebApiM c)) =>
                   (TokenContent c -> Url scheme -> Option scheme -> (Url scheme, Option scheme))
                -> Url scheme
                -> Option scheme
                -> WebApiM c LbsResponse
getLbsWithToken fOpts uri opts = do
  tok <- accessToken
  let (uri', opts') = fOpts tok uri opts
  getLbs uri' opts'

-- | Sending data with an authenticated call
--
-- The first function argument may be either 'postLbs' or 'putLbs'
sendWithToken :: HasToken c =>
                 (Url scheme -> Option scheme -> LB.ByteString -> WebApiM c b)
              -> (TokenContent c -> Url scheme -> Option scheme -> LB.ByteString -> (Url scheme, Option scheme, LB.ByteString))
              -> Url scheme
              -> Option scheme
              -> LB.ByteString
              -> WebApiM c b
sendWithToken f fOpts uri opts body = do   
  tok <- accessToken
  let (uri', opts', body') = fOpts tok uri opts body
  f uri' opts' body'

-- | 'GET' a lazy bytestring from an API endpoint
getLbs :: (HasCredentials c, MonadHttp (WebApiM c)) =>
               Url scheme -> Option scheme -> WebApiM c LbsResponse
getLbs uri opts = req GET uri NoReqBody lbsResponse opts 

-- | 'POST' a request to an API endpoint and receive a lazy bytestring in return
postLbs :: (HasCredentials c, MonadHttp (WebApiM c)) =>
               Url scheme -> Option scheme -> LB.ByteString -> WebApiM c LbsResponse
postLbs uri opts body = req POST uri (ReqBodyLbs body) lbsResponse opts

-- | 'PUT' a request to an API endpoint and receive a lazy bytestring in return
putLbs :: (HasCredentials c, MonadHttp (WebApiM c)) =>
               Url scheme -> Option scheme -> LB.ByteString -> WebApiM c LbsResponse
putLbs uri opts body = req PUT uri (ReqBodyLbs body) lbsResponse opts





-- | produce a '&'-separated list of parameters that can be passed to an HTTP querty from a list of key, value pairs 
encodeHttpParametersLB :: [(T.Text, T.Text)] -> LB.ByteString
encodeHttpParametersLB ps = LB.fromStrict $ T.encodeUtf8 $ T.intercalate "&" $ map ins ps where
  ins (k, v) = T.concat [k, "=", v]

encodeHttpParameters :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
encodeHttpParameters ll = mconcat $ map ins ll where
  ins (a, b) = a =: b



-- http://hackage.haskell.org/package/HTTP-4000.2.3/docs/src/Network-HTTP-Base.html

urlEncode :: String -> String
urlEncode     [] = []
urlEncode (ch:t) 
  | (isAscii ch && isAlphaNum ch) || ch `elem` ("-_.~" :: String) = ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (eightBs [] (fromEnum ch))
  | otherwise = escape (fromEnum ch) (urlEncode t)
    where
     escape b rs = '%':showH (b `div` 16) (showH (b `mod` 16) rs)
     
     showH x xs
       | x <= 9    = toEnum (o_0 + x) : xs
       | otherwise = toEnum (o_A + (x-10)) : xs
      where
       o_0 = fromEnum '0'
       o_A = fromEnum 'A'

     eightBs :: [Int]  -> Int -> [Int]
     eightBs acc x
      | x <= 0xff = (x:acc)
      | otherwise = eightBs ((x `mod` 256) : acc) (x `div` 256)


urlDecode :: String -> String
urlDecode ('%':a:b:rest) = toEnum (16 * digitToInt a + digitToInt b)
                         : urlDecode rest
urlDecode (h:t) = h : urlDecode t
urlDecode [] = []

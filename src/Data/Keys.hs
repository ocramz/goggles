{-# language OverloadedStrings #-}
module Data.Keys (parseSecrets) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A

import qualified Data.Map as M

import Data.Char

{- |
Parse a text file containing keys/environment variables of the form <KEY>=<VALUE>.
Obviously this file shouldn't be in source control (e.g. should be mentioned in .gitignore )
-}

fname :: String
fname = "secrets"

parseSecrets :: IO (M.Map T.Text T.Text)
parseSecrets = do
  secs <- T.readFile fname
  case A.parseOnly (A.sepBy kvPair A.endOfLine) secs of
    Left e -> error e
    Right x -> return $ M.fromList x

validName :: A.Parser T.Text
validName = A.takeWhile1 (\c ->
                            isAlphaNum c ||
                            c == '_')

quotedString :: A.Parser T.Text
quotedString = do
  _ <- "\""
  x <- A.takeTill (\c -> c == '\"')
  _ <- "\""
  return x

kvPair :: A.Parser (T.Text, T.Text)
kvPair = do
  k <- validName
  _ <- "="
  v <- quotedString
  return (k, v)



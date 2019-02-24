{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Salesforce.Query where

import Network.HTTP.Conduit
import Network.URI
import Network.URI.Encode as URI
import System.IO
import Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson as JSON
import Data.Maybe
import Data.Proxy as DP
import Data.List as L
import Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Char8 as BL8
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Text.XML.HaXml.Xtract.Parse
import Text.Regex.Posix
import Network.Salesforce.Auth
import Network.Salesforce.Core

data QueryResult a = QueryResult{
  done :: Bool,
  totalSize :: Int,
  records :: [a]
} deriving Show

instance (FromJSON a) => FromJSON (QueryResult a) where
  parseJSON = withObject "QueryResult" $ \v -> do
    records <- v .: "records"
    totalSize <- v .: "totalSize"
    done <- v .: "done"
    return QueryResult{..}

query :: (FromJSON a) => LoginResult -> String -> DP.Proxy a -> IO (QueryResult a)
query lr query proxy = do
  let queryUrl = instanceUrl lr ++ "/services/data/v" ++ resApiVersion lr ++ "/query/?q=" ++ URI.encode query
  response <- sfGet lr queryUrl
  let body = responseBody response
  let result = JSON.decode body :: (FromJSON a) => Maybe (QueryResult a)
  return (fromJust result)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment (getArgs, getEnv)
import System.IO
import Network.HTTP.Conduit
import Network.URI
import Network.URI.Encode as URI
import Data.Maybe
import Data.List as L
import Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Char8 as BL8
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Text.XML.HaXml.Xtract.Parse
import Text.Regex.Posix
import Data.Aeson as JSON
import Control.Applicative
import GHC.Generics
import Control.Monad (mzero)

data LoginRequest = LoginRequest{
  username :: String,
  password :: String,
  endpoint :: String,
  apiVersion :: String
} deriving Show

data LoginResult = LoginResult{
  sessionId :: String,
  userId :: String,
  serverUrl :: String,
  metadataServerUrl  :: String,
  instanceUrl :: String,
  resApiVersion :: String
} deriving Show

data QueryResult = QueryResult{
  done :: Bool,
  totalSize :: Int,
  records :: [Account]
} deriving Show

--instance FromJSON QueryResult where
--  parseJSON (Object v) = QueryResult
--    <$> v .: "done"
--    <*> v .: "totalSize"
--    <*> v .: "records"
--  parseJSON _ = mzero
instance FromJSON QueryResult where
  parseJSON = withObject "QueryResult" $ \v -> do
    records <- v .: "records"
    totalSize <- v .: "totalSize"
    done <- v .: "done"
    return QueryResult{..}

data Account = Account{
  sfid :: String,
  name :: String
} deriving Show

instance FromJSON Account where
  parseJSON (Object v) = Account
    <$> (v .: "Id")
    <*> (v .: "Name")

main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "login" -> do
      username <- getEnv "SALESFORCE_USERNAME"
      password <- getEnv "SALESFORCE_PASSWORD"
      loginResult <- login LoginRequest{
        username=username,
        password=password,
        endpoint="login.salesforce.com",
        apiVersion="44.0"
      }
      System.IO.print loginResult
      let query = args !! 1
      queryResult <- Main.query loginResult query
      System.IO.print queryResult
      mapM (print . sfid) $ records queryResult
      return ()
    _ -> return ()

replace :: String -> String -> String -> String
replace x y src = inner src where
  inner [] = []
  inner str@(s:ss)
    | L.isPrefixOf x str = y ++ inner (L.drop (L.length x) str)
    | otherwise = s:inner ss

login :: LoginRequest -> IO LoginResult
login LoginRequest{ username, password, endpoint, apiVersion} = do
  let body = "<?xml version=\"1.0\" encoding=\"utf-8\"?> \
  \<env:Envelope xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\"> \
    \<env:Body> \
      \<n1:login xmlns:n1=\"urn:partner.soap.sforce.com\"> \
        \<n1:username>{username}</n1:username> \
       \<n1:password>{password}</n1:password> \
     \</n1:login> \
   \</env:Body> \
  \</env:Envelope>" :: String
  initReq <- parseRequest $ "https://" ++ endpoint ++ "/services/Soap/u/" ++ apiVersion
  manager <- newManager tlsManagerSettings
  let requestBody = L.foldl (\body (bind,value) -> replace bind (escapeXML value) body) body [("{username}", username), ("{password}", password)]
  let req = initReq { method = "POST",
    requestHeaders = [("Content-Type", "text/xml"), ("SOAPAction", "''")],
    requestBody = RequestBodyBS $ B8.pack requestBody
  }
  response <- httpLbs req manager
  let Document _ _ root _ = xmlParse "" $ (BL8.unpack (responseBody response))
      cont = CElem root noPos
      result = xtract id "/soapenv:Envelope/soapenv:Body/loginResponse/result" cont !! 0
      sessionId = tagTextContent $ xtract id "/result/sessionId" result !! 0
      userId = tagTextContent $ xtract id "/result/userId" result !! 0
      serverUrl = tagTextContent $ xtract id "/result/serverUrl" result !! 0
      matches = serverUrl =~ ("^(https://[^/]*)/.*" :: String) :: [[String]]
      metadataServerUrl = tagTextContent $ xtract id "/result/metadataServerUrl" result !! 0
  System.IO.print matches
  return LoginResult{sessionId, userId, serverUrl, metadataServerUrl, resApiVersion = apiVersion, instanceUrl = matches !! 0 !! 1}

query :: LoginResult -> String -> IO QueryResult
query lr query = do
  let queryUrl = instanceUrl lr ++ "/services/data/v" ++ resApiVersion lr ++ "/query/?q=" ++ URI.encode query
  System.IO.print queryUrl
  response <- get lr queryUrl
  let body = responseBody response
  let result = JSON.decode body :: Maybe QueryResult
  System.IO.print body
  System.IO.print result
  return (fromJust result)

get :: LoginResult -> String -> IO (Response BL8.ByteString)
get LoginResult{sessionId} url = do
  initReq <- parseRequest url
  manager <- newManager tlsManagerSettings
  let req = initReq { method = "GET",
    requestHeaders = [("Authorization", B8.pack $ "Bearer " ++ sessionId)]
  }
  httpLbs req manager

post :: LoginResult -> String -> String -> IO (Response BL8.ByteString)
post LoginResult{sessionId} url body = do
  initReq <- parseRequest url
  manager <- newManager tlsManagerSettings
  let req = initReq { method = "POST",
    requestHeaders = [("Authorization", B8.pack $ "Bearer " ++ sessionId)],
    requestBody = RequestBodyBS $ B8.pack body
  }
  httpLbs req manager


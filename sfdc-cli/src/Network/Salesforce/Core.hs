{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Salesforce.Core where

import Network.HTTP.Conduit
import Network.URI
import Network.URI.Encode as URI
import System.IO
import Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Char8 as BL8
import Data.List as L

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

replace :: String -> String -> String -> String
replace x y src = inner src where
  inner [] = []
  inner str@(s:ss)
    | L.isPrefixOf x str = y ++ inner (L.drop (L.length x) str)
    | otherwise = s:inner ss

sfGet :: LoginResult -> String -> IO (Response BL8.ByteString)
sfGet LoginResult{sessionId} url = do
  initReq <- parseRequest url
  manager <- newManager tlsManagerSettings
  let req = initReq { method = "GET",
    requestHeaders = [("Authorization", B8.pack $ "Bearer " ++ sessionId)]
  }
  httpLbs req manager

sfPost :: LoginResult -> String -> String -> IO (Response BL8.ByteString)
sfPost LoginResult{sessionId} url body = do
  initReq <- parseRequest url
  manager <- newManager tlsManagerSettings
  let req = initReq { method = "POST",
    requestHeaders = [("Authorization", B8.pack $ "Bearer " ++ sessionId)],
    requestBody = RequestBodyBS $ B8.pack body
  }
  httpLbs req manager


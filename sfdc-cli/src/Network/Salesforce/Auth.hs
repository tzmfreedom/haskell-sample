{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Salesforce.Auth where

import Network.HTTP.Conduit
import Network.URI
import Network.URI.Encode as URI
import System.IO
import Data.Aeson as JSON
import Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe
import Data.List as L
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Text.XML.HaXml.Xtract.Parse
import Text.Regex.Posix
import Network.Salesforce.Core

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
  return LoginResult{sessionId, userId, serverUrl, metadataServerUrl, resApiVersion = apiVersion, instanceUrl = matches !! 0 !! 1}

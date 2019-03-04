{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Environment (getArgs, getEnv)
import System.IO
import Data.Aeson as JSON
import Data.Proxy
import Data.Maybe
import HSForce

data Account = Account{
  accId :: Maybe String,
  accName :: Maybe String,
  accEx :: Maybe String
} deriving Show

instance SObject Account where
  typeName a = "Account"
  getSfid = fromJust . accId

instance FromJSON Account where
  parseJSON = withObject "Account" $ \v -> do
    accId <- v .:? "Id"
    accName <- v .:? "Name"
    accEx <- v .:? "Ex__c"
    return Account{..}

instance ToJSON Account where
  toJSON (Account{accName}) =
    object ["Name" .= accName]

main :: IO ()
main = do
  loginRequest <- defaultLoginRequest
  client <- login loginRequest
  print $ clientAccessToken client
  let client' = client { clientDebug = True}
--  insert client Account{name="hogehoge"}
--  update client Account{sfid="0016F00003AfgQHQAZ", name="foobar"}
--  upsert client' Account{sfid=Nothing, name = Just "foobar", ex = Just "aaa"} "Ex__c" "aaa"
--  print =<< query client' "SELECT Id, Name FROM Account WHERE Name = 'foobar'" (Proxy :: Proxy Account)
--  print =<< HSForce.describe client' "Account" (Proxy :: Proxy Account)
--  print =<< HSForce.describeGlobal client'
--  print =<< queryAll client' "SELECT Id, Name FROM Account WHERE Name = 'foobar'" (Proxy :: Proxy Account)
  print =<< versions client'
  print =<< recordCount client' ["Account", "Contact", "Opportunity"]

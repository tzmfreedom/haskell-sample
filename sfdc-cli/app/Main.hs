{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment (getArgs, getEnv)
import System.IO
import Network.Salesforce.Core
import Network.Salesforce.Auth as Auth
import Network.Salesforce.Query as Query
import Data.Aeson as JSON
import Data.Proxy

data Account = Account{
  sfid :: String,
  name :: String
} deriving Show

instance FromJSON Account where
  parseJSON (Object v) = Account
    <$> (v .: "Id")
    <*> (v .: "Name")

data Contact = Contact{
  sfid' :: String,
  lastname :: String,
  firstname :: String
} deriving Show

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ \v -> do
    sfid' <- (v .: "Id")
    lastname <- (v .: "LastName")
    firstname <- (v .: "FirstName")
    return Contact{..}

main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
    "login" -> do
      loginResult <- Main.login
      print loginResult
    "query" -> do
      loginResult <- Main.login
      let query = args !! 1
      queryResult <- Query.query loginResult query (Proxy :: Proxy Contact)
      mapM print $ records queryResult
      return ()
    _ -> return ()

login :: IO LoginResult
login = do
  username <- getEnv "SALESFORCE_USERNAME"
  password <- getEnv "SALESFORCE_PASSWORD"
  Auth.login LoginRequest{
    username=username,
    password=password,
    endpoint="login.salesforce.com",
    apiVersion="44.0"
  }


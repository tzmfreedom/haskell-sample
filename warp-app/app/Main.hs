{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString.Builder
import Data.Map as M
import Data.Maybe
import Data.IORef
import Data.List as L
import Data.List.Split
import Codec.Binary.UTF8.String as CBS
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Text.Parsec as TP
import Text.Parsec.String
import Text.Parsec.Char

type Handler = Request -> IORef DB -> IO Response

type DB = M.Map String Task

data Task = Task {
  title :: String,
  description :: String
} deriving Show

main :: IO ()
main = do
  tasks <- defaultTasks
  run 8080 (server tasks)

defaultTasks :: IO (IORef DB)
defaultTasks = newIORef $ fromList [("1", Task{title="foo", description="bar"}), ("2", Task{title="hoge", description="fuga"})]

server :: IORef DB -> Application
server tasks req respond = do
  let handler = getHandler req
  respond =<< handler tasks

getHandler :: Handler
getHandler req = case parse (parseRoute req) "" (CBS.decode $ BS.unpack path) of
  Left err -> error $ "ParserError"
  Right ls -> ls req
  where path = rawPathInfo req

notFound :: Request -> IORef a -> IO Response
notFound _ _ = return $ responseLBS status404 [] "Not Found"

root :: Request -> IORef a -> IO Response
root _ _ = return $ responseFile status200 [] "./index1.html" Nothing

--showHoi :: Request -> IORef a -> IO Response
--showHoi _ _ = return $ responseLBS status200 [] (toLazyByteString $ stringUtf8 "ほい！\n")

parseRoute :: Request -> Parser Handler
parseRoute req = (try $ parseTop req) TP.<|> (try $ parseTask req)

parseTop :: Request -> Parser Handler
parseTop req = do
  string "/"
  eof
  return (case requestMethod req of
    "GET" -> root
    _ -> notFound)

parseTask :: Request -> Parser Handler
parseTask req = do
  let method = requestMethod req
  string "/posts"
  eof *> return (case method of
    "GET" -> indexTask
    "POST" -> addTask
    _ -> notFound) TP.<|> do
    char '/'
    postId <- TP.many1 digit
    return (case method of
        "GET" -> showTask postId
        "PATCH" -> updateTask postId
        "DELETE" -> deleteTask postId
        _ -> notFound
      )

indexTask :: Handler
indexTask req ref = do
  db <- readIORef ref
  return (responseLBS status200 [] $ toLazyByteString $ stringUtf8 $ show "index")

showTask :: String -> Handler
showTask taskId req ref = do
  db <- readIORef ref
  return (responseLBS status200 [] $ toLazyByteString $ stringUtf8 $ show "show" ++ taskId)

addTask :: Request -> IORef DB -> IO Response
addTask req ref = do
--  let kv = (queryString req !! 0)
--      key = fst kv
--      value = snd kv
  db <- readIORef ref
  writeIORef ref db
  return (responseLBS status200 [] $ toLazyByteString $ stringUtf8 $ show "add")
--  return (responseLBS status200 [] $ fromStrict $ key `BS.append` "=" `BS.append` (fromJust value) `BS.append` "\n")

updateTask :: String -> Request -> IORef DB -> IO Response
updateTask taskId req ref = do
  db <- readIORef ref
  writeIORef ref db
  return (responseLBS status200 [] $ toLazyByteString $ stringUtf8 $ show "update")

deleteTask :: String -> Request -> IORef DB -> IO Response
deleteTask taskId req ref = do
  db <- readIORef ref
  writeIORef ref db
  return (responseLBS status200 [] $ toLazyByteString $ stringUtf8 $ show "delete")

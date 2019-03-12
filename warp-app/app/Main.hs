{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString.Builder
import Data.Char
import Data.Map.Strict as M
import Data.Maybe
import Data.IORef
import Data.List as L
import Data.List.Split
import Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Codec.Binary.UTF8.String as CBS
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Text.Parsec as TP
import Text.Parsec.String
import Text.Parsec.Char

type Handler = Request -> IORef DB -> IO Response

data DB = DB{
  dbRecords :: M.Map Int Task,
  dbNextTaskId :: Int
} deriving Show

data Task = Task {
  taskId :: Int,
  taskTitle:: String,
  taskDescription :: String
} deriving Show

main :: IO ()
main = do
  ref <- initDBRef
  run 8080 (server ref)

initDBRef :: IO (IORef DB)
initDBRef = newIORef $ DB{
  dbRecords = fromList [
    (1, Task{taskId = 1, taskTitle="foo", taskDescription="bar"}),
    (2, Task{taskId = 2, taskTitle="hoge", taskDescription="fuga"})
  ],
  dbNextTaskId = 3
  }

server :: IORef DB -> Application
server ref req respond = do
  let handler = getHandler req
  respond =<< handler ref

getHandler :: Handler
getHandler req = case parse (parseRoute req) "" (CBS.decode $ BS.unpack path) of
  Left err -> error $ "ParserError"
  Right ls -> ls req
  where path = rawPathInfo req

notFoundResponse :: Response
notFoundResponse = responseLBS status404 [] "Not Found"

notFound :: Request -> IORef a -> IO Response
notFound _ _ = return notFoundResponse

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
    let postId' = read postId :: Int
    return (case method of
        "GET" -> showTask postId'
        "PATCH" -> updateTask postId'
        "DELETE" -> deleteTask postId'
        _ -> notFound
      )

indexTask :: Handler
indexTask req ref = do
  db <- readIORef ref
  return (responseLBS status200 [] $ JSON.encode $ elems $ dbRecords db)

showTask :: Int -> Handler
showTask taskId req ref = do
  db <- readIORef ref
  let task = M.lookup taskId (dbRecords db)
  if isNothing task then return notFoundResponse else return (responseLBS status200 [] $ JSON.encode task)

addTask :: Request -> IORef DB -> IO Response
addTask req ref = do
  db <- readIORef ref
  let nid = dbNextTaskId db
  writeIORef ref $ dbAddTask req db
  let task = M.lookup nid (dbRecords db)
  return (responseLBS status200 [] $ JSON.encode task)
--  return (responseLBS status200 [] $ fromStrict $ key `BS.append` "=" `BS.append` (fromJust value) `BS.append` "\n")

updateTask :: Int -> Request -> IORef DB -> IO Response
updateTask taskId req ref = do
  db <- readIORef ref
  writeIORef ref $ dbUpdateTask taskId req db
  return (responseLBS status204 [] "")

deleteTask :: Int -> Request -> IORef DB -> IO Response
deleteTask taskId req ref = do
  db <- readIORef ref
  writeIORef ref $ dbDeleteTask taskId db
  return (responseLBS status204 [] "")

dbAddTask :: Request -> DB -> DB
dbAddTask req db = do
  DB{
    dbRecords = M.insert taskId newTask (dbRecords db),
    dbNextTaskId = taskId + 1
  } where
    reqTask = buildTask req
    taskId = dbNextTaskId db
    newTask = Task{
      taskId = taskId,
      taskTitle = taskTitle reqTask,
      taskDescription = taskDescription reqTask
    }

dbUpdateTask :: Int -> Request -> DB -> DB
dbUpdateTask taskId req db = do
  case M.lookup taskId (dbRecords db) of
    Nothing -> error "Nothing"
    Just task -> db{
      dbRecords = M.insert taskId (foldTask req task) (dbRecords db)
    }

dbDeleteTask :: Int -> DB -> DB
dbDeleteTask taskId db = do
  db{
    dbRecords = M.delete taskId $ dbRecords db
  }

buildTask :: Request -> Task
buildTask req = do
  Task{
    taskId = 0,
    taskTitle = decodeParameter $ fromJust title,
    taskDescription = decodeParameter $ fromJust desc
  } where
    q = queryString req
    title = L.lookup "title" q
    desc = L.lookup "desc" q


foldTask :: Request -> Task -> Task
foldTask req task = do
  task'' where
    q = queryString req
    title = L.lookup "title" q
    desc = L.lookup "desc" q
    task' = if isNothing title then task else task{taskTitle = decodeParameter $ fromJust title}
    task'' = if isNothing title then task' else task'{taskDescription = decodeParameter $ fromJust desc}

decodeParameter :: Maybe BS.ByteString -> String
decodeParameter bs = do
  ret bs where
    ret (Just a) = bs2str a
    ret Nothing = ""

bs2str :: BS.ByteString -> String
bs2str str = do
  CBS.decode $ BS.unpack str

deriveJSON defaultOptions { fieldLabelModifier = (\str -> toLower (L.head str) : (L.tail str)) . L.drop (L.length ("task" :: String)) } ''Task

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

main :: IO ()
main = do
  tasks <- defaultTasks
  run 8080 (server tasks)

defaultTasks :: IO (IORef [String])
defaultTasks = newIORef ["foo", "bar", "baz"]

server :: IORef [String] -> Application
server tasks req respond = do
  let path = rawPathInfo req
      paths = splitOn "/" (CBS.decode . BS.unpack $ BS.drop 1 path)
      handler = getHandler paths
  respond =<< handler req tasks

getHandler :: [String] -> Request -> IORef [String] -> IO Response
getHandler (path:rest) = case path of
       ""         -> showIndex
       "hello"    -> showHoi
       "add"      -> addTask
       "delete"   -> deleteTask
       _        -> notFound

notFound :: Request -> IORef a -> IO Response
notFound _ _ = return $ responseLBS status404 [] "Not Found"

showIndex :: Request -> IORef a -> IO Response
showIndex _ _ = return $ responseFile status200 [] "./index1.html" Nothing

showHoi :: Request -> IORef a -> IO Response
showHoi _ _ = return $ responseLBS status200 [] (toLazyByteString $ stringUtf8 "ほい！\n")

addTask :: Request -> IORef [String] -> IO Response
addTask req ref = do
  let kv = (queryString req !! 0)
      key = fst kv
      value = snd kv
  tasks <- readIORef ref
  let newTask = key `BS.append` "=" `BS.append` (fromJust value)
      newTasks = (CBS.decode $ BS.unpack newTask):tasks
  writeIORef ref newTasks
  return (responseLBS status200 [] $ toLazyByteString $ stringUtf8 $ show newTasks)
--  return (responseLBS status200 [] $ fromStrict $ key `BS.append` "=" `BS.append` (fromJust value) `BS.append` "\n")

deleteTask :: Request -> IORef [String] -> IO Response
deleteTask req ref = do
  tasks <- readIORef ref
  writeIORef ref (L.drop 1 tasks)
  tasks' <- readIORef ref
  let kv = (queryString req !! 0)
      key = fst kv
      value = snd kv
  return (responseLBS status200 [] $ toLazyByteString $ stringUtf8 $ show tasks')

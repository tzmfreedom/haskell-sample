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

main :: IO ()
main = run 8080 server

server :: Application
server req respond = do
  respond $ routes (rawPathInfo req) (requestMethod req) req

routes :: BS.ByteString -> Method -> (Request -> Response)
routes path method
       | path == "/" && method == "GET"       = showIndex
       | path == "/hello" && method == "POST" = showHoi
       | path == "/req" && method == "GET"   = showRequest
       | otherwise                          = notFound

notFound :: Request -> Response
notFound _ = responseLBS status404 [] "Not Found"

showIndex :: Request -> Response
showIndex _ = responseFile status200 [] "./index1.html" Nothing

showHoi :: Request -> Response
showHoi _ = responseLBS status200 [] (toLazyByteString $ stringUtf8 "ほい！\n")

showRequest :: Request -> Response
showRequest req = do
  let kv = (queryString req !! 0)
      key = fst kv
      value = snd kv
    in responseLBS status200 [] $ fromStrict $ key `BS.append` "=" `BS.append` (fromJust value) `BS.append` "\n"


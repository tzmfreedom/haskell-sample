{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Directory
import System.Posix.Time
import System.Posix.Files
import System.Posix.Types
import Foreign.C.Types
import Control.Monad
import Control.Monad.Trans(lift)
import ListT
import CMark
import Data.Text
import System.FilePath.Posix
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Yaml (decodeFileEither)
import Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.List as L
import Lib

data Meta = Meta{
  metaTitle :: String,
  metaDescription :: String,
  metaKeywords :: [String]
} deriving Show

main :: IO ()
main = do
--  print =<< epochTime
  files <- findUnprocessedSource
  processFiles files
  current <- epochTime
  writeFile lastUpdatedFile $ show current
  return ()

lastUpdatedFile :: FilePath
lastUpdatedFile = "lastUpdated"

srcDirectory :: FilePath
srcDirectory = "blogs"

layoutFile :: String
layoutFile = "layout.html"

lastUpdated :: IO String
lastUpdated = readFile lastUpdatedFile

findUnprocessedSource :: IO [FilePath]
findUnprocessedSource = do
  files <- listDirectory srcDirectory
  toList $ findUnprocessedSource' (Prelude.map (\f -> srcDirectory ++ "/" ++ f) files)

findUnprocessedSource' :: [FilePath] -> ListT IO FilePath
findUnprocessedSource' files = do
  f <- fromFoldable files
  lift $ print f
  continue <- lift (shouldProcessed f)
  guard continue
  return f

shouldProcessed :: FilePath -> IO Bool
shouldProcessed f = do
  status <- getFileStatus f
--  last <- lastUpdated
--  let last' = read last :: EpochTime
  return True
--  return (last' < modificationTime status)

-- get layout html
-- Markdown => HTML
-- write HTML
processFiles :: [FilePath] -> IO [()]
processFiles files = do
  (flip mapM) files $ \f -> do
    content <- readFile f
    meta <- decodeFileEither $ metaFile f
    html <- generateHTML (render content) $ getMeta meta
    writeFile (destFile f) html

getMeta :: Either a Meta -> Meta
getMeta (Right a) = a

metaFile :: FilePath -> String
metaFile f = "meta/" ++ (Prelude.drop (Prelude.length srcDirectory) (replaceExtension f ".yml"))

destFile :: FilePath -> String
destFile f = "dest/" ++ (Prelude.drop (Prelude.length srcDirectory) (replaceExtension f ".html"))

render :: String -> String
render md = unpack $ commonmarkToHtml [] $ pack md

generateHTML :: String -> Meta -> IO String
generateHTML content meta = do
  f <- readFile layoutFile
  case parse (parser content meta) "" f of
    Left err -> error "ParseError"
    Right html -> return html

parser :: String -> Meta -> Parser String
parser src meta = do
  ls <- many $ (
    (noneOf "{" >>= (\c -> return [c])) <|>
    try (string "{{" *> spaces *> string "body" *> spaces *> string "}}" *> return src) <|>
    try (string "{{" *> spaces *> string "meta" *> spaces *> string "}}" *> return (renderMeta meta))
    )
  return $ Prelude.concat ls

renderMeta :: Meta -> String
renderMeta meta = do
  "<title>" ++ metaTitle meta ++ "</title>\n" ++
    "<meta name=\"keywords\" content=\"" ++ keys ++ "\"/>\n" ++
    "<meta name=\"description\" contents=\"" ++ metaDescription meta ++ "\"/>"
    where
      keys :: String
      keys = L.intercalate "," $ metaKeywords meta

deriveJSON defaultOptions { fieldLabelModifier = firstLower . Prelude.drop 4 } ''Meta
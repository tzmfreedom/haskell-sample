{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Directory
import System.Posix.Time
import System.Posix.Files
import System.Posix.Types
import System.Environment
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
import Data.Time.Format
import GHC.Exts
import Lib

data Meta = Meta{
  metaTitle :: String,
  metaDescription :: String,
  metaKeywords :: [String],
  metaSrc :: String,
  metaCreatedAt :: String
} deriving Show

main :: IO ()
main = do
  args <- getArgs
  case if L.length(args) == 0 then "generate" else args !! 0 of
    "generate" -> generate
    "index" -> generateIndex
    "new" -> newBlog args
    otherwise -> print $ "No such subcommand" ++ args !! 0
  return ()

generate :: IO ()
generate = do
  files <- findUnprocessedSource
  meta <- getMetaInfo files
  processFiles meta
  current <- epochTime
  writeFile lastUpdatedFile $ show current

generateIndex :: IO ()
generateIndex = do
  files <- findAllSource
  meta <- getMetaInfo files
  writeFile "dest/index.html" =<< renderIndex meta

newBlog :: [String] -> IO ()
newBlog args = do
  let fname = srcDirectory ++ "/new.md"
  let mname = metaDirectory ++ "/new.yml"
  let title = if L.length(args) == 2 then args !! 1 else "new article"
  doesExist <- doesFileExist fname
  if not doesExist then do
    writeFile fname $ "# " ++ title
    writeFile mname $ "description:\nkeywords: []\ntitle: " ++ title
    else return ()

lastUpdatedFile :: FilePath
lastUpdatedFile = "lastUpdated"

srcDirectory :: FilePath
srcDirectory = "blogs"

metaDirectory :: FilePath
metaDirectory = "meta"

layoutFile :: String
layoutFile = "layout.html"

lastUpdated :: IO String
lastUpdated = readFile lastUpdatedFile

findAllSource :: IO [FilePath]
findAllSource = do
  files <- listDirectory srcDirectory
  return $ Prelude.map (\f -> srcDirectory ++ "/" ++ f) files

findUnprocessedSource :: IO [FilePath]
findUnprocessedSource = do
  files <- listDirectory srcDirectory
  ListT.toList $ findUnprocessedSource' (Prelude.map (\f -> srcDirectory ++ "/" ++ f) files)

findUnprocessedSource' :: [FilePath] -> ListT IO FilePath
findUnprocessedSource' files = do
  f <- fromFoldable files
--  lift $ print f
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
processFiles :: [Meta] -> IO [()]
processFiles metaInfo = do
  (flip mapM) metaInfo $ \m -> do
    let src = metaSrc m
    content <- readFile src
    html <- generateHTML (render content) m
    writeFile (destFile src) html

getMetaInfo:: [FilePath] -> IO [Meta]
getMetaInfo files = do
  (flip mapM) files $ \f -> do
    meta <- decodeFileEither $ metaFile f
    return $ getMeta meta

getMeta :: Either a Meta -> Meta
getMeta (Right a) = a

metaFile :: FilePath -> String
metaFile f = metaDirectory ++ "/" ++ (Prelude.drop (Prelude.length srcDirectory) (replaceExtension f ".yml"))

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
    try (string "{{" *> spaces *> string "body" *> spaces *> string "}}" *> return src) <|>
    try (string "{{" *> spaces *> string "meta" *> spaces *> string "}}" *> return (renderMeta meta)) <|>
    try (string "{{" *> spaces *> string "title" *> spaces *> string "}}" *> return ("<h1>" ++ metaTitle meta ++ "</h1>")) <|>
    try (string "{{" *> spaces *> string "createdAt" *> spaces *> string "}}" *> return (metaCreatedAt meta)) <|>
    (anyChar >>= (\c -> return [c]))
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

renderIndex :: [Meta] -> IO String
renderIndex metaInfo = do
  layout <- readFile "index.html"
  case parse p "" layout of
    Left err -> error "ParseError"
    Right html -> return html
  where
    p :: Parser String
    p = do
      parts <- many (
        try (string "{{" *> spaces *> string "body" *> spaces *> string "}}" *> return lst) <|>
        (anyChar >>= (\c -> return [c]))
        )
      return $ Prelude.concat parts
    lst = L.foldl (\x m -> x ++ "<li>" ++ metaCreatedAt m ++ ": <a href=\"/" ++ metaToPath m ++ "\">" ++ metaTitle m ++ "</a></li>") "" sorted
    sorted = sortWith (\m -> metaCreatedAt m) metaInfo

metaToPath :: Meta -> String
metaToPath meta = replaceExtension (takeFileName $ metaSrc meta) ".html"

deriveJSON defaultOptions { fieldLabelModifier = firstLower . Prelude.drop 4 } ''Meta
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import System.Posix.Time
import System.Posix.Files
import System.Posix.Types
import Foreign.C.Types
import Control.Monad
import Control.Monad.List
import Lib

main :: IO ()
main = do
  print findUnprocessedSource
  return ()

lastUpdatedFile :: FilePath
lastUpdatedFile = "lastUpdated"

srcDirectory :: FilePath
srcDirectory = "src"

lastUpdated :: IO String
lastUpdated = readFile lastUpdatedFile

findUnprocessedSource :: IO [FilePath]
findUnprocessedSource = do
  files <- listDirectory srcDirectory
  findUnprocessedSource' files

findUnprocessedSource' :: [FilePath] -> IO [FilePath]
findUnprocessedSource' files = runListT $ do
--  return files :: ListT IO FilePath
  f <- files
  continue <- lift (shouldProcessed f)
  guard continue
  return f

shouldProcessed :: FilePath -> IO Bool
shouldProcessed f = do
  status <- getFileStatus f
  last <- lastUpdated
  let last' = read last :: EpochTime
  return (last' < modificationTime status)

processFiles :: [String] -> IO ()
processFiles = undefined



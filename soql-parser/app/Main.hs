module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  parseSOQLTest $ args !! 0
  print . parseSOQL $ args !! 0

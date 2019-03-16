module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  let md = args !! 0
  parseMarkdownTest md
  print $ generateHtmlFromMarkdown md

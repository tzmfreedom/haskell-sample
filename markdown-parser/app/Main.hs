module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Lib

main :: IO ()
main = do
  args <- getArgs
  parseMarkdownTest $ args !! 0
  print $ generateHtml $ parseMarkdown $ args !! 0

parseMarkdown :: String -> Root
parseMarkdown src = do
  case parse parser "" src of
    Left err -> error "ParseError"
    Right node -> node

parseMarkdownTest :: String -> IO ()
parseMarkdownTest src = parseTest parser src

data Root = Root{ blocks :: [Block] }
  deriving Show

data Block = MHead Int String
  | MString String
  | MList [Block]
  | Horizontal
  deriving Show

parser :: Parser Root
parser = do
  Root <$> parseBlock `sepBy` newline

parseBlock :: Parser Block
parseBlock = try parseHead <|> try parseList <|> try parseHorizontal <|> try parseString

parseHead :: Parser Block
parseHead = do
  MHead <$> (length <$> many1 (char '#') <* spaces) <*> many1 (noneOf "\n")

parseString :: Parser Block
parseString = do
  MString <$> many1 (noneOf "\n")

parseList :: Parser Block
parseList = do
  MList <$> generateList <$> many1 (char '*' *> spaces *> many1 (noneOf "\n") <* newline)

parseHorizontal :: Parser Block
parseHorizontal = return Horizontal <* string "---"

generateList :: [String] -> [Block]
generateList = map MString

generateHtml :: Root -> String
generateHtml Root{blocks=blocks} = "<html><head></head><body>" ++ foldl (\x y -> x ++ generateHtml' y ) "" blocks ++ "</body></html>"

generateHtml' :: Block -> String
generateHtml' (MHead x y) = "<" ++ tagName ++ ">" ++ y ++ "</" ++ tagName ++ ">" where tagName = 'h':show x
generateHtml' (MString s) = s
generateHtml' (MList blocks) = "<ul>" ++ foldl (\x y -> x ++ "<li>" ++ generateHtml' y ++ "</li>") "" blocks ++ "</ul>"
generateHtml' Horizontal = "<hr/>"

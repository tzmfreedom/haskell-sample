module Lib (
  parseMarkdown,
  parseMarkdownTest,
  generateHtml,
  generateHtmlBody,
  generateHtmlFromMarkdown,
  generateHtmlBodyFromMarkdown
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Data.List

-- | generate HTML from Markdown
--
-- >>> generateHtmlFromMarkdown " "
-- "<html><head></head><body><p> </p></body></html>"
generateHtmlFromMarkdown :: String -> String
generateHtmlFromMarkdown = generateHtml . parseMarkdown

generateHtmlBodyFromMarkdown :: String -> String
generateHtmlBodyFromMarkdown = generateHtmlBody . parseMarkdown

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
  | Paragraph [Block]
  | MList [String]
  | Horizontal
  | MString String
  | SoftBreak
  | LineBreak
  | Code String
  | Strong String
  deriving Show

parser :: Parser Root
parser = Root <$> many1 parseBlock

parseBlock :: Parser Block
parseBlock = do
  try parseHead <|> try parseList <|> try parseHorizontal <|> try pLineBreak <|> try parseCodeBlock <|> try parseParagraph

parseParagraph :: Parser Block
parseParagraph = Paragraph <$> many1 parseInline <* pSepBlock

parseInline :: Parser Block
parseInline = try pLineBreak <|> try pBlank <|> try parseList <|> try parseStrong <|> try parseString <|> try pSoftBreak

parseHead :: Parser Block
parseHead = do
  MHead <$> (length <$> many1 (char '#') <* space <* spaces) <*> many1 (noneOf "\n") <* (many1 newline <|> eof *> return "")

parseList :: Parser Block
parseList = do
  list <- many1 (char '*' *> space *> spaces *> many1 (noneOf "\n") <* (newline <|> eof *> return ' '))
  newline <|> eof *> return ' '
  return (MList list)

parseHorizontal :: Parser Block
parseHorizontal = return Horizontal <* string "---"

parseCodeBlock :: Parser Block
parseCodeBlock = do
  string "```"
  newline <|> (many1 (noneOf "\n") *> newline)
  codes <- many1 $ (notFollowedBy (string "```") *> many1 (noneOf "\n") <* newline)
  string "```"
  return (Code $ intercalate "\n" codes)

parseStrong :: Parser Block
parseStrong = do
  char '*'
  h <- noneOf "* \n"
  t <- many (noneOf "*\n")
  char '*'
  return (Strong (h:t))

pSepBlock :: Parser ()
pSepBlock = try (newline *> newline *> return ()) <|> eof

parseString :: Parser Block
parseString = do
  MString <$> many1 (noneOf " \n")

pSoftBreak :: Parser Block
pSoftBreak = newline *> notFollowedBy newline *> return SoftBreak

pLineBreak :: Parser Block
pLineBreak = try (count 2 (char ' ') *> newline *> return LineBreak)

pBlank :: Parser Block
pBlank = MString <$> many1 (char ' ')

-- generate HTML
generateHtml :: Root -> String
generateHtml root = "<html><head></head><body>" ++ generateHtmlBody root ++ "</body></html>"

generateHtmlBody :: Root -> String
generateHtmlBody Root{blocks=blocks} = foldl (\x y -> x ++ generateHtml' y ) "" blocks

generateHtml' :: Block -> String
generateHtml' (MHead x y) = "<" ++ tagName ++ ">" ++ y ++ "</" ++ tagName ++ ">" where tagName = 'h':show x
generateHtml' (MList inlines) = "<ul>" ++ foldl (\x y -> x ++ "<li>" ++ y ++ "</li>") "" inlines ++ "</ul>"
generateHtml' (Paragraph inlines) = "<p>" ++ foldl (\x y -> x ++ generateHtml' y) "" inlines ++ "</p>"
generateHtml' Horizontal = "<hr/>"
generateHtml' (MString s) = s
generateHtml' SoftBreak = " "
generateHtml' LineBreak = "<br/>"
generateHtml' (Code src) = "<code>" ++ src ++ "</code>"
generateHtml' (Strong src) = "<strong>" ++ src ++ "</strong>"

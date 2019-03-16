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
  | Paragraph [Inline]
  | MList [String]
  | Horizontal
  deriving Show

data Inline = MString String
  | SoftBreak
  | LineBreak
  deriving Show

parser :: Parser Root
parser = Root <$> many1 parseBlock

parseBlock :: Parser Block
parseBlock = do
  try parseHead <|> try parseList <|> try parseHorizontal <|> try parseParagraph

parseParagraph :: Parser Block
parseParagraph = Paragraph <$> many1 parseInline <* pSepBlock

parseInline :: Parser Inline
parseInline = try pLineBreakOrBlank <|> try parseString <|> try pSoftBreak

parseHead :: Parser Block
parseHead = do
  MHead <$> (length <$> many1 (char '#') <* spaces) <*> many1 (noneOf "\n") <* (many1 newline <|> eof *> return "")

parseList :: Parser Block
parseList = do
  list <- many1 (char '*' *> spaces *> many1 (noneOf "\n") <* (newline <|> eof *> return ' '))
  newline <|> eof *> return ' '
  return (MList list)

parseHorizontal :: Parser Block
parseHorizontal = return Horizontal <* string "---"

pSepBlock :: Parser ()
pSepBlock = try (newline *> newline *> return ()) <|> eof

parseString :: Parser Inline
parseString = do
  MString <$> many1 (noneOf " \n")

pSoftBreak :: Parser Inline
pSoftBreak = newline *> notFollowedBy newline *> return SoftBreak

pLineBreakOrBlank :: Parser Inline
pLineBreakOrBlank = try (count 2 (char ' ') *> newline *> return LineBreak) <|> (MString <$> many1 (char ' '))

-- generate HTML
generateHtml :: Root -> String
generateHtml root = "<html><head></head><body>" ++ generateHtmlBody root ++ "</body></html>"

generateHtmlBody :: Root -> String
generateHtmlBody Root{blocks=blocks} = foldl (\x y -> x ++ generateHtml' y ) "" blocks

generateHtml' :: Block -> String
generateHtml' (MHead x y) = "<" ++ tagName ++ ">" ++ y ++ "</" ++ tagName ++ ">" where tagName = 'h':show x
generateHtml' (MList inlines) = "<ul>" ++ foldl (\x y -> x ++ "<li>" ++ y ++ "</li>") "" inlines ++ "</ul>"
generateHtml' (Paragraph inlines) = "<p>" ++ foldl (\x y -> x ++ generateHtml'' y) "" inlines ++ "</p>"
generateHtml' Horizontal = "<hr/>"

generateHtml'' :: Inline -> String
generateHtml'' (MString s) = s
generateHtml'' SoftBreak = " "
generateHtml'' LineBreak = "<br/>"

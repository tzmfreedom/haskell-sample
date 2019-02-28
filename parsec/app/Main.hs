module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Control.Applicative hiding((<|>), many)
import System.Environment
import Lib

data JsonValue = JsonBool Bool |
                 JsonString String |
                 JsonInt Int |
                 JsonFloat Float |
                 JsonArray [JsonValue] |
                 JsonObject [(String, JsonValue)]
                 deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  print $ Main.parse (args !! 0)

boolTrue :: Parser Bool
boolTrue = string "true" *> spaces *> pure True

boolFalse :: Parser Bool
boolFalse = string "false" *> spaces *> pure False

bool :: Parser Bool
bool = boolTrue <|> boolFalse

boolObject :: Parser JsonValue
boolObject = JsonBool <$> bool

matchString :: Parser String
matchString = char '"' *> many (noneOf "\"") <* char '"' <* spaces

stringObject :: Parser JsonValue
stringObject = JsonString <$> matchString

matchInt :: Parser Int
matchInt = do
  head <- oneOf ['1'..'9']
  tail <- many digit
  spaces
  return (read (head:tail) :: Int)

intObject :: Parser JsonValue
intObject = JsonInt <$> matchInt

matchFloat :: Parser Float
matchFloat = do
  head <- oneOf ['1'..'9']
  tail <- many digit
  char '.'
  frac <- many digit
  spaces
  return (read (if length frac == 0 then head:tail else (head:tail ++ '.':frac)) :: Float)

floatObject :: Parser JsonValue
floatObject = JsonFloat <$> matchFloat

matchArray :: Parser [JsonValue]
matchArray = char '[' *> spaces *> parseValue `sepBy` (char ',' >> spaces) <* char ']'

arrayObject :: Parser JsonValue
arrayObject = JsonArray <$> matchArray

matchObject :: Parser [(String, JsonValue)]
matchObject = char '{' *> spaces *> parseKeyValue `sepBy` (char ',' >> spaces) <* char '}'

parseKeyValue :: Parser (String, JsonValue)
parseKeyValue = do
  key <- matchString
  char ':'
  spaces
  value <- parseValue
  return (key, value)

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> matchObject

parseValue :: Parser JsonValue
parseValue = boolObject <|> stringObject <|> try floatObject <|> intObject <|> arrayObject <|> jsonObject

parse :: String -> JsonValue
parse str =
  case Text.Parsec.parse parseValue "" str of
    Left err -> error $ "ParserError"
    Right ls -> ls

repl :: String -> String
repl str = do
  case Text.Parsec.parse p "" str of
    Left err -> error $ "ParserError"
    Right ls -> ls
  where
    p = do
      ls <- many $ (try date) <|> (anyChar >>= (\c -> return [c]))
      return $ concat ls

    date = do
      y <- many1 digit
      char '/'
      m <- many1 digit
      char '/'
      d <- many1 digit
      return $ y ++ "-" ++ m ++ "-" ++ d ++ "!!"

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Data.List
import Data.Maybe

data SOQL = SOQL{
  fields :: [SOQLField],
  object :: SObject,
  whereClause :: Maybe WhereExpression
} deriving (Show, Eq)

data SOQLField = Field String | SubQuery SOQL
  deriving (Show, Eq)

data SObject = SObject String
  deriving (Show, Eq)

data WhereExpression = SingleCondition SOQLField String Expression
  | MultipleCondition WhereExpression String WhereExpression
  deriving (Show, Eq)

data Expression = SOQLInt Int
  | SOQLString String
  | SOQLFloat Float
  | SOQLBool Bool
  deriving (Show, Eq)

parseSOQL :: String -> SOQL
parseSOQL src = do
  case parse parseQuery "" src of
    Left err -> error "ParseError"
    Right soql -> soql

parseSOQLTest :: String -> IO ()
parseSOQLTest = parseTest parseQuery

parseQuery :: Parser SOQL
parseQuery = do
  spaces
  string "SELECT" <|> string "select"
  spaces
  fields <- parseSelectFields
  spaces
  string "FROM" <|> string "from"
  spaces
  object <- parseSObject
  spaces
  whereClause <- parseWhereClause
  return (SOQL{..})

parseSelectFields :: Parser [SOQLField]
parseSelectFields = parseSelectField `sepBy` (char ',')

parseSelectField :: Parser SOQLField
parseSelectField = (Field <$> try (spaces *> parseIdentifier <* spaces)) <|>
  (SubQuery <$> try (spaces *> char '(' *> parseQuery <* char ')' <* spaces))

parseSObject :: Parser SObject
parseSObject = SObject <$> parseIdentifier

parseIdentifier :: Parser String
parseIdentifier = many1 (noneOf " ,():=!")

parseWhereClause :: Parser (Maybe WhereExpression)
parseWhereClause = do
  (string "WHERE" <|> string "where")
  spaces
  condition <- parseCondition
  return (Just condition)
  <|> return Nothing

parseCondition :: Parser WhereExpression
parseCondition = do
  left <- parseSingleCondition
  parseMultipleCondition left <|> return left

parseMultipleCondition :: WhereExpression -> Parser WhereExpression
parseMultipleCondition left = do
  operator <- string "AND" <|> string "OR"
  spaces
  right <- parseCondition
  return (MultipleCondition left operator right)

parseSingleCondition :: Parser WhereExpression
parseSingleCondition = do
  field <- parseIdentifier
  spaces
  operator <- (try $ string "!=") <|> (try $ string ">=") <|> (try $ string "<=") <|> string "=" <|> string "<" <|> string ">"
  spaces
  expression <- parseExpression
  spaces
  return (SingleCondition (Field field) operator expression)

parseExpression :: Parser Expression
parseExpression = do
  parseString <|> parseBool <|> parseInt

parseString :: Parser Expression
parseString = SOQLString <$> (char '\'' *> many (noneOf "'") <*  char '\'')

parseBool :: Parser Expression
parseBool = do
  (SOQLBool <$> return True <* (string "true" <|> string "TRUE")) <|> (SOQLBool <$> return False <* (string "false" <|> string "FALSE"))

parseInt :: Parser Expression
parseInt = do
  char '0' *> spaces *> return (SOQLInt 0) <|> do
    h <- oneOf ['1'..'9']
    t <- many digit
    return (SOQLInt (read (h:t) :: Int))
  <|> do
    char '-'
    h <- oneOf ['1'..'9']
    t <- many digit
    return (SOQLInt $ -(read (h:t) :: Int))


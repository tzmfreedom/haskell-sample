{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module SOQL (
    parseSOQL,
    parseSOQLTest,
    generateSOQL,
    SOQL(..),
    SOQLField(..),
    SObject(..),
    WhereExpression(..),
    Expression(..)
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Data.Char
import Data.List
import Data.Maybe

data SOQL = SOQL{
  soqlFields :: [SOQLField],
  soqlObject :: SObject,
  soqlWhereClause :: Maybe WhereExpression,
  soqlGroupByClause :: Maybe [SOQLField],
  soqlHavingClause :: Maybe WhereExpression
} deriving (Show, Eq)

data SOQLField = Field String
  | Function String [SOQLField]
  | SubQuery SOQL
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
  caseIgnoredString "select"
  spaces
  soqlFields <- parseSelectFields
  spaces
  caseIgnoredString "from"
  spaces
  soqlObject <- parseSObject
  spaces
  soqlWhereClause <- parseWhereClause
  spaces
  soqlGroupByClause <- parseGroupByClause
  spaces
  soqlHavingClause <- parseHavingClause
  return (SOQL{..})

parseSelectFields :: Parser [SOQLField]
parseSelectFields = parseSelectField `sepBy` (char ',')

parseSelectField :: Parser SOQLField
parseSelectField =  try parseFunction <|> try parseField <|> try parseSubQuery

parseField :: Parser SOQLField
parseField = Field <$> (spaces *> parseIdentifier <* spaces)

parseFunction :: Parser SOQLField
parseFunction = do
  spaces
  funcName <- parseIdentifier
  char '('
  fields <- parseField `sepBy` (char ',')
  char ')'
  spaces
  return (Function funcName fields)

parseSubQuery :: Parser SOQLField
parseSubQuery = SubQuery <$> (spaces *> char '(' *> parseQuery <* char ')' <* spaces)

parseSObject :: Parser SObject
parseSObject = SObject <$> parseIdentifier

parseIdentifier :: Parser String
parseIdentifier = many1 (noneOf " ,():=!")

parseWhereClause :: Parser (Maybe WhereExpression)
parseWhereClause = do
  caseIgnoredString "where"
  spaces
  condition <- parseCondition
  return (Just condition)
  <|> return Nothing

parseGroupByClause :: Parser (Maybe [SOQLField])
parseGroupByClause = do
   caseIgnoredString "group"
   spaces
   caseIgnoredString "by"
   spaces
   Just <$> (parseField `sepBy` (char ','))
   <|> return Nothing

parseHavingClause :: Parser (Maybe WhereExpression)
parseHavingClause = do
  caseIgnoredString "having"
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
  operator <- caseIgnoredString "and" <|> caseIgnoredString "or"
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
  (SOQLBool <$> return True <* caseIgnoredString "true") <|> (SOQLBool <$> return False <* caseIgnoredString "false")

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

generateSOQL :: SOQL -> String
generateSOQL SOQL{..} = do
  "SELECT " ++ expandFields soqlFields ++ " FROM " ++ expandSObject soqlObject ++ expandWhereClause soqlWhereClause

expandFields :: [SOQLField] -> String
expandFields (f:fs) = foldl (\x y -> x ++ ", " ++ expandField y) (expandField f) fs

expandField :: SOQLField -> String
expandField (Field f) = f
expandField (SubQuery s) = "(" ++ generateSOQL s ++ ")"

expandSObject :: SObject -> String
expandSObject (SObject s) = s

expandWhereClause :: Maybe WhereExpression -> String
expandWhereClause Nothing = ""
expandWhereClause (Just x) = " WHERE " ++ expandWhereExpression x

expandWhereExpression :: WhereExpression -> String
expandWhereExpression (SingleCondition (Field f) op exp) = f ++ " " ++ op ++ " " ++ expandExpression exp
expandWhereExpression (MultipleCondition left op right) = expandWhereExpression left ++ " " ++ op ++ " " ++ expandWhereExpression right

expandExpression :: Expression -> String
expandExpression (SOQLInt i) = show i
expandExpression (SOQLString s) = '\'':s ++ "'"
expandExpression (SOQLFloat f) = show f
expandExpression (SOQLBool True) = "true"
expandExpression (SOQLBool False) =  "false"

caseIgnoredString :: String -> Parser String
caseIgnoredString s = mapM caseIgnoredChar s <?> "\"" ++ s ++ "\""

caseIgnoredChar :: Char -> Parser Char
caseIgnoredChar c = do
  (char $ toUpper c) <|> (char $ toLower c)
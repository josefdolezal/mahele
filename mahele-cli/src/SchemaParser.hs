module SchemaParser where

import Text.ParserCombinators.Parsec
import qualified Data.Maybe

data ModelType = Type
               | Enum

data Keyword = Cast

data Symbol = Assignment
            | Alternative
            | Option
            | Colon
            | Apostrophe

data ParsedEnum = ParsedEnum
    { peIdentifier :: String
    , peCases      :: [EnumCase]
    } deriving (Show)
        
data EnumCase = EnumCase
    { ecIdentifier :: String
    , ecRawValue   :: Maybe String
    } deriving (Show)

instance Show ModelType where
    show Type = "type"
    show Enum = "enum"

instance Show Keyword where
    show Cast = "as"

symbol :: Symbol -> Char
symbol Assignment  = '='
symbol Alternative = '|'
symbol Option      = '?'
symbol Colon       = ':'
symbol Apostrophe  = '\''

enum :: GenParser Char st ParsedEnum
enum = do
    a  <- typeAnnotation Enum
    cs <- many1 $ do { newline; enumCase }
    return $ ParsedEnum a cs

enumCase :: GenParser Char st EnumCase
enumCase = do
    spaces
    alternative
    spaces
    c <- constructor
    v <- optionalRawValue
    return $ EnumCase c v

typeAnnotation :: ModelType -> GenParser Char st String
typeAnnotation modelType = do
    string $ show modelType
    spaces
    name <- typename
    spaces
    assignment
    return name

constructor :: GenParser Char st String
constructor = identifier

optionalRawValue :: GenParser Char st (Maybe String)
optionalRawValue = try (Just <$> rawValue)
                   <|> do { spaces; return Nothing }

rawValue :: GenParser Char st String
rawValue = do
    spaces
    string $ show Cast
    spaces
    quoted identifier

typename :: GenParser Char st String
typename = identifier

fieldType :: GenParser Char st String
fieldType = identifier

fieldName :: GenParser Char st String
fieldName = identifier

identifier :: GenParser Char st String
identifier = do
    c <- letter
    cs <- many alphaNum
    return (c:cs)

quoted :: GenParser Char st a -> GenParser Char st a
quoted = between (char $ symbol Apostrophe) (char $ symbol Apostrophe)

alternative :: GenParser Char st Char
alternative = reservedSymbol Alternative

assignment :: GenParser Char st Char
assignment = reservedSymbol Assignment

optionality :: GenParser Char st Char
optionality = reservedSymbol Option

colon :: GenParser Char st Char
colon = reservedSymbol Colon

reservedSymbol :: Symbol -> GenParser Char st Char
reservedSymbol = char . symbol

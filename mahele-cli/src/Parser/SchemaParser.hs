module Parser.SchemaParser
    ( parseSchema
    , enumCase
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Maybe
import Parser.Models

-- Internal data structures

data Keyword = Cast

data Symbol = Assignment
            | Alternative
            | Option
            | Colon
            | Apostrophe

instance Show Keyword where
    show Cast = "as"

symbol :: Symbol -> Char
symbol Assignment  = '='
symbol Alternative = '|'
symbol Option      = '?'
symbol Colon       = ':'
symbol Apostrophe  = '\''

-- Public API

parseSchema :: String -> Either ParseError [Model]
parseSchema = parse modelTypes ""

-- Parsers

modelTypes :: GenParser Char st [Model]
modelTypes = many modelType

modelType :: GenParser Char st Model
modelType = do
    many whiteChar
    (Left <$> typeDef) <|> (Right <$> enum)

typeDef :: GenParser Char st Type
typeDef = do
    a <- typeAnnotation "type"
    cs <- many $ do { newline; field }
    return $ Type a cs

enum :: GenParser Char st Enumeration
enum = do
    a  <- typeAnnotation "enum"
    cs <- many1 (try enumCase)
    whiteChars
    return $ Enumeration a cs

field :: GenParser Char st Property
field = do
    inlineInvisibles
    fId <- fieldName
    inlineInvisibles
    colon
    inlineInvisibles
    fType <- fieldType
    fOpt <- optionality
    return $ Property fId fType fOpt

enumCase :: GenParser Char st Case
enumCase = do
    inlineInvisibles
    alternative
    inlineInvisibles
    c <- constructor
    v <- optionalRawValue
    inlineInvisibles
    endOfLine
    return $ Case c v

typeAnnotation :: String -> GenParser Char st String
typeAnnotation modelType = do
    string modelType
    inlineInvisibles
    name <- typename
    inlineInvisibles
    assignment
    inlineInvisibles
    newline
    return name

constructor :: GenParser Char st String
constructor = identifier

optionalRawValue :: GenParser Char st (Maybe String)
optionalRawValue = try (Just <$> rawValue)
                    <|> do { inlineInvisibles; return Nothing }

rawValue :: GenParser Char st String
rawValue = do
    inlineInvisibles
    string $ show Cast
    inlineInvisibles
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

optionality :: GenParser Char st Bool
optionality = try $ const True <$> reservedSymbol Option
                <|> return False

colon :: GenParser Char st Char
colon = reservedSymbol Colon

reservedSymbol :: Symbol -> GenParser Char st Char
reservedSymbol = char . symbol

whiteChars :: GenParser Char st String
whiteChars = many whiteChar 

whiteChar :: GenParser Char st Char
whiteChar = inlineInvisible
         <|> newline
inlineInvisibles = many inlineInvisible

inlineInvisible :: GenParser Char st Char
inlineInvisible = oneOf "\t "

endOfLine :: GenParser Char st ()
endOfLine = eof
         <|> do { newline; return () }

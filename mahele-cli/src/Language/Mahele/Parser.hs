{-|
Module      : Language.Mahele.Parser
Description : Hamele language parser
Copyright   : (c) Josef Dolezal, 2018
License     : MIT
Stability   : experimental

Exposes parser for text inputs written in mahele language.
-}

module Language.Mahele.Parser
    ( parseMahele
    ) where

import           Text.ParserCombinators.Parsec
import qualified Data.Maybe
import           Language.Mahele.Syntax

-- Internal data structures

data Keyword = Cast -- ^ Cast keyword used for 'as' representation in enum raw value declaration

data Symbol = Assignment  -- ^ Represents '=' symbol
            | Alternative -- ^ Represents '=' symbol
            | Option      -- ^ Represents '|' symbol
            | Colon       -- ^ Represents ':' symbol
            | Apostrophe  -- ^ Represents '\'' symbol
            | Comment     -- ^ Represents '#' symbol

instance Show Keyword where
    -- | Converts given `Keyword` into its string representation.
    show Cast = "as"

-- | Converts given `Symbol` into its char representation.
symbol :: Symbol -> Char
symbol Assignment  = '='
symbol Alternative = '|'
symbol Option      = '?'
symbol Colon       = ':'
symbol Apostrophe  = '\''
symbol Comment     = '#'

-- Public API

-- | Mahele Token parser parser.
parseMahele :: String -> Either ParseError [Token]
parseMahele = parse models ""

-- Parsers

-- | Parses sequence of model definition.
models :: GenParser Char st [Token]
models = many model

-- | Parses single model deifinition.
model :: GenParser Char st Token
model = do
    many $ do { try comment; whiteChars }
    (Left <$> typeDef) <|> (Right <$> enum)

-- | Parses definition of `Type` model.
typeDef :: GenParser Char st Type
typeDef = do
    a <- typeAnnotation "type"
    props <- many $ try property
    whiteChars
    return $ Type a props

-- | Parses definition of `Enum` model.
enum :: GenParser Char st Enumeration
enum = do
    a <- typeAnnotation "enum"
    cases <- many1 $ try enumCase
    whiteChars
    return $ Enumeration a cases

-- | Parses single property field for `Type` model.
property :: GenParser Char st Property
property = do
    inlineInvisibles
    fId <- fieldName
    inlineInvisibles
    colon
    inlineInvisibles
    fType <- fieldType
    fOpt <- optionality
    inlineInvisibles
    endOfLine
    return $ Property fId fType fOpt

-- | Parses single case for `Enum` model.
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

-- | Parses annotation for all possible model objects.
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

-- | Enum case named constructor.
constructor :: GenParser Char st String
constructor = identifier

-- | Parses optional raw value for case.
optionalRawValue :: GenParser Char st (Maybe String)
optionalRawValue = try (Just <$> rawValue)
                    <|> do { inlineInvisibles; return Nothing }

-- | Parses required raw value for case.                    
rawValue :: GenParser Char st String
rawValue = do
    inlineInvisibles
    string $ show Cast
    inlineInvisibles
    quoted identifier

-- | Parses `Type` name declaration.
typename :: GenParser Char st String
typename = identifier

-- | Parses property data type.
fieldType :: GenParser Char st String
fieldType = identifier

-- | Parses property name.
fieldName :: GenParser Char st String
fieldName = identifier

-- | Parses generic type identifier.
identifier :: GenParser Char st String
identifier = do
    c <- letter
    cs <- many alphaNum
    return (c:cs)

-- | Parses with `p` parser enclosed into `Apostrophe` symbols.
quoted :: GenParser Char st a -> GenParser Char st a
quoted = between (char $ symbol Apostrophe) (char $ symbol Apostrophe)

-- | Parses `Alternative` symbol.
alternative :: GenParser Char st Char
alternative = reservedSymbol Alternative

-- | Parses `Assignment` symbol.
assignment :: GenParser Char st Char
assignment = reservedSymbol Assignment

-- | Parses symbol for property optionality type.
optionality :: GenParser Char st Bool
optionality = try $ const True <$> reservedSymbol Option
                <|> return False

-- | Parses toplevel comment.
comment :: GenParser Char st String
comment = do
    inlineInvisibles
    reservedSymbol Comment
    inlineInvisible
    manyTill anyChar endOfLine

-- | Parses `Colon` symbol.
colon :: GenParser Char st Char
colon = reservedSymbol Colon

-- | Parses given arbitrary `Symbol`.
reservedSymbol :: Symbol -> GenParser Char st Char
reservedSymbol = char . symbol

-- | Parses multiple white characters (newlines, spaces or tabs).
whiteChars :: GenParser Char st String
whiteChars = many whiteChar

-- | Parses single white character (newline, space or tab)
whiteChar :: GenParser Char st Char
whiteChar = inlineInvisible
         <|> newline

-- | Parses multiple inline white characters.
inlineInvisibles :: GenParser Char st String
inlineInvisibles = many inlineInvisible

-- | Parses inline white character (does not parse newlines).
inlineInvisible :: GenParser Char st Char
inlineInvisible = oneOf "\t "

-- | Parses end of line or end of input.
endOfLine :: GenParser Char st ()
endOfLine = eof
         <|> do { newline; return () }

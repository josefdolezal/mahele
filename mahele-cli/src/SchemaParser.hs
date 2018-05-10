module SchemaParser where

import Text.ParserCombinators.Parsec

data ModelType = Type
               | Enum

data Keyword = Cast

data Symbol = Assignment
            | Alternative
            | Option
            | Colon

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

module Language.Mahele.Syntax (
    Token(..),
    Type(..),
    Property(..),
    Enumeration(..),
    Case(..)
    ) where

type Token = Either Type Enumeration

-- Data

data Type = Type
    { typeIdentifier :: String
    , peFields     :: [Property]
    } deriving (Show, Eq)

data Property = Property
    { propertyIdentifier :: String
    , propertyDataType   :: String
    , propertyIsOptional :: Bool
    } deriving (Show, Eq)

-- Enumeration

data Enumeration = Enumeration
    { enumIdentifier :: String
    , enumCases      :: [Case]
    } deriving (Show, Eq)
        
data Case = Case
    { caseIdentifier :: String
    , caseRawValue   :: Maybe String
    } deriving (Show, Eq)

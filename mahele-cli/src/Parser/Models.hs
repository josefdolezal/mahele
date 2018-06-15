module Parser.Models (
    Model(..),
    Type(..),
    Property(..),
    Enumeration(..),
    Case(..)
    ) where

type Model = Either Type Enumeration

-- Data

data Type = Type
    { typeIdentifier :: String
    , peFields     :: [Property]
    } deriving (Show)

data Property = Property
    { propertyIdentifier :: String
    , propertyDataType   :: String
    , propertyIsOptional :: Bool
    } deriving (Show)

-- Enumeration

data Enumeration = Enumeration
    { enumIdentifier :: String
    , enumCases      :: [Case]
    } deriving (Show)
        
data Case = Case
    { caseIdentifier :: String
    , caseRawValue   :: Maybe String
    } deriving (Show)

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
    , peFields     :: [Property]
    } deriving (Show)

data Property = Property
    { tfIdentifier :: String
    , tfDataType   :: String
    , tfIsOptional :: Bool
    } deriving (Show)

-- Enumeration

data Enumeration = Enumeration
    { peIdentifier :: String
    , peCases      :: [Case]
    } deriving (Show)
        
data Case = Case
    { ecIdentifier :: String
    , ecRawValue   :: Maybe String
    } deriving (Show)

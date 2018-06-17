{-|
Module      : Language.Mahele.Syntax
Description : Defines Mahele syntax AST objects
Copyright   : (c) Josef Dolezal, 2018
License     : MIT
Stability   : experimental

Exposes AST model objects for Mahele language.
-}

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
    { typeIdentifier :: String     -- ^ Type identifier/name.
    , typeProp       :: [Property] -- ^ List of type properties.
    } deriving (Show, Eq)

data Property = Property
    { propertyIdentifier :: String -- ^ Property identifier/name.
    , propertyDataType   :: String -- ^ Property data type.
    , propertyIsOptional :: Bool   -- ^ Indicates whether the property is optional.
    } deriving (Show, Eq)

-- Enumeration

data Enumeration = Enumeration
    { enumIdentifier :: String -- ^ Enumeration identifier/name.
    , enumCases      :: [Case] -- ^ List of enumeration cases.
    } deriving (Show, Eq)
        
data Case = Case
    { caseIdentifier :: String       -- ^ Enumeration case identifier.
    , caseRawValue   :: Maybe String -- ^ Enumeration case raw value.
    } deriving (Show, Eq)

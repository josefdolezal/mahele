{-|
Module      : Language.Swift.Pretty
Description : Hamele model pretty printer in Swift
Copyright   : (c) Josef Dolezal, 2018
License     : MIT
Stability   : experimental

Pretty printer for Mahele model objects in Swift language.
-}

module Language.Swift.Pretty
    ( prettySwift
    ) where

import Text.PrettyPrint
import Language.Mahele.Syntax
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

-- | Pretty prints token in Swift language
prettySwift :: Token -> String
prettySwift (Left l) = show . pretty $ l
prettySwift (Right r) = show . pretty $ r

-- | Dot representation.
dot :: Doc
dot = char '.'

-- | Default indentaion level.
indentation :: Int
indentation = 4

-- | Converts srting into lower came case.
lowerCameCase :: String -> String
lowerCameCase (x:xs) = toLower x : xs
lowerCameCase x      = x

-- | Representation for optionality.
prettyOptional :: Bool -> Doc
prettyOptional True  = char '?'
prettyOptional False = empty

class PrettySwift a where
    -- | Pretty prints class instance.
    pretty :: a -> Doc

instance PrettySwift Type where
    pretty (Type id props) = hsep [ text "struct"
                                  , text id
                                  , char '{'
                                  ]
                          $+$ nest indentation verticalProps
                          $+$ char '}'
        where verticalProps = vcat $ map pretty props

instance PrettySwift Property where
    pretty (Property id t opt) = hsep [ text "var"
                                      , text id <> colon
                                      , text t <> prettyOptional opt
                                      ]

instance PrettySwift Enumeration where
    pretty (Enumeration id cases) = hsep [ text "enum"
                                         , text id <> colon
                                         , text "String"
                                         , char '{'
                                         ]
                                 $+$ nest indentation verticalCases
                                 $+$ char '}'
        where verticalCases = vcat $ map pretty cases

instance PrettySwift Case where
    pretty (Case id raw) = hsep [ text "case"
                                , dot <> text (lowerCameCase id)
                                , maybe empty prettyRaw raw
                                ]
        where prettyRaw r = hsep [ char '=', doubleQuotes $ text r ]

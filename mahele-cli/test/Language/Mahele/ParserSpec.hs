module Language.Mahele.ParserSpec
    ( spec
    ) where

import           Test.Hspec
import           Control.Monad
import           Data.Semigroup
import           Text.Printf
import qualified Language.Mahele.Parser as Parser
import           Text.ParserCombinators.Parsec
import           Language.Mahele.Syntax

fixture :: FilePath -> IO [Token]
fixture path = getModel =<< Parser.parseMahele <$> readFile fullPath
    where getModel res = case res of
                Left e  -> fail (cause e)
                Right m -> pure m
          fullPath = "test/fixtures/" ++ path
          cause e = "Could not parse '" ++ fullPath ++ "': " ++ show e

expectedEnums :: [Enumeration]
expectedEnums = [ Enumeration "UserRole" [ Case "Superuser" (Just "admin")
                                            , Case "Customer" (Just "user")
                                            ]
                , Enumeration "Device" [ Case "Laptop" (Just "notebook")
                                        , Case "Phone" Nothing
                                        , Case "PC" Nothing
                                        ]
                , Enumeration "OS" [ Case "Linux" Nothing
                                    , Case "MacOS" Nothing
                                    , Case "Windows" Nothing
                                    ]
                ]

expectedTypes :: [Type]
expectedTypes = [ Type "User" [ Property "id" "Int" False
                                , Property "email" "String" False
                                , Property "name" "String" True
                                , Property "role" "UserRole" False
                                ]
                , Type "BlogPost" [ Property "id" "Int" False
                                    , Property "title" "String" False
                                    , Property "content" "String" False
                                    , Property "rating" "Int" True
                                    , Property "userId" "Int" False
                                    , Property "categoryId" "Int" False
                                    ]
                , Type "Category" [ Property "id" "Int" False
                                    , Property "title" "String" False
                                    , Property "description" "String" False
                                    ]
                ]

spec :: Spec
spec = do
    describe "Enums" $ do
        actualEnums <- runIO $ fixture "Enums.mahele"
        let enums = zip actualEnums expectedEnums
        forM_ enums $ \(actual, expected) ->
            it (printf "it parse '%s' enum" $ enumIdentifier expected) $
                actual `shouldBe` Right expected

        it "all enums are tested" $
            length actualEnums `shouldBe` length expectedEnums
    
    describe "Types" $ do
        actualTypes <- runIO $ fixture "Types.mahele"
        let types = zip actualTypes expectedTypes
        forM_ types $ \(actual, expected) ->
            it (printf "it parse '%s' type" $ typeIdentifier expected) $
                actual `shouldBe` Left expected

        it "all types are tested" $
            length actualTypes `shouldBe` length expectedTypes

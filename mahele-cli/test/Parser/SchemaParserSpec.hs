module Parser.SchemaParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Semigroup
import Text.Printf
import qualified Parser.SchemaParser as Parser
import Text.ParserCombinators.Parsec
import Parser.Models

fixture :: FilePath -> IO [Model]
fixture path = getModel =<< Parser.parseSchema <$> readFile fullPath
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
    
    -- describe "Types" $ do
    --     actualTypes <- runIO $ fixture "Types.mahele"
    --     let types = zip actualTypes expectedTypes
    --     forM_ types $ \(actual, expected) ->
    --         it (printf "it parse '%s' type" $ typeIdentifier expected) $
    --             actual `shouldBe` Left expected

    --     it "all types are tested" $
    --         length actualTypes `shouldBe` length expectedTypes

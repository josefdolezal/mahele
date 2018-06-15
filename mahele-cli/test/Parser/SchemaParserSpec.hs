module Parser.SchemaParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parser.Models
import qualified Parser.SchemaParser as Parser
import Control.Monad

fixture :: FilePath -> IO [Model]
fixture path = getModel =<< Parser.parseSchema <$> readFile fullPath
    where getModel res = case res of
              Left e  -> fail (cause e)
              Right m -> pure m
          fullPath = "test/fixtures/" ++ path
          cause e = "Could not parse '" ++ fullPath ++ "': " ++ show e

plainEnum :: Enumeration
plainEnum = Enumeration "UserRole" roles
    where roles = [ Case "Superuser" (Just "admin")
                  , Case "Customer" (Just "user")
                  ]

expectedEnums :: [Enumeration]
expectedEnums = [plainEnum]

plainType :: Type
plainType = Type "User" []

spec :: Spec
spec = describe "Enums" $ do
        actualEnums <- runIO $ fixture "Enums.mahele"
        let enums = zip actualEnums expectedEnums
        forM_ enums $ \(actual, expected) ->
            it "parse enums" $
                actual `shouldBe` Right expected

        it "parse all cases" $
            length actualEnums `shouldBe` length expectedEnums

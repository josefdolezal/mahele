import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser.SchemaParser" $ do
        it "works" $
            True `shouldBe` True

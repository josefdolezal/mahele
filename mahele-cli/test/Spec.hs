import Test.Hspec

import qualified Parser.SchemaParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser.SchemaParserSpec" Parser.SchemaParserSpec.spec

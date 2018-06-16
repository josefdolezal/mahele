import Test.Hspec

import qualified Language.Mahele.ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Language.Mahele.ParserSpec" Language.Mahele.ParserSpec.spec

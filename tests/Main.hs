import Test.Hspec

import qualified KissDListTests

main :: IO ()
main = hspec $ do
    KissDListTests.tests

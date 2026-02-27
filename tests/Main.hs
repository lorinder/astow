import Test.Hspec

import qualified KissDListTests
import qualified FileUtilsTests

main :: IO ()
main = hspec $ do
    KissDListTests.tests
    FileUtilsTests.tests


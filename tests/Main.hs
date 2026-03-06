import Test.Hspec

import qualified KissDListTests
import qualified FileUtilsTests
import qualified DirTreeTests
import qualified FallibleTests

main :: IO ()
main = hspec $ do
    KissDListTests.tests
    FileUtilsTests.tests
    DirTreeTests.tests
    FallibleTests.tests


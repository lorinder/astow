import Test.Hspec

import qualified AstowMonadTTests
import qualified DiagnosticTests
import qualified DirTreeTests
import qualified FallibleTests
import qualified FileUtilsTests
import qualified KissDListTests

main :: IO ()
main = hspec $ do
    AstowMonadTTests.tests
    DiagnosticTests.tests
    DirTreeTests.tests
    FallibleTests.tests
    FileUtilsTests.tests
    KissDListTests.tests


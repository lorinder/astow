module FileUtilsTests (
    tests
) where

import Test.Hspec

tests :: SpecWith ()
tests = do
    describe "FileUtils unit tests" $ do
        -- Skip for now; I'm not sold on that OsPath idea, as support
        -- for it is meager.
        return ()

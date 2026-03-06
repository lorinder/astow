module KissDListTests (
    tests
) where

import Test.Hspec
import Test.QuickCheck

import KissDList

tests :: SpecWith ()
tests = do
    describe "KissDList unit tests" $ do
        it "empty construction" $ do
            toList (empty :: KissDList Int) `shouldBe` []
        it "singleton" $
            property $ \x -> toList (singleton x) == [x :: Int]
        it "fromList & toList" $
            property $ \xs -> (toList . fromList) xs == (xs :: [Char])
        it "<> (fixed)" $ do
            toList ((singleton (1 :: Int)) <> (singleton 2)
                        <> (singleton 3))
            `shouldBe` [1, 2, 3]
        it "<> associativity" $ do
            property $ \xs ys zs ->
                toList ((fromList (xs :: [Int]) <> fromList ys) <> fromList zs)
                ==
                toList (fromList xs <> (fromList ys <> fromList zs))
        it "<> isomorphism with lists" $ do
            property $ \xs ys ->
                toList (fromList xs <> fromList ys) == xs <> (ys :: [String])
        it "mempty left identity: mempty <> xs == xs" $
            property $ \xs ->
                toList (mempty <> fromList xs) == (xs :: [Int])
        it "mempty right identity: xs <> mempty == xs" $
            property $ \xs ->
                toList (fromList xs <> mempty) == (xs :: [Int])

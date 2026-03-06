{-# OPTIONS_GHC -fno-warn-orphans #-}

module FallibleTests (tests) where

import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(..), runIdentity)
import Test.Hspec
import Test.QuickCheck

import Fallible

-- ---------------------------------------------------------------------------
-- Orphan instance needed for testing

instance Arbitrary a => Arbitrary (Fallible a) where
    arbitrary = oneof
        [ pure Aborted
        , Completed False <$> arbitrary
        , Completed True  <$> arbitrary
        ]

-- ---------------------------------------------------------------------------
-- Helpers

-- | Run FallibleT over Identity.
runFI :: FallibleT Identity a -> Fallible a
runFI = runIdentity . runFallibleT

-- | Wrap a Fallible in FallibleT Identity.
mkFT :: Fallible a -> FallibleT Identity a
mkFT = FallibleT . Identity

-- | A set of representative Int -> Fallible Int functions, exercising
-- all three output constructors and mixed behaviour.
sampleFns :: [Int -> Fallible Int]
sampleFns =
    [ \_ -> Aborted
    , Completed False
    , Completed True
    , \x -> if even x then Completed True (x `div` 2) else Aborted
    , \x -> if x > 0  then Completed True x else Completed False (negate x)
    ]

-- ---------------------------------------------------------------------------
-- Tests

tests :: SpecWith ()
tests = do
    describe "Fallible"          fallibleTests
    describe "FallibleT Identity" fallibleTTests

-- ---------------------------------------------------------------------------
-- Fallible

fallibleTests :: SpecWith ()
fallibleTests = do
    describe "monad laws" $ do
        it "left identity: return a >>= f == f a" $
            property $ \(x :: Int) ->
                all (\f -> (return x >>= f) == f x) sampleFns

        it "right identity: m >>= return == m" $
            property $ \(m :: Fallible Int) ->
                (m >>= return) == m

        it "associativity: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $
            property $ \(m :: Fallible Int) ->
                all (\(f, g) ->
                        ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g)))
                    [(f, g) | f <- sampleFns, g <- sampleFns]

    describe "abort" $ do
        it "equals Aborted" $
            (abort :: Fallible Int) `shouldBe` Aborted
        it ">> anything equals Aborted" $
            property $ \(m :: Fallible Int) ->
                (abort >> m :: Fallible Int) == Aborted

    describe "invalid" $ do
        it "equals Completed False" $
            invalid (7 :: Int) `shouldBe` Completed False 7
        it ">>= return preserves the invalid flag" $
            (invalid 3 >>= return :: Fallible Int) `shouldBe` Completed False 3
        it ">>= valid-returning function stays invalid" $
            (invalid 3 >>= \x -> Completed True (x + 1) :: Fallible Int)
                `shouldBe` Completed False 4
        it ">>= invalid-returning function stays invalid" $
            (invalid 3 >>= \x -> Completed False (x + 1) :: Fallible Int)
                `shouldBe` Completed False 4
        it ">>= abort yields Aborted" $
            (invalid (3 :: Int) >>= \_ -> abort :: Fallible Int) `shouldBe` Aborted

    describe "pure / valid completion" $ do
        it ">>= valid-returning function stays valid" $
            (return 3 >>= \x -> return (x + 1) :: Fallible Int)
                `shouldBe` Completed True 4
        it ">>= invalid-returning function becomes invalid" $
            (return 3 >>= \x -> invalid (x + 1) :: Fallible Int)
                `shouldBe` Completed False 4
        it ">>= abort yields Aborted" $
            (return (3 :: Int) >>= \_ -> abort :: Fallible Int) `shouldBe` Aborted

-- ---------------------------------------------------------------------------
-- FallibleT Identity

fallibleTTests :: SpecWith ()
fallibleTTests = do
    let sampleFTFns :: [Int -> FallibleT Identity Int]
        sampleFTFns = map (mkFT .) sampleFns

    describe "monad laws" $ do
        it "left identity: return a >>= f == f a" $
            property $ \(x :: Int) ->
                all (\f -> runFI (return x >>= f) == runFI (f x)) sampleFTFns

        it "right identity: m >>= return == m" $
            property $ \(m :: Fallible Int) ->
                runFI (mkFT m >>= return) == m

        it "associativity: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $
            property $ \(m :: Fallible Int) ->
                all (\(f, g) ->
                        runFI ((mkFT m >>= f) >>= g)
                        == runFI (mkFT m >>= (\x -> f x >>= g)))
                    [(f, g) | f <- sampleFTFns, g <- sampleFTFns]

    describe "abort" $ do
        it "equals Aborted" $
            runFI (abort :: FallibleT Identity Int) `shouldBe` Aborted
        it ">> anything equals Aborted" $
            property $ \(m :: Fallible Int) ->
                runFI (abort >> mkFT m :: FallibleT Identity Int) == Aborted

    describe "invalid" $ do
        it "equals Completed False" $
            runFI (invalid (7 :: Int) :: FallibleT Identity Int)
                `shouldBe` Completed False 7
        it ">>= return preserves the invalid flag" $
            runFI (invalid 3 >>= return :: FallibleT Identity Int)
                `shouldBe` Completed False 3
        it ">>= valid-returning function stays invalid" $
            runFI (invalid 3 >>= \x -> return (x + 1) :: FallibleT Identity Int)
                `shouldBe` Completed False 4

    describe "lift (MonadTrans laws)" $ do
        it "wraps a pure value as a valid completion" $
            runFI (lift (Identity (42 :: Int))) `shouldBe` Completed True 42
        it "law 1: lift (return a) == return a" $
            property $ \(x :: Int) ->
                runFI (lift (return x)) == runFI (return x :: FallibleT Identity Int)
        it "law 2: lift (m >>= f) == lift m >>= (lift . f)" $
            property $ \(x :: Int) ->
                let m = Identity x
                    f n = Identity (n * 2 + 1)
                in runFI (lift (m >>= f))
                   == runFI (lift m >>= lift . f :: FallibleT Identity Int)

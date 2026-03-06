module AstowMonadTTests (tests) where

import Control.Monad.Trans.Class              (lift)
import qualified Control.Monad.Trans.Writer.Strict as W
import Data.Functor.Identity                  (Identity(..), runIdentity)
import Test.Hspec

import AstowMonadT
import Diagnostic
import Fallible
import KissDList                              (KissDList, toList, fromList)

-- ---------------------------------------------------------------------------
-- Helpers

-- | Run a pure AstowMonadT computation and return
--   (fallible-result, logged-diagnostics).
runPure :: AstowMonadT Identity a -> (Fallible a, [Diagnostic])
runPure m =
    let (fa, dl) = runIdentity (W.runWriterT (runFallibleT (runAstowMonadT m)))
    in  (fa, toList dl)

-- Predicate helpers for Fallible (avoid needing an Eq orphan instance).

isValidWith :: (a -> Bool) -> Fallible a -> Bool
isValidWith p (Completed True x) = p x
isValidWith _ _                  = False

isAborted :: Fallible a -> Bool
isAborted Aborted = True
isAborted _       = False

-- | Convenience: assert a result is exactly (Completed True x).
shouldBeValid :: (Show a, Eq a) => Fallible a -> a -> Expectation
shouldBeValid fa expected = case fa of
    Completed True x | x == expected -> return ()
    Completed True x  -> expectationFailure $
        "Expected valid " ++ show expected ++ ", got valid " ++ show x
    Completed False x -> expectationFailure $
        "Expected valid " ++ show expected ++ ", got invalid " ++ show x
    Aborted           -> expectationFailure $
        "Expected valid " ++ show expected ++ ", got Aborted"

-- | Convenience: assert a result is exactly (Completed False x).
shouldBeInvalid :: (Show a, Eq a) => Fallible a -> a -> Expectation
shouldBeInvalid fa expected = case fa of
    Completed False x | x == expected -> return ()
    Completed False x -> expectationFailure $
        "Expected invalid " ++ show expected ++ ", got invalid " ++ show x
    Completed True x  -> expectationFailure $
        "Expected invalid " ++ show expected ++ ", got valid " ++ show x
    Aborted           -> expectationFailure $
        "Expected invalid " ++ show expected ++ ", got Aborted"

-- ---------------------------------------------------------------------------
-- Tests

tests :: SpecWith ()
tests = do
    describe "return / pure"     returnTests
    describe "abort"             abortTests
    describe "invalid"           invalidTests
    describe "tell / tell1"      tellTests
    describe "lift"              liftTests
    describe "bind interactions" bindTests

-- ---------------------------------------------------------------------------
-- return / pure

returnTests :: SpecWith ()
returnTests = do
    it "produces a valid result" $ do
        let (fa, _) = runPure (return (42 :: Int))
        fa `shouldBeValid` 42

    it "produces no diagnostics" $ do
        let (_, msgs) = runPure (return (42 :: Int))
        length msgs `shouldBe` 0

-- ---------------------------------------------------------------------------
-- abort

abortTests :: SpecWith ()
abortTests = do
    it "result is Aborted" $ do
        let (fa, _) = runPure (abort :: AstowMonadT Identity Int)
        isAborted fa `shouldBe` True

    it "produces no diagnostics by itself" $ do
        let (_, msgs) = runPure (abort :: AstowMonadT Identity Int)
        length msgs `shouldBe` 0

    it "short-circuits: tell1 after abort is NOT executed" $ do
        let d = mkInfoDiagnostic "should not appear"
            action :: AstowMonadT Identity Int
            action = abort >> tell1 d >> return 0
            (fa, msgs) = runPure action
        isAborted fa `shouldBe` True
        length msgs `shouldBe` 0

    it "short-circuits: return after abort is NOT executed" $ do
        let (fa, _) = runPure (abort >> return (1 :: Int))
        isAborted fa `shouldBe` True

-- ---------------------------------------------------------------------------
-- invalid

invalidTests :: SpecWith ()
invalidTests = do
    it "result is Completed False" $ do
        let (fa, _) = runPure (invalid (7 :: Int))
        fa `shouldBeInvalid` 7

    it "produces no diagnostics by itself" $ do
        let (_, msgs) = runPure (invalid (7 :: Int))
        length msgs `shouldBe` 0

    it "does NOT short-circuit: continuation IS executed" $ do
        -- invalid continues the chain (unlike abort)
        let (fa, _) = runPure (invalid (3 :: Int) >>= \x -> return (x + 1))
        fa `shouldBeInvalid` 4

    it "invalid then valid keeps invalid flag" $ do
        let (fa, _) = runPure (invalid (1 :: Int) >>= \x -> return (x * 10))
        fa `shouldBeInvalid` 10

    it "invalid then invalid stays invalid" $ do
        let (fa, _) = runPure (invalid (1 :: Int) >>= \x -> invalid (x + 1))
        fa `shouldBeInvalid` 2

    it "invalid then abort yields Aborted" $ do
        let (fa, _) = runPure (invalid (1 :: Int) >>= \_ -> abort)
        isAborted fa `shouldBe` True

-- ---------------------------------------------------------------------------
-- tell / tell1

tellTests :: SpecWith ()
tellTests = do
    it "tell1 logs exactly one diagnostic" $ do
        let d = mkInfoDiagnostic "msg"
            (_, msgs) = runPure (tell1 d)
        length msgs `shouldBe` 1

    it "tell1 logs the correct message" $ do
        let d = mkInfoDiagnostic "hello"
            (_, msgs) = runPure (tell1 d)
        map diagWhen msgs `shouldBe` ["hello"]

    it "tell1 produces a valid unit result" $ do
        let (fa, _) = runPure (tell1 (mkInfoDiagnostic "x"))
        isValidWith (== ()) fa `shouldBe` True

    it "tell with a KissDList logs all entries" $ do
        let msgs3 = fromList
                [ mkInfoDiagnostic "a"
                , mkInfoDiagnostic "b"
                , mkInfoDiagnostic "c"
                ]
            (_, msgs) = runPure (tell msgs3)
        length msgs `shouldBe` 3

    it "tell with empty list logs nothing" $ do
        let (_, msgs) = runPure (tell (mempty :: KissDList Diagnostic))
        length msgs `shouldBe` 0

    it "multiple tell1 calls accumulate in order" $ do
        let d1 = mkInfoDiagnostic "first"
            d2 = mkInfoDiagnostic "second"
            d3 = mkInfoDiagnostic "third"
            (_, msgs) = runPure (tell1 d1 >> tell1 d2 >> tell1 d3 :: AstowMonadT Identity ())
        map diagWhen msgs `shouldBe` ["first", "second", "third"]

    it "tell1 before abort IS preserved in the log" $ do
        let d = mkInfoDiagnostic "logged before abort"
            (fa, msgs) = runPure (tell1 d >> abort :: AstowMonadT Identity Int)
        isAborted fa  `shouldBe` True
        length msgs   `shouldBe` 1
        case msgs of
            [m] -> diagWhen m `shouldBe` "logged before abort"
            _   -> expectationFailure "Expected exactly one logged message"

    it "tell1 before invalid IS preserved in the log" $ do
        let d = mkInfoDiagnostic "before invalid"
            (fa, msgs) = runPure (tell1 d >> invalid (42 :: Int))
        fa `shouldBeInvalid` 42
        length msgs `shouldBe` 1

    it "tell1 after invalid IS executed and logged" $ do
        let d = mkInfoDiagnostic "after invalid"
            (_, msgs) = runPure
                (invalid (1 :: Int) >>= \_ -> tell1 d >> return (2 :: Int))
        length msgs `shouldBe` 1

-- ---------------------------------------------------------------------------
-- lift

liftTests :: SpecWith ()
liftTests = do
    it "lift wraps a pure value as a valid completion" $ do
        let (fa, _) = runPure (lift (Identity (99 :: Int)))
        fa `shouldBeValid` 99

    it "lift produces no diagnostics" $ do
        let (_, msgs) = runPure (lift (Identity (99 :: Int)))
        length msgs `shouldBe` 0

-- ---------------------------------------------------------------------------
-- bind interactions

bindTests :: SpecWith ()
bindTests = do
    it "valid chain accumulates messages from each step" $ do
        let (fa, msgs) = runPure $ do
                tell1 (mkInfoDiagnostic "step1")
                x <- return (10 :: Int)
                tell1 (mkInfoDiagnostic "step2")
                return (x + 1)
        fa `shouldBeValid` 11
        length msgs `shouldBe` 2

    it "abort after several tells preserves earlier log entries" $ do
        let (fa, msgs) = runPure $ do
                tell1 (mkInfoDiagnostic "a")
                tell1 (mkInfoDiagnostic "b")
                abort :: AstowMonadT Identity Int
        isAborted fa `shouldBe` True
        length msgs  `shouldBe` 2

    it "invalid result with interleaved tells" $ do
        let (fa, msgs) = runPure $ do
                tell1 (mkInfoDiagnostic "before")
                x <- invalid (5 :: Int)
                tell1 (mkInfoDiagnostic "after")
                return (x * 2)
        fa `shouldBeInvalid` 10
        length msgs `shouldBe` 2

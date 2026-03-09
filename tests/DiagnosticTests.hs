module DiagnosticTests (tests) where

import qualified Data.Text          as T
import Test.Hspec

import Diagnostic

-- ---------------------------------------------------------------------------
-- Tests

tests :: SpecWith ()
tests = do
    describe "Severity" severityTests
    describe "diagnosticMessage" diagnosticMessageTests
    describe "mkInfoDiagnostic"  mkInfoDiagnosticTests

-- ---------------------------------------------------------------------------
-- Severity

severityTests :: SpecWith ()
severityTests = do
    it "Info < Warning" $
        Info < Warning `shouldBe` True
    it "Warning < Error" $
        Warning < Error `shouldBe` True
    it "Info < Error (transitive)" $
        Info < Error `shouldBe` True
    it "severity is equal to itself" $ do
        (Info    == Info)    `shouldBe` True
        (Warning == Warning) `shouldBe` True
        (Error   == Error)   `shouldBe` True

-- ---------------------------------------------------------------------------
-- diagnosticMessage

diagnosticMessageTests :: SpecWith ()
diagnosticMessageTests = do
    it "Info / NoPayload: 'Info: <when>'" $ do
        let d = Diagnostic "starting up" NoPayload Info
        diagnosticMessage d `shouldBe` "Info: starting up"

    it "Warning / NoPayload: 'Warning: <when>'" $ do
        let d = Diagnostic "slow query" NoPayload Warning
        diagnosticMessage d `shouldBe` "Warning: slow query"

    it "Error / NoPayload: 'Error: <when>'" $ do
        let d = Diagnostic "crash" NoPayload Error
        diagnosticMessage d `shouldBe` "Error: crash"

    it "TextPayload appends ': <text>'" $ do
        let d = Diagnostic "reading config" (TextPayload "file missing") Error
        diagnosticMessage d `shouldBe` "Error: reading config: file missing"

    it "TextPayload with empty text still appends the colon separator" $ do
        let d = Diagnostic "step" (TextPayload "") Warning
        diagnosticMessage d `shouldBe` "Warning: step: "

    it "IOPayload appends the IO-error string" $ do
        let e = userError "file not found"
            d = Diagnostic "opening log" (IOPayload e) Error
        T.isInfixOf "file not found" (diagnosticMessage d) `shouldBe` True

    it "IOPayload message starts with 'Error: <when>'" $ do
        let e = userError "err"
            d = Diagnostic "op" (IOPayload e) Error
        T.isPrefixOf "Error: op" (diagnosticMessage d) `shouldBe` True

-- ---------------------------------------------------------------------------
-- mkInfoDiagnostic

mkInfoDiagnosticTests :: SpecWith ()
mkInfoDiagnosticTests = do
    it "severity is Info" $
        diagSeverity (mkInfoDiagnostic "x") `shouldBe` Info

    it "diagWhen holds the supplied message" $
        diagWhen (mkInfoDiagnostic "hello") `shouldBe` "hello"

    it "payload is NoPayload" $
        case diagPayload (mkInfoDiagnostic "x") of
            NoPayload -> return ()
            other     -> expectationFailure $
                            "Expected NoPayload, got: " ++ show other

    it "diagnosticMessage format is 'Info: <msg>'" $
        diagnosticMessage (mkInfoDiagnostic "event occurred")
            `shouldBe` "Info: event occurred"

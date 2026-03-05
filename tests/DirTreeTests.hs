{-# LANGUAGE QuasiQuotes #-}

module DirTreeTests (tests) where

import Data.Functor.Identity          (Identity(..), runIdentity)
import Data.List.NonEmpty             (NonEmpty(..))
import Data.Maybe                     (isJust, isNothing)
import qualified Data.Map.Strict      as M
import qualified Control.Monad.Trans.Writer.Strict as W
import Control.Monad.State.Strict     (runState)
import System.OsPath                  (OsPath, osp, (</>))

import Test.Hspec

import AstowMonadT
import DirTree
import FakeFsOps
import Fallible

-- ---------------------------------------------------------------------------
-- Helpers

-- | Run a pure AstowMonadT and return the Fallible result.
runPure :: AstowMonadT Identity a -> Fallible a
runPure = fst . runIdentity . W.runWriterT . runFallibleT . runAstowMonadT

-- | Run readFromFs against a fake in-memory file system.
runFakeReadFromFs :: DirTree () -> OsPath -> Fallible (DirTree ())
runFakeReadFromFs initialFs path =
    fst . fst $
        runState
            (runFakeFsOps
                (W.runWriterT
                    (runFallibleT
                        (runAstowMonadT
                            (readFromFs path)))))
            initialFs

-- | Assert a Fallible is a valid (True) completion satisfying a predicate.
shouldSucceedWith :: String -> Fallible a -> (a -> Bool) -> Expectation
shouldSucceedWith msg result predicate = case result of
    Completed True x | predicate x -> return ()
    Completed True _               -> expectationFailure $ msg ++ ": predicate failed"
    Completed False _              -> expectationFailure $ msg ++ ": completed with errors"
    Aborted                        -> expectationFailure $ msg ++ ": aborted"

-- | Assert a Fallible computation ended in failure (abort or invalid).
shouldFail :: Show a => Fallible a -> Expectation
shouldFail Aborted           = return ()
shouldFail (Completed False _) = return ()
shouldFail other             =
    expectationFailure $ "Expected failure, got: " ++ show other

-- ---------------------------------------------------------------------------
-- Shared fixtures

-- | Dir { "a" -> File 1, "b" -> Dir { "c" -> File 2 } }
sampleTree :: DirTree Int
sampleTree = Dir $ M.fromList
    [ ([osp|a|], File 1)
    , ([osp|b|], Dir $ M.singleton [osp|c|] (File 2))
    ]

-- ---------------------------------------------------------------------------
-- Test groups

tests :: SpecWith ()
tests = do
    describe "mkLine"          mkLineTests
    describe "getNode"         getNodeTests
    describe "walkM"           walkMTests
    describe "modifyDirentWith" modifyTests
    describe "merge"           mergeTests
    describe "readFromFs"      readFromFsTests

-- ---------------------------------------------------------------------------
-- mkLine

mkLineTests :: SpecWith ()
mkLineTests = do
    it "empty path returns the endpoint unchanged" $
        case mkLine [] (File (42 :: Int)) of
            File 42 -> return ()
            other   -> expectationFailure $ "Expected File 42, got: " ++ show other

    it "single component wraps endpoint in a Dir with one entry" $
        case mkLine [[osp|x|]] (File ()) of
            Dir m | M.size m == 1 -> return ()
            other -> expectationFailure $ "Expected Dir with 1 entry: " ++ show other

    it "getNode on the result recovers the endpoint" $
        getNode [[osp|x|], [osp|y|]]
                (mkLine [[osp|x|], [osp|y|]] (File (42 :: Int)))
        `shouldSatisfy` (\x -> case x of
            Just (File 42) -> True
            _              -> False)

-- ---------------------------------------------------------------------------
-- getNode

getNodeTests :: SpecWith ()
getNodeTests = do
    it "empty path returns the root" $
        getNode ([] :: [OsPath]) sampleTree `shouldSatisfy` isJust

    it "locates a top-level file" $
        getNode [[osp|a|]] sampleTree `shouldSatisfy` (\x -> case x of
            Just (File 1) -> True
            _             -> False)

    it "locates a nested file" $
        getNode [[osp|b|], [osp|c|]] sampleTree `shouldSatisfy` (\x -> case x of
            Just (File 2) -> True
            _             -> False)

    it "locates a subdirectory node" $
        getNode [[osp|b|]] sampleTree `shouldSatisfy` (\x -> case x of
            Just (Dir _) -> True
            _            -> False)

    it "returns Nothing for a missing name" $
        getNode [[osp|z|]] sampleTree `shouldSatisfy` isNothing

    it "returns Nothing when trying to descend into a File" $
        getNode [[osp|a|], [osp|x|]] sampleTree `shouldSatisfy` isNothing

-- ---------------------------------------------------------------------------
-- walkM

walkMTests :: SpecWith ()
walkMTests = do
    it "a File yields exactly one visit" $ do
        rs <- walkM [osp|r|] (\_ _ -> return ()) (File ())
        length rs `shouldBe` 1

    it "visitor receives the base path for a File" $ do
        ps <- walkM [osp|root|] (\path _ -> return path) (File ())
        ps `shouldBe` [[osp|root|]]

    it "an empty Dir yields no visits" $ do
        rs <- walkM [osp|r|] (\_ _ -> return ()) (Dir M.empty :: DirTree ())
        rs `shouldBe` []

    it "visit count equals the number of leaf files" $ do
        let tr = Dir $ M.fromList
                    [ ([osp|a|],   File ())
                    , ([osp|b|],   File ())
                    , ([osp|sub|], Dir $ M.singleton [osp|c|] (File ()))
                    ]
        rs <- walkM [osp|r|] (\_ _ -> return ()) tr
        length rs `shouldBe` 3

    it "visitor receives the full path for nested files" $ do
        let tr = Dir $ M.singleton [osp|sub|]
                           (Dir $ M.singleton [osp|f|] (File ()))
        ps <- walkM [osp|base|] (\path _ -> return path) tr
        ps `shouldBe` [[osp|base|] </> [osp|sub|] </> [osp|f|]]

    it "Functor fmap is consistent with walkM on the attribute" $ do
        let tr  = Dir $ M.fromList [ ([osp|a|], File 1), ([osp|b|], File 2) ]
            tr' = fmap (+10) tr
        as <- walkM [osp|r|] (\_ x -> return x) tr'
        as `shouldBe` ([11, 12] :: [Int])

-- ---------------------------------------------------------------------------
-- modifyDirentWith

modifyTests :: SpecWith ()
modifyTests = do
    let emptyDir   = Dir M.empty :: DirTree ()
        singleFile = Dir $ M.singleton [osp|a|] (File ())
        insertF _ _ = return (Just (File ()))
        deleteF _ _ = return Nothing

    it "inserts a new entry into an empty Dir" $ do
        let result = runPure $
                modifyDirentWith insertF ([osp|a|] :| []) emptyDir
        shouldSucceedWith "insert" result $ \t ->
            isJust (getNode [[osp|a|]] t)

    it "deletes an existing entry" $ do
        let result = runPure $
                modifyDirentWith deleteF ([osp|a|] :| []) singleFile
        shouldSucceedWith "delete" result $ \t ->
            isNothing (getNode [[osp|a|]] t)

    it "replaces an existing entry" $ do
        let replaceF _ _ = return (Just (File ()))
            tr = Dir $ M.singleton [osp|a|] (File ())
            result = runPure $ modifyDirentWith replaceF ([osp|a|] :| []) tr
        shouldSucceedWith "replace" result $ \t ->
            case getNode [[osp|a|]] t of
                Just (File ()) -> True
                _              -> False

    it "aborts when a path component is a File (not a Dir)" $ do
        -- Descending "a/b" where "a" is a File must abort.
        let tr = Dir $ M.singleton [osp|a|] (File ())
            result = runPure $
                modifyDirentWith insertF ([osp|a|] :| [[osp|b|]]) tr
        shouldFail result

    it "calls modifier with remaining path when intermediate Dir is absent" $ do
        -- Path "a/b" on empty dir: "a" is absent, f receives ["b"] and Nothing.
        let recordRemainder rest _ = return (Just (mkLine rest (File ())))
            result = runPure $
                modifyDirentWith recordRemainder ([osp|a|] :| [[osp|b|]]) emptyDir
        shouldSucceedWith "remainder" result $ \t ->
            case getNode [[osp|a|], [osp|b|]] t of
                Just (File ()) -> True
                _              -> False

-- ---------------------------------------------------------------------------
-- merge

mergeTests :: SpecWith ()
mergeTests = do
    let root = [osp|root|]

    it "merges two Files, passing Both to the merger" $ do
        let result = runPure $ merge
                (\_ lra -> case lra of
                    Both l r -> return (l, r)
                    _        -> abort)
                root
                (File (1 :: Int))
                (File (2 :: Int))
        shouldSucceedWith "file-file" result $ \x -> case x of
            File (1, 2) -> True
            _           -> False

    it "fails on a File/Dir type mismatch" $ do
        let result = runPure $ merge
                (\_ _ -> return ())
                root
                (File ())
                (Dir M.empty :: DirTree ())
        shouldFail result

    it "merges two empty Dirs into an empty Dir" $ do
        let result = runPure $ merge
                (\_ _ -> return ())
                root
                (Dir M.empty :: DirTree ())
                (Dir M.empty :: DirTree ())
        shouldSucceedWith "empty dirs" result $ \x -> case x of
            Dir m | M.null m -> True
            _                -> False

    it "left-only entries are passed as LeftOnly to the merger" $ do
        let result = runPure $ merge
                (\_ lra -> case lra of
                    LeftOnly l -> return l
                    _          -> abort)
                root
                (Dir $ M.singleton [osp|a|] (File (42 :: Int)))
                (Dir M.empty)
        shouldSucceedWith "left-only" result $ \t ->
            case getNode [[osp|a|]] t of
                Just (File 42) -> True
                _              -> False

    it "right-only entries are passed as RightOnly to the merger" $ do
        let result = runPure $ merge
                (\_ lra -> case lra of
                    RightOnly r -> return r
                    _           -> abort)
                root
                (Dir M.empty)
                (Dir $ M.singleton [osp|b|] (File (7 :: Int)))
        shouldSucceedWith "right-only" result $ \t ->
            case getNode [[osp|b|]] t of
                Just (File 7) -> True
                _             -> False

    it "shared entries are passed as Both, results combined" $ do
        let result = runPure $ merge
                (\_ lra -> case lra of
                    Both l r -> return (l + r)
                    _        -> abort)
                root
                (Dir $ M.singleton [osp|a|] (File (3 :: Int)))
                (Dir $ M.singleton [osp|a|] (File (4 :: Int)))
        shouldSucceedWith "both" result $ \t ->
            case getNode [[osp|a|]] t of
                Just (File 7) -> True
                _             -> False

    it "merges nested dirs recursively" $ do
        let left  = Dir $ M.singleton [osp|d|]
                        (Dir $ M.singleton [osp|f|] (File (1 :: Int)))
            right = Dir $ M.singleton [osp|d|]
                        (Dir $ M.singleton [osp|g|] (File (2 :: Int)))
            result = runPure $ merge
                (\_ lra -> case lra of
                    LeftOnly  l -> return l
                    RightOnly r -> return r
                    Both l _    -> return l)
                root left right
        shouldSucceedWith "nested" result $ \t ->
            isJust (getNode [[osp|d|], [osp|f|]] t) &&
            isJust (getNode [[osp|d|], [osp|g|]] t)

-- ---------------------------------------------------------------------------
-- readFromFs

readFromFsTests :: SpecWith ()
readFromFsTests = do
    it "reads a single file node" $ do
        let fs     = Dir $ M.singleton [osp|f|] (File ())
            result = runFakeReadFromFs fs [osp|f|]
        shouldSucceedWith "single file" result $ \t -> case t of
            File () -> True
            _       -> False

    it "reads an empty directory" $ do
        let fs     = Dir $ M.singleton [osp|d|] (Dir M.empty)
            result = runFakeReadFromFs fs [osp|d|]
        shouldSucceedWith "empty dir" result $ \t -> case t of
            Dir m | M.null m -> True
            _                -> False

    it "reads a flat directory with multiple files" $ do
        let fs     = Dir $ M.singleton [osp|root|]
                        (Dir $ M.fromList
                            [ ([osp|a|], File ())
                            , ([osp|b|], File ())
                            ])
            result = runFakeReadFromFs fs [osp|root|]
        shouldSucceedWith "flat dir" result $ \t ->
            isJust (getNode [[osp|a|]] t) &&
            isJust (getNode [[osp|b|]] t)

    it "reads a nested directory tree" $ do
        let fs     = Dir $ M.singleton [osp|root|]
                        (Dir $ M.fromList
                            [ ([osp|f|],   File ())
                            , ([osp|sub|], Dir $ M.singleton [osp|g|] (File ()))
                            ])
            result = runFakeReadFromFs fs [osp|root|]
        shouldSucceedWith "nested" result $ \t ->
            isJust (getNode [[osp|f|]] t) &&
            isJust (getNode [[osp|sub|], [osp|g|]] t)

    it "fails on a non-existent path" $ do
        let result = runFakeReadFromFs (Dir M.empty) [osp|nonexistent|]
        shouldFail result

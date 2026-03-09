{-# LANGUAGE QuasiQuotes #-}

module ActionsTests (tests) where

import Control.Monad.State.Strict             (runState)
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Data.Map.Strict              as M
import System.OsPath                          (OsPath, osp, (</>))
import Test.Hspec

import Actions
import AstowMonadT                            (AstowMonadT, runAstowMonadT)
import DirTree                                (DirTree(..), getNode)
import FakeFsOps                              (FakeFsOps, runFakeFsOps)
import Fallible                               (Fallible(..), FallibleT(..))

-- ---------------------------------------------------------------------------
-- Run helper

-- | Run an action against an in-memory fake filesystem, returning the
-- fallible result and the filesystem state after the action completes.
runFake :: DirTree String
        -> AstowMonadT (FakeFsOps String) a
        -> (Fallible a, DirTree String)
runFake fs action =
    let ((fb, _diags), finalFs) =
            runState
                (runFakeFsOps
                    (W.runWriterT
                        (runFallibleT
                            (runAstowMonadT action))))
                fs
    in (fb, finalFs)

-- | Return the file content at a path, or Nothing.
fileAt :: [OsPath] -> DirTree String -> Maybe String
fileAt path tree = case getNode path tree of
    Just (File x) -> Just x
    _             -> Nothing

-- | Check whether a File node exists at a path.
fileExists :: [OsPath] -> DirTree a -> Bool
fileExists path tree = case getNode path tree of
    Just (File _) -> True
    _             -> False

-- ---------------------------------------------------------------------------
-- Shared fixtures

-- | Action context: stow root at "stow/", target root at "target/".
cx :: ActionContext
cx = ActionContext { acStowDir = [osp|stow|], acTargetDir = [osp|target|] }

-- | A single-file RootedDirTree with empty prefix and one file "f".
-- Stow path used by actions: stow/f   Target path: target/f
treeSingle :: [RootedDirTree ()]
treeSingle = [RootedDirTree mempty (Dir $ M.singleton [osp|f|] (File ()))]

-- | Stow has "f" = "v1"; target has no entry for "f".
fsStowOnly :: DirTree String
fsStowOnly = Dir $ M.singleton [osp|stow|]
    (Dir $ M.singleton [osp|f|] (File "v1"))

-- | Stow has "f" = "v1"; target has "f" = "v1" (same content).
fsSame :: DirTree String
fsSame = Dir $ M.fromList
    [ ([osp|stow|],   Dir $ M.singleton [osp|f|] (File "v1"))
    , ([osp|target|], Dir $ M.singleton [osp|f|] (File "v1"))
    ]

-- | Stow has "f" = "v1"; target has "f" = "v2" (contents differ).
fsDiffer :: DirTree String
fsDiffer = Dir $ M.fromList
    [ ([osp|stow|],   Dir $ M.singleton [osp|f|] (File "v1"))
    , ([osp|target|], Dir $ M.singleton [osp|f|] (File "v2"))
    ]

-- | A tree with one level of subdirectory: top, sub/a, sub/b.
treeNested :: [RootedDirTree ()]
treeNested = [RootedDirTree mempty
    (Dir $ M.fromList
        [ ([osp|top|], File ())
        , ([osp|sub|], Dir $ M.fromList
            [ ([osp|a|], File ())
            , ([osp|b|], File ())
            ])
        ])]

-- | Stow has top="v1", sub/a="v2", sub/b="v3"; target is empty.
fsNestedStowOnly :: DirTree String
fsNestedStowOnly = Dir $ M.singleton [osp|stow|]
    (Dir $ M.fromList
        [ ([osp|top|], File "v1")
        , ([osp|sub|], Dir $ M.fromList
            [ ([osp|a|], File "v2")
            , ([osp|b|], File "v3")
            ])
        ])

-- | Stow and target have identical content for treeNested.
fsNestedSame :: DirTree String
fsNestedSame = Dir $ M.fromList
    [ ([osp|stow|], Dir $ M.fromList
        [ ([osp|top|], File "v1")
        , ([osp|sub|], Dir $ M.fromList
            [ ([osp|a|], File "v2")
            , ([osp|b|], File "v3")
            ])
        ])
    , ([osp|target|], Dir $ M.fromList
        [ ([osp|top|], File "v1")
        , ([osp|sub|], Dir $ M.fromList
            [ ([osp|a|], File "v2")
            , ([osp|b|], File "v3")
            ])
        ])
    ]

-- | treeNested: target/sub/a differs from stow; everything else is identical.
fsNestedDiffer :: DirTree String
fsNestedDiffer = Dir $ M.fromList
    [ ([osp|stow|], Dir $ M.fromList
        [ ([osp|top|], File "v1")
        , ([osp|sub|], Dir $ M.fromList
            [ ([osp|a|], File "v2")
            , ([osp|b|], File "v3")
            ])
        ])
    , ([osp|target|], Dir $ M.fromList
        [ ([osp|top|], File "v1")
        , ([osp|sub|], Dir $ M.fromList
            [ ([osp|a|], File "MODIFIED")
            , ([osp|b|], File "v3")
            ])
        ])
    ]

-- | A tree with two levels of nesting: x/y (a single deeply nested file).
treeDeep :: [RootedDirTree ()]
treeDeep = [RootedDirTree mempty
    (Dir $ M.singleton [osp|x|]
        (Dir $ M.singleton [osp|y|] (File ())))]

-- | Stow has x/y="deep"; target is empty.
fsDeepStowOnly :: DirTree String
fsDeepStowOnly = Dir $ M.singleton [osp|stow|]
    (Dir $ M.singleton [osp|x|]
        (Dir $ M.singleton [osp|y|] (File "deep")))

-- ---------------------------------------------------------------------------
-- Tests

tests :: SpecWith ()
tests = do
    describe "manifest" manifestTests
    describe "status"   statusTests
    describe "push"     pushTests
    describe "pull"     pullTests
    describe "delete"   deleteTests
    describe "symlink"  symlinkTests
    describe "diff"     diffTests

-- ---------------------------------------------------------------------------
-- manifest
--
-- manifest just collects paths from the RootedDirTrees; it never touches
-- the target filesystem, so an empty DirTree suffices as the initial state.

manifestTests :: SpecWith ()
manifestTests = do
    it "empty tree list returns no paths" $ do
        let (fa, _) = runFake (Dir M.empty) (manifest cx [])
        fa `shouldBe` Completed True []

    it "single file returns its package-relative path as text" $ do
        let (fa, _) = runFake (Dir M.empty) (manifest cx treeSingle)
        fa `shouldBe` Completed True ["f"]

    it "multiple files in one tree are listed in sorted order" $ do
        let tree = [RootedDirTree mempty
                        (Dir $ M.fromList
                            [ ([osp|a|], File ())
                            , ([osp|b|], File ())
                            ])]
            (fa, _) = runFake (Dir M.empty) (manifest cx tree)
        fa `shouldBe` Completed True ["a", "b"]

    it "RootedDirTree root is prepended to each path" $ do
        let tree = [RootedDirTree [osp|pkg|]
                        (Dir $ M.singleton [osp|f|] (File ()))]
            (fa, _) = runFake (Dir M.empty) (manifest cx tree)
        fa `shouldBe` Completed True ["pkg/f"]

    it "paths from multiple RootedDirTrees are concatenated in order" $ do
        let t1   = RootedDirTree [osp|p1|] (Dir $ M.singleton [osp|a|] (File ()))
            t2   = RootedDirTree [osp|p2|] (Dir $ M.singleton [osp|b|] (File ()))
            (fa, _) = runFake (Dir M.empty) (manifest cx [t1, t2])
        fa `shouldBe` Completed True ["p1/a", "p2/b"]

    it "nested tree: subdirectory paths are listed with separators in sorted order" $ do
        let (fa, _) = runFake (Dir M.empty) (manifest cx treeNested)
        fa `shouldBe` Completed True ["sub/a", "sub/b", "top"]

    it "deeply nested tree: file is listed with its full relative path" $ do
        let (fa, _) = runFake (Dir M.empty) (manifest cx treeDeep)
        fa `shouldBe` Completed True ["x/y"]

-- ---------------------------------------------------------------------------
-- status

statusTests :: SpecWith ()
statusTests = do
    it "empty tree list produces no status entries" $ do
        let (fa, _) = runFake fsSame (status cx [])
        fa `shouldBe` Completed True []

    it "file missing from target is reported as [MISSING]" $ do
        let (fa, _) = runFake fsStowOnly (status cx treeSingle)
        fa `shouldBe` Completed True ["[MISSING] f"]

    it "file with same content produces no entry" $ do
        let (fa, _) = runFake fsSame (status cx treeSingle)
        fa `shouldBe` Completed True []

    it "file with different content is reported as [DIFFERS]" $ do
        let (fa, _) = runFake fsDiffer (status cx treeSingle)
        fa `shouldBe` Completed True ["[DIFFERS] f"]

    it "nested tree missing from target: all files reported as [MISSING]" $ do
        let (fa, _) = runFake fsNestedStowOnly (status cx treeNested)
        fa `shouldBe` Completed True ["[MISSING] sub/a", "[MISSING] sub/b", "[MISSING] top"]

    it "nested tree identical everywhere: no entries" $ do
        let (fa, _) = runFake fsNestedSame (status cx treeNested)
        fa `shouldBe` Completed True []

    it "nested tree with one differing file: only that file reported" $ do
        let (fa, _) = runFake fsNestedDiffer (status cx treeNested)
        fa `shouldBe` Completed True ["[DIFFERS] sub/a"]

-- ---------------------------------------------------------------------------
-- push

pushTests :: SpecWith ()
pushTests = do
    it "file absent from target is copied there" $ do
        let (fa, finalFs) = runFake fsStowOnly (push cx treeSingle)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "file with the same content in target is left unchanged" $ do
        let (fa, finalFs) = runFake fsSame (push cx treeSingle)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "file with different content in target is overwritten with package version" $ do
        let (fa, finalFs) = runFake fsDiffer (push cx treeSingle)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "stow file is not modified by push" $ do
        let (_, finalFs) = runFake fsStowOnly (push cx treeSingle)
        fileAt [[osp|stow|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "empty tree list leaves filesystem unchanged" $ do
        let (fa, finalFs) = runFake fsSame (push cx [])
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "nested tree: all files including those in subdirs are copied to target" $ do
        let (fa, finalFs) = runFake fsNestedStowOnly (push cx treeNested)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|top|]] finalFs `shouldBe` Just "v1"
        fileAt [[osp|target|], [osp|sub|], [osp|a|]] finalFs `shouldBe` Just "v2"
        fileAt [[osp|target|], [osp|sub|], [osp|b|]] finalFs `shouldBe` Just "v3"

    it "deeply nested tree: file is copied through two directory levels" $ do
        let (fa, finalFs) = runFake fsDeepStowOnly (push cx treeDeep)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|x|], [osp|y|]] finalFs `shouldBe` Just "deep"

-- ---------------------------------------------------------------------------
-- pull

pullTests :: SpecWith ()
pullTests = do
    it "file with the same content in target leaves stow unchanged" $ do
        let (fa, finalFs) = runFake fsSame (pull cx treeSingle)
        fa `shouldBe` Completed True ()
        fileAt [[osp|stow|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "file differing in target overwrites the stow copy" $ do
        let (fa, finalFs) = runFake fsDiffer (pull cx treeSingle)
        fa `shouldBe` Completed True ()
        fileAt [[osp|stow|], [osp|f|]] finalFs `shouldBe` Just "v2"

    it "file missing from target is deleted from stow" $ do
        let (fa, finalFs) = runFake fsStowOnly (pull cx treeSingle)
        fa `shouldBe` Completed True ()
        fileExists [[osp|stow|], [osp|f|]] finalFs `shouldBe` False

    it "target file is not modified by pull" $ do
        let (_, finalFs) = runFake fsDiffer (pull cx treeSingle)
        fileAt [[osp|target|], [osp|f|]] finalFs `shouldBe` Just "v2"

    it "nested tree: differing file in subdirectory is updated from target" $ do
        let (fa, finalFs) = runFake fsNestedDiffer (pull cx treeNested)
        fa `shouldBe` Completed True ()
        fileAt [[osp|stow|], [osp|sub|], [osp|a|]] finalFs `shouldBe` Just "MODIFIED"

    it "nested tree: unchanged files in subdirs are left alone by pull" $ do
        let (_, finalFs) = runFake fsNestedDiffer (pull cx treeNested)
        fileAt [[osp|stow|], [osp|sub|], [osp|b|]] finalFs `shouldBe` Just "v3"
        fileAt [[osp|stow|], [osp|top|]] finalFs `shouldBe` Just "v1"

-- ---------------------------------------------------------------------------
-- delete

deleteTests :: SpecWith ()
deleteTests = do
    it "existing target file is removed" $ do
        let (fa, finalFs) = runFake fsSame (delete cx treeSingle)
        fa `shouldBe` Completed True ()
        fileExists [[osp|target|], [osp|f|]] finalFs `shouldBe` False

    it "absent target file causes no error" $ do
        let (fa, _) = runFake fsStowOnly (delete cx treeSingle)
        fa `shouldBe` Completed True ()

    it "stow file is not touched by delete" $ do
        let (_, finalFs) = runFake fsSame (delete cx treeSingle)
        fileAt [[osp|stow|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "nested tree: files in subdirs are removed from target" $ do
        let (fa, finalFs) = runFake fsNestedSame (delete cx treeNested)
        fa `shouldBe` Completed True ()
        fileExists [[osp|target|], [osp|sub|], [osp|a|]] finalFs `shouldBe` False
        fileExists [[osp|target|], [osp|sub|], [osp|b|]] finalFs `shouldBe` False
        fileExists [[osp|target|], [osp|top|]] finalFs `shouldBe` False

-- ---------------------------------------------------------------------------
-- symlink
--
-- In FakeFsOps, foCreateFileLink = foCopyFileWithMetadata, so symlink
-- is tested as a copy to the target path.

symlinkTests :: SpecWith ()
symlinkTests = do
    it "creates a link at the target path when target is absent" $ do
        let (fa, finalFs) = runFake fsStowOnly (symlink cx treeSingle)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "overwrites an existing target file" $ do
        let (fa, finalFs) = runFake fsDiffer (symlink cx treeSingle)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "stow file is not modified by symlink" $ do
        let (_, finalFs) = runFake fsStowOnly (symlink cx treeSingle)
        fileAt [[osp|stow|], [osp|f|]] finalFs `shouldBe` Just "v1"

    it "nested tree: creates links for all files including those in subdirs" $ do
        let (fa, finalFs) = runFake fsNestedStowOnly (symlink cx treeNested)
        fa `shouldBe` Completed True ()
        fileAt [[osp|target|], [osp|top|]] finalFs `shouldBe` Just "v1"
        fileAt [[osp|target|], [osp|sub|], [osp|a|]] finalFs `shouldBe` Just "v2"
        fileAt [[osp|target|], [osp|sub|], [osp|b|]] finalFs `shouldBe` Just "v3"

-- ---------------------------------------------------------------------------
-- diff

diffTests :: SpecWith ()
diffTests = do
    it "empty tree list returns no pairs" $ do
        let (fa, _) = runFake (Dir M.empty) (diff cx [])
        fa `shouldBe` Completed True []

    it "file missing from target is included as a pair" $ do
        let (fa, _) = runFake fsStowOnly (diff cx treeSingle)
        fa `shouldBe` Completed True
            [( [osp|stow|]   </> [osp|f|]
             , [osp|target|] </> [osp|f|] )]

    it "file with same content produces no pair" $ do
        let (fa, _) = runFake fsSame (diff cx treeSingle)
        fa `shouldBe` Completed True []

    it "file with different content is included as a pair" $ do
        let (fa, _) = runFake fsDiffer (diff cx treeSingle)
        fa `shouldBe` Completed True
            [( [osp|stow|]   </> [osp|f|]
             , [osp|target|] </> [osp|f|] )]

    it "nested tree missing from target: all files included" $ do
        let (fa, _) = runFake fsNestedStowOnly (diff cx treeNested)
        fa `shouldBe` Completed True
            [ ( [osp|stow|]   </> [osp|sub|] </> [osp|a|]
              , [osp|target|] </> [osp|sub|] </> [osp|a|] )
            , ( [osp|stow|]   </> [osp|sub|] </> [osp|b|]
              , [osp|target|] </> [osp|sub|] </> [osp|b|] )
            , ( [osp|stow|]   </> [osp|top|]
              , [osp|target|] </> [osp|top|] )
            ]

    it "nested tree identical everywhere: no pairs" $ do
        let (fa, _) = runFake fsNestedSame (diff cx treeNested)
        fa `shouldBe` Completed True []

    it "nested tree with one differing file: only that pair is returned" $ do
        let (fa, _) = runFake fsNestedDiffer (diff cx treeNested)
        fa `shouldBe` Completed True
            [( [osp|stow|]   </> [osp|sub|] </> [osp|a|]
             , [osp|target|] </> [osp|sub|] </> [osp|a|] )]

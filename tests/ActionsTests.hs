{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module ActionsTests (tests) where

import Control.Monad.State.Strict             (runState)
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Data.Map.Strict              as M
import System.OsPath                          (OsPath, osp, (</>))
import Test.Hspec

import Actions
import AstowMonadT                            (AstowMonadT, runAstowMonadT)
import DirTree                                (DirTree(..), getNode,
                                                mergeRight, mkLine)
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

-- | DirTree containing a single file `f`.
treeSingle :: DirTree String
treeSingle = Dir $ M.singleton [osp|f|] (File "fv1")

-- | DirTree containing a subdirectory.
treeNested :: DirTree String
treeNested = 
    (Dir $ M.fromList
        [ ([osp|top|], File "v1")
        , ([osp|sub|], Dir $ M.fromList
            [ ([osp|a|], File "v2")
            , ([osp|b|], File "v3")
            ])
        ])

-- | 'treeSingle' rooted at "pkg".
srtSingle :: RootedDirTree ()
srtSingle = RootedDirTree [osp|pkg|]
                        ((const ()) <$> treeSingle)

-- | 'treeNested' rooted at "pkgN".
srtNested :: RootedDirTree ()
srtNested = RootedDirTree [osp|pkgN|]
                        ((const ()) <$> treeNested)

-- | Filesystem with both packages in the stow dir and an empty target.
-- Represents the state before any @push@ has been performed.
fsStowOnly :: DirTree String
fsStowOnly = Dir $ M.fromList [
    ([osp|stow|], (Dir $ M.fromList
                    [ ([osp|pkg|], treeSingle)
                    , ([osp|pkgN|], treeNested) ])),
    ([osp|target|], Dir M.empty) ]

-- | Filesystem where @pkgN@ has been fully pushed: stow and target are
-- identical for all of @treeNested@.  @pkg@ is still absent from target.
-- Derived from 'fsStowDifferN' by reverting the modified value back to @"v2"@.
fsStowPushedN :: DirTree String
fsStowPushedN = (\case { "modified" -> "v2"; x -> x }) <$> fsStowDifferN

-- | Filesystem where stow is intact but the target copy of @pkgN@ has drifted:
-- @sub\/a@ in target holds @"modified"@ instead of @"v2"@.
-- Used to test that @push@ overwrites a differing target file.
fsStowDifferN :: DirTree String
fsStowDifferN = Dir $ M.fromList [
    ([osp|stow|], Dir $ M.fromList [ ([osp|pkg|], treeSingle)
                                   , ([osp|pkgN|], treeNested) ])
    , ([osp|target|],  modifier <$> treeNested) ]
    where modifier = \case { "v2" -> "modified"; x -> x }

-- | Filesystem where the stow copy of @pkgN@ has drifted while the target is
-- intact: @sub\/a@ in stow holds @"modified"@ instead of @"v2"@.
-- Used to test that @pull@ overwrites a differing stow file.
fsStowDifferNinStow :: DirTree String
fsStowDifferNinStow = Dir $ M.fromList [
    ([osp|stow|], Dir $ M.fromList [ ([osp|pkg|], treeSingle)
                                   , ([osp|pkgN|],  modifier <$> treeNested) ])
    , ([osp|target|], treeNested) ]
    where modifier = \case { "v2" -> "modified"; x -> x }

-- ---------------------------------------------------------------------------
-- Tests

tests :: SpecWith ()
tests = do
    describe "manifest" manifestTests
    describe "status"   statusTests
    describe "push"     pushTests
    describe "pull"     pullTests
    describe "delete"   deleteTests
    describe "diff"     diffTests

-- ---------------------------------------------------------------------------
-- manifest
--
-- manifest just collects paths from the RootedDirTrees; it never touches
-- the target filesystem, so an empty DirTree suffices as the initial state.

manifestTests :: SpecWith ()
manifestTests = do
    it "empty tree list returns no paths" $ do
        let (fa, _) = runFake fsStowOnly (manifest cx [])
        fa `shouldBe` Completed True []

    it "single file returns its package-relative path as text" $ do
        let (fa, _) = runFake fsStowOnly (manifest cx [srtSingle])
        fa `shouldBe` Completed True ["pkg/f"]

    it "paths from multiple RootedDirTrees are concatenated in order" $ do
        let (fa, _) = runFake fsStowOnly (manifest cx [srtSingle, srtNested])
        fa `shouldBe` Completed True ["pkg/f", "pkgN/sub/a", "pkgN/sub/b",
                                      "pkgN/top"]

    it "nested tree: subdirectory paths are listed with separators in sorted order" $ do
        let (fa, _) = runFake fsStowOnly (manifest cx [srtNested])
        fa `shouldBe` Completed True ["pkgN/sub/a", "pkgN/sub/b", "pkgN/top"]

-- ---------------------------------------------------------------------------
-- status

statusTests :: SpecWith ()
statusTests = do
    it "empty tree list produces no status entries" $ do
        let (fa, _) = runFake fsStowDifferN (status cx [])
        fa `shouldBe` Completed True []

    it "file missing from target is reported as [MISSING]" $ do
        let (fa, _) = runFake fsStowPushedN (status cx [srtSingle])
        fa `shouldBe` Completed True ["[MISSING] pkg/f"]

    it "file with same content produces no entry" $ do
        let (fa, _) = runFake fsStowPushedN (status cx [srtNested])
        fa `shouldBe` Completed True []

    it "file with different content is reported as [DIFFERS]" $ do
        let (fa, _) = runFake fsStowDifferN (status cx [srtNested])
        fa `shouldBe` Completed True ["[DIFFERS] pkgN/sub/a"]

    it "nested tree missing from target: all files reported as [MISSING]" $ do
        let (fa, _) = runFake fsStowOnly (status cx [srtNested])
        fa `shouldBe` Completed True ["[MISSING] pkgN/sub/a",
                                      "[MISSING] pkgN/sub/b",
                                      "[MISSING] pkgN/top"]

    it "nested tree identical everywhere: no entries" $ do
        let (fa, _) = runFake fsStowPushedN (status cx [srtNested])
        fa `shouldBe` Completed True []

-- ---------------------------------------------------------------------------
-- push

pushTests :: SpecWith ()
pushTests = do
    it "files with the same content in target are left unchanged" $ do
        let (fa, finalFs) = runFake fsStowPushedN (push False cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

    it "file with different content in target is overwritten with package version" $ do
        let (fa, finalFs) = runFake fsStowDifferN (push False cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

    it "empty tree list leaves filesystem unchanged" $ do
        let (fa, finalFs) = runFake fsStowDifferN (push False cx [])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowDifferN

    it "nested tree: all files including those in subdirs are copied to target" $ do
        let (fa, finalFs) = runFake fsStowOnly (push False cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

    it "symlink: creates a link at the target path when target is absent" $ do
        let (fa, finalFs) = runFake fsStowOnly (push True cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

    it "symlink: overwrites existing target files" $ do
        let (fa, finalFs) = runFake fsStowDifferN (push True cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

-- ---------------------------------------------------------------------------
-- pull

pullTests :: SpecWith ()
pullTests = do
    it "file with the same content in target leaves stow unchanged" $ do
        let (fa, finalFs) = runFake fsStowPushedN (pull cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

    it "file differing in target overwrites the stow copy" $ do
        let (fa, finalFs) = runFake fsStowDifferNinStow (pull cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

    it "file missing from target is deleted from stow" $ do
        let (fa, finalFs) = runFake fsStowPushedN (pull cx [srtSingle])
        fa `shouldBe` Completed True ()
        fileExists [[osp|stow|], [osp|pkg|], [osp|f|]] finalFs `shouldBe` False

-- ---------------------------------------------------------------------------
-- delete

deleteTests :: SpecWith ()
deleteTests = do
    it "target files are removed & stow dir remains same" $ do
        let (fa, finalFs) = runFake fsStowPushedN (delete cx [srtNested])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` (fsStowOnly
            `mergeRight` (mkLine [ [osp|target|], [osp|sub|] ] (Dir M.empty)))

    it "absent target file causes no error" $ do
        let (fa, finalFs) = runFake fsStowPushedN (delete cx [srtSingle])
        fa `shouldBe` Completed True ()
        finalFs `shouldBe` fsStowPushedN

-- ---------------------------------------------------------------------------
-- diff

diffTests :: SpecWith ()
diffTests = do
    it "empty tree list returns no pairs" $ do
        let (fa, _) = runFake fsStowDifferN (diff cx [])
        fa `shouldBe` Completed True []

    it "file missing from target is included as a pair" $ do
        let (fa, _) = runFake fsStowOnly (diff cx [srtSingle])
        fa `shouldBe` Completed True
            [( [osp|stow|]   </> [osp|pkg|] </> [osp|f|]
             , [osp|target|] </> [osp|f|] )]

    it "files with same content produce no pairs" $ do
        let (fa, _) = runFake fsStowPushedN (diff cx [srtNested])
        fa `shouldBe` Completed True []

    it "file with different content is included as a pair" $ do
        let (fa, _) = runFake fsStowDifferN (diff cx [srtNested])
        fa `shouldBe` Completed True
            [( [osp|stow|]   </> [osp|pkgN|] </> [osp|sub|] </> [osp|a|]
             , [osp|target|] </> [osp|sub|] </> [osp|a|] )]

    it "nested tree missing from target: all files included" $ do
        let (fa, _) = runFake fsStowOnly (diff cx [srtNested])
        fa `shouldBe` Completed True
            [ ( [osp|stow|]   </> [osp|pkgN|] </> [osp|sub|] </> [osp|a|]
              , [osp|target|] </> [osp|sub|] </> [osp|a|] )
            , ( [osp|stow|]   </> [osp|pkgN|] </> [osp|sub|] </> [osp|b|]
              , [osp|target|] </> [osp|sub|] </> [osp|b|] )
            , ( [osp|stow|]   </> [osp|pkgN|] </> [osp|top|]
              , [osp|target|] </> [osp|top|] )
            ]

    it "nested tree identical everywhere: no pairs" $ do
        let (fa, _) = runFake fsStowPushedN (diff cx [srtNested])
        fa `shouldBe` Completed True []

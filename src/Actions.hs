module Actions (
    RootedDirTree(..)
  , ActionContext(..)

-- * actions to perform
  , status
  , statusIO
  , push
  , pushIO
  , pull
  , pullIO
  , delete
  , deleteIO
  , manifest
  , manifestIO
  , diff
  , diffIO
) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import System.Process                       (rawSystem)

import AstowMonadT
import DirTree
import FsOps
import FileUtils (osPathToString, osPathToText)

import System.OsPath (OsPath, (</>), takeDirectory)

-- | DirTree together with a root path.
data RootedDirTree a = RootedDirTree {
        rdtRoot         :: OsPath
      , rdtTree         :: DirTree a
    }

data ActionContext = ActionContext {
        acStowDir       :: OsPath
      , acTargetDir     :: OsPath
    }

-- | Compare target to stow directory.
--
-- Visits all nodes checking both sides.
--
-- Reports @[MISSING]@, @[UNKNOWN]@, or @[DIFFERS]@ as appropriate.
status :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m [ T.Text ]
status cx = visitFiles checker
    where   checker :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m [T.Text]
            checker sp tgtP = do
                let sp'   = acStowDir cx </> sp
                    tgtP' = acTargetDir cx </> tgtP
                bs <- foDoesFileExist sp'
                bt <- foDoesFileExist tgtP'
                case (bs, bt) of
                    (True,  False) -> return ["[MISSING] " <> osPathToText sp]
                    (False, True)  -> return ["[UNKNOWN] " <> osPathToText sp]
                    (True,  True)  -> do same <- foFilesHaveSameContent sp' tgtP'
                                         return $ if same then [] else ["[DIFFERS] " <> osPathToText sp]
                    (False, False) -> return []

-- | Status with IO.
statusIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
statusIO cx ts = do
    out <- status cx ts
    forM_ out (\l ->
        liftIO $ TIO.putStrLn l)

-- | Push files from the stow directory into the target directory.
--
--   When @useSymlink@ is @False@, files are copied: missing files are created,
--   changed files are overwritten, unchanged files are left alone.
--
--   When @useSymlink@ is @True@, symlinks pointing to the stow file are
--   created in the target (unconditionally, replacing any existing entry).
push :: (Monad m, FsOps m)
    => Bool             -- ^ use symlinks instead of copying
    -> ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
push useSymlink cx = visitFiles pusher
    where   pusher :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            pusher sp tgtP = do
                let sp' = acStowDir cx </> sp
                    tgtP' = acTargetDir cx </> tgtP
                foCreateDirectoryIfMissing True $
                    takeDirectory tgtP'
                if useSymlink
                    then foCreateFileLink sp' tgtP'
                    else do
                        e <- foDoesFileExist tgtP'
                        doCopy <- if not e then
                                return True
                            else do
                                same <- foFilesHaveSameContent sp' tgtP'
                                return (not same)
                        when doCopy $
                            foCopyFileWithMetadata sp' tgtP'

pushIO :: (Monad m, FsOps m, MonadIO m)
    => Bool
    -> ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
pushIO = push

-- | Pull (copy) changed files from the target directory into the stow directory.
--
--   Files that differ in the target from the stow directory are overwritten in
--   the stow directory to match the target version.  Files in the stow directory
--   but missing from the target are deleted from the stow directory.
--
--   When a tree entry has no counterpart in the stow directory (e.g. because
--   it was sourced from the target by the caller), the file is created in the
--   stow directory, including any intermediate directories.
--
--   Note that files newly added to the target cannot be detected automatically,
--   as the manifest of files to consider comes from the caller.  They can be
--   captured by specifying them explicitly on the command line.
pull :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
pull cx = visitFiles puller
    where   puller :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            puller sp tgtP = do
                let sp' = acStowDir cx </> sp
                    tgtP' = acTargetDir cx </> tgtP
                tgtExists <- foDoesFileExist tgtP'
                if not tgtExists
                    then do
                        -- Target gone: remove stow copy if present.
                        stowExists <- foDoesFileExist sp'
                        when stowExists $ foRemoveFile sp'
                    else do
                        -- Target exists: copy to stow if absent or differs.
                        stowExists <- foDoesFileExist sp'
                        doCopy <- if stowExists
                            then not <$> foFilesHaveSameContent sp' tgtP'
                            else return True
                        when doCopy $ do
                            foCreateDirectoryIfMissing True (takeDirectory sp')
                            foCopyFileWithMetadata tgtP' sp'

-- | Alias for pull in IO.
pullIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
pullIO = pull

-- | Remove files from the target directory.
--
--   Files that exist in the stow directory are deleted from the target.
delete :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
delete cx = visitFiles deleter
    where   deleter :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            deleter _ tgtP = do
                let tgtP' = acTargetDir cx </> tgtP
                doDelete <- foDoesFileExist tgtP'
                when doDelete $
                    foRemoveFile tgtP'

deleteIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
deleteIO = delete

-- | Display a list of all files in a subpath.
manifest :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m [ T.Text ]
manifest cx = visitFiles collector
    where   collector sp _ = do
                let sp' = acStowDir cx </> sp
                e <- foDoesFileExist sp'
                return $ if e
                    then [ osPathToText sp ]
                    else [ ]

manifestIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
manifestIO cx ts = do
    m <- manifest cx ts
    forM_ m (\l ->
        liftIO $ TIO.putStrLn l)

-- | Compare stow directory to target, returning pairs of absolute paths
-- @(stow, target)@ for files that are missing from the target or differ.
--
-- Files with identical content are omitted.  A missing target file is
-- included with its expected (non-existent) path so that @diff -uN@
-- will treat it as empty.
diff :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m [(OsPath, OsPath)]
diff cx = visitFiles checker
    where   checker sp tgtP = do
                let sp' = acStowDir cx </> sp
                    tgtP' = acTargetDir cx </> tgtP
                e <- foDoesFileExist tgtP'
                if not e then
                    return [(sp', tgtP')]
                else do
                    same <- foFilesHaveSameContent sp' tgtP'
                    return $ if same then [] else [(sp', tgtP')]

-- | Diff with IO: runs @diff -uN target stow@ for each differing file.
--
-- The exit code from @diff@ is ignored since 1 (files differ) is the
-- normal case here.  Output goes directly to stdout.
diffIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
diffIO cx ts = do
    pairs <- diff cx ts
    forM_ pairs $ \(sp', tgtP') ->
        liftIO $ void $ rawSystem "diff"
            ["-uN", osPathToString tgtP', osPathToString sp']

-- Internal helper functions.

-- | Visit each file and execute an action on it.
--
--   The visitor function takes two paths, one for the file in the stow
--   directory, and the other one for the corresponding file in the target.
--
--   Only proper files are visited, not directories.
visitFilesSingle :: (Monoid a, Monad m)
    => (OsPath -> OsPath -> m a)            -- ^ visitor (action)
    -> RootedDirTree ()                     -- ^ the tree to visit
    -> m a
visitFilesSingle f rdt = do
    let tr = rdtTree rdt
        root = rdtRoot rdt
    mconcat <$> walkM mempty (\p () -> f (root </> p) p) tr

-- | Visit each file in multiple rooted trees.
--
-- Variant of 'visitFilesSingle' that takes a list of trees instead.
visitFiles :: (Monoid a, Monad m)
    => (OsPath -> OsPath -> m a)         -- ^ visitor (action)
    -> [RootedDirTree ()]                   -- ^ the trees
    -> m a
visitFiles f trees = mconcat <$> mapM (visitFilesSingle f) trees 

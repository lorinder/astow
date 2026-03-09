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
  , symlink
  , symlinkIO
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
status :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m [ T.Text ]
status cx = visitFiles checker
    where   checker :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m [ T.Text ]
            checker sp tgtP = do
                let sp' = acStowDir cx </> sp
                    tgtP' = acTargetDir cx </> tgtP
                e <- foDoesFileExist tgtP'
                if not e then do
                    return [ "[MISSING] " <> osPathToText sp ]
                else do
                    same <- foFilesHaveSameContent sp' tgtP'
                    return $ if same
                        then []
                        else [ "[DIFFERS] " <> osPathToText sp ]
                    
-- | Status with IO.
statusIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
statusIO cx ts = do
    out <- status cx ts
    forM_ out (\l ->
        liftIO $ TIO.putStrLn l)

-- | Push (copy) files from the stow directory into the target directory.
--
--   Files in the stow directory missing in the target are copied into the target;
--   files different in the target are overwritten with the updated
--   version.  Unchanged files are left alone.
push :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
push cx = visitFiles pusher
    where   pusher :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            pusher sp tgtP = do
                let sp' = acStowDir cx </> sp
                    tgtP' = acTargetDir cx </> tgtP
                e <- foDoesFileExist tgtP'
                doCopy <- if not e then
                        return True
                    else do
                        same <- foFilesHaveSameContent sp' tgtP'
                        return (not same)
                when doCopy $ do
                    foCreateDirectoryIfMissing True $
                        takeDirectory tgtP'
                    foCopyFileWithMetadata sp' tgtP'

pushIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
pushIO = push

-- | Pull (copy) changed files from the target directory into the stow directory.
--
--   Files that differ in the target from the stow directory are overwritten in
--   the stow directory to match the target version.  Files in the stow directory
--   but missing from the target are deleted from the stow directory.
--
--   Note that files newly added to the target can never be detected in
--   this way, as the manifest of files to consider is in the stow directory.
--   Thus newly added files cannot be pulled automatically but need to be
--   manually copied into the stow directory.
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
                doDelete <- not <$> foDoesFileExist tgtP'
                doCopy <- if doDelete then
                        return False
                    else do
                        not <$> foFilesHaveSameContent sp' tgtP'
                if doDelete then do
                    foRemoveFile sp'
                else when doCopy $ do
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

-- | Symlink files in the target directory to the stow directory.
symlink :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
symlink cx = visitFiles linker
    where   linker :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            linker sp tgtP = do
                let sp' = acStowDir cx </> sp
                    tgtP' = acTargetDir cx </> tgtP
                foCreateDirectoryIfMissing True $
                    takeDirectory tgtP'
                foCreateFileLink sp' tgtP'

symlinkIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
symlinkIO = symlink
    
-- | Display a list of all files in a subpath.
manifest :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m [ T.Text ]
manifest _ = visitFiles collector
    where   collector sp _ = return [ osPathToText sp ]

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

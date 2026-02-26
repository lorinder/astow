module Actions (
    RootedDirTree(..)
  , ActionContext(..)

-- * actions to perform
  , status
  , push
  , pull
  , delete
  , symlink
  , manifest
) where

import Control.Monad
import Control.Monad.IO.Class

import AstowMonadT
import DirTree
import FsOps
import FileUtils (osPathToString)

import System.OsPath (OsPath, (</>), takeDirectory)

-- | DirTree together with a rooting path.
data RootedDirTree a = RootedDirTree {
        rdtRoot         :: OsPath
      , rdtTree         :: DirTree a
    }

data ActionContext = ActionContext {
        acStowDir       :: OsPath
      , acLiveDir       :: OsPath
    }

-- | Compare production to staging.
status :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
status cx = visitFiles checker
    where   checker :: (Monad m, FsOps m, MonadIO m)
                => OsPath -> OsPath -> AstowMonadT m ()
            checker sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                e <- foDoesFileExist pp'
                if not e then do
                    liftIO $ putStrLn $ "[MISSING] " ++ osPathToString sp
                else do
                    same <- foFilesHaveSameContent sp' pp'
                    unless same $ do
                        liftIO $ putStrLn $ "[DIFFERS] " ++ osPathToString sp


-- | Push (copy) files from staging into production.
--
--   Files in staging missing in production are copied into production;
--   files different in production are overwritten with the updated
--   version.  Unchanged files are left alone.
push :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
push cx = visitFiles pusher
    where   pusher :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            pusher sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                e <- foDoesFileExist pp'
                doCopy <- if not e then
                        return True
                    else do
                        same <- foFilesHaveSameContent sp' pp'
                        return (not same)
                when doCopy $ do
                    foCreateDirectoryIfMissing True $
                        takeDirectory pp'
                    foCopyFileWithMetadata sp' pp'

-- | Pull (copy) changed files from production into staging.
--
--   Files that differ in production from staging are overwritten in
--   staging to match the production version.  Files existant in staging
--   but missing from production are deleted in staging.
--
--   Note that files that have been newly added to production can never
--   be detected in this way; as the manifest of files to considers is
--   in the staging area.  Thus newly added files cannot be pulled
--   automatically but need to be manually copied into staging.
pull :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
pull cx = visitFiles puller
    where   puller :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            puller sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                doDelete <- not <$> foDoesFileExist pp'
                doCopy <- if doDelete then
                        return False
                    else do
                        not <$> foFilesHaveSameContent sp' pp'
                if doDelete then do
                    foRemoveFile sp'
                else when doCopy $ do
                    foCopyFileWithMetadata pp' sp'

-- | Unstow, i.e., remove files from production.
--
--   Files that exist in staging are deleted from production.
delete :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
delete cx = visitFiles deleter
    where   deleter :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            deleter _ pp = do
                let pp' = acLiveDir cx </> pp
                doDelete <- foDoesFileExist pp'
                when doDelete $
                    foRemoveFile pp'

-- | Symlink files in production to staging.
symlink :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
symlink cx = visitFiles linker
    where   linker :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m ()
            linker sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                foCreateDirectoryIfMissing True $
                    takeDirectory pp'
                foCreateFileLink sp' pp'
    
-- | Display a list of all files in a subpath.
manifest :: (Monad m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
manifest _ = visitFiles printer
    where   printer :: (Monad m, MonadIO m)
                => OsPath -> OsPath -> AstowMonadT m ()
            printer sp _ = liftIO $ print sp

-- Internal helper functions.

-- | Visit each file and execute an action on it.
--
--   The visitor function takes two paths, one for the file in staging,
--   and the other one for the corresponding file in production.
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

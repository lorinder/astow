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
    -> m Bool
status cx = visitFiles checker
    where   checker :: (Monad m, FsOps m, MonadIO m) => OsPath -> OsPath -> m Bool
            checker sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                e <- doesFileExist pp'
                if not e then do
                    liftIO $ putStrLn $ "[MISSING] " ++ osPathToString sp
                else do
                    same <- filesHaveSameContent sp' pp'
                    unless same $ do
                        liftIO $ putStrLn $ "[DIFFERS] " ++ osPathToString sp
                return True


-- | Push (copy) files from staging into production.
--
--   Files in staging missing in production are copied into production;
--   files different in production are overwritten with the updated
--   version.  Unchanged files are left alone.
push :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> m Bool
push cx = visitFiles pusher
    where   pusher :: (Monad m, FsOps m) => OsPath -> OsPath -> m Bool
            pusher sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                e <- doesFileExist pp'
                doCopy <- if not e then
                        return True
                    else do
                        same <- filesHaveSameContent sp' pp'
                        return (not same)
                when doCopy $ do
                    createDirectoryIfMissing True $
                        takeDirectory pp'
                    copyFileWithMetadata sp' pp'
                return True

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
    -> m Bool
pull cx = visitFiles puller
    where   puller :: (Monad m, FsOps m) => OsPath -> OsPath -> m Bool
            puller sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                doDelete <- not <$> doesFileExist pp'
                doCopy <- if doDelete then
                        return False
                    else do
                        not <$> filesHaveSameContent sp' pp'
                if doDelete then do
                    removeFile sp'
                else when doCopy $ do
                    copyFileWithMetadata pp' sp'
                return True

-- | Unstow, i.e., remove files from production.
--
--   Files that exist in staging are deleted from production.
delete :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> m Bool
delete cx = visitFiles deleter
    where   deleter :: (Monad m, FsOps m) => OsPath -> OsPath -> m Bool
            deleter _ pp = do
                let pp' = acLiveDir cx </> pp
                doDelete <- doesFileExist pp'
                when doDelete $
                    removeFile pp'
                return True

-- | Symlink files in production to staging.
symlink :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> m Bool
symlink cx = visitFiles linker
    where   linker :: (Monad m, FsOps m) => OsPath -> OsPath -> m Bool
            linker sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                createDirectoryIfMissing True $
                    takeDirectory pp'
                createFileLink sp' pp'
                return True
    
-- | Display a list of all files in a subpath.
manifest :: (Monad m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> m Bool
manifest _ = visitFiles printer
    where   printer :: (Monad m, MonadIO m) => OsPath -> OsPath -> m Bool
            printer sp _ = do
                liftIO $ print sp
                return True

-- Internal helper functions.

-- | Visit each file and execute an action on it.
--
--   The visitor function takes two paths, one for the file in staging,
--   and the other one for the corresponding file in production.
--
--   Only proper files are visited, not directories.
visitFilesSingle :: (Monad m)
    => (OsPath -> OsPath -> m Bool)         -- ^ visitor (action)
    -> RootedDirTree ()                     -- ^ the tree to visit
    -> m Bool
visitFilesSingle f rdt = do
    let tr = rdtTree rdt
        root = rdtRoot rdt
    r <- walkM mempty (\p () -> f (root </> p) p) tr
    return $ all id r

-- | Visit each file in multiple rooted trees.
--
-- Variant of 'visitFilesSingle' that takes a list of trees instead.
visitFiles :: (Monad m)
    => (OsPath -> OsPath -> m Bool)         -- ^ visitor (action)
    -> [RootedDirTree ()]                   -- ^ the trees
    -> m Bool
visitFiles f trees = do
    r <- mapM (visitFilesSingle f) trees 
    return $ all id r

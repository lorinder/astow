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
) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import AstowMonadT
import DirTree
import FsOps
import FileUtils (osPathToText)

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
status :: (Monad m, FsOps m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m [ T.Text ]
status cx = visitFiles checker
    where   checker :: (Monad m, FsOps m)
                => OsPath -> OsPath -> AstowMonadT m [ T.Text ]
            checker sp pp = do
                let sp' = acStowDir cx </> sp
                    pp' = acLiveDir cx </> pp
                e <- foDoesFileExist pp'
                if not e then do
                    return [ "[MISSING] " <> osPathToText sp ]
                else do
                    same <- foFilesHaveSameContent sp' pp'
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

pushIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
pushIO = push

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

-- | Alias for pull in IO.
pullIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
pullIO = pull

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

deleteIO :: (Monad m, FsOps m, MonadIO m)
    => ActionContext
    -> [RootedDirTree ()]
    -> AstowMonadT m ()
deleteIO = delete

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

{-# LANGUAGE QuasiQuotes #-}

module Actions (
    status
  , push
  , pull
  , delete
  , symlink
) where

import Control.Monad
import System.Directory.OsPath

import DirTree
import FileUtils

import System.OsPath (OsPath, (</>), osp, takeDirectory)

-- | Compare production to staging.
status
    :: OsPath       -- ^ staging base path
    -> DirTree ()   -- ^ staging tree
    -> IO Bool
status = visitFiles checker
    where   checker :: OsPath -> OsPath -> IO Bool
            checker sp pp = do
                e <- doesFileExist pp
                if not e then do
                    putStrLn $ "[MISSING] " ++ osPathToString pp
                else do
                    same <- filesHaveSameContent sp pp
                    unless same $ do
                        putStrLn $ "[DIFFERS] " ++ osPathToString pp
                return True


-- | Push (copy) files from staging into production.
--
--   Files in staging missing in production are copied into production;
--   files different in production are overwritten with the updated
--   version.  Unchanged files are left alone.
push
    :: OsPath       -- ^ staging base path
    -> DirTree ()   -- ^ staging tree
    -> IO Bool
push = visitFiles pusher 
    where   pusher :: OsPath -> OsPath -> IO Bool
            pusher sp pp = do
                e <- doesFileExist pp
                doCopy <- if not e then
                        return True
                    else do
                        same <- filesHaveSameContent sp pp
                        return (not same)
                when doCopy $ do
                    createDirectoryIfMissing True $
                        takeDirectory pp
                    copyFileWithMetadata sp pp
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
pull
    :: OsPath       -- ^ staging base path
    -> DirTree ()   -- ^ staging tree
    -> IO Bool
pull = visitFiles puller 
    where   puller :: OsPath -> OsPath -> IO Bool
            puller sp pp = do
                doDelete <- not <$> doesFileExist pp
                doCopy <- if doDelete then
                        return False
                    else do
                        not <$> filesHaveSameContent sp pp
                if doDelete then do 
                    removeFile sp
                else when doCopy $ do
                    copyFileWithMetadata pp sp
                return True

-- | Unstow, i.e., remove files from production.
--
--   Files that exist in staging are deleted from production.
delete
    :: OsPath       -- ^ staging base path
    -> DirTree ()
    -> IO Bool
delete = visitFiles deleter
    where   deleter :: OsPath -> OsPath -> IO Bool
            deleter _ pp = do
                doDelete <- doesFileExist pp
                when doDelete $
                    removeFile pp
                return True

-- | Symlink files in production to stagin
symlink
    :: OsPath       -- ^ staging base path
    -> DirTree ()
    -> IO Bool
symlink = visitFiles linker
    where   linker :: OsPath -> OsPath -> IO Bool
            linker sp pp = do
                createDirectoryIfMissing True $
                    takeDirectory pp
                spAbs <- makeAbsolute sp
                createFileLink spAbs pp
                return True
    

-- Internal helper functions.

-- | Visit each file and execute an action on it.
--
--   The visitor function takes two paths, one for the file in staging,
--   and the other one for the corresponding file in production.
--
--   Only proper files are visited, not directories.
visitFiles
    :: (OsPath -> OsPath -> IO Bool)        -- ^ visitor (action)
    -> OsPath                               -- ^ staging base path
    -> DirTree ()                           -- ^ the tree
    -> IO Bool
visitFiles f base tr = do
    r <- walk mempty (\p _ -> f (base </> p) ([osp|..|] </> p)) tr
    return $ any not r

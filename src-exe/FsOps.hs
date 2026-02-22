{-|
Module      : FsOps
Description : File System Operations
Copyright   : (c) Lorenz Minder, 2026
License     : BSD
Maintainer  : lminder@gmx.net
Stability   : experimental
Portability : POSIX

The 'FsOps' typeclass provides monadic file system access functions
needed by astow.  The purpose is to be able to pick the normal IO
backend for production use, and use other backends for testing and
debugging.

-}
module FsOps (
    FsOps(..)
  , LoggedFsOpsT(..)
) where

import Control.Monad.IO.Class

import qualified Data.ByteString            as B 
import qualified System.Directory.OsPath    as D
import System.IO                            (IOMode(ReadMode),hPutStrLn,stderr)
import System.OsPath                        (OsPath)
import System.File.OsPath                   (withBinaryFile)

-- | Monad with file system operations.
class FsOps m where
    filesHaveSameContent :: OsPath -> OsPath -> m Bool
    listDirectory :: OsPath -> m [OsPath]
    doesDirectoryExist :: OsPath -> m Bool
    doesFileExist :: OsPath -> m Bool
    createDirectoryIfMissing :: Bool -> OsPath -> m ()
    copyFileWithMetadata :: OsPath -> OsPath -> m ()
    removeFile :: OsPath -> m ()
    createFileLink :: OsPath -> OsPath -> m ()

instance FsOps IO where
    filesHaveSameContent a b = do
        withBinaryFile a ReadMode (\ha -> do
            withBinaryFile b ReadMode (\hb -> do
                checkMatch ha hb
                )
            )
        where   checkMatch ha hb = do
                    let blockSz = 65536
                    ba <- B.hGet ha blockSz
                    bb <- B.hGet hb blockSz
                    if ba /= bb then
                        return False
                    else if ba == B.empty then
                        -- EOF reached without finding a difference
                        return True
                    else
                        -- keep checking
                        checkMatch ha hb
    listDirectory = D.listDirectory
    doesDirectoryExist = D.doesDirectoryExist
    doesFileExist = D.doesFileExist
    createDirectoryIfMissing = D.createDirectoryIfMissing
    copyFileWithMetadata = D.copyFileWithMetadata
    removeFile = D.removeFile
    createFileLink = D.createFileLink

-- | Transformer to log file system operations.
newtype LoggedFsOpsT m a = LoggedFsOpsT { runLoggedFsOpsT :: m a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance (FsOps m, MonadIO m) => FsOps (LoggedFsOpsT m) where
    filesHaveSameContent a b = logFsOp
        ("filesHaveSameContent " ++ show a ++ " " ++ show b)
        (filesHaveSameContent a b)
    listDirectory a = logFsOp
        ("listDirectory " ++ show a)
        (listDirectory a)
    doesDirectoryExist a = logFsOp
        ("doesDirectoryExist " ++ show a)
        (doesDirectoryExist a)
    doesFileExist a = logFsOp
        ("doesFileExist " ++ show a)
        (doesFileExist a)
    createDirectoryIfMissing a b = logFsOp
        ("createDirectoryIfMissing " ++ show a ++ " " ++ show b)
        (createDirectoryIfMissing a b)
    copyFileWithMetadata a b = logFsOp
        ("copyFileWithMetadata " ++ show a ++ " " ++ show b)
        (copyFileWithMetadata a b)
    removeFile a = logFsOp
        ("removeFile " ++ show a)
        (removeFile a)
    createFileLink a b = logFsOp
        ("createFileLink " ++ show a ++ " " ++ show b)
        (createFileLink a b)

logFsOp :: (MonadIO m, FsOps m)
    => String
    -> m a
    -> LoggedFsOpsT m a
logFsOp msg action = LoggedFsOpsT (do
    liftIO $ hPutStrLn stderr msg
    r <- action
    return r)

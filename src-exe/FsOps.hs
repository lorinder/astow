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
-- * Typeclass
    FsOps(..)

-- * Instances
  , LoggedFsOpsT(..)
) where

import Control.Monad.IO.Class
import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString                    as B
import qualified System.Directory.OsPath            as D
import System.IO                                    (IOMode(ReadMode),
                                                     hPutStrLn,stderr)
import System.OsPath                                (OsPath)
import System.File.OsPath                           (withBinaryFile)

import AstowMonadT
import Fallible
import KissDList                                    (singleton)
import Diagnostic                                   (Diagnostic(..),
                                                     Payload(..), Severity(..))


-- | Monad with file system operations.
class FsOps m where
    foFilesHaveSameContent :: OsPath -> OsPath -> AstowMonadT m Bool
    foListDirectory :: OsPath -> AstowMonadT m [OsPath]
    foDoesDirectoryExist :: OsPath -> AstowMonadT m Bool
    foDoesFileExist :: OsPath -> AstowMonadT m Bool
    foCreateDirectoryIfMissing :: Bool -> OsPath -> AstowMonadT m ()
    foCopyFileWithMetadata :: OsPath -> OsPath -> AstowMonadT m ()
    foRemoveFile :: OsPath -> AstowMonadT m ()
    foCreateFileLink :: OsPath -> OsPath -> AstowMonadT m ()

instance FsOps IO where
    foFilesHaveSameContent a b =
        wrapIOAction ("Comparing files " ++ show a ++ " " ++ show b) (do
            withBinaryFile a ReadMode (\ha -> do
                withBinaryFile b ReadMode (\hb -> do
                    checkMatch ha hb
                    )
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
    foListDirectory d = wrapIOAction
        ("Reading directory " ++ show d)
        (D.listDirectory d)
    foDoesDirectoryExist d = wrapIOAction
        ("Checking for directory " ++ show d)
        (D.doesDirectoryExist d)
    foDoesFileExist p = wrapIOAction
        ("Checking for file " ++ show p)
        (D.doesFileExist p)
    foCreateDirectoryIfMissing e d = wrapIOAction
        ("Creating directory " ++ show d)
        (D.createDirectoryIfMissing e d)
    foCopyFileWithMetadata a b = wrapIOAction
        ("Copying file " ++ show a ++ " -> " ++ show b)
        (D.copyFileWithMetadata a b)
    foRemoveFile p = wrapIOAction
        ("Removing file " ++ show p)
        (D.removeFile p)
    foCreateFileLink a b = wrapIOAction
        ("Symlinking file " ++ show b ++ " -> " ++ show a)
        (D.createFileLink a b)

-- | Wrap an IO action in `FsOpMonadT IO`.
--
-- Catch exceptions; if errors occur, report the error, and abort.
wrapIOAction
    :: String               -- ^ error "when" message
    -> IO a                 -- ^ action to wrap
    -> AstowMonadT IO a     -- ^ resulting wrapped action
wrapIOAction msgWhen act = do
    r <- liftIO $ (Right <$> act)
                    `catch` \e -> return $ Left (e :: IOException)
    case r of
        Left e -> do
            let diag = Diagnostic msgWhen (IOPayload e) Error
            tell $ singleton diag
            abort
        Right v -> return v

-- | Transformer to log file system operations.
newtype LoggedFsOpsT m a = LoggedFsOpsT { runLoggedFsOpsT :: m a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance (FsOps m, MonadIO m) => FsOps (LoggedFsOpsT m) where
    foFilesHaveSameContent a b = logFsOp
        ("filesHaveSameContent " ++ show a ++ " " ++ show b)
        (foFilesHaveSameContent a b)
    foListDirectory a = logFsOp
        ("listDirectory " ++ show a)
        (foListDirectory a)
    foDoesDirectoryExist a = logFsOp
        ("doesDirectoryExist " ++ show a)
        (foDoesDirectoryExist a)
    foDoesFileExist a = logFsOp
        ("doesFileExist " ++ show a)
        (foDoesFileExist a)
    foCreateDirectoryIfMissing a b = logFsOp
        ("createDirectoryIfMissing " ++ show a ++ " " ++ show b)
        (foCreateDirectoryIfMissing a b)
    foCopyFileWithMetadata a b = logFsOp
        ("copyFileWithMetadata " ++ show a ++ " " ++ show b)
        (foCopyFileWithMetadata a b)
    foRemoveFile a = logFsOp
        ("removeFile " ++ show a)
        (foRemoveFile a)
    foCreateFileLink a b = logFsOp
        ("createFileLink " ++ show a ++ " " ++ show b)
        (foCreateFileLink a b)

logFsOp :: (MonadIO m, FsOps m)
    => String
    -> AstowMonadT m a
    -> AstowMonadT (LoggedFsOpsT m) a
logFsOp msg action = do
    liftIO $ hPutStrLn stderr msg
    (r, diag) <- lift $ lift $ (LoggedFsOpsT $ runAstowMonadT action)
    tell diag
    FallibleT $ pure r
{-
    case r of
        Aborted -> abort
        Completed False x -> invalid x
        Completed True x -> return x
-}

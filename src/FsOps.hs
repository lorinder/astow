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
import qualified Data.Text                          as T
import Data.Text.IO                                 (hPutStrLn)
import qualified Control.Monad.Trans.Writer.Strict  as W
import qualified Data.ByteString                    as B
import qualified System.Directory.OsPath            as D
import System.IO                                    (IOMode(ReadMode), stderr)
import System.OsPath                                (OsPath)
import System.File.OsPath                           (withBinaryFile)

import AstowMonadT
import Fallible
import FileUtils
import KissDList                                    (KissDList)
import qualified Diagnostic                         as Di


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
        wrapIOAction
            ("Comparing files " <> osPathToText a <> " " <> osPathToText b) (do
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
        ("Reading directory " <> osPathToText d)
        (D.listDirectory d)
    foDoesDirectoryExist d = wrapIOAction
        ("Checking for directory " <> osPathToText d)
        (D.doesDirectoryExist d)
    foDoesFileExist p = wrapIOAction
        ("Checking for file " <> osPathToText p)
        (D.doesFileExist p)
    foCreateDirectoryIfMissing e d = wrapIOAction
        ("Creating directory " <> osPathToText d)
        (D.createDirectoryIfMissing e d)
    foCopyFileWithMetadata a b = wrapIOAction
        ("Copying file " <> osPathToText a <> " -> " <> osPathToText b)
        (D.copyFileWithMetadata a b)
    foRemoveFile p = wrapIOAction
        ("Removing file " <> osPathToText p)
        (D.removeFile p)
    foCreateFileLink a b = wrapIOAction
        ("Symlinking file " <> osPathToText b <> " -> " <> osPathToText a)
        (D.createFileLink a b)

-- | Wrap an IO action in `FsOpMonadT IO`.
--
-- Catch exceptions; if errors occur, report the error, and abort.
wrapIOAction
    :: T.Text               -- ^ error "when" message
    -> IO a                 -- ^ action to wrap
    -> AstowMonadT IO a     -- ^ resulting wrapped action
wrapIOAction msgWhen act = do
    r <- liftIO $ (Right <$> act)
                    `catch` \e -> return $ Left (e :: IOException)
    case r of
        Left e -> do
            tell1 $ Di.Diagnostic msgWhen (Di.IOPayload e) Di.Error
            abort
        Right v -> return v

-- | Transformer to log file system operations.
newtype LoggedFsOpsT m a = LoggedFsOpsT { runLoggedFsOpsT :: m a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance (FsOps m, MonadIO m) => FsOps (LoggedFsOpsT m) where
    foFilesHaveSameContent a b = logFsOp
        ("filesHaveSameContent " <> osPathToText a <> " " <> osPathToText b)
        (foFilesHaveSameContent a b)
    foListDirectory a = logFsOp
        ("listDirectory " <> osPathToText a)
        (foListDirectory a)
    foDoesDirectoryExist a = logFsOp
        ("doesDirectoryExist " <> osPathToText a)
        (foDoesDirectoryExist a)
    foDoesFileExist a = logFsOp
        ("doesFileExist " <> osPathToText a)
        (foDoesFileExist a)
    foCreateDirectoryIfMissing a b = logFsOp
        ("createDirectoryIfMissing " <> if a then "True" else "False"
            <> " " <> osPathToText b)
        (foCreateDirectoryIfMissing a b)
    foCopyFileWithMetadata a b = logFsOp
        ("copyFileWithMetadata " <> osPathToText a
            <> " " <> osPathToText b)
        (foCopyFileWithMetadata a b)
    foRemoveFile a = logFsOp
        ("removeFile " <> osPathToText a)
        (foRemoveFile a)
    foCreateFileLink a b = logFsOp
        ("createFileLink " <> osPathToText a <> " " <> osPathToText b)
        (foCreateFileLink a b)

logFsOp :: forall m a. (MonadIO m, FsOps m)
    => T.Text
    -> AstowMonadT m a
    -> AstowMonadT (LoggedFsOpsT m) a
logFsOp msg action = do
    tell1 $ Di.mkInfoDiagnostic msg
    liftIO $ hPutStrLn stderr msg
    let l = runAstowMonadT action
                :: FallibleT (W.WriterT (KissDList Di.Diagnostic) m ) a
        l' = W.runWriterT $ runFallibleT l
    (r, diag) <- lift $ (LoggedFsOpsT $ l')
    tell diag
    case r of
        Aborted -> abort
        Completed False x -> invalid x
        Completed True x -> return x
